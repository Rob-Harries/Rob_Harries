rm(list=ls())
gc()


base_path <- "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/Allocation Tool"


library(data.table)
options(datatable.optimize = 1)

  # =========================
  # Load inputs
  # ========================
  
  
  load_inputs <- function(base_path) {
    list(
      xy   = fread(file.path(base_path, "X Y Allocation Priority.csv")),
      ss   = fread(file.path(base_path, "Station Sequence.csv")),
      lvl  = fread(file.path(base_path, "Level Priority.csv")),
      excl = fread(file.path(base_path, "Bay Exclude.csv"))
    )
  }
  
  # Usage
  inp <- load_inputs("C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/Allocation Tool")
  
  
  ##### 
  clean_inputs <- function(inp) {
    
    inp$xy[, `:=`(
      Station = as.character(Station),
      Aisle   = as.character(Aisle),
      Bay     = as.character(Bay),
      Print_Branch = as.character(Print_Branch)
    )]
    
    inp$excl[, `:=`(
      `Exclude Bays` = trimws(`Exclude Bays`)
    )]
    
    inp
  }
  
  inp <- clean_inputs(inp)
  
### Setkey
  setkey(inp$xy, Print_Branch, Station, AllocationSequence)
  setkey(inp$excl, Print_Branch, `Exclude Bays`)
  
  # =========================
  # Build FullBay
  # =========================
  inp$xy[, FullBay := paste0(Station, Aisle, Bay)]
  
  # =========================
  # Apply exclusions
  # =========================
  inp$excl <- inp$excl[
    !is.na(`Exclude Bays`) &
      `Exclude Bays` != "" &
      `Exclude Bays` != "0"
  ]
  
  inp$xy <- inp$xy[!inp$excl, on = .(Print_Branch, FullBay = `Exclude Bays`)]
  
  ##########Rank Bays within each Station
  setorder(inp$xy, Print_Branch, Station, AllocationSequence)
  
  inp$xy[, RankInStation := seq_len(.N),
         by = .(Print_Branch, Station)]
  
  # =========================================================
  # Assumes you already ran:
  # - load_inputs
  # - clean_inputs
  # - exclusions applied
  # - RankInStation created
  # =========================================================
  
  branch <- "305M"
  
  # =========================
  # Filter once
  # =========================
  xy_br <- inp$xy[Print_Branch == branch]
  lvl_br <- inp$lvl[Print_Branch == branch]
  ss_br  <- inp$ss[Print_Branch == branch]
  
  # =========================
  # Level order
  # =========================
  setorder(lvl_br, Sequence)
  lvl_br[, LevelIdx := seq_len(.N)]
  
  # =========================
  # Station sequence chunks
  # =========================
  setorder(ss_br, Sequence)
  
  n_station <- uniqueN(ss_br$Station)
  
  ss_br[, chunk := ((seq_len(.N) - 1L) %/% n_station) + 1L]
  ss_br[, chunk_pos := seq_len(.N) - (chunk - 1L) * n_station]
  
  # =========================
  # Create wave table
  # =========================
  max_wave <- xy_br[, max(RankInStation)]
  
  waves <- data.table(
    RankInStation = seq_len(max_wave)
  )
  
  # =========================
  # Cross join: wave x level
  # =========================
  wave_level <- CJ(
    RankInStation = seq_len(max_wave),
    LevelIdx = lvl_br$LevelIdx
  )
  
  # assign running chunk counter
  wave_level[, chunk_counter := seq_len(.N)]
  wave_level[, chunk := ((chunk_counter - 1L) %% uniqueN(ss_br$chunk)) + 1L]
  
  # =========================
  # Join wave/level to stations
  # =========================
  wl_station <- wave_level[
    ss_br,
    on = .(chunk),
    allow.cartesian = TRUE
  ]
  
  # =========================
  # Join to XY (actual bays)
  # =========================
  alloc_dt <- wl_station[
    xy_br,
    on = .(RankInStation, Station),
    nomatch = 0L
  ]
  
  # =========================
  # Add level + ordering fields
  # =========================
  alloc_dt[
    lvl_br,
    on = "LevelIdx",
    Level := i.Level
  ]
  
  # order driver
  setorder(
    alloc_dt,
    chunk_counter,     # serpentine progression
    chunk_pos          # position within station sequence
  )
  
  # =========================
  # Final sequence
  # =========================
  alloc_dt[, Sequence := seq_len(.N)]
  alloc_dt[, Bay_Shelf := paste0(FullBay, Level)]
  
  # =========================
  # Final output
  # =========================
  output_dt <- alloc_dt[, .(Sequence, Bay_Shelf)]
  
  output_dt[]
 
  fwrite(output_dt, file.path(base_path, "Allocation_Seq_Output.csv"))
  
  
  #########################
  #Allocation to Bay
  ##########################
  
  #########################
  # Allocation to Bay - with Backfill Logic
  #########################
  
  # =========================
  # Step 1 - Load product file
  # =========================
  
  prod <- fread(file.path(base_path, "Product Allocation Priority.csv"))
  
  prod <- prod[Print_Branch == branch]
  
  setorder(prod, `Allocation Order`)
  
  prod[, SlotsReq := as.integer(Adj_Shelf_Slot)]
  
  # Add stable row id
  prod[, ProductRowID := .I]
  
  slots_per_bay <- 35L
  
  # Safety checks
  if (any(is.na(prod$SlotsReq))) {
    stop("Some products have NA SlotsReq from Adj_Shelf_Slot")
  }
  
  if (any(prod$SlotsReq <= 0L)) {
    stop("Some products have SlotsReq <= 0")
  }
  
  if (any(prod$SlotsReq > slots_per_bay)) {
    stop("At least one product requires more than 35 slots and cannot fit in one bay")
  }
  
  
  # =========================
  # Step 2 - Build TRUE bay list
  # =========================
  
  bay_dt <- output_dt[, .(
    Bay = sub("[0-9]+$", "", Bay_Shelf)
  )]
  
  bay_dt <- unique(bay_dt, by = "Bay")
  
  bay_dt[, BaySequence := seq_len(.N)]
  
  bay_dt[, `:=`(
    Station = substr(Bay, 1, 1),
    Aisle   = substr(Bay, 2, 2),
    Bay2    = substr(Bay, 3, nchar(Bay))
  )]
  
  
  # =========================
  # Step 3 - FINAL bay allocation logic
  # =========================
  
  slots_per_bay_large <- 35L
  slots_per_bay_small_phase_a <- 30L
  slots_per_bay_final <- 35L
  
  large_slot_threshold <- 5L
  
  remaining_ids <- prod$ProductRowID
  
  allocation_rows <- vector("list", nrow(prod))
  
  alloc_i <- 1L
  current_bay <- 1L
  
  while (length(remaining_ids) > 0L) {
    
    current_used <- 0L
    bay_fill_position <- 1L
    
    bay_has_large_product <- FALSE
    
    repeat {
      
      # ============================================
      # Products still waiting
      # ============================================
      
      candidate_dt <- prod[
        ProductRowID %in% remaining_ids
      ]
      
      setorder(candidate_dt, `Allocation Order`)
      
      # ============================================
      # Determine current bay mode
      # ============================================
      
      if (bay_has_large_product) {
        
        # ------------------------------------------
        # LARGE PRODUCT BAY
        # ------------------------------------------
        # Full 35-slot behaviour
        
        current_bay_capacity <- slots_per_bay_large
        
        candidate_dt[
          ,
          CandidateFits := current_used + SlotsReq <= current_bay_capacity
        ]
        
        fit_candidates <- candidate_dt[CandidateFits == TRUE]
        
        if (nrow(fit_candidates) == 0L) {
          break
        }
        
        # Normal allocation order
        setorder(fit_candidates, `Allocation Order`)
        
        chosen <- fit_candidates[1]
        
        fill_mode <- "LARGE_BAY"
        
      } else {
        
        # ==========================================
        # SMALL-ONLY BAY
        # ==========================================
        
        # ------------------------------------------
        # PHASE A
        # Fill normally up to 30
        # ------------------------------------------
        
        if (current_used < slots_per_bay_small_phase_a) {
          
          candidate_dt[
            ,
            CandidateFits := current_used + SlotsReq <= slots_per_bay_small_phase_a
          ]
          
          phase_a_candidates <- candidate_dt[
            CandidateFits == TRUE
          ]
          
          if (nrow(phase_a_candidates) > 0L) {
            
            # Normal allocation order
            setorder(phase_a_candidates, `Allocation Order`)
            
            chosen <- phase_a_candidates[1]
            
            fill_mode <- "SMALL_PHASE_A"
            
          } else {
            
            # --------------------------------------
            # PHASE B
            # Final 5 slots reserved for slow movers
            # --------------------------------------
            
            candidate_dt[
              ,
              CandidateFits := current_used + SlotsReq <= slots_per_bay_final
            ]
            
            phase_b_candidates <- candidate_dt[
              CandidateFits == TRUE
            ]
            
            if (nrow(phase_b_candidates) == 0L) {
              break
            }
            
            # ✅ SLOWEST MOVERS FIRST
            setorder(phase_b_candidates, -`Allocation Order`)
            
            chosen <- phase_b_candidates[1]
            
            fill_mode <- "SMALL_PHASE_B_SLOW"
          }
          
        } else {
          
          # ----------------------------------------
          # Already above/equal 30
          # Continue slow-mover fill to 35
          # ----------------------------------------
          
          candidate_dt[
            ,
            CandidateFits := current_used + SlotsReq <= slots_per_bay_final
          ]
          
          phase_b_candidates <- candidate_dt[
            CandidateFits == TRUE
          ]
          
          if (nrow(phase_b_candidates) == 0L) {
            break
          }
          
          # ✅ SLOWEST MOVERS FIRST
          setorder(phase_b_candidates, -`Allocation Order`)
          
          chosen <- phase_b_candidates[1]
          
          fill_mode <- "SMALL_PHASE_B_SLOW"
        }
      }
      
      # ============================================
      # Selected product
      # ============================================
      
      chosen_id <- chosen$ProductRowID
      chosen_req <- chosen$SlotsReq
      
      first_waiting_id <- candidate_dt[1, ProductRowID]
      
      backfill_flag <- chosen_id != first_waiting_id
      
      # ============================================
      # Update bay type
      # ============================================
      
      chosen_is_large <- chosen_req > large_slot_threshold
      
      bay_has_large_product <- bay_has_large_product | chosen_is_large
      
      # Final bay capacity
      bay_capacity_after <- if (bay_has_large_product) {
        slots_per_bay_large
      } else {
        slots_per_bay_final
      }
      
      # ============================================
      # Store allocation
      # ============================================
      
      allocation_rows[[alloc_i]] <- data.table(
        ProductRowID = chosen_id,
        BaySequence = current_bay,
        BayFillPosition = bay_fill_position,
        SlotsUsed = chosen_req,
        RemainingBefore = bay_capacity_after - current_used,
        RemainingAfter = bay_capacity_after - current_used - chosen_req,
        BayCapacity = bay_capacity_after,
        BayHasLargeProduct = bay_has_large_product,
        BackfillFlag = backfill_flag,
        FillMode = fill_mode
      )
      
      alloc_i <- alloc_i + 1L
      
      bay_fill_position <- bay_fill_position + 1L
      
      # ============================================
      # Update bay state
      # ============================================
      
      current_used <- current_used + chosen_req
      
      # Remove product from waiting queue
      remaining_ids <- setdiff(remaining_ids, chosen_id)
      
      # Close bay if full
      if (current_used >= bay_capacity_after) {
        break
      }
    }
    
    current_bay <- current_bay + 1L
  }
  
  allocation_map <- rbindlist(
    allocation_rows,
    use.names = TRUE,
    fill = TRUE
  )
  
  # =========================
  # Step 4 - Join allocation back to product file and physical bay
  # =========================
  
  product_bay <- allocation_map[
    prod,
    on = "ProductRowID"
  ]
  
  product_bay <- product_bay[
    bay_dt,
    on = "BaySequence",
    nomatch = 0L
  ]
  
  
  # =========================
  # Step 5 - Add utilisation
  # =========================
  
  bay_util <- product_bay[
    ,
    .(
      UsedSlots = sum(SlotsUsed),
      BayCapacity = max(BayCapacity),
      FreeSlots = max(BayCapacity) - sum(SlotsUsed),
      ProductCount = .N,
      HasLargeProduct = any(SlotsReq > large_slot_threshold),
      BackfillCount = sum(BackfillFlag),
      SlowMoverFillCount = sum(
        FillMode == "SMALL_PHASE_B_SLOW"
      )
    ),
    by = .(BaySequence, Bay)
  ]
  
  # Join bay utilisation back to product level
  product_bay <- bay_util[
    product_bay,
    on = .(BaySequence, Bay)
  ]
  
  # =========================
  # Step 6 - Output check file
  # =========================
  
  product_check_file <- product_bay[
    order(BaySequence, BayFillPosition),
    .(
      BaySequence,
      Bay,
      Station,
      Aisle,
      Bay2,
      BayFillPosition,
      Product_Code,
      Product_Description,
      `Allocation Order`,
      SlotsReq,
      SlotsUsed,
      UsedSlots,
      FreeSlots,
      RemainingBefore,
      RemainingAfter,
      BackfillFlag,
      Print_Branch,
      On_Circuit_Total_Lines,
      On_Circuit_Total_Qty,
      Ideal_STD,
      Location_Code,
      Replenish_Quantity,
      Current_Shelf_Balance,
      Current_Order_Balance,
      Pack_Size,
      Product_Group,
      Report_Group,
      Location_Type,
      Full_Supplier,
      Umbrella_Group,
      Max_Shelf_Balance,
      Max_Demand,
      Max_Stock_Relex,
      Relex_Ave_Demand,
      Daily_Demand,
      Live_Max,
      Live_Min,
      Live_Median,
      Volume,
      Adj_Shelf_Slot,
      Shelf_STD,
      Ideal_Area,
      De_List,
      Central,
      Final_Action,
      `Demand Lines`,
      `Lines Per Slot`,
      `Allocation Rank`,
      `Days Cover`,
      STFRP
    )
  ]
  
  
  # =========================
  # Step 7 - Validation checks
  # =========================
  
  bay_overflow_check <- bay_util[UsedSlots > BayCapacity]
  
  if (nrow(bay_overflow_check) > 0L) {
    print(bay_overflow_check)
    stop("Bay allocation error: one or more bays exceed their allowed capacity")
  }
  
  small_only_bay_check <- bay_util[
    HasLargeProduct == FALSE &
      UsedSlots > slots_per_bay_final
    
  ]
  
  if (nrow(small_only_bay_check) > 0L) {
    print(small_only_bay_check)
    stop("Rule error: bay with no >5 slot products exceeds 30 slots")
  }
  
  
  # 2. No product should be lost
  missing_products <- prod[
    !allocation_map,
    on = "ProductRowID"
  ]
  
  if (nrow(missing_products) > 0L) {
    print(missing_products[, .(Product_Code, `Allocation Order`, SlotsReq)])
    stop("Product allocation error: one or more products were not allocated")
  }
  
  # 3. Allocated slots should equal required slots by product
  slot_check <- product_check_file[
    , .(
      Required = first(SlotsReq),
      Allocated = sum(SlotsUsed)
    ),
    by = .(Product_Code, `Allocation Order`)
  ][Required != Allocated]
  
  if (nrow(slot_check) > 0L) {
    print(slot_check)
    stop("Product allocation error: allocated slots do not match required slots")
  }
  
  # 4. Check all bay totals
  bay_util[UsedSlots > slots_per_bay]
  
  
  # =========================
  # 4. Products missing from final output
  # =========================
  
  missing_from_output <- prod[
    !product_check_file,
    on = .(Product_Code, `Allocation Order`)
  ]
  
  if (nrow(missing_from_output) > 0L) {
    
    cat("\n====================================\n")
    cat("PRODUCTS MISSING FROM OUTPUT FILE\n")
    cat("====================================\n")
    
    print(
      missing_from_output[
        ,
        .(
          Product_Code,
          Product_Description,
          `Allocation Order`,
          SlotsReq,
          Daily_Demand,
          Volume
        )
      ]
    )
    
    fwrite(
      missing_from_output,
      file.path(
        base_path,
        "Missing_Products_Check.csv"
      )
    )
    
  } else {
    
    cat("\nNo products missing from product_check_file\n")
  }
  
  
  
  # =========================
  # Step 8 - Export
  # =========================
  
  fwrite(product_check_file, file.path(base_path, "Product_Bay_Check_File.csv"))
  fwrite(bay_util, file.path(base_path, "Bay_Utilisation_Check_File.csv"))
  
  
  # =========================
  # Quick review outputs
  # =========================
  
  product_check_file[]
  bay_util[]
  
  
  # =========================
  # Bay utilisation summary
  # =========================
  
  bay_summary <- bay_util[
    ,
    .(
      Bays = .N,
      AvgUsed = round(mean(UsedSlots), 2),
      AvgFree = round(mean(FreeSlots), 2),
      MinUsed = min(UsedSlots),
      MaxUsed = max(UsedSlots),
      SlowMoverFillBays = sum(SlowMoverFillCount > 0),
      BackfillBays = sum(BackfillCount > 0)
    ),
    by = .(HasLargeProduct)
  ]
  
  bay_summary[]
  
  fwrite(
    bay_summary,
    file.path(base_path, "Bay_Utilisation_Summary.csv")
  )
  

  
  