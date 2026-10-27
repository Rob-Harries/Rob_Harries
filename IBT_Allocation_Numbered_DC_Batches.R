###############################################################################
## 01. PURPOSE AND BUSINESS RULES
###############################################################################
## Full replacement for the supplied IBT allocation and WD slotting script.
## 408T pallet-rack stock is pulled in section 05.6.
## Direct rack allocation is unchanged: priorities 1-3 round up; 4-5 round down.
## Primary tolerance is 30% of allocated quantity, with a 15% fallback.
## NEW: multi-slot shelf PAL demand is batched by receiving DC, up to five batches.
## Each DC/product request group stays together. Each PAL batch uses a different day.
## Aim for one pallet slot per product/day; multi-slot batches remain possible.
## This is a planning output, not an automatic stock-release instruction.

###############################################################################
## 02. RESET SESSION, WORKING DIRECTORY AND LIBRARIES
###############################################################################
rm(list = ls())
gc()
setwd('C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/')

library(RODBC)
library(data.table)
library(lubridate)
library(foreach)
library(plyr)
library(dplyr)
library(matrixStats)
library(stringr)
library(hms)
library(DBI)
library(odbc)
library(stringi)
library(zoo)
library(readxl)

###############################################################################
## 03. CONFIGURATION AND DATABASE CONNECTION
###############################################################################
Output_Directory <- paste0(
  'C:/Users/harriesr/OneDrive - Hallo Healthcare Group/',
  'R Coding/Allocation Tool'
)
if (!dir.exists(Output_Directory)) {
  dir.create(Output_Directory, recursive = TRUE)
}

## 03.1. Allocation parameters
Primary_Tolerance_Pct <- 30
Fallback_Tolerance_Pct <- 15
Max_Product_Batches <- 5L
Days <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

## 03.2. Read the database password from the existing credentials file
connection_function <- function() {
  password_file <- 'C:/Users/harriesr/credentials/azure_supply_chain.txt'
  if (!file.exists(password_file)) {
    stop('Password file not found: ', password_file, call. = FALSE)
  }
  pwd <- trimws(readLines(password_file, n = 1L, warn = FALSE))
  if (length(pwd) != 1L || !nzchar(pwd)) {
    stop('The password file is empty.', call. = FALSE)
  }
  con <- RODBC::odbcConnect(
    dsn = 'Azure Supply Chain', uid = 'Harriesr', pwd = pwd
  )
  rm(pwd)
  if (identical(con, -1L) || identical(con, -1)) {
    stop('Database connection failed.', call. = FALSE)
  }
  con
}
con <- connection_function()

## 03.3. Stop immediately if a SQL query returns an error rather than a table
query_table <- function(channel, query, label) {
  result <- RODBC::sqlQuery(channel, query)
  if (!is.data.frame(result)) {
    stop(label, ' failed: ', paste(result, collapse = '\n'), call. = FALSE)
  }
  as.data.table(result)
}

###############################################################################
## 04. LOAD LOCAL INPUT FILES
###############################################################################
IBT_Full <- fread(paste0(
  'C:/Users/harriesr/OneDrive - Hallo Healthcare Group/',
  'R Coding/txt files/408T/408T_IBT_Full.csv'
))
Zones <- fread(paste0(
  'C:/Users/harriesr/OneDrive - Hallo Healthcare Group/',
  'R Coding/txt files/408T/Zones.csv'
))

###############################################################################
## 05. DATABASE DATA PULLS
###############################################################################

## 05.1. Current branch stock and warehouse zones
Main_Query <- query_table(con, "
SELECT mq.Branch AS Print_Branch, Product_Code,
       Current_Shelf_Balance, Current_Order_Balance,
       Warehouse_Zone, Warehouse_Area
FROM SupplyChainReporting.dbo.main_query_all mq
LEFT JOIN AAHOperations.dbo.lu_warehouse_zones wz
  ON LEFT(mq.Location_Code,1) = wz.Station AND mq.Branch = wz.Branch
LEFT JOIN AAHOperations.dbo.lu_ws_aframe_long_short af
  ON LEFT(mq.Location_Code,5) = Aframe_Sector AND mq.Branch = Branch_ID
WHERE mq.Branch IN
  ('106m','201D','305m','405n','406p','501g','504m','703m','706s')
  AND Date >= DATEADD(day,-1,GETDATE())
", 'Main_Query')

## 05.2. Actual sales and days cover; retain the existing 11-day divisor
Actual_Days_Sales <- query_table(con, "
SELECT bi.Print_Branch, p.Product_Code,
       sdm.Total_Current_Shelf_Balance,
       COUNT(DISTINCT CONVERT(date,bi.Processing_Date)) AS Date_Count,
       COUNT(bi.Line_Quantity) AS Lines,
       SUM(bi.Line_Quantity) AS Line_Qty,
       CAST(COUNT(bi.Line_Quantity) AS decimal(18,2))/11.0 AS Ave_Daily_Lines,
       CAST(SUM(bi.Line_Quantity) AS decimal(18,2))/11.0 AS Ave_Daily_Qty,
       CAST(sdm.Total_Current_Shelf_Balance AS decimal(18,2)) /
         NULLIF(CAST(SUM(bi.Line_Quantity) AS decimal(18,2))/11.0,0)
         AS Act_Days_Cover
FROM SupplyChainReporting.dbo.b2b_invoices bi
INNER JOIN SupplyChainReporting.dbo.product_list p
  ON bi.Prod_Key_C = p.Prod_Key_C
INNER JOIN SupplyChainReporting.dbo.b2b_customers c
  ON bi.Cust_Key_C = c.Cust_Key_C
LEFT JOIN SupplyChainReporting.dbo.stock_daily_merged sdm
  ON p.Product_Code = sdm.Product_Code
  AND bi.Print_Branch = sdm.Branch
  AND sdm.Report_Date >= DATEADD(day,0,CONVERT(date,GETDATE()))
LEFT JOIN AAHOperations.dbo.lu_order_type_marker_details om
  ON bi.Order_Type_Marker = om.Order_Type_Marker
  AND bi.Print_Branch = om.Print_Branch
WHERE bi.Process_Month >= DATEADD(month,-2,GETDATE())
  AND bi.Processing_Date >= DATEADD(day,-15,CONVERT(date,GETDATE()))
  AND bi.Omit_Marker = '0'
  AND bi.Credit_Note_Analysis_Marker = '0'
  AND p.Product_Code NOT IN
    ('CUS0927W','LLO3350Q','AAH0081P','EME0088T','DEL0432N',
     'TRI0866V','LOW0008F','LOW0009B','FIN0507E','FUE0001X')
  AND p.Umbrella_Group NOT IN
    ('6','8','12','13','14','15','16','17','18','19','22','23')
  AND bi.Print_Branch IN
    ('106m','201D','305m','405n','406p','501g','504m',
     '602J','703m','706s','908b','408t')
GROUP BY bi.Print_Branch,p.Product_Code,sdm.Total_Current_Shelf_Balance
", 'Actual_Days_Sales')

## 05.3. Manual overstock locations
Auto_Overstock <- query_table(con, "
SELECT Print_Branch, pl.Product_Code,
       MAX(Location_Code) AS OverStock_Location,
       MAX(Maximum_Qty) AS Overstock_STD
FROM SupplyChainReporting.dbo.aah_location al
LEFT JOIN SupplyChainReporting.dbo.product_list pl
  ON al.Prod_Key_C = pl.Prod_Key_C
WHERE CONVERT(date,al.Update_Timestamp) >=
      DATEADD(day,-1,CONVERT(date,GETDATE()))
  AND Manual_Auto = 'M'
  AND (Print_Branch IN
       ('106m','201d','305m','405n','406p','501g','504m','703m','706s')
       OR (Print_Branch = '405n' AND LEFT(Location_Code,1) = 'C'))
GROUP BY Print_Branch,pl.Product_Code
", 'Auto_Overstock')

## 05.4. Relex forecast; retain the existing date filters and divisor
Relex <- query_table(con, "
SELECT Location AS Print_Branch, TRIM(Product_Code) AS Product_Code,
       SUM(Effective_Quantity)/11 AS Relex_Ave_Qty
FROM SupplyChainReporting.dbo.relex_forecast
WHERE CONVERT(date,Forecast_Date) >= DATEADD(day,1,GETDATE())
  AND CONVERT(date,Forecast_Date) <= DATEADD(day,15,GETDATE())
GROUP BY Location,TRIM(Product_Code)
", 'Relex')

## 05.5. Product weights; prefer local 408T dimensions
Weights <- query_table(con, "
SELECT pl.Product_Code,
       CASE WHEN ld.Weight IS NOT NULL THEN ld.Weight
            ELSE pl.Weight END AS Weight_Gram
FROM SupplyChainReporting.dbo.product_list pl
LEFT JOIN SupplyChainReporting.dbo.local_dimensions ld
  ON pl.Prod_Key_C = ld.Prod_Key_C AND ld.Branch = '408T'
WHERE pl.Stocked_Anywhere = 'Y'
ORDER BY pl.Product_Code
", 'Weights')

## 05.6. 408T pallet-rack stock, locations and expiry dates
Pallet_Rack <- query_table(con, "
SELECT Branch,Product_Code,Location AS PR_Location,Quantity,Expiry_Date
FROM SupplyChainReporting.dbo.pallet_racking_daily_snapshot
WHERE Product_Code IS NOT NULL AND Branch = '408T'
ORDER BY Product_Code,Branch
", 'Pallet_Rack')

## 05.7. Product dimensions
Dims <- query_table(con, "
SELECT Product_Code,Live_Max,Live_Min,Live_Median,Nat_or_Loc,Volume
FROM AAHOperations.dbo.worksmart_final_dim
", 'Dims')

RODBC::odbcClose(con)

###############################################################################
## 06. BUILD BRANCH / PRODUCT STOCK AND COVER DATA
###############################################################################
Product_File <- merge(
  Main_Query, Auto_Overstock,
  by = c('Print_Branch','Product_Code'), all.x = TRUE
)
Product_File[, Location_Code_Group := substring(OverStock_Location,1,2)]
Product_File <- merge(
  Product_File, Zones,
  by.x = c('Print_Branch','Location_Code_Group'),
  by.y = c('DC','Location_Range'), all.x = TRUE
)
Product_File <- merge(
  Product_File, Actual_Days_Sales,
  by = c('Print_Branch','Product_Code'), all.x = TRUE
)
Product_File <- merge(
  Product_File, Relex,
  by = c('Print_Branch','Product_Code'), all.x = TRUE
)
Product_File[is.na(Act_Days_Cover), Act_Days_Cover := 9999]
num_cols <- names(Product_File)[vapply(Product_File,is.numeric,logical(1))]
Product_File[, (num_cols) := lapply(
  .SD, function(x) fifelse(is.na(x),0,x)
), .SDcols = num_cols]
Product_File[, Forcast_Days_Cover := Current_Shelf_Balance/Relex_Ave_Qty]
## Include 0/0 as well as infinite cover in the no-forecast sentinel.
Product_File[!is.finite(Forcast_Days_Cover), Forcast_Days_Cover := 9999]
Product_File[, Days_Cover_Min := pmin(Forcast_Days_Cover,Act_Days_Cover)]

###############################################################################
## 07. BUILD IBT REQUEST DATA, PRIORITY, WEIGHT AND PALLET QUANTITY
###############################################################################
IBT_File <- merge(
  IBT_Full, Product_File,
  by.x = c('DC','Product'), by.y = c('Print_Branch','Product_Code'),
  all.x = TRUE
)
IBT_File[, New_Priority := fcase(
  Days_Cover_Min <= 5, 1L,
  Days_Cover_Min <= 10, 2L,
  Days_Cover_Min <= 15, 3L,
  Days_Cover_Min <= 20, 4L,
  default = 5L
)]
IBT_File <- merge(
  IBT_File,Dims,by.x = 'Product',by.y = 'Product_Code',all.x = TRUE
)
IBT_File <- merge(
  IBT_File,Weights,by.x = 'Product',by.y = 'Product_Code',all.x = TRUE
)
IBT_File[, Weight_Band := fcase(
  is.na(Weight_Gram) | Weight_Gram <= 0, 'Weight Missing',
  Weight_Gram <= 250, 'Very Light',
  Weight_Gram <= 500, 'Light',
  Weight_Gram <= 1000, 'Medium',
  Weight_Gram <= 2000, 'Heavy',
  default = 'Very Heavy'
)]

## 07.1. Avoid -Inf where every rack quantity for a product is missing
Max_Product <- Pallet_Rack[
  , .(Max_PR_Quantity = if (all(is.na(Quantity))) 0 else
        max(Quantity,na.rm = TRUE)),
  by = Product_Code
]
IBT_File <- merge(
  IBT_File,Max_Product,
  by.x = 'Product',by.y = 'Product_Code',all.x = TRUE
)
num_cols <- names(IBT_File)[vapply(IBT_File,is.numeric,logical(1))]
IBT_File[, (num_cols) := lapply(
  .SD, function(x) fifelse(is.na(x),0,x)
), .SDcols = num_cols]

## 07.2. Invalid volume must not create infinite pallet capacity
IBT_File[, Calc_Pallet_Qty := NA_real_]
IBT_File[is.finite(Volume) & Volume > 0,
         Calc_Pallet_Qty := floor(((1100*1400*1000)*0.75)/Volume)]
IBT_File[, Pallet_Rack_Qty := pmax(
  Max_PR_Quantity,Calc_Pallet_Qty,na.rm = TRUE
)]
IBT_File[!is.finite(Pallet_Rack_Qty) | Pallet_Rack_Qty <= 0,
         Pallet_Rack_Qty := NA_real_]

###############################################################################
## 08. WHOLE RACK-ROW ALLOCATION FUNCTION
###############################################################################
allocate_ibt_rack <- function(
    IBT_File, Pallet_Rack, max_dp_units = 2000000L,
    primary_tolerance_pct = 30, fallback_tolerance_pct = 15) {

  ## 08.1. Copy and validate inputs
  demand <- data.table::copy(data.table::as.data.table(IBT_File))
  rack <- data.table::copy(data.table::as.data.table(Pallet_Rack))
  required_demand <- c('Product','DC','Quantity','New_Priority','Days_Cover_Min')
  required_rack <- c('Branch','Product_Code','PR_Location','Quantity','Expiry_Date')
  if (!all(required_demand %in% names(demand))) {
    stop('IBT_File is missing: ',paste(
      setdiff(required_demand,names(demand)),collapse = ', '
    ))
  }
  if (!all(required_rack %in% names(rack))) {
    stop('Pallet_Rack is missing: ',paste(
      setdiff(required_rack,names(rack)),collapse = ', '
    ))
  }
  if (!is.numeric(max_dp_units) || length(max_dp_units) != 1L ||
      !is.finite(max_dp_units) || max_dp_units < 1 || max_dp_units > 20000000) {
    stop('max_dp_units must be between 1 and 20000000.')
  }
  for (pct in list(primary_tolerance_pct,fallback_tolerance_pct)) {
    if (!is.numeric(pct) || length(pct) != 1L || !is.finite(pct) || pct < 0) {
      stop('Tolerance percentages must be finite, non-negative numbers.')
    }
  }

  ## 08.2. IDs identify input rows, not physical pallet IDs
  demand[, IBT_Request_ID := .I]
  rack[, Rack_Row_ID := .I]
  demand[, `:=`(
    Product = toupper(trimws(as.character(Product))),
    DC = toupper(trimws(as.character(DC)))
  )]
  rack[, `:=`(
    Product_Code = toupper(trimws(as.character(Product_Code))),
    Branch = toupper(trimws(as.character(Branch))),
    PR_Location = toupper(trimws(as.character(PR_Location)))
  )]
  to_number <- function(x) suppressWarnings(as.numeric(as.character(x)))
  demand[, `:=`(
    Requested_Qty = to_number(Quantity),
    Allocation_Priority = to_number(New_Priority),
    Allocation_Cover = to_number(Days_Cover_Min)
  )]
  rack[, Rack_Qty := to_number(Quantity)]
  rack[, Expiry_Sort := suppressWarnings(as.Date(as.character(Expiry_Date)))]

  ## 08.3. Hold ambiguous or invalid rack records
  dup_cols <- c('Branch','Product_Code','PR_Location','Quantity','Expiry_Date')
  duplicate_rows <- duplicated(rack,by = dup_cols) |
    duplicated(rack,by = dup_cols,fromLast = TRUE)
  rack[, Hold_Reason := '']
  rack[duplicate_rows, Hold_Reason := 'Ambiguous identical rack records']
  rack[!is.finite(Rack_Qty) | Rack_Qty <= 0 | Rack_Qty != floor(Rack_Qty),
       Hold_Reason := 'Invalid rack quantity']
  rack[is.na(Branch) | Branch != '408T', Hold_Reason := 'Not 408T stock']
  rack[is.na(Product_Code) | Product_Code == '' |
         is.na(PR_Location) | PR_Location == '',
       Hold_Reason := 'Missing product or rack location']
  rack[, Allocated_Request_ID := NA_integer_]

  ## 08.4. Validate demand
  demand[, `:=`(
    Allocated_Qty = 0, Rack_Entries_Selected = 0L,
    Allocation_Status = 'Pending'
  )]
  demand[!is.finite(Requested_Qty) | Requested_Qty <= 0 |
           Requested_Qty != floor(Requested_Qty),
         Allocation_Status := 'Invalid requested quantity']
  demand[Allocation_Status == 'Pending' &
           (is.na(Product) | Product == '' | is.na(DC) | DC == ''),
         Allocation_Status := 'Missing product or destination']
  demand[Allocation_Status == 'Pending' &
           (!Allocation_Priority %in% 1:5 | !is.finite(Allocation_Cover)),
         Allocation_Status := 'Missing priority/cover data']

  ## 08.5. Exact subset-sum search; minimum pick count is not guaranteed
  choose_rows <- function(qty,target,round_up) {
    if (sum(qty) < target && round_up) return(seq_along(qty))
    if (round_up) {
      single <- qty[qty >= target]
      cap <- if (length(single)) min(single) else target + max(qty) - 1
    } else {
      cap <- target
    }
    cap <- min(cap,sum(qty))
    if (cap > max_dp_units) {
      stop('Subset search exceeds max_dp_units; review this product/request.')
    }
    cap <- as.integer(cap)
    reached <- rep(FALSE,cap + 1L)
    previous_sum <- rep(NA_integer_,cap + 1L)
    previous_item <- rep(NA_integer_,cap + 1L)
    reached[1L] <- TRUE
    for (j in seq_along(qty)) {
      if (qty[j] > cap) next
      q <- as.integer(qty[j])
      old <- which(reached) - 1L
      old <- old[old <= cap - q]
      new <- old + q
      fresh <- !reached[new + 1L]
      if (any(fresh)) {
        new <- new[fresh]
        previous_sum[new + 1L] <- old[fresh]
        previous_item[new + 1L] <- j
        reached[new + 1L] <- TRUE
      }
      if (target <= cap && reached[target + 1L]) break
    }
    sums <- which(reached) - 1L
    total <- if (round_up) min(sums[sums >= target]) else max(sums)
    selected <- integer()
    while (total > 0L) {
      j <- previous_item[total + 1L]
      if (is.na(j)) stop('Subset reconstruction failed.')
      selected <- c(selected,j)
      total <- previous_sum[total + 1L]
    }
    selected
  }

  ## 08.6. Share product stock across all destinations; urgent requests first
  pools <- split(
    rack[Hold_Reason == '',Rack_Row_ID],
    rack[Hold_Reason == '',Product_Code]
  )
  request_order <- demand[Allocation_Status == 'Pending'][
    order(Allocation_Priority,Allocation_Cover,IBT_Request_ID),IBT_Request_ID
  ]
  fallback_up_status <- paste0(
    'Rounded up (',fallback_tolerance_pct,'% fallback)'
  )
  fallback_down_status <- paste0(
    'Rounded down (',fallback_tolerance_pct,'% fallback)'
  )

  for (i in request_order) {
    product <- demand$Product[i]
    target <- demand$Requested_Qty[i]
    priority <- demand$Allocation_Priority[i]
    ids <- pools[[product]]
    if (!length(ids)) {
      data.table::set(demand,i,'Allocation_Status','No available rack stock')
      next
    }
    candidates <- rack[ids][order(Expiry_Sort,Rack_Row_ID,na.last = TRUE)]
    ids <- candidates$Rack_Row_ID
    qty <- candidates$Rack_Qty

    ## 08.7. Primary rule: priorities 1-3 up, priorities 4-5 down
    selected <- choose_rows(qty,target,priority <= 3)
    chosen <- ids[selected]
    allocated <- sum(qty[selected])
    within_tolerance <- allocated > 0 &&
      abs(target-allocated)*100 <= allocated*primary_tolerance_pct
    used_fallback <- FALSE

    ## 08.8. Fallback: search above and below for every priority
    if (!within_tolerance) {
      fallback_selections <- list(
        choose_rows(qty,target,FALSE),choose_rows(qty,target,TRUE)
      )
      fallback_totals <- vapply(
        fallback_selections,function(rows) sum(qty[rows]),numeric(1)
      )
      fallback_valid <- fallback_totals > 0 &
        abs(target-fallback_totals)*100 <=
        fallback_totals*fallback_tolerance_pct
      valid_options <- which(fallback_valid)
      if (length(valid_options)) {
        best_option <- valid_options[order(
          abs(fallback_totals[valid_options]-target),
          fallback_totals[valid_options]
        )[1L]]
        selected <- fallback_selections[[best_option]]
        chosen <- ids[selected]
        allocated <- fallback_totals[best_option]
        within_tolerance <- TRUE
        used_fallback <- TRUE
      } else {
        selected <- integer()
        chosen <- integer()
        allocated <- 0
      }
    }

    ## 08.9. Record status and consume only accepted stock
    if (!within_tolerance) {
      status <- paste0(
        'No combination within ',primary_tolerance_pct,
        '% rule or ',fallback_tolerance_pct,'% fallback'
      )
    } else if (allocated == target) {
      status <- 'Exact match'
    } else if (used_fallback && allocated > target) {
      status <- fallback_up_status
    } else if (used_fallback && allocated < target) {
      status <- fallback_down_status
    } else if (allocated > target) {
      status <- 'Rounded up'
    } else if (priority <= 3) {
      status <- 'Insufficient rack stock'
    } else {
      status <- 'Rounded down'
    }
    data.table::set(demand,i,'Allocated_Qty',allocated)
    data.table::set(demand,i,'Rack_Entries_Selected',length(chosen))
    data.table::set(demand,i,'Allocation_Status',status)
    if (length(chosen)) {
      data.table::set(rack,chosen,'Allocated_Request_ID',as.integer(i))
    }
    pools[[product]] <- ids[!ids %in% chosen]
  }

  ## 08.10. Variance and pick-detail outputs
  demand[, `:=`(
    Qty_Variance = Allocated_Qty-Requested_Qty,
    Unfilled_Qty = pmax(Requested_Qty-Allocated_Qty,0),
    Over_Supply_Qty = pmax(Allocated_Qty-Requested_Qty,0),
    Allocation_to_Request_Ratio = data.table::fifelse(
      is.finite(Requested_Qty) & Requested_Qty > 0,
      Allocated_Qty/Requested_Qty,NA_real_
    )
  )]
  detail <- merge(
    rack[!is.na(Allocated_Request_ID), .(
      IBT_Request_ID = Allocated_Request_ID,Rack_Row_ID,Branch,
      Product = Product_Code,PR_Location,Expiry_Date,
      Allocated_Rack_Qty = Rack_Qty
    )],
    demand[, .(IBT_Request_ID,DC,New_Priority)],
    by = 'IBT_Request_ID',all.x = TRUE,sort = FALSE
  )
  data.table::setorder(detail,IBT_Request_ID,Rack_Row_ID)
  rack[, Allocated := !is.na(Allocated_Request_ID)]

  ## 08.11. Reconcile quantities and prevent reused or held rack stock
  totals <- detail[, .(Check_Qty = sum(Allocated_Rack_Qty)),by = IBT_Request_ID]
  checks <- merge(
    demand[, .(IBT_Request_ID,Allocated_Qty)],totals,
    by = 'IBT_Request_ID',all.x = TRUE
  )
  checks[is.na(Check_Qty),Check_Qty := 0]
  stopifnot(
    !anyDuplicated(detail$Rack_Row_ID),
    all(checks$Allocated_Qty == checks$Check_Qty),
    nrow(rack[Allocated & Hold_Reason != '']) == 0L,
    nrow(demand[
      Allocation_Priority %in% 4:5 & is.finite(Requested_Qty) &
        Allocated_Qty > Requested_Qty &
        (Allocation_Status != fallback_up_status |
           abs(Requested_Qty-Allocated_Qty)*100 >
           Allocated_Qty*fallback_tolerance_pct)
    ]) == 0L,
    !any(demand$Allocation_Status == 'Pending')
  )
  demand[, c('Allocation_Priority','Allocation_Cover') := NULL]
  list(
    Summary = demand,Pick_Detail = detail,
    Exceptions = demand[Allocation_Status != 'Exact match'],Rack_Audit = rack
  )
}

###############################################################################
## 09. RUN DIRECT RACK ALLOCATION AND SEPARATE PICK ROUTES
###############################################################################
IBT_Allocation <- allocate_ibt_rack(
  IBT_File,Pallet_Rack,
  primary_tolerance_pct = Primary_Tolerance_Pct,
  fallback_tolerance_pct = Fallback_Tolerance_Pct
)
IBT_Allocation_Summary <- IBT_Allocation$Summary
IBT_Pallet_Pick_Detail <- IBT_Allocation$Pick_Detail
IBT_Allocation_Exceptions <- IBT_Allocation$Exceptions
IBT_Rack_Audit <- IBT_Allocation$Rack_Audit

IBT_Direct_Pallet_File <- copy(IBT_Allocation_Summary[Allocated_Qty > 0])
IBT_Direct_Pallet_File[, `:=`(
  Pick_Route = 'Direct Pallet',Qty_To_Process = Allocated_Qty
)]
IBT_Shelf_File <- copy(IBT_Allocation_Summary[Allocated_Qty == 0])
IBT_Shelf_File[, `:=`(
  Pick_Route = 'Shelf Pick',Qty_To_Process = Requested_Qty
)]
setnames(IBT_Shelf_File,'Allocation_Status','Rack_Allocation_Status')
stopifnot(
  nrow(IBT_Direct_Pallet_File)+nrow(IBT_Shelf_File) ==
    nrow(IBT_Allocation_Summary),
  !any(IBT_Direct_Pallet_File$IBT_Request_ID %in% IBT_Shelf_File$IBT_Request_ID)
)

###############################################################################
## 10. VALIDATE AND SUMMARISE SHELF-PATH DEMAND
###############################################################################
required_shelf_columns <- c(
  'Product','DC','Weight_Band','Qty_To_Process','Volume',
  'Pallet_Rack_Qty','New_Priority'
)
missing_shelf_columns <- setdiff(required_shelf_columns,names(IBT_Shelf_File))
if (length(missing_shelf_columns)) {
  stop('Shelf allocation is missing required columns: ',
       paste(missing_shelf_columns,collapse = ', '),call. = FALSE)
}
Weight_Band_Check <- IBT_Shelf_File[
  , .(Weight_Bands = uniqueN(Weight_Band)),by = Product
][Weight_Bands > 1L]
if (nrow(Weight_Band_Check)) {
  stop('Some shelf products have multiple weight bands.',call. = FALSE)
}

## 10.1. Do not schedule invalid requests as normal shelf demand
Invalid_Shelf_Requests <- IBT_Shelf_File[
  !is.finite(Qty_To_Process) | Qty_To_Process <= 0 |
    Qty_To_Process != floor(Qty_To_Process) |
    is.na(Product) | Product == '' | is.na(DC) | DC == ''
]
if (nrow(Invalid_Shelf_Requests)) {
  fwrite(Invalid_Shelf_Requests,file.path(
    Output_Directory,'IBT_Invalid_Shelf_Requests.csv'
  ))
  stop('Invalid shelf requests found; review IBT_Invalid_Shelf_Requests.csv.',
       call. = FALSE)
}

## 10.2. Helper for valid positive dimensions and pallet quantities
min_positive <- function(x) {
  values <- suppressWarnings(as.numeric(as.character(x)))
  values <- values[is.finite(values) & values > 0]
  if (!length(values)) return(NA_real_)
  min(values)
}
IBT_Summary <- IBT_Shelf_File[
  , .(
    Total_Qty = sum(Qty_To_Process),
    Total_Volume = sum(Qty_To_Process)*min_positive(Volume),
    Pallet_Qty = min_positive(Pallet_Rack_Qty),
    New_Proirity = min(New_Priority),
    Lines = .N
  ),
  by = .(Product,Weight_Band)
]

###############################################################################
## 11. ORIGINAL PRODUCT-LEVEL SLOT CALCULATIONS
###############################################################################
## Retain New_Proirity spelling for downstream compatibility.
IBT_Summary[, Above_Pallet_Slot := ceiling(
  Total_Volume/(((1000*600*2700)/3)*0.65)
)]
IBT_Summary[, Pallet_Slot := fifelse(
  Above_Pallet_Slot > 1,ceiling(Total_Qty/Pallet_Qty),0
)]
IBT_Summary[, Pick_Type := fifelse(Pallet_Slot > 0,'Pal','Above_Pal')]
IBT_Summary[, Used_Slot := fifelse(
  Pick_Type == 'Pal',Pallet_Slot,Above_Pallet_Slot
)]
IBT_Product_Base <- copy(IBT_Summary)
stopifnot(!anyDuplicated(IBT_Product_Base$Product))

###############################################################################
## 12. NEW RULE: SPLIT MULTI-SLOT PAL DEMAND INTO DC-BASED BATCHES
###############################################################################
## 12.1. Each Product/DC stays together; other products retain batch 1
DC_Batch_Demand <- IBT_Shelf_File[
  , .(DC_Qty = sum(Qty_To_Process),DC_Priority = min(New_Priority)),
  by = .(Product,DC)
]
DC_Batch_Demand[, Pick_Batch_ID := 1L]
Products_To_Split <- IBT_Product_Base[
  Pick_Type == 'Pal' & is.finite(Used_Slot) & Used_Slot > 1 &
    is.finite(Pallet_Qty) & Pallet_Qty > 0,
  Product
]

## 12.2. Priority-first best-fit packing, up to five batches per product
## This is a heuristic, not a guaranteed globally optimal packing solution.
for (Current_Product in Products_To_Split) {
  Current_Pallet_Qty <- IBT_Product_Base[
    Product == Current_Product,Pallet_Qty
  ]
  Product_DC_Rows <- DC_Batch_Demand[
    Product == Current_Product,which = TRUE
  ]
  Product_DC_Rows <- Product_DC_Rows[order(
    DC_Batch_Demand$DC_Priority[Product_DC_Rows],
    -DC_Batch_Demand$DC_Qty[Product_DC_Rows],
    DC_Batch_Demand$DC[Product_DC_Rows]
  )]
  Batch_Loads <- numeric(0)
  for (Current_Row in Product_DC_Rows) {
    Current_Qty <- DC_Batch_Demand$DC_Qty[Current_Row]
    Suitable_Batches <- which(
      Batch_Loads+Current_Qty <= Current_Pallet_Qty
    )
    if (length(Suitable_Batches)) {
      Selected_Batch <- Suitable_Batches[
        which.max(Batch_Loads[Suitable_Batches])
      ]
    } else if (length(Batch_Loads) < Max_Product_Batches) {
      Selected_Batch <- length(Batch_Loads)+1L
      Batch_Loads <- c(Batch_Loads,0)
    } else {
      ## Five batches exist: minimise extra slots, then resulting load.
      Extra_Slots <- ceiling((Batch_Loads+Current_Qty)/Current_Pallet_Qty) -
        ceiling(Batch_Loads/Current_Pallet_Qty)
      Selected_Batch <- order(
        Extra_Slots,Batch_Loads+Current_Qty,seq_along(Batch_Loads)
      )[1L]
    }
    Batch_Loads[Selected_Batch] <- Batch_Loads[Selected_Batch]+Current_Qty
    set(DC_Batch_Demand,Current_Row,'Pick_Batch_ID',as.integer(Selected_Batch))
  }
}

## 12.3. Add batch IDs to individual requests without multiplying rows
Shelf_Rows_Before <- nrow(IBT_Shelf_File)
IBT_Shelf_File <- merge(
  IBT_Shelf_File,
  DC_Batch_Demand[, .(Product,DC,Pick_Batch_ID)],
  by = c('Product','DC'),all.x = TRUE,sort = FALSE
)
stopifnot(
  nrow(IBT_Shelf_File) == Shelf_Rows_Before,
  !anyNA(IBT_Shelf_File$Pick_Batch_ID),
  !anyDuplicated(IBT_Shelf_File$IBT_Request_ID)
)

## 12.4. Rebuild the planning summary at Product + Pick_Batch_ID level
Batch_Totals <- IBT_Shelf_File[
  , .(
    Total_Qty = sum(Qty_To_Process),
    Total_Volume = sum(Qty_To_Process)*min_positive(Volume),
    New_Proirity = min(New_Priority),Lines = .N,
    DC_Count = uniqueN(DC),
    Requesting_DCs = paste(sort(unique(as.character(DC))),collapse = ', ')
  ),
  by = .(Product,Pick_Batch_ID)
]
IBT_Summary <- merge(
  Batch_Totals,
  IBT_Product_Base[, .(
    Product,Weight_Band,Pallet_Qty,Pick_Type,
    Above_Pallet_Slot,Pallet_Slot,Used_Slot
  )],
  by = 'Product',all.x = TRUE,sort = FALSE
)
## Keep the original PAL classification even when a batch is small.
IBT_Summary[Pick_Type == 'Pal', `:=`(
  Pallet_Slot = ceiling(Total_Qty/Pallet_Qty),
  Used_Slot = ceiling(Total_Qty/Pallet_Qty)
)]

## 12.5. Reconcile batch quantities to the original product totals
Batch_Quantity_Check <- merge(
  IBT_Product_Base[, .(Product,Original_Qty = Total_Qty)],
  IBT_Summary[, .(Batched_Qty = sum(Total_Qty)),by = Product],
  by = 'Product',all = TRUE
)
stopifnot(
  !anyNA(Batch_Quantity_Check),
  all(Batch_Quantity_Check$Original_Qty == Batch_Quantity_Check$Batched_Qty),
  !anyDuplicated(IBT_Summary,by = c('Product','Pick_Batch_ID'))
)

###############################################################################
## 13. DEFINE WD PHYSICAL LOCATIONS AND CAPACITY
###############################################################################
## Preserve the actual bay ranges in your supplied code.
## Pal: 07-52 excluding 15,16,45,46.
## Above_Pal: 15-52 excluding 15,16,45,46; first usable bay is 17.
WD_Pal_Bays <- sprintf('%02d',setdiff(7:52,c(15,16,45,46)))
WD_Above_Pal_Bays <- sprintf('%02d',setdiff(15:52,c(15,16,45,46)))

WD_Above_Pal_Locations <- CJ(
  Bay_Number = WD_Above_Pal_Bays,Slot_Position = 1:3
)
WD_Above_Pal_Locations[, `:=`(
  Aisle = 'WD',Pick_Type = 'Above_Pal',Level_Number = 1L
)]
WD_Pal_Locations <- CJ(Bay_Number = WD_Pal_Bays,Slot_Position = 1:2)
WD_Pal_Locations[, `:=`(
  Aisle = 'WD',Pick_Type = 'Pal',Level_Number = 2L
)]
WD_Location_Master <- rbindlist(
  list(WD_Above_Pal_Locations,WD_Pal_Locations),use.names = TRUE,fill = TRUE
)
WD_Location_Master[, `:=`(
  Location = paste0(Aisle,Bay_Number,Level_Number,Slot_Position),
  Bay_Number_Numeric = as.integer(Bay_Number)
)]
setorder(WD_Location_Master,Bay_Number_Numeric,Level_Number,Slot_Position)
Above_Pallet_Slots_Cap <- WD_Location_Master[
  Pick_Type == 'Above_Pal',uniqueN(Location)
]
Pallet_Slots_Cap <- WD_Location_Master[Pick_Type == 'Pal',uniqueN(Location)]

## 13.1. Capacity resets daily, as in the original script
Daily_Capacity <- CJ(Day_Number = 1:5,Pick_Type = c('Pal','Above_Pal'))
Daily_Capacity[, Allocation_Day := Days[Day_Number]]
Daily_Capacity[Pick_Type == 'Pal',Slot_Capacity := Pallet_Slots_Cap]
Daily_Capacity[Pick_Type == 'Above_Pal',Slot_Capacity := Above_Pallet_Slots_Cap]
Daily_Capacity[, `:=`(Slots_Used = 0,Slots_Remaining = Slot_Capacity)]
setorder(Daily_Capacity,Day_Number,Pick_Type)

###############################################################################
## 14. ALLOCATE PRODUCT BATCHES ACROSS MONDAY TO FRIDAY
###############################################################################
Slotting_Allocation <- copy(IBT_Summary)
Slotting_Allocation[, `:=`(
  Day_Number = NA_integer_,Allocation_Day = NA_character_,
  Allocation_Status = 'Not Allocated',Slots_Remaining_After = NA_real_
)]
## Preserve priority-first / largest-slot-first ordering; batch ID breaks ties.
setorder(
  Slotting_Allocation,New_Proirity,Pick_Type,-Used_Slot,Product,Pick_Batch_ID
)
for (i in seq_len(nrow(Slotting_Allocation))) {
  Current_Product <- Slotting_Allocation$Product[i]
  Current_Pick_Type <- Slotting_Allocation$Pick_Type[i]
  Slots_Needed <- Slotting_Allocation$Used_Slot[i]

  ## 14.1. Validate pick type and slot requirement
  if (is.na(Current_Pick_Type) ||
      !Current_Pick_Type %in% c('Pal','Above_Pal')) {
    Slotting_Allocation[i,Allocation_Status := 'Invalid Pick_Type']
    next
  }
  if (is.na(Slots_Needed) || !is.finite(Slots_Needed) ||
      Slots_Needed <= 0 || Slots_Needed != floor(Slots_Needed)) {
    Slotting_Allocation[i,Allocation_Status := 'Invalid Used_Slot']
    next
  }

  ## 14.2. NEW: never schedule two PAL batches of one product on the same day
  Already_Used_Days <- integer(0)
  if (Current_Pick_Type == 'Pal') {
    Already_Used_Days <- Slotting_Allocation[
      Product == Current_Product & Pick_Type == 'Pal' &
        Allocation_Status == 'Allocated' & !is.na(Day_Number),
      unique(Day_Number)
    ]
  }
  Available_Days <- Daily_Capacity[
    Pick_Type == Current_Pick_Type & Slots_Remaining >= Slots_Needed &
      !(Day_Number %in% Already_Used_Days)
  ][order(Day_Number)]

  ## 14.3. Allocate the earliest suitable day and deduct capacity
  if (nrow(Available_Days)) {
    Selected_Day_Number <- Available_Days$Day_Number[1L]
    Selected_Day <- Available_Days$Allocation_Day[1L]
    Capacity_Row <- Daily_Capacity[
      Day_Number == Selected_Day_Number & Pick_Type == Current_Pick_Type,
      which = TRUE
    ]
    if (length(Capacity_Row) == 1L) {
      Daily_Capacity[Capacity_Row, `:=`(
        Slots_Used = Slots_Used+Slots_Needed,
        Slots_Remaining = Slots_Remaining-Slots_Needed
      )]
      Slotting_Allocation[i, `:=`(
        Day_Number = Selected_Day_Number,Allocation_Day = Selected_Day,
        Allocation_Status = 'Allocated',
        Slots_Remaining_After = Daily_Capacity$Slots_Remaining[Capacity_Row]
      )]
    } else {
      Slotting_Allocation[i,Allocation_Status := 'Capacity Row Error']
    }
  } else {
    ## 14.4. Explain why the batch could not be scheduled
    Maximum_Daily_Capacity <- Daily_Capacity[
      Pick_Type == Current_Pick_Type,max(Slot_Capacity,na.rm = TRUE)
    ]
    if (!length(Maximum_Daily_Capacity) || !is.finite(Maximum_Daily_Capacity)) {
      Slotting_Allocation[i,Allocation_Status := 'Capacity Not Configured']
    } else if (Slots_Needed > Maximum_Daily_Capacity) {
      Slotting_Allocation[i,Allocation_Status := 'Exceeds Single Day Capacity']
    } else {
      Slotting_Allocation[
        i,Allocation_Status := 'No Eligible Day Capacity'
      ]
    }
  }
}
setorder(
  Slotting_Allocation,Day_Number,Pick_Type,New_Proirity,
  -Used_Slot,Product,Pick_Batch_ID,na.last = TRUE
)
Weekly_Slotting_Plan <- Slotting_Allocation[Allocation_Status == 'Allocated']
Unallocated_Products <- Slotting_Allocation[
  Allocation_Status != 'Allocated'
][order(New_Proirity,Pick_Type,-Used_Slot)]
Daily_Capacity_Summary <- Daily_Capacity[, .(
  Day_Number,Allocation_Day,Pick_Type,Slot_Capacity,Slots_Used,Slots_Remaining,
  Capacity_Used_Pct = round(Slots_Used/Slot_Capacity*100,1)
)]

###############################################################################
## 15. ADD BATCH PICK DAYS TO IBT_SUMMARY
###############################################################################
Pick_Day_File <- Slotting_Allocation[, .(
  Product,Pick_Batch_ID,
  Pick_Day = fifelse(Allocation_Status == 'Allocated',Allocation_Day,'Unallocated'),
  Pick_Day_Number = Day_Number,Allocation_Status,Slots_Remaining_After
)]
IBT_Summary <- merge(
  IBT_Summary,Pick_Day_File,
  by = c('Product','Pick_Batch_ID'),all.x = TRUE,sort = FALSE
)

###############################################################################
## 16. ASSIGN SPECIFIC WD LOCATIONS TO EACH ALLOCATED BATCH
###############################################################################
Product_Location_Allocation <- copy(
  Slotting_Allocation[Allocation_Status == 'Allocated']
)
Product_Location_Allocation[, Weight_Band_Order := fcase(
  Weight_Band == 'Very Heavy',1L,
  Weight_Band == 'Heavy',2L,
  Weight_Band == 'Medium',3L,
  Weight_Band == 'Light',4L,
  Weight_Band == 'Very Light',5L,
  default = 6L
)]
setorder(
  Product_Location_Allocation,Day_Number,Pick_Type,Weight_Band_Order,
  New_Proirity,-Used_Slot,Product,Pick_Batch_ID
)
Product_Location_Allocation[, `:=`(
  First_Location = NA_character_,Last_Location = NA_character_,
  Assigned_Locations = NA_character_,Locations_Assigned = 0L,
  Location_Status = 'Not Assigned'
)]

for (Current_Day in 1:5) {
  for (Current_Pick_Type in c('Pal','Above_Pal')) {

    ## 16.1. Reset physical locations for this day and pick type
    Available_Locations <- copy(
      WD_Location_Master[Pick_Type == Current_Pick_Type]
    )
    setorder(Available_Locations,Bay_Number_Numeric,Slot_Position)
    Available_Locations[, `:=`(
      Location_Sequence = seq_len(.N),Location_Available = TRUE
    )]
    Product_Rows <- Product_Location_Allocation[
      Day_Number == Current_Day & Pick_Type == Current_Pick_Type,
      which = TRUE
    ]
    if (!length(Product_Rows)) next

    for (Current_Row in Product_Rows) {
      Slots_Needed <- Product_Location_Allocation$Used_Slot[Current_Row]
      if (length(Slots_Needed) != 1L || is.na(Slots_Needed) ||
          !is.finite(Slots_Needed) || Slots_Needed <= 0 ||
          Slots_Needed != floor(Slots_Needed)) {
        Product_Location_Allocation[
          Current_Row,Location_Status := 'Invalid Used_Slot'
        ]
        next
      }
      Slots_Needed <- as.integer(Slots_Needed)
      Selected_Rows <- integer(0)

      ## 16.2. Preserve the original one-slot Above_Pal restriction
      ## The original code rejected Above_Pal requirements greater than one.
      if (Current_Pick_Type == 'Above_Pal') {
        if (Slots_Needed > 1L) {
          Product_Location_Allocation[
            Current_Row,
            Location_Status := 'Requires More Than One Above-Pallet Bay'
          ]
          next
        }
        Free_Rows <- Available_Locations[
          Location_Available == TRUE,which = TRUE
        ]
        if (length(Free_Rows)) Selected_Rows <- Free_Rows[1L]
        if (length(Selected_Rows) != Slots_Needed) {
          Product_Location_Allocation[
            Current_Row,Location_Status := 'No Consecutive Same-Bay Space'
          ]
          next
        }
      } else {
        ## 16.3. PAL locations may span bays when multiple slots are unavoidable
        Free_Rows <- Available_Locations[
          Location_Available == TRUE,which = TRUE
        ]
        if (length(Free_Rows) < Slots_Needed) {
          Product_Location_Allocation[
            Current_Row,Location_Status := 'Insufficient Physical Locations'
          ]
          next
        }
        Selected_Rows <- Free_Rows[seq_len(Slots_Needed)]
      }

      ## 16.4. Save and consume the selected physical locations
      if (length(Selected_Rows) != Slots_Needed) {
        Product_Location_Allocation[
          Current_Row,Location_Status := 'Location Selection Error'
        ]
        next
      }
      Selected_Locations <- Available_Locations[Selected_Rows,Location]
      Available_Locations[Selected_Rows,Location_Available := FALSE]
      Product_Location_Allocation[Current_Row, `:=`(
        First_Location = Selected_Locations[1L],
        Last_Location = Selected_Locations[length(Selected_Locations)],
        Assigned_Locations = paste(Selected_Locations,collapse = ', '),
        Locations_Assigned = as.integer(length(Selected_Locations)),
        Location_Status = 'Assigned'
      )]
    }
  }
}

###############################################################################
## 17. ADD PHYSICAL LOCATIONS USING PRODUCT + BATCH KEYS
###############################################################################
Product_Location_File <- Product_Location_Allocation[, .(
  Product,Pick_Batch_ID,First_Location,Last_Location,
  Assigned_Locations,Locations_Assigned,Location_Status
)]
IBT_Summary <- merge(
  IBT_Summary,Product_Location_File,
  by = c('Product','Pick_Batch_ID'),all.x = TRUE,sort = FALSE
)
IBT_Summary[Pick_Day == 'Unallocated' & is.na(Location_Status), `:=`(
  First_Location = NA_character_,Last_Location = NA_character_,
  Assigned_Locations = 'Unallocated',Locations_Assigned = 0L,
  Location_Status = 'Not Allocated to Day'
)]
IBT_Summary[Pick_Day != 'Unallocated' & is.na(Location_Status),
            Location_Status := 'Missing Location Assignment']

###############################################################################
## 18. VALIDATE LOCATIONS, DAILY CAPACITY AND BATCH SCHEDULING
###############################################################################
Location_Count_Check <- Product_Location_Allocation[
  Location_Status == 'Assigned' & Locations_Assigned != Used_Slot
]
Expanded_Location_Allocation <- Product_Location_Allocation[
  Location_Status == 'Assigned',
  .(Assigned_Location = unlist(strsplit(
    Assigned_Locations,split = ', ',fixed = TRUE
  ))),
  by = .(
    Product,Pick_Batch_ID,Day_Number,Allocation_Day,
    Pick_Type,Weight_Band,New_Proirity
  )
]
Duplicate_Location_Check <- Expanded_Location_Allocation[
  , .N,by = .(Day_Number,Pick_Type,Assigned_Location)
][N > 1L]
Expanded_Location_Allocation[, Assigned_Bay := substr(Assigned_Location,3,4)]
Above_Pal_Same_Bay_Check <- Expanded_Location_Allocation[
  Pick_Type == 'Above_Pal',
  .(
    Number_Of_Bays = uniqueN(Assigned_Bay),
    Assigned_Bays = paste(sort(unique(Assigned_Bay)),collapse = ', '),
    Assigned_Locations = paste(Assigned_Location,collapse = ', ')
  ),
  by = .(Product,Pick_Batch_ID,Day_Number,Allocation_Day)
][Number_Of_Bays > 1L]

## 18.1. No two PAL batches of one product may share a day
PAL_Product_Day_Check <- Slotting_Allocation[
  Allocation_Status == 'Allocated' & Pick_Type == 'Pal',
  .(Batches_On_Day = .N),by = .(Product,Day_Number)
][Batches_On_Day > 1L]

## 18.2. Reconcile scheduled slot totals against daily capacity deductions
Scheduled_Slots <- Slotting_Allocation[
  Allocation_Status == 'Allocated',
  .(Scheduled_Slots = sum(Used_Slot)),by = .(Day_Number,Pick_Type)
]
Capacity_Check <- merge(
  Daily_Capacity,Scheduled_Slots,
  by = c('Day_Number','Pick_Type'),all.x = TRUE
)
Capacity_Check[is.na(Scheduled_Slots),Scheduled_Slots := 0]
stopifnot(
  nrow(Location_Count_Check) == 0L,
  nrow(Duplicate_Location_Check) == 0L,
  nrow(Above_Pal_Same_Bay_Check) == 0L,
  nrow(PAL_Product_Day_Check) == 0L,
  all(Daily_Capacity$Slots_Remaining >= 0),
  all(Capacity_Check$Slots_Used == Capacity_Check$Scheduled_Slots),
  all(Capacity_Check$Slots_Used+Capacity_Check$Slots_Remaining ==
        Capacity_Check$Slot_Capacity),
  !anyDuplicated(IBT_Summary,by = c('Product','Pick_Batch_ID'))
)

###############################################################################
## 19. CREATE PRODUCT / BATCH LOCATION ALLOCATION OUTPUT
###############################################################################
Product_Allocation_File <- merge(
  Slotting_Allocation[, .(
    Product,Pick_Batch_ID,
    Pick_Day = fifelse(
      Allocation_Status == 'Allocated',Allocation_Day,'Unallocated'
    ),
    Pick_Day_Number = Day_Number,Allocation_Status,
    Pick_Type,Used_Slot,Total_Qty,Pallet_Qty,Slots_Remaining_After,
    DC_Count,Requesting_DCs
  )],
  Product_Location_File,
  by = c('Product','Pick_Batch_ID'),all.x = TRUE,sort = FALSE
)
Product_Allocation_File[, STD_Qty := fifelse(
  Pick_Type == 'Above_Pal',Total_Qty,Pallet_Qty*Used_Slot
)]
setorder(
  Product_Allocation_File,Pick_Day_Number,First_Location,
  Product,Pick_Batch_ID,na.last = TRUE
)

###############################################################################
## 20. ADD BATCH PICK DAYS BACK TO INDIVIDUAL SHELF REQUESTS
###############################################################################
IBT_Final <- merge(
  IBT_Shelf_File,
  IBT_Summary[, .(
    Product,Pick_Batch_ID,Pick_Day,Pick_Day_Number,
    Allocation_Status,Pick_Type,Used_Slot
  )],
  by = c('Product','Pick_Batch_ID'),all.x = TRUE,sort = FALSE
)
## One output row per original shelf-path request; no batch-join multiplication.
stopifnot(
  nrow(IBT_Final) == nrow(IBT_Shelf_File),
  !anyDuplicated(IBT_Final$IBT_Request_ID),
  setequal(IBT_Final$IBT_Request_ID,IBT_Shelf_File$IBT_Request_ID),
  sum(IBT_Final$Qty_To_Process) == sum(IBT_Shelf_File$Qty_To_Process),
  !anyNA(IBT_Final$Pick_Day)
)

###############################################################################
## 21. CREATE PUNCHING GROUP NUMBERS
###############################################################################
## Preserve receiving DC + pick day + newest zone grouping.
setorder(IBT_Final,DC,Pick_Day_Number,Newest_Zone,na.last = TRUE)
IBT_Final[, Punching_Group := .GRP,by = .(DC,Pick_Day,Newest_Zone)]

###############################################################################
## 22. CREATE A DC / BATCH / DAY AUDIT FILE
###############################################################################
IBT_DC_Batch_Plan <- merge(
  DC_Batch_Demand,
  IBT_Summary[, .(
    Product,Pick_Batch_ID,Pick_Day,Pick_Day_Number,
    Pick_Type,Used_Slot,Batch_Total_Qty = Total_Qty,
    Pallet_Qty,Allocation_Status,Assigned_Locations,Location_Status
  )],
  by = c('Product','Pick_Batch_ID'),all.x = TRUE,sort = FALSE
)
## Used_Slot and Batch_Total_Qty repeat for each DC in a batch.
## Do not sum those repeated columns across DC rows; sum DC_Qty instead.
setorder(
  IBT_DC_Batch_Plan,Pick_Day_Number,Product,Pick_Batch_ID,DC,na.last = TRUE
)

###############################################################################
## 23. EXPORT EXISTING OUTPUTS AND NEW PLANNING / AUDIT OUTPUTS
###############################################################################
fwrite(IBT_Allocation_Summary,file.path(
  Output_Directory,'IBT_Allocation_Summary.csv'
))
fwrite(IBT_Pallet_Pick_Detail,file.path(
  Output_Directory,'IBT_Pallet_Pick_Detail.csv'
))
fwrite(IBT_Allocation_Exceptions,file.path(
  Output_Directory,'IBT_Allocation_Exceptions.csv'
))
fwrite(IBT_Rack_Audit,file.path(Output_Directory,'IBT_Rack_Audit.csv'))
write.csv(IBT_Final,file.path(
  Output_Directory,'IBT_Final.csv'
),row.names = FALSE)
write.csv(Product_Allocation_File,file.path(
  Output_Directory,'IBT_Product_Allocation_File.csv'
),row.names = FALSE)
fwrite(IBT_Direct_Pallet_File,file.path(
  Output_Directory,'IBT_Direct_Pallet_File.csv'
))
fwrite(IBT_DC_Batch_Plan,file.path(Output_Directory,'IBT_DC_Batch_Plan.csv'))
fwrite(Daily_Capacity_Summary,file.path(
  Output_Directory,'IBT_Daily_Capacity_Summary.csv'
))
fwrite(Unallocated_Products,file.path(
  Output_Directory,'IBT_Unallocated_Products.csv'
))

###############################################################################
## 24. DISPLAY RUN SUMMARIES
###############################################################################
print(IBT_Allocation_Summary[, .N,by = Allocation_Status][order(-N)])
print(Daily_Capacity_Summary)
print(Weekly_Slotting_Plan[, .(
  Allocation_Day,Product,Pick_Batch_ID,Weight_Band,Pick_Type,
  New_Proirity,Used_Slot,Slots_Remaining_After,Total_Qty,
  Lines,DC_Count,Requesting_DCs
)])
print(IBT_Summary[Pick_Type == 'Pal', .(
  Batches = .N,
  Single_Slot_Batches = sum(Used_Slot == 1,na.rm = TRUE),
  Multi_Slot_Batches = sum(Used_Slot > 1,na.rm = TRUE),
  Total_Qty = sum(Total_Qty)
)])
print(Location_Count_Check)
print(Duplicate_Location_Check)
print(Above_Pal_Same_Bay_Check)
print(PAL_Product_Day_Check)
message('IBT planning outputs written to: ',Output_Directory)
