
rm(list = ls())
gc()

t1 <- Sys.time()

setwd(
  "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/"
)

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


# ============================================================
# Password reader
# ============================================================

read_password <- function(path) {
  
  if (!file.exists(path)) {
    stop(
      "Password file not found: ",
      path,
      call. = FALSE
    )
  }
  
  pwd <- trimws(
    readLines(
      path,
      n = 1L,
      warn = FALSE
    )
  )
  
  if (!nzchar(pwd)) {
    stop(
      "The password file is empty: ",
      path,
      call. = FALSE
    )
  }
  
  pwd
}


password_file <- paste0(
  "C:/Users/harriesr/",
  "credentials/azure_supply_chain.txt"
)


# ============================================================
# RODBC connection function
# ============================================================

connection_function <- function() {
  
  pwd <- read_password(password_file)
  
  rodbc_con <- RODBC::odbcConnect(
    dsn = "Azure Supply Chain",
    uid = "Harriesr",
    pwd = pwd
  )
  
  rm(pwd)
  
  if (identical(rodbc_con, -1)) {
    stop(
      "RODBC database connection failed.",
      call. = FALSE
    )
  }
  
  rodbc_con
}


# ============================================================
# Extract A-Frame lookup data
# ============================================================

rodbc_con <- NULL

A_Frame_OS_Query <- tryCatch(
  
  expr = {
    
    rodbc_con <- connection_function()
    
    query_result <- RODBC::sqlQuery(
      channel = rodbc_con,
      query = "
        SELECT
            [Channel_Type],
            SUBSTRING(AFrame_Sector, 3, 3) AS Sector
        FROM [AAHOperations].[dbo].[lu_ws_aframe_long_short]
        WHERE Branch_ID = '305M'
      ",
      stringsAsFactors = FALSE
    )
    
    # sqlQuery returns character messages when SQL fails
    if (is.character(query_result)) {
      stop(
        paste(
          "A-Frame lookup query failed:",
          paste(query_result, collapse = "\n"),
          sep = "\n"
        ),
        call. = FALSE
      )
    }
    
    as.data.table(query_result)
  },
  
  error = function(e) {
    
    stop(
      paste0(
        "Unable to retrieve the A-Frame lookup data.\n",
        "Reason: ",
        conditionMessage(e)
      ),
      call. = FALSE
    )
  },
  
  finally = {
    
    if (
      !is.null(rodbc_con) &&
      !identical(rodbc_con, -1)
    ) {
      try(
        RODBC::odbcClose(rodbc_con),
        silent = TRUE
      )
    }
  }
)

rm(rodbc_con)


# ============================================================
# Import and prepare A-Frame statistics
# ============================================================

Knapp_AFrame_Stats <- fread(
  paste0(
    "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/",
    "R Coding/txt files/Knapp Picking/158595.csv"
  )
)

Knapp_AFrame_Stats_1 <- merge(
  x = Knapp_AFrame_Stats,
  y = A_Frame_OS_Query,
  by.x = "loc_cbs_sector",
  by.y = "Sector",
  all.x = TRUE
)

setDT(Knapp_AFrame_Stats_1)

Knapp_AFrame_Stats_1[
  ,
  Overstock_loc := substring(loc_location_no, 1, 5)
][
  ,
  Min_Dim := pmin(
    art_pack_unit_height,
    art_pack_unit_length,
    art_pack_unit_width,
    na.rm = TRUE
  )
][
  ,
  Channel_STD := fifelse(
    Channel_Type == "Long",
    floor(2450 / Min_Dim),
    floor(1350 / Min_Dim)
  )
]


# Replace missing numeric values with zero

numeric_cols <- names(Knapp_AFrame_Stats_1)[
  vapply(
    Knapp_AFrame_Stats_1,
    is.numeric,
    logical(1)
  )
]

Knapp_AFrame_Stats_1[
  ,
  (numeric_cols) := lapply(
    .SD,
    function(x) {
      x[!is.finite(x)] <- NA_real_
      x[is.na(x)] <- 0
      as.integer(round(x))
    }
  ),
  .SDcols = numeric_cols
]


# ============================================================
# Import and prepare Knapp Picking
# ============================================================

Knapp_Picking <- fread(
  paste0(
    "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/",
    "R Coding/txt files/Knapp Picking/215418.csv"
  )
)

setnames(
  Knapp_Picking,
  
  old = c(
    "print_branch",
    "processing_date_ts",
    "user_no",
    "product_code",
    "pick_note_number",
    "line_quantity",
    "discrepancy",
    "tote_no",
    "start_time_v2",
    "end_time_v2",
    "station",
    "location_no",
    "invoice_line_number",
    "order_type",
    "event_time",
    "pick_flag",
    "account_branch",
    "account_number",
    "process_duration",
    "order_id",
    "load_unit_id"
  ),
  
  new = c(
    "Print_Branch",
    "Processing_Date",
    "User_ID",
    "Product_Code",
    "Pick_Note_Number",
    "Line_Quantity",
    "Discrepancy",
    "Tote",
    "Start_Time",
    "End_Time",
    "Station",
    "Location",
    "Invoice_Line_Number",
    "Rec_Type",
    "Event_Time",
    "Pick_Flag",
    "Account_Branch_Int",
    "Account_Number",
    "Duration",
    "Order_ID",
    "Load_Unit_ID"
  )
)


# Remove unused columns only if they exist

remove_cols <- intersect(
  c("order_type_orig", "order_type_id"),
  names(Knapp_Picking)
)

if (length(remove_cols)) {
  Knapp_Picking[, (remove_cols) := NULL]
}


Knapp_Picking[, Rec_Type := "PICK"]

Knapp_Picking[
  Discrepancy > 0,
  Pick_Flag := "S"
][
  Discrepancy == 0,
  Pick_Flag := "P"
]


# Assign account branch with a lookup table

branch_lookup <- c(
  "106" = "106M",
  "201" = "201D",
  "305" = "305M",
  "405" = "405N",
  "406" = "406P",
  "501" = "501G",
  "504" = "504M",
  "606" = "606R",
  "602" = "602J",
  "703" = "703M",
  "706" = "706S",
  "908" = "908B"
)

Knapp_Picking[
  ,
  Account_Branch := unname(
    branch_lookup[as.character(Account_Branch_Int)]
  )
]


# ============================================================
# Import and prepare throughput data
# ============================================================

Troughputs <- fread(
  paste0(
    "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/",
    "R Coding/txt files/Knapp Picking/160924.csv"
  ),
  colClasses = c(
    time_hour_iso = "character"
  )
)

Troughputs_L <- fread(
  paste0(
    "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/",
    "R Coding/txt files/Knapp Picking/258461.csv"
  ),
  colClasses = c(
    time_hour_iso = "character"
  )
)

Troughputs <- merge(
  x = Troughputs,
  y = Troughputs_L,
  by = c(
    "calendar_date",
    "time_hour_iso"
  ),
  all.x = TRUE
)


# Preserve original timestamp for auditing

Troughputs[
  ,
  time_hour_iso_original := time_hour_iso
]


# Remove ISO timezone suffix and preserve the displayed clock time

Troughputs[
  ,
  time_hour_iso := lubridate::ymd_hms(
    sub(
      pattern = "(Z|[+-][0-9]{2}:[0-9]{2})$",
      replacement = "",
      x = time_hour_iso
    ),
    tz = "Europe/London",
    quiet = TRUE
  )
]


# Add reporting fields

Troughputs[
  ,
  `:=`(
    Print_Branch = "305M",
    Target = 1300L,
    Hr = lubridate::hour(time_hour_iso)
  )
]


# Assign shifts

Troughputs[
  ,
  Shift := fcase(
    Hr %between% c(1L, 5L),
    "Night",
    
    Hr %between% c(6L, 10L),
    "Early_Day_Shift",
    
    Hr %between% c(11L, 13L),
    "Day_Shift",
    
    Hr == 0L | Hr >= 14L,
    "Back_Shift",
    
    default = NA_character_
  )
]


# ============================================================
# Preserve wall-clock timestamps when uploading
# ============================================================

freeze_as_utc <- function(dt) {
  
  datetime_cols <- names(dt)[
    vapply(
      dt,
      function(x) inherits(x, "POSIXt"),
      logical(1)
    )
  ]
  
  if (length(datetime_cols)) {
    
    dt[
      ,
      (datetime_cols) := lapply(
        .SD,
        lubridate::force_tz,
        tzone = "UTC"
      ),
      .SDcols = datetime_cols
    ]
  }
  
  invisible(dt)
}


freeze_as_utc(Knapp_Picking)
freeze_as_utc(Troughputs)
freeze_as_utc(Knapp_AFrame_Stats_1)


# ============================================================
# DBI connection and database upload
# ============================================================

azure_password <- read_password(password_file)

dbi_con <- NULL

tryCatch(
  
  expr = {
    
    dbi_con <- DBI::dbConnect(
      odbc::odbc(),
      Driver = "ODBC Driver 17 for SQL Server",
      Server = paste0(
        "sqldwhscuks0001.ba8e81fcf26b.",
        "database.windows.net"
      ),
      Database = "AAHOperations",
      UID = "Harriesr",
      PWD = azure_password,
      Port = 1433
    )
    
    # Remove password after successfully opening the connection
    rm(azure_password)
    
    message("Connected to the database successfully.")
    
    DBI::dbWithTransaction(
      dbi_con,
      {
        
        # ----------------------------------------------------
        # Knapp Picking
        # ----------------------------------------------------
        
        DBI::dbWriteTable(
          conn = dbi_con,
          name = DBI::SQL(
            "AAHOperations.dbo.Knapp_Picking"
          ),
          value = Knapp_Picking,
          overwrite = TRUE,
          row.names = FALSE
        )
        
        message(
          "Knapp_Picking uploaded: ",
          format(nrow(Knapp_Picking), big.mark = ","),
          " rows."
        )
        
        
        # ----------------------------------------------------
        # Tote Throughputs
        # ----------------------------------------------------
        
        DBI::dbWriteTable(
          conn = dbi_con,
          name = DBI::SQL(
            "AAHOperations.dbo.Knapp_Tote_Throughputs"
          ),
          value = Troughputs,
          overwrite = TRUE,
          row.names = FALSE
        )
        
        message(
          "Knapp_Tote_Throughputs uploaded: ",
          format(nrow(Troughputs), big.mark = ","),
          " rows."
        )
        
        
        # ----------------------------------------------------
        # A-Frame Channel Statistics
        # ----------------------------------------------------
        
        DBI::dbExecute(
          dbi_con,
          paste0(
            "DELETE FROM ",
            "[AAHOperations].[dbo].",
            "[Knapp_AFrame_Channel_Stats_v1]"
          )
        )
        
        DBI::dbWriteTable(
          conn = dbi_con,
          name = DBI::SQL(
            paste0(
              "AAHOperations.dbo.",
              "Knapp_AFrame_Channel_Stats_v1"
            )
          ),
          value = Knapp_AFrame_Stats_1,
          append = TRUE,
          row.names = FALSE
        )
        
        message(
          "Knapp_AFrame_Channel_Stats_v1 uploaded: ",
          format(
            nrow(Knapp_AFrame_Stats_1),
            big.mark = ","
          ),
          " rows."
        )
      }
    )
    
    message("Database transaction completed successfully.")
  },
  
  error = function(e) {
    
    message(
      paste0(
        "\nDatabase upload failed.\n",
        "The transaction has been rolled back.\n",
        "Reason: ",
        conditionMessage(e)
      )
    )
    
    stop(
      "Script stopped because the database upload failed.",
      call. = FALSE
    )
  },
  
  finally = {
    
    # Remove password if connection creation failed
    if (exists("azure_password", inherits = FALSE)) {
      rm(azure_password)
    }
    
    # Always close the DBI connection
    if (
      !is.null(dbi_con) &&
      DBI::dbIsValid(dbi_con)
    ) {
      
      try(
        DBI::dbDisconnect(dbi_con),
        silent = TRUE
      )
      
      message("Database connection closed.")
    }
  }
)


# ============================================================
# Completion time
# ============================================================

t2 <- Sys.time()

message(
  "Total run time: ",
  round(
    as.numeric(
      difftime(
        t2,
        t1,
        units = "mins"
      )
    ),
    2
  ),
  " minutes."
)
















































##################

DBI::dbWithTransaction(con, {
  
  # Knapp Picking
  DBI::dbWriteTable(
    con,
    name = SQL("AAHOperations.dbo.Knapp_Picking"),
    value = Knapp_Picking,
    overwrite = TRUE,
    row.names = FALSE
  )
  
  # Tote Throughputs
  DBI::dbWriteTable(
    con,
    name = SQL("AAHOperations.dbo.Knapp_Tote_Throughputs"),
    value = Troughputs,
    overwrite = TRUE,
    row.names = FALSE
  )
 
## Tote Channel_Stats
  DBI::dbWriteTable(
    con,
    name = SQL("AAHOperations.dbo.Knapp_AFrame_Channel_Stats_v1"),
    value = Knapp_AFrame_Stats_1,
##    overwrite = TRUE,
    append = TRUE,
    row.names = FALSE
  )
##   
})















####################
Epod_LU_Failed_Reason_Codes <- fread("C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/EPOD/Epod_LU_Failed_Reason_Codes.csv")
## -----------------------------
con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "ODBC Driver 17 for SQL Server",
                      Server   = "sqldwhscuks0001.ba8e81fcf26b.database.windows.net",
                      Database = "AAHOperations",
                      UID      = "Harriesr",
                      PWD      = "G00ners4every=+",
                      Port     = 1433)

on.exit(DBI::dbDisconnect(con), add = TRUE)
DBI::dbWithTransaction(con, {
  DBI::dbWriteTable(con, name = SQL("AAHOperations.dbo.epod_failed_reason_codes_lu"),
                    value = Epod_LU_Failed_Reason_Codes, overwrite = TRUE, row.names = FALSE)
})

####################
Segmenation <- fread("C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/SQL Uploads/Segmentsv3.csv")
## -----------------------------
con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "ODBC Driver 17 for SQL Server",
                      Server   = "sqldwhscuks0001.ba8e81fcf26b.database.windows.net",
                      Database = "AAHOperations",
                      UID      = "Harriesr",
                      PWD      = "G00ners4everyeE",
                      Port     = 1433)

on.exit(DBI::dbDisconnect(con), add = TRUE)
DBI::dbWithTransaction(con, {
  DBI::dbWriteTable(con, name = SQL("AAHOperations.dbo.Customer_Segmentation_lu"),
                    value = Segmenation, overwrite = TRUE, row.names = FALSE)
})
#################
A_Frame_Chan_Query = sqlQuery(con, paste("SELECT al.Print_Branch
      ,pl.Product_Code
      ,al.Location_Code
	  , SUBSTRING(al.Location_Code,3,3) as Sector
	  , convert(int,SUBSTRING(al.Location_Code,7,4)) as Channel
	  , ls.AFrame_Number
	  , ls.Channel_Type
	  , ls.Aframe_Type
      ,al.Assembly_Qty
      ,al.Maximum_Qty
      ,al.Sequence
      ,convert(date,[Update_Timestamp]) as Import_Date
	  ,DATEADD(DAY, -1, CONVERT(date, [Update_Timestamp])) AS Pick_Date
  FROM [SupplyChainReporting].[dbo].[aah_location] al
  INNER JOIN SupplyChainReporting.dbo.product_list pl on al.Prod_Key_C = pl.Prod_Key_C
  INNER JOIN AAHoperations.dbo.lu_ws_aframe_long_short ls on left(al.Location_Code,5) = ls.AFrame_Sector and al.Print_Branch = ls.Branch_ID
  Where Manual_Auto = 'A' and Print_Branch = '305M' and Update_Timestamp >= DATEADD(DAY,-65, getdate()) 
  ORDER BY convert(date,[Update_Timestamp]) DESC"))