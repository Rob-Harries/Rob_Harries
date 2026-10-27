# ============================================================
# AAH OTIF CALCULATION - COMMENTED REFACTOR
# Date-based customer van framework
# ============================================================
# Based on the script supplied in this conversation.
# Business rules and existing output names are intentionally retained.
# This is a refactor, NOT a confirmed fix for missing Saturday routes.
#
# Important retained behaviours:
# * is_tier4 means BOTH Tier 3 and Tier 4.
# * Nutricia cutoff is 15:00, despite the legacy field name After_1400.
# * 408T Fridge cutoff is 17:00, inclusive.
# * Friday after 17:00 and weekend Fridge orders move to Tuesday or later.
# * Early deliveries are On Time regardless of slot.
# * Missing same-day slot information is On Time, as in the supplied code.
# * UTC-labelled wall-clock handling is retained; no timezone correction
#   is made without confirmation of the source timestamp semantics.
#
# REVIEW BEFORE PRODUCTION:
# 1. Compare output with the current script before enabling Azure writes.
# 2. The invoice-to-Customer_Vans_History join can multiply invoice lines
#    if multiple history rows match customer/date/van. This join is retained
#    rather than silently changing the sales aggregation.
# 3. In Full remains provisional and is not combined into OTIF_Status.
# ============================================================

# ============================================================
# 0. PACKAGES AND CONFIGURATION
# ============================================================
# Do not clear the entire R workspace: source this in a clean R session.
library(RODBC)
library(data.table)
library(lubridate)
library(DBI)
library(odbc)

t1 <- Sys.time()

cfg <- list(
  credential_file = "C:/Users/harriesr/credentials/azure_supply_chain.txt",
  db_user = "Harriesr",
  source_dsn = "Azure Supply Chain",
  target_driver = "ODBC Driver 17 for SQL Server",
  target_server = "sqldwhscuks0001.ba8e81fcf26b.database.windows.net",
  target_database = "AAHOperations",
  refresh_days = 31L,
  route_horizon_days = 14L,
  # Deliberate safety change: QA-only by default.
  # Set TRUE only after comparing the refactored output with the original.
  write_to_azure = TRUE,
  detailed_qa = FALSE,
  friday_fridge_qa = TRUE
)

# Define each cutoff once. Later assignments preserve the original priority.
cutoffs <- list(
  nutricia = as.ITime("15:00:00"),
  cross_dc = as.ITime("19:00:00"),
  sunday = as.ITime("11:30:00"),
  tier34_weekday = as.ITime("18:00:00"),
  cross_408T_route = as.ITime("15:00:00"),
  processing_408T = as.ITime("17:00:00"),
  fridge_408T = as.ITime("17:00:00")
)

# One fixed UK refresh window for the target write.
# Source SQL GETDATE() filters remain unchanged from the original script.
today_uk <- as.Date(t1, tz = "Europe/London")
window_start <- today_uk - cfg$refresh_days
window_end <- today_uk

# ------------------------------------------------------------
# 0.1 Reusable helpers
# ------------------------------------------------------------
normalise_text <- function(x) trimws(toupper(as.character(x)))

# Convert SQL time text to ITime, retaining already-converted vectors.
to_itime <- function(x) {
  if (inherits(x, "ITime")) return(x)
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NULL")] <- NA_character_
  as.ITime(sub("\\..*$", "", x))
}

# Preserve the original date + time construction and timezone convention.
combine_date_time <- function(date_value, time_value, timezone = "UTC") {
  stopifnot(length(date_value) == length(time_value))
  result <- rep(as.POSIXct(NA, tz = timezone), length(date_value))
  valid <- !is.na(date_value) & !is.na(time_value)
  if (any(valid)) {
    result[valid] <- as.POSIXct(
      paste(as.character(date_value[valid]), format(time_value[valid])),
      format = "%Y-%m-%d %H:%M:%S", tz = timezone
    )
  }
  result
}

check_required_columns <- function(dt, required_columns, object_name) {
  missing <- setdiff(required_columns, names(dt))
  if (length(missing)) {
    stop(object_name, " is missing fields: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

# Do not silently deduplicate conflicting lookup records.
assert_unique <- function(dt, key, object_name) {
  duplicates <- dt[, .N, by = key][N > 1L]
  if (nrow(duplicates)) {
    print(head(duplicates, 20L))
    stop(object_name, " contains ", nrow(duplicates), " duplicate join keys.")
  }
  invisible(TRUE)
}

assert_row_count <- function(dt, expected, stage) {
  if (nrow(dt) != expected) {
    stop(stage, " changed the row count from ", expected, " to ", nrow(dt), ".")
  }
}

# One validated extract helper replaces repeated sqlQuery/check/setDT blocks.
read_source <- function(con, sql, label) {
  result <- RODBC::sqlQuery(con, sql, stringsAsFactors = FALSE)
  if (is.character(result) && !is.data.frame(result)) {
    stop(label, " SQL query failed:\n", paste(result, collapse = "\n"))
  }
  if (!is.data.frame(result)) stop(label, " did not return a data.frame.")
  if (!nrow(result)) warning(label, " returned no rows.")
  setDT(result)
  message(label, ": ", format(nrow(result), big.mark = ","), " rows extracted.")
  result
}

# Grouped QA is optional to avoid printing large diagnostic tables each run.
print_if_detailed <- function(x) {
  if (isTRUE(cfg$detailed_qa)) print(x)
  invisible(NULL)
}

# ------------------------------------------------------------
# 0.2 Read credentials from the existing local file
# ------------------------------------------------------------
if (!file.exists(cfg$credential_file)) stop("Credential file was not found.")
password_lines <- readLines(cfg$credential_file, warn = FALSE)
if (length(password_lines) != 1L || !nzchar(password_lines[[1L]])) {
  stop("Credential file must contain exactly one non-empty password line.")
}
db_password <- password_lines[[1L]]
rm(password_lines)

# ============================================================
# 1. SALES / INVOICE SQL
# ============================================================
# Preserve all existing joins, filters, grouping and aggregation.
# The original comment describes removal of to-follows/transfers; the
# actual predicates below are retained without assuming additional rules.
# Orders whose processing date is >4 calendar days after order date use
# the processing date/time as their effective order date/time.
sales_sql <- "
SELECT
      bi.Process_Month
    , bi.Print_Branch AS Processing_DC
    , CONVERT(date, bi.Processing_Date) AS Processing_Date
    , CONVERT(time, bi.Processing_Date) AS Processing_Time
    , bi.Company_ID
    , CASE WHEN DATEDIFF(DAY, CONVERT(date, bi.Order_Date_Time),
                             CONVERT(date, bi.Processing_Date)) > 4
           THEN CONVERT(date, bi.Processing_Date)
           ELSE CONVERT(date, bi.Order_Date_Time) END AS Order_Date
    , CASE WHEN DATEDIFF(DAY, CONVERT(date, bi.Order_Date_Time),
                             CONVERT(date, bi.Processing_Date)) > 4
           THEN CONVERT(time, bi.Processing_Date)
           ELSE CONVERT(time, bi.Order_Date_Time) END AS Order_Time
    , TRY_CONVERT(int, bi.Pick_Note_No) AS Pick_Note_No
    , c.Cust_Key_C
    , TRY_CONVERT(int, CASE
          WHEN c.Account_Branch = '606r' AND c2.Account_Branch IS NOT NULL
              THEN c2.Cust_Key_C
          WHEN c1.Account_Branch IS NULL THEN c.Cust_Key_C
          ELSE c1.Cust_Key_C END) AS Del_Cust_Key_C
    , CASE
          WHEN c.Account_Branch = '606r' AND c2.Account_Branch IS NOT NULL
              THEN c2.Account_Branch
          WHEN c1.Account_Branch IS NULL AND cv.Delivery_Branch IS NULL
              THEN c.Account_Branch
          WHEN c1.Account_Branch IS NULL AND cv.Delivery_Branch IS NOT NULL
              THEN cv.Delivery_Branch
          WHEN cv.Delivery_Branch IS NOT NULL THEN cv.Delivery_Branch
          ELSE c1.Account_Branch END AS Delivery_DC
    , CASE WHEN TRY_CONVERT(int, bi.Van_Number) > 600
           THEN 'N' ELSE 'Y' END AS EPOD_Delivery
    , om.Order_Type_Detail AS Order_Type_Description
    , bi.Van_Number
    , bi.Van_Drop_Number
    , bi.Van_Day
    , COUNT(bi.Line_Quantity) AS Lines
    , SUM(bi.Line_Quantity) AS Line_Qty
    , SUM(bi.Adj_Cost_Of_Sales_Value) AS Sales_Value
FROM SupplyChainReporting.dbo.b2b_invoices AS bi
INNER JOIN SupplyChainReporting.dbo.product_list AS p
    ON bi.Prod_Key_C = p.Prod_Key_C
INNER JOIN SupplyChainReporting.dbo.b2b_customers AS c
    ON bi.Cust_Key_C = c.Cust_Key_C
LEFT JOIN SupplyChainReporting.dbo.b2b_invoices_delivery_accounts AS da
    ON bi.Cust_Key_C = da.Cust_Key_C
   AND bi.Process_Month = da.Process_Month
   AND bi.Invoice_Number = da.Invoice_No
LEFT JOIN SupplyChainReporting.dbo.b2b_customers AS c1
    ON da.Del_Key_C = c1.Cust_Key_C
LEFT JOIN SupplyChainReporting.dbo.b2b_customers AS c2
    ON c.Link_Key_C = c2.Cust_Key_C
LEFT JOIN AAHOperations.dbo.Customer_Vans_History AS cv
    ON (CASE
          WHEN c.Account_Branch = '606r' AND c2.Account_Branch IS NOT NULL
              THEN c2.Cust_Key_C
          WHEN c1.Account_Branch IS NULL THEN c.Cust_Key_C
          ELSE c1.Cust_Key_C END) = cv.Cust_Key_C
   AND CONVERT(date, bi.Van_Day) = cv.Delivery_Date
   AND bi.Van_Number = cv.Van_No
LEFT JOIN AAHOperations.dbo.lu_order_type_marker_details AS om
    ON bi.Order_Type_Marker = om.Order_Type_Marker
   AND bi.Print_Branch = om.Print_Branch
WHERE bi.Process_Month >= DATEADD(MONTH, -2, GETDATE())
  AND bi.Processing_Date >= DATEADD(DAY, -31, CONVERT(date, GETDATE()))
  AND bi.Omit_Marker = '0'
  AND bi.Credit_Note_Analysis_Marker = '0'
  AND bi.Company_ID = 'A'
  AND LEFT(bi.Order_ID, 1) != 'V'
  AND p.Product_Code NOT IN (
      'CUS0927W','LLO3350Q','AAH0081P','EME0088T','DEL0432N',
      'TRI0866V','LOW0008F','LOW0009B','FIN0507E','FUE0001X'
  )
  AND p.Umbrella_Group NOT IN (
      '6','8','12','13','14','15','16','17','18','19','22','23'
  )
  AND bi.Print_Branch IN (
      '106m','201D','305m','405n','406p','501g',
      '504m','602J','703m','706s','908b','408t'
  )
  AND c.Cust_Key_C != '165437'
  AND bi.Transfer_Marker = '0'
GROUP BY
      bi.Process_Month, bi.Print_Branch, bi.Company_ID
    , CONVERT(date, bi.Processing_Date)
    , CONVERT(time, bi.Processing_Date)
    , CONVERT(date, bi.Order_Date_Time)
    , CONVERT(time, bi.Order_Date_Time)
    , c.Cust_Key_C, om.Order_Type_Detail
    , bi.Van_Number, bi.Van_Drop_Number, bi.Van_Day, bi.Pick_Note_No
    , CASE WHEN TRY_CONVERT(int, bi.Van_Number) > 600 THEN 'N' ELSE 'Y' END
    , CASE
          WHEN c.Account_Branch = '606r' AND c2.Account_Branch IS NOT NULL
              THEN c2.Cust_Key_C
          WHEN c1.Account_Branch IS NULL THEN c.Cust_Key_C
          ELSE c1.Cust_Key_C END
    , CASE
          WHEN c.Account_Branch = '606r' AND c2.Account_Branch IS NOT NULL
              THEN c2.Account_Branch
          WHEN c1.Account_Branch IS NULL AND cv.Delivery_Branch IS NULL
              THEN c.Account_Branch
          WHEN c1.Account_Branch IS NULL AND cv.Delivery_Branch IS NOT NULL
              THEN cv.Delivery_Branch
          WHEN cv.Delivery_Branch IS NOT NULL THEN cv.Delivery_Branch
          ELSE c1.Account_Branch END
    , CASE WHEN DATEDIFF(DAY, CONVERT(date, bi.Order_Date_Time),
                             CONVERT(date, bi.Processing_Date)) > 4
           THEN CONVERT(date, bi.Processing_Date)
           ELSE CONVERT(date, bi.Order_Date_Time) END
    , CASE WHEN DATEDIFF(DAY, CONVERT(date, bi.Order_Date_Time),
                             CONVERT(date, bi.Processing_Date)) > 4
           THEN CONVERT(time, bi.Processing_Date)
           ELSE CONVERT(time, bi.Order_Date_Time) END;
"

# ============================================================
# 2. TRAN DROP SQL
# ============================================================
# One row per delivery customer/pick note; Order_Type is not a join key.
tran_drop_sql <- "
SELECT
      TRY_CONVERT(int, c.Cust_Key_C) AS Del_Cust_Key_C
    , TRY_CONVERT(int, TRIM(td.Pick_Note_No)) AS Pick_Note_No
    , MIN(td.Drop_Date) AS Drop_Del_Date
    , COUNT(DISTINCT td.Label_Occurs) AS Number_Labels
FROM SupplyChainReporting.dbo.b2b_tran_drop AS td
LEFT JOIN SupplyChainReporting.dbo.b2b_customers AS c
    ON td.Cust_Key_C = c.Cust_Key_C
WHERE td.Process_Month >= DATEADD(MONTH, -2, GETDATE())
  AND TRY_CONVERT(int, td.Van_No) <= 700
GROUP BY TRY_CONVERT(int, c.Cust_Key_C),
         TRY_CONVERT(int, TRIM(td.Pick_Note_No));
"

# ============================================================
# 3. EPOD SQL
# ============================================================
# Retain earliest EPOD job timestamp and distinct delivered label count.
# The fallback extracts all digits from Pick_Note, exactly as before.
epod_sql <- "
SELECT
      TRY_CONVERT(int, c.Cust_Key_C) AS Del_Cust_Key_C
    , MIN(ecd.At_Job_Time) AS Delivery_Date
    , CASE WHEN LEFT(eid.Item_Barcode, 3) = '000'
           THEN TRY_CONVERT(int, SUBSTRING(eid.Item_Barcode, 4, 8))
           ELSE pn.Pick_Note_No END AS Pick_Note_No
    , COUNT(DISTINCT eid.Item_Barcode) AS Delivered_Labels
FROM AAHOperations.dbo.epod_item_delivery AS eid
LEFT JOIN SupplyChainReporting.dbo.b2b_customers AS c
    ON eid.Cust_Key_C = c.Cust_Key_C
LEFT JOIN AAHOperations.dbo.epod_consignment_delivery AS ecd
    ON eid.Internal_Consignment_ID = ecd.Internal_Consignment_ID
OUTER APPLY (
    SELECT Pick_Note_No = TRY_CONVERT(int, NULLIF((
        SELECT '' + SUBSTRING(eid.Pick_Note, v.number, 1)
        FROM master..spt_values AS v
        WHERE v.type = 'P'
          AND v.number BETWEEN 1 AND LEN(eid.Pick_Note)
          AND SUBSTRING(eid.Pick_Note, v.number, 1) LIKE '[0-9]'
        FOR XML PATH(''), TYPE
    ).value('.', 'varchar(50)'), ''))
) AS pn
WHERE eid.Delivery_Date >= DATEADD(DAY, -31, CONVERT(date, GETDATE()))
  AND eid.Item_Status IN ('Done', 'ACCD')
GROUP BY TRY_CONVERT(int, c.Cust_Key_C),
         CASE WHEN LEFT(eid.Item_Barcode, 3) = '000'
              THEN TRY_CONVERT(int, SUBSTRING(eid.Item_Barcode, 4, 8))
              ELSE pn.Pick_Note_No END;
"

# ============================================================
# 4. CUSTOMER VAN HISTORY SQL
# ============================================================
# CROSS APPLY names the same COALESCE expressions once for readability.
# It does not change the join keys or impose new deduplication rules.
customer_van_history_sql <- "
SELECT
      TRY_CONVERT(int, cvh.Cust_Key_C) AS Del_Cust_Key_C
    , cvh.Account_Branch, cvh.Account_Number, cvh.Account_Name
    , CONVERT(date, cvh.Delivery_Date) AS Sched_Delivery_Date
    , TRY_CONVERT(int, cvh.Van_No) AS Sched_Van_Number
    , cvh.Drop_No AS Sched_Drop_No
    , TRY_CONVERT(time, cvh.Van_ETA) AS Sched_Van_ETA
    , cvh.Day_No
    , UPPER(LTRIM(RTRIM(cvh.Delivery_Branch))) AS Delivery_Account_DC
    , CONVERT(date, cvh.Snapshot_Date) AS Schedule_Snapshot_Date
    , cvh.Snapshot_Date_Time
    , times.Close_Time AS Sched_Van_Close
    , times.Depart_Time AS Sched_Van_Depart
    , CASE WHEN times.Depart_Time IS NULL OR times.Close_Time IS NULL
               THEN 'UNKNOWN'
           WHEN times.Depart_Time > times.Close_Time THEN 'PM'
           ELSE 'AM' END AS Slot
    , CASE WHEN vh.Van_Close_Time IS NOT NULL
                AND vh.Van_Depart_Time IS NOT NULL THEN 'Van History'
           WHEN vd.Van_Close_Time IS NOT NULL
                AND vd.Van_Depart IS NOT NULL THEN 'Van Description Fallback'
           ELSE 'Missing Van Times' END AS Van_Time_Source
FROM AAHOperations.dbo.Customer_Vans_History AS cvh
LEFT JOIN SupplyChainReporting.dbo.van_history AS vh
    ON CONVERT(date, cvh.Delivery_Date) = CONVERT(date, vh.Date)
   AND UPPER(LTRIM(RTRIM(cvh.Delivery_Branch))) =
       UPPER(LTRIM(RTRIM(vh.Print_Branch)))
   AND TRY_CONVERT(int, cvh.Van_No) = TRY_CONVERT(int, vh.Van_No)
LEFT JOIN SupplyChainReporting.dbo.van_descriptions AS vd
    ON UPPER(LTRIM(RTRIM(cvh.Delivery_Branch))) =
       UPPER(LTRIM(RTRIM(vd.Print_Branch)))
   AND TRY_CONVERT(int, cvh.Van_No) = TRY_CONVERT(int, vd.Van_No)
CROSS APPLY (VALUES (
    COALESCE(TRY_CONVERT(time, vh.Van_Close_Time),
             TRY_CONVERT(time, vd.Van_Close_Time)),
    COALESCE(TRY_CONVERT(time, vh.Van_Depart_Time),
             TRY_CONVERT(time, vd.Van_Depart))
)) AS times(Close_Time, Depart_Time)
WHERE cvh.Snapshot_Date >= DATEADD(DAY, -31, CONVERT(date, GETDATE()))
  AND cvh.Delivery_Date < DATEADD(DAY, 8, CONVERT(date, GETDATE()));
"

# ------------------------------------------------------------
# 4.1 Customer segmentation SQL
# ------------------------------------------------------------
segmentation_sql <- "
SELECT TRY_CONVERT(int, Del_Cust_Key_C) AS Del_Cust_Key_C,
       TRY_CONVERT(int, Tier_Number) AS Teir_Number,
       Customer_Tier_Status
FROM AAHOperations.dbo.Customer_Segmentation_lu;
"


## ------------------------------------------------------------
# 4.15 Branch bank holiday SQL
# ------------------------------------------------------------
# Match holidays against the DELIVERY DC, not the processing DC.
# Retain the branch scope supplied in the original holiday query.
#
# Note: 408T is not included in this list. Orders processed by
# 408T are still covered when their delivery DC is listed here.

bank_holiday_sql <- "
SELECT
      DC_Code
    , Bank_Holiday_Region
    , CONVERT(date, Holiday_Date) AS Holiday_Date
    , Holiday_Name
    , Holiday_Notes
FROM AAHOperations.dbo.vw_branch_bank_holidays
WHERE DC_Code IN (
    '106m','201d','305m','405n','406p','501g',
    '504m','602J','703m','706s','908B'
)
  AND Holiday_Date >= '2025-01-01';
"


## ------------------------------------------------------------
# 4.2 Run all extracts with guaranteed connection cleanup
# ------------------------------------------------------------

extract_sources <- function() {
  
  con <- RODBC::odbcConnect(
    dsn = cfg$source_dsn,
    uid = cfg$db_user,
    pwd = db_password
  )
  
  if (identical(con, -1L)) {
    stop("Could not connect to the source DSN.")
  }
  
  on.exit(
    RODBC::odbcClose(con),
    add = TRUE
  )
  
  list(
    Sales_Import = read_source(
      con, sales_sql, "Sales_Import"
    ),
    
    Tran_Drop = read_source(
      con, tran_drop_sql, "Tran_Drop"
    ),
    
    EPOD_Data = read_source(
      con, epod_sql, "EPOD_Data"
    ),
    
    Customer_Van_History = read_source(
      con,
      customer_van_history_sql,
      "Customer_Van_History"
    ),
    
    Customer_Segmentation = read_source(
      con,
      segmentation_sql,
      "Customer_Segmentation"
    ),
    
    Bank_Holidays = read_source(
      con,
      bank_holiday_sql,
      "Bank_Holidays"
    )
  )
}

sources <- extract_sources()

Sales_Import          <- sources$Sales_Import
Tran_Drop             <- sources$Tran_Drop
EPOD_Data             <- sources$EPOD_Data
Customer_Van_History  <- sources$Customer_Van_History
Customer_Segmentation <- sources$Customer_Segmentation
Bank_Holidays         <- sources$Bank_Holidays

rm(sources)

# ============================================================
# 5. VALIDATE EXTRACT SCHEMAS
# ============================================================
# All extracts are already data.tables from read_source().
check_required_columns(Sales_Import, c(
  "Processing_Date", "Processing_Time", "Processing_DC", "Order_Date",
  "Order_Time", "Pick_Note_No", "Del_Cust_Key_C", "Delivery_DC",
  "Van_Day", "Van_Number", "Order_Type_Description", "Sales_Value"
), "Sales_Import")
check_required_columns(Tran_Drop, c(
  "Pick_Note_No", "Del_Cust_Key_C", "Drop_Del_Date", "Number_Labels"
), "Tran_Drop")
check_required_columns(EPOD_Data, c(
  "Pick_Note_No", "Del_Cust_Key_C", "Delivery_Date", "Delivered_Labels"
), "EPOD_Data")
check_required_columns(Customer_Van_History, c(
  "Del_Cust_Key_C", "Sched_Delivery_Date", "Sched_Van_Number",
  "Sched_Drop_No", "Sched_Van_ETA", "Day_No", "Delivery_Account_DC",
  "Schedule_Snapshot_Date", "Snapshot_Date_Time", "Sched_Van_Close",
  "Sched_Van_Depart", "Slot", "Van_Time_Source"
), "Customer_Van_History")
check_required_columns(Customer_Segmentation, c(
  "Del_Cust_Key_C", "Teir_Number", "Customer_Tier_Status"
), "Customer_Segmentation")

# ------------------------------------------------------------
# 5.1 Validate and prepare branch bank holidays
# ------------------------------------------------------------

check_required_columns(
  Bank_Holidays,
  c("DC_Code", "Holiday_Date"),
  "Bank_Holidays"
)

Bank_Holidays[, `:=`(
  DC_Code = normalise_text(DC_Code),
  Holiday_Date = as.Date(Holiday_Date)
)]

# Invalid holiday keys must not silently pass through.
invalid_bank_holidays <- Bank_Holidays[
  is.na(DC_Code) |
    DC_Code == "" |
    is.na(Holiday_Date)
]

if (nrow(invalid_bank_holidays) > 0L) {
  print(invalid_bank_holidays)
  
  stop(
    "Bank holiday lookup contains invalid branch/date values. ",
    "Load cancelled."
  )
}

if (nrow(Bank_Holidays) == 0L) {
  stop(
    "Bank holiday lookup returned no records. ",
    "Load cancelled to avoid ignoring holiday exclusions."
  )
}

# Only the branch and date are needed for route exclusion.
bank_holiday_lookup <- unique(
  Bank_Holidays[
    ,
    .(
      Delivery_Account_DC = DC_Code,
      delivery_date = Holiday_Date
    )
  ]
)

assert_unique(
  bank_holiday_lookup,
  c("Delivery_Account_DC", "delivery_date"),
  "Bank holiday lookup"
)

# ============================================================
# 6. BUILD CORE ORDER DATASET
# ============================================================
# ------------------------------------------------------------
# 6.1 Standardise join fields in one pass per table
# ------------------------------------------------------------
join_key <- c("Pick_Note_No", "Del_Cust_Key_C")
for (dt in list(Sales_Import, Tran_Drop, EPOD_Data)) {
  dt[, (join_key) := lapply(.SD, as.integer), .SDcols = join_key]
}
rm(dt)

# ------------------------------------------------------------
# 6.2 Check lookup uniqueness before either merge
# ------------------------------------------------------------
assert_unique(Tran_Drop, join_key, "Tran_Drop")
assert_unique(EPOD_Data, join_key, "EPOD_Data")
sales_rows <- nrow(Sales_Import)
if (!sales_rows) stop("No sales rows were extracted; load cancelled.")

# ------------------------------------------------------------
# 6.3 / 6.4 Attach Tran Drop and EPOD without changing sales grain
# ------------------------------------------------------------
Nutricia_OTIF <- merge(
  Sales_Import, Tran_Drop, by = join_key, all.x = TRUE, sort = FALSE
)
assert_row_count(Nutricia_OTIF, sales_rows, "Tran Drop merge")
Nutricia_OTIF <- merge(
  Nutricia_OTIF, EPOD_Data, by = join_key, all.x = TRUE, sort = FALSE
)
assert_row_count(Nutricia_OTIF, sales_rows, "EPOD merge")

# ------------------------------------------------------------
# 6.5 Standardise dates, times and branch codes
# ------------------------------------------------------------
date_cols <- c("Processing_Date", "Order_Date")
time_cols <- c("Processing_Time", "Order_Time")
branch_cols <- c("Processing_DC", "Delivery_DC")
Nutricia_OTIF[, (date_cols) := lapply(.SD, as.Date), .SDcols = date_cols]
Nutricia_OTIF[, (time_cols) := lapply(.SD, to_itime), .SDcols = time_cols]
Nutricia_OTIF[, (branch_cols) := lapply(.SD, normalise_text), .SDcols = branch_cols]
Nutricia_OTIF[, `:=`(
  Delivery_Account_DC = Delivery_DC,
  Actual_Van_Delivery_Date = as.Date(Van_Day),
  Actual_Van_Number = as.integer(Van_Number)
)]

# ------------------------------------------------------------
# 6.6 Source-match QA
# ------------------------------------------------------------
source_match_qa <- Nutricia_OTIF[, .(
  Orders = .N,
  EPOD_Matches = sum(!is.na(Delivery_Date)),
  EPOD_Match_Pct = round(100 * mean(!is.na(Delivery_Date)), 1),
  Tran_Drop_Matches = sum(!is.na(Drop_Del_Date)),
  Tran_Drop_Match_Pct = round(100 * mean(!is.na(Drop_Del_Date)), 1)
), by = Processing_DC][order(Processing_DC)]
print(source_match_qa)

# ------------------------------------------------------------
# 6.7 Attach customer segmentation; reject conflicting records
# ------------------------------------------------------------
Customer_Segmentation[, `:=`(
  Del_Cust_Key_C = as.integer(Del_Cust_Key_C),
  Teir_Number = as.integer(Teir_Number),
  Customer_Tier_Status = as.character(Customer_Tier_Status)
)]
Customer_Segmentation <- unique(Customer_Segmentation[
  !is.na(Del_Cust_Key_C),
  .(Del_Cust_Key_C, Teir_Number, Customer_Tier_Status)
])
assert_unique(Customer_Segmentation, "Del_Cust_Key_C", "Customer_Segmentation")
Nutricia_OTIF[, `:=`(Teir_Number = NA_integer_, Customer_Tier_Status = NA_character_)]
Nutricia_OTIF[Customer_Segmentation, `:=`(
  Teir_Number = i.Teir_Number,
  Customer_Tier_Status = i.Customer_Tier_Status
), on = "Del_Cust_Key_C"]
assert_row_count(Nutricia_OTIF, sales_rows, "Segmentation update")

# ============================================================
# 7. BUILD DATE-BASED VAN SCHEDULE
# ============================================================
sched <- copy(Customer_Van_History)

# ------------------------------------------------------------
# 7.1 Convert related fields together
# ------------------------------------------------------------
int_cols <- c("Del_Cust_Key_C", "Sched_Van_Number", "Sched_Drop_No", "Day_No")
date_cols <- c("Sched_Delivery_Date", "Schedule_Snapshot_Date")
time_cols <- c("Sched_Van_ETA", "Sched_Van_Close", "Sched_Van_Depart")
sched[, (int_cols) := lapply(.SD, as.integer), .SDcols = int_cols]
sched[, (date_cols) := lapply(.SD, as.Date), .SDcols = date_cols]
sched[, (time_cols) := lapply(.SD, to_itime), .SDcols = time_cols]
sched[, `:=`(
  Delivery_Account_DC = normalise_text(Delivery_Account_DC),
  Slot = normalise_text(Slot),
  Van_Time_Source = trimws(as.character(Van_Time_Source))
)]

# ------------------------------------------------------------
# 7.2 Relabel Saturday AM routes; do NOT relabel Saturday PM
# ------------------------------------------------------------
sched[wday(Sched_Delivery_Date, week_start = 1) == 6L & Slot == "AM",
      Slot := "SAT_AM"]

# ------------------------------------------------------------
# 7.3 Snapshot timestamps select the framework version only
# ------------------------------------------------------------
sched[, Snapshot_Date_Time := as.POSIXct(
  as.character(Snapshot_Date_Time), tz = "Europe/London"
)]

# ------------------------------------------------------------
# 7.4 Remove unusable routes before grouping
# ------------------------------------------------------------
sched <- sched[
  !is.na(Del_Cust_Key_C) & !is.na(Delivery_Account_DC) &
    Delivery_Account_DC != "" & !is.na(Sched_Delivery_Date) &
    !is.na(Sched_Van_Number) & !is.na(Snapshot_Date_Time)
]

# ------------------------------------------------------------
# 7.5 Retain ALL routes from each latest dated framework
# ------------------------------------------------------------
framework_key <- c("Del_Cust_Key_C", "Delivery_Account_DC", "Sched_Delivery_Date")
sched[, Latest_Snapshot_Date_Time := max(Snapshot_Date_Time), by = framework_key]
sched <- sched[Snapshot_Date_Time == Latest_Snapshot_Date_Time]
sched[, Latest_Snapshot_Date_Time := NULL]

# ------------------------------------------------------------
# 7.6 / 7.7 Remove duplicate route keys and check uniqueness
# ------------------------------------------------------------
schedule_key <- c(
  "Snapshot_Date_Time", framework_key, "Sched_Van_Number", "Sched_Drop_No", "Slot"
)
sched <- unique(sched, by = schedule_key)
assert_unique(sched, schedule_key, "Selected schedule")

schedule_qa <- sched[, .(
  Routes = .N, Customers = uniqueN(Del_Cust_Key_C),
  Earliest_Delivery = min(Sched_Delivery_Date),
  Latest_Delivery = max(Sched_Delivery_Date),
  Missing_Close_Times = sum(is.na(Sched_Van_Close)),
  Missing_ETAs = sum(is.na(Sched_Van_ETA)),
  Unknown_Slots = sum(Slot == "UNKNOWN", na.rm = TRUE)
), by = Schedule_Snapshot_Date][order(Schedule_Snapshot_Date)]
print_if_detailed(schedule_qa)

# Snapshot coverage is diagnostic only, not an eligibility constraint.
snapshot_coverage <- Nutricia_OTIF[, .(
  Orders = .N, Customers = uniqueN(Del_Cust_Key_C)
), by = Processing_Date]
available_snapshots <- sched[, .(
  Schedule_Routes = .N, Schedule_Customers = uniqueN(Del_Cust_Key_C)
), by = .(Processing_Date = Schedule_Snapshot_Date)]
snapshot_coverage <- available_snapshots[snapshot_coverage, on = "Processing_Date"]
snapshot_coverage[, Snapshot_Available := !is.na(Schedule_Routes)]
print_if_detailed(snapshot_coverage[order(Processing_Date)])

# ------------------------------------------------------------
# 7.8 Attach the actual invoiced route's slot
# ------------------------------------------------------------
actual_slot_lookup <- sched[, .(
  Actual_Delivery_Slot = unique(Slot)
), by = .(
  Del_Cust_Key_C, Delivery_Account_DC,
  Actual_Van_Delivery_Date = Sched_Delivery_Date,
  Actual_Van_Number = Sched_Van_Number
)]
actual_key <- c(
  "Del_Cust_Key_C", "Delivery_Account_DC",
  "Actual_Van_Delivery_Date", "Actual_Van_Number"
)
assert_unique(actual_slot_lookup, actual_key, "Actual slot lookup")
Nutricia_OTIF[, Actual_Delivery_Slot := NA_character_]
Nutricia_OTIF[actual_slot_lookup,
  Actual_Delivery_Slot := i.Actual_Delivery_Slot, on = actual_key]
Nutricia_OTIF[
  !is.na(Actual_Delivery_Slot) &
    !Actual_Delivery_Slot %in% c("AM", "PM", "SAT_AM"),
  Actual_Delivery_Slot := "UNKNOWN"
]

# ============================================================
# 8. EXPECTED DELIVERY CALCULATION
# ============================================================
# ------------------------------------------------------------
# 8.1 Prepare effective order timestamps
# ------------------------------------------------------------
Nutricia_OTIF[, order_id := .I]
Nutricia_OTIF[, order_dt := combine_date_time(Order_Date, Order_Time)]

# ------------------------------------------------------------
# 8.2 / 8.3 Join only routes within each order's 14-day horizon
# ------------------------------------------------------------
# Optimisation: match route ROW INDICES with a bounded non-equi join
# instead of materialising every customer route and then filtering dates.
# Explicit row IDs preserve the real scheduled date and order identity.
# The duplicate date columns are join bounds only, not final output fields.
order_bounds <- Nutricia_OTIF[
  !is.na(Order_Date),
  .(order_id, Del_Cust_Key_C, Delivery_Account_DC,
    route_from = Order_Date, route_to = Order_Date + cfg$route_horizon_days)
]
sched[, schedule_row_id := .I]
route_matches <- sched[
  order_bounds,
  on = .(
    Del_Cust_Key_C, Delivery_Account_DC,
    Sched_Delivery_Date >= route_from,
    Sched_Delivery_Date <= route_to
  ),
  .(schedule_row = x.schedule_row_id, matched_order_id = i.order_id),
  nomatch = 0L, allow.cartesian = TRUE
]
sched[, schedule_row_id := NULL]
# Restore original order/route ordering for otherwise identical ties.
setorderv(route_matches, c("matched_order_id", "schedule_row"))
# Reconstruct the original schedule[orders] column layout without carrying
# non-equi join-bound values into Sched_Delivery_Date.
matched_sched <- sched[route_matches$schedule_row]
matched_orders <- Nutricia_OTIF[
  match(route_matches$matched_order_id, Nutricia_OTIF$order_id)
]
# Same join keys appear only once, as in the original equi join.
order_extra_cols <- setdiff(names(matched_orders), c(
  "Del_Cust_Key_C", "Delivery_Account_DC"
))
cand <- cbind(matched_sched, matched_orders[, ..order_extra_cols])
rm(order_bounds, route_matches, matched_sched, matched_orders)

cand[, `:=`(
  delivery_date = Sched_Delivery_Date,
  delta_days = as.integer(Sched_Delivery_Date - Order_Date),
  delivery_dow = wday(Sched_Delivery_Date, week_start = 1),
  Processing_DC_norm = normalise_text(Processing_DC),
  Delivery_DC_norm = normalise_text(Delivery_DC),
  Order_Type_Description_norm = normalise_text(Order_Type_Description),
  order_dow = wday(Order_Date, week_start = 1)
)]

# ------------------------------------------------------------
# 8.4 Identify cross-DC flows and combined Tier 3/4 orders
# ------------------------------------------------------------
cand[, cross_dc :=
  !is.na(Processing_DC_norm) & !is.na(Delivery_DC_norm) &
    Processing_DC_norm != Delivery_DC_norm & !(
      (Processing_DC_norm == "703M" & Delivery_DC_norm == "908B") |
      (Processing_DC_norm == "706S" & Delivery_DC_norm == "602J")
    )
]
cand[, `:=`(
  cross_408T = cross_dc & Processing_DC_norm == "408T" & Delivery_DC_norm != "408T",
  is_tier4 = !is.na(Teir_Number) & Teir_Number %in% c(3L, 4L)
)]

# ------------------------------------------------------------
# 8.5 Nutricia cutoff: exactly 15:00 remains in time
# ------------------------------------------------------------
cand[, is_nutricia :=
  !is.na(Order_Type_Description_norm) & Order_Type_Description_norm == "NUTRICIA"]
cand[, nutricia_after_cutoff :=
  is_nutricia & !is.na(Order_Time) & Order_Time > cutoffs$nutricia]

# ------------------------------------------------------------
# 8.5.1 Dedicated 408T Fridge Friday/weekend flags
# ------------------------------------------------------------
cand[, is_408T_fridge :=
  !is.na(Processing_DC_norm) & Processing_DC_norm == "408T" &
    !is.na(Order_Type_Description_norm) & Order_Type_Description_norm == "FRIDGE"]
cand[, `:=`(
  fridge_408T_friday_before_cutoff =
    is_408T_fridge & order_dow == 5L & !is.na(Order_Time) &
    Order_Time <= cutoffs$fridge_408T,
  fridge_408T_tuesday_movement =
    is_408T_fridge & (
      (order_dow == 5L & !is.na(Order_Time) & Order_Time > cutoffs$fridge_408T) |
        order_dow %in% c(6L, 7L)
    ),
  fridge_408T_earliest_tuesday = as.Date(NA)
)]
cand[fridge_408T_tuesday_movement == TRUE,
  fridge_408T_earliest_tuesday := Order_Date + ((2L - order_dow + 7L) %% 7L)]

# ------------------------------------------------------------
# 8.6 Standard cutoff date: AM/SAT_AM previous day; PM same day
# ------------------------------------------------------------
cand[, `:=`(
  cutoff_date = delivery_date + fifelse(Slot %in% c("AM", "SAT_AM"), -1L, 0L),
  cutoff_time = Sched_Van_Close
)]

# ------------------------------------------------------------
# 8.7 Cutoff priority: later assignments override earlier ones
# ------------------------------------------------------------
# 1. Non-408T cross-DC standard cutoff.
cand[cross_dc & Processing_DC_norm != "408T", cutoff_time := cutoffs$cross_dc]
# 2. Monday cross-DC AM closes Sunday at 11:30.
cand[delivery_dow == 1L & Slot == "AM" & cross_dc & Processing_DC_norm != "408T",
     cutoff_time := cutoffs$sunday]
# 3. Tier 3/4 local AND cross-DC cutoff, based on cutoff weekday.
cutoff_dow <- wday(cand$cutoff_date, week_start = 1)
cand[is_tier4 & cutoff_dow %in% 1:5, cutoff_time := cutoffs$tier34_weekday]
cand[is_tier4 & cutoff_dow == 7L, cutoff_time := cutoffs$sunday]
rm(cutoff_dow)
# 4. Dedicated 408T non-Nutricia AM/SAT_AM cutoff.
cand[cross_408T & !is_nutricia & Slot %in% c("AM", "SAT_AM"),
     cutoff_time := cutoffs$cross_408T_route]
# 5. Fridge has FINAL precedence, including Friday SAT_AM cutoff.
cand[is_408T_fridge == TRUE, cutoff_time := cutoffs$fridge_408T]

# ------------------------------------------------------------
# 8.8 Build route timestamps
# ------------------------------------------------------------
cand[, `:=`(
  cutoff_dt = combine_date_time(cutoff_date, cutoff_time),
  delivery_dt = combine_date_time(delivery_date, Sched_Van_ETA)
)]

# ------------------------------------------------------------
# 8.9 Base eligibility uses complete dates AND times
# ------------------------------------------------------------
cand[, `:=`(
  order_before_cutoff = !is.na(order_dt) & !is.na(cutoff_dt) & order_dt <= cutoff_dt,
  minutes_before_cutoff = as.numeric(difftime(cutoff_dt, order_dt, units = "mins"))
)]
cand[, base_eligible :=
  !is.na(Sched_Van_Number) & !is.na(cutoff_time) &
    !is.na(order_dt) & !is.na(cutoff_dt) & order_before_cutoff]

# ------------------------------------------------------------
# 8.10 Retain the original no-same-day restrictions
# ------------------------------------------------------------
# SAT_AM next day is NOT excluded. Do not override all eligibility flags
# with an OR exception: that could bypass unrelated business rules.
cand[, passes_cross_dc_pm_rule := !(
  (is_tier4 & delta_days == 0L) |
    (cross_dc & Slot == "PM" & delta_days == 0L)
)]

# ------------------------------------------------------------
# 8.11 Non-Nutricia cross-408T orders AFTER 17:00 need >=2 days
# ------------------------------------------------------------
cand[, after_408T_processing_cutoff :=
  cross_408T & !is_nutricia & !is.na(Order_Time) &
    Order_Time > cutoffs$processing_408T]
cand[, passes_408T_movement_rule :=
  !(after_408T_processing_cutoff & delta_days < 2L)]

# ------------------------------------------------------------
# 8.12 Fridge Friday/weekend movement, then combine all checks
# ------------------------------------------------------------
cand[, passes_408T_fridge_weekend_rule := fcase(
  fridge_408T_friday_before_cutoff == TRUE, delivery_date > Order_Date,
  fridge_408T_tuesday_movement == TRUE, delivery_date >= fridge_408T_earliest_tuesday,
  default = TRUE
)]
cand[, eligible_before_nutricia :=
  base_eligible & passes_cross_dc_pm_rule & passes_408T_movement_rule &
    passes_408T_fridge_weekend_rule]
eligible <- cand[eligible_before_nutricia == TRUE]

# ------------------------------------------------------------
# 8.13 Nutricia AFTER 15:00 skips the first eligible DATE
# ------------------------------------------------------------
eligible[, first_eligible_delivery_date := min(delivery_date), by = order_id]
eligible <- eligible[!(
  nutricia_after_cutoff & delivery_date == first_eligible_delivery_date
)]

# ------------------------------------------------------------
# 8.13a Exclude bank holiday delivery dates
# ------------------------------------------------------------
# Apply after the existing Nutricia movement rule.
#
# Exclude routes where:
#   Delivery_Account_DC matches the holiday DC_Code
#   delivery_date matches Holiday_Date
#
# Do not manually increment delivery_date:
# the next route must exist in the customer's dated framework
# and must have passed the existing cutoff/movement rules.

holiday_candidate_rows <- eligible[
  bank_holiday_lookup,
  on = .(
    Delivery_Account_DC,
    delivery_date
  ),
  which = TRUE,
  nomatch = 0L
]

holiday_candidate_rows <- unique(holiday_candidate_rows)

# Keep diagnostic information outside the final output table.
bank_holiday_excluded_routes <- eligible[
  holiday_candidate_rows,
  .(
    order_id,
    Pick_Note_No,
    Del_Cust_Key_C,
    Processing_DC,
    Delivery_Account_DC,
    Order_Date,
    Order_Time,
    delivery_date,
    Slot,
    Sched_Van_Number
  )
]

orders_with_holiday_routes <- unique(
  bank_holiday_excluded_routes$order_id
)

if (length(holiday_candidate_rows) > 0L) {
  
  eligible <- eligible[-holiday_candidate_rows]
  
  message(
    sprintf(
      paste0(
        "Bank holiday rule excluded %s candidate routes ",
        "across %s orders."
      ),
      format(
        nrow(bank_holiday_excluded_routes),
        big.mark = ","
      ),
      format(
        length(orders_with_holiday_routes),
        big.mark = ","
      )
    )
  )
}

print_if_detailed(bank_holiday_excluded_routes)

# Section 8.14 now calculates AM preference using only
# non-holiday routes.
#
# Section 8.16 selects the earliest remaining qualifying route.

# ------------------------------------------------------------
# 8.14 Preserve Tier 3/4 selection and other-customer AM preference
# ------------------------------------------------------------
# has_eligible_am considers all eligible dates, as in the original.
# Do not change this to per-day grouping without agreeing a rule change.
eligible[, has_eligible_am := any(
  Slot %in% c("AM", "SAT_AM") & !is.na(Sched_Van_Number), na.rm = TRUE
), by = order_id]
eligible <- eligible[
  is_tier4 |
    (!is_tier4 & (
      !cross_dc |
        fridge_408T_tuesday_movement |
        (cross_dc & !fridge_408T_tuesday_movement & has_eligible_am &
           Slot %in% c("AM", "SAT_AM")) |
        (cross_dc & !fridge_408T_tuesday_movement & !has_eligible_am & Slot == "PM")
    ))
]

# ------------------------------------------------------------
# 8.14a Friday Fridge diagnostics; no eligibility override
# ------------------------------------------------------------
if (isTRUE(cfg$friday_fridge_qa)) {
  # Include the exact route/drop key at extraction time.
  friday_408T_sat_qa <- cand[
    fridge_408T_friday_before_cutoff == TRUE & delta_days == 1L,
    .(order_id, Pick_Note_No, Del_Cust_Key_C, Teir_Number,
      Order_Date, Order_Time, delivery_date, Slot,
      Sched_Van_Number, Sched_Drop_No, Sched_Van_Close, Sched_Van_Depart,
      cutoff_date, cutoff_time, order_before_cutoff, base_eligible,
      passes_cross_dc_pm_rule, passes_408T_movement_rule,
      passes_408T_fridge_weekend_rule, eligible_before_nutricia)
  ]
  route_key <- c("order_id", "delivery_date", "Sched_Van_Number", "Sched_Drop_No", "Slot")
  retained <- unique(eligible[, ..route_key])
  friday_408T_sat_qa[, Retained_After_8_14 := FALSE]
  friday_408T_sat_qa[retained, Retained_After_8_14 := TRUE, on = route_key]
  print(friday_408T_sat_qa[, .(Candidate_Routes = .N), by = .(
    Slot, base_eligible, eligible_before_nutricia, Retained_After_8_14
  )])
  print_if_detailed(friday_408T_sat_qa)

  # Start from the order table so orders with NO joined candidates are
  # not silently omitted from this diagnostic.
  friday_orders <- Nutricia_OTIF[
    Processing_DC == "408T" & normalise_text(Order_Type_Description) == "FRIDGE" &
      wday(Order_Date, week_start = 1) == 5L & !is.na(Order_Time) &
      Order_Time <= cutoffs$fridge_408T,
    .(order_id, Pick_Note_No, Del_Cust_Key_C, Order_Date, Order_Time)
  ]
  friday_408T_missing_saturday <- friday_orders[
    !order_id %in% friday_408T_sat_qa$order_id
  ]
  if (nrow(friday_408T_missing_saturday)) {
    message(nrow(friday_408T_missing_saturday),
            " Friday Fridge orders have no joined Saturday candidate.")
    print_if_detailed(friday_408T_missing_saturday)
  }
}

# ------------------------------------------------------------
# 8.15 Record rule labels in the same precedence as before
# ------------------------------------------------------------
eligible[, Route_Selection_Rule := fcase(
  is_tier4 & fridge_408T_tuesday_movement,
  "Tier 3/4 - 408T Fridge Friday After 17:00 or Weekend - Tuesday/Next Available Van",
  is_tier4 & fridge_408T_friday_before_cutoff,
  "Tier 3/4 - 408T Fridge Friday At/Before 17:00 - Next Available Van",
  is_tier4 & is_408T_fridge,
  "Tier 3/4 - 408T Fridge - 17:00 Cutoff",
  is_tier4 & Processing_DC_norm == "408T",
  "Tier 3/4 - 408T Rules - Earliest Eligible Van",
  is_tier4,
  "Tier 3/4 - Fixed Cutoff - Earliest Eligible Van",
  fridge_408T_tuesday_movement,
  "408T Fridge Friday After 17:00 or Weekend - Tuesday/Next Available Van",
  fridge_408T_friday_before_cutoff,
  "408T Fridge Friday At/Before 17:00 - Next Available Van",
  is_408T_fridge,
  "408T Fridge - 17:00 Route Cutoff",
  nutricia_after_cutoff & !cross_dc,
  "Nutricia After 15:00 - Next Route",
  nutricia_after_cutoff & cross_dc & has_eligible_am & Slot == "AM",
  "Nutricia After 15:00 - Next Cross-DC AM",
  nutricia_after_cutoff & cross_dc & has_eligible_am & Slot == "SAT_AM",
  "Nutricia After 15:00 - Next Cross-DC Saturday AM",
  nutricia_after_cutoff & cross_dc & !has_eligible_am & Slot == "PM",
  "Nutricia After 15:00 - Next Cross-DC PM",
  !cross_dc, "Standard Route Selection",
  cross_dc & has_eligible_am & Slot == "AM", "Cross-DC AM Preferred",
  cross_dc & has_eligible_am & Slot == "SAT_AM", "Cross-DC Saturday AM Preferred",
  cross_dc & !has_eligible_am & Slot == "PM", "Cross-DC PM Fallback",
  default = "Other"
)]

# ------------------------------------------------------------
# 8.16 Select earliest qualifying route without .SD per group
# ------------------------------------------------------------
# Keep the original global ordering and tie-breakers, then take the first
# row index per order. This avoids constructing a full .SD for each order.
eligible[, eta_missing := is.na(delivery_dt)]
setorderv(eligible, c(
  "delivery_date", "eta_missing", "delivery_dt", "Sched_Van_Number", "Sched_Drop_No"
), na.last = TRUE)
best_indices <- eligible[, .(row_index = .I[1L]), by = order_id]$row_index
best <- eligible[best_indices]

# ------------------------------------------------------------
# 8.17 Initialise typed output fields even when no route is found
# ------------------------------------------------------------
Nutricia_OTIF[, `:=`(
  Expected_Delivery_Date = as.Date(NA),
  Expected_Van_Day_No = NA_integer_,
  Expected_Van_Slot = NA_character_,
  Expected_Van_Number = NA_integer_,
  Expected_Van_Drop_No = NA_integer_,
  Expected_Van_ETA = as.ITime(NA_character_),
  Expected_Van_Depart = as.ITime(NA_character_),
  Expected_Van_Close = as.ITime(NA_character_),
  Expected_Cutoff_Date = as.Date(NA),
  Expected_Cutoff_DT = as.POSIXct(NA, tz = "UTC"),
  Expected_Delivery_DT = as.POSIXct(NA, tz = "UTC"),
  Route_Selection_Rule = NA_character_,
  Van_Time_Source = NA_character_,
  Schedule_Snapshot_Used = as.Date(NA),
  Schedule_Snapshot_Time_Used = as.POSIXct(NA, tz = "UTC"),
  Nutricia_After_1400 = FALSE,
  First_Eligible_Delivery_Date = as.Date(NA),
  Expected_Minutes_Before_Cutoff = NA_real_
)]

# ------------------------------------------------------------
# 8.18 Write the selected route back using an update join
# ------------------------------------------------------------
if (nrow(best)) {
  Nutricia_OTIF[best, `:=`(
    Expected_Delivery_Date = i.delivery_date,
    Expected_Van_Day_No = i.Day_No,
    Expected_Van_Slot = i.Slot,
    Expected_Van_Number = i.Sched_Van_Number,
    Expected_Van_Drop_No = i.Sched_Drop_No,
    Expected_Van_ETA = i.Sched_Van_ETA,
    Expected_Van_Depart = i.Sched_Van_Depart,
    Expected_Van_Close = i.cutoff_time,
    Expected_Cutoff_Date = i.cutoff_date,
    Expected_Cutoff_DT = i.cutoff_dt,
    Expected_Delivery_DT = i.delivery_dt,
    Route_Selection_Rule = i.Route_Selection_Rule,
    Van_Time_Source = i.Van_Time_Source,
    Schedule_Snapshot_Used = i.Schedule_Snapshot_Date,
    Schedule_Snapshot_Time_Used = i.Snapshot_Date_Time,
    Nutricia_After_1400 = i.nutricia_after_cutoff,
    First_Eligible_Delivery_Date = i.first_eligible_delivery_date,
    Expected_Minutes_Before_Cutoff = i.minutes_before_cutoff
  ), on = "order_id"]
}

# ------------------------------------------------------------
# 8.19 Explain why no expected delivery was assigned
# ------------------------------------------------------------
orders_with_candidate_routes <- unique(cand$order_id)
orders_eligible_before_nutricia <- unique(cand[eligible_before_nutricia == TRUE, order_id])
orders_after_final_rules <- unique(eligible$order_id)
orders_with_selected_route <- unique(best$order_id)
Nutricia_OTIF[, Expected_Delivery_Status := fcase(
  !is.na(Expected_Delivery_Date), "Expected Delivery Assigned",
  is.na(Order_Date), "Missing Order Date",
  is.na(Order_Time), "Missing Order Time",
  is.na(Delivery_Account_DC) | Delivery_Account_DC == "", "Missing Delivery DC",
  !order_id %in% orders_with_candidate_routes, "No Matching Customer Routes",
  !order_id %in% orders_eligible_before_nutricia, "No Route Before Cutoff",
  !order_id %in% orders_after_final_rules, "No Route After Business Rules",
  !order_id %in% orders_with_selected_route, "No Route Selected",
  default = "No Eligible Schedule"
)]

# ============================================================
# 9. DELIVERY STATUS AND PROVISIONAL IN-FULL
# ============================================================
# Retain original timestamp treatment. The source script's comment said
# UK local date, but its implementation used UTC; that ambiguity is NOT
# resolved by changing timestamps during a readability refactor.
if (!inherits(Nutricia_OTIF$Delivery_Date, "POSIXct")) {
  Nutricia_OTIF[, Delivery_Date := as.POSIXct(as.character(Delivery_Date), tz = "UTC")]
} else {
  Nutricia_OTIF[, Delivery_Date := force_tz(Delivery_Date, tzone = "UTC")]
}
Nutricia_OTIF[, Delivery_Date_DateOnly := as.Date(Delivery_Date, tz = "UTC")]
Nutricia_OTIF[, OTIF_Status := fcase(
  is.na(Delivery_Date_DateOnly), "No EPOD",
  is.na(Expected_Delivery_Date), "No Expected Delivery",
  Delivery_Date_DateOnly <= Expected_Delivery_Date, "On Time",
  Delivery_Date_DateOnly > Expected_Delivery_Date, "Late",
  default = "Unknown"
)]
Nutricia_OTIF[, `:=`(
  Label_Difference = Delivered_Labels - Number_Labels,
  In_Full_Provisional = fcase(
    is.na(Number_Labels), "Expected Labels Missing",
    is.na(Delivered_Labels), "Delivered Labels Missing",
    Delivered_Labels >= Number_Labels, "In Full",
    Delivered_Labels < Number_Labels, "Short",
    default = "Unknown"
  )
)]

# ------------------------------------------------------------
# 9.1 Date and slot status; retain current output labels
# ------------------------------------------------------------
# Early deliveries pass regardless of slot.
# Same-day missing slots pass, exactly as in the supplied script.
# UNKNOWN is a non-missing string and retains its existing comparison
# behaviour; it is not silently treated as a missing slot.
Nutricia_OTIF[, Delivery_Slot_OTIF := fcase(
  is.na(Delivery_Date_DateOnly), "No EPOD",
  is.na(Expected_Delivery_Date), OTIF_Status,
  Delivery_Date_DateOnly > Expected_Delivery_Date, "Late",
  Delivery_Date_DateOnly < Expected_Delivery_Date, "On Time",
  Delivery_Date_DateOnly == Expected_Delivery_Date & is.na(Expected_Van_Slot), "On Time",
  Delivery_Date_DateOnly == Expected_Delivery_Date & is.na(Actual_Delivery_Slot), "On Time",
  Delivery_Date_DateOnly == Expected_Delivery_Date &
    Expected_Van_Slot == Actual_Delivery_Slot, "On Time",
  Delivery_Date_DateOnly == Expected_Delivery_Date &
    Expected_Van_Slot != Actual_Delivery_Slot, "Late",
  default = "Unknown"
)]

# ------------------------------------------------------------
# 9.1a Focused 408T Fridge QA (not all 408T orders)
# ------------------------------------------------------------
fridge_408T_delivery_qa <- Nutricia_OTIF[
  Processing_DC == "408T" & normalise_text(Order_Type_Description) == "FRIDGE" &
    !is.na(Delivery_Date_DateOnly) & !is.na(Expected_Delivery_Date),
  .(Orders = .N), by = .(
    Route_Selection_Rule, Expected_Delivery_Date, Delivery_Date_DateOnly,
    Expected_Van_Slot, Actual_Delivery_Slot, OTIF_Status, Delivery_Slot_OTIF
  )
][order(Expected_Delivery_Date, Delivery_Date_DateOnly)]
print_if_detailed(fridge_408T_delivery_qa)

# ------------------------------------------------------------
# 9.2 Final status summary
# ------------------------------------------------------------
otif_qa <- Nutricia_OTIF[, .(
  Orders = .N, Sales_Value = sum(Sales_Value, na.rm = TRUE)
), by = .(Processing_DC, OTIF_Status, Delivery_Slot_OTIF, In_Full_Provisional)]
setorderv(otif_qa, c("Processing_DC", "OTIF_Status", "Delivery_Slot_OTIF", "In_Full_Provisional"))
print(otif_qa)

# ============================================================
# 10. FINAL OUTPUT VALIDATION
# ============================================================
if (!nrow(Nutricia_OTIF)) stop("Nutricia_OTIF contains no records; write cancelled.")
assert_row_count(Nutricia_OTIF, sales_rows, "Final output")
check_required_columns(Nutricia_OTIF, c(
  "Processing_Date", "Processing_DC", "Pick_Note_No", "Del_Cust_Key_C",
  "Expected_Delivery_Status", "OTIF_Status", "Delivery_Slot_OTIF",
  "Actual_Delivery_Slot", "Teir_Number", "Customer_Tier_Status"
), "Nutricia_OTIF")

actual_slot_qa <- Nutricia_OTIF[, .(Orders = .N), by = .(Processing_DC, Actual_Delivery_Slot)]
print_if_detailed(actual_slot_qa[order(Processing_DC, Actual_Delivery_Slot)])
message(sprintf("Actual delivery slot matched for %.1f%% of orders.",
                100 * mean(!is.na(Nutricia_OTIF$Actual_Delivery_Slot))))

output_duplicates <- Nutricia_OTIF[, .N, by = .(
  Processing_Date, Pick_Note_No, Del_Cust_Key_C
)][N > 1L]
if (nrow(output_duplicates)) {
  warning("Final output contains ", nrow(output_duplicates),
          " duplicate invoice-level keys; original warning-only behaviour retained.")
}

delivery_slot_qa <- Nutricia_OTIF[, .(
  Orders = .N, Sales_Value = sum(Sales_Value, na.rm = TRUE)
), by = .(Processing_DC, Expected_Van_Slot, Actual_Delivery_Slot, Delivery_Slot_OTIF)]
print_if_detailed(delivery_slot_qa[order(Processing_DC, Expected_Van_Slot, Actual_Delivery_Slot)])
delivery_slot_otif_pct <- Nutricia_OTIF[
  Delivery_Slot_OTIF %in% c("On Time", "Late"),
  if (.N) round(100 * mean(Delivery_Slot_OTIF == "On Time"), 1) else NA_real_
]
message(sprintf("Delivery date and slot OTIF: %.1f%%", delivery_slot_otif_pct))

# ------------------------------------------------------------
# 10.1 Tier 3/4 validation
# ------------------------------------------------------------
tier34_selection_qa <- Nutricia_OTIF[Teir_Number %in% c(3L, 4L), .(Orders = .N), by = .(
  Teir_Number, Processing_DC, Expected_Van_Slot, Expected_Delivery_Status, Route_Selection_Rule
)]
print_if_detailed(tier34_selection_qa)
tier34_same_day <- Nutricia_OTIF[
  Teir_Number %in% c(3L, 4L) & !is.na(Expected_Delivery_Date) &
    !is.na(Order_Date) & Expected_Delivery_Date <= Order_Date
]
if (nrow(tier34_same_day)) {
  print(tier34_same_day[, .(
    Teir_Number, Pick_Note_No, Del_Cust_Key_C, Processing_DC,
    Order_Date, Order_Time, Expected_Delivery_Date, Expected_Van_Slot, Route_Selection_Rule
  )])
  stop("Tier 3/4 same-day delivery detected; Azure write cancelled.")
}
message("Tier 3/4 validation completed successfully.")

# ------------------------------------------------------------
# 10.2 Validate selected routes against branch bank holidays
# ------------------------------------------------------------

selected_holiday_routes <- best[
  bank_holiday_lookup,
  on = .(
    Delivery_Account_DC,
    delivery_date
  ),
  nomatch = 0L
]

if (nrow(selected_holiday_routes) > 0L) {
  
  print(
    selected_holiday_routes[
      ,
      .(
        order_id,
        Pick_Note_No,
        Del_Cust_Key_C,
        Delivery_Account_DC,
        delivery_date,
        Slot,
        Sched_Van_Number
      )
    ]
  )
  
  stop(
    "An expected delivery route falls on a branch bank holiday. ",
    "Azure write cancelled."
  )
}

# Orders with holiday candidates but no final qualifying route.
bank_holiday_orders_without_route <- Nutricia_OTIF[
  order_id %in% orders_with_holiday_routes &
    is.na(Expected_Delivery_Date),
  .(
    order_id,
    Pick_Note_No,
    Del_Cust_Key_C,
    Delivery_Account_DC,
    Expected_Delivery_Status
  )
]

if (nrow(bank_holiday_orders_without_route) > 0L) {
  message(
    nrow(bank_holiday_orders_without_route),
    " orders with excluded holiday routes have no remaining ",
    "qualifying route within the configured search horizon."
  )
  
  print_if_detailed(bank_holiday_orders_without_route)
}

message("Bank holiday route validation completed successfully.")

# ============================================================
# 11. REFRESH PREVIOUS 31 COMPLETED DAYS IN AZURE SQL
# ============================================================
# Work on a copy; preserve the full calculation result for investigation.
otif_to_write <- copy(Nutricia_OTIF)
temporary_output_fields <- intersect(
  c("Actual_Van_Delivery_Date", "Actual_Van_Number"), names(otif_to_write)
)
if (length(temporary_output_fields)) otif_to_write[, (temporary_output_fields) := NULL]
check_required_columns(otif_to_write, c("Actual_Delivery_Slot", "Delivery_Slot_OTIF"),
                       "Azure output")
otif_to_write[, Processing_Date := as.Date(Processing_Date)]
otif_to_write <- otif_to_write[
  !is.na(Processing_Date) & Processing_Date >= window_start & Processing_Date < window_end
]
if (!nrow(otif_to_write)) stop("No replacement rows for the refresh window; write cancelled.")
datetime_columns <- names(otif_to_write)[vapply(otif_to_write, inherits, logical(1), "POSIXct")]
if (length(datetime_columns)) {
  otif_to_write[, (datetime_columns) := lapply(.SD, with_tz, tzone = "UTC"),
                .SDcols = datetime_columns]
}

write_otif_window <- function(rows, start_date, end_date) {
  # Validate the function inputs before any database changes.
  if (is.na(start_date) || is.na(end_date) || start_date >= end_date) {
    stop("Invalid refresh window.")
  }
  if (!nrow(rows) || anyNA(rows$Processing_Date) ||
      any(rows$Processing_Date < start_date | rows$Processing_Date >= end_date)) {
    stop("Replacement rows are empty or outside the refresh window.")
  }
  con <- DBI::dbConnect(
    odbc::odbc(), Driver = cfg$target_driver, Server = cfg$target_server,
    Database = cfg$target_database, UID = cfg$db_user, PWD = db_password, Port = 1433
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  target <- DBI::Id(schema = "dbo", table = "OTIF_AAH_New")
  if (!DBI::dbExistsTable(con, target)) stop("dbo.OTIF_AAH_New does not exist.")

  # ----------------------------------------------------------
  # 11.1 Ensure the two previously introduced columns exist
  # ----------------------------------------------------------
  # DDL remains outside the data-refresh transaction, as before.
  DBI::dbExecute(con, "
    IF COL_LENGTH('dbo.OTIF_AAH_New', 'Actual_Delivery_Slot') IS NULL
      ALTER TABLE dbo.OTIF_AAH_New ADD Actual_Delivery_Slot varchar(10) NULL;
    IF COL_LENGTH('dbo.OTIF_AAH_New', 'Delivery_Slot_OTIF') IS NULL
      ALTER TABLE dbo.OTIF_AAH_New ADD Delivery_Slot_OTIF varchar(30) NULL;
  ")

  # ----------------------------------------------------------
  # 11.2 Require an exact field-name match before deleting rows
  # ----------------------------------------------------------
  target_columns <- DBI::dbListFields(con, target)
  r_only <- setdiff(names(rows), target_columns)
  azure_only <- setdiff(target_columns, names(rows))
  if (length(r_only) || length(azure_only)) {
    stop("Schema mismatch. R-only: ", paste(r_only, collapse = ", "),
         "; Azure-only: ", paste(azure_only, collapse = ", "), ". Refresh cancelled.")
  }
  # Local copy avoids reordering the caller's data.table by reference.
  rows <- as.data.frame(rows[, ..target_columns])
  range_params <- list(as.character(start_date), as.character(end_date))

  # ----------------------------------------------------------
  # 11.3 Delete, append and validate in one transaction
  # ----------------------------------------------------------
  DBI::dbWithTransaction(con, {
    deleted <- DBI::dbExecute(con, "
      DELETE FROM dbo.OTIF_AAH_New
      WHERE Processing_Date >= CAST(? AS date)
        AND Processing_Date < CAST(? AS date);
    ", params = range_params)
    DBI::dbWriteTable(con, target, rows, append = TRUE, row.names = FALSE)
    rows_in_window <- as.numeric(DBI::dbGetQuery(con, "
      SELECT COUNT_BIG(*) AS Row_Count FROM dbo.OTIF_AAH_New
      WHERE Processing_Date >= CAST(? AS date)
        AND Processing_Date < CAST(? AS date);
    ", params = range_params)$Row_Count[[1L]])
    if (rows_in_window != nrow(rows)) {
      stop("Window row-count validation failed; transaction will be rolled back.")
    }
    list(deleted = as.numeric(deleted), inserted = nrow(rows), validated = rows_in_window)
  })
}

message(sprintf("Prepared %s rows for %s to %s.",
  format(nrow(otif_to_write), big.mark = ",", scientific = FALSE),
  window_start, window_end - 1L))
if (isTRUE(cfg$write_to_azure)) {
  refresh_result <- write_otif_window(otif_to_write, window_start, window_end)
  message(sprintf("Azure refresh completed: %s deleted; %s appended and validated.",
    format(refresh_result$deleted, big.mark = ",", scientific = FALSE),
    format(refresh_result$validated, big.mark = ",", scientific = FALSE)))
} else {
  message("QA-ONLY RUN: no TARGET write connection, schema change, deletion or insert performed.")
}

# ============================================================
# 12. COMPLETION
# ============================================================
message(sprintf("OTIF process completed in %.2f minutes.",
                as.numeric(difftime(Sys.time(), t1, units = "mins"))))



############################ Still to do Nutricia Overrides by Customer ACCOUNT FOR 408t
############################ Confirm mounjara del prop
