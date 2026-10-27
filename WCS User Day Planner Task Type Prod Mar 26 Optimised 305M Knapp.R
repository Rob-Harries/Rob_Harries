
rm(list=ls())
gc()
t1 <- Sys.time()

setwd('C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/')

library(RODBC); library(data.table); library(lubridate); library(foreach); library(plyr); library(dplyr); library(matrixStats); library(stringr); library(hms); library(DBI); library(odbc)

##R package tstools has the ability to run ABC and XYZ analysis library(tstools)
##https://kourentzes.com/forecasting/2016/10/15/abc-xyz-analysis-for-forecasting/

connection_function = function() {
  pwd = "G00ners4everywW"  
  #readline("Enter password: ")
  con <<- odbcConnect("Azure Supply Chain", uid = "Harriesr", pwd = pwd)
}; connection_function();



Pick_Import = sqlQuery(con, paste("SELECT kp.Print_Branch
	  ,kp.Rec_Type
      ,convert(date,[Processing_Date]) as Processing_Date
	  ,kp.User_ID
	  ,kp.Product_Code
	  ,p.Product_Description
	  ,p.Pack_Size
	  ,p.Full_Supplier
	  ,p.Report_Group
	  ,p.Product_Group
	  ,p.Umbrella_Group
	  ,p.Product_Category_ID
      ,kp.Pick_Note_Number
	  ,Null as Carrier
	  ,Null as Out_Of_Stock
	  ,CASE when p.Location_Type = 'C' THEN 'Y' Else 'N' end as Controlled_Line
	  ,kp.Line_Quantity + kp.Discrepancy as Expected_Quantity
	  ,kp.[Line_Quantity] as Actual_Quantity
      ,kp.Location as Destination
	  ,'RF_Picking' as Licence_Plate
	  ,(kp.Invoice_Line_Number - 1) as Pick_Note_Line_Number
	 ,Null as [Expiry_Date]
	 ,Null as Expiry_Capture
	 ,Null as Expiry_Warning
	 ,kp.Pick_Flag as Exception_Flag
	 ,p.Location_Type
	 ,Null as GTIN
	 ,null as Batch_Number
	 ,kp.Start_Time
     ,kp.End_Time
	 ,kp.Duration as Duration_Seconds    
	 ,'On Circuit Picking' as Task_Type	  
	 ,kp.Tote
     ,kp.Station
	 ,kp.Account_Branch
	 ,kp.Account_Number
	 ,c.Account_Name 
	 ,'K' as Source
  FROM [AAHOperations].[dbo].[Knapp_Picking] kp
  Left Join SupplyChainReporting.dbo.product_list p on kp.Product_Code = p.Product_Code 
  Left Join SupplyChainReporting.dbo.b2b_customers c on kp.Account_Branch = c.Account_Branch and kp.Account_Number = c.Account_Number
 Where kp.Processing_Date >= CONVERT (date,GETDATE()-21) 
-- and Pick_Flag in ('P','S')
 and kp.Print_Branch  in ('106m','201D' ,'305m','405n','406p' ,'501g' ,'504m', '602j','703m' ,'706s','908b','408t')
 Union
  SELECT Distinct
		ps.[Print_Branch]
		,'PICK' as Rec_Type 
		,convert(date,ps.Processing_Date) as Processing_Date   
		,trim(Picker_ID) as User_ID
		 ,pl.[Product_Code]
		 ,Product_Description
		 ,Pack_Size
		 ,Full_Supplier
	  ,Report_Group
	  ,Product_Group
	  ,Umbrella_Group
	  ,Product_Category_ID
      ,ps.[Pick_Note_Number] as Pick_Note_Number
	  ,Null as Carrier
	  ,Null as Out_Of_Stock
	  ,CASE when Location_Type = 'C' THEN 'Y' Else 'N' end as Controlled_Line
	  ,ps.Line_Quantity + Discrepancy as Expected_Quantity
	  ,ps.[Line_Quantity] as Actual_Quantity
	  ,Location as Destination
	  ,'RF_Picking' as Licence_Plate
      ,ps.Invoice_Line_Number as Pick_Note_Line_Number
     ,Null as [Expiry_Date]
	 ,Null as Expiry_Capture
	 ,Null as Expiry_Warning
	 ,Pick_Flag as Exception_Flag
	 ,Location_Type
	 ,Null as GTIN
	 ,null as Batch_Number
	  ,CASE WHEN Start_Time ='0' THEN NULL ELSE CAST(ps.Processing_Date as datetime) + CAST(DATEADD(ms,Start_Time*1000,0) as datetime) END as Start_Time
	  ,CASE WHEN End_Time = '0' THEN NULL ELSE CAST(ps.Processing_Date as datetime) + cast(DATEADD(ms,End_Time*1000,0) AS datetime) END as End_Time
	  ,Duration as Duration_Seconds
	  ,ot.Order_Type_Detail + ' Picking' as Task_Type
	  ,[Tote]  
	  ,[Station]
	   ,c.[Account_Branch]
      ,c.[Account_Number]
	  ,Account_Name
	  ,'R' as Source
  FROM [SupplyChainReporting].[dbo].[aah_picking_statistics] ps
	Left join SupplyChainReporting.dbo.b2b_invoices bi on ps.Cust_Key_C = bi.Cust_Key_C and ps.Print_Branch = bi.Print_Branch 
	and  (ps.Pick_Note_Number = bi.Pick_Note_No or ps.Pick_Note_Number = bi.Original_Order_Number)
	and ps.Processing_Date = Convert(Date,bi.Processing_Date) and ps.Prod_Key_C = bi.Prod_Key_C 
	left join AAHOperations.dbo.lu_order_type_marker_details ot on bi.Order_Type_Marker = ot.Order_Type_Marker and bi.Print_Branch =ot.Print_Branch
	left join SupplyChainReporting.dbo.product_list pl on ps.Prod_Key_C= pl.Prod_Key_C
	left join SupplyChainReporting.dbo.b2b_customers c on ps.Cust_Key_C = c.Cust_Key_C
  where ps.Processing_Date >= CONVERT (date,GETDATE()-21) and Pick_Flag in ('P','S') 
  and ps.Print_Branch in ('106m','201D' ,'305m','405n','406p' ,'501g' ,'504m', '602j','703m' ,'706s','908b','408t')
  and Bi.Process_Month >= convert(date,GETDATE()-63)"))

##  UNION

WCS_Import = sqlQuery(con, paste("SELECT [Print_Branch]
      ,[Rec_Type]
      ,Processing_Date
      ,trim([User_ID]) as User_ID
	  ,Product_Code
	  ,Product_Description
	  ,Pack_Size
	  ,Full_Supplier
	  ,Report_Group
	  ,Product_Group
	  ,Umbrella_Group
	  ,Product_Category_ID
		/*,CASE WHEN Start_Time ='0' THEN NULL ELSE datepart(hour,convert(varchar,DATEADD(ms,Start_Time*1000,0),108)) END as Start_Hour */
      ,[SR_Number] as Pick_Note_Number
      ,[Carrier]
      ,[Out_Of_Stock]
      ,[Controlled_Line]
      ,[Expected_Quantity]
      ,[Actual_Quantity]
      ,[Destination]
      ,[Licence_Plate]
	  ,'0' as Pick_Note_Line_Number
      ,[Expiry_Date]
      ,[Expiry_Capture]
      ,[Expiry_Warning]
	  ,[Exception_Flag]
      ,wcs.[Location_Type]
      ,[GTIN]
      ,[Batch_Number]
      ,CASE WHEN Start_Time ='0' THEN NULL ELSE CAST(Processing_Date as datetime) + CAST(DATEADD(ms,Start_Time*1000,0) as datetime) END as Start_Time
		,CASE WHEN End_Time = '0' THEN NULL ELSE CAST(Processing_Date as datetime) + cast(DATEADD(ms,End_Time*1000,0) AS datetime) END as End_Time
		,Duration as Duration_Seconds
	  ,CASE when Rec_Type in ('WCHA','WCHB','WCHI','RDOC') then 'Goods Inwards'
			when Rec_Type in ('WGIP','WPTL','WPUT') then 'Putaway'
			when Rec_Type in ('WCDP') then 'Credit Putaway'
			when Rec_Type in ('WHQR','WMRT','WMST','WPRI','WRET','WRST') then 'Pallet Rack Movement'
			when Rec_Type in ('BPIK') then 'Picking'
			when Rec_Type in ('WOSR') then 'OSR Putaway'
			when Rec_Type in ('WDZM') then 'Drop Zone Movement'
			Else 'Admin' END as Task_Type
		,'0' as Tote
		,Null as Station
	,Null as Account_Branch
      ,Null as  Account_Number
	  ,Null as Account_Name
	  ,'WCS' as Source
  FROM [SupplyChainReporting].[dbo].[wcs_statistics] wcs
  left join SupplyChainReporting.dbo.product_list pl on wcs.Product_Key_C= pl.Prod_Key_C
  Left Join SupplyChainReporting.dbo.lu_wcs_task_code lu on wcs.Rec_Type = lu.Task_Code
  where wcs.Print_Branch in ('106m','201D' ,'305m','405n','406p' ,'501g' ,'504m', '602j','703m' ,'706s','908b','408t')
  and Processing_Date >= DATEADD(day,-21, getdate())"))

HQ_Pull_Import = sqlQuery(con, paste("  SELECT [Processing_Date]
	  ,CAST([Processing_Date] AS DATETIME) + CAST([Time_Id] AS DATETIME) AS FullDateTime
      ,[Branch_Id]
      ,[Date_Id]
      ,[Time_Id]
      ,[Log_Type]
      ,[Sequence]
      ,[Log_Message]
	   FROM [SupplyChainReporting].[dbo].[branch_security_log]
  Where Date_Id >= DATEADD(DAY,-5, getdate())  
  and Log_Type = 'HQR'
  and Log_Message not like '%Account%'
  ORDER BY CAST([Processing_Date] AS DATETIME) + CAST([Time_Id] AS DATETIME)"))


# Use more CPU where possible
data.table::setDTthreads(parallel::detectCores() - 1)

##WCS_Shift_Date_Pick_Path = sqlQuery(con, paste("select max(Shift_date) as Max_Shift_Date_Pick_Path
##  FROM [AAHOperations].[dbo].[wcs_pickpath]"))

##

WCS_Import <- as.data.table(WCS_Import)
Pick_Import <- as.data.table(Pick_Import)
HQ_Pull_Import <- as.data.table(HQ_Pull_Import)
WCS_Day <- rbind(WCS_Import,Pick_Import)
##rm(WCS_Day,Gap_Rows,UNPR_Rows,Merged_Tasks,Picking_Walk)

XY_Coorid <- fread("X:/R Code Extracts/WCS/Import Folder/Circuit XY Bay Coordinates.csv")
##XY_Coorid <- as.data.table(XY_Coorid)
XY_Coorid <- XY_Coorid[,.(Print_Branch,Full_Bay,X_Coord,Y_Coord)]


############## Fixing Data Errors
WCS_Day <- WCS_Day[Duration_Seconds < 0,End_Time := End_Time + 86400]
WCS_Day <- WCS_Day[!is.na(Start_Time),]

WCS_Day <- WCS_Day[, Max_Hour := max(hour(Start_Time)), by = .(User_ID, Processing_Date,Print_Branch)]

########### Filter and Ordering
WCS_Day <- WCS_Day[!(Rec_Type  %in% c('WCHA','WPTL','WDZM','WEXP')),]
WCS_Day <- setorder(WCS_Day, Print_Branch,User_ID,Start_Time)

########## Create Shift Date for overnight processing 
WCS_Day[,Shift_Date := Processing_Date
][hour(End_Time) <= 5 ,Shift_Date := Processing_Date-1
][(hour(End_Time) >= 5 & Max_Hour %in% c(8,9,10,11,12,13,14,15,16,17,18)),Shift_Date := Processing_Date
]

WCS_Day <- WCS_Day[Shift_Date >= min(Processing_Date),]

WCS_Day <- WCS_Day[, Next_Start := shift(Start_Time, type = "lead"), by = c('User_ID','Print_Branch','Shift_Date')]
WCS_Day <- WCS_Day[, Previous_Start := shift(Start_Time, type = "lag"), by = c('User_ID','Print_Branch','Shift_Date')]
WCS_Day <- WCS_Day[, Next_Task := shift(Task_Type, type = "lead"), by = c('User_ID','Print_Branch','Shift_Date')]
WCS_Day <- WCS_Day[, Previous_Task := shift(Task_Type, type = "lag"), by = c('User_ID','Print_Branch','Shift_Date')]

############### Fix Issue with 
WCS_Day[Next_Start <= Start_Time, Next_Start := End_Time]


##Finding the Gap Time Between task
WCS_Day[, Orginal_Gap := as.numeric(difftime(Next_Start, End_Time, units = "secs"))
][Start_Time == Next_Start, Orginal_Gap := 0]

# Adjust for overnight shifts (Next_Start is earlier than End_Time)
WCS_Day[Orginal_Gap < 0, Orginal_Gap := as.numeric(difftime(Next_Start + 86400, End_Time, units = "secs"))]


#### Insert UNPR in gaps > 600
##Step 1: Identify rows with gap > 600 seconds
##Gap_Rows <- WCS_Day[Orginal_Gap > 600 & Previous_Start != Start_Time]
##Step 1: Identify rows with gap > 300 seconds
Gap_Rows <- WCS_Day[(Orginal_Gap > 300 & Orginal_Gap <= 21600) & Previous_Start != Start_Time & !hour(Start_Time) %in% c(3:4)]
## Step 2: Create UNPR rows
UNPR_Rows <- Gap_Rows[, .(
  User_ID,
  Print_Branch,
  Start_Time = End_Time,
  End_Time = Next_Start,
  Rec_Type = 'UNPR',
  Task_Type = 'UNPR',
  Processing_Date,
  Shift_Date
)]

# Step 3: Combine original and UNPR rows
WCS_Day <- rbind(WCS_Day, UNPR_Rows, fill = TRUE)

# Step 4: Reorder by User and Start_Time
setorder(WCS_Day, Print_Branch,User_ID,Shift_Date, Start_Time)

##### step 5 recalc lead and lag
WCS_Day <- WCS_Day[, Next_Start := shift(Start_Time, type = "lead"), by = c('User_ID','Print_Branch','Shift_Date')]
WCS_Day <- WCS_Day[, Previous_Start := shift(Start_Time, type = "lag"), by = c('User_ID','Print_Branch','Shift_Date')]
WCS_Day <- WCS_Day[, Next_Task := shift(Task_Type, type = "lead"), by = c('User_ID','Print_Branch','Shift_Date')]
WCS_Day <- WCS_Day[, Previous_Task := shift(Task_Type, type = "lag"), by = c('User_ID','Print_Branch','Shift_Date')]

############## Recalc Duration

##Finding the Gap Time Between task
WCS_Day[, New_Gap := as.numeric(difftime(Next_Start, End_Time, units = "secs"))
][Start_Time == Next_Start, New_Gap := 0]

WCS_Day[, New_Duration_Seconds := as.numeric(difftime(End_Time,Start_Time, units = "secs"))]

########## Create Shift Date for overnight processing 
WCS_Day[,Shift_Date := Processing_Date
][hour(End_Time) <= 5 ,Shift_Date := Processing_Date-1
][(hour(End_Time) >= 5 & Max_Hour %in% c(8,9,10,11,12,13,14,15,16,17,18)),Shift_Date := Processing_Date
]

##################

##write.csv(WCS_Day, file = "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/WorkSmart/WCS_Day.csv",
##          row.names = FALSE)



########### Grouping Each Task Together
WCS_Day <- WCS_Day[,Task_Group := rleid(Task_Type), by = c('User_ID','Print_Branch','Shift_Date')]


############## where there is over 10 mins gap between task create split and add unproductive task type
# Group and calculate merged duration
Merged_Tasks <- WCS_Day[, .(
  Task_Type = first(Task_Type),
  Lines = (.N),
  Qty = sum(Actual_Quantity),
  Start_Time = min(Start_Time),
  End_Time = max(End_Time),
  Duration = as.numeric(max(End_Time) - min(Start_Time))
), by = .(User_ID,Task_Group,Shift_Date,Print_Branch)]

Merged_Tasks <- Merged_Tasks[, Next_Start := shift(Start_Time, type = "lead"), by = .(Print_Branch,User_ID,Shift_Date)]
Merged_Tasks <- Merged_Tasks[, Next_Task := shift(Task_Type, type = "lead"), by = c('User_ID','Print_Branch','Shift_Date')]
Merged_Tasks <- Merged_Tasks[, Previous_Task := shift(Task_Type, type = "lag"), by = c('User_ID','Print_Branch','Shift_Date')]


# Calculate gaps between groups
Merged_Tasks[, Next_Start := shift(Start_Time, type = "lead"), by = .(User_ID,Shift_Date,Print_Branch)]
Merged_Tasks[, Gap_Start := End_Time]
Merged_Tasks[, Gap_End := Next_Start]
Merged_Tasks[, Gap_Duration := as.numeric(Gap_End - Gap_Start)]


##write.csv(Merged_Tasks, file = "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/WorkSmart/Final_Merged_Task_WCS.csv",
##         row.names = FALSE)

##Merged_Tasks <- fread("C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/WorkSmart/Final_Merged_Task_WCS.csv")
######################
####################### Walking Distance For Circuit Picking\

Picking_Walk <- WCS_Day[Rec_Type == "PICK" & Shift_Date >= Sys.Date() - 5]
Picking_Walk[,Full_Bay :=  fifelse(nchar(trimws(Destination)) == 6, substring(Destination,1,4), substring(Destination,1,3))]


Picking_Walk <- merge(
  Picking_Walk,
  XY_Coorid,
  by = c('Print_Branch','Full_Bay'),
  all.x = TRUE)

Picking_Walk <- Picking_Walk[Print_Branch %in% c('703M','305M','201D','504M','106M') & Task_Type == 'On Circuit Picking' & trimws(Station) %in% c('C','D','E','F','G','H','J','K','L','M','N','P'),]

allowed_y_map <- data.table(
  Print_Branch = c("703M", "305M","201D","504M","106M"),
  Allowed_Y_Paths = list(c(0, -7.9), c(0, -5, -12.9), c(0, -4, -9.9),c(0, -6.9), c(0,-8))  # Example values
)

Picking_Walk <- merge(Picking_Walk, allowed_y_map, by = "Print_Branch", all.x = TRUE)


# Ensure Picking_Walk is a data.table and ordered
setDT(Picking_Walk)
setorder(Picking_Walk, Print_Branch,Shift_Date,User_ID, Station, Pick_Note_Number, Start_Time)

# Add a group ID
Picking_Walk[, Group_ID := .GRP, by = .(Print_Branch,Shift_Date,User_ID,Station,Pick_Note_Number)]

########################
#### FAST PICK PATH (STEP LEVEL) - REPLACES LOOP
########################



# Clean once (avoid repeated trimws during filtering/calcs)
Picking_Walk[, `:=`(
  Station     = trimws(Station),
  Destination = trimws(Destination)
)]

# Ensure numeric coords (important)
Picking_Walk[, `:=`(
  X_Coord = as.numeric(X_Coord),
  Y_Coord = as.numeric(Y_Coord)
)]

# Optional but highly recommended: reduce float uniqueness (your allowed paths are 1dp)
# Prevents millions of unique Y values due to floating point noise
##Picking_Walk[, Y_Coord := round(Y_Coord, 1)]

# Define grouping columns (same logic as your Group_ID)
gcols <- c("Print_Branch","Shift_Date","User_ID","Station","Pick_Note_Number")

# Ensure correct order
setorder(Picking_Walk, Print_Branch, Shift_Date, User_ID, Station, Pick_Note_Number, Start_Time)

# Compute step legs within each group:
# From = previous location (or origin 0,0 for first row), To = current row coords
Picking_Walk[, `:=`(
  From_X = shift(X_Coord, type = "lag", fill = 0),
  From_Y = shift(Y_Coord, type = "lag", fill = 0),
  To_X   = X_Coord,
  To_Y   = Y_Coord
), by = gcols]

# Manhattan distance
Picking_Walk[, Manhattan := abs(To_X - From_X) + abs(To_Y - From_Y)]

# Build ONE end-leg per group: last location -> origin (0,0)
end_legs <- Picking_Walk[, .SD[.N], by = gcols][, .(
  Print_Branch, Shift_Date, User_ID, Station, Pick_Note_Number,
  From_X = To_X,
  From_Y = To_Y,
  To_X = 0,
  To_Y = 0,
  Destination = "End",
  Product_Code = NA_character_,
  Product_Description = NA_character_,
  Expected_Quantity = NA_real_,
  Actual_Quantity = NA_real_,
  Start_Time = End_Time,
  End_Time = End_Time
)]

end_legs[, Manhattan := abs(To_X - From_X) + abs(To_Y - From_Y)]

# Combine legs (step-level rows + end legs)
coords <- rbindlist(list(
  Picking_Walk[, .(
    Print_Branch, Shift_Date, User_ID, Station, Pick_Note_Number,
    From_X, From_Y, To_X, To_Y,
    Manhattan,
    Destination, Product_Code, Product_Description,
    Expected_Quantity, Actual_Quantity,
    Start_Time, End_Time
  )],
  end_legs
), use.names = TRUE, fill = TRUE)

# ---- Nearest allowed Y-path mapping (vectorised) ----

# Function to map y -> nearest allowed path (vectorised)
nearest_allowed_vec <- function(y, allowed) {
  allowed <- sort(allowed)
  mids <- (head(allowed, -1) + tail(allowed, -1)) / 2
  idx <- findInterval(y, mids) + 1L
  allowed[idx]
}

# Ensure allowed_y_map is a data.table (it is in your code) and contains Allowed_Y_Paths list-column
setDT(allowed_y_map)

# Build unique Y values we need to map (From_Y and To_Y), per branch
uy <- unique(rbind(
  coords[, .(Print_Branch, Y = From_Y)],
  coords[, .(Print_Branch, Y = To_Y)]
))
uy <- uy[!is.na(Y)]
uy[, Y := round(Y, 1)]  # keep consistent

# Map each unique Y to nearest allowed path per branch
uy[, Nearest := {
  allowed <- allowed_y_map[Print_Branch == .BY$Print_Branch, Allowed_Y_Paths][[1]]
  nearest_allowed_vec(Y, allowed)
}, by = Print_Branch]

setkey(uy, Print_Branch, Y)

# Join nearest values back
coords[, `:=`(
  From_Y_r = round(From_Y, 1),
  To_Y_r   = round(To_Y, 1)
)]

coords[uy, Nearest_Y_From := i.Nearest, on = .(Print_Branch, From_Y_r = Y)]
coords[uy, Nearest_Y_To   := i.Nearest, on = .(Print_Branch, To_Y_r   = Y)]

# Override rule: if either is on Y=0, use Manhattan and nearest values = 0
coords[From_Y == 0, Nearest_Y_From := 0]
coords[To_Y   == 0, Nearest_Y_To   := 0]

# Identify if From_Y is exactly an allowed path
allowed_y_long <- allowed_y_map[, .(Y_Path = unlist(Allowed_Y_Paths)), by = Print_Branch]
setkey(allowed_y_long, Print_Branch, Y_Path)
coords[, Allowed_From := !is.na(allowed_y_long[.(Print_Branch, From_Y), Y_Path])]

# ---- Your realistic distance logic (vectorised) ----
coords[, Distance := fifelse(
  (To_Y == 0 | From_Y == 0),
  Manhattan,  # override: use Manhattan if either end is on Y=0
  fifelse(
    From_X == To_X,
    Manhattan,  # vertical movement only
    fifelse(
      (From_Y == To_Y & Allowed_From),
      abs(To_X - From_X),  # direct horizontal movement
      abs(From_Y - Nearest_Y_From) + abs(To_X - From_X) + abs(To_Y - Nearest_Y_From)
    )
  )
)]

# Final step-level output (same “shape” as yours)
Pick_Path_Table <- coords[, .(
  Shift_Date, User_ID, Station, Print_Branch, Pick_Note_Number,
  From_X, From_Y, To_X, To_Y,
  Nearest_Y_From, Nearest_Y_To,
  Manhattan, Distance,
  Destination = trimws(Destination),
  Product_Code, Product_Description,
  Expected_Quantity, Actual_Quantity,
  Start_Time, End_Time
)]

# Cleanup helper cols
##Pick_Path_Table[, c("From_Y_r","To_Y_r","Allowed_From") := NULL]

########################
#### OPTIONAL “EVEN FASTER” OUTPUT (SUMMARY) - does NOT replace step-level
#### Useful for QA/reporting + faster PowerBI visuals
########################

Pick_Path_Summary <- Pick_Path_Table[, .(
  Steps = .N,
  Total_Manhattan = sum(Manhattan, na.rm = TRUE),
  Total_Distance  = sum(Distance,  na.rm = TRUE)
), by = .(Print_Branch, Shift_Date, User_ID, Station, Pick_Note_Number)]



################### Holding Queue Pulls
###################
setorder(HQ_Pull_Import, Branch_Id,FullDateTime,Sequence)
HQ_Pull_Grouped <- HQ_Pull_Import[, Group := cumsum(str_detect(Log_Message, "REQUEST"))]

# Extract MERGE and RESULT messages per group
merge_msgs <- HQ_Pull_Import[str_detect(Log_Message, "MERGE"), .(MERGE_Message = Log_Message[1]), by = Group]
result_msgs <- HQ_Pull_Import[str_detect(Log_Message, "RESULT"), .(RESULT_Message = Log_Message[1]), by = Group]

# Filter only rows with 'Request'
HQ_Pull__Request <- HQ_Pull_Import[str_detect(Log_Message, "REQUEST")]

# Merge in the MERGE and RESULT messages
HQ_Pull__Request <- merge(HQ_Pull__Request, merge_msgs, by = "Group", all.x = TRUE)
HQ_Pull__Request <- merge(HQ_Pull__Request, result_msgs, by = "Group", all.x = TRUE)

HQ_Pull__Request[is.na(MERGE_Message), MERGE_Message := "Not found"]
HQ_Pull__Request[is.na(RESULT_Message), RESULT_Message := "Not found"]

HQ_Pull__Request[, Order_Type := str_extract(Log_Message, "(?<=\\[)[A-Za-z]+(?=\\])")]
HQ_Pull__Request[, Van_Number := str_extract(Log_Message, "(?<=Van )\\d+(?=,)")]
HQ_Pull__Request[, Tote_Released := str_extract(MERGE_Message, "(?<=totes \\[)\\d+(?=\\])")]
HQ_Pull__Request[is.na(Tote_Released), Tote_Released := str_extract(RESULT_Message, "(?<=Rel   )\\d+")]
HQ_Pull__Request[, Work_Station_ID := str_extract(RESULT_Message, "\\S+$")]
HQ_Pull__Request[, Request := str_extract(Log_Message, "\\S+$")]



########################## makes changes to DB
## 7) Output to Azure SQL (append)
## -----------------------------
con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "ODBC Driver 17 for SQL Server",
                      Server   = "sqldwhscuks0001.ba8e81fcf26b.database.windows.net",
                      Database = "AAHOperations",
                      UID      = "Harriesr",
                      PWD      = "G00ners4everywW",
                      Port     = 1433)

on.exit(DBI::dbDisconnect(con), add = TRUE)


freeze_as_utc <- function(dt) {
  is_time <- vapply(dt, inherits, logical(1), "POSIXct")
  cols <- names(dt)[is_time]
  if (length(cols)) {
    # 'force_tz' keeps the displayed clock time and changes the underlying instant
    dt[, (cols) := lapply(.SD, lubridate::force_tz, tzone = "UTC"), .SDcols = cols]
  }
  invisible(dt)
}

# Apply before dbWriteTable if you need wall-clock times preserved
freeze_as_utc(HQ_Pull__Request)
freeze_as_utc(Pick_Path_Table)
freeze_as_utc(Merged_Tasks)
freeze_as_utc(WCS_Day)



# Delete rows older than 5 days from each table
dbExecute(con, "
  DELETE FROM AAHOperations.dbo.branch_security_holding_queue
  WHERE Processing_Date >= DATEADD(DAY, -5, GETDATE());
")

dbExecute(con, "
  DELETE FROM AAHOperations.dbo.wcs_pickpath
  WHERE Shift_Date >= DATEADD(DAY, -5, GETDATE());
")



DBI::dbWithTransaction(con, {
  DBI::dbWriteTable(con, name = SQL("AAHOperations.dbo.branch_security_holding_queue"),
                    value = HQ_Pull__Request, append = TRUE, row.names = FALSE)
  DBI::dbWriteTable(con, name = SQL("AAHOperations.dbo.wcs_pickpath"),
                    value = Pick_Path_Table, append = TRUE, row.names = FALSE)
  DBI::dbWriteTable(con, name = SQL("AAHOperations.dbo.wcs_mapping_grouped_tasks"),
                    value = Merged_Tasks, overwrite = TRUE, row.names = FALSE)
  DBI::dbWriteTable(con, name = SQL("AAHOperations.dbo.wcs_mapping_all_task"),
                    value = WCS_Day, overwrite = TRUE, row.names = FALSE)
})

##Sys.time() - t1
Sys.time() - t1
##write.csv(Pick_Path_Summary, file = "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/WorkSmart/Pick_Path_Summary_new.csv",
 ##         row.names = TRUE)

##append