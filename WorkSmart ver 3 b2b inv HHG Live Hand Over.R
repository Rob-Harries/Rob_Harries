rm(list = ls()[!ls() %in% c("Sales_Import","Main_Query","Main_Query_Max","Main_Query_Max_Relex","Main_Query_Ave_Demand",
                            "Product_Data","Auto_Locations","Auto_Overstock","Local_Dim","Pallet_Rack","Aframe_Summary","Main_Query_Before","Auto_Move_Check_Before","Auto_Overstock_Before")])


setwd('C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/')

library(RODBC); library(data.table); library(lubridate); library(foreach); library(plyr); library(dplyr); library(matrixStats); library(stringr); library(hms); library(DBI); library(odbc);
library(stringi)

##R package tstools has the ability to run ABC and XYZ analysis library(tstools)
##https://kourentzes.com/forecasting/2016/10/15/abc-xyz-analysis-for-forecasting/



connection_function <- function() {
  
  password_file <- "C:/Users/harriesr/credentials/azure_supply_chain.txt"
  
  if (!file.exists(password_file)) {
    stop("Password file not found: ", password_file, call. = FALSE)
  }
  
  pwd <- trimws(readLines(
    password_file,
    n = 1L,
    warn = FALSE
  ))
  
  if (!nzchar(pwd)) {
    stop("The password file is empty.", call. = FALSE)
  }
  
  con <- RODBC::odbcConnect(
    dsn = "Azure Supply Chain",
    uid = "Harriesr",
    pwd = pwd
  )
  
  rm(pwd)
  
  if (identical(con, -1)) {
    stop("Database connection failed.", call. = FALSE)
  }
  
  con
}

con <- connection_function()

Sales_Import = sqlQuery(con, paste("select d.Print_Branch, convert(date,Processing_Date) as Date, Product_Code, count(Line_Quantity) as Number_lines,sum(Line_Quantity) as Line_Quantity,
		                                  d.Order_Type_Marker,Company_ID,
										  CASE When Warehouse_Zone = 'Auto OverStock' and d.Order_Type_Marker = 'G' Then 'Auto Overstock'
										  when Location_ID in ('CLA43',	'CLG12',	'CRB44',	'DRB51',	'DRD11',	'DRF22',	'ELA11',	'ELG12',	'ERH11'
                                                                             ,	'ERH12',	'FLA12',	'GLE22',	'GRH45',	'HLE11',	'HLE41',	'HRB12')
										  and d.Print_Branch = '201D' and d.Order_Type_Marker = 'G' Then 'Auto Overstock'
										  ELSE otm.Order_Type_Zone END as Warehouse_Zone,
                                      CASE
                                        WHEN d.Print_Branch = Account_Branch THEN 'Local'
                                        ELSE 'Switch'
                                        END AS Local_switch
                                    from SupplyChainReporting.dbo.b2b_invoices d
                                      join SupplyChainReporting.dbo.product_list p on d.Prod_Key_C = p.Prod_Key_C
                                      join SupplyChainReporting.dbo.b2b_customers c on d.Cust_Key_C = c.Cust_Key_C
                                      join AAHOperations.dbo.lu_warehouse_zones w on left(d.Location_ID,1) = w.Station and d.Print_Branch = w.Branch
									  Join AAHOperations.dbo.lu_order_type_marker_details otm on d.Order_Type_Marker = otm.Order_Type_Marker and d.Print_Branch = otm.Print_Branch
                                    where convert(date,Processing_Date) >= DATEADD(day,-29, getdate())
                                 --     WHERE convert(date,Processing_Date) between '2024-09-01' and '2024-09-05'
                                      and D.Print_Branch in ('106m','201D' ,'305m','408t','405n','406p','501g','504m','602j','703m','706s','908b','408T')
                                      and Credit_Note_Analysis_Marker ='0'
                                      and Omit_Marker = '0'
									  and Product_Code   Not in ('CUS0927W','LLO3350Q','AAH0081P','EME0088T','DEL0432N','TRI0866V','LOW0008F','LOW0009B', 'FIN0507E', 'FUE0001X') 
                                      and Umbrella_Group  Not in ('6','8','12','13','14','15','16','17','18','19','22','23') 
                                      and Process_Month >= DATEADD(Month,-3, getdate())
                                    group by D.Print_Branch,Company_ID,convert(date,Processing_Date),Product_Code,d.Order_Type_Marker,
									(CASE When Warehouse_Zone = 'Auto OverStock' and d.Order_Type_Marker = 'G' Then 'Auto Overstock'
										  when Location_ID in ('CLA43',	'CLG12',	'CRB44',	'DRB51',	'DRD11',	'DRF22',	'ELA11',	'ELG12',	'ERH11'
                                                                             ,	'ERH12',	'FLA12',	'GLE22',	'GRH45',	'HLE11',	'HLE41',	'HRB12')
										  and d.Print_Branch = '201D' and d.Order_Type_Marker = 'G' Then 'Auto Overstock'
										  ELSE otm.Order_Type_Zone END),
                                      (CASE
                                        WHEN d.Print_Branch = Account_Branch THEN 'Local'
                                        ELSE 'Switch'
                                        END)"))


Main_Query = sqlQuery(con, paste("SELECT date, mq.Branch as Print_Branch, Product_Code, Location_Code,First_Receipt_Date,Replenish_Quantity,
                                      Current_Shelf_Balance, Current_Order_Balance,Station,Warehouse_Zone,Warehouse_Area,Tote_Delivery, Channel_Type, Aframe_Number
                                   FROM [SupplyChainReporting].[dbo].[main_query] mq
                                    Left join AAHOperations.dbo.lu_warehouse_zones wz on left(mq.Location_Code,1) = wz.Station and mq.Branch = wz.Branch
                                    Left Join AAHOperations.dbo.lu_ws_aframe_long_short af on left(mq.Location_Code,5) = Aframe_Sector and mq.Branch = Branch_ID
                                   WHERE mq.Branch in ('106m','201D' ,'305m','408t','405n','406p','501g','504m','602j','703m','706s','908b','408T')
                                    and Date >= DATEADD(day,-1, getdate())"))

Main_Query_Max = sqlQuery(con, paste("SELECT Branch as Print_Branch,Product_Code,Max(Current_Shelf_Balance) as Max_Shelf_Balance,Max(Demand) As Max_Demand
                                        FROM SupplyChainReporting.dbo.main_query
                                        Where Date >= DATEADD(day,-63, getdate())
                                          and Branch in ('106m','201D' ,'305m','408t','405n','406p','501g','504m','602j','703m','706s','908b','408T')
                                        GROUP BY Branch, Product_Code"))

Main_Query_Max_Relex = sqlQuery(con, paste("SELECT PRODUCT_CODE as Product_Code,
                                                LOCATION as Print_Branch,
                                                Max(BALANCE) As Max_Stock_Relex
                                              FROM SupplyChainReporting.dbo.relex_projected_inventory
                                              Where Convert(date,PROJECTED_INVENTORY_DATE) >= DATEADD(day,-61, getdate()) and Convert(date,PROJECTED_INVENTORY_DATE) <= DATEADD(day,+61, getdate())
                                              GROUP BY PRODUCT_CODE, LOCATION"))

Main_Query_Ave_Demand = sqlQuery(con, paste("SELECT Location as Print_Branch,trim(Product_Code) as Product_Code
                                                 ,(sum(Effective_Quantity)/(8.57/2)) as Relex_Ave_Demand
                                                  FROM SupplyChainReporting.dbo.relex_forecast
                                                WHERE Convert(date,Forecast_Date) >= DATEADD(day,+1, getdate()) and Convert(date,Forecast_Date) <= DATEADD(day,+61, getdate())
                                                GROUP BY Location,trim(Product_Code)"))



Product_Data = sqlQuery(con, paste("SELECT Product_Code, Product_Description, Pack_Size, Product_Group, Report_Group,Location_Type, 
                                     Full_Supplier, Umbrella_Group, Insertion_Date,Discontinued_Date,Hospital_Restriction,Height,width as Width,Depth
                                    FROM [SupplyChainReporting].[dbo].[product_list] pl"))

Auto_Locations = sqlQuery(con, paste("SELECT Print_Branch,pl.Product_Code
                                          ,Count(Location_Code) as Number_of_Auto_Channels
                                      	  ,Max(Assembly_Qty) as Assembly_Qty
                                      	  ,Sum(Maximum_Qty) as Total_Auto_Stock_Holding
                                        FROM SupplyChainReporting.dbo.aah_location al
                                        Left Join SupplyChainReporting.dbo.product_list pl on al.Prod_Key_C = pl.Prod_Key_C
                                        Where convert(Date,al.Update_Timestamp) >= DATEADD(day,-1, getdate())
                                          and Left(Location_Code,2) in ('A-','B-')
                                        GROUP BY Print_Branch,pl.Product_Code"))

Auto_Overstock = sqlQuery(con, paste("SELECT Print_Branch,pl.Product_Code
                                      	,Max(Location_Code) as OverStock_Location
                                      	,Max(Maximum_Qty) as Overstock_STD
                                      FROM SupplyChainReporting.dbo.aah_location al
                                      	Left Join SupplyChainReporting.dbo.product_list pl on al.Prod_Key_C = pl.Prod_Key_C
                                      Where convert(Date,al.Update_Timestamp) >= DATEADD(day,-1, getdate())
                                      	and Print_Branch in ('106m','201d','305m','405n','406p','501g','504m','703m','706s')
                                      	and Manual_Auto = 'M'
                                      	or (Print_Branch = '405n' and Manual_Auto = 'M' and left(Location_Code,1) = 'C')
                                      GROUP BY Print_Branch,pl.Product_Code"))

Local_Dim = sqlQuery(con, paste("SELECT Branch,trim(ld.Product_Code) as Product_Code,ld.Product_Height as Local_H,ld.Product_Width as Local_W,ld.Product_Depth as Local_D 
                                FROM SupplyChainReporting.dbo.product_local_dimensions ld
                                Join SupplyChainReporting.dbo.product_list pl on trim(ld.Product_Code) = pl.Product_Code
                                Where (ld.Product_Height is not null and ld.Product_Height between '5' and'550')
                                and (ld.Product_Width is not null and ld.Product_Width between '5' and'550')
                                and (ld.Product_Depth is not null and ld.Product_Depth between '5' and'550')
                                and Branch in ('106m','201D','305m','406p','501g','405n','504m','703m','706s')"))

Pallet_Rack = sqlQuery(con, paste("SELECT Branch as Print_Branch
                                          ,Product_Code
                                          ,count(Location) as Count_PR_Location
                                          ,Sum(Quantity) as PR_Qty
										  ,mAX(Quantity) as PR_Qty_MAX
                                    FROM [SupplyChainReporting].[dbo].[pallet_racking_daily_snapshot]
                                    wHERE Product_Code is not Null
                                    GROUP BY Branch
                                        ,Product_Code
                                    ORder BY Product_Code, Branch"))

Aframe_Summary = sqlQuery(con, paste("SELECT Branch_ID,Channel_Type,Channel_Count,Channel_Length", 
                                     "FROM [AAHOperations].[dbo].[lu_ws_aframe_summary]"))

Main_Query_Before = sqlQuery(con, paste("SELECT date as Date_Before, mq.Branch as Print_Branch, Product_Code, Location_Code as Location_Code_Before ,
                                     Warehouse_Zone as Warehouse_Zone_Before ,Warehouse_Area as Warehouse_Area_Before, Channel_Type as Channel_Type_Before
                                   FROM [SupplyChainReporting].[dbo].[main_query] mq
                                    Left join AAHOperations.dbo.lu_warehouse_zones wz on left(mq.Location_Code,1) = wz.Station and mq.Branch = wz.Branch
                                    Left Join AAHOperations.dbo.lu_ws_aframe_long_short af on left(mq.Location_Code,5) = Aframe_Sector and mq.Branch = Branch_ID
                                   WHERE mq.Branch in ('101c','106m','201d','204j','305m','405n','406p','501g','504m','602j','703m','706s','908b','408t' ,'606R')
                                    and (Date >= DATEADD(day,-8, getdate()) and Date < DATEADD(day,-7, getdate()))"))

Auto_Move_Check_Before = sqlQuery(con, paste("SELECT Print_Branch,pl.Product_Code
                                          ,Count(Location_Code) as Number_Auto_Channels_Before
                                      	  ,Max(Assembly_Qty) as Assembly_Qty_Before
                                      	  ,Sum(Maximum_Qty) as Total_Aframe_Holding_Before
                                        FROM SupplyChainReporting.dbo.aah_location al
                                        Left Join SupplyChainReporting.dbo.product_list pl on al.Prod_Key_C = pl.Prod_Key_C
                                        Where (convert(Date,al.Update_Timestamp) >= DATEADD(day,-8, getdate()) and convert(Date,al.Update_Timestamp) < DATEADD(day,-7, getdate()))
                                          and Left(Location_Code,2) in ('A-','B-')
                                        GROUP BY Print_Branch,pl.Product_Code"))


Auto_Overstock_Before = sqlQuery(con, paste("SELECT Print_Branch,pl.Product_Code
                                      	,Max(Location_Code) as OverStock_Location_Before
                                      	,Max(Maximum_Qty) as Overstock_STD_Before
                                      FROM SupplyChainReporting.dbo.aah_location al
                                      	Left Join SupplyChainReporting.dbo.product_list pl on al.Prod_Key_C = pl.Prod_Key_C
                                      Where (convert(Date,al.Update_Timestamp) >= DATEADD(day,-8, getdate()) and convert(Date,al.Update_Timestamp) < DATEADD(day,-7, getdate()))
                                      	and Print_Branch in ('106m','201d','305m','405n','406p','501g','504m','703m','706s')
                                      	and Manual_Auto = 'M'-- and Location_Code > 'U'
                                      	or (Print_Branch = '405n' and Manual_Auto = 'M' and left(Location_Code,1) = 'C')
                                      GROUP BY Print_Branch,pl.Product_Code"))


############################# Restart here
gc()
Aframe_Summary = data.table(Aframe_Summary)

##SDA_Not_Suitable <- read.csv("C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Worksmart/SDA_NotSuitable.csv")
##OSR_Not_Suitable <- read.csv("C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Worksmart/OSR_Not_Suitable.csv")
##Slotting_Param <- read.csv("C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/AAH_Slot_Configuration.csv")

SDA_Not_Suitable <- read.csv("X:/R Code Extracts/Worksmart/Import Folder/SDA_NotSuitable.csv")
OSR_Not_Suitable <- read.csv("X:/R Code Extracts/Worksmart/Import Folder/OSR_Not_Suitable.csv")
Slotting_Param <- read.csv("X:/R Code Extracts/Worksmart/Import Folder/AAH_Slot_Configuration.csv")


Slotting_Param = as.data.table(Slotting_Param)
SDA_Not_Suitable = as.data.table(SDA_Not_Suitable)

Global_Param <- data.table(Attribute = c("Days_Divide"),
                           Metric = c(19))


Sales_Import = as.data.table(Sales_Import)

On_Circuit_sales_Local <- Sales_Import[Local_switch == 'Local' & !(Warehouse_Zone %in% .('Auto Overstock','Bulk'))]
On_Circuit_sales_Switch <- Sales_Import[Local_switch == 'Switch' & !(Warehouse_Zone %in% .('Auto Overstock','Bulk'))]
AutoBulk_sales_Local  <- Sales_Import[Local_switch == 'Local' & Warehouse_Zone %in% .('Bulk')]
AutoBulk_sales_Switch  <- Sales_Import[Local_switch == 'Switch'  &  Warehouse_Zone %in% .('Bulk')]
AFrameMan_sales_Local <- Sales_Import[Local_switch == 'Local' & Warehouse_Zone == 'Auto Overstock']
AFrameMan_sales_Switch <- Sales_Import[Local_switch == 'Switch' & Warehouse_Zone == 'Auto Overstock']



####################################
#creates data.tables 
L_ON_C_DT = as.data.table(On_Circuit_sales_Local)
S_ON_C_DT = as.data.table(On_Circuit_sales_Switch)
L_AB_DT = as.data.table(AutoBulk_sales_Local)
S_AB_DT = as.data.table(AutoBulk_sales_Switch)
L_AFM_DT = as.data.table(AFrameMan_sales_Local)
S_AFM_DT = as.data.table(AFrameMan_sales_Switch)
Auto_Locs = as.data.table(Auto_Locations)
Auto_Overstock = as.data.table(Auto_Overstock)
##Product_Data = as.data.table(Product_Data)


### remove Old DF's
rm(On_Circuit_sales_Local,On_Circuit_sales_Switch,AutoBulk_sales_Local,AutoBulk_sales_Switch,AFrameMan_sales_Local,AFrameMan_sales_Switch)


### ---------------------------
### 1. Local Dimensions
### ---------------------------

Local_Dim <- as.data.table(Local_Dim)

# Calculate row-wise Max, Min, Median dimension
Local_Dim_Ave <- Local_Dim[, .(
  MaxDim_L = pmax(Local_H, Local_W, Local_D),
  MinDim_L = pmin(Local_H, Local_W, Local_D),
  MedDim_L = Local_H + Local_W + Local_D - pmax(Local_H, Local_W, Local_D) - pmin(Local_H, Local_W, Local_D)
), by = .(Product_Code, Branch, Local_H, Local_W, Local_D)]

# Aggregate to product level: median of Max, Min, Med
Local_Dim_Ave <- Local_Dim_Ave[, .(
  Ave_Max = round(median(MaxDim_L, na.rm = TRUE)),
  Ave_Min = round(median(MinDim_L, na.rm = TRUE)),
  Ave_Med = round(median(MedDim_L, na.rm = TRUE))
), by = .(Product_Code)]



### ---------------------------
### 2. National Dimensions
### ---------------------------
Product_Data <- as.data.table(Product_Data)
National_Dim <- Product_Data[, c("Product_Code","Height","Width","Depth"), with = FALSE]
National_Dim = as.data.table(National_Dim)

# Ensure numeric
National_Dim[, `:=`(
  Height = as.numeric(Height),
  Width  = as.numeric(Width),
  Depth  = as.numeric(Depth)
)]

# Calculate row-wise Max, Min, Median
National_Dim <- National_Dim[, .(
  MaxDim_N = pmax(Height, Width, Depth),
  MinDim_N = pmin(Height, Width, Depth),
  MedDim_N = Height + Width + Depth - pmax(Height, Width, Depth) - pmin(Height, Width, Depth)
), by = .(Product_Code, Height, Width, Depth)]



### ---------------------------
### 3. Combine Local & National
### ---------------------------

# Use merge instead of left_join for speed
Local_Dim_Nat <- merge(National_Dim, Local_Dim_Ave, by = "Product_Code", all.x = TRUE)

# Fallback constants
fallback_Max <- 150
fallback_Min <- 55
fallback_Med <- 55

# Source priority
Local_Dim_Nat[, Nat_or_Loc := fifelse(!is.na(Ave_Max), "Local",
                                      fifelse(Height > 1, "National", "NoDATA"))]

# Final dimensions: pick Local, else National, else fallback
Local_Dim_Nat[, Live_Max := fifelse(Nat_or_Loc == "Local", Ave_Max,
                                    fifelse(Nat_or_Loc == "National", MaxDim_N, fallback_Max))]

Local_Dim_Nat[, Live_Min := fifelse(Nat_or_Loc == "Local", Ave_Min,
                                    fifelse(Nat_or_Loc == "National", MinDim_N, fallback_Min))]

Local_Dim_Nat[, Live_Median := fifelse(Nat_or_Loc == "Local", Ave_Med,
                                       fifelse(Nat_or_Loc == "National", MedDim_N, fallback_Med))]



### ---------------------------
### 4. Final Output
### ---------------------------

Final_Dim <- Local_Dim_Nat[, .(
  Product_Code,
  Live_Max,
  Live_Min,
  Live_Median,
  Nat_or_Loc
)]

Final_Dim[, Volume := Live_Max * Live_Min * Live_Median]

# Clean up
rm(Local_Dim_Ave, Local_Dim_Nat, National_Dim)

# write out to Csv
##fwrite(Final_Dim, file = "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Final_Dim.csv",
##       row.names = TRUE)

##fwrite(Final_Dim, file = "X:/R Code Extracts/Worksmart/Output Folder/Final_Dim.csv",
##       row.names = TRUE)

##################################################################################################################################################
####### adds Daily.Averages, Max, Frequency and SD

Days_Divide <- unique(Global_Param[Attribute == "Days_Divide", Metric])

L_ON_C_DT_Agg <- L_ON_C_DT[, .(
  L_Ave_Lines = round(sum(Number_lines, na.rm = TRUE) / Days_Divide, 2),
  L_Ave_Qty   = round(sum(Line_Quantity, na.rm = TRUE) / Days_Divide, 2),
  L_Max_Qty   = max(Line_Quantity, na.rm = TRUE),
  L_Max_Lines = max(Number_lines, na.rm = TRUE),
  L_SD_Lines  = sd(Number_lines, na.rm = TRUE),
  L_SD_Qty    = sd(Line_Quantity, na.rm = TRUE),
  L_Days_Picked = uniqueN(Date)
), by = .(Product_Code, Print_Branch)]


S_ON_C_DT_Agg <- S_ON_C_DT[,.(S_Ave_Lines=round(sum(Number_lines, na.rm = TRUE)/Days_Divide, 2),
                              S_Ave_Qty=round(sum(Line_Quantity, na.rm = TRUE)/Days_Divide, 2),
                              S_Max_Qty=max(Line_Quantity, na.rm = TRUE),
                              S_Max_Lines=max(Number_lines, na.rm = TRUE),
                              S_SD_Lines=sd(Number_lines, na.rm = TRUE),
                              S_SD_Qty=sd(Line_Quantity, na.rm = TRUE),
                              S_Days_Picked=(uniqueN(Date))
), by = .(Product_Code,Print_Branch)]


L_AB_DT_Agg <- L_AB_DT[,.(L_AB_Ave_Lines=round(sum(Number_lines, na.rm = TRUE)/Days_Divide, 2),
                          L_AB_Ave_Qty=round(sum(Line_Quantity, na.rm = TRUE)/Days_Divide, 2),
                          L_AB_Days_Picked=(.N)
), by = .(Product_Code,Print_Branch)]

S_AB_DT_Agg <- S_AB_DT[,.(S_AB_Ave_Lines=round(sum(Number_lines, na.rm = TRUE)/Days_Divide, 2),
                          S_AB_Ave_Qty=round(sum(Line_Quantity, na.rm = TRUE)/Days_Divide, 2),
                          S_AB_Days_Picked=(.N)
), by = .(Product_Code,Print_Branch)]

L_AFM_DT_Agg <- L_AFM_DT[,.(L_AFM_Ave_Lines=round(sum(Number_lines, na.rm = TRUE)/Days_Divide, 2),
                            L_AFM_Ave_Qty=round(sum(Line_Quantity, na.rm = TRUE)/Days_Divide, 2),
                            L_AFM_Days_Picked=(.N)
), by = .(Product_Code,Print_Branch)]

S_AFM_DT_Agg <- S_AFM_DT[,.(S_AFM_Ave_Lines=round(sum(Number_lines, na.rm = TRUE)/Days_Divide, 2),
                            S_AFM_Ave_Qty=round(sum(Line_Quantity, na.rm = TRUE)/Days_Divide, 2),
                            S_AFM_Days_Picked=(.N)
), by = .(Product_Code,Print_Branch)]


######### Remove Not needed Tables
rm(L_ON_C_DT,S_ON_C_DT,L_AB_DT,S_AB_DT,L_AFM_DT,S_AFM_DT)


##### Merge Main Query with Summary Sales Table

Main_Query_Final <- merge(
  as.data.table(Main_Query),
  as.data.table(Product_Data)[, 1:8, with = FALSE],
  by = "Product_Code",
  all.x = TRUE
)
########## Adding Max Stock Data
Main_Query_Final <- merge(
  as.data.table(Main_Query_Final),
  as.data.table(Main_Query_Max),
  by = c('Print_Branch','Product_Code'),
  all.x = TRUE)
######### Adding Max Stock Relex
Main_Query_Final <- merge(
  Main_Query_Final,
  as.data.table(Main_Query_Max_Relex),
  by = c('Print_Branch','Product_Code'),
  all.x = TRUE)
######### Adding Ave Demands
Main_Query_Final <- merge(
  Main_Query_Final,
  as.data.table(Main_Query_Ave_Demand),
  by = c('Print_Branch','Product_Code'),
  all.x = TRUE)

############ Adding Local Dims
Main_Query_Final <- merge(Main_Query_Final,
                          Final_Dim,
                          by = c('Product_Code'),
                          all.x = TRUE)  
############### Adding Auto Locations
Main_Query_Final <- merge(
  Main_Query_Final,
  Auto_Locs,
  by = c('Product_Code','Print_Branch'),
  all.x = TRUE)  

############## Adding OverStock Locations
Main_Query_Final <- merge(
  Main_Query_Final,
  Auto_Overstock,
  by = c('Product_Code','Print_Branch'),
  all.x = TRUE)  
################ Adding Pallet
Main_Query_Final <- merge(
  Main_Query_Final,
  as.data.table(Pallet_Rack),
  by = c('Product_Code','Print_Branch'),
  all.x = TRUE)


################### Creating Temp Table Sales On Circuit
AA_Temp_table_ON <- merge(as.data.table(Main_Query)[,2:3, with = FALSE],
                          L_ON_C_DT_Agg,
                          by = c('Product_Code','Print_Branch'),
                          all.x = TRUE)  
AA_Temp_table_ON <- merge(AA_Temp_table_ON,
                          S_ON_C_DT_Agg,
                          by = c('Product_Code','Print_Branch'),
                          all.x = TRUE)  
################# Creating Temp Tables for Bulk
AA_Temp_table_AB <- merge(as.data.table(Main_Query)[,2:3 , with = FALSE],
                          L_AB_DT_Agg,
                          by = c('Product_Code','Print_Branch'),
                          all.x = TRUE) 
AA_Temp_table_AB <- merge(
  AA_Temp_table_AB,
  S_AB_DT_Agg,
  by = c('Product_Code','Print_Branch'),
  all.x = TRUE)

################### Creating Aframe Manual Picks
AA_Temp_table_AFM <- merge(
  as.data.table(Main_Query)[,2:3 , with = FALSE],
  L_AFM_DT_Agg, by = c('Product_Code','Print_Branch'),
  all.x = TRUE)
AA_Temp_table_AFM <- merge(
  AA_Temp_table_AFM,
  S_AFM_DT_Agg,
  by = c('Product_Code','Print_Branch'),
  all.x = TRUE)


##### change n/a to 0's for numerical Fields##############################################################
# Ensure these are data.tables
setDT(AA_Temp_table_ON)
setDT(AA_Temp_table_AB)
setDT(AA_Temp_table_AFM)
setDT(Main_Query_Final)

replace_na_with_zero <- function(dt, cols) {
  dt[
    , (cols) := lapply(.SD, function(x) {
      x[is.na(x)] <- 0
      x
    }),
    .SDcols = cols
  ]
  invisible(dt)
}

on_cols <- c(
  "L_Ave_Lines", "L_Ave_Qty", "L_Max_Qty", "L_Max_Lines",
  "L_SD_Lines", "L_SD_Qty", "L_Days_Picked",
  "S_Ave_Lines", "S_Ave_Qty", "S_Max_Qty", "S_Max_Lines",
  "S_SD_Lines", "S_SD_Qty", "S_Days_Picked"
)

ab_cols <- c(
  "L_AB_Ave_Lines", "L_AB_Ave_Qty", "L_AB_Days_Picked",
  "S_AB_Ave_Lines", "S_AB_Ave_Qty", "S_AB_Days_Picked"
)

afm_cols <- c(
  "L_AFM_Ave_Lines", "L_AFM_Ave_Qty", "L_AFM_Days_Picked",
  "S_AFM_Ave_Lines", "S_AFM_Ave_Qty", "S_AFM_Days_Picked"
)

main_numeric_cols <- c(
  "Number_of_Auto_Channels", "Total_Auto_Stock_Holding",
  "Assembly_Qty", "Overstock_STD", "Count_PR_Location",
  "PR_Qty", "PR_Qty_MAX"
)

replace_na_with_zero(AA_Temp_table_ON, on_cols)
replace_na_with_zero(AA_Temp_table_AB, ab_cols)
replace_na_with_zero(AA_Temp_table_AFM, afm_cols)
replace_na_with_zero(Main_Query_Final, main_numeric_cols)

# Location codes are text, so handle them separately.
# "0" retains the placeholder used by your existing logic.
Main_Query_Final[
  is.na(OverStock_Location),
  OverStock_Location := "0"
]


Main_Query_Final <- merge(
  Main_Query_Final,
  AA_Temp_table_ON,
  by = c('Product_Code','Print_Branch'),
  all.x = TRUE)  
Main_Query_Final <- merge(
  Main_Query_Final,
  AA_Temp_table_AB,
  by = c('Product_Code','Print_Branch'),
  all.x = TRUE)  
Main_Query_Final <- merge(
  Main_Query_Final,
  AA_Temp_table_AFM,
  by = c('Product_Code','Print_Branch'),
  all.x = TRUE)   

############# Remove all Old Tables

rm(AA_Temp_table_AB,AA_Temp_table_AFM,AA_Temp_table_ON,
   L_AB_DT_Agg,L_AFM_DT_Agg,L_ON_C_DT_Agg,S_AB_DT_Agg,S_AFM_DT_Agg,S_ON_C_DT_Agg)


#######################################################################################################
#######################################################################################################
################################################## lines checks


Main_Query_Final <- Main_Query_Final[is.na(Max_Stock_Relex), Max_Stock_Relex := 0
][is.na(Relex_Ave_Demand), Relex_Ave_Demand := 0]


################# rowSums is better wey as it can treat NA's better
Main_Query_Final[,On_Circuit_Total_Lines := rowSums(.SD, na.rm = TRUE), .SDcols = c("L_Ave_Lines", "S_Ave_Lines")
][,On_Circuit_Total_Qty := rowSums(.SD, na.rm = TRUE), .SDcols = c("L_Ave_Qty","S_Ave_Qty")
][,Total_Daily_Lines := rowSums(.SD, na.rm = TRUE), .SDcols = c("L_Ave_Lines","L_AB_Ave_Lines","L_AFM_Ave_Lines","S_Ave_Lines","S_AB_Ave_Lines","S_AFM_Ave_Lines")
][,Total_Daily_Qty := rowSums(.SD, na.rm = TRUE), .SDcols = c("L_Ave_Qty","L_AB_Ave_Qty","L_AFM_Ave_Qty","S_Ave_Qty","S_AB_Ave_Qty","S_AFM_Ave_Qty")
][,Daily_Demand := Relex_Ave_Demand/10
][,Ideal_STD := pmax(Max_Stock_Relex,Total_Daily_Qty*10,Daily_Demand*10,Max_Shelf_Balance,1),by = .(Product_Code,Print_Branch)
][,Five_Day_STD := pmax((Daily_Demand*3),(On_Circuit_Total_Qty*5),1),by = .(Product_Code,Print_Branch)]   
#####################


####### SDA Suitbale Checks
Main_Query_SDA_Suitble <- Main_Query_Final[, c("Print_Branch","Product_Code", "Tote_Delivery","Live_Max","Live_Min","Live_Median",
                                               "On_Circuit_Total_Lines","On_Circuit_Total_Qty","L_Days_Picked","Warehouse_Area","S_Days_Picked")]
Main_Query_SDA_Suitble[,Qty_Per_Line := On_Circuit_Total_Qty/On_Circuit_Total_Lines]
#### Join SDA not Suitbale list --- Make DB at some point
Main_Query_SDA_Suitble <- merge(Main_Query_SDA_Suitble,
                                SDA_Not_Suitable,
                                by = c('Product_Code'),
                                all.x = TRUE)  
############# Rules of AFrame Suitability

Main_Query_SDA_Suitble[,SDA_Suitable := 1,
][Live_Min <= 11, SDA_Suitable := 0
][Live_Median >= 115, SDA_Suitable := 0
][Live_Max >= 215, SDA_Suitable := 0
][Qty_Per_Line >= 15 , SDA_Suitable := 0
][On_Circuit_Total_Lines <= 1 & Print_Branch == "405N" , SDA_Suitable := 0
][Not_Suitable_SDA == 1, SDA_Suitable := 0
][Tote_Delivery == 0, SDA_Suitable := 0
][Warehouse_Area == "A-frame", SDA_Suitable := 1]


Main_Query_SDA_Suitble <- Main_Query_SDA_Suitble[, c("Print_Branch","Product_Code","SDA_Suitable")]

Main_Query_Final <- merge(
  Main_Query_Final,
  Main_Query_SDA_Suitble,
  by = c('Print_Branch','Product_Code'),
  all.x = TRUE) 



##############OSR Suitbale add different rules for 405N as OSR is different

Main_Query_OSR_Suitble <- Main_Query_Final[, c("Print_Branch","Product_Code", "Tote_Delivery","Live_Max","Live_Min","Live_Median",
                                               "On_Circuit_Total_Lines","On_Circuit_Total_Qty"
                                               ,"L_Days_Picked","Warehouse_Area","Daily_Demand","L_AB_Ave_Lines","S_AB_Ave_Lines")]

Main_Query_OSR_Suitble[,Qty_Per_Line := On_Circuit_Total_Qty/On_Circuit_Total_Lines]

Main_Query_OSR_Suitble <- merge(
  Main_Query_OSR_Suitble,
  OSR_Not_Suitable,
  by = c('Product_Code'),
  all.x = TRUE)  



Main_Query_OSR_Suitble[,OSR_Suitable := 1,
][Live_Min >= 130, OSR_Suitable := 0
][Live_Median >= 240, OSR_Suitable := 0
][Live_Max >= 400, OSR_Suitable := 0
][Qty_Per_Line >= 25, OSR_Suitable := 0
][On_Circuit_Total_Lines > 6 & On_Circuit_Total_Qty > 60 , OSR_Suitable := 0
][L_AB_Ave_Lines > 0 , OSR_Suitable := 0
][S_AB_Ave_Lines > 0 , OSR_Suitable := 0
][OSR_Not_Suitable == 1, OSR_Suitable := 0
][Tote_Delivery == 0, OSR_Suitable := 0
]

Main_Query_OSR_Suitble <- Main_Query_OSR_Suitble[, c("Print_Branch","Product_Code","OSR_Suitable")]

Main_Query_Final <- merge(
  Main_Query_Final,
  Main_Query_OSR_Suitble,
  by = c('Print_Branch','Product_Code'),
  all.x = TRUE)

Main_Query_Final[substring(Location_Code,1,1) == "O", OSR_Suitable := 1]

#####

###### Calc Channel Requirements
Main_Query_Final[
  ,Long_Side_STD_Calc := fifelse(Print_Branch == "405N",
                                 floor(1800/Live_Min),
                                 floor(2500/Live_Min))
][
  ,Short_Side_STD_Calc := fifelse(Print_Branch %in% .('106M','703M','504M'),
                                  floor(1750/Live_Min),
                                  floor(1350/Live_Min))
][
  ,Super_Short_Side_STD_Calc := floor(800/Live_Min)
  
][
  ,Ideal_Number_Long_Channel := fifelse(Print_Branch %in% .('305M') & ceiling((L_Ave_Qty+S_Ave_Qty)/Long_Side_STD_Calc)>=8,
                                        ceiling(((L_Ave_Qty*0.70)+S_Ave_Qty*1)/Long_Side_STD_Calc),
                                        fifelse(Print_Branch %in% .('305M') & ceiling(((L_Ave_Qty)+S_Ave_Qty)/Long_Side_STD_Calc)<8,
                                                ceiling(((L_Ave_Qty*0.70)+S_Ave_Qty*1)/Long_Side_STD_Calc),
                                                fifelse(Print_Branch %in% .('703M','706S','501G','406P'),
                                                        ceiling(((L_Ave_Qty*0.75)+S_Ave_Qty*1)/Long_Side_STD_Calc),
                                                        fifelse(Print_Branch %in% .('405N'),
                                                                ceiling(((L_Ave_Qty*0.60)+S_Ave_Qty*1)/Long_Side_STD_Calc),
                                                                ceiling(((L_Ave_Qty*1.25)+S_Ave_Qty*1.25)/Long_Side_STD_Calc)))))
][
  ,Ideal_Number_Short_Channel := pmax(fifelse(Print_Branch %in% .('305M','504M','706S'),
                                              ceiling(((L_Ave_Qty*0.75)+(S_Ave_Qty*1))/Short_Side_STD_Calc),
                                              ceiling(((L_Ave_Qty*1.2)+(S_Ave_Qty*1.2))/Short_Side_STD_Calc)),1),
  by = .(Product_Code,Print_Branch)
][
  ,Ideal_Number_Super_Short_Channel := fifelse(Print_Branch %in% .('405N'),
                                               ceiling(((L_Ave_Qty*1.2)+S_Ave_Qty)/Super_Short_Side_STD_Calc),
                                               99)
][
  ,Ideal_Long_Short := fifelse(Ideal_Number_Super_Short_Channel <=1 & Ideal_Number_Short_Channel <=1,"Super Short",
                               fifelse(Print_Branch == '305M' & Ideal_Number_Short_Channel == 2 & (On_Circuit_Total_Lines/Ideal_Number_Short_Channel) >4.0,"Short",
                                       fifelse(Print_Branch == '305M' & Ideal_Number_Short_Channel == 3 & (On_Circuit_Total_Lines/Ideal_Number_Short_Channel) >15.0,"Short",       
                                               fifelse(Ideal_Number_Long_Channel <=1 & Ideal_Number_Short_Channel <=1,"Short","Long"))))
][
  ,Ideal_Number_Channel := fifelse(Ideal_Long_Short == "Long",Ideal_Number_Long_Channel,
                                   fifelse(Ideal_Long_Short == "Short",Ideal_Number_Short_Channel,1))
][
  , Lines_Per_Channel := (On_Circuit_Total_Lines)/Ideal_Number_Channel
][
  ,OSR_Tray := fifelse(Print_Branch %in% .("406P"),ceiling(((Ideal_STD*Volume)/9600000)/0.25)*0.25,ceiling(((Ideal_STD*Volume)/18000000)/0.25)*0.25)]



### Slots SF and Tunnel 
### Calc Slot Required

#################### Create Shelf Slot Parameter
Slotting_Param <-  as.data.table(Slotting_Param)
Slotting_Param_Shelf <- Slotting_Param[Slot_Type == "OC_Shelf_Front",c("Print_Branch","Slot_Volume")]
Slotting_Param_Shelf <- setnames(Slotting_Param_Shelf,"Slot_Volume", "Slot_Volume_Shelf")


########## Join Shelf Slot to Main Table
Main_Query_Final <- merge(
  Main_Query_Final,
  Slotting_Param_Shelf,
  by = c('Print_Branch'),
  all.x = TRUE)



Main_Query_Final <- Main_Query_Final[,Shelf_Slot := ceiling((Ideal_STD*Volume)/Slot_Volume_Shelf)
][,Shelf_Slot_Max5 := ifelse(Shelf_Slot >5,5,ifelse(Shelf_Slot == 0,1,Shelf_Slot))
][,One_Days_Sales := ifelse(Daily_Demand >= Total_Daily_Qty,Daily_Demand,Total_Daily_Qty)
][,Shelf_Slot_Max5_STD := floor((Slot_Volume_Shelf*Shelf_Slot_Max5)/Volume)
][,Shelf_Slot_Max5_Days_Cover := Shelf_Slot_Max5_STD/On_Circuit_Total_Qty
][,SF_Pallet_STD := ifelse(floor(1008000000/Volume)<PR_Qty_MAX,PR_Qty_MAX,floor(1008000000/Volume))
][,SF_Pallet__Days_Cover := SF_Pallet_STD/On_Circuit_Total_Qty
][,SF_Flow_Slot := ifelse((substring(OverStock_Location,1,1)=='R' | substring(OverStock_Location,1,1)=='Q')
                          ,ceiling(((On_Circuit_Total_Qty*5)*Volume)/187200000),ceiling((Five_Day_STD*Volume)/187200000))
][,SF_Flow_STD := SF_Flow_Slot*187200000/Volume
][,SF_Flow_Days_Cover := SF_Flow_STD/On_Circuit_Total_Qty
][,SF_Flow_Lines_Per_Slot := On_Circuit_Total_Lines/SF_Flow_Slot]   


################# Tunnel Long Slot
Slotting_Param <-  as.data.table(Slotting_Param)
Slotting_Param_Tunnel_Long <- Slotting_Param[Slot_Type == "OC_Tunnel_Long",c("Print_Branch","Slot_Volume")]
Slotting_Param_Tunnel_Long <- setnames(Slotting_Param_Tunnel_Long,"Slot_Volume", "Slot_Volume_Tunnel_Long")


########## Join Shelf Slot to Main Table
Main_Query_Final <- merge(
  Main_Query_Final,
  Slotting_Param_Tunnel_Long,
  by = c('Print_Branch'),
  all.x = TRUE)

############# Calculate Tunnel Long Slot And STD
Main_Query_Final <-Main_Query_Final[,OC_Tunnel_Long_Slot := ifelse(substring(OverStock_Location,1,1) %in% .('Q','R','G')
                                                                   ,ceiling(((On_Circuit_Total_Qty*5)*Volume)/Slot_Volume_Tunnel_Long)
                                                                   ,ifelse(Print_Branch == '201D'
                                                                           ,ceiling(((Five_Day_STD*1.5)*Volume)/Slot_Volume_Tunnel_Long)
                                                                           ,ceiling((Five_Day_STD*Volume)/Slot_Volume_Tunnel_Long)))
][,OC_Tunnel_Long_Slot := pmin(OC_Tunnel_Long_Slot,7), by =c("Print_Branch","Product_Code")
][,OC_Tunnel_Long_STD := floor((OC_Tunnel_Long_Slot*Slot_Volume_Tunnel_Long)/Volume)] 

############### Tunnel Long Days Cover  maybe chyange to exlcuide demand?
Main_Query_Final <-Main_Query_Final[,OC_Tunnel_Long_Days_Cover :=  OC_Tunnel_Long_STD/pmax(On_Circuit_Total_Qty), by = c("Print_Branch","Product_Code")
][,OC_Tunnel_Long_Lines_Per_Slot := On_Circuit_Total_Lines/OC_Tunnel_Long_Slot]   



################# MEd Tunnel Slot
Slotting_Param <-  as.data.table(Slotting_Param)
Slotting_Param_Tunnel_Med <- Slotting_Param[Slot_Type == "OC_Tunnel_Med",c("Print_Branch","Slot_Volume")]
Slotting_Param_Tunnel_Med <- setnames(Slotting_Param_Tunnel_Med,"Slot_Volume", "Slot_Volume_Tunnel_Med")


########## Join Shelf Slot to Main Table
Main_Query_Final <- merge(
  Main_Query_Final,
  Slotting_Param_Tunnel_Med,
  by = c('Print_Branch'),
  all.x = TRUE)

################## Tunnel Slot and STD
Main_Query_Final <-Main_Query_Final[,OC_Tunnel_Med_Slot := ifelse(substring(OverStock_Location,1,1) %in% .('Q','R','G') 
                                                                  ,ceiling(((On_Circuit_Total_Qty*5)*Volume)/Slot_Volume_Tunnel_Med),ceiling((Five_Day_STD*Volume)/Slot_Volume_Tunnel_Med))
][,OC_Tunnel_Med_Slot := pmin(OC_Tunnel_Med_Slot,7), by =c("Print_Branch","Product_Code")
][,OC_Tunnel_Med_STD := floor((OC_Tunnel_Med_Slot*Slot_Volume_Tunnel_Med)/Volume)]                                      


Main_Query_Final <-Main_Query_Final[,OC_Tunnel_Med_Days_Cover :=  OC_Tunnel_Med_STD/pmax(On_Circuit_Total_Qty), by = c("Print_Branch","Product_Code")
][,OC_Tunnel_Med_Lines_Per_Slot := On_Circuit_Total_Lines/OC_Tunnel_Med_Slot]    



################# Tunnel Short Slot
Slotting_Param <-  as.data.table(Slotting_Param)
Slotting_Param_Tunnel_Short <- Slotting_Param[Slot_Type == "OC_Tunnel_Short",c("Print_Branch","Slot_Volume")]
Slotting_Param_Tunnel_Short <- setnames(Slotting_Param_Tunnel_Short,"Slot_Volume", "Slot_Volume_Tunnel_Short")


########## Join Shelf Slot to Main Table
Main_Query_Final <- merge(
  Main_Query_Final,
  Slotting_Param_Tunnel_Short,
  by = c('Print_Branch'),
  all.x = TRUE)

#################### Calc Tunnel Short slot and STD
Main_Query_Final <-Main_Query_Final[,OC_Tunnel_Short_Slot := ifelse(substring(OverStock_Location,1,1) %in% .('Q','R','G')
                                                                    ,ceiling(((On_Circuit_Total_Qty*5)*Volume)/Slot_Volume_Tunnel_Short)
                                                                    ,ifelse(Print_Branch == '201D'
                                                                            ,ceiling(((Five_Day_STD*2)*Volume)/Slot_Volume_Tunnel_Short)
                                                                            ,ceiling((Five_Day_STD*Volume)/Slot_Volume_Tunnel_Short)))
][,OC_Tunnel_Short_Slot := pmin(OC_Tunnel_Short_Slot,7), by =c("Print_Branch","Product_Code")
][,OC_Tunnel_Short_STD := floor((OC_Tunnel_Short_Slot*Slot_Volume_Tunnel_Short)/Volume)]


############### Tunnel Short Days Cover maybe chyange to exlcuide demand?
Main_Query_Final <-Main_Query_Final[,OC_Tunnel_Short_Days_Cover :=  OC_Tunnel_Short_STD/pmax(On_Circuit_Total_Qty), by = c("Print_Branch","Product_Code")
][,OC_Tunnel_Short_Lines_Per_Slot := On_Circuit_Total_Lines/OC_Tunnel_Short_Slot]                                          


################# Flow Rack Slot
Slotting_Param <-  as.data.table(Slotting_Param)
Slotting_Param_Flow <- Slotting_Param[Slot_Type == "OC_Flow_Rack",c("Print_Branch","Slot_Volume")]
Slotting_Param_Flow <- setnames(Slotting_Param_Flow,"Slot_Volume", "Slot_Volume_Flow")


########## Join Shelf Slot to Main Table
Main_Query_Final <- merge(
  Main_Query_Final,
  Slotting_Param_Flow,
  by = c('Print_Branch'),
  all.x = TRUE)



Main_Query_Final <-Main_Query_Final[,OC_Flow_Slot := ifelse(substring(OverStock_Location,1,1)%in% .('Q','R')
                                                            ,ceiling(((On_Circuit_Total_Qty*5)*Volume)/Slot_Volume_Flow)
                                                              ,ifelse(Print_Branch %in% c('201D','504M')
                                                                      ,ceiling(((Five_Day_STD*1.5)*Volume)/Slot_Volume_Flow)
                                                                        ,ceiling(((Five_Day_STD)*Volume)/Slot_Volume_Flow)))
][,OC_Flow_Slot := pmin(OC_Flow_Slot,7), by =c("Print_Branch","Product_Code")
][,OC_Flow_STD := floor((OC_Flow_Slot*Slot_Volume_Flow)/Volume)] 


############### Flow Rack Days Cover  maybe chyange to exlcuide demand?
Main_Query_Final <-Main_Query_Final[,OC_Flow_Days_Cover :=  OC_Flow_STD/pmax(On_Circuit_Total_Qty), by = c("Print_Branch","Product_Code")
][,OC_Flow_Lines_Per_Slot := On_Circuit_Total_Lines/OC_Flow_Slot]

############## Suitability Rules by Rack Type
############## Pallet Suitability
Main_Query_Final <-Main_Query_Final[,Suitable_OC_Sup_F_Pallet := ifelse(Print_Branch %in% .('305M') & OC_Flow_Slot >= 5 & On_Circuit_Total_Lines >= 15 & Tote_Delivery == 1 ,1
                                                                        ,ifelse((Print_Branch %in% .('703M','201D','406P','706S') & OC_Tunnel_Long_Slot >= 3 & On_Circuit_Total_Lines >= 35 & Tote_Delivery == 1 ) | (substring(Product_Code,1,3) %in% .('MAI','ENT') & Full_Supplier == 'VES/005E'),1
                                                                                ,ifelse(Print_Branch %in% .('501G','405N','106M') & OC_Tunnel_Long_Slot >= 5 & On_Circuit_Total_Lines >= 30  & Tote_Delivery == 1,1
                                                                                        ,ifelse(Print_Branch %in% .('602J') & Shelf_Slot > 20 & Tote_Delivery == 1,1
                                                                                                ,ifelse(Print_Branch %in% .('908B') & Shelf_Slot > 70 & Tote_Delivery == 1,1
                                                                                                        ,ifelse(Print_Branch %in% .('504M') & OC_Flow_Slot >= 5 & On_Circuit_Total_Lines >= 10 & Tote_Delivery == 1,1,0))))))]

############## SF Flow Suitability
Main_Query_Final <-Main_Query_Final[,Suitable_OC_Sup_F_Flow := ifelse((Print_Branch %in% .('305M') & OC_Flow_Slot < 5 & Shelf_Slot > 5 & SF_Flow_Lines_Per_Slot >= 10 & Tote_Delivery == 1 & SDA_Suitable == 0) | (substring(Product_Code,1,3) %in% .('MAI','ENT') & Full_Supplier == 'VES/005E'),1
                                                                      ,ifelse(Print_Branch %in% .('703M','201D','406P','706S') & Shelf_Slot > 5 & OC_Tunnel_Long_Slot < 5 & SF_Flow_Lines_Per_Slot >= 10 & Tote_Delivery == 1& SDA_Suitable == 0,1
                                                                              ,ifelse(Print_Branch %in% .('501G','405N','106M') & OC_Tunnel_Long_Slot < 5 & Shelf_Slot > 5& SF_Flow_Lines_Per_Slot >= 10  & Tote_Delivery == 1& SDA_Suitable == 0,1
                                                                                      ,ifelse(Print_Branch %in% .('504M') & OC_Flow_Slot < 5 & SF_Flow_Lines_Per_Slot >= 8 & Shelf_Slot > 5 & Tote_Delivery == 1& SDA_Suitable == 0,1,0))))]

############## Tunnel Long Suitability
Main_Query_Final <-Main_Query_Final[,Suitable_OC_Tunnel_Long := ifelse(Print_Branch %in% .('406P') & Shelf_Slot > 15 & OC_Tunnel_Long_Slot >= 1 &  Tote_Delivery == 1  ,1
                                                                       ,ifelse(Print_Branch %in% .('201D') & Shelf_Slot > 5 & OC_Tunnel_Long_Slot >= 1 &  Tote_Delivery == 1  ,1
                                                                               ,ifelse(Print_Branch %in% .('501G','405N','106M','703M','706S') & Shelf_Slot > 10 & Tote_Delivery == 1 & OC_Tunnel_Long_Slot >= 1 ,1,0)))]
############## Tunnel Med Suitability
Main_Query_Final <-Main_Query_Final[,Suitable_OC_Tunnel_Med := ifelse(Print_Branch %in% .('703M','201D','406P','706S') & Shelf_Slot > 10 &  OC_Tunnel_Med_Slot >= 1  & Tote_Delivery == 1 ,1
                                                                      ,ifelse(Print_Branch %in% .('501G','405N','106M') & Shelf_Slot > 5 &  OC_Tunnel_Med_Slot >= 1 & Tote_Delivery == 1 ,1,0))]
############## Tunnel Short Suitability
Main_Query_Final <-Main_Query_Final[,Suitable_OC_Tunnel_Short := ifelse(Print_Branch %in% .('406P','703M','706S') & Shelf_Slot > 10 &  OC_Tunnel_Short_Slot >= 1 &  Tote_Delivery == 1  ,1
                                                                        ,ifelse(Print_Branch %in% .('201D') & Shelf_Slot > 1 &  OC_Tunnel_Short_Slot >= 1 &  Tote_Delivery == 1  ,1
                                                                                ,ifelse(Print_Branch %in% .('501G','405N','106M','504M') & Shelf_Slot > 5 &  OC_Tunnel_Short_Slot >= 1 & Tote_Delivery == 1 ,1,0)))]

############## Flow Suitability
Main_Query_Final <-Main_Query_Final[,Suitable_OC_Flow := ifelse(Print_Branch %in% .('406P') & Shelf_Slot > 10 &  Tote_Delivery == 1 & OC_Flow_Slot >= 1 ,1
                                                                ,ifelse(Print_Branch %in% .('305M') & Shelf_Slot > 10 &  Tote_Delivery == 1 & OC_Flow_Slot >= 1 ,1
                                                                        ,ifelse(Print_Branch %in% .('706S','703M') & Shelf_Slot > 10 &  Tote_Delivery == 1 & OC_Flow_Slot >= 1 ,1
                                                                                ,ifelse(Print_Branch %in% .('501G','908B','405N','106M','201D','504M') & Shelf_Slot > 5 & Tote_Delivery == 1 & OC_Flow_Slot >= 1,1,0))))]



#################################################
#################################################



Aframe_Summary <- as.data.table(Aframe_Summary)


######## A-frame Allocator New
# Filter & group
df.AF <- Main_Query_Final[
  !is.na(Lines_Per_Channel) & !is.infinite(Lines_Per_Channel) & SDA_Suitable == 1 &
    Print_Branch %in% c('106M','201D','305M','405N','406P','501G','504M','703M','706S'),
  .(
    Acc_Per_Chan = sum(Lines_Per_Channel),
    Ideal_Number_Channels = sum(Ideal_Number_Channel)
  ),
  by = .(Product_Code, Print_Branch, Ideal_Long_Short)
]

# Order & cumsum
df.AF <- df.AF[order(Print_Branch, Ideal_Long_Short, -Acc_Per_Chan)]
df.AF[, Cumsum_Channel := cumsum(Ideal_Number_Channels), by = Print_Branch]

# Join Aframe_Summary by branch & type
df.AF <- merge(
  df.AF,
  Aframe_Summary[Channel_Type == 'Long', .(Branch_ID, Channel_Count)],
  by.x = c("Print_Branch"),
  by.y = c("Branch_ID"),
  all.x = TRUE
)

# Allocation
df.AF[, Ideal_Area := fifelse(Cumsum_Channel <= Channel_Count, "A-Frame Long", "Not Allocated")]
df.AF[, Allocated := as.integer(Ideal_Area == "A-Frame Long")]


###################Aframe Short Calc

# Filter & group
df.AFDone <- df.AF[Allocated == 1,]
df.AFSh <- df.AF[Allocated == 0 & Ideal_Long_Short != "Long",]


# Order & cumsum
df.AFSh <- df.AFSh[,Cumsum_Channel := cumsum(Ideal_Number_Channels), by =c("Print_Branch","Ideal_Long_Short")]
df.AFSh[,c("Channel_Count") :=NULL]

# Join Aframe_Summary by branch & type
df.AFSh <- merge(
  df.AFSh,
  Aframe_Summary[Channel_Type == 'Short', .(Branch_ID, Channel_Count)],
  by.x = c("Print_Branch"),
  by.y = c("Branch_ID"),
  all.x = TRUE
)

# Allocation
df.AFSh[, Ideal_Area := fifelse(Cumsum_Channel <= Channel_Count, "A-Frame Short", "Not Allocated")]
df.AFSh[, Allocated := as.integer(Ideal_Area == "A-Frame Short")]


############# Combine to One Table
df.AFShDone <- df.AFSh[Allocated == 1,]
df.AFDone <- rbind(df.AFDone,df.AFShDone)


###############A-frame Channel Super Short

# Filter & group
df.AFSSh <- df.AFSh[Allocated == 0 & Ideal_Long_Short == "Super Short",]
df.AFSSh[,c("Channel_Count") :=NULL]
# Order & cumsum
df.AFSSh <- df.AFSSh[,Cumsum_Channel := cumsum(Ideal_Number_Channels), by =c("Print_Branch","Ideal_Long_Short")]


# Join Aframe_Summary by branch & type
df.AFSSh <- merge(
  df.AFSSh,
  Aframe_Summary[Channel_Type == 'Short 80', .(Branch_ID, Channel_Count)],
  by.x = c("Print_Branch"),
  by.y = c("Branch_ID"),
  all.x = TRUE
)

# Allocation
df.AFSSh[, Ideal_Area := fifelse(Cumsum_Channel <= Channel_Count, "A-Frame Super Short", "Not Allocated")]
df.AFSSh[, Allocated := as.integer(Ideal_Area == "A-Frame Super Short")]


df.AFDone <- rbind(df.AFDone,df.AFSSh)
df.AFDone[,c("Channel_Count") :=NULL]

############## Combine back  
df.AFDone[,c("Ideal_Long_Short","Acc_Per_Chan","Ideal_Number_Channels","Cumsum_Channel") :=NULL]
Main_Query_Final_1 <- left_join(Main_Query_Final,df.AFDone) 
Main_Query_Final_1 <- as.data.table(Main_Query_Final_1)
Main_Query_Final_1[is.na(Ideal_Area), "Ideal_Area" := "Not Allocated"
][is.na(Allocated), "Allocated" := 0] 


################## OSR

df.OSR <- Main_Query_Final_1[OSR_Suitable == 1 & Print_Branch %in% .("406P","405N") & Allocated == 0 & OSR_Tray <= 20,
                             .(OSR_Tray_Calc=sum(OSR_Tray)),
                             by=c('Product_Code','Print_Branch')]
df.OSR <- df.OSR[order(Print_Branch,OSR_Tray_Calc)]
df.OSR <- df.OSR[,Cumsum_Tray := cumsum(OSR_Tray_Calc), by = "Print_Branch"]
df.OSR <- df.OSR[,Ideal_Area_OSR := "Not Allocated",
][Print_Branch == "406P" & Cumsum_Tray <= 24000, Ideal_Area_OSR := "OSR"
][Print_Branch == "405N" & Cumsum_Tray <= 4500, Ideal_Area_OSR := "OSR"
][,Allocated_OSR := 1
][Ideal_Area_OSR == "Not Allocated", Allocated_OSR :=  0]

df.OSR[, c("OSR_Tray_Calc","Cumsum_Tray") :=NULL]

Main_Query_Final_3 <- left_join(Main_Query_Final_1,df.OSR)
Main_Query_Final_3 <- as.data.table(Main_Query_Final_3)
Main_Query_Final_3[Ideal_Area_OSR == "OSR", Ideal_Area := "OSR"
][Ideal_Area_OSR == "OSR", Allocated := 1]

Main_Query_Final_3[,c("Ideal_Area_OSR","Allocated_OSR") :=NULL]

Main_Query_Final_2 <- Main_Query_Final_3

rm(df.OSR,Main_Query_Final_3,Main_Query_Final_1,Main_Query_Final,Main_Query_OSR_Suitble,Main_Query_SDA_Suitble,df.AF,df.AFDone,df.AFSh,df.AFShDone,df.AFSSh,OSR_Not_Suitable,SDA_Not_Suitable) 
gc()

#################### Super Fast / Pallet and Flow Allocator
Slotting_Param <- as.data.table(Slotting_Param)

## Filter and Group
df.SF <- Main_Query_Final_2[Suitable_OC_Sup_F_Pallet == 1 & Allocated == 0 
                            & Print_Branch %in% .('106M','201D','305M','405N','406P','501G','504M','703M','706S','908B','602J'),
                            .(Acc_Per_Chan=sum(On_Circuit_Total_Lines)),by=c('Product_Code','Print_Branch')]

##### Add Counter
df.SF <- df.SF[,Counter := 1L]

############# Order and cumsum
df.SF <- df.SF[order(Print_Branch,-Acc_Per_Chan)]
df.SF <- df.SF[, Cumsum_Channel := cumsum(Counter), by ="Print_Branch"]


#########Merge in slotting capacity
df.SF <- merge(
  df.SF,
  Slotting_Param[Slot_Type == "OC_Sup_F_Pallet", .(Print_Branch, Capacity_Slots)],
  by = "Print_Branch",
  all.x = TRUE
)

###Allocate
df.SF[, Ideal_Area_SF_Pallet := fifelse(Cumsum_Channel <= Capacity_Slots, "SF_Pallet", "Not Allocated")]
df.SF[, Allocated_SF_Pallet := as.integer(Ideal_Area_SF_Pallet == "SF_Pallet")]



############# Combining Back into Main File
Main_Query_Final_3 <- left_join(Main_Query_Final_2,df.SF)
Main_Query_Final_3 <- as.data.table(Main_Query_Final_3)
Main_Query_Final_3[Ideal_Area_SF_Pallet == "SF_Pallet", Ideal_Area := "SF_Pallet"
][Ideal_Area_SF_Pallet == "SF_Pallet", Allocated := 1]


Main_Query_Final_3[,c("Ideal_Area_SF_Pallet","Allocated_SF_Pallet","Counter","Cumsum_Channel","Acc_Per_Chan","Capacity_Slots") :=NULL]

Main_Query_Final_2 <- Main_Query_Final_3

rm(Main_Query_Final_3,df.SF)

###################### SF Flow

##########Filter and Group
df.SFF <- Main_Query_Final_2[Suitable_OC_Sup_F_Flow == 1 & Allocated == 0 
                             & Print_Branch %in% .('106M','201D','305M','405N','406P','501G','504M','703M','706S','908B','602J'),
                             .(Acc_Per_Chan=sum(SF_Flow_Lines_Per_Slot),Ideal_Number_Channels=sum(SF_Flow_Slot)),by=c('Product_Code','Print_Branch')]


######## Order and cumsum
df.SFF <- df.SFF[order(Print_Branch,-Acc_Per_Chan)]
df.SFF <- df.SFF[, Cumsum_Channel := cumsum(Ideal_Number_Channels), by ="Print_Branch"]


#########Merge in slotting capacity
df.SFF <- merge(
  df.SFF,
  Slotting_Param[Slot_Type == "OC_Sup_F_Flow", .(Print_Branch, Capacity_Slots)],
  by = "Print_Branch",
  all.x = TRUE
)

###Allocate
df.SFF[, Ideal_Area_SF_Flow := fifelse(Cumsum_Channel <= Capacity_Slots, "SF_Flow", "Not Allocated")]
df.SFF[, Allocated_SF_Flow := as.integer(Ideal_Area_SF_Flow == "SF_Flow")]


############# Combining Back into Main File
Main_Query_Final_3 <- left_join(Main_Query_Final_2,df.SFF)
Main_Query_Final_3 <- as.data.table(Main_Query_Final_3)
Main_Query_Final_3[Ideal_Area_SF_Flow == "SF_Flow", Ideal_Area := "SF_Flow"
][Ideal_Area_SF_Flow == "SF_Flow", Allocated := 1]


Main_Query_Final_3[,c("Ideal_Area_SF_Flow","Allocated_SF_Flow","Cumsum_Channel","Acc_Per_Chan","Ideal_Number_Channels","Capacity_Slots") :=NULL]

Main_Query_Final_2 <- Main_Query_Final_3

rm(Main_Query_Final_3,df.SFF)

###################### OC_Pallet


#### Filter and Group
df.OC_P <- Main_Query_Final_2[Suitable_OC_Sup_F_Pallet == 1 & Allocated == 0 
                              & Print_Branch %in% .('106M','201D','305M','405N','406P','501G','504M','703M','706S','908B','602J'),
                              .(Acc_Per_Chan=sum(On_Circuit_Total_Lines)),by=c('Product_Code','Print_Branch')]
#########Counter
df.OC_P <- df.OC_P[,Counter := 1]

########## order and cumsum
df.OC_P <- df.OC_P[order(Print_Branch,-Acc_Per_Chan)]
df.OC_P <- df.OC_P[, Cumsum_Channel := cumsum(Counter), by ="Print_Branch"]

#########Merge in slotting capacity
df.OC_P <- merge(
  df.OC_P,
  Slotting_Param[Slot_Type == "OC_Pallet", .(Print_Branch, Capacity_Slots)],
  by = "Print_Branch",
  all.x = TRUE
)

###Allocate
df.OC_P[, Ideal_Area_OC_Pallet := fifelse(Cumsum_Channel <= Capacity_Slots, "OC_Pallet", "Not Allocated")]
df.OC_P[, Allocated_OC_Pallet := as.integer(Ideal_Area_OC_Pallet == "OC_Pallet")]


############# Combining Back into Main File
Main_Query_Final_3 <- left_join(Main_Query_Final_2,df.OC_P)
Main_Query_Final_3 <- as.data.table(Main_Query_Final_3)
Main_Query_Final_3[Ideal_Area_OC_Pallet == "OC_Pallet", Ideal_Area := "OC_Pallet"
][Ideal_Area_OC_Pallet == "OC_Pallet", Allocated := 1]


Main_Query_Final_3[,c("Ideal_Area_OC_Pallet","Allocated_OC_Pallet","Counter","Cumsum_Channel","Acc_Per_Chan","Capacity_Slots") :=NULL]

Main_Query_Final_2 <- Main_Query_Final_3

rm(Main_Query_Final_3,df.OC_P)

#################### Tunnel Long

############## Filter and Group
df.TL <- Main_Query_Final_2[Suitable_OC_Tunnel_Long == 1 & Allocated == 0 
                            & Print_Branch %in% .('106M','201D','305M','405N','406P','501G','703M','706S','908B','602J','504M'),
                            .(Acc_Per_Chan=sum(OC_Tunnel_Long_Lines_Per_Slot),Ideal_Number_Channels=sum(OC_Tunnel_Long_Slot)),by=c('Product_Code','Print_Branch')]

############ Cumsum and Order
df.TL <- df.TL[order(Print_Branch,-Acc_Per_Chan)]
df.TL <- df.TL[, Cumsum_Channel := cumsum(Ideal_Number_Channels), by ="Print_Branch"]

#########Merge in slotting capacity
df.TL <- merge(
  df.TL,
  Slotting_Param[Slot_Type == "OC_Tunnel_Long", .(Print_Branch, Capacity_Slots)],
  by = "Print_Branch",
  all.x = TRUE
)

###Allocate
df.TL[, Ideal_Area_OC_Tunnel_Long := fifelse(Cumsum_Channel <= Capacity_Slots, "OC_Tunnel_Long", "Not Allocated")]
df.TL[, Allocated_OC_Tunnel_Long := as.integer(Ideal_Area_OC_Tunnel_Long == "OC_Tunnel_Long")]


############# Combining Back into Main File
Main_Query_Final_3 <- left_join(Main_Query_Final_2,df.TL)
Main_Query_Final_3 <- as.data.table(Main_Query_Final_3)
Main_Query_Final_3[Ideal_Area_OC_Tunnel_Long == "OC_Tunnel_Long", Ideal_Area := "OC_Tunnel_Long"
][Ideal_Area_OC_Tunnel_Long == "OC_Tunnel_Long", Allocated := 1]


Main_Query_Final_3[,c("Ideal_Area_OC_Tunnel_Long","Allocated_OC_Tunnel_Long","Cumsum_Channel","Acc_Per_Chan","Ideal_Number_Channels","Capacity_Slots") :=NULL]

Main_Query_Final_2 <- Main_Query_Final_3

rm(Main_Query_Final_3,df.TL)

################### Tunnel Med
#######Filter and Group
df.TM <- Main_Query_Final_2[Suitable_OC_Tunnel_Med == 1 & Allocated == 0 
                            & Print_Branch %in% .('106M','201D','305M','405N','406P','501G','703M','706S','908B','602J','504M'),
                            .(Acc_Per_Chan=sum(OC_Tunnel_Med_Lines_Per_Slot),Ideal_Number_Channels=sum(OC_Tunnel_Med_Slot)),by=c('Product_Code','Print_Branch')]
########## Order and cumsum
df.TM <- df.TM[order(Print_Branch,-Acc_Per_Chan)]
df.TM <- df.TM[, Cumsum_Channel := cumsum(Ideal_Number_Channels), by ="Print_Branch"]

#########Merge in slotting capacity
df.TM <- merge(
  df.TM,
  Slotting_Param[Slot_Type == "OC_Tunnel_Med", .(Print_Branch, Capacity_Slots)],
  by = "Print_Branch",
  all.x = TRUE
)

###Allocate
df.TM[, Ideal_Area_OC_Tunnel_Med := fifelse(Cumsum_Channel <= Capacity_Slots, "OC_Tunnel_Med", "Not Allocated")]
df.TM[, Allocated_OC_Tunnel_Med := as.integer(Ideal_Area_OC_Tunnel_Med == "OC_Tunnel_Med")]

############# Combining Back into Main File
Main_Query_Final_3 <- left_join(Main_Query_Final_2,df.TM)
Main_Query_Final_3 <- as.data.table(Main_Query_Final_3)
Main_Query_Final_3[Ideal_Area_OC_Tunnel_Med == "OC_Tunnel_Med", Ideal_Area := "OC_Tunnel_Med"
][Ideal_Area_OC_Tunnel_Med == "OC_Tunnel_Med", Allocated := 1]


Main_Query_Final_3[,c("Ideal_Area_OC_Tunnel_Med","Allocated_OC_Tunnel_Med","Cumsum_Channel","Acc_Per_Chan","Ideal_Number_Channels","Capacity_Slots") :=NULL]

Main_Query_Final_2 <- Main_Query_Final_3

rm(Main_Query_Final_3,df.TM)


###################  Tunnel Short
########## Filter and Group
df.TS <- Main_Query_Final_2[Suitable_OC_Tunnel_Short == 1 & Allocated == 0 
                            & Print_Branch %in% .('106M','201D','305M','405N','406P','501G','703M','706S','908B','602J','504M'),
                            .(Acc_Per_Chan=sum(OC_Tunnel_Short_Lines_Per_Slot),Ideal_Number_Channels=sum(OC_Tunnel_Short_Slot)),by=c('Product_Code','Print_Branch')]
########Order and Cumsum
df.TS <- df.TS[order(Print_Branch,-Acc_Per_Chan)]
df.TS <- df.TS[, Cumsum_Channel := cumsum(Ideal_Number_Channels), by ="Print_Branch"]

#########Merge in slotting capacity
df.TS <- merge(
  df.TS,
  Slotting_Param[Slot_Type == "OC_Tunnel_Short", .(Print_Branch, Capacity_Slots)],
  by = "Print_Branch",
  all.x = TRUE
)

###Allocate
df.TS[, Ideal_Area_OC_Tunnel_Short := fifelse(Cumsum_Channel <= Capacity_Slots, "OC_Tunnel_Short", "Not Allocated")]
df.TS[, Allocated_OC_Tunnel_Short := as.integer(Ideal_Area_OC_Tunnel_Short == "OC_Tunnel_Short")]


############# Combining Back into Main File
Main_Query_Final_3 <- left_join(Main_Query_Final_2,df.TS)
Main_Query_Final_3 <- as.data.table(Main_Query_Final_3)
Main_Query_Final_3[Ideal_Area_OC_Tunnel_Short == "OC_Tunnel_Short", Ideal_Area := "OC_Tunnel_Short"
][Ideal_Area_OC_Tunnel_Short == "OC_Tunnel_Short", Allocated := 1
][Ideal_Area == "OC_Tunnel_Short" & Print_Branch == '504M', Ideal_Area := "OC_Flow_Long"]


Main_Query_Final_3[,c("Ideal_Area_OC_Tunnel_Short","Allocated_OC_Tunnel_Short","Cumsum_Channel","Acc_Per_Chan","Ideal_Number_Channels","Capacity_Slots") :=NULL]

Main_Query_Final_2 <- Main_Query_Final_3

rm(Main_Query_Final_3,df.TS)




###################  Flow Rack

##########Filter and Group
df.OCF <- Main_Query_Final_2[Suitable_OC_Flow == 1 & Allocated == 0 
                             & Print_Branch %in% .('106M','201D','305M','405N','406P','501G','703M','706S','504M','908B','602J'),
                             .(Acc_Per_Chan=sum(OC_Flow_Lines_Per_Slot),Ideal_Number_Channels=sum(OC_Flow_Slot)),by=c('Product_Code','Print_Branch')]

############Order and Cumsum
df.OCF <- df.OCF[order(Print_Branch,-Acc_Per_Chan)]
df.OCF <- df.OCF[, Cumsum_Channel := cumsum(Ideal_Number_Channels), by ="Print_Branch"]


#########Merge in slotting capacity
df.OCF <- merge(
  df.OCF,
  Slotting_Param[Slot_Type == "OC_Flow_Rack", .(Print_Branch, Capacity_Slots)],
  by = "Print_Branch",
  all.x = TRUE
)

###Allocate
df.OCF[, Ideal_Area_OC_Flow := fifelse(Cumsum_Channel <= Capacity_Slots, "OC_Flow", "Not Allocated")]
df.OCF[, Allocated_OC_Flow := as.integer(Ideal_Area_OC_Flow == "OC_Flow")]

############# Combining Back into Main File
Main_Query_Final_3 <- left_join(Main_Query_Final_2,df.OCF)
Main_Query_Final_3 <- as.data.table(Main_Query_Final_3)
Main_Query_Final_3[Ideal_Area_OC_Flow == "OC_Flow", Ideal_Area := "OC_Flow"
][Ideal_Area_OC_Flow == "OC_Flow", Allocated := 1]


Main_Query_Final_3[,c("Ideal_Area_OC_Flow","Allocated_OC_Flow","Cumsum_Channel","Acc_Per_Chan","Ideal_Number_Channels","Capacity_Slots") :=NULL]

Main_Query_Final_2 <- Main_Query_Final_3

rm(Main_Query_Final_3,df.OCF)

########################### Spec Hand and Bulk

Main_Query_Final_2[Warehouse_Zone == "Bulk", Ideal_Area := Warehouse_Area
][Location_Type == "C", Ideal_Area := "CD"
][Location_Type == "R", Ideal_Area := "Fridge"
][Location_Type == "M", Ideal_Area := "Cytotoxic"
][Location_Type == "F", Ideal_Area := "Flammable"
][Location_Type == "N", Ideal_Area := "Fridge"
]

Main_Query_Final_2[Warehouse_Zone == "Bulk", Allocated := 1
][Location_Type %in% c("C","R","M","F","N"), Allocated := 1
]

################### Shelves

##################### Slot Adjuster
Param_Shelf_Total_Slot <- Slotting_Param[Slot_Type %in% .('OC_Shelf_Front','OC_Shelf_Middle','OC_Shelf_Back'),.(Slots =sum(Capacity_Slots)), by = c("Print_Branch")]

Slot_Circuit_Calc <- Main_Query_Final_2[Allocated == 0 & Tote_Delivery == 1
                                        & Print_Branch %in% .('106M','201D','305M','405N','406P','501G','703M','706S','504M','908B','602J'),
                                        .(Shelf_Slot=sum(Shelf_Slot),
                                          Shelf_Slot_Max5=sum(Shelf_Slot_Max5),
                                          Ideal_STD=sum(Ideal_STD),
                                          Upto_Full_Shelf_Days_Cover=sum(Shelf_Slot_Max5_Days_Cover),
                                          On_Circuit_Total_Lines=sum(On_Circuit_Total_Lines),
                                          Auto_Bulk_Daily_Pick=sum(Total_Daily_Lines-On_Circuit_Total_Lines),
                                          On_Circuit_Total_Qty=sum(On_Circuit_Total_Qty),
                                          Auto_Bulk_Daily_Qty=sum(Total_Daily_Qty-On_Circuit_Total_Qty)
                                          ,One_Day_Sales=sum(One_Days_Sales)
                                        ),,by=c('Product_Code','Print_Branch','OverStock_Location')]

Slot_Circuit_Calc <- Slot_Circuit_Calc[,Percent_Autobulk := (Auto_Bulk_Daily_Pick/(On_Circuit_Total_Lines+Auto_Bulk_Daily_Pick))
]

Slot_Circuit_Calc[,Ideal_Area_Bulk := ifelse(substring(OverStock_Location,1,1) <= 'O' & Percent_Autobulk >= 0.4 & Shelf_Slot >= 20,'Bulk'
                                             ,ifelse(Percent_Autobulk >= 0.7 & Shelf_Slot >= 10,'Bulk','OK'))]
Slot_Circuit_Calc[is.nan(Percent_Autobulk),Ideal_Area_Bulk := 'OK']


Slot_Circuit_Calc <- Slot_Circuit_Calc[is.infinite(Upto_Full_Shelf_Days_Cover),Upto_Full_Shelf_Days_Cover := 9999]
Slot_Circuit_Calc <- Slot_Circuit_Calc[is.nan(Percent_Autobulk),Percent_Autobulk := 0]


Slot_Circuit_Calc <- Slot_Circuit_Calc[Shelf_Slot <= 5 | Ideal_Area_Bulk == 'Bulk', Allocated_Circuit := 1
][Shelf_Slot > 5 & Shelf_Slot <= 10,Allocated_Circuit := 1
][,Adj_Shelf_Slot := Shelf_Slot
][Ideal_Area_Bulk == 'Bulk', Adj_Shelf_Slot := 0
][Shelf_Slot > 5 & Shelf_Slot <= 10 & Ideal_Area_Bulk != 'Bulk',Adj_Shelf_Slot := 10
][(Shelf_Slot > 10 &  Shelf_Slot <= 15 )& Ideal_Area_Bulk != 'Bulk' ,Adj_Shelf_Slot := 15
][Shelf_Slot > 10 & Ideal_Area_Bulk != 'Bulk' & Upto_Full_Shelf_Days_Cover >= 200 ,Adj_Shelf_Slot := 10
][Shelf_Slot > 10 & Ideal_Area_Bulk != 'Bulk' & Upto_Full_Shelf_Days_Cover <= 3,Adj_Shelf_Slot := 20
][Shelf_Slot > 10 & Ideal_Area_Bulk != 'Bulk' & Upto_Full_Shelf_Days_Cover > 3 & Upto_Full_Shelf_Days_Cover <= 5,Adj_Shelf_Slot := 15
][Shelf_Slot > 10 & Ideal_Area_Bulk != 'Bulk' & Upto_Full_Shelf_Days_Cover > 5 ,Adj_Shelf_Slot := 10
][Shelf_Slot >= 10 & Ideal_Area_Bulk != 'Bulk' & Upto_Full_Shelf_Days_Cover >= 14 & Print_Branch %in% c('305M','406P') ,Adj_Shelf_Slot := 5
][Shelf_Slot >= 8 & Ideal_Area_Bulk != 'Bulk' & Upto_Full_Shelf_Days_Cover >= 18 & Print_Branch %in% c('706S') ,Adj_Shelf_Slot := 5
][(Shelf_Slot > 10 &  Shelf_Slot <= 15) &Ideal_Area_Bulk != 'Bulk' & Print_Branch %in% .('501G','703M','504M','201D','602J','908B') ,Adj_Shelf_Slot := 15
][(Shelf_Slot > 15 &  Shelf_Slot <= 20) &Ideal_Area_Bulk != 'Bulk' & Print_Branch %in% .('501G','703M','504M','201D','602J','908B') ,Adj_Shelf_Slot := 20
][(Shelf_Slot > 20 &  Shelf_Slot <= 25) &Ideal_Area_Bulk != 'Bulk' & Print_Branch %in% .('501G','703M','504M','201D') ,Adj_Shelf_Slot := 25
][(Shelf_Slot > 20 &  Shelf_Slot <= 30) &Ideal_Area_Bulk != 'Bulk' & Print_Branch %in% .('602J','908B') ,Adj_Shelf_Slot := 20
][,Allocated_Circuit := 1]



#################Overrides DC 501G Full got to 25 slots


Total_Slot_Allocated <- Slot_Circuit_Calc[Allocated_Circuit == 1 & Ideal_Area_Bulk != 'Bulk',.(Shelf_Slot_Used=sum(Adj_Shelf_Slot)), by = c("Print_Branch")]

Param_Shelf_Total_Slot_1 <- merge(
  Param_Shelf_Total_Slot,
  Total_Slot_Allocated,
  by = "Print_Branch",
  all.x = TRUE)


Param_Shelf_Total_Slot_2  <- Param_Shelf_Total_Slot_1[,Slot_Remaining := Slots-Shelf_Slot_Used]

common <- intersect(names(Main_Query_Final_2), names(Slot_Circuit_Calc))
Main_Query_Final_2 <- merge(
  Main_Query_Final_2,
  Slot_Circuit_Calc,
  by     = common,
  all.x  = TRUE,
  sort   = FALSE
)

Main_Query_Final_2 <- Main_Query_Final_2[Ideal_Area_Bulk == 'Bulk', Ideal_Area := 'Bulk'
][Ideal_Area_Bulk == 'Bulk', Allocated := 1
][(substring(Product_Code,1,3) %in% .('MAI','ENT') & Full_Supplier == 'VES/005E'),Ideal_Area := 'Mailouts'
][(substring(Product_Code,1,3) %in% .('MAI','ENT') & Full_Supplier == 'VES/005E'),Allocated := 0
]
Main_Query_Final_2 <- Main_Query_Final_2[Tote_Delivery == 1 & Allocated == 0,OC_Shelf_Lines_Per_Slot:=On_Circuit_Total_Lines/Adj_Shelf_Slot
][is.na(OC_Shelf_Lines_Per_Slot), OC_Shelf_Lines_Per_Slot := 0.001
][OC_Shelf_Lines_Per_Slot <= 0,OC_Shelf_Lines_Per_Slot := 0.002]

Main_Query_Final_2[,c("Ideal_Area_Bulk","Percent_Autobulk","One_Day_Sales","Auto_Bulk_Daily_Qty","Auto_Bulk_Daily_Pick") :=NULL]




####################### Slot Circuit

############ Circuit_Front
########## Filter and Group
df.OC_SF <- Main_Query_Final_2[Tote_Delivery == 1 & Allocated == 0 
                               & Print_Branch %in% .('106M','201D','305M','405N','406P','501G','703M','706S','504M','908B','602J'),
                               .(Acc_Per_Chan=sum(OC_Shelf_Lines_Per_Slot),Ideal_Number_Channels=sum(Adj_Shelf_Slot)),by=c('Product_Code','Print_Branch')]

########### Order and cumsum
df.OC_SF <- df.OC_SF[order(Print_Branch,-Acc_Per_Chan)]
df.OC_SF <- df.OC_SF[, Cumsum_Channel := cumsum(Ideal_Number_Channels), by ="Print_Branch"]

#########Merge in slotting capacity
df.OC_SF <- merge(
  df.OC_SF,
  Slotting_Param[Slot_Type == "OC_Shelf_Front", .(Print_Branch, Capacity_Slots)],
  by = "Print_Branch",
  all.x = TRUE
)

###Allocate
df.OC_SF[, Ideal_Area_OC_Shelf_Front := fifelse(Cumsum_Channel <= Capacity_Slots, "OC_Shelf_Front", "Not Allocated")]
df.OC_SF[, Allocated_OC_Circuit_Front := as.integer(Ideal_Area_OC_Shelf_Front == "OC_Shelf_Front")]


############# Combining Back into Main File
Main_Query_Final_3 <- left_join(Main_Query_Final_2,df.OC_SF)
Main_Query_Final_3 <- as.data.table(Main_Query_Final_3)



Main_Query_Final_3[Ideal_Area_OC_Shelf_Front == "OC_Shelf_Front", Ideal_Area := "OC_Shelf_Front"
][Ideal_Area_OC_Shelf_Front == "OC_Shelf_Front", Allocated := 1
]


Main_Query_Final_3[,c("Ideal_Area_OC_Shelf_Front","Allocated_OC_Circuit_Front","Cumsum_Channel","Acc_Per_Chan","Ideal_Number_Channels","Capacity_Slots") :=NULL]

Main_Query_Final_2 <- Main_Query_Final_3

rm(Main_Query_Final_3,df.OC_SF)


##test1<- Main_Query_Final_2[Print_Branch %in% c('504M','501G') & Ideal_Area == 'Not Allocated',]
##Sys.sleep(10)


############# Circuit Middle
# Filter & group
df.OC_SM <- Main_Query_Final_2[Tote_Delivery == 1 & Allocated == 0 
                               & Print_Branch %in% .('106M','201D','305M','405N','406P','501G','703M','706S','504M','908B','602J'),
                               .(Acc_Per_Chan=sum(OC_Shelf_Lines_Per_Slot),Ideal_Number_Channels=sum(Adj_Shelf_Slot)),by=c('Product_Code','Print_Branch')]


########## Order and cumsum
df.OC_SM <- df.OC_SM[order(Print_Branch,-Acc_Per_Chan)]
df.OC_SM <- df.OC_SM[, Cumsum_Channel := cumsum(Ideal_Number_Channels), by ="Print_Branch"]


#########Merge in slotting capacity
df.OC_SM <- merge(
  df.OC_SM,
  Slotting_Param[Slot_Type == "OC_Shelf_Middle", .(Print_Branch, Capacity_Slots)],
  by = "Print_Branch",
  all.x = TRUE
)

###Allocate
df.OC_SM[, Ideal_Area_OC_Shelf_Middle := fifelse(Cumsum_Channel <= Capacity_Slots, "OC_Shelf_Middle", "Not Allocated")]
df.OC_SM[, Allocated_OC_Circuit_Middle := as.integer(Ideal_Area_OC_Shelf_Middle == "OC_Shelf_Middle")]

############# Combining Back into Main File
Main_Query_Final_3 <- left_join(Main_Query_Final_2,df.OC_SM)
Main_Query_Final_3 <- as.data.table(Main_Query_Final_3)
Main_Query_Final_3[Ideal_Area_OC_Shelf_Middle == "OC_Shelf_Middle", Ideal_Area := "OC_Shelf_Middle"
][Ideal_Area_OC_Shelf_Middle == "OC_Shelf_Middle", Allocated := 1
]


Main_Query_Final_3[,c("Ideal_Area_OC_Shelf_Middle","Allocated_OC_Circuit_Middle","Cumsum_Channel","Acc_Per_Chan","Ideal_Number_Channels","Capacity_Slots") :=NULL]

Main_Query_Final_2 <- Main_Query_Final_3

rm(Main_Query_Final_3,df.OC_SM)


##test2<- Main_Query_Final_2[Print_Branch %in% c('504M','501G') & Ideal_Area == 'Not Allocated',]
##Sys.sleep(10)

############# Circuit BAck
# Filter & group
df.OC_SB <- Main_Query_Final_2[Tote_Delivery == 1 & Allocated == 0 
                               & Print_Branch %in% .('106M','201D','305M','405N','406P','501G','703M','706S','504M','908B','602J'),
                               .(Acc_Per_Chan=sum(OC_Shelf_Lines_Per_Slot),Ideal_Number_Channels=sum(Adj_Shelf_Slot)),by=c('Product_Code','Print_Branch')]

########## Order and cumsum
df.OC_SB <- df.OC_SB[order(Print_Branch,-Acc_Per_Chan)]
df.OC_SB <- df.OC_SB[, Cumsum_Channel := cumsum(Ideal_Number_Channels), by ="Print_Branch"]


#########Merge in slotting capacity
df.OC_SB <- merge(
  df.OC_SB,
  Slotting_Param[Slot_Type == "OC_Shelf_Back", .(Print_Branch, Capacity_Slots)],
  by = "Print_Branch",
  all.x = TRUE
)

###Allocate
df.OC_SB[, Ideal_Area_OC_Shelf_Back := fifelse(Cumsum_Channel <= Capacity_Slots, "OC_Shelf_Back", "Not Allocated")]
df.OC_SB[, Allocated_OC_Circuit_Back := as.integer(Ideal_Area_OC_Shelf_Back == "OC_Shelf_Back")]


############# Combining Back into Main File
Main_Query_Final_3 <- left_join(Main_Query_Final_2,df.OC_SB)
Main_Query_Final_3 <- as.data.table(Main_Query_Final_3)
Main_Query_Final_3[Ideal_Area_OC_Shelf_Back == "OC_Shelf_Back", Ideal_Area := "OC_Shelf_Back"
][Ideal_Area_OC_Shelf_Back == "OC_Shelf_Back", Allocated := 1
]


Main_Query_Final_3[,c("Ideal_Area_OC_Shelf_Back","Allocated_OC_Circuit_Back","Cumsum_Channel","Acc_Per_Chan","Ideal_Number_Channels","Capacity_Slots") :=NULL]

Main_Query_Final_2 <- Main_Query_Final_3

rm(Main_Query_Final_3,df.OC_SB)

##test3<- Main_Query_Final_2[Print_Branch %in% c('504M','501G') & Ideal_Area == 'Not Allocated',]


Main_Query_Final_2 <- Main_Query_Final_2[Station %in% .("S","T","R") & Allocated == 0 & Location_Type %in% .("E","S","P"),Ideal_Area := "High_Value"
][Station %in% .("S","T","R") & Allocated == 0 & Location_Type %in% .("E","S","P"),Allocated := 1
]
##test4<- Main_Query_Final_2[Print_Branch %in% c('504M','501G') & Ideal_Area == 'Not Allocated',]
##write.csv(Main_Query_Final_2, file = "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/WorkSmart/Main_Query_Move_Check_Test.csv",
##          row.names = TRUE) 

Main_Query_Final_2[,Ideal_Slot := 0
][Ideal_Area %in% .("A-Frame Long","A-Frame Short","A-Frame Super Short"),Ideal_Slot := Ideal_Number_Channel
][Ideal_Area %in% .("OC_Shelf_Front","OC_Shelf_Middle","OC_Shelf_Back"),Ideal_Slot := Adj_Shelf_Slot
][Ideal_Area == "OC_Flow", Ideal_Slot := OC_Flow_Slot
][Ideal_Area == "OC_Tunnel_Long", Ideal_Slot := OC_Tunnel_Long_Slot
][Ideal_Area == "OC_Tunnel_Med", Ideal_Slot := OC_Tunnel_Med_Slot
][Ideal_Area == "OC_Tunnel_Short", Ideal_Slot := OC_Tunnel_Short_Slot
][Ideal_Area == "OC_Flow_Long" , Ideal_Slot := OC_Tunnel_Short_Slot
][Ideal_Area == "OC_Pallet", Ideal_Slot := 1
][Ideal_Area == "SF_Pallet", Ideal_Slot := 1
][Ideal_Area == "SF_Flow", Ideal_Slot := SF_Flow_Slot
  ]


#################Calc New Shelf STD                               
Main_Query_Final_2[,Shelf_STD := 0
][Ideal_Area %in% .("OC_Shelf_Front","OC_Shelf_Middle","OC_Shelf_Back") ,Shelf_STD := Adj_Shelf_Slot*Slot_Volume_Shelf/Volume
]

Main_Query_Final_2 <-Main_Query_Final_2[,OC_Shelf_New_Days_Cover :=  Shelf_STD/pmax(On_Circuit_Total_Qty), by = c("Print_Branch","Product_Code")
]

#######Calualte New Aframe Over Stock STD
Slotting_Param <-  as.data.table(Slotting_Param)
Slotting_Param_AF_Long <- Slotting_Param[Slot_Type == "A-Frame Long",c("Print_Branch","Slot_Volume")]
Slotting_Param_AF_Long <- setnames(Slotting_Param_AF_Long,"Slot_Volume", "Slot_Volume_AF_Long")


########## Join A-Frame Long to Main Table
Main_Query_Final_2 <- merge(
  Main_Query_Final_2,
  Slotting_Param_AF_Long,
  by = c('Print_Branch'),
  all.x = TRUE)

#######Calualte New Aframe Over Stock STD
Slotting_Param <-  as.data.table(Slotting_Param)
Slotting_Param_AF_Short <- Slotting_Param[Slot_Type == "A-Frame Short",c("Print_Branch","Slot_Volume")]
Slotting_Param_AF_Short <- setnames(Slotting_Param_AF_Short,"Slot_Volume", "Slot_Volume_AF_Short")


########## Join A-Frame Long to Main Table
Main_Query_Final_2 <- merge(
  Main_Query_Final_2,
  Slotting_Param_AF_Short,
  by = c('Print_Branch'),
  all.x = TRUE)


Main_Query_Final_2[,New_AFrame_Overstock_STD := 0
][Ideal_Area %in% .("A-Frame Long"), New_AFrame_Overstock_STD := (Ideal_Number_Long_Channel*Slot_Volume_AF_Long)/Volume
][Ideal_Area %in% .("A-Frame Short","A-Frame Super Short") ,New_AFrame_Overstock_STD := (Ideal_Number_Short_Channel*Slot_Volume_AF_Short)/Volume
]

############## Calcualte New Days Cover AFrame OVerStock

Main_Query_Final_2 <- Main_Query_Final_2[Ideal_Area %in% .("A-Frame Short","A-Frame Long"),New_AFrame_Overstock_Days_Cover :=  New_AFrame_Overstock_STD/pmax(On_Circuit_Total_Qty), by = c("Print_Branch","Product_Code")]

################# Consolidate New STD's
Main_Query_Final_2[,New_STD := 0
][Ideal_Area %in% .("A-Frame Long"),New_STD := New_AFrame_Overstock_STD
][Ideal_Area %in% .("A-Frame Short",),New_STD := New_AFrame_Overstock_STD
][Ideal_Area %in% .("OC_Shelf_Front","OC_Shelf_Middle","OC_Shelf_Back"),New_STD := Shelf_STD
][Ideal_Area == "OC_Flow", New_STD := OC_Flow_STD
][Ideal_Area == "OC_Tunnel_Long", New_STD := OC_Tunnel_Long_STD
][Ideal_Area == "OC_Tunnel_Med", New_STD := OC_Tunnel_Med_STD
][Ideal_Area == "OC_Tunnel_Short", New_STD := OC_Tunnel_Short_STD
][Ideal_Area == "OC_Flow_Long", New_STD := OC_Tunnel_Short_STD
][Ideal_Area == "OC_Pallet", New_STD := 1680000000/Volume
][Ideal_Area == "SF_Pallet", New_STD := 1680000000/Volume
][Ideal_Area == "SF_Flow", New_STD := SF_Flow_STD] 



################ shelf Days cover
Main_Query_Final_2 <-Main_Query_Final_2[,OC_Shelf_Days_Cover :=  Shelf_STD/max(On_Circuit_Total_Qty), by = c("Print_Branch","Product_Code")
]
###################### Current Days Cover
Main_Query_Final_2 <-Main_Query_Final_2[substring(Location_Code,2,2)!='-',Current_Day_Cover :=  Replenish_Quantity/max(On_Circuit_Total_Qty), by = c("Print_Branch","Product_Code")
]
Main_Query_Final_2 <-Main_Query_Final_2[substring(Location_Code,2,2)=='-',Current_Day_Cover :=  Overstock_STD/max(On_Circuit_Total_Qty), by = c("Print_Branch","Product_Code")
]
################### Consolidate New Days cover
Main_Query_Final_2[,New_Days_Cover := 9997.999
][Ideal_Area %in% .("A-Frame Long","A-Frame Short","A-Frame Super Short"),New_Days_Cover := New_AFrame_Overstock_Days_Cover
][Ideal_Area %in% .("OC_Shelf_Front","OC_Shelf_Middle","OC_Shelf_Back"),New_Days_Cover := OC_Shelf_Days_Cover
][Ideal_Area == "OC_Flow", New_Days_Cover := OC_Flow_Days_Cover
][Ideal_Area == "OC_Tunnel_Long", New_Days_Cover := OC_Tunnel_Long_Days_Cover
][Ideal_Area == "OC_Tunnel_Med", New_Days_Cover := OC_Tunnel_Med_Days_Cover
][Ideal_Area == "OC_Tunnel_Short", New_Days_Cover := OC_Tunnel_Short_Days_Cover
][Ideal_Area == "OC_Flow_Long", New_Days_Cover := OC_Tunnel_Short_Days_Cover
][Ideal_Area == "OC_Pallet", New_Days_Cover := SF_Pallet__Days_Cover
][Ideal_Area == "SF_Pallet", New_Days_Cover := SF_Pallet__Days_Cover
][Ideal_Area == "SF_Flow", New_Days_Cover := SF_Flow_Days_Cover] 

################ STFRP

Main_Query_Final_2[, STFRP_Current :=  1
][PR_Qty > 0, STFRP_Current :=  0
][,STFRP_New := 1
][PR_Qty > 0, STFRP_New := 0
][New_STD >= Current_Shelf_Balance & PR_Qty > 0 & Ideal_STD <= New_STD ,STFRP_New := 2]


###################### Replenishment Current

Main_Query_Final_2[,Current_Daily_Replen := 0
][Count_PR_Location > 0,Current_Daily_Replen := (21/(Current_Day_Cover*0.66))/21]

###################### Replenishment New
Main_Query_Final_2[,New_Daily_Replen := 0
][STFRP_New == 0,  New_Daily_Replen := (21/(New_Days_Cover*0.66))/21
][Warehouse_Zone %in% .('Bulk','Spec Hand'),New_Daily_Replen := Current_Daily_Replen]


###########################   Current Zone
Main_Query_Final_2[,Current_Area := Warehouse_Area
][Station == "A" & (Channel_Type == "Long" | Channel_Type == "long" ),Current_Area := "A-Frame Long"
][Station == "A" & (Channel_Type == "Short" | Channel_Type == "short" ),Current_Area := "A-Frame Short"
][Station == "A" & (Channel_Type == "Short 80"),Current_Area := "A-Frame Super Short"
][Location_Code == 'OSR',Current_Area := "OSR"
][Print_Branch == '405N'& Station == 'C',Current_Area := "MPS"
][Print_Branch %in% .('406P') & substring(Location_Code,1,2) %in% .('CL','DL','EL'),Current_Area := "OC_Tunnel_Long"
][Print_Branch %in% .('406P') & substring(Location_Code,1,2) %in% .('FL','GL','HL'),Current_Area := "OC_Tunnel_Med"
][Print_Branch %in% .('406P') & substring(Location_Code,1,2) %in% .('CR','DR','ER','FR','GR','HR'),Current_Area := "OC_Tunnel_Short"
][Print_Branch %in% .('406P') & substring(Location_Code,1,2) %in% .('JA','KA','LA'),Current_Area := "OC_Flow"
][Print_Branch %in% .('406P') & substring(Location_Code,1,4) %in% .('EL22','FL32','FL34','FL36','FL38'),Current_Area := "OC_Pallet"
][Print_Branch %in% .('406P') & substring(Location_Code,1,5) %in% .('CL085','DL165','EL265'),Current_Area := "OC_Pallet_Live"
][Print_Branch == '406P' & Station %in% .('J','K','L') & substring(Location_Code,2,2) != 'A',Current_Area := "OC_Shelf"
][Print_Branch == '406P' & Station %in% .('J','K','L') & substring(Location_Code,3,4) %in% .('01','02','03','04','05','06') & substring(Location_Code,2,2) != 'A',Current_Area := "OC_Shelf_Front"
][Print_Branch == '406P' & Station %in% .('J','K','L') & substring(Location_Code,3,4) %in% .('07','08','09','10','11','12','13','14') & substring(Location_Code,2,2) != 'A',Current_Area := "OC_Shelf_Middle"
][Print_Branch == '406P' & Station %in% .('J','K','L') & substring(Location_Code,3,4) %in% .('15','16','17','18','19','20','21','22') & substring(Location_Code,2,2) != 'A',Current_Area := "OC_Shelf_Back"
][Print_Branch == '305M' & Station %in% .('C','D','E','F','G','H','J','K') & substring(Location_Code,2,2) != 'A',Current_Area := "OC_Shelf"
][Station %in% .('C','D','E','F','G','H','J','K','L','M','N','P') & substring(Location_Code,2,2) == 'A',Current_Area := "OC_Flow"
][Print_Branch == '305M' & Station %in% .('C','D','E','F','G','H','J','K') & substring(Location_Code,3,3) %in% .('A','B','C','D','E','F','G','H') & substring(Location_Code,2,2) != 'A',Current_Area := "OC_Shelf_Front"
][Print_Branch == '305M' & Station %in% .('C','D','E','F','G','H','J','K') & substring(Location_Code,3,3) %in% .('J','K','L','M','N','P') & substring(Location_Code,2,2) != 'A',Current_Area := "OC_Shelf_Middle"
][Print_Branch == '305M' & Station %in% .('C','D','E','F','G','H','J','K') & substring(Location_Code,3,3) %in% .('Q','R','S','T','U','W') & substring(Location_Code,2,2) != 'A',Current_Area := "OC_Shelf_Back"
][Print_Branch == '305M' &  substring(Location_Code,1,3) %in% .('FJA','FKA'),Current_Area := "OC_Pallet"
][Print_Branch %in% .('305M') & substring(Location_Code,1,3) %in% .('BA0','BB0'),Current_Area := "SF_Pallet"
][Print_Branch %in% .('305M') & substring(Location_Code,1,3) %in% .('BBA','BBB','BBC'),Current_Area := "SF_Flow"
][Print_Branch %in% .('706S') & substring(Location_Code,1,2) %in% .('CL','DL','EL','FL','GL','HL'),Current_Area := "OC_Tunnel_Long"
][Print_Branch %in% .('706S') & substring(Location_Code,1,2) %in% .('CR','DR','ER','FR','GR','HR'),Current_Area := "OC_Tunnel_Short"
][Print_Branch %in% .('706S') & substring(Location_Code,1,1) %in% .('B'),Current_Area := "SF_Pallet"
][Print_Branch == '706S' & Station %in% .('J','K','L','M','N','P') & substring(Location_Code,2,2) != 'A',Current_Area := "OC_Shelf"
][Print_Branch == '706S' & Station %in% .('J','K','L','M','N','P') & substring(Location_Code,3,4) %in% .('01','02','03','04') & substring(Location_Code,2,2) != 'A',Current_Area := "OC_Shelf_Front"
][Print_Branch == '706S' & Station %in% .('J','K','L','M','N','P') & substring(Location_Code,3,4) %in% .('05','06','07','08') & substring(Location_Code,2,2) != 'A',Current_Area := "OC_Shelf_Middle"
][Print_Branch == '706S' & Station %in% .('J','K','L','M','N','P') & substring(Location_Code,3,4) %in% .('09','10','11','12') & substring(Location_Code,2,2) != 'A',Current_Area := "OC_Shelf_Back"
][Print_Branch == '706S' & substring(Location_Code,1,2) %in% .('JC','JD','JE','JF') ,Current_Area := "OC_Pallet"
][Print_Branch %in% .('703M') & substring(Location_Code,1,2) %in% .('CL','DL','EL'),Current_Area := "OC_Tunnel_Short"
][Print_Branch %in% .('703M') & substring(Location_Code,1,2) %in% .('CR','DR'),Current_Area := "OC_Tunnel_Long"
][Print_Branch %in% .('703M') & substring(Location_Code,1,3) %in% .('ERD','ERF','ERH','ERK','DRK'),Current_Area := "OC_Pallet"
][Print_Branch == '703M' & Station %in% .('F','G','H','J','K','L','M','N','P') & substring(Location_Code,2,2) != 'A',Current_Area := "OC_Shelf"
][Print_Branch == '703M' & Station %in% .('F','G','H','J','K','L','M','N','P') & substring(Location_Code,3,3) %in% .('A','B','C','D') & substring(Location_Code,2,2) != 'A',Current_Area := "OC_Shelf_Front"
][Print_Branch == '703M' & Station %in% .('F','G','H','J','K','L','M','N','P') & substring(Location_Code,3,3) %in% .('E','F','G','H','J','K') & substring(Location_Code,2,2) != 'A',Current_Area := "OC_Shelf_Middle"
][Print_Branch == '703M' & Station %in% .('F','G','H','J','K','L','M','N','P') & substring(Location_Code,3,3) %in% .('L','M','N','P') & substring(Location_Code,2,2) != 'A' ,Current_Area := "OC_Shelf_Back"
][Print_Branch == '703M' &  substring(Location_Code,1,3) %in% .('KEN') ,Current_Area := "OC_Shelf_OOS"
][Print_Branch %in% .('501G') & substring(Location_Code,1,2) %in% .('CL','DL','EL'),Current_Area := "OC_Tunnel_Long"
][Print_Branch %in% .('501G') & substring(Location_Code,1,2) %in% .('CR','DR','ER'),Current_Area := "OC_Tunnel_Short"
][Print_Branch %in% .('501G') & substring(Location_Code,1,4) %in% .('CRH5','DRH5','ERK5','EBR0'),Current_Area := "OC_Pallet"
][Print_Branch == '501G' & Station %in% .('F','G','H','J','K','L','M','N','P') & substring(Location_Code,2,2) != 'A',Current_Area := "OC_Shelf"
][Print_Branch == '501G' & Station %in% .('F','G','H','J','K','L','M','N','P') & substring(Location_Code,3,3) %in% .('A','B','C','D') & substring(Location_Code,2,2) != 'A' ,Current_Area := "OC_Shelf_Front"
][Print_Branch == '501G' & Station %in% .('F','G','H','J','K','L','M','N','P') & substring(Location_Code,3,3) %in% .('E','F','G','H','J','K') & substring(Location_Code,2,2) != 'A',Current_Area := "OC_Shelf_Middle"
][Print_Branch == '501G' & Station %in% .('F','G','H','J','K','L','M','N','P') & substring(Location_Code,3,3) %in% .('L','M','N','P') & substring(Location_Code,2,2) != 'A' ,Current_Area := "OC_Shelf_Back"
][Print_Branch == '501G' &  substring(Location_Code,1,2) %in% .('HH') ,Current_Area := "OC_Shelf_New_Line"
][Print_Branch == '501G' &  substring(Location_Code,1,2) %in% .('LJ') ,Current_Area := "High_Value"
][Print_Branch == '504M' & Station %in% .('C','D','E','F','G','H') & substring(Location_Code,2,2) != 'A',Current_Area := "OC_Shelf"
][Print_Branch == '504M' & Station %in% .('C','D','E') & substring(Location_Code,2,2)  == 'A',Current_Area := "OC_Flow_Long"
][Print_Branch == '504M' & Station %in% .('C','D','E','F','G','H') & substring(Location_Code,3,3) %in% .('A','B','C','D') & substring(Location_Code,2,2) != 'A' ,Current_Area := "OC_Shelf_Front"
][Print_Branch == '504M' & Station %in% .('C','D','E','F','G','H') & substring(Location_Code,3,3) %in% .('E','F','G','H') & substring(Location_Code,2,2) != 'A',Current_Area := "OC_Shelf_Middle"
][Print_Branch == '504M' & Station %in% .('C','D','E','F','G','H') & substring(Location_Code,3,3) %in% .('J','K','L','M','N','P') & substring(Location_Code,2,2) != 'A' ,Current_Area := "OC_Shelf_Back"
][Print_Branch %in% .('504M') & substring(Location_Code,1,4) %in% .('FGA1','EAD5','HAD4'),Current_Area := "OC_Pallet"
][Print_Branch %in% .('106M') & substring(Location_Code,1,2) %in% .('CR','DR','ER','FR','GR','HR'),Current_Area := "OC_Tunnel_Long"
][Print_Branch %in% .('106M') & substring(Location_Code,1,2) %in% .('CL','DL','EL','FL','GL','HL'),Current_Area := "OC_Tunnel_Short"
][Print_Branch %in% .('106M') & substring(Location_Code,1,3) %in% .('FRH','ERB'),Current_Area := "OC_Pallet"
][Print_Branch == '106M' & Station %in% .('J','K','L','M','N','P') & substring(Location_Code,2,2) != 'A',Current_Area := "OC_Shelf"
][Print_Branch == '106M' & Station %in% .('J','K','L','M','N','P') & substring(Location_Code,3,3) %in% .('A','B','C','D','E') & substring(Location_Code,2,2) != 'A' ,Current_Area := "OC_Shelf_Front"
][Print_Branch == '106M' & Station %in% .('J','K','L','M','N','P') & substring(Location_Code,3,3) %in% .('F','G','H','J','K','L') & substring(Location_Code,2,2) != 'A',Current_Area := "OC_Shelf_Middle"
][Print_Branch == '106M' & Station %in% .('J','K','L','M','N','P') & substring(Location_Code,3,3) %in% .('M','N','P','R') & substring(Location_Code,2,2) != 'A' ,Current_Area := "OC_Shelf_Back"
][Print_Branch %in% .('201D') & substring(Location_Code,1,2) %in% .('CR','DR','ER','FR','GR','HR'),Current_Area := "OC_Tunnel_Long"
][Print_Branch %in% .('201D') & substring(Location_Code,1,2) %in% .('CL','DL','EL','FL','GL','HL'),Current_Area := "OC_Tunnel_Short"
][Print_Branch == '201D' & Station %in% .('J','K','L','M','N','P') & substring(Location_Code,2,2) != 'A',Current_Area := "OC_Shelf"
][Print_Branch == '201D' & Station %in% .('J','K','L','M','N','P') & substring(Location_Code,3,3) %in% .('A','B','C','D','E','F') & substring(Location_Code,2,2) != 'A' ,Current_Area := "OC_Shelf_Front"
][Print_Branch == '201D' & Station %in% .('J','K','L','M','N','P') & substring(Location_Code,3,3) %in% .('G','H','J','K','L','M') & substring(Location_Code,2,2) != 'A',Current_Area := "OC_Shelf_Middle"
][Print_Branch == '201D' & Station %in% .('J','K','L','M','N','P') & substring(Location_Code,3,3) %in% .('N','P','Q','R') & substring(Location_Code,2,2) != 'A' ,Current_Area := "OC_Shelf_Back"
][Print_Branch %in% .('201D') & substring(Location_Code,1,3) %in% .('JHB','NDB','NEA','NFA','NGA','NHA','PDA','PEA','PFA','PGA','PHA'),Current_Area := "OC_Pallet"
][Location_Type %in% .('R','N'),Current_Area := "Fridge"
][Location_Type %in% .('C'),Current_Area := "CD"
][Location_Type %in% .('M'),Current_Area := "Cytotoxic"
][Location_Type %in% .('F'),Current_Area := "Flammable"
][Warehouse_Zone == 'Bulk',Current_Area := "Bulk"
][(substring(Product_Code,1,3) %in% .('MAI','ENT') & Full_Supplier == 'VES/005E'),Ideal_Area := 'Mailouts'
][(substring(Product_Code,1,3) %in% .('MAI','ENT') & Full_Supplier == 'VES/005E'),Allocated := 0
][Ideal_Area == "Mailouts", Ideal_Slot := 0]


Main_Query_Final_2[Print_Branch == '908B' & Station %in% .('C','D','E','F','G','H') & substring(Location_Code,2,2) != 'A',Current_Area := "OC_Shelf"
][Print_Branch == '908B' & Station %in% .('D','H') & substring(Location_Code,2,2) %in% .('F')  ,Current_Area := "OC_Shelf_Front"
][Print_Branch == '908B' & Station %in% .('G','H') & substring(Location_Code,2,2) %in% .('G')  ,Current_Area := "OC_Shelf_Front"
][Print_Branch == '908B' & Station %in% .('E','G') & substring(Location_Code,2,2) %in% .('H','J')  ,Current_Area := "OC_Shelf_Front"
][Print_Branch == '908B' & Station %in% .('H') & substring(Location_Code,2,2) %in% .('D')  ,Current_Area := "OC_Shelf_Middle"
][Print_Branch == '908B' & Station %in% .('H','D','G') & substring(Location_Code,2,2) %in% .('E')  ,Current_Area := "OC_Shelf_Middle"
][Print_Branch == '908B' & Station %in% .('E','G') & substring(Location_Code,2,2) %in% .('F')  ,Current_Area := "OC_Shelf_Middle"
][Print_Branch == '908B' & Station %in% .('E') & substring(Location_Code,2,2) %in% .('G')  ,Current_Area := "OC_Shelf_Middle"
][Print_Branch == '908B' & Station %in% .('D','E','G') & substring(Location_Code,2,2) %in% .('D')  ,Current_Area := "OC_Shelf_Back"
][Print_Branch == '908B' & Station %in% .('E') & substring(Location_Code,2,2) %in% .('E')  ,Current_Area := "OC_Shelf_Back"
][Print_Branch == '908B' & Station %in% .('G','H') & substring(Location_Code,2,2) %in% .('C')  ,Current_Area := "OC_Shelf_Back"
][Print_Branch %in% .('908B') & substring(Location_Code,1,4) %in% .('CAA0','CAB0','DAA0','EAA0','HAA0'),Current_Area := "OC_Pallet"
]

Main_Query_Final_2[Print_Branch == '602J' & Station %in% .('D','E','F','G','H','J','K','L','M','N','P') ,Current_Area := "OC_Shelf"
][Print_Branch == '602J' & Station %in% .('R'),Current_Area := "Bulk_Bottles"
][Print_Branch == '602J' & Station %in% .('Q') ,Current_Area := "Bulk"
][Print_Branch == '602J' & Station %in% .('D','E','F','G','H','J','K','L','M','N','P') & substring(Location_Code,2,3) %in% .('01','02','03','04','05','06','07','08','09','10','11','12')  ,Current_Area := "OC_Shelf_Front"
][Print_Branch == '602J' & Station %in% .('D','E','F','G','H','J','K','L','M','N','P') & substring(Location_Code,2,3) %in% .('13','14','15','16','17','18','19','20','21','22','23','24') ,Current_Area := "OC_Shelf_Middle"
][Print_Branch == '602J' & Station %in% .('D','E','F','G','H','J','K','L','M','N','P') & substring(Location_Code,2,3) %in% .('25','26','27','28','29','30','31','32','33','34','35','36')  ,Current_Area := "OC_Shelf_Back"
][Print_Branch %in% .('602J') & substring(Location_Code,1,3) %in% .('PAA','D02','D01','P05','D29','F29'),Current_Area := "OC_Pallet"
]

##########################
##########################




Main_Query_Final_2[, Action := "No Move"
][,Action :=  paste0(Current_Area," to ",Ideal_Area)
][Current_Area == Ideal_Area ,Action :=  "No Move"
][Station != "A" & Ideal_Area == "A-Frame Long", Action := "New A-Frame Long"
][Station != "A" & Ideal_Area == "A-Frame Short", Action := "New A-frame Short"
][Station != "A" & Ideal_Area == "A-Frame Super Short", Action := "New A-Frame Super Short"
][, "A-Frame_Channel_Change" := Ideal_Number_Long_Channel-Number_of_Auto_Channels
][Station == "A" & (Channel_Type == "Long" | Channel_Type == "long" ) & Ideal_Area == "A-Frame Long" 
  & (Ideal_Number_Long_Channel-Number_of_Auto_Channels) < 0
  , Action := "A-Frame Long Reduce"
][Station == "A" & (Channel_Type == "Long" | Channel_Type == "long" ) & Ideal_Area == "A-Frame Long" 
  & (Ideal_Number_Long_Channel-Number_of_Auto_Channels) > 0
  , Action := "A-Frame Long Increase"
][Station == "A" & Channel_Type == "Short" & Ideal_Area == "A-Frame Short" 
  & (Ideal_Number_Long_Channel-Number_of_Auto_Channels) > 0
  , Action := "A-Frame Short Increase"
][Station == "A" & Channel_Type == "Short" & Ideal_Area == "A-Frame Short" 
  & (Ideal_Number_Long_Channel-Number_of_Auto_Channels) < 0
  , Action := "A-Frame Short Decrease"
][Station == "A" & (Channel_Type == "Long" | Channel_Type == "long" ) 
  & Ideal_Area == "A-Frame Short", Action := "A-Frame Long to Short"
][Station == "A" & Channel_Type == "Short" 
  & Ideal_Area == "A-Frame Long", Action := "A-Frame Short to Long"
][Station == "A" & Daily_Demand <= 100 & substring(Ideal_Area,1,3) != "A-F", Action := "A-Frame Remove Demand < 100"
][Station == "A" & Daily_Demand > 100 & substring(Ideal_Area,1,3) != "A-F", Action := "A-Frame Remove Demand > 100"
][Station != "O" & Ideal_Area == "OSR",Action := "OSR_In"
]

#################### Join the before and after query
##Main_Query_Move_Check <- left_join(Main_Query_Final_2, Auto_Move_Check_After, by = c('Product_Code','Print_Branch'))  #### Join the Queries
Main_Query_Move_Check <- left_join(Main_Query_Final_2, Main_Query_Before, by = c('Product_Code','Print_Branch'))  #### Join the Queries
Main_Query_Move_Check <- left_join(Main_Query_Move_Check, Auto_Move_Check_Before, by = c('Product_Code','Print_Branch'))  #### Join the Queries
Main_Query_Move_Check <- left_join(Main_Query_Move_Check, Auto_Overstock_Before, by = c('Product_Code','Print_Branch'))  #### Join the Queries





############## Remove Inf from Days Cover Columns and Change to 9999
cols_days_cover_num <- names(Main_Query_Move_Check)[
  grepl("Cover", names(Main_Query_Move_Check), ignore.case = TRUE) &
    vapply(Main_Query_Move_Check, is.numeric, logical(1))
]

Main_Query_Move_Check[, (cols_days_cover_num) := lapply(.SD, function(x) fifelse(is.infinite(x), 9999, x)),
                      .SDcols = cols_days_cover_num]

############## Remove Inf from Days Replen and Change to 0
cols_Replen <- names(Main_Query_Move_Check)[
  grepl("Replen", names(Main_Query_Move_Check), ignore.case = TRUE) &
    vapply(Main_Query_Move_Check, is.numeric, logical(1))
]

Main_Query_Move_Check[, (cols_Replen) := lapply(.SD, function(x) fifelse(is.infinite(x), 0, x)),
                      .SDcols = cols_Replen]
############replace all NA with 0 on number columns
num_cols <- names(Main_Query_Move_Check)[sapply(Main_Query_Move_Check, is.numeric)]
Main_Query_Move_Check[, (num_cols) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = num_cols]

############## replace all NA with None in Character columns
chr_cols <- names(Main_Query_Move_Check)[sapply(Main_Query_Move_Check, is.character)]
Main_Query_Move_Check[, (chr_cols) := lapply(.SD, function(x) fifelse(is.na(x), 'None', x)), .SDcols = chr_cols]
########################## makes changes to DB

##### Remove NA and Inf
Main_Query_Move_Check[, names(Main_Query_Move_Check) :=
     lapply(.SD, function(x) {
       if (is.numeric(x)) {
         x[!is.finite(x)] <- 0
       }
       x
     })
]

password_file <- "C:/Users/harriesr/credentials/azure_supply_chain.txt"

if (!file.exists(password_file)) {
  stop(
    paste0(
      "Password file not found: ",
      password_file
    ),
    call. = FALSE
  )
}

db_password <- trimws(
  readLines(
    con  = password_file,
    n    = 1L,
    warn = FALSE
  )
)

if (length(db_password) != 1L || !nzchar(db_password)) {
  stop(
    "The password file is empty or invalid.",
    call. = FALSE
  )
}

# ------------------------------------------------------------------
# Open database connection
# ------------------------------------------------------------------

con <- DBI::dbConnect(
  odbc::odbc(),
  Driver   = "ODBC Driver 17 for SQL Server",
  Server   = "sqldwhscuks0001.ba8e81fcf26b.database.windows.net",
  Database = "AAHOperations",
  UID      = "Harriesr",
  PWD      = db_password,
  Port     = 1433
)

# Remove password variable from the R global environment
rm(db_password)

if (!DBI::dbIsValid(con)) {
  stop(
    "The connection to AAHOperations is not valid.",
    call. = FALSE
  )
}

# ------------------------------------------------------------------
# Write tables within one transaction
# ------------------------------------------------------------------

tryCatch(
  {
    DBI::dbWithTransaction(
      con,
      {
        DBI::dbWriteTable(
          con,
          name      = DBI::SQL("AAHOperations.dbo.worksmart_powerbi_data"),
          value     = Main_Query_Move_Check,
          overwrite = TRUE,
          row.names = FALSE
        )
        
        DBI::dbWriteTable(
          con,
          name      = DBI::SQL("AAHOperations.dbo.worksmart_final_dim"),
          value     = Final_Dim,
          overwrite = TRUE,
          row.names = FALSE
        )
      }
    )
    
    message("All AAHOperations tables were written successfully.")
  },
  error = function(e) {
    stop(
      paste0(
        "The SQL transaction failed and was rolled back: ",
        conditionMessage(e)
      ),
      call. = FALSE
    )
  },
  finally = {
    if (DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con)
      message("AAHOperations database connection closed.")
    }
  }
)


##############end code here


##write.csv(Main_Query_Move_Check, file = "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/WorkSmart/Main_Query_Move_Check.csv",
 ##         row.names = TRUE)
##

##write.csv(Main_Query_Move_Check, file = "X:/R Code Extracts/Worksmart/Output Folder/Main_Query_Move_Check.csv",
##          row.names = TRUE)


#############################
###################################### ad hoc SQL Table loads
############Import and convert to Data.table
###########################
##Sort_New_Area <- fread("X:/R Code Extracts/Worksmart/SQL/Sort_New_Area.csv")#
##Sort_Current_Area <- fread("X:/R Code Extracts/Worksmart/SQL/Sort_Current_Area.csv")
##Sort_Data_Current <- fread("X:/R Code Extracts/Worksmart/SQL/Sort_Data_Current.csv")
##Sort_Data_New <- fread("X:/R Code Extracts/Worksmart/SQL/Sort_Data_New.csv")
##DC_Codes <- fread("X:/R Code Extracts/Worksmart/SQL/DC_Codes.csv")

######################### makes changes to DB
## 7) Output to Azure SQL (append)
#####################################
##con <- DBI::dbConnect(odbc::odbc(),
##                      Driver   = "ODBC Driver 17 for SQL Server",
##                      Server   = "sqldwhscuks0001.ba8e81fcf26b.database.windows.net",
##                      Database = "AAHOperations",
##                      UID      = "Harriesr",
##                      PWD      = "G00ners4every0)",
##                      Port     = 1433)

##on.exit(DBI::dbDisconnect(con), add = TRUE)


##DBI::dbWithTransaction(con, {
##  DBI::dbWriteTable(con, name = SQL("AAHOperations.dbo.worksmart_tier_one_area_Sort_new"),
##                    value = Sort_New_Area, overwrite = TRUE, row.names = FALSE)
##  DBI::dbWriteTable(con, name = SQL("AAHOperations.dbo.worksmart_tier_one_area_Sort_current"),
##                    value = Sort_Current_Area, overwrite = TRUE, row.names = FALSE)
##  DBI::dbWriteTable(con, name = SQL("AAHOperations.dbo.worksmart_tier_two_area_Sort_new"),
##                    value = Sort_Data_New, overwrite = TRUE, row.names = FALSE)
##  DBI::dbWriteTable(con, name = SQL("AAHOperations.dbo.worksmart_tier_two_area_Sort_current"),
##                    value = Sort_Data_Current, overwrite = TRUE, row.names = FALSE)
##  DBI::dbWriteTable(con, name = SQL("AAHOperations.dbo.worksmart_dc_codes"),
##                    value = DC_Codes, overwrite = TRUE, row.names = FALSE)
##})connection_function = function() {

##AA_Temp_table_ON = data.table(AA_Temp_table_ON)
##AA_Temp_table_ON[, c("L_Ave_Lines","L_Ave_Qty","L_Max_Qty","L_Max_Lines","L_SD_Lines","L_SD_Qty","L_Days_Picked","S_Ave_Lines","S_Ave_Qty",
##                     "S_Max_Qty","S_Max_Lines","S_SD_Lines","S_SD_Qty","S_Days_Picked")
##][is.na(AA_Temp_table_ON[, c("L_Ave_Lines","L_Ave_Qty","L_Max_Qty","L_Max_Lines","L_SD_Lines","L_SD_Qty",
##                             "L_Days_Picked","S_Ave_Lines","S_Ave_Qty","S_Max_Qty","S_Max_Lines","S_SD_Lines","S_SD_Qty","S_Days_Picked")])] <- 0

##AA_Temp_table_AB = data.table(AA_Temp_table_AB)
##AA_Temp_table_AB[, c("L_AB_Ave_Lines","L_AB_Ave_Qty","L_AB_Days_Picked","S_AB_Ave_Lines","S_AB_Ave_Qty","S_AB_Days_Picked")
##][is.na(AA_Temp_table_AB[, c("L_AB_Ave_Lines","L_AB_Ave_Qty","L_AB_Days_Picked","S_AB_Ave_Lines","S_AB_Ave_Qty","S_AB_Days_Picked")])] <- 0


##AA_Temp_table_AFM = data.table(AA_Temp_table_AFM)
##AA_Temp_table_AFM[, c("L_AFM_Ave_Lines","L_AFM_Ave_Qty","L_AFM_Days_Picked","S_AFM_Ave_Lines","S_AFM_Ave_Qty","S_AFM_Days_Picked")
##][is.na(AA_Temp_table_AFM[, c("L_AFM_Ave_Lines","L_AFM_Ave_Qty","L_AFM_Days_Picked","S_AFM_Ave_Lines","S_AFM_Ave_Qty","S_AFM_Days_Picked")])] <- 0

##Main_Query_Final = data.table(Main_Query_Final)
##Main_Query_Final[,c("Number_of_Auto_Channels","Total_Auto_Stock_Holding","Assembly_Qty","OverStock_Location","Overstock_STD","Count_PR_Location","PR_Qty","PR_Qty_MAX")
##][is.na(Main_Query_Final[, c("Number_of_Auto_Channels","Total_Auto_Stock_Holding","Assembly_Qty","OverStock_Location","Overstock_STD","Count_PR_Location","PR_Qty","PR_Qty_MAX")])] <- 0


####### Merge Sales on to Main Table
