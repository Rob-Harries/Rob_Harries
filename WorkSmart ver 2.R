rm(list=ls())


setwd('C:/Users/harriesr/OneDrive - McKesson Europe/R Coding/')

library(RODBC); library(data.table); library(lubridate); library(foreach); library(dplyr); library(plyr); library(matrixStats)

##R package tstools has the ability to run ABC and XYZ analysis library(tstools)
##https://kourentzes.com/forecasting/2016/10/15/abc-xyz-analysis-for-forecasting/

connection_function = function() {
  pwd = readline("Enter password: ")
  con <<- odbcConnect("SolariDB Supply Chain", uid = "Harriesr", pwd = pwd)
}; connection_function();

On_Circuit_sales_Local = sqlQuery(con, paste("SELECT Date, Print_Branch, Product_Code, sum(Number_Lines) as Number_lines, sum(Line_Quantity) as Line_Quantity",
                                        #"CASE WHEN Print_Branch = Account_Branch THEN 'Local' ELSE 'Switch' END AS Local_switch",
                                        "FROM [SupplyChainReporting].[dbo].[daily_sales_on_circuit] dsoc",
                                        "WHERE Print_Branch in ('101c' , '106m','201D' ,'204j' , '305m','402h' ,'405n', '406p' ,'501g' ,'502x' , '504m', '602j' ,'703m' , '706s', '908b')",
                                       "and Date >= DATEADD(day,-32, getdate())",
                                      # "and Date between '2019-05-16' and '2019-06-17'",
                                        "and Omit_Marker = 0",
                                        "and (CASE WHEN Print_Branch = Account_Branch THEN 'Local' ELSE 'Switch' END) = 'Local'",
                                       # "and Product_Code = 'VEN0164W'",
                                        "GROUP BY Print_Branch, Date, Product_Code"))
 

On_Circuit_sales_Switch = sqlQuery(con, paste("SELECT Date, Print_Branch, Product_Code, sum(Number_Lines) as Number_lines, sum(Line_Quantity) as Line_Quantity",
                                             #"CASE WHEN Print_Branch = Account_Branch THEN 'Local' ELSE 'Switch' END AS Local_switch",
                                             "FROM [SupplyChainReporting].[dbo].[daily_sales_on_circuit] dsoc",
                                             "WHERE Print_Branch in ('101c' , '106m','201D' ,'204j' , '305m','402h' ,'405n', '406p' ,'501g' ,'502x' , '504m', '602j' ,'703m' , '706s', '908b')",
                                              "and Date >= DATEADD(day,-32, getdate())",
                                             #"and Date between '2019-05-16' and '2019-06-17'",
                                             "and Omit_Marker = 0",
                                             "and (CASE WHEN Print_Branch = Account_Branch THEN 'Local' ELSE 'Switch' END) = 'Switch'",
                                             # "and Product_Code = 'VEN0164W'",
                                             "GROUP BY Print_Branch, Date, Product_Code"))


AFrameMan_sales_Local = sqlQuery(con, paste("SELECT Date, Print_Branch, Product_Code, sum(Number_Lines) as Number_lines, sum(Line_Quantity) as Line_Quantity",
                                              #"CASE WHEN Print_Branch = Account_Branch THEN 'Local' ELSE 'Switch' END AS Local_switch",
                                              "FROM [SupplyChainReporting].[dbo].[daily_sales_aframe_manual] afm",
                                              "WHERE Print_Branch in ('101c' , '106m','201D' ,'204j' , '305m','402h' ,'405n', '406p' ,'501g' ,'502x' , '504m', '602j' ,'703m' , '706s', '908b')",
                                               "and Date >= DATEADD(day,-32, getdate())",
                                              # "and Date between '2019-05-16' and '2019-06-17'",
                                              "and Omit_Marker = 0",
                                              "and (CASE WHEN Print_Branch = Account_Branch THEN 'Local' ELSE 'Switch' END) = 'Local'",
                                              # "and Product_Code = 'VEN0164W'",
                                              "GROUP BY Print_Branch, Date, Product_Code"))


AFrameMan_sales_Switch = sqlQuery(con, paste("SELECT Date, Print_Branch, Product_Code, sum(Number_Lines) as Number_lines, sum(Line_Quantity) as Line_Quantity",
                                             #"CASE WHEN Print_Branch = Account_Branch THEN 'Local' ELSE 'Switch' END AS Local_switch",
                                             "FROM [SupplyChainReporting].[dbo].[daily_sales_aframe_manual] afm",
                                             "WHERE Print_Branch in ('101c' , '106m','201D' ,'204j' , '305m','402h' ,'405n', '406p' ,'501g' ,'502x' , '504m', '602j' ,'703m' , '706s', '908b')",
                                              "and Date >= DATEADD(day,-32, getdate())",
                                             #"and Date between '2019-05-16' and '2019-06-17'",
                                             "and Omit_Marker = 0",
                                             "and (CASE WHEN Print_Branch = Account_Branch THEN 'Local' ELSE 'Switch' END) = 'Switch'",
                                             # "and Product_Code = 'VEN0164W'",
                                             "GROUP BY Print_Branch, Date, Product_Code"))

AutoBulk_sales_Local = sqlQuery(con, paste("SELECT Date, Print_Branch, Product_Code, sum(Number_Lines) as Number_lines, sum(Line_Quantity) as Line_Quantity",
                                            #"CASE WHEN Print_Branch = Account_Branch THEN 'Local' ELSE 'Switch' END AS Local_switch",
                                            "FROM [SupplyChainReporting].[dbo].[daily_sales_autobulk] ab",
                                            "WHERE Print_Branch in ('101c' , '106m','201D' ,'204j' , '305m','402h' ,'405n', '406p' ,'501g' ,'502x' , '504m', '602j' ,'703m' , '706s', '908b')",
                                             "and Date >= DATEADD(day,-32, getdate())",
                                            #"and Date between '2019-05-16' and '2019-06-17'",
                                            "and Omit_Marker = 0",
                                            "and (CASE WHEN Print_Branch = Account_Branch THEN 'Local' ELSE 'Switch' END) = 'Local'",
                                            # "and Product_Code = 'VEN0164W'",
                                              "GROUP BY Print_Branch, Date, Product_Code"))

AutoBulk_sales_Switch = sqlQuery(con, paste("SELECT Date, Print_Branch, Product_Code, sum(Number_Lines) as Number_lines, sum(Line_Quantity) as Line_Quantity",
                                           #"CASE WHEN Print_Branch = Account_Branch THEN 'Local' ELSE 'Switch' END AS Local_switch",
                                           "FROM [SupplyChainReporting].[dbo].[daily_sales_autobulk] ab",
                                           "WHERE Print_Branch in ('101c' , '106m','201D' ,'204j' , '305m','402h' ,'405n', '406p' ,'501g' ,'502x' , '504m', '602j' ,'703m' , '706s', '908b')",
                                            "and Date >= DATEADD(day,-32, getdate())",
                                           #"and Date between '2019-05-16' and '2019-06-17'",
                                           "and Omit_Marker = 0",
                                           "and (CASE WHEN Print_Branch = Account_Branch THEN 'Local' ELSE 'Switch' END) = 'Switch'",
                                           # "and Product_Code = 'VEN0164W'",
                                           "GROUP BY Print_Branch, Date, Product_Code"))


Main_Query = sqlQuery(con, paste("SELECT Date, Branch, Product_Code, Location_Code,First_Receipt_Date,Replenish_Quantity, S_Category, Manual_Sales_Category,Demand, Current_Shelf_Balance, Current_Order_Balance",
                                 "FROM [SupplyChainReporting].[dbo].[main_query] mq",
                                 "WHERE Branch in ('101c' , '106m' , '201d' , '204j' , '305m' , '402h' , '405n' , '406p' , '501g' , '502x', '504m' , '602j' , '703m' , '706s' , '908b')",
                                  "and Op_Shelf_Balance = 0",
                                 "and Location_Code != '   '",
                               #   "and Date = DATEADD(day,-1, getdate()))",
                                 "and Date >= DATEADD(day,-1, getdate())"))


Product_data = sqlQuery(con, paste("SELECT Product_Code, Product_Description, Pack_Size, Product_Group, Report_Group,Location_Type,", 
                                   "Full_Supplier, Umbrella_Group, Insertion_Date,Discontinued_Date, Hospital_Restriction,Depth, Height,Width",
                                  "FROM [SupplyChainReporting].[dbo].[product_list] pl"))


Auto_Locations <- read.delim("C:/Users/HarriesR/OneDrive - McKesson Europe/R Coding/txt files/Worksmart/WSAutoLoc.txt",header = TRUE, sep = "\t")
Auto_Overstock <- read.delim("C:/Users/HarriesR/OneDrive - McKesson Europe/R Coding/txt files/Worksmart/WSAutoLocOverStock.txt",header = TRUE, sep = "\t")



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


############# Change Headings

Auto_Locs <- setnames(Auto_Locs, old =c("branch_id","product_code","MAX.sequence_no","SUM.replenish_qty","SUM.assembly_qty","SUM.dispense") 
                      , new =c("Print_Branch","Product_Code","Number_of_Auto_Channels","Total_Auto_Stock_Holding","Assembly_Qty","Dispensing_Speed"))

Auto_Overstock <- setnames(Auto_Overstock, old =c("branch_id","product_code","MIN.loc_code","SUM.replenish_qty") 
                      , new =c("Print_Branch","Product_Code","Overstock_Location","Overstock_Stock_Holding"))

Main_Query <- setnames(Main_Query, old ="Branch" , new ="Print_Branch")


### remove Old DF's
rm(On_Circuit_sales_Local,On_Circuit_sales_Switch,AutoBulk_sales_Local,AutoBulk_sales_Switch,AFrameMan_sales_Local,AFrameMan_sales_Switch)

################## Local Dimnensions

Local_Dim <- read.delim("C:/Users/HarriesR/OneDrive - McKesson Europe/R Coding/txt files/Worksmart/LocalDim.txt",header = TRUE, sep = "\t")
Local_Dim = as.data.table(Local_Dim)
Local_Dim <- Local_Dim[height %between% .(1,550) & width %between% .(1,550) & depth %between% .(1,550)]
setnames(Local_Dim, c("height","width","depth"), c("Local_H","Local_W","Local_D"))

Local_Dim <- Local_Dim[,.(MaxDim_L = max(Local_H,Local_W,Local_D)
                          ,MinDim_L = min(Local_H,Local_W,Local_D)
                          ,MedDim_L = sum(Local_H,Local_W,Local_D)-(max(Local_H,Local_W,Local_D)+min(Local_H,Local_W,Local_D)))
                       ,by = .(product_code,branch_id,Local_H,Local_W,Local_D)]

Local_Dim_ave <- Local_Dim[,.(Ave_Max=round(mean(MaxDim_L)),
                              Ave_Min = round(mean(MinDim_L)),
                              Ave_Med = round(mean(MedDim_L))),
                           by = .(product_code)]


####### adds Daily.Averages, Max, Frequency and SD


L_ON_C_DT_Agg <- L_ON_C_DT[,.(L_Ave_Lines=sum(Number_lines, na.rm = T)/21,
                                    L_Ave_Qty=sum(Line_Quantity, na.rm = T)/21,
                                    L_Max_Qty=max(Line_Quantity, na.rm = T),
                                    L_Max_Lines=max(Number_lines, na.rm = T),
                                    L_SD_Lines=sd(Number_lines, na.rm = T),
                                    L_SD_Qty=sd(Line_Quantity, na.rm = T),
                                    L_Days_Picked=(.N)),
                                    by=list(Product_Code,Print_Branch)]

S_ON_C_DT_Agg <- S_ON_C_DT[,.(S_Ave_Lines=sum(Number_lines, na.rm = T)/21,
                            S_Ave_Qty=sum(Line_Quantity, na.rm = T)/21,
                            S_Max_Qty=max(Line_Quantity, na.rm = T),
                            S_Max_Lines=max(Number_lines, na.rm = T),
                            S_SD_Lines=sd(Number_lines, na.rm = T),
                            S_SD_Qty=sd(Line_Quantity, na.rm = T),
                            S_Days_Picked=(.N)),
                         by=list(Product_Code,Print_Branch)]


L_AB_DT_Agg <- L_AB_DT[,.(L_AB_Ave_Lines=sum(Number_lines, na.rm = T)/21,
                                                L_AB_Ave_Qty=sum(Line_Quantity, na.rm = T)/21,
                                                L_AB_Days_Picked=(.N)),
                                                  by=list(Product_Code,Print_Branch)]

S_AB_DT_Agg <- S_AB_DT[,.(S_AB_Ave_Lines=sum(Number_lines, na.rm = T)/21,
                      S_AB_Ave_Qty=sum(Line_Quantity, na.rm = T)/21,
                      S_AB_Days_Picked=(.N)),
                   by= .(Product_Code,Print_Branch)]

L_AFM_DT_Agg <- L_AFM_DT[,.(L_AFM_Ave_Lines=sum(Number_lines, na.rm = T)/21,
                          L_AFM_Ave_Qty=sum(Line_Quantity, na.rm = T)/21,
                          L_AFM_Days_Picked=(.N)),
                       by=list(Product_Code,Print_Branch)]

S_AFM_DT_Agg <- S_AFM_DT[,.(S_AFM_Ave_Lines=sum(Number_lines, na.rm = T)/21,
                        S_AFM_Ave_Qty=sum(Line_Quantity, na.rm = T)/21,
                        S_AFM_Days_Picked=(.N)),
                     by=list(Product_Code,Print_Branch)]



rm(L_ON_C_DT,S_ON_C_DT,L_AB_DT,S_AB_DT,L_AFM_DT,S_AFM_DT)


##### Merge Main Query with Summary Sales Table
Main_Query_Final <- left_join(Main_Query, Product_data, by = c('Product_Code'))  #### Join the Queries
Main_Query_Final <- left_join(Main_Query, Auto_Locs, by = c('Product_Code','Print_Branch'))  #### Join the Queries


AA_Temp_table_ON <- left_join(Main_Query[2:3], L_ON_C_DT_Agg, by = c('Product_Code','Print_Branch'))  #### Join the Queries
AA_Temp_table_ON <- left_join(AA_Temp_table_ON, S_ON_C_DT_Agg, by = c('Product_Code','Print_Branch'))  #### Join the Queries

AA_Temp_table_AB <- left_join(Main_Query[2:3], L_AB_DT_Agg, by = c('Product_Code','Print_Branch'))  #### Join the Queries
AA_Temp_table_AB <- left_join(AA_Temp_table_AB, S_AB_DT_Agg, by = c('Product_Code','Print_Branch'))

AA_Temp_table_AFM <- left_join(Main_Query[2:3], L_AFM_DT_Agg, by = c('Product_Code','Print_Branch'))  #### Join the Queries
AA_Temp_table_AFM <- left_join(AA_Temp_table_AFM, S_AFM_DT_Agg, by = c('Product_Code','Print_Branch'))


##### change n/a to 0's for numerical Fields##############################################################

AA_Temp_table_ON = data.table(AA_Temp_table_ON)
AA_Temp_table_ON[, c("L_Ave_Lines","L_Ave_Qty","L_Max_Qty","L_Max_Lines","L_SD_Lines","L_SD_Qty","L_Days_Picked","S_Ave_Lines","S_Ave_Qty",
                       "S_Max_Qty","S_Max_Lines","S_SD_Lines","S_SD_Qty","S_Days_Picked")
                      ][is.na(AA_Temp_table_ON[, c("L_Ave_Lines","L_Ave_Qty","L_Max_Qty","L_Max_Lines","L_SD_Lines","L_SD_Qty",
                                                   "L_Days_Picked","S_Ave_Lines","S_Ave_Qty","S_Max_Qty","S_Max_Lines","S_SD_Lines","S_SD_Qty","S_Days_Picked")])] <- 0

AA_Temp_table_AB = data.table(AA_Temp_table_AB)
AA_Temp_table_AB[, c("L_AB_Ave_Lines","L_AB_Ave_Qty","L_AB_Days_Picked","S_AB_Ave_Lines","S_AB_Ave_Qty","S_AB_Days_Picked")
                 ][is.na(AA_Temp_table_AB[, c("L_AB_Ave_Lines","L_AB_Ave_Qty","L_AB_Days_Picked","S_AB_Ave_Lines","S_AB_Ave_Qty","S_AB_Days_Picked")])] <- 0


AA_Temp_table_AFM = data.table(AA_Temp_table_AFM)
AA_Temp_table_AFM[, c("L_AFM_Ave_Lines","L_AFM_Ave_Qty","L_AFM_Days_Picked","S_AFM_Ave_Lines","S_AFM_Ave_Qty","S_AFM_Days_Picked")
                 ][is.na(AA_Temp_table_AFM[, c("L_AFM_Ave_Lines","L_AFM_Ave_Qty","L_AFM_Days_Picked","S_AFM_Ave_Lines","S_AFM_Ave_Qty","S_AFM_Days_Picked")])] <- 0

Main_Query_Final = data.table(Main_Query_Final)
Main_Query_Final[,c("Number_of_Auto_Channels","Total_Auto_Stock_Holding","Assembly_Qty","Dispensing_Speed")
                  ][is.na(Main_Query_Final[, c("Number_of_Auto_Channels","Total_Auto_Stock_Holding","Assembly_Qty","Dispensing_Speed")])] <- 0


####### Merge back into one big table

Main_Query_Final <- left_join(Main_Query_Final, AA_Temp_table_ON, by = c('Product_Code','Print_Branch'))  #### Join the Queries
Main_Query_Final <- left_join(Main_Query_Final,AA_Temp_table_AB, by = c('Product_Code','Print_Branch'))  #### Join the Queries
Main_Query_Final <- left_join(Main_Query_Final,AA_Temp_table_AFM, by = c('Product_Code','Print_Branch'))   #### Join the Queries


############# Remove all Old Tables

rm(AA_Temp_table_AB,AA_Temp_table_AFM,AA_Temp_table_ON,Auto_Locations,Auto_Locs,Auto_Overstock,
   L_AB_DT_Agg,L_AFM_DT_Agg,L_ON_C_DT_Agg,Local_Dim,Main_Query,S_AB_DT_Agg,S_AFM_DT_Agg,S_ON_C_DT_Agg,Product_data)


#######################################################################################################
#######################################################################################################
################################################## checks

Main_Query_Final1 <- data.table(Main_Query_Final)

Main_Query_Final1 <- Main_Query_Final1[,.(L_Ave_Lines=sum((L_Ave_Lines+S_Ave_Lines), na.rm = T),
                                          S_Ave_Lines=sum(S_Ave_Lines, na.rm = T)),
                                       by=list(Print_Branch)]



Main_Query_Final1 <- Main_Query_Final1[,.(L_Ave_Lines=sum(L_Ave_Lines, na.rm = T),
                                          S_Ave_Lines=sum(S_Ave_Lines, na.rm = T))]


###### Things to do 
### Local Dimension Files  - Do Prir of Join
### SDA Rules
### SDA CHannel Calcs
### Sales Cat Logic - Do Prior to join
### LMS Rules
### OSR Rules

####### Allocate SDA's all at the same time




################################ Write CSV
###write.csv(L_ON_C_DT_Agg_501G, file = "C:/Users/harriesr/OneDrive - McKesson Europe/R Coding/txt files/Stock PLatform/L_ON_C_DT_Agg_501G.csv",
##          row.names = TRUE)

##write.csv(S_ON_C_DT_Agg_501G, file = "C:/Users/harriesr/OneDrive - McKesson Europe/R Coding/txt files/Stock PLatform/S_ON_C_DT_Agg_501G.csv",
##          row.names = TRUE)
























###########################################################################################################################################
############################################  https://cloud.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
###Data.table training OR NOT REQUIRED CODE

#Sales_table <- inner_join(df.AVE, df.AVE1, by = c('Product_Code','Print_Branch','Local_switch'))  #### Join the Queries
#Sales_table <- setnames(Sales_table, old ="N" , new ="Frequency")  ##### rename the N Column to Frequecny

#df.SD <- On_Circuit_sales_noSUN_table[, lapply(.SD, {function (x) sd(x, na.rm = TRUE)}), by = c('Product_Code','Print_Branch', 'Local_switch')]

###DT[i, j, by]

##   R:                 i                 j        by
## SQL:  where | order by   select | update  group by


#####Exmaple of Data.Frame Subset
On_Circuit_sales_tabletest <- On_Circuit_sales_table[Product_Code == 'VEN0164W' & Print_Branch == '406P']
head(On_Circuit_sales_tabletest)

#####Exmaple of Data.Frame Subset rows
On_Circuit_sales_tabletest <- On_Circuit_sales_table[1:3]
head(On_Circuit_sales_tabletest)

#####Example of Data.Frame Sorting
On_Circuit_sales_tabletest <- On_Circuit_sales_table[order(Product_Code, -Number_lines)]
head(On_Circuit_sales_tabletest)

#### Example of Selecting Columns you can use .() or list()
On_Circuit_sales_tabletest <- On_Circuit_sales_table[, .(Product_Code, Print_Branch)]
head(On_Circuit_sales_tabletest)

#### Example of Selecting Columns and Rename them  [,.(rename = Oringal)] 
On_Circuit_sales_tabletest <- On_Circuit_sales_table[, .(Lines=Number_lines, Qty=Line_Quantity)]
head(On_Circuit_sales_tabletest)

#### Example of Selecting Columns and ask how many Lines have a combined total of over 2000
On_Circuit_sales_tabletest <- On_Circuit_sales_table[, .(Lines=Number_lines, Qty=Line_Quantity)]
On_Circuit_sales_tabletest <- On_Circuit_sales_tabletest[, sum((Lines + Qty) > 2000)]
head(On_Circuit_sales_tabletest)

#### Example of Subsetting then calcualted a mean from the Subset
On_Circuit_sales_tabletest <- On_Circuit_sales_table[Product_Code == 'VEN0164W' & Print_Branch == '406P',
                                                     .(Mean_Lines=mean(Number_lines), Mean_Qty=mean(Line_Quantity))] 
head(On_Circuit_sales_tabletest)

#### Example of Subsetting then use len command to show how many rows have been produced   NOTE THAT .N DOES THE SAME THING
On_Circuit_sales_tabletest <- On_Circuit_sales_table[Product_Code == 'VEN0164W' & Print_Branch == '406P', length(Number_lines)]
On_Circuit_sales_tabletest <- On_Circuit_sales_table[Product_Code == 'VEN0164W' & Print_Branch == '305M', .N]
head(On_Circuit_sales_tabletest)

#### refering to column names
On_Circuit_sales_tabletest <- On_Circuit_sales_table[, c("Number_lines" ,"Line_Quantity")]
head(On_Circuit_sales_tabletest)

####### you can use .. to slect columns named in a variable
selectcol <- c("Number_lines" ,"Line_Quantity")
On_Circuit_sales_tabletest <- On_Circuit_sales_table[, ..selectcol]
head(On_Circuit_sales_tabletest)


####### you can use .. the With Command disables the ability to refer to columns and returns them to more tradtional R data.frame
selectcol <- c("Number_lines" ,"Line_Quantity")
On_Circuit_sales_tabletest <- On_Circuit_sales_table[, ..selectcol, with = FALSE]
head(On_Circuit_sales_tabletest)

############# Negative select all apart from eitehr use !c or -c

On_Circuit_sales_tabletest <- On_Circuit_sales_table[, !c("Number_lines" ,"Line_Quantity")]
head(On_Circuit_sales_tabletest)
On_Circuit_sales_tabletest <- On_Circuit_sales_table[, -c("Number_lines" ,"Line_Quantity")]
head(On_Circuit_sales_tabletest)

############# can select ranges using the : sperators
On_Circuit_sales_tabletest <- On_Circuit_sales_table[, Product_Code:Line_Quantity]
head(On_Circuit_sales_tabletest)

########### and the negative versions of range selection !() or -()

On_Circuit_sales_tabletest <- On_Circuit_sales_table[, -(Product_Code:Line_Quantity)]
head(On_Circuit_sales_tabletest)

On_Circuit_sales_tabletest <- On_Circuit_sales_table[, !(Product_Code:Line_Quantity)]
head(On_Circuit_sales_tabletest)

######### Aggregations --:)  .N Counts rows by command 
On_Circuit_sales_tabletest <- On_Circuit_sales_table[, .N, by = .(Date)]
head(On_Circuit_sales_tabletest)

######### Aggregations --:)  .N Counts rows by command this time using a character vector
On_Circuit_sales_tabletest <- On_Circuit_sales_table[, .(.N), by = "Date"]
head(On_Circuit_sales_tabletest)

######### Aggregations --:)  .N Counts rows by command with a subset
On_Circuit_sales_tabletest <- On_Circuit_sales_table[Print_Branch == "406P", .N, by = .(Date, Local_switch)]
head(On_Circuit_sales_tabletest)


######### Aggregations --:)  Summing  by command with a subset
On_Circuit_sales_tabletest <- On_Circuit_sales_table[Print_Branch == "406P",
                                                      .(sum(Line_Quantity),sum(Number_lines)), by = .(Date, Local_switch)]
head(On_Circuit_sales_tabletest)

######### Aggregations --:)  Mean and Max by command with a subset
On_Circuit_sales_tabletest <- On_Circuit_sales_table[Print_Branch == "406P",
                                                     .(mean(Line_Quantity),max(Line_Quantity)), by = .(Date, Local_switch ,Product_Code)]
head(On_Circuit_sales_tabletest)

######### Aggregations --:)  Min and ave by command with a subset
On_Circuit_sales_tabletest <- On_Circuit_sales_table[Print_Branch == "406P",
                                                     .(min(Line_Quantity),ave(Line_Quantity)), by = .(Date, Local_switch ,Product_Code)]
head(On_Circuit_sales_tabletest)


######### Aggregations --:)  Sorting by Keyby and naming the new columns
On_Circuit_sales_tabletest <- On_Circuit_sales_table[Print_Branch == "406P",
                                                     .(Ave_Qty=sum(Line_Quantity)/21,Ave_Lines=sum(Number_lines)/21), 
                                                     keyby = .(Print_Branch,Local_switch ,Product_Code)]
head(On_Circuit_sales_tabletest)


##################### CHAINING ...... 
######### PArt 1 
On_Circuit_sales_tabletest <- On_Circuit_sales_table[Print_Branch == "406P", .N, by = .(Date, Local_switch)]
head(On_Circuit_sales_tabletest)
######## Part 2 order on date Asending and - = descending)
On_Circuit_sales_tabletest <- On_Circuit_sales_tabletest[order(Date, -Local_switch)]
head(On_Circuit_sales_tabletest)
###### Chained example much quicker and more effecient use of memeory
On_Circuit_sales_tabletest <- On_Circuit_sales_table[Print_Branch == "406P", .N, by = .(Date, Local_switch)
                                                     ][order(Date, -Local_switch)]
head(On_Circuit_sales_tabletest,20)

############# Expression in the By 

On_Circuit_sales_tabletest <- On_Circuit_sales_table[, .N, by = .(Date,Product_Code,Number_lines > 500, Line_Quantity > 1000)]
head(On_Circuit_sales_tabletest)


############# Multiple columns in J and the .SD command (subset Data)
########
On_Circuit_sales_tabletest <- On_Circuit_sales_table[, .SD, by = .(Print_Branch,Product_Code)]
head(On_Circuit_sales_tabletest)

############.SD combined with lapply to do calcuation for all Columns
On_Circuit_sales_tabletest <- On_Circuit_sales_table[, lapply(.SD, mean), by = .(Print_Branch,Product_Code,Local_switch)]
head(On_Circuit_sales_tabletest)

############.SD combined with lapply to use .SDcols to choose which columns to run the calc
On_Circuit_sales_tabletest <- S_AFM_DT[, lapply(.SD, sum),
                                                     by = .(Print_Branch,Product_Code),
                                                     .SDcols = c("Number_lines","Line_Quantity")]
head(On_Circuit_sales_tabletest)


############.SD combined with lapply to use .Subset on .SD 
On_Circuit_sales_tabletest <- On_Circuit_sales_table[, head(.SD, sum),
                                                     by = .(Product_Code,print_branch),
                                                     .SDcols = c("Number_lines","Line_Quantity")]
head(On_Circuit_sales_tabletest)

############.???? de
On_Circuit_sales_tabletest <- On_Circuit_sales_table[, .(val = c(VEN0164W,LLO1799S)),
                                                     by = .(Product_Code)]
                                                     
head(On_Circuit_sales_tabletest)



###############example  
DT <- data.table(On_Circuit_sales_table)
# this will get you your mean and SD's for each column
DT[, sapply(.SD, function(x) list(mean=mean(x), sd=sd(x)))]
# adding a `by` argument will give you the groupings
DT[, sapply(.SD, function(x) list(mean=mean(x), sd=sd(x))), by=ID]
# If you would like to round the values: 
DT[, sapply(.SD, function(x) list(mean=round(mean(x), 3), sd=round(sd(x), 3))), by=ID]
# If we want to add names to the columns 
wide <- setnames(DT[, sapply(.SD, function(x) list(mean=round(mean(x), 3), sd=round(sd(x), 3))), by=ID], c("ID", sapply(names(DT)[-1], paste0, c(".men", ".SD"))))









