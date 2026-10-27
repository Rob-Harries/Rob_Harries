rm(list=ls())
gc()


setwd('C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/')

library(RODBC); library(data.table); library(lubridate); library(foreach); library(plyr); library(dplyr); library(matrixStats); library(stringr); library(hms); library(DBI); library(odbc)

##R package tstools has the ability to run ABC and XYZ analysis library(tstools)
##https://kourentzes.com/forecasting/2016/10/15/abc-xyz-analysis-for-forecasting/

connection_function = function() {
  pwd = "G00ners4everywW"  
  #readline("Enter password: ")
  con <<- odbcConnect("Azure Supply Chain", uid = "Harriesr", pwd = pwd)
}; connection_function();




Stocked_list = sqlQuery(con, paste("SELECT *",
                                   "FROM [SupplyChainReporting].[dbo].[stocked_products_view]",
                                   "where Branch_Number in ('101c' , '106m','201D' ,'204j' , '305m','405n', '406p' ,'501g' ,'502x' , '504m', '602j' ,'703m' , '706s', '908b')"))

setnames(Stocked_list, c("Product_Code" , "Branch_Number", "Is_Stocked"), c("Product_Code" , "Account_Branch" ,"stocked_mkr"))

DC_teirs = sqlQuery(con, paste("SELECT *",
                               "FROM SupplyChainReporting.dbo.Stock_Platform_DC_Twin_RDC"))



Stocked_list = as.data.table(Stocked_list)
DC_teirs = as.data.table(DC_teirs)

##DC_teirs <- DC_teirs[Account_Branch %in% .('106M','201D') , Twin_Branch_ID := '305M'
##                     ][Account_Branch %in% .('106M','201D') , Twin := 2]

Stocked_list <- left_join(Stocked_list,DC_teirs)
Stocked_list = as.data.table(Stocked_list)
Stocked_list_Tier1 <- Stocked_list[Account_Branch %in% .("106M","501G","703M","305M"),.(Product_Code, stocked_mkr, Account_Branch)]
Stocked_test1 <- left_join(Stocked_list,Stocked_list_Tier1, by = c("Twin_Branch_ID" = "Account_Branch", "Product_Code"))
colnames(Stocked_test1)[colnames(Stocked_test1)=="stocked_mkr.y"] <- "Stocked_mkr_Twin"
colnames(Stocked_test1)[colnames(Stocked_test1)=="stocked_mkr.x"] <- "Stocked_mkr_local"

Stocked_list_RDC <- Stocked_list[Account_Branch %in% .("305M","406P"),.(Product_Code, stocked_mkr, Account_Branch)]
Stocked_test2 <- left_join(Stocked_test1,Stocked_list_RDC, by = c("RDC_Branch_ID" = "Account_Branch", "Product_Code"))
colnames(Stocked_test2)[colnames(Stocked_test2)=="stocked_mkr"] <- "Stocked_mkr_RDC"

Stocked_list_RDC305M <- Stocked_list[Account_Branch %in% .("305M"),.(Product_Code, stocked_mkr, Account_Branch)]
Stocked_test2 = as.data.table(Stocked_test2)
Stocked_test2[RDC_Branch_ID == "406P", NewRDC := "305M"]
Stocked_test3 <- left_join(Stocked_test2,Stocked_list_RDC, by = c("NewRDC" = "Account_Branch",  "Product_Code"))
colnames(Stocked_test3)[colnames(Stocked_test3)=="stocked_mkr"] <- "Stock_305M"


Stocked_test3 = as.data.table(Stocked_test3)
Stocked_test3[Stock_305M == 1 , Pick_DC := NewRDC
][Stocked_mkr_RDC == 1 , Pick_DC := RDC_Branch_ID
][Stocked_mkr_Twin == 1 , Pick_DC := Twin_Branch_ID
][Stocked_mkr_local == 1 , Pick_DC := Account_Branch]

Stocked_test3$Pick_DC[is.na(Stocked_test3$Pick_DC)] <- "Not Stocked"
Stocked_List_final <- Stocked_test3[Product_Code !="", 
                                    .(Account_Branch, Product_Code,Pick_DC,Stocked_mkr_local, Stocked_mkr_Twin,Stocked_mkr_RDC,Stock_305M)] 

rm(Stocked_test1,Stocked_test2,Stocked_test3,Stocked_list,Stocked_list_RDC,Stocked_list_RDC305M,Stocked_list_Tier1)
gc()