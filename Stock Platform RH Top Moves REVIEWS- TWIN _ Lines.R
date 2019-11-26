########################Partial Review Stock Platform Code Review 
rm(list=ls())

#rm(Sales_ALL)
#### hello

gc()

setwd('C:/Users/harriesr/OneDrive - McKesson Europe/R Coding/')

library(RODBC); library(data.table); library(lubridate); library(foreach); library(dplyr); library(plyr); library(doParallel);library(tidyverse)


connection_function = function() {
  pwd = readline("Enter password: ")
  con <<- odbcConnect("SolariDB Supply Chain", uid = "harriesr", pwd = pwd)
}; connection_function(); 

sales_data = sqlQuery(con, paste("SELECT dsc.Account_Branch, Product_Code, SUM(True_Demand) as True_Demand, SUM((case True_Demand when 0 then 0 else Number_Lines end)) as Demand_Lines",
                                 "FROM [SupplyChainReporting].[dbo].[daily_sales_customer] dsc",
                                 "Left join SupplyChainReporting.dbo.customer_info ci on dsc.Account_Number = ci.Account_Number and dsc.Account_Branch = ci.Account_Branch",
                                 "WHERE dsc.Account_Branch in ('101c' , '106m','201D' ,'204j' , '305m', '405N', '406P', '402h' ,'501g' ,'502x' , '504m','602j' ,'703m' , '706s', '908b')",
                                 "and Print_Branch not in ('408t','606r','608v')",
                                 "and Date >= DATEADD(day,-90, getdate())",
                                 #"and Customer_Type_Number > 40",
                                 #"and Product_Code like 'TEA0062E'",
                                 "group by dsc.Account_Branch, Product_Code"))


sales_data_hosp = sqlQuery(con, paste("SELECT dsc.Account_Branch, Product_Code, SUM(True_Demand) as True_Demand, SUM((case True_Demand when 0 then 0 else Number_Lines end)) as Demand_Lines",
                                    "FROM [SupplyChainReporting].[dbo].[daily_sales_customer] dsc",
                                      "Left join SupplyChainReporting.dbo.customer_info ci on dsc.Account_Number = ci.Account_Number and dsc.Account_Branch = ci.Account_Branch",
                                      "WHERE dsc.Account_Branch in ('106m','201D' , '305m','402h', '405N', '406P','501g' ,'502x' , '504m','703m' ,'706s')",
                                      "and Print_Branch not in ('408t','606r','608v')",
                                      "and Date >= DATEADD(day,-90, getdate())",
                                      "and Customer_Type_Number in (15,10,18,20)",
                                      #"and Product_Code like 'TEA0062E'",
                                      "group by dsc.Account_Branch, Product_Code"))


sales_data_nonLLO = sqlQuery(con, paste("SELECT dsc.Account_Branch, Product_Code, SUM(True_Demand) as True_Demand, SUM((case True_Demand when 0 then 0 else Number_Lines end)) as Demand_Lines",
                                      "FROM [SupplyChainReporting].[dbo].[daily_sales_customer] dsc",
                                      "Left join SupplyChainReporting.dbo.customer_info ci on dsc.Account_Number = ci.Account_Number and dsc.Account_Branch = ci.Account_Branch",
                                      "WHERE dsc.Account_Branch in ('101c' , '106m','201D' ,'204j' , '305m','402h', '405N', '406P','501g' ,'502x' , '504m','602j' ,'703m' , '706s', '908b')",
                                      "and Print_Branch not in ('408t','606r','608v')",
                                      "and Date >= DATEADD(day,-90, getdate())",
                                      "and Customer_Type_Number not in (15,41,10,42,18,20)",
                                      #"and Product_Code like 'TEA0062E'",
                                      "group by dsc.Account_Branch, Product_Code"))


sales_data_LLO = sqlQuery(con, paste("SELECT dsc.Account_Branch, Product_Code, SUM(True_Demand) as True_Demand, SUM((case True_Demand when 0 then 0 else Number_Lines end)) as Demand_Lines",
                                        "FROM [SupplyChainReporting].[dbo].[daily_sales_customer] dsc",
                                        "Left join SupplyChainReporting.dbo.customer_info ci on dsc.Account_Number = ci.Account_Number and dsc.Account_Branch = ci.Account_Branch",
                                        "WHERE dsc.Account_Branch in ('101c' , '106m','201D' ,'204j' , '305m','402h', '405N', '406P','501g' ,'502x' , '504m','602j' ,'703m' , '706s', '908b')",
                                        "and Print_Branch not in ('408t','606r','608v')",
                                        "and Date >= DATEADD(day,-90, getdate())",
                                        "and Customer_Type_Number in (41,42)",
                                        #"and Product_Code like 'TEA0062E'",
                                        "group by dsc.Account_Branch, Product_Code"))



Main_Query = sqlQuery(con, paste("SELECT Date, Branch, Product_Code,Location_Code, First_Receipt_Date, Op_Shelf_Balance, Demand, Current_Shelf_Balance, Current_Order_Balance",
                                 "FROM [SupplyChainReporting].[dbo].[main_query] mq",
                                 "WHERE Branch in ('101c' , '106m' , '201d' , '204j' , '305m' , '402h' , '405n' , '406p' , '501g' , '502x', '504m' , '602j' , '703m' , '706s' , '908b')",
                                 "and Op_Shelf_Balance = 0",
                                 "and Date >= DATEADD(day,-1, getdate())"))




current_date<-Sys.Date()

Stocked_list_original <- sqlQuery(con, paste0("exec SupplyChainReporting.dbo.stocked_product_historic @date ='", current_date, "'"))

Stocked_list = Stocked_list_original

setnames(Stocked_list, c("Product_Code" , "Branch_Number", "Is_Stocked"), c("product_code" , "branch_id" ,"stocked_mkr"))

##Stocked_list <- read.delim("C:/Users/harriesr/OneDrive - McKesson Europe/R Coding/txt files/Knapp/Stocked_list.txt",header = TRUE, sep = "\t")
DC_teirs = read.csv("C:/Users/harriesr/OneDrive - McKesson Europe/R Coding/txt files/Knapp/BranchID_Twin_RDC.csv")
MasterProdData <- read.csv("C:/Users/harriesr/OneDrive - McKesson Europe/R Coding/txt files/Knapp/MasterProdData.csv")
parameters = read.csv("C:/Users/harriesr/OneDrive - McKesson Europe/R Coding/txt files/Stock Platform/param_test_table.csv")
Prod_Grp_adj = read.csv("C:/Users/harriesr/OneDrive - McKesson Europe/R Coding/txt files/Stock Platform/Prod_grp_Adj.csv")

###############Insertaion Date Rule - 6months#####################################################################################################



Stocked_list = as.data.table(Stocked_list)
DC_teirs = as.data.table(DC_teirs)
MasterProdData = as.data.table(MasterProdData)
parameters = as.data.table(parameters)
Prod_Grp_adj = as.data.table(Prod_Grp_adj)

colnames(Stocked_list)[colnames(Stocked_list)=="branch_id"] <- "account_branch"
####################################################################################################################
# 
# 
# ########### Primarty pick branch logic Done Remove############################################################################----


Stocked_list <- left_join(Stocked_list,DC_teirs)
Stocked_list = as.data.table(Stocked_list)
Stocked_list_Tier1 <- Stocked_list[account_branch %in% .("106M","501G","703M"),.(product_code, stocked_mkr, account_branch)]
Stocked_test1 <- left_join(Stocked_list,Stocked_list_Tier1, by = c("Twin_branch_id" = "account_branch", "product_code"))
colnames(Stocked_test1)[colnames(Stocked_test1)=="stocked_mkr.y"] <- "Stocked_mkr_Twin"
colnames(Stocked_test1)[colnames(Stocked_test1)=="stocked_mkr.x"] <- "Stocked_mkr_local"

Stocked_list_RDC <- Stocked_list[account_branch %in% .("305M","406P"),.(product_code, stocked_mkr, account_branch)]
Stocked_test2 <- left_join(Stocked_test1,Stocked_list_RDC, by = c("RDC_branch_id" = "account_branch", "product_code"))
colnames(Stocked_test2)[colnames(Stocked_test2)=="stocked_mkr"] <- "Stocked_mkr_RDC"

Stocked_list_RDC305M <- Stocked_list[account_branch %in% .("305M"),.(product_code, stocked_mkr, account_branch)]
Stocked_test2 = as.data.table(Stocked_test2)
Stocked_test2[RDC_branch_id == "406P", NewRDC := "305M"]
Stocked_test3 <- left_join(Stocked_test2,Stocked_list_RDC, by = c("NewRDC" = "account_branch",  "product_code"))
colnames(Stocked_test3)[colnames(Stocked_test3)=="stocked_mkr"] <- "Stock_305M"


Stocked_test3 = as.data.table(Stocked_test3)
Stocked_test3[Stock_305M == "Y" , Pick_DC := NewRDC
              ][Stocked_mkr_RDC == "Y" , Pick_DC := RDC_branch_id
                ][Stocked_mkr_Twin == "Y" , Pick_DC := Twin_branch_id
                  ][Stocked_mkr_local == "Y" , Pick_DC := account_branch]

Stocked_test3$Pick_DC[is.na(Stocked_test3$Pick_DC)] <- "Not Stocked"
Stocked_List_final <- Stocked_test3[product_code !="", 
                                    .(account_branch, product_code,Pick_DC,Stocked_mkr_local, Stocked_mkr_Twin,Stocked_mkr_RDC,Stock_305M)] 

rm(Stocked_test1,Stocked_test2,Stocked_test3,Stocked_list,Stocked_list_RDC,Stocked_list_RDC305M,Stocked_list_Tier1)
##################################################################################################################################

############ Change COl NAmes

setnames(sales_data, c("True_Demand" , "Demand_Lines"), c("True_Demand_all" , "Demand_Lines_all"))
setnames(sales_data_hosp, c("True_Demand" , "Demand_Lines"), c("True_Demand_hosp" , "Demand_Lines_hosp"))
setnames(sales_data_LLO, c("True_Demand" , "Demand_Lines"), c("True_Demand_LLO" , "Demand_Lines_LLO"))
setnames(sales_data_nonLLO, c("True_Demand" , "Demand_Lines"), c("True_Demand_NonLLO" , "Demand_Lines_NonLLO"))
setnames(Stocked_List_final, c("account_branch","product_code"), c("Account_Branch", "Product_Code"))
setnames(MasterProdData, c("product_code"), c("Product_Code"))
setnames(DC_teirs, c("account_branch"), c("Account_Branch"))


#################### Joins

Stocked_List_final <- Stocked_List_final[!(Account_Branch %in% .("606R","408T","608V","405N"))]

Stocked_List_final <- left_join(Stocked_List_final,MasterProdData)
Stocked_List_final <- left_join(Stocked_List_final,sales_data)
Stocked_List_final <- left_join(Stocked_List_final,sales_data_hosp)
Stocked_List_final <- left_join(Stocked_List_final,sales_data_LLO)
Stocked_List_final <- left_join(Stocked_List_final,sales_data_nonLLO)
Stocked_List_final <- left_join(Stocked_List_final,DC_teirs)
Stocked_List_final <- left_join(Stocked_List_final,Prod_Grp_adj)


################# Remove NA
Stocked_List_final = as.data.table(Stocked_List_final)
Stocked_List_final[is.na(True_Demand_all), "True_Demand_all" := 0
                   ][is.na(Demand_Lines_all), "Demand_Lines_all" := 0
                     ][is.na(True_Demand_hosp), "True_Demand_hosp" := 0
                       ][is.na(Demand_Lines_hosp), "Demand_Lines_hosp" := 0
                         ][is.na(True_Demand_LLO), "True_Demand_LLO" := 0
                           ][is.na(Demand_Lines_LLO), "Demand_Lines_LLO" := 0
                             ][is.na(True_Demand_NonLLO), "True_Demand_NonLLO" := 0
                               ][is.na(Demand_Lines_NonLLO), "Demand_Lines_NonLLO" := 0]

Stocked_List_final <- Stocked_List_final[!(is.na(description)),]

################ Ideintify Spec Handling, Adjust Umb 69 to 406p, remove AAA0001P#######################################

Stocked_List_final[location_type %in% .("C","R","M","N","F"), Spec_hand := 1]
Stocked_List_final[is.na(Spec_hand), "Spec_hand" := 0]
Stocked_List_final[umbrella_group == 69, Pick_DC :="406P"]
Stocked_List_final <- Stocked_List_final[Product_Code != "AAA0001P",]


################# Supplier Overides / TPOS identifier   ####################################################################################

Supplier_adj = read.csv("C:/Users/harriesr/OneDrive - McKesson Europe/R Coding/txt files/Stock Platform/Supplier_Adj.csv")
Supplier_adj = as.data.table(Supplier_adj)
setnames(Supplier_adj, c("Full_supplier"), c("supplier_code"))
Stocked_List_final <- left_join(Stocked_List_final,Supplier_adj)
Stocked_List_final <- as.data.table(Stocked_List_final)
Stocked_List_final[is.na(Supplier_adj), "Supplier_adj" := 0
                    ][is.na(TPOS), "TPOS" := 0]

##############################AM PM Split current #####################################################################

Stocked_List_final <- Stocked_List_final[,AM_Pick_DC := Account_Branch
                                   ][umbrella_group %in% .(32) & twin == 2 & Spec_hand == 0, AM_Pick_DC := Twin_branch_id
                                     ][umbrella_group %in% .(1,5,47),AM_Pick_DC := RDC_branch_id
                                       ][Spec_hand == 1, AM_Pick_DC := Account_Branch
                                         ][umbrella_group == 69, AM_Pick_DC := "406P"]

Stocked_List_final <- Stocked_List_final[,PM_Pick_DC := Account_Branch
            ][twin == 2 & Spec_hand == 0, PM_Pick_DC := Twin_branch_id
                ][umbrella_group %in% .(1,5,47),PM_Pick_DC := RDC_branch_id
                  ][Spec_hand == 1, PM_Pick_DC := Account_Branch
                    ][umbrella_group == 69, PM_Pick_DC := "406P"]

AM_PM_SPlit1 <- Stocked_List_final[,True_Demand_all_AM := True_Demand_all * 0.45
            ][,True_Demand_all_PM := True_Demand_all * 0.55
              ][, Demand_Lines_all_AM := Demand_Lines_all * 0.45
                ][, Demand_Lines_all_PM := Demand_Lines_all *0.55]

AM_PM_SPlit1[,True_Demand_hosp_AM := True_Demand_hosp * 0.45
            ][,True_Demand_hosp_PM := True_Demand_hosp * 0.55
              ][, Demand_Lines_hosp_AM := Demand_Lines_hosp * 0.45
                ][, Demand_Lines_hosp_PM := Demand_Lines_hosp *0.55]

AM_PM_SPlit1[,True_Demand_LLO_AM := True_Demand_LLO * 0.45
             ][,True_Demand_LLO_PM := True_Demand_LLO * 0.55
               ][,Demand_Lines_LLO_AM := Demand_Lines_LLO * 0.45
                 ][,Demand_Lines_LLO_PM := Demand_Lines_LLO * 0.55
                   ][location_type != "E" & umbrella_group %in% .(11,25) & twin == 2,True_Demand_LLO_AM := 0
                     ][location_type != "E" & umbrella_group %in% .(11,25) & twin == 2,True_Demand_LLO_PM := True_Demand_LLO
                       ][location_type != "E" & umbrella_group %in% .(11,25) & twin == 2,True_Demand_LLO_AM := 0
                         ][location_type != "E" & umbrella_group %in% .(11,25) & twin ==2,True_Demand_LLO_AM := Demand_Lines_LLO]

AM_PM_SPlit1[,True_Demand_NonLLO_AM := True_Demand_NonLLO * 0.45
            ][,True_Demand_NonLLO_PM := True_Demand_NonLLO* 0.55
              ][, Demand_Lines_NonLLO_AM := Demand_Lines_NonLLO * 0.45
                ][, Demand_Lines_NonLLO_PM := Demand_Lines_NonLLO *0.55]


AM_PM_SPlit1[,True_Demand_AM_Twin := True_Demand_hosp_AM + True_Demand_LLO_AM + True_Demand_NonLLO_AM
            ][,True_Demand_PM_Twin := True_Demand_hosp_PM + True_Demand_LLO_PM + True_Demand_NonLLO_PM
              ][, Demand_Lines_AM_Twin := Demand_Lines_hosp_AM + Demand_Lines_LLO_AM + Demand_Lines_NonLLO_AM
                ][,Demand_Lines_PM_Twin := Demand_Lines_hosp_PM + Demand_Lines_LLO_PM + Demand_Lines_NonLLO_PM]



ALL_Twin_Adj_AM1 <- AM_PM_SPlit1[,.(TrueDemand_Local_AM = sum(True_Demand_all_AM)
                                          ,DemandLines_Local_AM = sum(Demand_Lines_all_AM)
                                            ,True_Demand_AM_TwinAdj = sum(True_Demand_AM_Twin)
                                                ,DemandLines_AM_TwinAdj = sum(Demand_Lines_AM_Twin))
                                       , by =.(AM_Pick_DC,Product_Code)]

ALL_Twin_Adj_PM1 <- AM_PM_SPlit1[,.(TrueDemand_Local_PM = sum(True_Demand_all_PM)
                                   ,DemandLines_Local_PM = sum(Demand_Lines_all_PM)
                                    ,True_Demand_PM_TwinAdj = sum(True_Demand_PM_Twin)
                                   ,DemandLines_PM_TwinAdj = sum(Demand_Lines_PM_Twin))
                                       , by =.(PM_Pick_DC,Product_Code)]

setnames(ALL_Twin_Adj_AM1, c("AM_Pick_DC"), c("Account_Branch"))
setnames(ALL_Twin_Adj_PM1, c("PM_Pick_DC"), c("Account_Branch"))

Stocked_List_final <- left_join(Stocked_List_final,ALL_Twin_Adj_AM1)
Stocked_List_final <- left_join(Stocked_List_final,ALL_Twin_Adj_PM1)
Stocked_List_final <- as.data.table(Stocked_List_final)

################################ Remove NA #############################################################################################

Stocked_List_final[is.na(TrueDemand_Local_AM), "TrueDemand_Local_AM" := 0
                    ][is.na(DemandLines_Local_AM), "DemandLines_Local_AM" := 0
                      ][is.na(True_Demand_AM_TwinAdj), "True_Demand_AM_TwinAdj" := 0
                        ][is.na(DemandLines_AM_TwinAdj), "DemandLines_AM_TwinAdj" := 0
                          ][is.na(TrueDemand_Local_PM), "TrueDemand_Local_PM" := 0
                            ][is.na(DemandLines_Local_PM), "DemandLines_Local_PM" := 0
                              ][is.na(True_Demand_PM_TwinAdj), "True_Demand_PM_TwinAdj" := 0
                                ][is.na(DemandLines_PM_TwinAdj), "DemandLines_PM_TwinAdj" := 0]


Stocked_List_final <- Stocked_List_final[, c("True_Demand_hosp_AM","True_Demand_hosp_PM"
                                             ,"Demand_Lines_hosp_AM","Demand_Lines_hosp_PM"
                                             ,"True_Demand_LLO_AM","True_Demand_LLO_PM","Demand_Lines_LLO_AM","Demand_Lines_LLO_PM"
                                             ,"True_Demand_NonLLO_AM","True_Demand_NonLLO_PM","Demand_Lines_NonLLO_AM","Demand_Lines_NonLLO_PM"
                                             ,"True_Demand_AM_Twin","True_Demand_PM_Twin","Demand_Lines_AM_Twin","Demand_Lines_PM_Twin") := NULL]

Stocked_List_final <- Stocked_List_final[, c("True_Demand_all_AM","True_Demand_all_PM"
                                             ,"Demand_Lines_all_AM","Demand_Lines_all_PM") := NULL]

Stocked_List_final[,Total_DemandLines_twin := DemandLines_AM_TwinAdj + DemandLines_PM_TwinAdj
                   ][,Total_TrueDemand_twin := True_Demand_AM_TwinAdj + True_Demand_PM_TwinAdj]

##rm(Stocked_List_final2)
############### First ABC just Account Branch  ##############################################################################################

abc <- function(Stocked_List_final, parameters) {
  # Calculate the ABC segmentation
  
  df.abc <- Stocked_List_final[!is.na(Demand_Lines_all) & TPOS == 0,.(sum_qty=sum(Demand_Lines_all)),by="Product_Code"]
  df.abc <- df.abc[order(-sum_qty)]
  df.abc <- df.abc[, cumsum_qty := cumsum(sum_qty)]
  df.abc <- df.abc[, perc_cumsum_qty := round(cumsum_qty/sum(df.abc$sum_qty) * 100,2)]
  df.abc <- df.abc[, ABC_acc := ifelse(perc_cumsum_qty <= as.numeric(parameters[parameter=='a', param_value]), 1,
                                       ifelse(perc_cumsum_qty <= as.numeric(parameters[parameter=='b', param_value]), 2, 3))]
  
  # per_A_skus <- round(length(df.abc[segment_qty == 1, segment_qty])/length(df.abc[, segment_qty]) * 100, 2)
  #  per_B_skus <- round(length(df.abc[segment_qty ==  2, segment_qty])/length(df.abc[, segment_qty]) * 100, 2)
  # per_C_skus <- round(length(df.abc[segment_qty ==  3, segment_qty])/length(df.abc[, segment_qty]) * 100, 2)
  #  abc_resume <- data.table('segment' = c(1, 2, 3), "perc_product_codes" = c(per_A_skus, per_B_skus, per_C_skus))
  
  return(df.abc)
}
abc_global = abc(Stocked_List_final,parameters)

## split by branch

abc_branch_fn <- function(Stocked_List_final, parameters) {
  SegmentsABC.branch <- foreach (i=unique(Stocked_List_final$Account_Branch), .combine = rbind) %do% {
    ts.branch <- Stocked_List_final[Account_Branch==i,]
    abc.data.branch <- abc(ts.branch, parameters)
    abc.data.branch[, Account_Branch:=i]}
}
abc_branch = abc_branch_fn(Stocked_List_final,parameters)



################ ABC on HOSP##############################################################################

abc_h <- function(Stocked_List_final, parameters) {
  # Calculate the ABC segmentation
  
  df.abc <- Stocked_List_final[!is.na(Demand_Lines_hosp)& TPOS == 0,.(sum_qty=sum(Demand_Lines_hosp)),by="Product_Code"]
  df.abc <- df.abc[order(-sum_qty)]
  df.abc <- df.abc[, cumsum_qty := cumsum(sum_qty)]
  df.abc <- df.abc[, perc_cumsum_qty := round(cumsum_qty/sum(df.abc$sum_qty) * 100,2)]
  df.abc <- df.abc[, ABC_Hosp := ifelse(perc_cumsum_qty <= as.numeric(parameters[parameter=='a', Hosp_val]), 1,
                                       ifelse(perc_cumsum_qty <= as.numeric(parameters[parameter=='b', Hosp_val]), 2, 3))]

  return(df.abc)
}
abc_global_hosp = abc_h(Stocked_List_final,parameters)

## split by branch

abc_branch_fn_h <- function(Stocked_List_final, parameters) {
  SegmentsABC.branch <- foreach (i=unique(Stocked_List_final$Account_Branch), .combine = rbind) %do% {
    ts.branch <- Stocked_List_final[Account_Branch==i,]
    abc.data.branch <- abc_h(ts.branch, parameters)
    abc.data.branch[, Account_Branch:=i]}
}
abc_branch_hosp = abc_branch_fn_h(Stocked_List_final,parameters)


################ ABC on Non LLO##############################################################################

abc_nonLLO <- function(Stocked_List_final, parameters) {
  # Calculate the ABC segmentation
  
  df.abc <- Stocked_List_final[!is.na(Demand_Lines_NonLLO)& TPOS == 0,.(sum_qty=sum(Demand_Lines_NonLLO)),by="Product_Code"]
  df.abc <- df.abc[order(-sum_qty)]
  df.abc <- df.abc[, cumsum_qty := cumsum(sum_qty)]
  df.abc <- df.abc[, perc_cumsum_qty := round(cumsum_qty/sum(df.abc$sum_qty) * 100,2)]
  df.abc <- df.abc[, ABC_NonLLO := ifelse(perc_cumsum_qty <= as.numeric(parameters[parameter=='a', param_value]), 1,
                                        ifelse(perc_cumsum_qty <= as.numeric(parameters[parameter=='b', param_value]), 2, 3))]
  
  return(df.abc)
}
abc_global_NonLLO = abc(Stocked_List_final,parameters)

## split by branch

abc_branch_fn_NL <- function(Stocked_List_final, parameters) {
  SegmentsABC.branch <- foreach (i=unique(Stocked_List_final$Account_Branch), .combine = rbind) %do% {
    ts.branch <- Stocked_List_final[Account_Branch==i,]
    abc.data.branch <- abc_nonLLO(ts.branch, parameters)
    abc.data.branch[, Account_Branch:=i]}
}
abc_branch_NonLLO = abc_branch_fn_NL(Stocked_List_final,parameters)


#######################################################################################################################################
abc_twin1 <- function(Stocked_List_final, parameters) {
  # Calculate the ABC segmentation
  
  df.abc <- Stocked_List_final[!is.na(Total_DemandLines_twin) & TPOS == 0,.(sum_qty=sum(Total_DemandLines_twin)),by="Product_Code"]
  df.abc <- df.abc[order(-sum_qty)]
  df.abc <- df.abc[, cumsum_qty := cumsum(sum_qty)]
  df.abc <- df.abc[, perc_cumsum_qty := round(cumsum_qty/sum(df.abc$sum_qty) * 100,2)]
  df.abc <- df.abc[, ABC_Twin := ifelse(perc_cumsum_qty <= as.numeric(parameters[parameter=='a', param_value]), 1,
                                        ifelse(perc_cumsum_qty <= as.numeric(parameters[parameter=='b', param_value]), 2, 3))]
  
  return(df.abc)
}
abc_global_Twin = abc_twin1(Stocked_List_final,parameters)

## split by branch

abc_branch_fn_Twin <- function(Stocked_List_final, parameters) {
  SegmentsABC.branch <- foreach (i=unique(Stocked_List_final$Account_Branch), .combine = rbind) %do% {
    ts.branch <- Stocked_List_final[Account_Branch==i,]
    abc.data.branch <- abc_twin1(ts.branch, parameters)
    abc.data.branch[, Account_Branch:=i]}
}
abc_branch_Twin = abc_branch_fn_Twin(Stocked_List_final,parameters)




############################## Join back to Main Table

setnames(abc_branch, c("sum_qty"), c("ALL_TD"))
abc_branch_J <- abc_branch[,.(Product_Code,Account_Branch,ALL_TD,ABC_acc)]
#setnames(abc_branch_J, c("Adj_Pick_Branch"), c("Account_Branch"))

setnames(abc_branch_hosp, c("sum_qty"), c("Hosp_TD"))
abc_branch_hosp_J <- abc_branch_hosp[,.(Product_Code,Account_Branch,Hosp_TD,ABC_Hosp)]
#setnames(abc_branch_hosp_J, c("Adj_Pick_Branch"), c("Account_Branch"))

setnames(abc_branch_NonLLO, c("sum_qty"), c("NonLLO_TD"))
abc_branch_NonLLO_J <- abc_branch_NonLLO[,.(Product_Code,Account_Branch,NonLLO_TD,ABC_NonLLO)]
#setnames(abc_branch_hosp_J, c("Adj_Pick_Branch"), c("Account_Branch"))

setnames(abc_branch_Twin, c("sum_qty"), c("Twin_TD"))
abc_branch_Twin_J <- abc_branch_Twin[,.(Product_Code,Account_Branch,Twin_TD,ABC_Twin)]
#setnames(abc_branch_hosp_J, c("Adj_Pick_Branch"), c("Account_Branch"))


#########################
######################### do an ABC on Twinned Volumes  ########################################
#########################


#####rm(Stocked_List_final1) rest
Stocked_List_final1 <- left_join(Stocked_List_final,abc_branch)
Stocked_List_final1 <- as.data.table(Stocked_List_final1)
Stocked_List_final1[is.na(ALL_TD), "ALL_TD" := 0
                    ][is.na(ABC_acc), "ABC_acc" := 3]

Stocked_List_final1 <- left_join(Stocked_List_final1,abc_branch_hosp_J)
Stocked_List_final1 <- as.data.table(Stocked_List_final1)
Stocked_List_final1[is.na(Hosp_TD), "Hosp_TD" := 0
                    ][is.na(ABC_Hosp), "ABC_Hosp" := 3]

Stocked_List_final1 <- left_join(Stocked_List_final1,abc_branch_NonLLO_J)
Stocked_List_final1 <- as.data.table(Stocked_List_final1)
Stocked_List_final1[is.na(NonLLO_TD), "NonLLO_TD" := 0
                    ][is.na(ABC_NonLLO), "ABC_NonLLO" := 3]

Stocked_List_final1 <- left_join(Stocked_List_final1,abc_branch_Twin_J)
Stocked_List_final1 <- as.data.table(Stocked_List_final1)
Stocked_List_final1[is.na(Twin_TD), "Twin_TD" := 0
                    ][is.na(ABC_Twin), "ABC_Twin" := 3]

Stocked_List_final1 <- Stocked_List_final1[Product_Code != "AAA0001P"]

#rm(Stocked_List_final1)
################################## Adj ABC##########################################################################

Stocked_List_final1[,ABC_adj := min(ABC_acc,ABC_Hosp,ABC_NonLLO,ABC_Twin), by = .(Product_Code,Account_Branch)
                    ][, Sum_TD := sum(ALL_TD) , by =.(Product_Code)]


Stocked_List_final1[,ABC_adj := ABC_adj+ Supplier_adj + T1_Adj
                    ][twin == 2 & Spec_hand == 0, ABC_adj := ABC_adj + T2_Adj + Supplier_adj
                        ][umbrella_group %in% .(32,39) & twin == 2 & Spec_hand == 0, ABC_adj := 3
                          ][umbrella_group %in% .(39) & twin == 1 & Spec_hand == 0, ABC_adj := 2.5
                              ][umbrella_group %in% .(1,5,47) & twin >= 1 & Spec_hand == 0, ABC_adj := 3
                                ][location_type == "AA" & Spec_hand == 0, ABC_adj := 3]


################################ Final Decisions##################### Limited by Sales#############################


Stocked_List_final1<- as.data.table(Stocked_List_final1)
Stocked_List_final1[,Decision := "Remain",
                      ][ABC_adj <= 2.5 & Stocked_mkr_local =="Y" & Spec_hand == 0,  Decision :=  "Remain"
                          ][ABC_adj > 2.5 & Stocked_mkr_local =="N" & Spec_hand == 0,  Decision :=  "Remain" 
                            ][ABC_adj <= 2 & Stocked_mkr_local =="N"& Spec_hand == 0 & twin == 1 & Total_DemandLines_twin >= 80 & report_group <= 5 , Decision :=  "IN"
                              ][ABC_adj <= 2 & Stocked_mkr_local =="N"& Spec_hand == 0 & twin == 2 & Total_DemandLines_twin >= 30 & report_group <= 5 ,Decision :=  "IN"
                                ][ABC_adj <= 2 & Stocked_mkr_local =="N"& Spec_hand == 0 & twin == 1 & Total_DemandLines_twin >= 250 & report_group > 5,Decision :=  "IN"
                                  ][ABC_adj <= 2 & Stocked_mkr_local =="N"& Spec_hand == 0 & twin == 2 & Total_DemandLines_twin >= 340 & report_group > 5 ,Decision :=  "IN"
                                    ][ABC_adj > 2.5 & Stocked_mkr_local == "Y" & Spec_hand == 0 & Total_DemandLines_twin <= 5 , Decision :=  "OUT"
                                      ][ABC_adj > 2.5 & Stocked_mkr_local == "Y" & Spec_hand == 0 & Account_Branch %in% c("406P","305M"), Decision :=  "Remain"
                                        ][Sum_TD == 0 & Stocked_mkr_local == "Y" & Spec_hand == 0 & Account_Branch %in% c("406P","305M"), Decision :=  "OUT"
                                          ][Sum_TD == 0 & Pick_DC == "Not Stocked", Decision :=  "*DELETE"
                                            ][Spec_hand == 1 & ALL_TD > 0 & Stocked_mkr_local =="N" & ALL_TD >= 15 ,Decision :=  "IN_SH"
                                              ][Spec_hand == 1 & ALL_TD <= 0 & Stocked_mkr_local =="Y",Decision :=  "OUT_SH"
                                                ][Spec_hand == 1 & ALL_TD > 0 & Stocked_mkr_local =="Y",Decision :=  "Remain"
                                                  ][Discont == 1 & Pick_DC == "Not Stocked" , Decision := "*DELETE"
                                                      ][TPOS == 1 & Pick_DC == "Not Stocked" ,Decision := "*DELETE"
                                                        ][New.Line == 1,  Decision := "New SKU"
                                                          ][TPOS == 1 & Pick_DC != "Not Stocked", Decision := "Remain"
                                                            ][umbrella_group == 69, Decision := "Remain"
                                                              ][product_group == 81 , Decision := "Remain"
                                                                ][Stocked_mkr_local =="N" & umbrella_group == 11, Decision :=  "Remain"]

Stocked_List_final2 <- Stocked_List_final1[Decision != "*DELETE",]

Stocked_List_final2 <- Stocked_List_final2[, c("Central.Fill","cumsum_qty", "perc_cumsum_qty") := NULL]

################## Calcualte New Primary Pick Branch AM and PM#############################################################################################
######New Stocked List

New_Stocked_List <- Stocked_List_final2[,.(Account_Branch,Pick_DC,Product_Code,Decision,twin,Twin_branch_id,RDC_branch_id)]

New_Stocked_List[,New_Pick_DC := "CHECK"
                 ][Decision == "Remain", New_Pick_DC := Pick_DC
                    ][Decision=="IN", New_Pick_DC := Account_Branch
                      ][Decision == "OUT" & twin < 2,New_Pick_DC := RDC_branch_id
                        ][Decision == "New SKU", New_Pick_DC := Pick_DC
                          ][Decision =="*Discon",  New_Pick_DC := Pick_DC
                            ][Decision == "OUT" & twin == 2,New_Pick_DC := Twin_branch_id
                              ][Decision =="IN_SH" , New_Pick_DC := Account_Branch
                                ][Decision =="OUT_SH" , New_Pick_DC := "Not Stocked"
                                  ][twin == 0, New_Pick_DC := Pick_DC]

New_Stocked_List[,New_Stocked := ifelse(Account_Branch == New_Pick_DC,"Y","N")]

######################################################################################################
#################### Stocked @ Twin
New_Stocked_List_twin <- New_Stocked_List[Account_Branch %in% .("106M","501G","703M"),.(Product_Code, New_Stocked, Account_Branch)]
New_Stocked_List1 <- left_join(New_Stocked_List,New_Stocked_List_twin, by = c("Twin_branch_id" = "Account_Branch", "Product_Code"))
colnames(New_Stocked_List1)[colnames(New_Stocked_List1)=="New_Stocked.y"] <- "New_Stocked_mkr_Twin1"
colnames(New_Stocked_List1)[colnames(New_Stocked_List1)=="New_Stocked.x"] <- "New_Stocked_mkr_local"

################### Stocked @ RDC?

New_Stocked_list_RDC <- New_Stocked_List[Account_Branch %in% .("305M","406P"),.(Product_Code, New_Stocked, Account_Branch)]
New_Stocked_List2 <- left_join(New_Stocked_List1,New_Stocked_list_RDC, by = c("RDC_branch_id" = "Account_Branch", "Product_Code"))
colnames(New_Stocked_List2)[colnames(New_Stocked_List2)=="New_Stocked"] <- "New_Stocked_mkr_RDC1"



######################## Stocked @ 305m?
Stocked_list_RDC305M <- New_Stocked_List[Account_Branch %in% .("305M"),.(Product_Code, New_Stocked, Account_Branch)]
New_Stocked_List2 = as.data.table(New_Stocked_List2)
New_Stocked_List2[RDC_branch_id == "406P", NewRDC := "305M"]
New_Stocked_List3 <- left_join(New_Stocked_List2,New_Stocked_list_RDC, by = c("NewRDC" = "Account_Branch",  "Product_Code"))
colnames(New_Stocked_List3)[colnames(New_Stocked_List3)=="New_Stocked"] <- "New_Stock_305M"

####################### Combine Back to Main Table
New_Stocked_List3 = as.data.table(New_Stocked_List3)
New_Stocked_List3[New_Stock_305M == "Y" , New_Pick_DC_a := NewRDC
                  ][New_Stocked_mkr_RDC1 == "Y" , New_Pick_DC_a := RDC_branch_id
                    ][New_Stocked_mkr_Twin1 == "Y" , New_Pick_DC_a := Twin_branch_id
                      ][New_Stocked_mkr_local == "Y" , New_Pick_DC_a := Account_Branch
                        ][is.na(New_Pick_DC_a),New_Pick_DC_a := Account_Branch]

################# Remove NA
##New_Stocked_List3$New_Pick_DC_a[is.na(New_Stocked_List3$New_Pick_DC_a)] <- New_Stocked_List3$Pick_DC

New_Stocked_List_final <- New_Stocked_List3[Product_Code !="", 
                                    .(Account_Branch, Product_Code,New_Pick_DC_a,New_Stocked_mkr_RDC1,New_Stocked_mkr_Twin1)]

################ Remove OLD Tables 
rm(New_Stocked_List,New_Stocked_List1,New_Stocked_list_RDC,New_Stocked_List_twin,New_Stocked_List2,New_Stocked_List3,Stocked_list_RDC305M)


################ Create a New Table
Stocked_List_final3 <- left_join(Stocked_List_final2,New_Stocked_List_final)
Stocked_List_final3 <- as.data.table(Stocked_List_final3)
#Stocked_List_final3 <- Stocked_List_final3[,c("Stocked_mkr_local","Stocked_mkr_Twin","Stocked_mkr_RDC","Stock_305M") := NULL]

###################################################################################################################################################
################ create a New Pick Branch then do the AM/PM Split
###################################################################################################################################################

####This is the NEw State AM/PM Split
#############need on and off cirucit check############################

############## need to add LLO rules for PPP and others###############   umb 69 == 406p

AM_PM_SPlit <- Stocked_List_final3[, New_AM_Pick_DC := New_Pick_DC_a
                   ][umbrella_group %in% .(32) & twin == 2 & Spec_hand == 0, New_AM_Pick_DC := Twin_branch_id
                     ][umbrella_group %in% .(1,5,47),New_AM_Pick_DC := RDC_branch_id
                       ][Spec_hand == 1, New_AM_Pick_DC := Account_Branch
                           ][umbrella_group == 69, New_AM_Pick_DC := "406P"]

AM_PM_SPlit[,New_PM_Pick_DC := New_Pick_DC_a
                   ][twin == 2 & Spec_hand == 0 & Account_Branch == New_Pick_DC_a & New_Stocked_mkr_RDC1 == "Y", New_PM_Pick_DC := RDC_branch_id
                     ][twin == 2 & Spec_hand == 0 & Account_Branch == New_Pick_DC_a & New_Stocked_mkr_Twin1 == "Y", New_PM_Pick_DC := Twin_branch_id
                       ][umbrella_group %in% .(1,5,47),New_PM_Pick_DC := RDC_branch_id
                          ][Spec_hand == 1, New_PM_Pick_DC := Account_Branch
                            ][umbrella_group == 69, New_PM_Pick_DC := "406P"]

AM_PM_SPlit[,True_Demand_all_AM := True_Demand_all * 0.45
                   ][,True_Demand_all_PM := True_Demand_all * 0.55
                     ][, Demand_Lines_all_AM := Demand_Lines_all * 0.45
                       ][, Demand_Lines_all_PM := Demand_Lines_all *0.55]

AM_PM_SPlit[,True_Demand_hosp_AM := True_Demand_hosp * 0.45
                   ][,True_Demand_hosp_PM := True_Demand_hosp * 0.55
                     ][, Demand_Lines_hosp_AM := Demand_Lines_hosp * 0.45
                       ][, Demand_Lines_hosp_PM := Demand_Lines_hosp *0.55]

AM_PM_SPlit[,True_Demand_LLO_AM := True_Demand_LLO * 0.45
            ][,True_Demand_LLO_PM := True_Demand_LLO* 0.55
              ][, Demand_Lines_LLO_AM := Demand_Lines_LLO * 0.45
                ][, Demand_Lines_LLO_PM := Demand_Lines_LLO *0.55]

AM_PM_SPlit[,True_Demand_NonLLO_AM := True_Demand_NonLLO * 0.45
                   ][,True_Demand_NonLLO_PM := True_Demand_NonLLO* 0.55
                     ][, Demand_Lines_NonLLO_AM := Demand_Lines_NonLLO * 0.45
                       ][, Demand_Lines_NonLLO_PM := Demand_Lines_NonLLO *0.55]




################   Join Back to Main Table

ALL_Twin_Adj_AM <- Stocked_List_final3[,.(Adj_TrueDemand_ALL_AM = sum(True_Demand_all_AM)
                                         ,Adj_DemandLines_ALL_AM = sum(Demand_Lines_all_AM))
                                      , by =.(New_AM_Pick_DC,Product_Code)]

ALL_Twin_Adj_PM <- Stocked_List_final3[,.(Adj_TrueDemand_ALL_PM = sum(True_Demand_all_PM)
                                         ,Adj_DemandLines_ALL_PM = sum(Demand_Lines_all_PM))
                                      , by =.(New_PM_Pick_DC,Product_Code)]

setnames(ALL_Twin_Adj_AM, c("New_AM_Pick_DC"), c("Account_Branch"))
setnames(ALL_Twin_Adj_PM, c("New_PM_Pick_DC"), c("Account_Branch"))

Stocked_List_final4 <- left_join(Stocked_List_final3,ALL_Twin_Adj_AM)
Stocked_List_final4 <- left_join(Stocked_List_final4,ALL_Twin_Adj_PM)
Stocked_List_final4 <- as.data.table(Stocked_List_final4)

################################ Remove NA

Stocked_List_final4[is.na(Adj_TrueDemand_ALL_AM), "Adj_TrueDemand_ALL_AM" := 0
             ][is.na(Adj_TrueDemand_ALL_PM), "Adj_TrueDemand_ALL_PM" := 0
               ][is.na(Adj_DemandLines_ALL_AM), "Adj_DemandLines_ALL_AM" := 0
                 ][is.na(Adj_DemandLines_ALL_PM), "Adj_DemandLines_ALL_PM" := 0]

Stocked_List_final4[,Twin_Adj_TrueDemand_final := Adj_TrueDemand_ALL_AM +Adj_TrueDemand_ALL_PM
                    ][,Twin_Adj_DemandLines_final := Adj_DemandLines_ALL_AM + Adj_DemandLines_ALL_PM]


write.csv(Stocked_List_final4, file = "C:/Users/harriesr/OneDrive - McKesson Europe/R Coding/txt files/Stock PLatform/OutPutFile1.csv",
          row.names = TRUE)


##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################


rm(abc_branch,abc_branch_hosp,abc_branch_hosp_J,abc_branch_J,abc_branch_NonLLO
   ,abc_branch_NonLLO_J,abc_branch_Twin,abc_branch_Twin_J,abc_global,abc_global_hosp
   ,abc_global_NonLLO,abc_global_Twin,ALL_Twin_Adj_AM,ALL_Twin_Adj_AM1,ALL_Twin_Adj_PM,ALL_Twin_Adj_PM1,AM_PM_SPlit,AM_PM_SPlit1
   ,New_Stocked_List_final,Stocked_List_final,Stocked_List_final1,Stocked_List_final2,Stocked_List_final4,Stocked_List_final3)

Filter <- Stocked_List_final4[c(Product_Code == "PRE0606M"),]

############################## Inseration Date Rule 

##################### Min order Qty Rule
##################### 


write.csv(abc_global, file = "C:/Users/harriesr/OneDrive - McKesson Europe/R Coding/txt files/Stock PLatform/abc_global.csv",
          row.names = TRUE)


