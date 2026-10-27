rm(list=ls())
gc()


setwd('C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/')

library(RODBC); library(data.table); library(lubridate); library(foreach); library(plyr); library(dplyr); library(matrixStats); library(stringr); library(keyring)

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



Sales_Import = sqlQuery(con, paste("SELECT  bi.Print_Branch,
		bi.Process_Month,
		convert(DATE,bi.Processing_Date) as Processing_Date,
		convert(Time,bi.Processing_Date) as Processing_Time,
		DATENAME(Weekday,convert(DATE,bi.Processing_Date)) as Processing_Day,
		Order_Date_Time,
		Location_ID,
		bi.Pick_Note_No,
		C.Account_Branch as Charge_DC,
		c.Account_Number as Charge_Account, 
		c.Account_Name as Charge_Acc_Name,
		v.Van_No As Van_Number,
		v.Van_Sort_Group,
		v.Is_Out_Based,
		Order_Type_Zone,
		Order_Type_Detail,
		Product_Code,
		COUNT(bi.Line_Quantity) as Lines,
		SUM(bi.Line_Quantity) as Line_Qty,
		SUM(bi.Adj_Cost_Of_Sales_Value) as Sales_Value,
	CASE when C.Account_Number like '0000%' then 'IBT' ELSE bi.Company_ID end as Comp_ID,
	CASE when p.Product_Code in ('MOU0060M','MOU0061X','MOU0062E','MOU0063A','MOU0064T','MOU0065P') and bi.Print_Branch = '408T' then 1 else 0 END as Mounjaro,
	
	CASE	WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Branch
				WHEN C1.Account_Branch is NUll then C.Account_Branch
				ELSE C1.Account_Branch END as Delivery_DC,
	
	CASE	WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Number
				WHEN C1.Account_Branch is NUll then C.Account_Number
				ELSE C1.Account_Number END as Delivery_Account,
	
	CASE	WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Name
				WHEN C1.Account_Branch is NUll then C.Account_Name
				ELSE C1.Account_Name END as Delivery_Name,
	
	CASE	WHEN (CASE	WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Branch				
					WHEN C1.Account_Branch is NUll then C.Account_Branch
					ELSE C1.Account_Branch END) != '406P' and bi.Print_Branch = '406P' THEN 'Next Day Slow_Platform'
				WHEN (CASE	WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Branch				
					WHEN C1.Account_Branch is NUll then C.Account_Branch
					ELSE C1.Account_Branch END) != '305M' and bi.Print_Branch = '305M' THEN 'Next Day Slow_Platform'
				WHEN (CASE	WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Branch				
					WHEN C1.Account_Branch is NUll then C.Account_Branch
					ELSE C1.Account_Branch END) != '408T' and bi.Print_Branch = '408T' THEN 'Next Day Slow_Platform'
				WHEN  bi.Company_ID = 'T' THEN 'Next Day AM'		
--				WHEN  bi.Company_ID = 'E' THEN 'Next Day PM'
--				WHEN C.Account_Branch != bi.Print_Branch THEN 'Next Day Twin'
				WHEN V.Van_Depart_Time <= v.Van_Close_Time THEN 'Next Day AM'
				ELSE 'Same Day PM' END as Delivery_Type,

	CASE WHEN Warehouse_Zone = 'Spec Hand' and p.location_type in ('R','N') THEN 'Fridge'
			WHEN Warehouse_Zone = 'Spec Hand' and p.location_type in ('M') THEN 'Cyto'
			WHEN Warehouse_Zone = 'Spec Hand' and p.location_type in ('F') THEN 'Flamm'
			WHEN Warehouse_Zone = 'Spec Hand' and p.location_type in ('C') THEN 'CD'
			WHEN Order_Type_Detail = 'Autobulk' THEN 'Auto bulk'
			WHEN Warehouse_Zone is Null THEN 'Bulk'
		ELSE Warehouse_Area END as Warehouse_Zone


	FROM [SupplyChainReporting].[dbo].[b2b_invoices] bi

  left join SupplyChainReporting.dbo.b2b_customers c on bi.Cust_Key_C = c.Cust_Key_C
  Left Join SupplyChainReporting.dbo.b2b_invoices_delivery_accounts da on bi.Cust_Key_C = da.Cust_Key_C
		and bi.Process_Month = da.Process_Month
		and bi.Invoice_Number = da.Invoice_No
  left join SupplyChainReporting.dbo.b2b_customers c1 on da.Del_Key_C = c1.Cust_Key_C
  left Join SupplyChainReporting.dbo.b2b_customers c2 on c.Link_Key_C = c2.Cust_Key_C
  
  left Join SupplyChainReporting.dbo.customer_vans cv on (CASE	WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Cust_Key_C
				WHEN C1.Account_Branch is NUll then C.Cust_Key_C
				ELSE C1.Cust_Key_C END) = cv.Cust_Key_C 
  and datepart(WEEKDAY,(BI.Van_Day))-1 = convert(int,cv.Day_No) 
  and bi.Van_Number = cv.Van_No AND cV.Destination_Branch IN ('504m','908b')
  

    
  Left join SupplyChainReporting.dbo.product_list p on bi.Prod_Key_C = p.Prod_Key_C
  left join SupplyChainReporting.dbo.product_list_extra pe on bi.Prod_Key_C = pe.Prod_Key_C 

  left join SupplyChainReporting.dbo.van_history v on (CASE	WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Branch
				WHEN C1.Account_Branch is NUll then C.Account_Branch
				ELSE C1.Account_Branch END) = v.Print_Branch and bi.Van_Number = v.Van_No and v.date = convert(DATE,bi.Processing_Date) --DATEADD(day,-2, getdate())


  left join AAHOperations.dbo.lu_order_type_marker_details ot on bi.Order_Type_Marker = ot.Order_Type_Marker and bi.Print_Branch =ot.Print_Branch

  left Join AAHOperations.dbo.LU_Warehouse_Zones wz on  left(bi.Location_ID,1) = Station and bi.Print_Branch = wz.Branch

  Left Join SupplyChainReporting.dbo.customer_type_description ctd on C.Customer_Type_ID = ctd.Customer_Type_Number 


  WHERE bi.Process_Month >= DATEADD(MONTH,-2, getdate())
					and bi.Document_Type = 'I'
					and bi.Omit_Marker ='0'
					and Product_Code   Not in ('CUS0927W','LLO3350Q','AAH0081P','EME0088T','DEL0432N','TRI0866V','LOW0008F','LOW0009B', 'FIN0507E', 'FUE0001X') 
					and Umbrella_Group  Not in ('6','8','12','13','14','15','16','17','18','19','22','23') 
					and Full_Supplier NOT IN ('DEP/009M','VES/005E')
				and bi.Print_Branch in ('106m','201D' ,'305m','405n','406p' ,'501g' ,'504m', '602j','703m' ,'706s','908b','408t')
				and p.Full_Supplier not in ('NOR/011R','ALL/241Y', 'FOR/039T')
					and convert(date,bi.Processing_Date) >= DATEADD(day,-42, getdate())
GROUP BY  bi.Print_Branch,
		bi.Process_Month,
		convert(DATE,bi.Processing_Date),
		DATENAME(Weekday,convert(DATE,bi.Processing_Date)),
		convert(Time,bi.Processing_Date),
		Order_Date_Time,
		bi.Location_ID,
		bi.Pick_Note_No,
		C.Account_Branch,
		c.Account_Number, 
		c.Account_Name,
		v.Van_No,
		v.Van_Sort_Group,
		v.Is_Out_Based,
		Order_Type_Zone,
		Order_Type_Detail,
		Product_Code,
(CASE when p.Product_Code in ('MOU0060M','MOU0061X','MOU0062E','MOU0063A','MOU0064T','MOU0065P') and bi.Print_Branch = '408T' then 1 else 0 END),
		(CASE when C.Account_Number like '0000%' then 'IBT' ELSE bi.Company_ID end),
		(CASE	WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Branch
				WHEN C1.Account_Branch is NUll then C.Account_Branch 
				ELSE C1.Account_Branch END),
		(CASE	WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Number
				WHEN C1.Account_Branch is NUll then C.Account_Number
				ELSE C1.Account_Number END),
		(CASE	WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Name
				WHEN C1.Account_Branch is NUll then C.Account_Name
				ELSE C1.Account_Name END),
		(CASE	WHEN (CASE	WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Branch				
					WHEN C1.Account_Branch is NUll then C.Account_Branch
					ELSE C1.Account_Branch END) != '406P' and bi.Print_Branch = '406P' THEN 'Next Day Slow_Platform'
				WHEN (CASE	WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Branch				
					WHEN C1.Account_Branch is NUll then C.Account_Branch
					ELSE C1.Account_Branch END) != '305M' and bi.Print_Branch = '305M' THEN 'Next Day Slow_Platform'
				WHEN (CASE	WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Branch				
					WHEN C1.Account_Branch is NUll then C.Account_Branch
					ELSE C1.Account_Branch END) != '408T' and bi.Print_Branch = '408T' THEN 'Next Day Slow_Platform'
				WHEN  bi.Company_ID = 'T' THEN 'Next Day AM'		
--				WHEN  bi.Company_ID = 'E' THEN 'Next Day PM'
--				WHEN C.Account_Branch != bi.Print_Branch THEN 'Next Day Twin'
				WHEN V.Van_Depart_Time <= v.Van_Close_Time THEN 'Next Day AM'
				ELSE 'Same Day PM' END),
		(CASE WHEN Warehouse_Zone = 'Spec Hand' and p.location_type in ('R','N') THEN 'Fridge'
			WHEN Warehouse_Zone = 'Spec Hand' and p.location_type in ('M') THEN 'Cyto'
			WHEN Warehouse_Zone = 'Spec Hand' and p.location_type in ('F') THEN 'Flamm'
			WHEN Warehouse_Zone = 'Spec Hand' and p.location_type in ('C') THEN 'CD'
			WHEN Order_Type_Detail = 'Autobulk' THEN 'Auto bulk'
			WHEN Warehouse_Zone is Null THEN 'Bulk'
		ELSE Warehouse_Area END)"))


Dual_Loc_Warrington = sqlQuery(con, paste("SELECT 
Product_Code
 	,1 as Dual_located
  FROM [SupplyChainReporting].[dbo].[aah_location] L
  Left join SupplyChainReporting.dbo.product_list p on L.Prod_Key_C = p.Prod_Key_C 
  Where Print_Branch = '305M' and left(Location_Code,1)='R' and Sequence > 1  and CONVERT(Date,update_TimeStamp)>= DATEADD(DAY,-1, getdate())"))


Main_Query = sqlQuery(con, paste("SELECT date, mq.Branch as Print_Branch, 
                                    mq.Product_Code, 
                                    mq.Location_Code,
                                    
                                    p.Product_Description,
                                    p.Pack_Size,
                                    p.Full_Supplier,
                                    p.Location_Type,
                                    p.Report_Group,
                                    p.Product_Group,
                                    p.Hospital_Restriction,
                                    p.Umbrella_Group,
                                    
                                    mq.First_Receipt_Date,
                                    mq.Replenish_Quantity,
                                    mq.Current_Shelf_Balance, 
                                    mq.Current_Order_Balance,
                                    
                                    wz.Station,
                                    wz.Warehouse_Zone,
                                    wz.Warehouse_Area,
                                    wz.Tote_Delivery, 
                                    
                                    af.Channel_Type, 
                                    af.AFrame_Number


                                   FROM [SupplyChainReporting].[dbo].[main_query_all] mq
                                    Left join AAHOperations.dbo.lu_warehouse_zones wz on left(mq.Location_Code,1) = wz.Station and mq.Branch = wz.Branch
                                    Left Join AAHOperations.dbo.lu_ws_aframe_long_short af on left(mq.Location_Code,5) = Aframe_Sector and mq.Branch = Branch_ID
								                  	Left Join SupplyChainReporting.dbo.product_list p on mq.Product_Code = p.Product_Code
                                   WHERE mq.Branch in ('106m','201D' ,'305m','408t','405n','406p','501g','504m','602j','703m','706s','908b')
                                    and Date >= DATEADD(day,-1, getdate())"))


##Segment <- fread("C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Segmentv1.csv")
Segment <- fread("C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Segmentv2.csv")
Stay_Local <- fread("C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Reset_Local.csv")
Central_SKU <- fread("C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/SKU List Central2.csv")
Central_SKU_504M <- fread("C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Central_504M.csv")

Sales_Import = as.data.table(Sales_Import)
Main_Query = as.data.table(Main_Query)
Dual_Loc_Warrington = as.data.table(Dual_Loc_Warrington)

########## Adding Segment
Sales_Import1 <- merge(
  as.data.table(Sales_Import),
  as.data.table(Segment),
  by.x = c('Delivery_Account','Delivery_DC'),
  by.y = c('Delivery_Account','Account_Branch'),
  all.x = TRUE)

########## Dual Loc
Sales_Import1 <- merge(
  as.data.table(Sales_Import1),
  as.data.table(Dual_Loc_Warrington),
  by = c('Product_Code'),
##  by.y = c('Product_Code'),
  all.x = TRUE)

########## Stay Local
Sales_Import1 <- merge(
  as.data.table(Sales_Import1),
  as.data.table(Stay_Local),
  by = c('Delivery_DC','Van_Number'),
  ##  by.y = c('Product_Code'),
  all.x = TRUE)



Sales_Import1 <- Sales_Import1[is.na(Dual_located), Dual_located := 0 ]
Sales_Import1 <- Sales_Import1[is.na(Stay_Local), Stay_Local := 0 ]

Sales_Import1 <- Sales_Import1[is.na(Segment), Segment := 'None'
                             ][Print_Branch == '406P' & Delivery_DC == '305M'& Order_Type_Zone == 'On Circuit', Print_Branch := '305M'
                               ][Print_Branch == '406P' & Delivery_DC == '305M' & Order_Type_Zone == 'On Circuit', Delivery_Type := 'Next Day AM'
                                 ][Print_Branch == '305M' & Delivery_DC == '305M' & Delivery_Type == 'Next Day Slow_Platform', Delivery_Type := 'Next Day AM'] 



################## Adjust Monjaro BP2
Sales_Import1 <-  Sales_Import1[Product_Code %in% .('MOU0060M','MOU0061X','MOU0062E','MOU0063A','MOU0064T','MOU0065P') & Print_Branch %in% .('602J','908B'), Mounjaro := 1
                              ][,Adj_Print_DC := Print_Branch
                                ][Product_Code %in% .('MOU0060M','MOU0061X','MOU0062E','MOU0063A','MOU0064T','MOU0065P') & Print_Branch %in% .('602J','908B'), Adj_Print_DC := '408T'
                                  ][, Adj_Delivery_Type := Delivery_Type
                                    ][Mounjaro == 1 ,Adj_Delivery_Type := 'Next Day Slow_Platform']




############### Add Stock Elements
Sales_Import1 <- merge(
  as.data.table(Sales_Import1),
  as.data.table(Central_SKU),
  by.x = c('Print_Branch','Product_Code'),
  by.y = c('Branch','Product_Code'),
  all.x = TRUE)


############### Add Stock Elements
Sales_Import1 <- merge(
  as.data.table(Sales_Import1),
  as.data.table(Central_SKU_504M),
  by.x = c('Print_Branch','Product_Code'),
  by.y = c('Branch','Product_Code'),
  all.x = TRUE)


################### Centralisation
Sales_Import1 <-  Sales_Import1[Final_Action %in% .('De List'), Lines := 0
      ][Final_Action %in% .('De List'), Line_Qty := 0
        ][Final_Action %in% .('De List'), Sales_Value := 0]



############### Big Twin 106M and 201D and other changes
Sales_Import1 <-  Sales_Import1[Print_Branch == '305M' & Adj_Delivery_Type == 'Next Day Slow_Platform' & Delivery_DC %in% c('106M','201D') & Stay_Local == '0' ,Adj_Delivery_Type := 'Next Day AM'
][Print_Branch == '201D' & Adj_Delivery_Type == 'Next Day AM' & Order_Type_Zone == 'On Circuit' & Stay_Local == '0', Adj_Print_DC := '305M'
][Print_Branch == '201D' & Adj_Delivery_Type == 'Next Day AM' & Order_Type_Detail == 'Autobulk'  & Stay_Local == '0', Adj_Print_DC := '305M'
][Print_Branch == '201D' & Comp_ID %in% c('T','E'), Adj_Print_DC := '201D'
][Print_Branch == '106M' & Adj_Delivery_Type == 'Next Day AM' & Order_Type_Zone == 'On Circuit' & Stay_Local == '0', Adj_Print_DC := '305M'
][Print_Branch == '106M' & Adj_Delivery_Type == 'Next Day AM' & Order_Type_Detail == 'Autobulk'  & Stay_Local == '0', Adj_Print_DC := '305M'
][Print_Branch == '106M' & Comp_ID %in% c('T','E'), Adj_Print_DC := '305M'
][Print_Branch != '305M' & Delivery_DC == '305M' & Comp_ID %in% c('A') & Order_Type_Zone %in% c('On Circuit','Autobulk') , Adj_Print_DC := '305M' ################ adjust for all the twining
][Print_Branch != '406P' & Delivery_DC == '406P' & Comp_ID %in% c('A') & Order_Type_Zone %in% c('On Circuit','Autobulk') & Is_Out_Based != 'Y'  , Adj_Print_DC := '406P' ############## Adj for 406P vans out due to BCP
][Print_Branch == '305M' & Delivery_DC == '406P' & Comp_ID %in% c('A') & Order_Type_Zone %in% c('On Circuit','Autobulk')   , Adj_Print_DC := '305M'  ####Adj 406P back to 305M for Comp Mark H
][Print_Branch == '504M' &  Remove_504M == '1', Adj_Print_DC := '406P'  ####Central_504M
][Print_Branch == '501G' & Comp_ID %in% c('T') & Delivery_DC == '504M', Adj_Print_DC := '504M'  ####Move TRI to 504M from 501G
]


##[Print_Branch == '504M' & Adj_Delivery_Type == 'Next Day AM' & Order_Type_Detail == 'On Circuit' & Stay_Local == '0', Adj_Print_DC := '501G'
##][Print_Branch == '504M' & Adj_Delivery_Type == 'Next Day AM' & Order_Type_Detail == 'Autobulk' & Stay_Local == '0', Adj_Print_DC := '501G'
##]


############## Current Daily Ave
Current_Daily_Aves <- Sales_Import1[, .(
  Orders = round((uniqueN(Pick_Note_No) / uniqueN(Processing_Date)),2), 
  Ave_Lines = round(sum(Lines, na.rm = TRUE) / uniqueN(Processing_Date), 2),
  Ave_Qty   = round(sum(Line_Qty, na.rm = TRUE) / uniqueN(Processing_Date), 2),
  Ave_Sales_Value   = round(sum(Sales_Value, na.rm = TRUE) / uniqueN(Processing_Date), 2),
  LPT = round(sum(Lines, na.rm = TRUE)/uniqueN(Pick_Note_No),2),
  QPT = round(sum(Line_Qty, na.rm = TRUE)/uniqueN(Pick_Note_No),2),
  Days_Picked = uniqueN(Processing_Date)
), by = .(Print_Branch,Delivery_DC,Delivery_Type,Order_Type_Detail,Segment,Delivery_Point,Processing_Day)]


############ New Daily Ave
New_Daily_Aves <- Sales_Import1[, .(
  Orders = round((uniqueN(Pick_Note_No) / uniqueN(Processing_Date)),2), 
  Ave_Lines = round(sum(Lines, na.rm = TRUE) / uniqueN(Processing_Date), 2),
  Ave_Qty   = round(sum(Line_Qty, na.rm = TRUE) / uniqueN(Processing_Date), 2),
  Ave_Sales_Value   = round(sum(Sales_Value, na.rm = TRUE) / uniqueN(Processing_Date), 2),
  LPT = round(sum(Lines, na.rm = TRUE)/uniqueN(Pick_Note_No),2),
  QPT = round(sum(Line_Qty, na.rm = TRUE)/uniqueN(Pick_Note_No),2),
  Days_Picked = uniqueN(Processing_Date)
), by = .(Adj_Print_DC,Delivery_DC,Adj_Delivery_Type,Order_Type_Detail,Segment,Delivery_Point,Processing_Day)]



###################### Creating new Orders
New_Daily_Aves <- New_Daily_Aves[,New_Orders := Orders
  ][Delivery_DC %in% .('501M','504M','602J','703M','706S','908B') & Adj_Print_DC == '406P' & Order_Type_Detail == 'On Circuit', New_Orders := round(Ave_Lines/2.85,0)
  ][Delivery_DC %in% .('106M') & Adj_Print_DC %in% .('305M')  & Order_Type_Detail == 'On Circuit', New_Orders := round(Ave_Lines/6.5,0)
  ][Delivery_DC %in% .('201D') & Adj_Print_DC %in% .('305M')  & Order_Type_Detail == 'On Circuit', New_Orders := round(Ave_Lines/6.5,0)
  ][Delivery_DC %in% .('305M') & Adj_Print_DC %in% .('305M')  & Order_Type_Detail == 'On Circuit', New_Orders := round(Ave_Lines/6.25,0)
  ]

##[Delivery_DC %in% .('504M') & Adj_Print_DC %in% .('501G')  & Order_Type_Detail == 'On Circuit', New_Orders := round(Ave_Lines/6.5,0)
##          ]





write.csv(Current_Daily_Aves, file = "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/WorkSmart/Project_X_Feb_Curr_day.csv",
          row.names = TRUE)

write.csv(New_Daily_Aves, file = "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/WorkSmart/Project_X_Feb_New_day.csv",
          row.names = TRUE)


############## Current Daily Ave
Current_Daily_Aves_Vans <- Sales_Import1[Adj_Print_DC == '504M', .(
  Orders = round((uniqueN(Pick_Note_No) / uniqueN(Processing_Date)),2), 
  Ave_Lines = round(sum(Lines, na.rm = TRUE)), ##/ uniqueN(Processing_Date), 2),
  Ave_Qty   = round(sum(Line_Qty, na.rm = TRUE)), ##/ uniqueN(Processing_Date), 2),
  Ave_Sales_Value   = round(sum(Sales_Value, na.rm = TRUE)), ##/ uniqueN(Processing_Date), 2),
  Days_Picked = uniqueN(Processing_Date)
), by = .(Print_Branch,Product_Code)]


write.csv(Current_Daily_Aves_Vans, file = "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/WorkSmart/Product_Level.csv",
          row.names = TRUE)




####################### End here

























































############## Current Daily Ave
Current_Daily_Aves <- Sales_Import1[, .(
  Orders = round((uniqueN(Pick_Note_No) / 21),2), 
  Ave_Lines = round(sum(Lines, na.rm = TRUE) / 21, 2),
  Ave_Qty   = round(sum(Line_Qty, na.rm = TRUE) / 21, 2),
  Ave_Sales_Value   = round(sum(Sales_Value, na.rm = TRUE) / 21, 2),
  LPT = round(sum(Lines, na.rm = TRUE)/uniqueN(Pick_Note_No),2),
  QPT = round(sum(Line_Qty, na.rm = TRUE)/uniqueN(Pick_Note_No),2),
  Days_Picked = uniqueN(Processing_Date)
), by = .(Print_Branch,Delivery_DC,Delivery_Type,Order_Type_Detail,Segment,Delivery_Point)]


############ New Daily Ave
New_Daily_Aves <- Sales_Import1[, .(
  Orders = round((uniqueN(Pick_Note_No) / 21),2), 
  Ave_Lines = round(sum(Lines, na.rm = TRUE) / 21, 2),
  Ave_Qty   = round(sum(Line_Qty, na.rm = TRUE) / 21, 2),
  Ave_Sales_Value   = round(sum(Sales_Value, na.rm = TRUE) / 21, 2),
  LPT = round(sum(Lines, na.rm = TRUE)/uniqueN(Pick_Note_No),2),
  QPT = round(sum(Line_Qty, na.rm = TRUE)/uniqueN(Pick_Note_No),2),
  Days_Picked = uniqueN(Processing_Date)
), by = .(Adj_Print_DC,Delivery_DC,Adj_Delivery_Type,Order_Type_Detail,Segment,Delivery_Point)]



###################### Creating new Orders
New_Daily_Aves <- New_Daily_Aves[,New_Orders := Orders
][Delivery_DC %in% .('106M') & Adj_Print_DC %in% .('305M','106M')  & Order_Type_Detail == 'On Circuit', New_Orders := round(Ave_Lines/6.5,0)
][Delivery_DC %in% .('201D') & Adj_Print_DC %in% .('305M','106M')  & Order_Type_Detail == 'On Circuit', New_Orders := round(Ave_Lines/6.5,0)
][Delivery_DC %in% .('501M','504M','602J','703M','706S','908B') & Adj_Print_DC == '406P' & Order_Type_Detail == 'On Circuit', New_Orders := round(Ave_Lines/2.85,0)]



write.csv(Current_Daily_Aves, file = "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/WorkSmart/Project_X_Feb_Curr.csv",
          row.names = TRUE)

write.csv(New_Daily_Aves, file = "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/WorkSmart/Project_X_Feb_New.csv",
          row.names = TRUE)

Sales_Import_P <- Sales_Import1[Adj_Print_DC == Delivery_DC, Local_Switch := 'Local'
                                ][Adj_Print_DC != Delivery_DC, Local_Switch := 'Switch']

Sales_Import_P <- Sales_Import_P[Print_Branch == '201D' & Location_ID %in% .('CLA43',	'CLG12',	'CRB44',	'DRB51',	'DRD11',	'DRF22',	'ELA11',	'ELG12'
                                                                             ,	'ERH11',	'ERH12',	'FLA12',	'GLE22',	'HLE11',	'HLE41',	'HRB12') 
                                                                            & Order_Type_Zone == 'Bulk', Warehouse_Zone := 'Auto bulk']
Sales_Import_P <- Sales_Import_P[Print_Branch == '201D' & Location_ID %in% .('CLA43',	'CLG12',	'CRB44',	'DRB51',	'DRD11',	'DRF22',	'ELA11',	'ELG12',	'ERH11'
                                                                             ,	'ERH12',	'FLA12',	'GLE22','HLE11',	'HLE41',	'HRB12') 
                                                                            & Order_Type_Zone == 'On Circuit', Warehouse_Zone := 'Auto OverStock']
#################################### Product Level Sales 1
############ New Daily Ave
New_Daily_Aves_Product <- Sales_Import_P[, .(
  Ave_Lines = round(sum(Lines, na.rm = TRUE) / 21, 2),
  Ave_Qty   = round(sum(Line_Qty, na.rm = TRUE) / 21, 2),
  Max_Lines = max(Lines, na.rm = TRUE),
  Max_Qty   = max(Line_Qty, na.rm = TRUE),
  SD_Lines  = sd(Lines, na.rm = TRUE),
  SD_Qty    = sd(Line_Qty, na.rm = TRUE),
  Days_Picked = uniqueN(Processing_Date)
), by = .(Adj_Print_DC,Warehouse_Zone,Product_Code,Local_Switch)]

Lin_Coop <- fread("C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/WorkSmart/Lin_Coop.csv")
New_Daily_Aves_Product1 <- rbind(New_Daily_Aves_Product,Lin_Coop)


############ New Daily Ave for Slotting Tool
New_Daily_Aves_Product2 <- New_Daily_Aves_Product1[, .(
  Ave_Lines = round(sum(Ave_Lines, na.rm = TRUE) / 1, 2),
  Ave_Qty   = round(sum(Ave_Qty, na.rm = TRUE) / 1, 2),
  Max_Lines = max(Max_Lines, na.rm = TRUE),
  Max_Qty   = max(Max_Qty, na.rm = TRUE),
  SD_Lines  = sd(SD_Lines, na.rm = TRUE),
  SD_Qty    = sd(SD_Qty, na.rm = TRUE),
  Days_Picked = round(sum(Days_Picked, na.rm = TRUE) / 1, 2)
), by = .(Adj_Print_DC,Warehouse_Zone,Product_Code,Local_Switch)]


write.csv(New_Daily_Aves_Product2, file = "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/WorkSmart/Project_X_Product.csv",
          row.names = FALSE)

################## Product File with Current Sales
#################################### Product Level Sales 1

############ New Daily Ave  Product Level
New_Daily_Aves_Product_Cur_v_New <- Sales_Import_P[De_List == 0, .(
  Ave_Lines = round(sum(Lines, na.rm = TRUE) / 21, 2),
  Ave_Qty   = round(sum(Line_Qty, na.rm = TRUE) / 21, 2),
  Days_Picked = uniqueN(Processing_Date)
), by = .(Print_Branch,Adj_Print_DC,Product_Code)]

write.csv(New_Daily_Aves_Product_Cur_v_New, file = "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/WorkSmart/Project_X_Product_Supplier.csv",
          row.names = FALSE)






