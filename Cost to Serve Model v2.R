rm(list=ls())
gc()


setwd('C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/')

library(RODBC); library(data.table); library(lubridate); library(foreach); library(plyr); library(dplyr); library(matrixStats); library(stringr)

##R package tstools has the ability to run ABC and XYZ analysis library(tstools)
##https://kourentzes.com/forecasting/2016/10/15/abc-xyz-analysis-for-forecasting/

Connection_function <- function() {
  
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
########
Sales_Import = sqlQuery(con, paste("/****** Script for SelectTopNRows command from SSMS  ******/
/******  Summary By Month ******/
Declare @Date Date
Set @Date = '2026-07-01'

SELECT  bi.Print_Branch,
		bi.Process_Month,
		bi.Document_Type,
		TRY_CONVERT(DATE,bi.Processing_Date) as Processing_Date,
		C.Account_Branch as Charge_DC,
		c.Account_Number as Charge_Account, 
		c.Account_Name as Charge_Acc_Name,
		c.Customer_Type_ID,
		c.Customer_Group_ID,
		c.Customer_Channel_ID,
		Order_Type_Zone,
		Order_Type_Detail,
		bi.Van_Number,
		BI.Van_Drop_Number,
		TRY_CONVERT(DATE,bi.Van_Day) as Van_Day,
		COUNT(DISTINCT Pick_Note_No) as Orders,
		COUNT(bi.Line_Quantity) as Lines,
		SUM(bi.Line_Quantity) as Line_Qty,
		SUM(bi.Adj_Cost_Of_Sales_Value) as Sales_Value,
	CASE when C.Account_Number like '0000%' then 'IBT' ELSE bi.Company_ID end as Comp_ID,
	
	CASE	WHEN c.Account_Number in ('00000155S','00000133S') THEN '405N'
				WHEN c.Account_Number in ('00000157C','00000137J') THEN '501G'
				WHEN c.Account_Number in ('00000159J','00000139Q') THEN '106M'
				WHEN c.Account_Number in ('00000162E','00000142L') THEN '706S'
				WHEN c.Account_Number in ('00000163T','00000143D') THEN '504M'
				WHEN c.Account_Number in ('00000164L','00000144S') THEN '305M'
				WHEN c.Account_Number in ('00000166S','00000146C') THEN '703M'
				WHEN c.Account_Number in ('00000168C','00000153L') THEN '406P'
				WHEN c.Account_Number in ('00000141T') THEN '502X'
				WHEN c.Account_Number in ('00000140E','00000160U') THEN '201D'WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Branch
				WHEN C1.Account_Branch is NUll then C.Account_Branch
				
				ELSE C1.Account_Branch END as Delivery_DC,
	
	CASE	WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Number
				WHEN C1.Account_Branch is NUll then C.Account_Number
				ELSE C1.Account_Number END as Delivery_Account,
	
	CASE	WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Name
				WHEN C1.Account_Branch is NUll then C.Account_Name
				ELSE C1.Account_Name END as Delivery_Name,
	
	CASE	--	WHEN Product_Code in ('CUS0927W','LLO3350Q','AAH0081P','EME0088T','DEL0432N','TRI0866V','LOW0008F','LOW0009B', 'FIN0507E', 'FUE0001X')  THEN 'Charges'
			--	WHEN Umbrella_Group in ('6','8','12','13','14','15','16','17','18','19','22','23') THEN 'Charges'
			--	WHEN Full_Supplier IN ('DEP/009M','VES/005E') THEN 'Charges or Mail_Outs'
				WHEN bi.PAS = '1' and c.Customer_Group_ID  in ('890', '1050') THEN 'X-Dock'
				WHEN c.Account_Branch = '204J' and bi.Print_Branch = '305M' THEN 'X-Dock'
				WHEN c.Account_Branch != '406P' and bi.Print_Branch = '406P' THEN 'X-Dock'
				WHEN c.Account_Branch != '305M' and bi.Print_Branch = '305M' THEN 'X-Dock'
				WHEN c.Account_Branch != '408T' and bi.Print_Branch = '408T' THEN 'X-Dock'
				WHEN bi.Print_Branch = '606R' and bi.Company_ID = 'T' THEN 'X-Dock'		
				when bi.Print_Branch = '606R' and C.Account_Number like '0000%' then 'IBT'
				WHEN bi.Print_Branch = '606R' and bi.Company_ID != 'T' THEN 'X-Dock'
				WHEN C.Account_Branch != bi.Print_Branch THEN 'X-Dock' 
				WHEN V.Van_Depart_Time <= v.Van_Close_Time THEN 'Local'
				ELSE 'Local' END as Delivery_Type,

	CASE WHEN V.Van_Depart_Time >= v.Van_Close_Time THEN 'PM Van'
				ELSE 'AM Van' END as AM_PM

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
  and datepart(WEEKDAY,(TRY_CONVERT(date ,BI.Van_Day)))-1 = TRY_CONVERT(int,cv.Day_No) 
  and bi.Van_Number = cv.Van_No AND cV.Destination_Branch IN ('504m','908b')
    
  Left join SupplyChainReporting.dbo.product_list p on bi.Prod_Key_C = p.Prod_Key_C
  left join SupplyChainReporting.dbo.product_list_extra pe on bi.Prod_Key_C = pe.Prod_Key_C 

  left join SupplyChainReporting.dbo.van_history v on (CASE	WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Branch
				WHEN C1.Account_Branch is NUll then C.Account_Branch
				ELSE C1.Account_Branch END) = v.Print_Branch and bi.Van_Number = v.Van_No and TRY_CONVERT(DATE,v.date) = TRY_CONVERT(DATE,bi.Processing_Date) --DATEADD(day,-2, getdate())


  left join SupplyChainReporting.dbo.order_type_marker_details ot on bi.Order_Type_Marker = ot.Order_Type_Marker and bi.Print_Branch =ot.Print_Branch

  left Join AAHOperations.dbo.LU_Warehouse_Zones wz on  left(bi.Location_ID,1) = Station and bi.Print_Branch = wz.Branch

  Left Join SupplyChainReporting.dbo.customer_type_description ctd on C.Customer_Type_ID = ctd.Customer_Type_Number 

--  Left Join SupplyChainReporting.dbo.b2b_invoices bi2 on bi.Cust_Key_C = bi2.Cust_Key_C and bi2.Original_Order_Number = bi.Invoice_Number and bi.Prod_Key_C = bi2.Prod_Key_C 

  WHERE bi.Process_Month = @Date
					and bi.Document_Type = 'I'
					and bi.Omit_Marker ='0'
					and Product_Code   Not in ('CUS0927W','LLO3350Q','AAH0081P','EME0088T','DEL0432N','TRI0866V','LOW0008F','LOW0009B', 'FIN0507E', 'FUE0001X') 
					and Umbrella_Group  Not in ('6','8','12','13','14','15','16','17','18','19','22','23') 
					and Full_Supplier NOT IN ('DEP/009M','VES/005E')
					and bi.Print_Branch in ('101c','106m','201D' ,'204j' ,'305m','405n','406p' ,'501g' ,'502x','504m', '602j','703m' ,'706s','908b','606r','408t')
--					and TRY_CONVERT(date,bi.Processing_Date) >= DATEADD(DAY,-2, getdate())
GROUP BY  bi.Print_Branch,
		bi.Process_Month,
		bi.Document_Type,
		TRY_CONVERT(DATE,bi.Processing_Date),
		C.Account_Branch,
		c.Account_Number, 
		c.Account_Name,
		c.Customer_Type_ID,
		c.Customer_Group_ID,
		c.Customer_Channel_ID,
		Order_Type_Zone,
		Order_Type_Detail,
		bi.Van_Number,
		BI.Van_Drop_Number,
		TRY_CONVERT(DATE,bi.Van_Day),
		(CASE when C.Account_Number like '0000%' then 'IBT' ELSE bi.Company_ID end),
		(CASE	WHEN c.Account_Number in ('00000155S','00000133S') THEN '405N'
				WHEN c.Account_Number in ('00000157C','00000137J') THEN '501G'
				WHEN c.Account_Number in ('00000159J','00000139Q') THEN '106M'
				WHEN c.Account_Number in ('00000162E','00000142L') THEN '706S'
				WHEN c.Account_Number in ('00000163T','00000143D') THEN '504M'
				WHEN c.Account_Number in ('00000164L','00000144S') THEN '305M'
				WHEN c.Account_Number in ('00000166S','00000146C') THEN '703M'
				WHEN c.Account_Number in ('00000168C','00000153L') THEN '406P'
				WHEN c.Account_Number in ('00000141T') THEN '502X'
				WHEN c.Account_Number in ('00000140E','00000160U') THEN '201D'WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Branch
				WHEN C1.Account_Branch is NUll then C.Account_Branch
				
				ELSE C1.Account_Branch END),
		(CASE	WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Number
				WHEN C1.Account_Branch is NUll then C.Account_Number
				ELSE C1.Account_Number END),
		(CASE	WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Name
				WHEN C1.Account_Branch is NUll then C.Account_Name
				ELSE C1.Account_Name END),
		(CASE	--	WHEN Product_Code in ('CUS0927W','LLO3350Q','AAH0081P','EME0088T','DEL0432N','TRI0866V','LOW0008F','LOW0009B', 'FIN0507E', 'FUE0001X')  THEN 'Charges'
			--	WHEN Umbrella_Group in ('6','8','12','13','14','15','16','17','18','19','22','23') THEN 'Charges'
			--	WHEN Full_Supplier IN ('DEP/009M','VES/005E') THEN 'Charges or Mail_Outs'
				WHEN bi.PAS = '1' and c.Customer_Group_ID  in ('890', '1050') THEN 'X-Dock'
				WHEN c.Account_Branch = '204J' and bi.Print_Branch = '305M' THEN 'X-Dock'
				WHEN c.Account_Branch != '406P' and bi.Print_Branch = '406P' THEN 'X-Dock'
				WHEN c.Account_Branch != '305M' and bi.Print_Branch = '305M' THEN 'X-Dock'
				WHEN c.Account_Branch != '408T' and bi.Print_Branch = '408T' THEN 'X-Dock'
				WHEN bi.Print_Branch = '606R' and bi.Company_ID = 'T' THEN 'X-Dock'		
				when bi.Print_Branch = '606R' and C.Account_Number like '0000%' then 'IBT'
				WHEN bi.Print_Branch = '606R' and bi.Company_ID != 'T' THEN 'X-Dock'
				WHEN C.Account_Branch != bi.Print_Branch THEN 'X-Dock' 
				WHEN V.Van_Depart_Time <= v.Van_Close_Time THEN 'Local'
				ELSE 'Local' END),
		(CASE WHEN V.Van_Depart_Time >= v.Van_Close_Time THEN 'PM Van'
				ELSE 'AM Van' END)
UNION
SELECT [Print_Branch]
	  ,TRY_CONVERT(DATE,[Process_Month])
	  ,'I' as Document_Type
	  ,TRY_CONVERT(DATE,[Doc_Date]) as Processing_Date
      ,[Account_Branch] as Charge_DC
      ,[Account_No] as Charge_Account
	  , CASE	WHEN  (Account_No in ('80000101S','80141541J','80141509F','80141523A','80141574J') or Trunker_No = '911') THEN 'Aberdeen'
			WHEN  (Account_No in ('80000106B','80144540G','80144510F','80132568V','80144522U','80144504L') or Trunker_No = '916') THEN 'Glasgow'
			WHEN  (Account_No in ('80000201Y','80138543A','80138511P','80138515G','80138619Q','80138524W','80138525Y','80138622L') or Trunker_No = '921') THEN 'Leeds'
			WHEN  (Account_No in ('80000204N','80117540B','80117509M','80117511P','80117522P','80117571R')  or Trunker_No = '924') THEN 'Gateshead'
			WHEN  (Account_No in ('80000305B','80172559U','80172558F','80172498S','80172548U','80172550A','80172552H','80172546N','80172560X') or Trunker_No = '935') THEN 'Warrington'
			WHEN  (Account_No in ('80000405U','80112534K','80112509P','80112511S','80112566S') or Trunker_No = '945') THEN 'Belfast'
			WHEN  (Account_No in ('80000406M','84301069M','84301056F','84301057U','84301059E','84301061H','84301064G','84301067F','84301062W') or Trunker_No = '946') THEN 'Birmingham'
			WHEN  (Account_No in ('80000501C','80130575W','80130498S','80130523V','80130524N','80130121H','80130692M','80130509D','80130669B') or Trunker_No = '951') THEN 'Bristol'
			WHEN  (Account_No in ('80000502R','80131540T','80131504B','80131524R','80131534C') or Trunker_No = '952') THEN 'Paignton'
			WHEN  (Account_No in ('80000504B','80125540Y','80125579M','80125509C','80125525E','80125534U','80125523U','80125524M') or Trunker_No = '954') THEN 'Swansea'
			WHEN  (Account_No in ('80000602N','80128540D','80128527A','80128523J','80128534J') or Trunker_No = '962') THEN 'Bexhill'
			WHEN  (Account_No in ('80000703B','80121546T','80121582Y','80121513T','80121576U','80121527S','80121528K','80121578E','80121572G','80121535T')or Trunker_No = '973') THEN 'Ruislip'
			WHEN  (Account_No in ('80000706A','80052545A','82526574Q','80052532Q','80052544X','80052546P','80052555X')or Trunker_No = '976') THEN 'Romford'
			WHEN  (Account_No in ('80000908W','80025540M','80025530E','80025539B','80025549J') or Trunker_No = '998') THEN 'Southampton'
			WHEN  (Account_No in ('80068352Q','80025540M') or Trunker_No = '942') THEN 'Weedon'
			WHEN  (Account_No in ('80000014G','84217081U','84217075D','87314132B','80711205V','84217076S',
								'80235035J','84217078C','84830626D','84217080F','89001119D','83712112M','84217082M',
								'83627179U','82907091A','80102014Q','84217087S','83303726X','85103064J','82448208L','80000015V') or Trunker_No in ('966','968')) THEN 'Stoke'
			WHEN (Account_No in ('80119499E','80025540M','80119575Q','80119570K','80119550R','80119622T','80119546H',
								'80119562J','80119557H','80119563B','80119554X','80119625S','80119625S','80119577A','80119560C','80174079S') or Trunker_No = '948') THEN 'Tamworth'
		ELSE 'No Match' END as Charge_Acc_Name
	  ,'43' as Customer_Type_ID
	  ,'9999' as Customer_Group_ID
	  , '9999' as Customer_Channel_ID
	  ,'IBT' as Order_Type_Zone
	  ,'IBT' as Order_Type_Detail
	  ,Trunker_No as Van_Number
	  ,'0' as Van_Drop_Number
	  ,TRY_CONVERT(DATE,[Doc_Date]) as Van_Day
	,count(ibts.Doc_No) as Orders
	  ,count(Line_Qty) as Lines
	  ,sum(Line_Qty) as Line_Qty
	  ,Sum(Line_Cost) as Sales_Value
	  ,'IBT' as Comp_ID
 	   ,CASE	WHEN  (Account_No in ('80000101S','80141541J','80141509F','80141523A','80141574J') or Trunker_No = '911') THEN '101C'
			WHEN  (Account_No in ('80000106B','80144540G','80144510F','80132568V','80144522U','80144504L') or Trunker_No = '916') THEN '106M'
			WHEN  (Account_No in ('80000201Y','80138543A','80138511P','80138515G','80138619Q','80138524W','80138525Y','80138622L') or Trunker_No = '921') THEN '201D'
			WHEN  (Account_No in ('80000204N','80117540B','80117509M','80117511P','80117522P','80117571R')  or Trunker_No = '924') THEN '204J'
			WHEN  (Account_No in ('80000305B','80172559U','80172558F','80172498S','80172548U','80172550A','80172552H','80172546N','80172560X') or Trunker_No = '935') THEN '305M'
			WHEN  (Account_No in ('80000405U','80112534K','80112509P','80112511S','80112566S') or Trunker_No = '945') THEN '405N'
			WHEN  (Account_No in ('80000406M','84301069M','84301056F','84301057U','84301059E','84301061H','84301064G','84301067F','84301062W') or Trunker_No = '946') THEN '406P'
			WHEN  (Account_No in ('80000501C','80130575W','80130498S','80130523V','80130524N','80130121H','80130692M','80130509D','80130669B') or Trunker_No = '951') THEN '501G'
			WHEN  (Account_No in ('80000502R','80131540T','80131504B','80131524R','80131534C') or Trunker_No = '952') THEN '502X'
			WHEN  (Account_No in ('80000504B','80125540Y','80125579M','80125509C','80125525E','80125534U','80125523U','80125524M') or Trunker_No = '954') THEN '504M'
			WHEN  (Account_No in ('80000602N','80128540D','80128527A','80128523J','80128534J') or Trunker_No = '962') THEN '602J'
			WHEN  (Account_No in ('80000703B','80121546T','80121582Y','80121513T','80121576U','80121527S','80121528K','80121578E','80121572G','80121535T')or Trunker_No = '973') THEN '703M'
			WHEN  (Account_No in ('80000706A','80052545A','82526574Q','80052532Q','80052544X','80052546P','80052555X')or Trunker_No = '976') THEN '706S'
			WHEN  (Account_No in ('80000908W','80025540M','80025530E','80025539B','80025549J') or Trunker_No = '998') THEN '908B'
			WHEN  (Account_No in ('80068352Q','80025540M') or Trunker_No = '942') THEN '402H'
			WHEN  (Account_No in ('80000014G','84217081U','84217075D','87314132B','80711205V','84217076S',
								'80235035J','84217078C','84830626D','84217080F','89001119D','83712112M','84217082M',
								'83627179U','82907091A','80102014Q','84217087S','83303726X','85103064J','82448208L','80000015V') or Trunker_No in ('966','968')) THEN '606R'
			WHEN (Account_No in ('80119499E','80025540M','80119575Q','80119570K','80119550R','80119622T','80119546H',
								'80119562J','80119557H','80119563B','80119554X','80119625S','80119625S','80119577A','80119560C','80174079S') or Trunker_No = '948') THEN '408T'
		ELSE 'No Match' END as Delivery_DC
	      ,Account_No as Delivery_Account

	   ,CASE	WHEN  (Account_No in ('80000101S','80141541J','80141509F','80141523A','80141574J') or Trunker_No = '911') THEN 'Aberdeen'
			WHEN  (Account_No in ('80000106B','80144540G','80144510F','80132568V','80144522U','80144504L') or Trunker_No = '916') THEN 'Glasgow'
			WHEN  (Account_No in ('80000201Y','80138543A','80138511P','80138515G','80138619Q','80138524W','80138525Y','80138622L') or Trunker_No = '921') THEN 'Leeds'
			WHEN  (Account_No in ('80000204N','80117540B','80117509M','80117511P','80117522P','80117571R')  or Trunker_No = '924') THEN 'Gateshead'
			WHEN  (Account_No in ('80000305B','80172559U','80172558F','80172498S','80172548U','80172550A','80172552H','80172546N','80172560X') or Trunker_No = '935') THEN 'Warrington'
			WHEN  (Account_No in ('80000405U','80112534K','80112509P','80112511S','80112566S') or Trunker_No = '945') THEN 'Belfast'
			WHEN  (Account_No in ('80000406M','84301069M','84301056F','84301057U','84301059E','84301061H','84301064G','84301067F','84301062W') or Trunker_No = '946') THEN 'Birmingham'
			WHEN  (Account_No in ('80000501C','80130575W','80130498S','80130523V','80130524N','80130121H','80130692M','80130509D','80130669B') or Trunker_No = '951') THEN 'Bristol'
			WHEN  (Account_No in ('80000502R','80131540T','80131504B','80131524R','80131534C') or Trunker_No = '952') THEN 'Paignton'
			WHEN  (Account_No in ('80000504B','80125540Y','80125579M','80125509C','80125525E','80125534U','80125523U','80125524M') or Trunker_No = '954') THEN 'Swansea'
			WHEN  (Account_No in ('80000602N','80128540D','80128527A','80128523J','80128534J') or Trunker_No = '962') THEN 'Bexhill'
			WHEN  (Account_No in ('80000703B','80121546T','80121582Y','80121513T','80121576U','80121527S','80121528K','80121578E','80121572G','80121535T')or Trunker_No = '973') THEN 'Ruislip'
			WHEN  (Account_No in ('80000706A','80052545A','82526574Q','80052532Q','80052544X','80052546P','80052555X')or Trunker_No = '976') THEN 'Romford'
			WHEN  (Account_No in ('80000908W','80025540M','80025530E','80025539B','80025549J') or Trunker_No = '998') THEN 'Southampton'
			WHEN  (Account_No in ('80068352Q','80025540M') or Trunker_No = '942') THEN 'Weedon'
			WHEN  (Account_No in ('80000014G','84217081U','84217075D','87314132B','80711205V','84217076S',
								'80235035J','84217078C','84830626D','84217080F','89001119D','83712112M','84217082M',
								'83627179U','82907091A','80102014Q','84217087S','83303726X','85103064J','82448208L','80000015V') or Trunker_No in ('966','968')) THEN 'Stoke'
			WHEN (Account_No in ('80119499E','80025540M','80119575Q','80119570K','80119550R','80119622T','80119546H',
								'80119562J','80119557H','80119563B','80119554X','80119625S','80119625S','80119577A','80119560C','80174079S') or Trunker_No = '948') THEN 'Tamworth'
		ELSE 'No Match' END as Delivery_Name
	,'IBT' as Delivery_Type
	,'IBT' as AM_PM
--	,'IBT' as Warehouse_Zone
  FROM [SupplyChainReporting].[dbo].[aah_ibts] ibts
  Left join SupplyChainReporting.dbo.product_list p on ibts.Product_Code = p.Product_Code
  Where Print_Branch in ('408T','608V','101c','106m','201D' ,'204j' ,'305m','405n','406p' ,'501g' ,'502x' , '504m', '602j','703m' ,'706s','908b') 
  and Process_Month = @Date and Omit_Marker = '0' 
  GROUP BY
  [Print_Branch]
	,TRY_CONVERT(DATE,[Process_Month])
	,[Account_Branch]
	,TRY_CONVERT(DATE,[Doc_Date])
	,[Account_No]
	,(CASE	WHEN  (Account_No in ('80000101S','80141541J','80141509F','80141523A','80141574J') or Trunker_No = '911') THEN 'Aberdeen'
			WHEN  (Account_No in ('80000106B','80144540G','80144510F','80132568V','80144522U','80144504L') or Trunker_No = '916') THEN 'Glasgow'
			WHEN  (Account_No in ('80000201Y','80138543A','80138511P','80138515G','80138619Q','80138524W','80138525Y','80138622L') or Trunker_No = '921') THEN 'Leeds'
			WHEN  (Account_No in ('80000204N','80117540B','80117509M','80117511P','80117522P','80117571R')  or Trunker_No = '924') THEN 'Gateshead'
			WHEN  (Account_No in ('80000305B','80172559U','80172558F','80172498S','80172548U','80172550A','80172552H','80172546N','80172560X') or Trunker_No = '935') THEN 'Warrington'
			WHEN  (Account_No in ('80000405U','80112534K','80112509P','80112511S','80112566S') or Trunker_No = '945') THEN 'Belfast'
			WHEN  (Account_No in ('80000406M','84301069M','84301056F','84301057U','84301059E','84301061H','84301064G','84301067F','84301062W') or Trunker_No = '946') THEN 'Birmingham'
			WHEN  (Account_No in ('80000501C','80130575W','80130498S','80130523V','80130524N','80130121H','80130692M','80130509D','80130669B') or Trunker_No = '951') THEN 'Bristol'
			WHEN  (Account_No in ('80000502R','80131540T','80131504B','80131524R','80131534C') or Trunker_No = '952') THEN 'Paignton'
			WHEN  (Account_No in ('80000504B','80125540Y','80125579M','80125509C','80125525E','80125534U','80125523U','80125524M') or Trunker_No = '954') THEN 'Swansea'
			WHEN  (Account_No in ('80000602N','80128540D','80128527A','80128523J','80128534J') or Trunker_No = '962') THEN 'Bexhill'
			WHEN  (Account_No in ('80000703B','80121546T','80121582Y','80121513T','80121576U','80121527S','80121528K','80121578E','80121572G','80121535T')or Trunker_No = '973') THEN 'Ruislip'
			WHEN  (Account_No in ('80000706A','80052545A','82526574Q','80052532Q','80052544X','80052546P','80052555X')or Trunker_No = '976') THEN 'Romford'
			WHEN  (Account_No in ('80000908W','80025540M','80025530E','80025539B','80025549J') or Trunker_No = '998') THEN 'Southampton'
			WHEN  (Account_No in ('80068352Q','80025540M') or Trunker_No = '942') THEN 'Weedon'
			WHEN  (Account_No in ('80000014G','84217081U','84217075D','87314132B','80711205V','84217076S',
								'80235035J','84217078C','84830626D','84217080F','89001119D','83712112M','84217082M',
								'83627179U','82907091A','80102014Q','84217087S','83303726X','85103064J','82448208L','80000015V') or Trunker_No in ('966','968')) THEN 'Stoke'
			WHEN (Account_No in ('80119499E','80025540M','80119575Q','80119570K','80119550R','80119622T','80119546H',
								'80119562J','80119557H','80119563B','80119554X','80119625S','80119625S','80119577A','80119560C','80174079S') or Trunker_No = '948') THEN 'Tamworth'
		ELSE 'No Match' END)
		,Trunker_No
	  ,(CASE	WHEN  (Account_No in ('80000101S','80141541J','80141509F','80141523A','80141574J') or Trunker_No = '911') THEN '101C'
			WHEN  (Account_No in ('80000106B','80144540G','80144510F','80132568V','80144522U','80144504L') or Trunker_No = '916') THEN '106M'
			WHEN  (Account_No in ('80000201Y','80138543A','80138511P','80138515G','80138619Q','80138524W','80138525Y','80138622L') or Trunker_No = '921') THEN '201D'
			WHEN  (Account_No in ('80000204N','80117540B','80117509M','80117511P','80117522P','80117571R')  or Trunker_No = '924') THEN '204J'
			WHEN  (Account_No in ('80000305B','80172559U','80172558F','80172498S','80172548U','80172550A','80172552H','80172546N','80172560X') or Trunker_No = '935') THEN '305M'
			WHEN  (Account_No in ('80000405U','80112534K','80112509P','80112511S','80112566S') or Trunker_No = '945') THEN '405N'
			WHEN  (Account_No in ('80000406M','84301069M','84301056F','84301057U','84301059E','84301061H','84301064G','84301067F','84301062W') or Trunker_No = '946') THEN '406P'
			WHEN  (Account_No in ('80000501C','80130575W','80130498S','80130523V','80130524N','80130121H','80130692M','80130509D','80130669B') or Trunker_No = '951') THEN '501G'
			WHEN  (Account_No in ('80000502R','80131540T','80131504B','80131524R','80131534C') or Trunker_No = '952') THEN '502X'
			WHEN  (Account_No in ('80000504B','80125540Y','80125579M','80125509C','80125525E','80125534U','80125523U','80125524M') or Trunker_No = '954') THEN '504M'
			WHEN  (Account_No in ('80000602N','80128540D','80128527A','80128523J','80128534J') or Trunker_No = '962') THEN '602J'
			WHEN  (Account_No in ('80000703B','80121546T','80121582Y','80121513T','80121576U','80121527S','80121528K','80121578E','80121572G','80121535T')or Trunker_No = '973') THEN '703M'
			WHEN  (Account_No in ('80000706A','80052545A','82526574Q','80052532Q','80052544X','80052546P','80052555X')or Trunker_No = '976') THEN '706S'
			WHEN  (Account_No in ('80000908W','80025540M','80025530E','80025539B','80025549J') or Trunker_No = '998') THEN '908B'
			WHEN  (Account_No in ('80068352Q','80025540M') or Trunker_No = '942') THEN '402H'
			WHEN  (Account_No in ('80000014G','84217081U','84217075D','87314132B','80711205V','84217076S',
								'80235035J','84217078C','84830626D','84217080F','89001119D','83712112M','84217082M',
								'83627179U','82907091A','80102014Q','84217087S','83303726X','85103064J','82448208L','80000015V') or Trunker_No in ('966','968')) THEN '606R'
			WHEN (Account_No in ('80119499E','80025540M','80119575Q','80119570K','80119550R','80119622T','80119546H',
								'80119562J','80119557H','80119563B','80119554X','80119625S','80119625S','80119577A','80119560C','80174079S') or Trunker_No = '948') THEN '408T'
		ELSE 'No Match' END)
		,Account_No
		,(CASE	WHEN  (Account_No in ('80000101S','80141541J','80141509F','80141523A','80141574J') or Trunker_No = '911') THEN 'Aberdeen'
			WHEN  (Account_No in ('80000106B','80144540G','80144510F','80132568V','80144522U','80144504L') or Trunker_No = '916') THEN 'Glasgow'
			WHEN  (Account_No in ('80000201Y','80138543A','80138511P','80138515G','80138619Q','80138524W','80138525Y','80138622L') or Trunker_No = '921') THEN 'Leeds'
			WHEN  (Account_No in ('80000204N','80117540B','80117509M','80117511P','80117522P','80117571R')  or Trunker_No = '924') THEN 'Gateshead'
			WHEN  (Account_No in ('80000305B','80172559U','80172558F','80172498S','80172548U','80172550A','80172552H','80172546N','80172560X') or Trunker_No = '935') THEN 'Warrington'
			WHEN  (Account_No in ('80000405U','80112534K','80112509P','80112511S','80112566S') or Trunker_No = '945') THEN 'Belfast'
			WHEN  (Account_No in ('80000406M','84301069M','84301056F','84301057U','84301059E','84301061H','84301064G','84301067F','84301062W') or Trunker_No = '946') THEN 'Birmingham'
			WHEN  (Account_No in ('80000501C','80130575W','80130498S','80130523V','80130524N','80130121H','80130692M','80130509D','80130669B') or Trunker_No = '951') THEN 'Bristol'
			WHEN  (Account_No in ('80000502R','80131540T','80131504B','80131524R','80131534C') or Trunker_No = '952') THEN 'Paignton'
			WHEN  (Account_No in ('80000504B','80125540Y','80125579M','80125509C','80125525E','80125534U','80125523U','80125524M') or Trunker_No = '954') THEN 'Swansea'
			WHEN  (Account_No in ('80000602N','80128540D','80128527A','80128523J','80128534J') or Trunker_No = '962') THEN 'Bexhill'
			WHEN  (Account_No in ('80000703B','80121546T','80121582Y','80121513T','80121576U','80121527S','80121528K','80121578E','80121572G','80121535T')or Trunker_No = '973') THEN 'Ruislip'
			WHEN  (Account_No in ('80000706A','80052545A','82526574Q','80052532Q','80052544X','80052546P','80052555X')or Trunker_No = '976') THEN 'Romford'
			WHEN  (Account_No in ('80000908W','80025540M','80025530E','80025539B','80025549J') or Trunker_No = '998') THEN 'Southampton'
			WHEN  (Account_No in ('80068352Q','80025540M') or Trunker_No = '942') THEN 'Weedon'
			WHEN  (Account_No in ('80000014G','84217081U','84217075D','87314132B','80711205V','84217076S',
								'80235035J','84217078C','84830626D','84217080F','89001119D','83712112M','84217082M',
								'83627179U','82907091A','80102014Q','84217087S','83303726X','85103064J','82448208L','80000015V') or Trunker_No in ('966','968')) THEN 'Stoke'
			WHEN (Account_No in ('80119499E','80025540M','80119575Q','80119570K','80119550R','80119622T','80119546H',
								'80119562J','80119557H','80119563B','80119554X','80119625S','80119625S','80119577A','80119560C','80174079S') or Trunker_No = '948') THEN 'Tamworth'
		ELSE 'No Match' END)"))


##write.csv2(Sales_Import, file = "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/WorkSmart/OutBound_Orders_Line_Qty_202402.csv",
##          sep = "|",row.names = TRUE)



###########

Sales_Import_Line = sqlQuery(con, paste("/******  Summary By Month ******/
Declare @Date Date
Set @Date = '2026-07-01'

SELECT  bi.Print_Branch,
		bi.Process_Month,
		bi.Document_Type,
		bi.Credit_Note_Analysis_Marker,
		C.Account_Branch as Charge_DC,
		c.Account_Number as Charge_Account, 
		c.Account_Name as Charge_Acc_Name,
		c.Customer_Type_ID,
		c.Customer_Group_ID,
		c.Customer_Channel_ID,
		Order_Type_Zone,
		bi.EOE_Flag,
		Order_Type_Detail,
		Product_Code,
		Product_Description,
		Pack_Size,
		Full_Supplier,
		p.Report_Group,
		p.Product_Group,
		p.Umbrella_Group,
		p.Location_Type,
		COUNT(bi.Line_Quantity) as Lines,
		SUM(bi.Line_Quantity) as Line_Qty,
		SUM(bi.Adj_Cost_Of_Sales_Value) as Sales_Value,
	CASE when C.Account_Number like '0000%' then 'IBT' ELSE bi.Company_ID end as Comp_ID,
	
	CASE	WHEN c.Account_Number in ('00000155S','00000133S') THEN '405N'
				WHEN c.Account_Number in ('00000157C','00000137J') THEN '501G'
				WHEN c.Account_Number in ('00000159J','00000139Q') THEN '106M'
				WHEN c.Account_Number in ('00000162E','00000142L') THEN '706S'
				WHEN c.Account_Number in ('00000163T','00000143D') THEN '504M'
				WHEN c.Account_Number in ('00000164L','00000144S') THEN '305M'
				WHEN c.Account_Number in ('00000166S','00000146C') THEN '703M'
				WHEN c.Account_Number in ('00000168C','00000153L') THEN '406P'
				WHEN c.Account_Number in ('00000141T') THEN '502X'
				WHEN c.Account_Number in ('00000140E','00000160U') THEN '201D'WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Branch
				WHEN C1.Account_Branch is NUll then C.Account_Branch
				
				ELSE C1.Account_Branch END as Delivery_DC,
	
	CASE	WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Number
				WHEN C1.Account_Branch is NUll then C.Account_Number
				ELSE C1.Account_Number END as Delivery_Account,
	
	CASE	WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Name
				WHEN C1.Account_Branch is NUll then C.Account_Name
				ELSE C1.Account_Name END as Delivery_Name,
	
	CASE	--	WHEN Product_Code in ('CUS0927W','LLO3350Q','AAH0081P','EME0088T','DEL0432N','TRI0866V','LOW0008F','LOW0009B', 'FIN0507E', 'FUE0001X')  THEN 'Charges'
			--	WHEN Umbrella_Group in ('6','8','12','13','14','15','16','17','18','19','22','23') THEN 'Charges'
			--	WHEN Full_Supplier IN ('DEP/009M','VES/005E') THEN 'Charges or Mail_Outs'
				WHEN bi.PAS = '1' and c.Customer_Group_ID  in ('890', '1050') THEN 'X-Dock'
				WHEN c.Account_Branch = '204J' and bi.Print_Branch = '305M' THEN 'X-Dock'
				WHEN c.Account_Branch != '406P' and bi.Print_Branch = '406P' THEN 'X-Dock'
				WHEN c.Account_Branch != '305M' and bi.Print_Branch = '305M' THEN 'X-Dock'
				WHEN c.Account_Branch != '408T' and bi.Print_Branch = '408T' THEN 'X-Dock'
				WHEN bi.Print_Branch = '606R' and bi.Company_ID = 'T' THEN 'X-Dock'		
				when bi.Print_Branch = '606R' and C.Account_Number like '0000%' then 'IBT'
				WHEN bi.Print_Branch = '606R' and bi.Company_ID != 'T' THEN 'X-Dock'
				WHEN C.Account_Branch != bi.Print_Branch THEN 'X-Dock' 
				WHEN V.Van_Depart_Time <= v.Van_Close_Time THEN 'Local'
				ELSE 'Local' END as Delivery_Type,

	CASE WHEN Warehouse_Zone = 'Spec Hand' and p.location_type in ('R','N') THEN 'Fridge'
			WHEN Warehouse_Zone = 'Spec Hand' and p.location_type in ('M') THEN 'Cyto'
			WHEN Warehouse_Zone = 'Spec Hand' and p.location_type in ('F') THEN 'Flamm'
			WHEN Warehouse_Zone = 'Spec Hand' and p.location_type in ('C') THEN 'CD'
			WHEN Order_Type_Detail = 'Autobulk' THEN 'Auto bulk'
			WHEN Warehouse_Zone is Null THEN 'Bulk'
		ELSE Warehouse_Area END as Warehouse_Zone,

	CASE WHEN V.Van_Depart_Time >= v.Van_Close_Time THEN 'PM Van'
				ELSE 'AM Van' END as AM_PM


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
  and datepart(WEEKDAY,(TRY_CONVERT(date,BI.Van_Day)))-1 = convert(int,cv.Day_No) 
  and bi.Van_Number = cv.Van_No AND cV.Destination_Branch IN ('504m','908b')
    
  Left join SupplyChainReporting.dbo.product_list p on bi.Prod_Key_C = p.Prod_Key_C
  left join SupplyChainReporting.dbo.product_list_extra pe on bi.Prod_Key_C = pe.Prod_Key_C 

  left join SupplyChainReporting.dbo.van_history v on (CASE	WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Branch
				WHEN C1.Account_Branch is NUll then C.Account_Branch
				ELSE C1.Account_Branch END) = v.Print_Branch and bi.Van_Number = v.Van_No and v.date = convert(DATE,bi.Processing_Date) --DATEADD(day,-2, getdate())


  left join SupplyChainReporting.dbo.order_type_marker_details ot on bi.Order_Type_Marker = ot.Order_Type_Marker and bi.Print_Branch =ot.Print_Branch

  left Join AAHOperations.dbo.LU_Warehouse_Zones wz on  left(bi.Location_ID,1) = Station and bi.Print_Branch = wz.Branch

  Left Join SupplyChainReporting.dbo.customer_type_description ctd on C.Customer_Type_ID = ctd.Customer_Type_Number 

--  Left Join SupplyChainReporting.dbo.b2b_invoices bi2 on bi.Cust_Key_C = bi2.Cust_Key_C and bi2.Original_Order_Number = bi.Invoice_Number and bi.Prod_Key_C = bi2.Prod_Key_C 

  WHERE bi.Process_Month = @Date
--					and bi.Document_Type = 'I'
					and bi.Omit_Marker ='0'
					and Product_Code   Not in ('CUS0927W','LLO3350Q','AAH0081P','EME0088T','DEL0432N','TRI0866V','LOW0008F','LOW0009B', 'FIN0507E', 'FUE0001X') 
					and Umbrella_Group  Not in ('6','8','12','13','14','15','16','17','18','19','22','23','24') 
					and Full_Supplier NOT IN ('DEP/009M','VES/005E')
					and bi.Print_Branch in ('101c','106m','201D' ,'204j' ,'305m','405n','406p' ,'501g' ,'502x','504m', '602j','703m' ,'706s','908b','606r','408t')
--					and TRY_convert(date,bi.Processing_Date) >= DATEADD(DAY,-2, getdate())
GROUP BY  bi.Print_Branch,
		bi.Process_Month,
		bi.Document_Type,
		bi.Credit_Note_Analysis_Marker,
		C.Account_Branch,
		c.Account_Number, 
		c.Account_Name,
		c.Customer_Type_ID,
		c.Customer_Group_ID,
		c.Customer_Channel_ID,
		Order_Type_Zone,
		bi.EOE_Flag,
		Order_Type_Detail,
		Product_Code,
		Product_Description,
		Pack_Size,
		Full_Supplier,
		bi.Reprint_Marker,
		p.Report_Group,
		p.Product_Group,
		p.Umbrella_Group,
		p.Location_Type,
		(CASE when C.Account_Number like '0000%' then 'IBT' ELSE bi.Company_ID end),
		(CASE	WHEN c.Account_Number in ('00000155S','00000133S') THEN '405N'
				WHEN c.Account_Number in ('00000157C','00000137J') THEN '501G'
				WHEN c.Account_Number in ('00000159J','00000139Q') THEN '106M'
				WHEN c.Account_Number in ('00000162E','00000142L') THEN '706S'
				WHEN c.Account_Number in ('00000163T','00000143D') THEN '504M'
				WHEN c.Account_Number in ('00000164L','00000144S') THEN '305M'
				WHEN c.Account_Number in ('00000166S','00000146C') THEN '703M'
				WHEN c.Account_Number in ('00000168C','00000153L') THEN '406P'
				WHEN c.Account_Number in ('00000141T') THEN '502X'
				WHEN c.Account_Number in ('00000140E','00000160U') THEN '201D'WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Branch
				WHEN C1.Account_Branch is NUll then C.Account_Branch
				
				ELSE C1.Account_Branch END),
		(CASE	WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Number
				WHEN C1.Account_Branch is NUll then C.Account_Number
				ELSE C1.Account_Number END),
		(CASE	WHEN C.Account_Branch = '606r' AND c2.Account_Branch is Not Null then c2.Account_Name
				WHEN C1.Account_Branch is NUll then C.Account_Name
				ELSE C1.Account_Name END),
		(	CASE	--	WHEN Product_Code in ('CUS0927W','LLO3350Q','AAH0081P','EME0088T','DEL0432N','TRI0866V','LOW0008F','LOW0009B', 'FIN0507E', 'FUE0001X')  THEN 'Charges'
			--	WHEN Umbrella_Group in ('6','8','12','13','14','15','16','17','18','19','22','23') THEN 'Charges'
			--	WHEN Full_Supplier IN ('DEP/009M','VES/005E') THEN 'Charges or Mail_Outs'
				WHEN bi.PAS = '1' and c.Customer_Group_ID  in ('890', '1050') THEN 'X-Dock'
				WHEN c.Account_Branch = '204J' and bi.Print_Branch = '305M' THEN 'X-Dock'
				WHEN c.Account_Branch != '406P' and bi.Print_Branch = '406P' THEN 'X-Dock'
				WHEN c.Account_Branch != '305M' and bi.Print_Branch = '305M' THEN 'X-Dock'
				WHEN c.Account_Branch != '408T' and bi.Print_Branch = '408T' THEN 'X-Dock'
				WHEN bi.Print_Branch = '606R' and bi.Company_ID = 'T' THEN 'X-Dock'		
				when bi.Print_Branch = '606R' and C.Account_Number like '0000%' then 'IBT'
				WHEN bi.Print_Branch = '606R' and bi.Company_ID != 'T' THEN 'X-Dock'
				WHEN C.Account_Branch != bi.Print_Branch THEN 'X-Dock' 
				WHEN V.Van_Depart_Time <= v.Van_Close_Time THEN 'Local'
				ELSE 'Local' END),

		(CASE WHEN Warehouse_Zone = 'Spec Hand' and p.location_type in ('R','N') THEN 'Fridge'
			WHEN Warehouse_Zone = 'Spec Hand' and p.location_type in ('M') THEN 'Cyto'
			WHEN Warehouse_Zone = 'Spec Hand' and p.location_type in ('F') THEN 'Flamm'
			WHEN Warehouse_Zone = 'Spec Hand' and p.location_type in ('C') THEN 'CD'
			WHEN Order_Type_Detail = 'Autobulk' THEN 'Auto bulk'
			WHEN Warehouse_Zone is Null THEN 'Bulk'
		ELSE Warehouse_Area END),
				(CASE WHEN V.Van_Depart_Time >= v.Van_Close_Time THEN 'PM Van'
				ELSE 'AM Van' END)
UNION
SELECT [Print_Branch]
	  ,[Process_Month]
	  ,'I' as Document_Type
	  ,'0' as Credit_Note_Analysis_Marker
      ,[Account_Branch] as Charge_DC
      ,[Account_No] as Charge_Account
	  , CASE	WHEN  (Account_No in ('80000101S','80141541J','80141509F','80141523A','80141574J') or Trunker_No = '911') THEN 'Aberdeen'
			WHEN  (Account_No in ('80000106B','80144540G','80144510F','80132568V','80144522U','80144504L') or Trunker_No = '916') THEN 'Glasgow'
			WHEN  (Account_No in ('80000201Y','80138543A','80138511P','80138515G','80138619Q','80138524W','80138525Y','80138622L') or Trunker_No = '921') THEN 'Leeds'
			WHEN  (Account_No in ('80000204N','80117540B','80117509M','80117511P','80117522P','80117571R')  or Trunker_No = '924') THEN 'Gateshead'
			WHEN  (Account_No in ('80000305B','80172559U','80172558F','80172498S','80172548U','80172550A','80172552H','80172546N','80172560X') or Trunker_No = '935') THEN 'Warrington'
			WHEN  (Account_No in ('80000405U','80112534K','80112509P','80112511S','80112566S') or Trunker_No = '945') THEN 'Belfast'
			WHEN  (Account_No in ('80000406M','84301069M','84301056F','84301057U','84301059E','84301061H','84301064G','84301067F','84301062W') or Trunker_No = '946') THEN 'Birmingham'
			WHEN  (Account_No in ('80000501C','80130575W','80130498S','80130523V','80130524N','80130121H','80130692M','80130509D','80130669B') or Trunker_No = '951') THEN 'Bristol'
			WHEN  (Account_No in ('80000502R','80131540T','80131504B','80131524R','80131534C') or Trunker_No = '952') THEN 'Paignton'
			WHEN  (Account_No in ('80000504B','80125540Y','80125579M','80125509C','80125525E','80125534U','80125523U','80125524M') or Trunker_No = '954') THEN 'Swansea'
			WHEN  (Account_No in ('80000602N','80128540D','80128527A','80128523J','80128534J') or Trunker_No = '962') THEN 'Bexhill'
			WHEN  (Account_No in ('80000703B','80121546T','80121582Y','80121513T','80121576U','80121527S','80121528K','80121578E','80121572G','80121535T')or Trunker_No = '973') THEN 'Ruislip'
			WHEN  (Account_No in ('80000706A','80052545A','82526574Q','80052532Q','80052544X','80052546P','80052555X')or Trunker_No = '976') THEN 'Romford'
			WHEN  (Account_No in ('80000908W','80025540M','80025530E','80025539B','80025549J') or Trunker_No = '998') THEN 'Southampton'
			WHEN  (Account_No in ('80068352Q','80025540M') or Trunker_No = '942') THEN 'Weedon'
			WHEN  (Account_No in ('80000014G','84217081U','84217075D','87314132B','80711205V','84217076S',
								'80235035J','84217078C','84830626D','84217080F','89001119D','83712112M','84217082M',
								'83627179U','82907091A','80102014Q','84217087S','83303726X','85103064J','82448208L','80000015V') or Trunker_No in ('966','968')) THEN 'Stoke'
			WHEN (Account_No in ('80119499E','80025540M','80119575Q','80119570K','80119550R','80119622T','80119546H',
								'80119562J','80119557H','80119563B','80119554X','80119625S','80119625S','80119577A','80119560C','80174079S') or Trunker_No = '948') THEN 'Tamworth'
		ELSE 'No Match' END as Charge_Acc_Name
	  ,'43' as Customer_Type_ID
	  ,'9999' as Customer_Group_ID
	  , '9999' as Customer_Channel_ID
	  ,'IBT' as Order_Type_Zone
	  ,'0' as EOE_Flag
	  ,'IBT' as Order_Type_Detail
      ,ibts.[Product_Code]
	  ,Product_Description
	  ,Pack_Size
	  ,Full_Supplier
	  ,Report_Group
	  ,Product_Group
	  ,Umbrella_Group
	  ,Location_Type
	  ,count(Line_Qty) as Lines
	  ,sum(Line_Qty) as Line_Qty
	  ,Sum(Line_Cost) as Sales_Value
	  ,'IBT' as Comp_ID
 	   ,CASE	WHEN  (Account_No in ('80000101S','80141541J','80141509F','80141523A','80141574J') or Trunker_No = '911') THEN '101C'
			WHEN  (Account_No in ('80000106B','80144540G','80144510F','80132568V','80144522U','80144504L') or Trunker_No = '916') THEN '106M'
			WHEN  (Account_No in ('80000201Y','80138543A','80138511P','80138515G','80138619Q','80138524W','80138525Y','80138622L') or Trunker_No = '921') THEN '201D'
			WHEN  (Account_No in ('80000204N','80117540B','80117509M','80117511P','80117522P','80117571R')  or Trunker_No = '924') THEN '204J'
			WHEN  (Account_No in ('80000305B','80172559U','80172558F','80172498S','80172548U','80172550A','80172552H','80172546N','80172560X') or Trunker_No = '935') THEN '305M'
			WHEN  (Account_No in ('80000405U','80112534K','80112509P','80112511S','80112566S') or Trunker_No = '945') THEN '405N'
			WHEN  (Account_No in ('80000406M','84301069M','84301056F','84301057U','84301059E','84301061H','84301064G','84301067F','84301062W') or Trunker_No = '946') THEN '406P'
			WHEN  (Account_No in ('80000501C','80130575W','80130498S','80130523V','80130524N','80130121H','80130692M','80130509D','80130669B') or Trunker_No = '951') THEN '501G'
			WHEN  (Account_No in ('80000502R','80131540T','80131504B','80131524R','80131534C') or Trunker_No = '952') THEN '502X'
			WHEN  (Account_No in ('80000504B','80125540Y','80125579M','80125509C','80125525E','80125534U','80125523U','80125524M') or Trunker_No = '954') THEN '504M'
			WHEN  (Account_No in ('80000602N','80128540D','80128527A','80128523J','80128534J') or Trunker_No = '962') THEN '602J'
			WHEN  (Account_No in ('80000703B','80121546T','80121582Y','80121513T','80121576U','80121527S','80121528K','80121578E','80121572G','80121535T')or Trunker_No = '973') THEN '703M'
			WHEN  (Account_No in ('80000706A','80052545A','82526574Q','80052532Q','80052544X','80052546P','80052555X')or Trunker_No = '976') THEN '706S'
			WHEN  (Account_No in ('80000908W','80025540M','80025530E','80025539B','80025549J') or Trunker_No = '998') THEN '908B'
			WHEN  (Account_No in ('80068352Q','80025540M') or Trunker_No = '942') THEN '402H'
			WHEN  (Account_No in ('80000014G','84217081U','84217075D','87314132B','80711205V','84217076S',
								'80235035J','84217078C','84830626D','84217080F','89001119D','83712112M','84217082M',
								'83627179U','82907091A','80102014Q','84217087S','83303726X','85103064J','82448208L','80000015V') or Trunker_No in ('966','968')) THEN '606R'
			WHEN (Account_No in ('80119499E','80025540M','80119575Q','80119570K','80119550R','80119622T','80119546H',
								'80119562J','80119557H','80119563B','80119554X','80119625S','80119625S','80119577A','80119560C','80174079S') or Trunker_No = '948') THEN '408T'
		ELSE 'No Match' END as Delivery_DC
	      ,Account_No as Delivery_Account

	   ,CASE	WHEN  (Account_No in ('80000101S','80141541J','80141509F','80141523A','80141574J') or Trunker_No = '911') THEN 'Aberdeen'
			WHEN  (Account_No in ('80000106B','80144540G','80144510F','80132568V','80144522U','80144504L') or Trunker_No = '916') THEN 'Glasgow'
			WHEN  (Account_No in ('80000201Y','80138543A','80138511P','80138515G','80138619Q','80138524W','80138525Y','80138622L') or Trunker_No = '921') THEN 'Leeds'
			WHEN  (Account_No in ('80000204N','80117540B','80117509M','80117511P','80117522P','80117571R')  or Trunker_No = '924') THEN 'Gateshead'
			WHEN  (Account_No in ('80000305B','80172559U','80172558F','80172498S','80172548U','80172550A','80172552H','80172546N','80172560X') or Trunker_No = '935') THEN 'Warrington'
			WHEN  (Account_No in ('80000405U','80112534K','80112509P','80112511S','80112566S') or Trunker_No = '945') THEN 'Belfast'
			WHEN  (Account_No in ('80000406M','84301069M','84301056F','84301057U','84301059E','84301061H','84301064G','84301067F','84301062W') or Trunker_No = '946') THEN 'Birmingham'
			WHEN  (Account_No in ('80000501C','80130575W','80130498S','80130523V','80130524N','80130121H','80130692M','80130509D','80130669B') or Trunker_No = '951') THEN 'Bristol'
			WHEN  (Account_No in ('80000502R','80131540T','80131504B','80131524R','80131534C') or Trunker_No = '952') THEN 'Paignton'
			WHEN  (Account_No in ('80000504B','80125540Y','80125579M','80125509C','80125525E','80125534U','80125523U','80125524M') or Trunker_No = '954') THEN 'Swansea'
			WHEN  (Account_No in ('80000602N','80128540D','80128527A','80128523J','80128534J') or Trunker_No = '962') THEN 'Bexhill'
			WHEN  (Account_No in ('80000703B','80121546T','80121582Y','80121513T','80121576U','80121527S','80121528K','80121578E','80121572G','80121535T')or Trunker_No = '973') THEN 'Ruislip'
			WHEN  (Account_No in ('80000706A','80052545A','82526574Q','80052532Q','80052544X','80052546P','80052555X')or Trunker_No = '976') THEN 'Romford'
			WHEN  (Account_No in ('80000908W','80025540M','80025530E','80025539B','80025549J') or Trunker_No = '998') THEN 'Southampton'
			WHEN  (Account_No in ('80068352Q','80025540M') or Trunker_No = '942') THEN 'Weedon'
			WHEN  (Account_No in ('80000014G','84217081U','84217075D','87314132B','80711205V','84217076S',
								'80235035J','84217078C','84830626D','84217080F','89001119D','83712112M','84217082M',
								'83627179U','82907091A','80102014Q','84217087S','83303726X','85103064J','82448208L','80000015V') or Trunker_No in ('966','968')) THEN 'Stoke'
			WHEN (Account_No in ('80119499E','80025540M','80119575Q','80119570K','80119550R','80119622T','80119546H',
								'80119562J','80119557H','80119563B','80119554X','80119625S','80119625S','80119577A','80119560C','80174079S') or Trunker_No = '948') THEN 'Tamworth'
		ELSE 'No Match' END as Delivery_Name
	,'IBT' as Delivery_Type
	,'IBT' as Warehouse_Zone
	,'IBT' as AM_PM
  FROM [SupplyChainReporting].[dbo].[aah_ibts] ibts
  Left join SupplyChainReporting.dbo.product_list p on ibts.Product_Code = p.Product_Code
  Where Print_Branch in ('408T','608V','101c','106m','201D' ,'204j' ,'305m','405n','406p' ,'501g' ,'502x' , '504m', '602j','703m' ,'706s','908b') 
  and Process_Month = @Date and Omit_Marker = '0' 
  GROUP BY
  [Print_Branch]
	,[Process_Month]
	,[Account_Branch]
	,[Account_No]
	,(CASE	WHEN  (Account_No in ('80000101S','80141541J','80141509F','80141523A','80141574J') or Trunker_No = '911') THEN 'Aberdeen'
			WHEN  (Account_No in ('80000106B','80144540G','80144510F','80132568V','80144522U','80144504L') or Trunker_No = '916') THEN 'Glasgow'
			WHEN  (Account_No in ('80000201Y','80138543A','80138511P','80138515G','80138619Q','80138524W','80138525Y','80138622L') or Trunker_No = '921') THEN 'Leeds'
			WHEN  (Account_No in ('80000204N','80117540B','80117509M','80117511P','80117522P','80117571R')  or Trunker_No = '924') THEN 'Gateshead'
			WHEN  (Account_No in ('80000305B','80172559U','80172558F','80172498S','80172548U','80172550A','80172552H','80172546N','80172560X') or Trunker_No = '935') THEN 'Warrington'
			WHEN  (Account_No in ('80000405U','80112534K','80112509P','80112511S','80112566S') or Trunker_No = '945') THEN 'Belfast'
			WHEN  (Account_No in ('80000406M','84301069M','84301056F','84301057U','84301059E','84301061H','84301064G','84301067F','84301062W') or Trunker_No = '946') THEN 'Birmingham'
			WHEN  (Account_No in ('80000501C','80130575W','80130498S','80130523V','80130524N','80130121H','80130692M','80130509D','80130669B') or Trunker_No = '951') THEN 'Bristol'
			WHEN  (Account_No in ('80000502R','80131540T','80131504B','80131524R','80131534C') or Trunker_No = '952') THEN 'Paignton'
			WHEN  (Account_No in ('80000504B','80125540Y','80125579M','80125509C','80125525E','80125534U','80125523U','80125524M') or Trunker_No = '954') THEN 'Swansea'
			WHEN  (Account_No in ('80000602N','80128540D','80128527A','80128523J','80128534J') or Trunker_No = '962') THEN 'Bexhill'
			WHEN  (Account_No in ('80000703B','80121546T','80121582Y','80121513T','80121576U','80121527S','80121528K','80121578E','80121572G','80121535T')or Trunker_No = '973') THEN 'Ruislip'
			WHEN  (Account_No in ('80000706A','80052545A','82526574Q','80052532Q','80052544X','80052546P','80052555X')or Trunker_No = '976') THEN 'Romford'
			WHEN  (Account_No in ('80000908W','80025540M','80025530E','80025539B','80025549J') or Trunker_No = '998') THEN 'Southampton'
			WHEN  (Account_No in ('80068352Q','80025540M') or Trunker_No = '942') THEN 'Weedon'
			WHEN  (Account_No in ('80000014G','84217081U','84217075D','87314132B','80711205V','84217076S',
								'80235035J','84217078C','84830626D','84217080F','89001119D','83712112M','84217082M',
								'83627179U','82907091A','80102014Q','84217087S','83303726X','85103064J','82448208L','80000015V') or Trunker_No in ('966','968')) THEN 'Stoke'
			WHEN (Account_No in ('80119499E','80025540M','80119575Q','80119570K','80119550R','80119622T','80119546H',
								'80119562J','80119557H','80119563B','80119554X','80119625S','80119625S','80119577A','80119560C','80174079S') or Trunker_No = '948') THEN 'Tamworth'
		ELSE 'No Match' END)
		,ibts.[Product_Code]
	  ,Product_Description
	  ,Pack_Size
	  ,Full_Supplier
	  ,Report_Group
	  ,Product_Group
	  ,Umbrella_Group
	  ,Location_Type
	  ,(CASE	WHEN  (Account_No in ('80000101S','80141541J','80141509F','80141523A','80141574J') or Trunker_No = '911') THEN '101C'
			WHEN  (Account_No in ('80000106B','80144540G','80144510F','80132568V','80144522U','80144504L') or Trunker_No = '916') THEN '106M'
			WHEN  (Account_No in ('80000201Y','80138543A','80138511P','80138515G','80138619Q','80138524W','80138525Y','80138622L') or Trunker_No = '921') THEN '201D'
			WHEN  (Account_No in ('80000204N','80117540B','80117509M','80117511P','80117522P','80117571R')  or Trunker_No = '924') THEN '204J'
			WHEN  (Account_No in ('80000305B','80172559U','80172558F','80172498S','80172548U','80172550A','80172552H','80172546N','80172560X') or Trunker_No = '935') THEN '305M'
			WHEN  (Account_No in ('80000405U','80112534K','80112509P','80112511S','80112566S') or Trunker_No = '945') THEN '405N'
			WHEN  (Account_No in ('80000406M','84301069M','84301056F','84301057U','84301059E','84301061H','84301064G','84301067F','84301062W') or Trunker_No = '946') THEN '406P'
			WHEN  (Account_No in ('80000501C','80130575W','80130498S','80130523V','80130524N','80130121H','80130692M','80130509D','80130669B') or Trunker_No = '951') THEN '501G'
			WHEN  (Account_No in ('80000502R','80131540T','80131504B','80131524R','80131534C') or Trunker_No = '952') THEN '502X'
			WHEN  (Account_No in ('80000504B','80125540Y','80125579M','80125509C','80125525E','80125534U','80125523U','80125524M') or Trunker_No = '954') THEN '504M'
			WHEN  (Account_No in ('80000602N','80128540D','80128527A','80128523J','80128534J') or Trunker_No = '962') THEN '602J'
			WHEN  (Account_No in ('80000703B','80121546T','80121582Y','80121513T','80121576U','80121527S','80121528K','80121578E','80121572G','80121535T')or Trunker_No = '973') THEN '703M'
			WHEN  (Account_No in ('80000706A','80052545A','82526574Q','80052532Q','80052544X','80052546P','80052555X')or Trunker_No = '976') THEN '706S'
			WHEN  (Account_No in ('80000908W','80025540M','80025530E','80025539B','80025549J') or Trunker_No = '998') THEN '908B'
			WHEN  (Account_No in ('80068352Q','80025540M') or Trunker_No = '942') THEN '402H'
			WHEN  (Account_No in ('80000014G','84217081U','84217075D','87314132B','80711205V','84217076S',
								'80235035J','84217078C','84830626D','84217080F','89001119D','83712112M','84217082M',
								'83627179U','82907091A','80102014Q','84217087S','83303726X','85103064J','82448208L','80000015V') or Trunker_No in ('966','968')) THEN '606R'
			WHEN (Account_No in ('80119499E','80025540M','80119575Q','80119570K','80119550R','80119622T','80119546H',
								'80119562J','80119557H','80119563B','80119554X','80119625S','80119625S','80119577A','80119560C','80174079S') or Trunker_No = '948') THEN '408T'
		ELSE 'No Match' END)
		,Account_No
		,(CASE	WHEN  (Account_No in ('80000101S','80141541J','80141509F','80141523A','80141574J') or Trunker_No = '911') THEN 'Aberdeen'
			WHEN  (Account_No in ('80000106B','80144540G','80144510F','80132568V','80144522U','80144504L') or Trunker_No = '916') THEN 'Glasgow'
			WHEN  (Account_No in ('80000201Y','80138543A','80138511P','80138515G','80138619Q','80138524W','80138525Y','80138622L') or Trunker_No = '921') THEN 'Leeds'
			WHEN  (Account_No in ('80000204N','80117540B','80117509M','80117511P','80117522P','80117571R')  or Trunker_No = '924') THEN 'Gateshead'
			WHEN  (Account_No in ('80000305B','80172559U','80172558F','80172498S','80172548U','80172550A','80172552H','80172546N','80172560X') or Trunker_No = '935') THEN 'Warrington'
			WHEN  (Account_No in ('80000405U','80112534K','80112509P','80112511S','80112566S') or Trunker_No = '945') THEN 'Belfast'
			WHEN  (Account_No in ('80000406M','84301069M','84301056F','84301057U','84301059E','84301061H','84301064G','84301067F','84301062W') or Trunker_No = '946') THEN 'Birmingham'
			WHEN  (Account_No in ('80000501C','80130575W','80130498S','80130523V','80130524N','80130121H','80130692M','80130509D','80130669B') or Trunker_No = '951') THEN 'Bristol'
			WHEN  (Account_No in ('80000502R','80131540T','80131504B','80131524R','80131534C') or Trunker_No = '952') THEN 'Paignton'
			WHEN  (Account_No in ('80000504B','80125540Y','80125579M','80125509C','80125525E','80125534U','80125523U','80125524M') or Trunker_No = '954') THEN 'Swansea'
			WHEN  (Account_No in ('80000602N','80128540D','80128527A','80128523J','80128534J') or Trunker_No = '962') THEN 'Bexhill'
			WHEN  (Account_No in ('80000703B','80121546T','80121582Y','80121513T','80121576U','80121527S','80121528K','80121578E','80121572G','80121535T')or Trunker_No = '973') THEN 'Ruislip'
			WHEN  (Account_No in ('80000706A','80052545A','82526574Q','80052532Q','80052544X','80052546P','80052555X')or Trunker_No = '976') THEN 'Romford'
			WHEN  (Account_No in ('80000908W','80025540M','80025530E','80025539B','80025549J') or Trunker_No = '998') THEN 'Southampton'
			WHEN  (Account_No in ('80068352Q','80025540M') or Trunker_No = '942') THEN 'Weedon'
			WHEN  (Account_No in ('80000014G','84217081U','84217075D','87314132B','80711205V','84217076S',
								'80235035J','84217078C','84830626D','84217080F','89001119D','83712112M','84217082M',
								'83627179U','82907091A','80102014Q','84217087S','83303726X','85103064J','82448208L','80000015V') or Trunker_No in ('966','968')) THEN 'Stoke'
			WHEN (Account_No in ('80119499E','80025540M','80119575Q','80119570K','80119550R','80119622T','80119546H',
								'80119562J','80119557H','80119563B','80119554X','80119625S','80119625S','80119577A','80119560C','80174079S') or Trunker_No = '948') THEN 'Tamworth'
		ELSE 'No Match' END)"))

##write.csv(Sales_Import_Line, file = "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/WorkSmart/OutBound_Line_Qty_202304.csv",
##          row.names = TRUE)

write.table(Sales_Import, file  = "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/WorkSmart/OutBound_Orders_Line_Qty_202607.txt"
            , sep = "|",quote = FALSE ,row.names = FALSE)

write.table(Sales_Import_Line, file  = "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/WorkSmart/OutBound_Line_Qty_202607.txt"
            , sep = '|',quote = FALSE ,row.names = FALSE)





#################



ThreePL = sqlQuery(con, paste("SELECT tm.Print_Branch
      ,Consignment_ID
      ,Pallet_ID
      ,tm.Account_Branch 
	  ,CASE WHEN tm.print_Branch = '807V' then 'Day_Lewis'
	    WHEN tm.print_Branch = '806T' then 'Tesco'
		WHEN tm.print_Branch = '805R' then 'Roshban'
		WHEN tm.print_Branch = '804P' then 'Cohens'
		ELSE 'Unknown'End as Customer_3PL
      ,convert(Char(9),Account) as Account
	  ,Pick_Note_Number
	  ,convert(DATE,Pick_Note_Date) as Pick_Note_Date
	  ,convert(Time,Pick_Note_Date) as Pick_Note_Time
  FROM [SupplyChainReporting].[dbo].[3pl_trunker_manifest] tm
  left join SupplyChainReporting.dbo.b2b_customers c on tm.Account = c.Account_Number and tm.Account_Branch = c.Account_Branch
 where Pick_Note_Date between '2025-04-01' and '2026-07-01' and tm.Print_Branch in ('807V','806T','805R','804P')"))

write.csv(ThreePL, file = "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/WorkSmart/ThreePL_Apr25_to_Jul_26.csv",
          row.names = FALSE)


WCS = sqlQuery(con, paste("/****** Script for SelectTopNRows command from SSMS  ******/
SELECT [Print_Branch]
      ,[Rec_Type]
	  ,Task_Code_Description
      ,datepart(month,Processing_Date) as Month
	  ,datepart(YEAR,Processing_Date) as Year
 --     ,[User_ID] as User_ID
	  ,Product_Code,Product_Description,Pack_Size,Full_Supplier
	  ,Count(Product_Code) as Lines
	  ,Sum(Actual_Quantity) as Qty
--		,CASE WHEN Start_Time ='0' THEN NULL ELSE convert(varchar,DATEADD(ms,Start_Time*1000,0),108) END as Start_Time
	--	,CASE WHEN End_Time = '0' THEN NULL ELSE convert(varchar,DATEADD(ms,End_Time*1000,0),108) END as End_Time
--		,convert(varchar,DATEADD(ms,Duration*1000,0),108) as Duration
--		,Duration as Duration_Seconds
 --     ,[SR_Number]
--      ,[Out_Of_Stock]
 --     ,[Controlled_Line]
--      ,[Expected_Quantity]
 --     ,[Actual_Quantity]
--      ,[Destination]
 --     ,[Licence_Plate]
--      ,[Expiry_Date]
 --     ,DESCRIPTION as Exception
--      ,[Expiry_Capture]
  --    ,[Expiry_Warning]
 --     ,wcs.[Location_Type]
 --     ,[GTIN]
 --     ,[Batch_Number]
   FROM [SupplyChainReporting].[dbo].[wcs_statistics] wcs
  left join SupplyChainReporting.dbo.product_list pl on wcs.Product_Key_C= pl.Prod_Key_C
  Left Join SupplyChainReporting.dbo.lu_wcs_task_code lu on wcs.Rec_Type = lu.Task_Code
  left Join SupplyChainReporting.dbo.lu_wcs_exceptions wcse on wcs.Exception_Flag = wcse.EXCEPTION_FLAG	
  where wcs.Print_Branch in ('101c','106m','201D' ,'204j' ,'305m','405n','406p' ,'501g' ,'502x','504m', '602j','703m' ,'706s','908b','606r','408t')
  and Processing_Date between '2025-04-01' and '2026-06-30'
  and Rec_Type in ('WPUT','WRST','WPRI','WGIP','WCHB','WOSR','WHQR','WRET','WCDP')
  GROUP BY 
  [Print_Branch]
      ,[Rec_Type]
	  ,datepart(month,Processing_Date)
	  ,datepart(YEAR,Processing_Date)
	  ,Task_Code_Description
	   ,Product_Code,Product_Description,Pack_Size,Full_Supplier--,DESCRIPTION"))


write.table(WCS, file  = "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/WorkSmart/WCS_Apr2025_to_June_2026.txt"
            , sep = '|',quote = FALSE ,row.names = FALSE,)
