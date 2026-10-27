########################
#######################

rm(list=ls())
gc()

setwd('C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/')

library(RODBC); library(data.table); library(lubridate); library(foreach); library(plyr); library(dplyr); library(matrixStats); library(stringr); library(hms); library(DBI); library(odbc);
library(stringi)


################### 
Shift_Pattern <- fread("C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Call Centre/Shift_Breakdown.txt")
#### timezoned
tz <- "UTC"
cutoff_date <- as.Date("2026-06-04")

############ Helper function to load + parse per file
load_and_parse <- function(file, comp_id) {
  
  dt <- fread(file)
  
  # Use file modified date for Stoke cutoff logic
  file_date <- as.Date(file.info(file)$mtime)
  
  # UTF-8 conversion
  dt[] <- lapply(dt, function(x) {
    if (is.character(x)) iconv(x, from = "", to = "UTF-8") else x
  })
  
  # Date format rule
  # Stoke: pre-cutoff = MDY, post-cutoff = DMY
  # AAH: always MDY
  if (comp_id == "ENT") {
    is_dmy <- file_date >= cutoff_date
  } else {
    is_dmy <- FALSE
  }
  
  # Apply correct datetime parsing
  if (is_dmy) {
    dt[, `Interval Start` := dmy_hm(`Interval Start`, tz = tz)]
    dt[, `Interval End`   := dmy_hm(`Interval End`,   tz = tz)]
    dt[, `Start Time`     := dmy_hms(`Start Time`,    tz = tz)]
    dt[, `End Time`       := dmy_hms(`End Time`,      tz = tz)]
  } else {
    dt[, `Interval Start` := mdy_hm(`Interval Start`, tz = tz)]
    dt[, `Interval End`   := mdy_hm(`Interval End`,   tz = tz)]
    dt[, `Start Time`     := mdy_hms(`Start Time`,    tz = tz)]
    dt[, `End Time`       := mdy_hms(`End Time`,      tz = tz)]
  }
  
  # Add metadata
  dt[, `:=`(
    Comp_ID = comp_id,
    File_Date = file_date,
    Date_Format = ifelse(is_dmy, "DMY", "MDY")
  )]
  
  return(dt)
}

############ Load Enterprise (Stoke)
path <- "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Call Centre/"

files_ent <- list.files(
  path = path,
  pattern = "^Daily Agent Performance Status Timeline Stoke.*\\.csv$",
  full.names = TRUE
)

Agent_Status_Timelines_Enterprise <- rbindlist(
  lapply(files_ent, load_and_parse, comp_id = "ENT"),
  use.names = TRUE,
  fill = TRUE
)

############ Load AAH
files_aah <- list.files(
  path = path,
  pattern = "^Daily Agent Performance Status Timeline AAH.*\\.csv$",
  full.names = TRUE
)

Agent_Status_Timelines_AAH <- rbindlist(
  lapply(files_aah, load_and_parse, comp_id = "AAH"),
  use.names = TRUE,
  fill = TRUE
)

######### Bind together
Agent_Status_Timelines <- rbindlist(
  list(Agent_Status_Timelines_Enterprise, Agent_Status_Timelines_AAH),
  use.names = TRUE,
  fill = TRUE
)

######## Remove Original Files
rm(Agent_Status_Timelines_AAH, Agent_Status_Timelines_Enterprise)

############ Duration fix
Agent_Status_Timelines[, Duration_secs :=
                         ifelse(trimws(Duration) == "" | is.na(Duration), 0, Duration / 1000)
]

################# Delete Not Needed Columns
Agent_Status_Timelines[, c("Filters", "Duration") := NULL]

################# Change Headings to include _
setnames(Agent_Status_Timelines, gsub(" ", "_", names(Agent_Status_Timelines)))





###########Import and convert to Data.table
# Define the folder path
path <- "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Call Centre/"
# List all matching files
file_list <- list.files(path = path, pattern = "^Log In Information Stoke.*\\.csv$", full.names = TRUE)
# Load each file into its own variable

Agent_Login_Enterprise <- rbindlist(lapply(file_list, fread), use.names = TRUE, fill = TRUE)

# Define the folder path
path <- "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Call Centre/"
# List all matching files
file_list <- list.files(path = path, pattern = "^Log In Information AAH.*\\.csv$", full.names = TRUE)
# Load each file into its own variable

Agent_Login_AAH <- rbindlist(lapply(file_list, fread), use.names = TRUE, fill = TRUE)

##Agent_Login_AAH <- fread("C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Call Centre/Log In Information AAH.csv")
##Agent_Login_Enterprise <- fread("C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Call Centre/Log In Information Stoke.csv")

###### Add a Column AAH / Ent
Agent_Login_Enterprise <- Agent_Login_Enterprise[,Comp_ID := 'ENT']
Agent_Login_AAH <- Agent_Login_AAH[,Comp_ID := 'AAH']

######### Bind together
Agent_Login <- rbind(Agent_Login_Enterprise,Agent_Login_AAH)

######## Remove Original Files
rm(Agent_Login_Enterprise,Agent_Login_AAH)

# Convert to POSIXct (date-time)
Agent_Login[, `Interval Start` := lubridate::dmy_hm(`Interval Start`, tz = tz)]
Agent_Login[, `Interval End`   := lubridate::dmy_hm(`Interval End`,   tz = tz)]
Agent_Login[, `Log In`         := lubridate::dmy_hm(`Log In`,      tz = tz)]
Agent_Login[, `Log Out`        := lubridate::dmy_hm(`Log Out`,     tz = tz)]
Agent_Login[, Duration_secs := fifelse(is.na(`Log In`), 0, as.numeric(`Log Out` - `Log In`, units = "secs"))]


################# Delete Not Need Columns
Agent_Login[,c("Filters") :=NULL]

################# Change Headings to include _
setnames(Agent_Login, gsub(" ", "_", names(Agent_Login)))

###########Import and convert to Data.table

###########Import and convert to Data.table
# Define the folder path
path <- "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Call Centre/"
# List all matching files
file_list <- list.files(path = path, pattern = "^Daily Agent Performance Information Stoke.*\\.csv$", full.names = TRUE)
# Load each file into its own variable

Agent_Performance_Enterprise <- rbindlist(lapply(file_list, fread), use.names = TRUE, fill = TRUE)

# Define the folder path
path <- "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Call Centre/"
# List all matching files
file_list <- list.files(path = path, pattern = "^Daily Agent Performance Information AAH.*\\.csv$", full.names = TRUE)
# Load each file into its own variable

Agent_Performance_AAH <- rbindlist(lapply(file_list, fread), use.names = TRUE, fill = TRUE)


##Agent_Performance_AAH <- fread("C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Call Centre/Daily Agent Performance Information AAH.csv")
##Agent_Performance_Enterprise <- fread("C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Call Centre/Daily Agent Performance Information Stoke.csv")

###### Add a Column AAH / Ent
Agent_Performance_Enterprise <- Agent_Performance_Enterprise[,Comp_ID := 'ENT']
Agent_Performance_AAH <- Agent_Performance_AAH[,Comp_ID := 'AAH']

######### Bind together
Agent_Performance <- rbind(Agent_Performance_AAH,Agent_Performance_Enterprise)
##Agent_Performance  <-Agent_Performance_Enterprise
######## Remove Original Files
rm(Agent_Performance_Enterprise,Agent_Performance_AAH)


# Convert to POSIXct (date-time)
Agent_Performance[, `Interval Start` := lubridate::dmy_hm(`Interval Start`, tz = tz)]
Agent_Performance[, `Interval End`   := lubridate::dmy_hm(`Interval End`,   tz = tz)]


#############Convert to Seconds
Agent_Performance[, Avg_Handle_secs := fifelse(is.na(`Avg Handle`),0,`Avg Handle`/1000)]
Agent_Performance[, Avg_Talk_secs := fifelse(is.na(`Avg Talk`),0,`Avg Talk`/1000)]
Agent_Performance[, Avg_Hold_secs := fifelse(is.na(`Avg Hold`),0,`Avg Hold`/1000)]
Agent_Performance[, Avg_ACW_secs := fifelse(is.na(`Avg ACW`),0,`Avg ACW`/1000)]
Agent_Performance[, Total_Handle_secs := fifelse(is.na(`Total Handle`),0,`Total Handle`/1000)]
Agent_Performance[, Total_Talk_secs := fifelse(is.na(`Total Talk`),0,`Total Talk`/1000)]
Agent_Performance[, Total_Hold_secs := fifelse(is.na(`Total Hold`),0,`Total Hold`/1000)]
Agent_Performance[, Total_ACW_secs := fifelse(is.na(`Total ACW`),0,`Total ACW`/1000)]
Agent_Performance[, Total_Dialing_secs := fifelse(is.na(`Total Dialing`),0,`Total Dialing`/1000)]
Agent_Performance[, Total_Contacting_secs := fifelse(is.na(`Total Contacting`),0,`Total Contacting`/1000)]
Agent_Performance[, Max_Talk_secs := fifelse(is.na(`Max Talk`),0,`Max Talk`/1000)]
Agent_Performance[, Max_ACW_secs := fifelse(is.na(`Max ACW`),0,`Max ACW`/1000)]


############## Remove all NA from num columns and replace with 0
num_cols <- names(Agent_Performance)[sapply(Agent_Performance, is.numeric)]
Agent_Performance[, (num_cols) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = num_cols]


################# Delete Not Need Columns
Agent_Performance[,c("Max Talk","Max ACW","Total Handle","Total Talk","Total Hold","Total ACW","Total Dialing","Total Contacting","Avg Talk","Avg Handle","Avg ACW","Avg Hold","Filters") :=NULL]

################# Change Headings to include _
setnames(Agent_Performance, gsub(" ", "_", names(Agent_Performance)))

################### 

# Define the folder path
path <- "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Call Centre/"
# List all matching files
file_list <- list.files(path = path, pattern = "^Daily Agent Performance Status Stoke.*\\.csv$", full.names = TRUE)
# Load each file into its own variable

Agent_Status_Daily_Enterprise <- rbindlist(lapply(file_list, fread), use.names = TRUE, fill = TRUE)

# Define the folder path
path <- "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Call Centre/"
# List all matching files
file_list <- list.files(path = path, pattern = "^Daily Agent Performance Status AAH.*\\.csv$", full.names = TRUE)
# Load each file into its own variable

Agent_Status_Daily_AAH <- rbindlist(lapply(file_list, fread), use.names = TRUE, fill = TRUE)



##Agent_Status_Daily_AAH <- fread("C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Call Centre/Daily Agent Performance Status AAH.csv")
##Agent_Status_Daily_Enterprise <- fread("C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Call Centre/Daily Agent Performance Status Stoke.csv")
###### Add a Column AAH / Ent
Agent_Status_Daily_Enterprise <- Agent_Status_Daily_Enterprise[,Comp_ID := 'ENT']
Agent_Status_Daily_AAH <- Agent_Status_Daily_AAH[,Comp_ID := 'AAH']

######### Bind together
Agent_Status_Daily <- rbind(Agent_Status_Daily_AAH,Agent_Status_Daily_Enterprise)

######## Remove Original Files
rm(Agent_Status_Daily_AAH,Agent_Status_Daily_Enterprise)

# Convert to POSIXct (date-time)
Agent_Status_Daily[, `Interval Start` := lubridate::dmy_hm(`Interval Start`, tz = tz)]
Agent_Status_Daily[, `Interval End`   := lubridate::dmy_hm(`Interval End`,   tz = tz)]

#############Convert to Seconds
Agent_Status_Daily[, Logged_In_secs := fifelse(is.na(`Logged In`),0,`Logged In`/1000)]
Agent_Status_Daily[, On_Queue_secs := fifelse(is.na(`On Queue`),0,`On Queue`/1000)]
Agent_Status_Daily[, Idle_secs := fifelse(is.na(`Idle`),0,`Idle`/1000)]
Agent_Status_Daily[, Not_Responding_secs := fifelse(is.na(`Not Responding`),0,`Not Responding`/1000)]
Agent_Status_Daily[, Off_Queue_secs := fifelse(is.na(`Off Queue`),0,`Off Queue`/1000)]
Agent_Status_Daily[, Busy_secs := fifelse(is.na(`Busy`),0,`Busy`/1000)]
Agent_Status_Daily[, Available_secs := fifelse(is.na(`Available`),0,`Available`/1000)]
Agent_Status_Daily[, Away_secs := fifelse(is.na(`Away`),0,`Away`/1000)]
Agent_Status_Daily[, Interacting_secs := fifelse(is.na(`Interacting`),0,`Interacting`/1000)]
Agent_Status_Daily[, Break_secs := fifelse(is.na(`Break`),0,`Break`/1000)]
Agent_Status_Daily[, Meal_secs := fifelse(is.na(`Meal`),0,`Meal`/1000)]
Agent_Status_Daily[, Meeting_secs := fifelse(is.na(`Meeting`),0,`Meeting`/1000)]
Agent_Status_Daily[, Training_secs := fifelse(is.na(`Training`),0,`Training`/1000)]

################# Delete Not Need Columns
Agent_Status_Daily[,c("Logged In","On Queue","Idle","Not Responding","Off Queue","Busy","Available","Away","Interacting","Break","Meal","Meeting","Training","Filters") :=NULL]

################# Change Headings to include _
setnames(Agent_Status_Daily, gsub(" ", "_", names(Agent_Status_Daily)))

###################

# Define the folder path
path <- "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Call Centre/"
# List all matching files
file_list <- list.files(path = path, pattern = "^Daily Call Interactions Stoke.*\\.csv$", full.names = TRUE)
# Load each file into its own variable

Agent_Interactions_Enterprise <- rbindlist(lapply(file_list, fread), use.names = TRUE, fill = TRUE)

# Define the folder path
path <- "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Call Centre/"
# List all matching files
file_list <- list.files(path = path, pattern = "^Daily Call Interactions AAH.*\\.csv$", full.names = TRUE)
# Load each file into its own variable

Agent_Interactions_AAH <- rbindlist(lapply(file_list, fread), use.names = TRUE, fill = TRUE)




##Agent_Interactions_AAH <- fread("C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Call Centre/Daily Call Interactions AAH.csv")
##Agent_Interactions_Enterprise <- fread("C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Call Centre/Daily Call Interactions Stoke.csv")
###### Add a Column AAH / Ent
Agent_Interactions_Enterprise <- Agent_Interactions_Enterprise[,Comp_ID := 'ENT']
Agent_Interactions_AAH <- Agent_Interactions_AAH[,Comp_ID := 'AAH']

######### Bind together
Agent_Interactions_Daily <- rbind(Agent_Interactions_AAH,Agent_Interactions_Enterprise)

######## Remove Original Files
rm(Agent_Interactions_AAH,Agent_Interactions_Enterprise)

###Remove the second user (keep only the first before the first ;)
Agent_Interactions_Daily[, Users := sub(";.*", "", Users)]

# If you want to also trim any leading/trailing whitespace:
Agent_Interactions_Daily[, Users := trimws(Users)]


# Convert to POSIXct (date-time)
Agent_Interactions_Daily[, `Date` := lubridate::dmy_hm(`Date`, tz = tz)]
Agent_Interactions_Daily[, Duration_secs := fifelse(is.na(`Duration`),0,`Duration`/1000)]
Agent_Interactions_Daily[, Total_Queue_secs := fifelse(is.na(`Total Queue`),0,`Total Queue`/1000)]
Agent_Interactions_Daily[, Total_Talk_secs := fifelse(is.na(`Total Talk`),0,`Total Talk`/1000)]
Agent_Interactions_Daily[, Total_ACW_secs := fifelse(is.na(`Total ACW`),0,`Total ACW`/1000)]
Agent_Interactions_Daily[, Total_Hold_secs := fifelse(is.na(`Total Hold`),0,`Total Hold`/1000)]
Agent_Interactions_Daily[, Total_Handle_secs := fifelse(is.na(`Total Handle`),0,`Total Handle`/1000)]

################# Delete Not Need Columns
Agent_Interactions_Daily[,c("Duration","Total Queue","Total Talk","Total Hold","Total Handle","Total ACW","Filters","Full Export Completed","Partial Result Timestamp") :=NULL]

################# Change Headings to include _
setnames(Agent_Interactions_Daily, gsub(" ", "_", names(Agent_Interactions_Daily)))


#############
################### 


# Define the folder path
path <- "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Call Centre/"
# List all matching files
file_list <- list.files(path = path, pattern = "^Daily Call Stats All Queues.*\\.csv$", full.names = TRUE)
# Load each file into its own variable

CC_Stats <- rbindlist(lapply(file_list, fread), use.names = TRUE, fill = TRUE)



##CC_Stats <- fread("C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Call Centre/Daily Call Stats All Queues.csv")

# Convert to POSIXct (date-time)
CC_Stats[, `Interval Start` := lubridate::dmy_hm(`Interval Start`, tz = tz)]
CC_Stats[, `Interval End`   := lubridate::dmy_hm(`Interval End`,   tz = tz)]


################# Change Headings to include _
setnames(CC_Stats, gsub(" ", "_", names(CC_Stats)))





###########################
################## Salesforce 
# Define the folder path
path <- "C:/Users/harriesr/OneDrive - Hallo Healthcare Group/R Coding/txt files/Call Centre/"
# List all matching files
file_list <- list.files(path = path, pattern = "^RH W.*\\.csv$", full.names = TRUE)
# Load each file into its own variable


# Read and convert each file to UTF-8
data_list <- lapply(file_list, function(file) {
  dt <- fread(file)
  # Convert all character columns to UTF-8
  dt[] <- lapply(dt, function(x) {
    if (is.character(x)) iconv(x, from = "", to = "UTF-8") else x
  })
  return(dt)
})

##Latin-1 "UTF-8"

CC_WhatsApp <- rbindlist(lapply(file_list, fread), use.names = TRUE, fill = TRUE)


CC_WhatsApp[] <- lapply(CC_WhatsApp, function(x) {
  if (is.character(x)) stri_trans_general(x, "Latin-ASCII") else x
})


##CC_WhatsApp <- fread(file_list[1])

# Convert to POSIXct (date-time)
CC_WhatsApp[, `Start Time` := lubridate::dmy_hm(`Start Time`, tz = tz)]
CC_WhatsApp[, `Created Date`   := lubridate::dmy(`Created Date`,   tz = tz)]
CC_WhatsApp[, `Accept Time` := lubridate::dmy_hm(`Accept Time`, tz = tz)]

################# Change Headings to include _
setnames(CC_WhatsApp, gsub(" ", "_", names(CC_WhatsApp)))

############## Remove all NA from num columns and replace with 0
num_cols <- names(CC_WhatsApp)[sapply(CC_WhatsApp, is.numeric)]
CC_WhatsApp[, (num_cols) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = num_cols]

############## Remove all Special Character and replace with a
##char_cols <- names(CC_WhatsApp)[sapply(CC_WhatsApp, is.character)]
##CC_WhatsApp[, (char_cols) := lapply(.SD, function(x) gsub("á", "a", x)), .SDcols = char_cols]



###################### e returns
# List all matching files
file_list <- list.files(path = path, pattern = "^RH E.*\\.csv$", full.names = TRUE)
# Load each file into its own variable
##E_Returns <- fread(file_list[1])
E_Returns <- rbindlist(lapply(file_list, fread), use.names = TRUE, fill = TRUE)

E_Returns[] <- lapply(E_Returns, function(x) {
  if (is.character(x)) stri_trans_general(x, "Latin-ASCII") else x
})

# Convert to POSIXct (date-time)
E_Returns[, `Date/Time Opened` := lubridate::dmy_hm(`Date/Time Opened`, tz = tz)]

############## Change Headings to include _
setnames(E_Returns, gsub(" ", "_", names(E_Returns)))
setnames(E_Returns,"Date/Time_Opened","DateTime_Opened")

############## Remove all NA from num columns and replace with 0
num_cols <- names(E_Returns)[sapply(E_Returns, is.numeric)]
E_Returns[, (num_cols) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = num_cols]
E_Returns[, Account_Branch := substr(Subject,1,4)]
E_Returns[, Account_Number := substr(Subject,5,13)]

############## Remove all Special Character and replace with a
##char_cols <- names(DT)[sapply(DT, is.character)]
##E_Returns[, (char_cols) := lapply(.SD, function(x) gsub("á", "a", x)), .SDcols = char_cols]


#######################Chat Bots
# List all matching files
file_list <- list.files(path = path, pattern = "^RH Chat.*\\.csv$", full.names = TRUE)
# Load each file into its own variable
#Chat_Bots <- fread(file_list[1])
Chat_Bots <- rbindlist(lapply(file_list, fread), use.names = TRUE, fill = TRUE)

Chat_Bots[] <- lapply(Chat_Bots, function(x) {
  if (is.character(x)) stri_trans_general(x, "Latin-ASCII") else x
})

# Convert to POSIXct (date-time)
Chat_Bots[, `Start Time` := lubridate::dmy_hm(`Start Time`, tz = tz)]
Chat_Bots[, `Created Date` := lubridate::dmy(`Created Date`, tz = tz)]

################# Delete Not Need Columns
Chat_Bots[,c("Body") :=NULL]

################# Change Headings to include _
setnames(Chat_Bots, gsub(" ", "_", names(Chat_Bots)))
setnames(Chat_Bots, gsub(":", "", names(Chat_Bots)))

############## Remove all NA from num columns and replace with 0
num_cols <- names(Chat_Bots)[sapply(Chat_Bots, is.numeric)]
Chat_Bots[, (num_cols) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = num_cols]

############## Remove all Special Character and replace with a
##char_cols <- names(Chat_Bots)[sapply(Chat_Bots, is.character)]
##Chat_Bots[, (char_cols) := lapply(.SD, function(x) gsub("á", "a", x)), .SDcols = char_cols]




###################### Complaints Cases
# List all matching files
file_list <- list.files(path = path, pattern = "^RH Comp.*\\.csv$", full.names = TRUE)
# Load each file into its own variable
#Complaint_Cases <- fread(file_list[1])
Complaint_Cases <- rbindlist(lapply(file_list, fread), use.names = TRUE, fill = TRUE)
Complaint_Cases[] <- lapply(Complaint_Cases, function(x) {
  if (is.character(x)) stri_trans_general(x, "Latin-ASCII") else x
})
# Convert to POSIXct (date-time)
Complaint_Cases[, `Date/Time Opened` := lubridate::dmy_hm(`Date/Time Opened`, tz = tz)]

################# Change Headings to include _
setnames(Complaint_Cases, gsub(" ", "_", names(Complaint_Cases)))
setnames(Complaint_Cases,c("Date/Time_Opened","AAH_Branch_No","Customer_Number","Customer_Name"),c("DateTime_Opened","Account_Branch","Account_Number","Account_Name"))

############## Remove all NA from num columns and replace with 0
num_cols <- names(Complaint_Cases)[sapply(Complaint_Cases, is.numeric)]
Complaint_Cases[, (num_cols) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = num_cols]

############## Remove all Special Character and replace with a
##char_cols <- names(Complaint_Cases)[sapply(Complaint_Cases, is.character)]
##Complaint_Cases[, (char_cols) := lapply(.SD, function(x) gsub("á", "a", x)), .SDcols = char_cols]


########################## Case Modified
# List all matching files
file_list <- list.files(path = path, pattern = "^RH Cases.*\\.csv$", full.names = TRUE)
# Load each file into its own variable
##Case_Modified <- fread(file_list[1])
Case_Modified <- rbindlist(lapply(file_list, fread), use.names = TRUE, fill = TRUE)
Case_Modified[] <- lapply(Case_Modified, function(x) {
  if (is.character(x)) stri_trans_general(x, "Latin-ASCII") else x
})
# Convert to POSIXct (date-time)
Case_Modified[, `Date/Time Opened` := lubridate::dmy_hm(`Date/Time Opened`, tz = tz)]
Case_Modified[, `Case Comment Last Modified Date` := lubridate::dmy_hm(`Case Comment Last Modified Date`, tz = tz)]

################# Change Headings to include _
setnames(Case_Modified, gsub(" ", "_", names(Case_Modified)))
setnames(Case_Modified,c("Date/Time_Opened","AAH_Branch_No","Customer_Number","Customer_Name"),c("DateTime_Opened","Account_Branch","Account_Number","Account_Name"))

############## Remove all NA from num columns and replace with 0
num_cols <- names(Case_Modified)[sapply(Case_Modified, is.numeric)]
Case_Modified[, (num_cols) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = num_cols]

############## Remove all Special Character and replace with a
##char_cols <- names(Case_Modified)[sapply(Case_Modified, is.character)]
##Case_Modified[, (char_cols) := lapply(.SD, function(x) gsub("á", "a", x)), .SDcols = char_cols]


########################### Quota
# List all matching files
file_list <- list.files(path = path, pattern = "^RH Quota.*\\.csv$", full.names = TRUE)

# Load each file into its own variable
#Quota <- fread(file_list[1])
Quota <- rbindlist(lapply(file_list, fread), use.names = TRUE, fill = TRUE)
Quota[] <- lapply(Quota, function(x) {
  if (is.character(x)) stri_trans_general(x, "Latin-ASCII") else x
})
# Convert to POSIXct (date-time)
Quota[, `Created Date` := lubridate::dmy(`Created Date`, tz = tz)]
Quota[, `Last Modified Date` := lubridate::dmy(`Last Modified Date`, tz = tz)]
Quota[, `Date/Time Completed` := lubridate::dmy_hm(`Date/Time Completed`, tz = tz)]

################# Change Headings to include _
setnames(Quota, gsub(" ", "_", names(Quota)))
setnames(Quota, gsub(":", "", names(Quota)))
setnames(Quota,c("Date/Time_Completed","Branch_Number","AAH_Account_Number"),c("DateTime_Completed","Account_Branch","Account_Number"))

############## Remove all NA from num columns and replace with 0
num_cols <- names(Quota)[sapply(Quota, is.numeric)]
Quota[, (num_cols) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = num_cols]

############## Remove all Special Character and replace with a
##char_cols <- names(Quota)[sapply(Quota, is.character)]
##Quota[, (char_cols) := lapply(.SD, function(x) gsub("á", "a", x)), .SDcols = char_cols]


###################### Contact us
# List all matching files
file_list <- list.files(path = path, pattern = "^RH Contact.*\\.csv$", full.names = TRUE,)
# Load each file into its own variable
##Contact_Us_Case <- fread(file_list[1],encoding = "Latin-1")

Contact_Us_Case <- rbindlist(
  lapply(file_list, function(f) fread(f, encoding = "Latin-1")),
  use.names = TRUE,
  fill = TRUE
)
Contact_Us_Case[] <- lapply(Contact_Us_Case, function(x) {
  if (is.character(x)) stri_trans_general(x, "Latin-ASCII") else x
})


# Convert to POSIXct (date-time)
Contact_Us_Case[, `Date/Time Opened` := lubridate::dmy_hm(`Date/Time Opened`, tz = tz)]
Contact_Us_Case[, `Date/Time Closed` := lubridate::dmy_hm(`Date/Time Closed`, tz = tz)]
Contact_Us_Case[, `Case Date/Time Last Modified` := lubridate::dmy_hm(`Case Date/Time Last Modified`, tz = tz)]

Contact_Us_Case[, Account_Branch := substr(Subject,1,4)]
Contact_Us_Case[, Account_Number := substr(Subject,5,13)]
Contact_Us_Case[,c("AAH Branch No","Customer Number") :=NULL]


################# Change Headings to include _
setnames(Contact_Us_Case, gsub(" ", "_", names(Contact_Us_Case)))
setnames(Contact_Us_Case, gsub("/", "", names(Contact_Us_Case)))


############## Remove all NA from num columns and replace with 0
num_cols <- names(Contact_Us_Case)[sapply(Contact_Us_Case, is.numeric)]
Contact_Us_Case[, (num_cols) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = num_cols]

############## Remove all Special Character and replace with a
##char_cols <- names(Contact_Us_Case)[sapply(Contact_Us_Case, is.character)]
##Contact_Us_Case[, (char_cols) := lapply(.SD, function(x) gsub("á", "a", x)), .SDcols = char_cols]
# ------------------------------------------------------------------
# Read database password from local text file
# ------------------------------------------------------------------

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
# Function to preserve displayed clock times as UTC
# ------------------------------------------------------------------

freeze_as_utc <- function(dt) {
  
  is_time <- vapply(
    dt,
    inherits,
    logical(1),
    "POSIXct"
  )
  
  cols <- names(dt)[is_time]
  
  if (length(cols)) {
    
    # force_tz keeps the displayed clock time and changes
    # the underlying timezone to UTC
    dt[
      ,
      (cols) := lapply(
        .SD,
        lubridate::force_tz,
        tzone = "UTC"
      ),
      .SDcols = cols
    ]
  }
  
  invisible(dt)
}

# ------------------------------------------------------------------
# Apply UTC timezone before writing to SQL Server
# ------------------------------------------------------------------

freeze_as_utc(Agent_Status_Timelines)
freeze_as_utc(Agent_Login)
freeze_as_utc(Agent_Performance)
freeze_as_utc(Agent_Status_Daily)
freeze_as_utc(Agent_Interactions_Daily)
freeze_as_utc(CC_Stats)
freeze_as_utc(CC_WhatsApp)
freeze_as_utc(E_Returns)
freeze_as_utc(Chat_Bots)
freeze_as_utc(Complaint_Cases)
freeze_as_utc(Case_Modified)
freeze_as_utc(Quota)
freeze_as_utc(Contact_Us_Case)

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
          name      = DBI::SQL("AAHOperations.dbo.CC_Agent_Status"),
          value     = Agent_Status_Daily,
          overwrite = TRUE,
          row.names = FALSE
        )
        
        DBI::dbWriteTable(
          con,
          name      = DBI::SQL("AAHOperations.dbo.CC_Agent_Status_Timelines"),
          value     = Agent_Status_Timelines,
          overwrite = TRUE,
          row.names = FALSE
        )
        
        DBI::dbWriteTable(
          con,
          name      = DBI::SQL("AAHOperations.dbo.CC_Agent_Performance"),
          value     = Agent_Performance,
          overwrite = TRUE,
          row.names = FALSE
        )
        
        DBI::dbWriteTable(
          con,
          name      = DBI::SQL("AAHOperations.dbo.CC_Agent_Login"),
          value     = Agent_Login,
          overwrite = TRUE,
          row.names = FALSE
        )
        
        DBI::dbWriteTable(
          con,
          name      = DBI::SQL("AAHOperations.dbo.CC_Agent_Interactions"),
          value     = Agent_Interactions_Daily,
          overwrite = TRUE,
          row.names = FALSE
        )
        
        DBI::dbWriteTable(
          con,
          name      = DBI::SQL("AAHOperations.dbo.CC_Call_Stats"),
          value     = CC_Stats,
          overwrite = TRUE,
          row.names = FALSE
        )
        
        DBI::dbWriteTable(
          con,
          name      = DBI::SQL("AAHOperations.dbo.CC_Whatsapp"),
          value     = CC_WhatsApp,
          overwrite = TRUE,
          row.names = FALSE
        )
        
        DBI::dbWriteTable(
          con,
          name      = DBI::SQL("AAHOperations.dbo.CC_eReturns"),
          value     = E_Returns,
          overwrite = TRUE,
          row.names = FALSE
        )
        
        DBI::dbWriteTable(
          con,
          name      = DBI::SQL("AAHOperations.dbo.CC_Chat_Bots"),
          value     = Chat_Bots,
          overwrite = TRUE,
          row.names = FALSE
        )
        
        DBI::dbWriteTable(
          con,
          name      = DBI::SQL("AAHOperations.dbo.CC_Complaint_Cases"),
          value     = Complaint_Cases,
          overwrite = TRUE,
          row.names = FALSE
        )
        
        DBI::dbWriteTable(
          con,
          name      = DBI::SQL("AAHOperations.dbo.CC_Modified_Cases"),
          value     = Case_Modified,
          overwrite = TRUE,
          row.names = FALSE
        )
        
        DBI::dbWriteTable(
          con,
          name      = DBI::SQL("AAHOperations.dbo.CC_Quota"),
          value     = Quota,
          overwrite = TRUE,
          row.names = FALSE
        )
        
        DBI::dbWriteTable(
          con,
          name      = DBI::SQL("AAHOperations.dbo.CC_Contact_Us_Case"),
          value     = Contact_Us_Case,
          overwrite = TRUE,
          row.names = FALSE
        )
        DBI::dbWriteTable(
          con,
          name      = DBI::SQL("AAHOperations.dbo.CC_Shift_Time_Groups"),
          value     = Shift_Pattern,
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







############################### new End Here




