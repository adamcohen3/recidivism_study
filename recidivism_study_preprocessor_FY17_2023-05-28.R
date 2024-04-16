#---------------------------------------------------------------------------------#
#recidivism_study_preprocessor_FY17_[YYYY-MM-DD].R
#forked from recidivism_2022-03-15.R on 2023/03/15 by Adam S. Cohen
#Last modified on 2023/05/03 by Adam S. Cohen
#---------------------------------------------------------------------------------#
#change log:
#1. add QA code to check for missing SIDs, charges, dispositions, etc in probation
#2. add QA code to check for bad SIDs and any discrepancies between HPA and DPS
#---------------------------------------------------------------------------------#
#clear workspace----
#---------------------------------------------------------------------------------#
rm(list = ls())

#---------------------------------------------------------------------------------#
#load libraries----
#---------------------------------------------------------------------------------#
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
#https://stackoverflow.com/questions/33026167/reading-sav-file-into-r
library(haven)
library(scales)
library(janitor)
library(report)
library(ggplot2)
library(lubridate)

#---------------------------------------------------------------------------------#
#set constants----
#---------------------------------------------------------------------------------#
path_to_recidivism_study <- "H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\FY17_recidivism_study\\"
filename_current_prob <- "CX_extract_FY17_newProb_2023-01-13_1112.xlsx" #everyone with a probation supervision start date in the FY
filename_prior_prob <- "CX_extract_FY17_priorProb_2023-01-13_1107.xlsx" #everyone with a probation supervision start date before the FY that terminated after the start of the FY
filename_DPS_sf_TS <- "DPS\\sf_TS_2017.csv"
filename_DPS_sf_PAR <- "DPS\\sf_PAR_2017.csv"
filename_DPS_parv_TS <- "DPS\\parv_TS_2017.csv"
filename_DPS_parv_PAR <- "DPS\\parv_PAR_2017.csv"
filename_CJIS <- ""
fy <- "2017"

#---------------------------------------------------------------------------------#
#import probation and DPS data----
#---------------------------------------------------------------------------------#
#FY17
cx_prob_current <- read_excel(paste0(path_to_recidivism_study, filename_current_prob)) %>%
  mutate(agency = "probation_current")
cx_prob_prior <- read_excel(paste0(path_to_recidivism_study, filename_prior_prob)) %>%
  mutate(agency = "probation_prior")
sf_TS <- read_csv(paste0(path_to_recidivism_study, filename_DPS_sf_TS)) %>%
  mutate(agency = "sf_TS")
sf_PAR <- read_csv(paste0(path_to_recidivism_study, filename_DPS_sf_PAR)) %>%
  mutate(agency = "sf_PAR")
parv_TS <- read_csv(paste0(path_to_recidivism_study, filename_DPS_parv_TS)) %>%
  mutate(agency = "parv_TS")
parv_PAR <- read_csv(paste0(path_to_recidivism_study, filename_DPS_parv_PAR)) %>%
  mutate(agency = "parv_PAR")

#----------------------------------------------------#
# import HPA data----
#----------------------------------------------------#
#the "_ac" version of HPA file manually fixes typos in the REL DATE and DOB columns
path <- paste0("H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\FY17_recidivism_study\\HPA\\RELEASES JUL 2016 - JUN 2017 (FY 2017)- revised2_ac",".xlsx")
sheets <- excel_sheets(path)
# hpaRaw1 <- lapply(sheets, function(X) read_excel(path, sheet = X, skip = 1, col_types = c('numeric','logical','text','numeric','logical','logical','numeric','text')))
hpa_raw1 <- lapply(sheets, function(X) read_excel(path, sheet = X, skip = 1))
hpa_raw2 <- lapply(hpa_raw1, as.data.frame)
names(hpa_raw2) <- sheets

#----------------------------------------------------#
# wrangle HPA data----
#----------------------------------------------------#
#FY16 HPA file is not properly formatted, remove extra blank cols with no data
hpa_raw2$AUG <- hpa_raw2$AUG[,1:8]
hpa_raw2$OCT <- hpa_raw2$OCT[,1:8]

#Reformat dates
#   the date format for JAN release date is a mix of numeric and text (formatting in the Excel file is all screwy)
#   need to convert a) numeric to date and b) text to numeric and then to date using Excel origin
#   use if_else inside lapply to get into a common format
#   use unlist to get back elements in list instead of a list in a list
#   unlist and dates don't play nicely together, converts everything back to a number, do convert back to dates, using R origin
#   h/t: https://stackoverflow.com/questions/15659783/why-does-unlist-kill-dates-in-r
hpa_raw2$JAN$`REL DATE` <- as.Date(unlist(lapply(hpa_raw2$JAN$`REL DATE`, 
                                                 function(x)if_else(nchar(x)==5,as.Date(as.numeric(x), origin = "1899-12-30"),as.Date(x, "%m/%d/%Y")))
),
origin = '1970-01-01')

#eliminate spacer rows and cols
hpa_raw3 <- bind_rows(hpa_raw2, .id = "id") %>%
  select(1,2,4,5,8,9) %>%
  filter(!is.na(SID)) %>%
  mutate(agency = "HPA")

rm(hpa_raw1, hpa_raw2)
#---------------------------------------------------------------------------------#
#QA: Tim's FY16 list----
#appears to be probation only based on nrows
#---------------------------------------------------------------------------------#
# tim_fy16 <- read_excel("H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\Tim_Wong_files\\from_tim_flash\\FY 2016 Recidivism Study\\FY 2016 Recidivism Data CJIS.xls")
# 
# tim_fy16_collapse <- tim_fy16 %>% 
#   group_by(SID) %>% 
#   summarize(n = n(), followup_dates = paste0(followup_date, collapse = ", ")) %>% 
#   ungroup()
# 
# #no duplicates, so Tim removed prior to creating this file
# #not clear if this list was created before or after request to CJIS

#_######### QA CHECK ######### ----
#---------------------------------------------------------------------------------#
#1. PROBATION (ACSB)----
#---------------------------------------------------------------------------------#
#Missing SIDs, charges, dispositions, etc in probation
#TO-DO 
#ACSB
#1. Organized based on PIN, docket #, and case supervision # -> do we need to include other variables to assist ACSB in data entry clean up?
      #a.	figure out how best organize so that staff can locate missing values (group by PIN and docket and case name
#2. should we filter out Termination = "Interstate Transfer"? Does that mean they transferred from somewhere else to HI or from HI to somewhere else?
#3. discuss with ACSB what non-probation Dispositions mean given these are supervision cases
#4.	ask ACSB why disposition and disposition date is sometimes missing
#5.	ask ACSB about entry of circuit and court on main case screen vs docket attribute vs
#Script improvements
#6. check for data entry error in start/termination dates (e.g., 2216-01-12)
#7. "The SID numbers that start with Z150 are Interstate Compact cases that Hawaii is providing courtesy supervision for another state where the offense occurred."
  #should we include or exclude? Tim seemed to include these...
#---------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------#
#QA: confirm PIN unique----
#---------------------------------------------------------------------------------#
# ce_pin <- cx_prob_current %>%
#   count(Pin)

ce_SID_DOB_Last <- cx_prob_current %>%
  count(SID, DOB, LastName) %>%
  nrow(.)
# same as ce_pin

#---------------------------------------------------------------------------------#
#QA: compare supervision start date to historical supervision start date----
#these are uncollapsed, so not that good for counting # of people or cases -> see code below for persons/cases
#---------------------------------------------------------------------------------#
cx_start_date_comparison<- cx_prob_current %>%
  # rename(ID=1) %>%
  mutate(offenseDegreeFactor = factor(OffenseDegree, levels = c('XX','VL','V','PM','MD','FC','FB','FA')), #set up as a factor in order to slice max charge
         # DOB = as.Date(DOB, format = '%m/%d/%Y'),
         # Main___Supervision_Start_Date = as.Date(Main___Supervision_Start_Date, format = '%m/%d/%Y'),
         # StartDate = as.Date(StartDate, format = '%m/%d/%Y')
         DOB2 = as.Date(as.numeric(DOB),origin = '1899-12-30'),
         SupervisionTermDate_AdHoc2 = as.Date(as.numeric(SupervisionTermDate_AdHoc),origin = '1899-12-30'),
         DispDate_AdHoc2 = as.Date(as.numeric(DispDate_AdHoc),origin = '1899-12-30'),
         ConvAdjudDate_AdHoc2 = as.Date(as.numeric(ConvAdjudDate_AdHoc),origin = '1899-12-30')) %>%
  select(OffenderId,SID_CX,SupervisionStartDate,SupervisionStartDate_AdHoc,HistoricalSupevisionStartDate) %>%
  mutate(equalCheck13 = if_else(SupervisionStartDate==HistoricalSupevisionStartDate,1,0),
         equalCheck12 = if_else(SupervisionStartDate==SupervisionStartDate_AdHoc,1,0))

#QA: compare SupervisionEndDate and SupervisionTermDate----
cx_end_date_comparison <- cx_prob_current %>%
  mutate(offenseDegreeFactor = factor(OffenseDegree, levels = c('XX','VL','V','PM','MD','FC','FB','FA')), #set up as a factor in order to slice max charge
         # DOB = as.Date(DOB, format = '%m/%d/%Y'),
         # Main___Supervision_Start_Date = as.Date(Main___Supervision_Start_Date, format = '%m/%d/%Y'),
         # StartDate = as.Date(StartDate, format = '%m/%d/%Y')
         DOB2 = as.Date(as.numeric(DOB),origin = '1899-12-30'),
         SupervisionTermDate_AdHoc2 = as.Date(as.numeric(SupervisionTermDate_AdHoc),origin = '1899-12-30'),
         DispDate_AdHoc2 = as.Date(as.numeric(DispDate_AdHoc),origin = '1899-12-30'),
         ConvAdjudDate_AdHoc2 = as.Date(as.numeric(ConvAdjudDate_AdHoc),origin = '1899-12-30')) %>%
  select(OffenderId,SID_CX,CaseNumber,SupervisionEndDate,SupervisionTermDate_AdHoc2) %>%
  mutate(equalCheck = if_else(SupervisionEndDate==SupervisionTermDate_AdHoc2,1,0))

#---------------------------------------------------------------------------------#
#___1a. Error Report for all variables excluding certain columns (see select below)----
#---------------------------------------------------------------------------------#
acsb_current_and_prior_error_report <- cx_prob_current %>%
  bind_rows(cx_prob_prior) %>% 
  select(-c(`Caseload Name`, SpecialGroups, Termination, TerminationDate, CloseDateTime, OffenseLevel, Disposition, DispositionDate)) %>% 
  #h/t: https://stackoverflow.com/questions/64448066/return-list-of-column-names-with-missing-na-data-for-each-row-of-a-data-frame
  mutate(across(c(where(is.character), -docCaseNumber, -supCaseNumber, -agency), ~ .x == "NULL")) %>%  # replace all NULL with TRUE and else FALSE
  # mutate(across(where(is.character), is.na)) %>%  # replace all NA with TRUE and else FALSE
  mutate(across(where(lubridate::is.POSIXct), is.na)) %>%  # replace all NA in date col with TRUE and else FALSE
  mutate(across(c(where(is.numeric), -Pin), is.na)) %>%  # replace all NA in numeric col with TRUE and else FALSE
  pivot_longer(-c(Pin, docCaseNumber, supCaseNumber, agency), names_to = "var") %>%  # pivot longer
  filter(value) %>%   # remove the FALSE rows
  distinct() %>%
  group_by(Pin, docCaseNumber, supCaseNumber, agency) %>%    # group by the ID
  summarise(`Missing Variables` = toString(var)) # convert the variable names to a string column

# write.csv(acsb_current_and_prior_error_report, 
#           paste0("H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\FY17_recidivism_study\\CX_extract_FY", fy, 
#                  "_ACSB_current_and_prior_data_correction_report_", format(Sys.time(), "%Y-%m-%d_%H%M"), ".csv"), 
#           row.names = FALSE)

# #---------------------------------------------------------------------------------#
#___1b. Error Report for missing SIDs only----
# #---------------------------------------------------------------------------------#
# acsb_current_and_prior_no_SID_report <- cx_prob_current %>%
#   bind_rows(cx_prob_prior) %>% 
#   filter(SID == "NULL") %>%
#   group_by(DOB, FirstName, LastName) %>%
#   slice_head(n = 1) %>%
#   ungroup()

# write.csv(acsb_current_and_prior_no_SID_report, 
#           paste0("H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\FY17_recidivism_study\\CX_extract_FY", fy, 
#                  "_ACSB_current_and_prior_no_SID_report", format(Sys.time(), "%Y-%m-%d_%H%M"), ".csv"), 
#           row.names = FALSE)

# #---------------------------------------------------------------------------------#
# #___1c: Error Report, second approach, for missing SID and charge data----
# #---------------------------------------------------------------------------------#
# ceFY17.CX.totalProb3 <- cx_prob_3
# ceFY17.CX.totalProb3.SID.NULL <- ceFY17.CX.totalProb3 %>%
#   filter(SID_CX == '' | is.null(SID_CX) | SID_CX == 'NULL') %>%
#   select(Pin,SID_CX,CaseNumber)
# #send list to ACSB for fixing
# write.csv(ceFY17.CX.totalProb3.SID.NULL,paste0('H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\recidivism_study_FY17_felony_SID_NULL_',format(Sys.time(),"%Y-%m-%d_%H%M"),'.csv'))
# ceFY17.CX.totalProb3.SID.NULL %>%
#   tally()
# #3
# 
# ceFY17.CX.totalProb1 <- totalProb1
# ceFY17.CX.totalProb3.Charge.NULL <- ceFY17.CX.totalProb1 %>%
#   filter(ChargeId == '' | is.null(ChargeId) | ChargeId == 'NULL') %>%
#   select(SID_CX,CaseNumber)
# #send list to ACSB for fixing
# write.csv(ceFY17.CX.totalProb3.Charge.NULL,paste0('H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\recidivism_study_FY17_felony_Charge_NULL_',format(Sys.time(),"%Y-%m-%d_%H%M"),'.csv'))
# ceFY17.CX.totalProb3.Charge.NULL%>%
#   tally()
# #44

#---------------------------------------------------------------------------------#
#2. PAROLE (HPA)----
#note that SIDs can appear multiple times b/c HPA includes all parolees released during FY
#do not remove during QA check; remove when building STUDY FINAL LIST
#after HPA fixes data in error report, send DPS-HPA non-overlap report (see QA below)
#---------------------------------------------------------------------------------#
hpa_error_report <- hpa_raw3 %>% 
  mutate(valid_sid = if_else(nchar(SID) == 8, "good", "bad"), #check for invalid SIDs based on length
         valid_rel_date = if_else(as.Date(paste0(as.numeric(fy) - 1, "-07-01"), "%Y-%m-%d") <= `REL DATE` &
                                    `REL DATE` <= as.Date(paste0(as.numeric(fy), "-06-30"), "%Y-%m-%d"),
                                  "good", "bad")) #check for invalid release dates, dates not within FY

# write.csv(hpa_error_report, 
#           paste0("H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\FY17_recidivism_study\\CX_extract_FY", fy, 
#                  "_HPA_data_correction_report_", format(Sys.time(), "%Y-%m-%d_%H%M"), ".csv"), 
#           row.names = FALSE)

#---------------------------------------------------------------------------------#
#3. MAX-OUT/MAX-TERM (DPS)----
#check for SIDs that appear on multiple sheets since they're supposed to be mutually exclusive
#after DPS fixes data in error report, send DPS-HPA non-overlap report (see QA below)
#---------------------------------------------------------------------------------#
sf_TS_sid <- sf_TS %>%
  distinct(sid) %>% #reduce duplicates
  select(SID = sid) %>%
  mutate(source_file = "sf_TS")
if(nrow(sf_TS) != nrow(distinct(sf_TS,sid))){
  sprintf("sf_TS: DUPLICATE SIDs")
}else 
    sprintf("sf_TS: no duplicates SIDs")

parv_TS_sid <- parv_TS %>%
  distinct(sid) %>% #reduce duplicates
  select(SID = sid) %>%
  mutate(source_file = "parv_TS")
if(nrow(parv_TS) != nrow(distinct(parv_TS,sid))){
  sprintf("parv_TS: DUPLICATE SIDs")
}else 
  sprintf("parv_TS: no duplicates SIDs")

sf_PAR_sid <- sf_PAR %>%
  distinct(sid) %>% #reduce duplicates
  select(SID = sid) %>%
  mutate(source_file = "sf_PAR")
if(nrow(sf_PAR) != nrow(distinct(sf_PAR,sid))){
  sprintf("sf_PAR: DUPLICATE SIDs")
}else 
  sprintf("sf_PAR: no duplicates SIDs")

parv_PAR_sid <- parv_PAR %>%
  distinct(sid) %>% #reduce duplicates
  select(SID = sid) %>%
  mutate(source_file = "parv_PAR")
if(nrow(parv_PAR) != nrow(distinct(parv_PAR,sid))){
  sprintf("parv_PAR: DUPLICATE SIDs")
}else 
  sprintf("parv_PAR: no duplicates SIDs")

dps_all_sid <- sf_TS_sid %>% 
  bind_rows(parv_TS_sid, sf_PAR_sid, parv_PAR_sid)
rm(sf_TS_sid, parv_TS_sid, sf_PAR_sid, parv_PAR_sid)

#2023/05/19: add code to check for offenses that are neither felony nor MD -> caught a few in FY17 that can't be right, need DPS to investigate
dps_error_report <- dps_all_sid %>%
  group_by(SID) %>%
  mutate(source_all = paste(source_file, collapse = ","),
         number_of_repeats = n()) %>% 
  ungroup() %>% 
  filter(source_file != source_all) %>% 
  arrange(SID) #leave in duplicate rows in case George wants to sort by source_file

# write.csv(dps_error_report, 
#           paste0("H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\FY17_recidivism_study\\CX_extract_FY", fy, 
#                  "_DPS_data_correction_report_", format(Sys.time(), "%Y-%m-%d_%H%M"), ".csv"), 
#           row.names = FALSE)

#----------------------------------------------------#
#4. HPA-DPS NON-OVERLAP REPORT----
#combine DPS parole (sf_PAR) and parole violators released to parole (parv_PAR), full join with HPA list
#run this after DPS and HPA get a chance to work on error reports
#----------------------------------------------------#
dps_parole_HPA <- sf_PAR %>%
  bind_rows(parv_PAR, .id = "id") %>%
  select(id, sid, dob, enddt, relto) %>%
  mutate(dob = as.Date(dob, '%m/%d/%Y'),
         enddt = as.Date(enddt, '%m/%d/%Y')) %>%
  full_join(hpa_raw3, by = c("sid" = "SID")) %>%
  arrange(id.x, sid)

ac_in_HPA_not_DPS <- dps_parole_HPA %>%
  filter(is.na(id.x))
# write.csv(ac_in_HPA_not_DPS, paste0("H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\FY17_recidivism_study\\DPS\\in_hpa_not_dps_",Sys.Date(),".csv"))
ac_in_DPS_not_HPA <- dps_parole_HPA %>%
  filter(is.na(id.y))
# write.csv(ac_in_DPS_not_HPA, paste0("H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\FY17_recidivism_study\\DPS\\in_dps_not_hpa_",Sys.Date(),".csv"))

#---------------------------------------------------------------------------------#
#_######### DF FOR RECIDIVISM STUDY ######### ----
#EXCLUSION CRITERIA
#     ACSB: remove DANC/DAGP, select highest charge/remove duplicates, remove non-felonies
#     HPA: Bad/Invalid SIDs; Release Dates not in FY; if SID repeats, select earliest release date in FY; notes from Hana for cases not in DPS list
#     DPS: released to another jurisdiction -> at least mark these for replication, remove for v2.0
#---------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------#
#1. PROBATION (ACSB)----
#two versions for probation - one filtered by prior prob and one that's not
#N.B. Tim did not remove probabtioners that were interstate -> considering removing for v2.0?
#---------------------------------------------------------------------------------#
#copied from recidivism_study_FY17_2023-04-21.R, section: Probation Cohort Analysis
#ICIS report method: 1) calculate total probationers, 2) calculate total felony probationers, and 3) calculate felony probationers - DANC/DAG
#problems with ICIS report method - need to remove DANCP/DAGP BEFORE removing non-felons, otherwise will drop felons with DANCP/DAGP on highest charge but no DA on lower felony charge
#AC report method: 0) calculate total probationers, 1) remove DANCP/DAGP, 2) select highest charge and remove duplicates, 3) remove non-felony
#---------------------------------------------------------------------------------#
#___v1.0: FY17 Analysis without Prior Probation Removed----
#___step 0 - count # of probationers----
cx_prob_current_clean <- cx_prob_current %>%
  # rename(ID=1) %>%
  mutate(offense_degree_factor = factor(OffenseDegree, levels = c('XX','VL','V','PM','MD','FC','FB','FA')), #set up as a factor in order to slice max charge
         TerminationDate = as.Date(as.numeric(TerminationDate),origin = '1899-12-30'),
         CloseDateTime = as.Date(as.numeric(CloseDateTime),origin = '1899-12-30'),
         DispositionDate = as.Date(as.numeric(DispositionDate),origin = '1899-12-30')) 

#______QA: count how many people are on probation (F + MD)----
#need to collapse cases/SIDs with multiple rows
#prob should use Pin or OffenderId b/c CaseId doesn't lump people w/multiple cases and SID has 111 rows = NULL
# countProb.SIDCases.collapsed <- cx_prob_current_clean %>%
#   count(SID,ChildCaseId)
# 
# countProb.SID.collapsed <- cx_prob_current_clean %>%
#   count(SID)
# 
# countProb.CaseId.collapsed <- cx_prob_current_clean %>%
#   count(ChildCaseId)
# 
# countProb.Pin.collapsed <- cx_prob_current_clean %>%
#   count(Pin)

# countProb.OffenderId.collapsed <- cx_prob_current_clean %>%
#   count(OffenderId)

#___step 1 - remove DAGP/DANCP----
#need to go back to uncollapsed b/c we want to leave in any rows without a DANCP or DAGP
cx_prob_1 <- cx_prob_current_clean %>%
  filter(!(Disposition %in% c("DAGP", "DANCP"))) #remove DANC/DAG based on notes
# filter(Disposition %in% c('Probation','Conditional Release')) %>%
# filter(ChargeType %in% c('Conviction')) %>%
# group_by(FullName) %>% #full names - alias problem; SSNs - not everyone lists a SSN; 
# group_by(Identifier___SID) %>% #full names - alias problem; SSNs - not everyone lists a SSN; SID - not everyone has an SID or temp SID

#2023/05/03: switched steps 2 and 3; to select the *earliest* felony offense in the FY, first filter for felonies, then select earliest offense, then select max offense if there are ties
            #previously, was getting max offense and removing duplicates, then filtering for only felonies, but this doesn't guarantee that offense was earliest felony
#___step 2 - remove non-felony----
cx_prob_2 <- cx_prob_1 %>%
  filter(offense_degree_factor %in% c("FA", "FB", "FC"))

#___step 3 - select earliest felony probation supervision; if ties, select highest charge if ties; if ties for both date and charge level, select one row----
cx_prob_3  <- cx_prob_2 %>%
  group_by(Pin) %>% #full names - alias problem; SSNs - not everyone lists a SSN; SID - not everyone has an SID or temp SID
  slice_min(HistoricalSupervisionStartDate) %>% #First, select earliest felony probation start date
  slice_max(offense_degree_factor, n = 1, with_ties = FALSE) %>% #break any ties by selecting highest charge/if charge level tie, select only one row
  ungroup() %>% 
  mutate(probation_type = "felony_probation")

#create list of SIDs with misdemeanor probation
#___step 0.MD - remove all SIDs for felony probation----
cx_md_prob_0 <- cx_prob_current_clean %>% 
  anti_join(cx_prob_3, by = c("SID", "DOB", "LastName"))

#___step 1.MD - remove DAGP/DANCP----
cx_md_prob_1 <- cx_md_prob_0 %>%
  filter(!(Disposition %in% c("DAGP", "DANCP"))) #remove DANC/DAG based on notes

#___step 2.MD - remove non-misdemeanor----
cx_md_prob_2 <- cx_md_prob_1 %>%
  filter(offense_degree_factor %in% c("MD", "PM"))

#___step 3.MD - select earliest felony probation supervision; if ties, select highest charge if ties; if ties for both date and charge level, select one row----
cx_md_prob_3  <- cx_md_prob_2 %>%
  group_by(Pin) %>% #full names - alias problem; SSNs - not everyone lists a SSN; SID - not everyone has an SID or temp SID
  slice_min(HistoricalSupervisionStartDate) %>% #First, select earliest felony probation start date
  slice_max(offense_degree_factor, n = 1, with_ties = FALSE) %>% #break any ties by selecting highest charge/if charge level tie, select only one row
  ungroup() %>% 
  mutate(probation_type = "misdemeanor_probation")

#___step 4.MD - check there is no overlap between felony probation and misdemeanor probation lists----
cx_prob_F_and_MD <- cx_prob_3 %>% 
  inner_join(cx_md_prob_3, by = c("SID", "DOB", "LastName")) %>% 
  nrow(.)

if(cx_prob_F_and_MD == 0){
  sprintf("Success: No overlap between felony and MD probation lists")
}else{
  sprintf("Error: Overlap between felony and probation lists")
}

#___step 5.MD - check who is on neither felony probation nor misdemeanor probation list----
cx_prob_F_or_MD <- cx_prob_3 %>% 
  bind_rows(cx_md_prob_3)
  
cx_prob_NOT_F_or_MD <- cx_prob_current_clean %>% 
  anti_join(cx_prob_F_or_MD, by = c("SID", "DOB", "LastName"))

cx_felony_nrows <- cx_prob_3 %>% 
  count(SID, DOB, LastName) %>% 
  nrow(.)

cx_misdem_nrows <- cx_md_prob_3 %>% 
  count(SID, DOB, LastName) %>% 
  nrow(.)

cx_remainder_nrows <- cx_prob_NOT_F_or_MD %>% 
  count(SID, DOB, LastName) %>% 
  nrow(.)

if((cx_felony_nrows + cx_misdem_nrows + cx_remainder_nrows) == ce_SID_DOB_Last){
  sprintf("Success: All SIDs/DOB-LastNames accounted for")
}else{
  sprintf("Error: Sum of felony, misdemeanor, and remainder does NOT equal unique SIDs/DOB-LastNames in cx_prob_current")
}

#___step 1.other - select earliest other probation supervision; if ties, select highest charge if ties; if ties for both date and charge level, select one row----
cx_prob_other_1 <- cx_prob_NOT_F_or_MD  %>% 
  group_by(Pin) %>% 
  slice_min(HistoricalSupervisionStartDate) %>% #First, select earliest felony probation start date
  slice_max(offense_degree_factor, n = 1, with_ties = FALSE) %>% #break any ties by selecting highest charge/if charge level tie, select only one row
  ungroup()

#___step 2.other - count DAG/DANC and non-felony/non-MD probationers----
cx_prob_other_DA_nrows <- cx_prob_other_1 %>% 
  filter(Disposition %in% c("DAGP","DANCP")) %>% 
  nrow(.)

cx_prob_other_other_nrows <- cx_prob_other_1 %>% 
  filter(Disposition %in% c("NULL","Probation", "Restitution-Monitoring")) %>% 
  nrow(.)

#___step 3.other - merge counts of MD, DA, and other----
probation_stats <- c(cx_misdem_nrows, cx_prob_other_DA_nrows, cx_prob_other_other_nrows)
names(probation_stats) <- c("MD", "DA", "Other")

#use matching file naming with other agencys
master_acsb_jr <- cx_prob_F_or_MD

# #______QA: compare historical and regular supervision start dates----
# ceStartDateComparison <- cx_prob_3 %>%
#   select(Pin,SID,`Supervision Start Date`, HistoricalSupervisionStartDate) %>%
#   mutate(equalCheck13 = if_else(`Supervision Start Date` == HistoricalSupervisionStartDate, 1, 0))
# 
# ceStartDateComparison %>%
#   filter(equalCheck13==0) %>%
#   tally()
# #22 DO NOT MATCH

#---------------------------------------------------------------------------------#
#___v2.0: FY17 Analysis with Prior Probation Removed----
#---------------------------------------------------------------------------------#
#QUESTIONS: do we want to include/exclude those with DANCP/DAGP, MD or lower offenses?
#---------------------------------------------------------------------------------#
# #___step 1: compute max term date (some people have multiple prior cases)----
# cePriorProbList2 <- ce_prob_prior %>%
#   mutate(SupervisionTermDate_AdHoc2 = as.Date(as.numeric(SupervisionTermDate_AdHoc), origin = "1899-12-30"),
#          SupervisionTermDate_AdHoc3 = if_else(is.na(SupervisionTermDate_AdHoc2),Sys.Date(),SupervisionTermDate_AdHoc2)) %>%
#   group_by(SID_CX) %>%
#   summarize(termDateMax = max(SupervisionTermDate_AdHoc3), HistoricalSupevisionStartDate2 = first(HistoricalSupevisionStartDate)) %>%
#   ungroup()
# 
# #___step 2: merge current and prior prob----
# totalProb3.noPriorProb <- totalProb3 %>%
#   left_join(cePriorProbList2, by = "SID_CX") %>%
#   mutate(ongoingSupv = if_else(termDateMax > HistoricalSupevisionStartDate, "yes","no"))
# 
# totalProb3.noPriorProb %>%
#   count(ongoingSupv)
# 
# #___step 3: count how many in newProb have a priorProb----
# new_prob_minus_old_prob <- ceCJIS %>% 
#   anti_join(cePriorProbList, by = c("SID","DOB","LastName"))

#---------------------------------------------------------------------------------#
#2. PAROLE (HPA)----
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#___FY2017 Special clean up of HPA----
#remove bad SIDs, bad release dates, and SIDs miscategorized as parole according to Hana's notes
#N.B. Tim did not remove parolees released to other jurisdictions -> remove for v2.0
#---------------------------------------------------------------------------------#
master_hpa_jr <- hpa_error_report %>% 
  #clean release dates since they are clearly typos: 2106 -> 2016
  mutate(`REL DATE` = if_else(valid_rel_date == "bad", as.Date(paste0("2016-", format(`REL DATE`, "%m-%d"))), `REL DATE`)) %>% 
  filter(valid_sid == "good", valid_rel_date == "good") %>% 
  filter(!SID %in% c("A0265817", #based on input from George (DPS) and Hana (HPA), these were all max-outs, not parolees
                     "A6007286",
                     "A6024242",
                     "A1013142",
                     "A1045896")) %>% 
  group_by(SID) %>% 
  slice_min(`REL DATE`, n = 1, with_ties = FALSE) %>% 
  ungroup()

#---------------------------------------------------------------------------------#
#3. MAX-OUT/MAX-TERM (DPS)----
#N.B. Tim did not remove max-outs released to other jurisdictions -> remove for v2.0
#---------------------------------------------------------------------------------#
master_dps_jr <- sf_TS %>% 
  bind_rows(parv_TS) %>% 
  mutate()

#---------------------------------------------------------------------------------#
#4. CROSS-AGENCY DUPLICATE LIST----
#run this after DPS and HPA get a chance to work on error reports and non-overlap report
#---------------------------------------------------------------------------------#
#pull out SIDs from each agency list
sid_prob <- master_acsb_jr %>%
  distinct(SID, DOB, LastName, .keep_all = TRUE) %>% 
  select(SID, source_file = probation_type)# %>% #updated this so that I can see in confusion report if duplicate SID comes from felony of MD probation
# mutate(source_file = "probation")

#confirm unique SIDs within each agency list
if(nrow(master_acsb_jr) != nrow(distinct(master_acsb_jr, SID, DOB, LastName))){
  duplicates <- master_acsb_jr %>% 
    count(SID, DOB, LastName) %>% 
    filter(n > 1)
  cat(sprintf("master_acsb_jr\n"),
      sprintf("DUPLICATE SID: %s\n", duplicates$SID))
}else 
  sprintf("master_acsb_jr: no duplicates SIDs")

sid_hpa <- master_hpa_jr %>%
  distinct(SID) %>% 
  select(SID) %>%
  mutate(source_file = "hpa")

if(nrow(master_hpa_jr) != nrow(distinct(master_hpa_jr,SID))){
  duplicates <- master_hpa_jr %>% 
    count(SID) %>% 
    filter(n > 1)
  cat(sprintf("master_hpa_jr\n"),
      sprintf("DUPLICATE SID: %s\n", master_hpa_jr$SID))
}else 
  sprintf("master_hpa_jr: no duplicates SIDs")

sid_dps <- master_dps_jr %>% 
  distinct(sid, .keep_all = TRUE) %>% 
  select(SID = sid, source_file = agency)# %>% #updated this so that I can see in confusion report which DPS file duplicate SID comes from
  # mutate(source_file = "dps")

if(nrow(master_dps_jr) != nrow(distinct(master_dps_jr,sid))){
  duplicates <- master_dps_jr %>% 
    count(sid) %>% 
    filter(n > 1)
  sprintf("master_dps_jr: DUPLICATE SIDs")
  cat(sprintf("master_dps_jr\n"),
      sprintf("DUPLICATE SID: %s\n", duplicates$sid))
}else 
  sprintf("master_dps_jr: no duplicates SIDs")

#determine which SIDs appear on multiple lists
cross_agency_confusion_report <- sid_prob %>% 
  bind_rows(sid_hpa, sid_dps) %>%
  group_by(SID) %>%
  mutate(source_all = paste(source_file, collapse = ","),
         number_of_repeats = n()) %>% 
  ungroup() %>% 
  filter(source_file != source_all) %>% 
  arrange(SID) #leave in duplicate rows, easier to sort by source_file

#same as above but with duplicates removed
cross_agency_confusion_report_distinct <- cross_agency_confusion_report %>% 
  distinct(SID, source_all, number_of_repeats)

#use cross_agency_confusion_report and master_[agency]_jr lists to compare release dates
compare_release_dates_acsb <- cross_agency_confusion_report %>%
  filter(grepl("probation", source_file), SID != "NULL") %>% 
  left_join(master_acsb_jr) %>% 
  select(SID, source_file, source_all, followup_date = HistoricalSupervisionStartDate)

compare_release_dates_hpa <- cross_agency_confusion_report %>% 
  filter(source_file == "hpa") %>% 
  left_join(master_hpa_jr) %>% 
  select(SID, source_file, source_all, followup_date = `REL DATE`)

compare_release_dates_sf_TS <- cross_agency_confusion_report %>% 
  filter(source_file == "sf_TS") %>% 
  left_join(master_dps_jr, by = c("SID"="sid")) %>% 
  mutate(enddt = as.Date(enddt, "%m/%d/%Y")) %>% 
  select(SID, source_file, source_all, followup_date = enddt)

compare_release_dates_parv_TS <- cross_agency_confusion_report %>% 
  filter(source_file == "parv_TS") %>% 
  left_join(master_dps_jr, by = c("SID"="sid")) %>% 
  mutate(enddt = as.Date(enddt, "%m/%d/%Y")) %>% 
  select(SID, source_file, source_all, followup_date = enddt)

#this table adjudicates between SIDs appearing on multiple agency lists; use toa assign any non-unique SIDs to one and only one list
unconfusion_table_FY17 <- compare_release_dates_acsb %>% 
  bind_rows(compare_release_dates_hpa, compare_release_dates_sf_TS, compare_release_dates_parv_TS)
#unconfusion_table_FY17 confirms that for all 10 HPA/DPS combos, HPA release precedes DPS, suggesting parolee was revoked and then later released from prison -> use HPA, remove from DPS
#unconfusion_table_FY17 confirms that for all 3 DPS/ACSB-felony combos, DPS release precedes probation, suggesting prisoner was released and then began probation supervision -> use ACSB, remove from DPS
#unconfusion_table_FY17 confirms that for all 4 DPS/ACSB-MD combos, DPS release precedes probation, suggesting prisoner was released and then began probation supervision -> use ACSB, remove from DPS

#---------------------------------------------------------------------------------#
#5. JOIN AGENCY MASTER LISTS----
#---------------------------------------------------------------------------------#
#create master_[agency]_sr lists and join to create master dataframe for recidivism study
master_acsb_sr <- master_acsb_jr %>% 
  separate_wider_delim(cols = OffenseCode, delim = "-", names = c("offense_chapter_tmp","offense_section"),
                       too_many = "merge", cols_remove = FALSE) %>%
  # separate_wider_regex(cols = OffenseCode, patterns =  c("(?:HRS )?", offense_chapter = "\\w", ":-", 
  #                                                        offense_section = "\\w"), 
  #                      cols_remove = FALSE) %>%
  mutate(offense_chapter = gsub("HRS ", "", offense_chapter_tmp),
         county = case_when(grepl("1", CircuitCourt) ~ "Honolulu",
                            grepl("2", CircuitCourt) ~ "Maui",
                            grepl("3", CircuitCourt) ~ "Hawaii",
                            grepl("5", CircuitCourt) ~ "Kauai",
                            TRUE ~ "Unknown"),
         agency = "Probationers") %>% 
  select(agency, SID, DOB, LastName, FirstName, followup_date = HistoricalSupervisionStartDate,
         initial_supervision_level = InitialSupervisionLevel, most_recent_supervision_level = SupervisionLevel,
         county, offense_chapter, offense_section, offense_description = OffenseDescription, 
         offense_degree = OffenseDegree, probation_type)

master_hpa_sr <- master_hpa_jr %>% 
  mutate(county = case_when(LOC %in% c("Honolulu", "Kaneohe", "Kapolei", "Waianae") ~ "Honolulu",
                            LOC %in% c("Maui") ~ "Maui",
                            LOC %in% c("Hawaii", "Kona") ~ "Hawaii",
                            LOC %in% c("Kauai") ~ "Kauai",
                            LOC %in% c(state.name[-11], "Philadelphia") ~ "Other",
                            TRUE ~ "Uknown"
                            ),
         agency = "Parolees") %>% 
  select(agency, SID, DOB, followup_date = `REL DATE`, county)

#filter SIDs based on unconfusion_table_FY17
master_dps_sr <- master_dps_jr %>% 
  separate_wider_delim(cols = charge, delim = "-", names = c("offense_chapter","offense_section"),
                       too_few = "align_start", too_many = "merge", cols_remove = FALSE) %>% 
  mutate(dob2 = as.Date(dob, "%m/%d/%Y"),
         enddt2 = as.Date(enddt, "%m/%d/%Y"),
         county = case_when(lastfac %in% c("OCCC", "HCF", "WCF", "WCCC") ~ "Honolulu",
                            lastfac %in% c("MCCC") ~ "Maui",
                            lastfac %in% c("HCCC", "KCF") ~ "Hawaii",
                            lastfac %in% c("KCCC") ~ "Kauai",
                            lastfac %in% c("OOS-ZFEDS", "AZSC") ~ "Other",
                            TRUE ~ "Unknown"),
         agency = "MTR-Prisoners") %>% 
  filter(!sid %in% c("A0108650", "A0143521", "A0187024", "A0195268", "A0259656", "A0708795",
                     "A0760725", "A1039556", "A1040874", "A1065183", "A5007719", "A6010677", "A6095412")) %>% 
  filter(!sid %in% c("A0253181", "A1042957", "A1057945", "A1058556")) %>% 
  select(agency, SID = sid, DOB = dob2, followup_date = enddt2, county, offense_chapter, offense_section, offense_description = descrip, 
         offense_degree = severity, laststat)

master_df_1 <- master_acsb_sr %>% 
  bind_rows(master_hpa_sr, master_dps_sr)

#---------------------------------------------------------------------------------#
#6. RECODE VARIABLES----
#---------------------------------------------------------------------------------#
#___age and age range----
master_df_2 <- master_df_1  %>% 
  mutate(age = interval(DOB,followup_date)/years(1),
         age_range = case_when(age < 20 ~ "< 20 years old",
                               20 <= age & age < 30 ~ "20-29 years old",
                               30 <= age & age < 40 ~ "30-39 years old",
                               40 <= age & age < 50 ~ "40-49 years old",
                               50 <= age & age < 60 ~ "50-59 years old",
                               60 <= age ~ "+60 years old"
                               ))
  
#___initial offense type----
#QA: first, examine Tim's FY16 codings
master_fy16 <-  read_sav("H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\Tim_Wong_files\\from_Erin_Tim_Wong_files\\Master Recidivism Study_rev FY 2016.sav")
master_fy16_offense_type <- master_fy16 %>% 
  mutate(MRrecoded_offtype = as_factor(MRrecoded_offtype),
         MRrecoded_offtype_recode = if_else(!is.na(MRrecoded_offtype), 1, 0),
         Agency_rc = as_factor(Agency_rc)) 
master_fy16_offense_type_counter <-  master_fy16_offense_type %>% 
  count(Agency_rc, MRrecoded_offtype_recode) %>% 
  group_by(Agency_rc) %>% 
  mutate(agency_total = sum(n)) %>% 
  ungroup() %>% 
  mutate(pct = round_half_up(100*n/agency_total, digits = 1))
#~65% of prob, 70% of parolees, and 40% of MTR-prisoners have initial offense code

#compare codes in master file to robbery initial offense in probation FY16 sample
prob_FY16 <- read_sav("H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\Tim_Wong_files\\from_Erin_Tim_Wong_files\\probation_reciv_study_FY2016_unup.sav")
prob_FY16_robbery <- prob_FY16 %>% 
  filter(grepl("robbery", OffenseDescription, ignore.case = TRUE))
master_fy16_offense_type_robbery <- prob_FY16_robbery %>% 
  left_join(master_fy16_offense_type, by = "SID") %>% 
  select(SID, OffenseDescription.x, OffenseDegree_rc.x, MRrecoded_offtype)

#Method A: recode initial offense type according to HRS
# https://law.justia.com/codes/hawaii/2022/title-37/
# Person is under Chapter 707 and is broken down into non-sex and sex offenses 
#     sex offenses are under Part 5 of Chapter 707). 
#     Although there may be some sex offenses that fall under chapter 712 (e.g., in Parts I and II).  
# Property is Chapter 708. 
# Drug offenses are under Part IV of Chapter 712.
# for reference, categories from DOJ BJS and NCSC: 
#       DOJ/BJS: https://www.bjs.gov/index.cfm/dataonline/index.cfm?ty=tdtp&tid=2
#       NCSC CSP: state-court-guide-to-statistical-reporting.pdf  
master_df_3 <- master_df_2 %>%
  mutate(initial_offense_category = case_when(offense_degree %in% c("MD", "PM") ~ "Misdemeanor and other",
                                              grepl("707", offense_chapter) & grepl("7[01256][0-9]", offense_section) ~ "Non-sex Violent Offenses",
                                              grepl("708", offense_chapter) & grepl("84[0-2]", offense_section) ~ "Non-sex Violent Offenses", #robbery
                                              grepl("709", offense_chapter) & grepl("906", offense_section) ~ "Non-sex Violent Offenses", #Abuse of family or household members
                                              grepl("707", offense_chapter) & grepl("7[3-4][0-49]", offense_section) ~ "Sex Offenses",
                                              grepl("708", offense_chapter) & grepl("8[0-35-9][0-9]", offense_section) ~ "Property Offenses",
                                              grepl("712", offense_chapter) & grepl("12[45][0-9]", offense_section) ~ "Drug Offenses",
                                              grepl("329", offense_chapter) ~ "Drug Offenses",
                                              TRUE ~ "Felony Other"))

#explore counts of initial offense categories
master_df_3 %>% count(initial_offense_category)
  
#explore NA initial_offense_categories
tmp <- master_df_3 %>% filter(is.na(initial_offense_category)) %>% distinct(offense_chapter,offense_section, offense_description)

#TESTING 2023/05/19: after first pass at categorizing, 891 offenses are NA...
#     669 are Parolees who we don't have initial offense data from HPA (although we do for most from DPS)
#     222 are non-Parolee but didn't fit into the first pass categories, so let's explore and try to categorize into violent, sex, property, and drug before bucketing into felony "other"
#     pretty much all of the 222 fell into weapons violations, vehicular manslaughter/serious bodily harm, habitual DUI,
#           parole violations, insurance fraud, revocation of probation, escape, bail jumping, prison contraband,
#           prostitution, failure to comply with sex offender registration
offense_category_unknown <- master_df_3 %>% 
  filter(agency != "Parolees", is.na(initial_offense_category))

# #Method B: recode initial offense type using strickly empirical approach based on Tim's FY16 codings
# #first check that no charge codes have more than one label
# tmpA <- master_fy16_offense_type %>% 
#   count(MRrecoded_offtype, OffenseDescription) #first collapse all duplicates within a category
# tmpB <- tmpA %>% 
#   count(OffenseDescription) #then count how many times charge code appears across categories
# #investigate charge codes with multiple labels
# tmpA %>% filter(ARRESTCHARGE=="710-1077")
# #investigate 710-1077 and what categories it shows up in
# tmpc <- master_fy16_offense_type %>% 
#   filter(ARRESTCHARGE=="710-1077") %>% 
#   select(SID, MRrecoded_offtype, ARRESTCHARGE, ARRESTCHARGEDESCRIPTION, FINALCHARGE, FINALCHARGEDESCRIPTION, OffenseDescription)
# #investigate "Theft in the Second Degree" and what categories it shows up in
# tmpc <- master_fy16_offense_type %>% 
#   filter(OffenseDescription=="Theft in the Second Degree") %>% 
#   select(SID, MRrecoded_offtype, ARRESTCHARGE, ARRESTCHARGEDESCRIPTION, FINALCHARGE, FINALCHARGEDESCRIPTION, OffenseDescription)
# 
# #get unique offense descriptions x category and collapse into a cell
# master_fy16_offense_category_lists <- master_fy16_offense_type %>% 
#   distinct(MRrecoded_offtype,OffenseDescription) %>% 
#   group_by(MRrecoded_offtype) %>% 
#   summarize(charge_codes = paste0(OffenseDescription, collapse = ","))
# 
# #to recover the charge codes for the offense descriptions, since the FY16 extract only has the descriptions, not the codes
# #create a key to map codes and descriptions: first get all unique arrest codes and descriptions
# arrest_codes <- master_fy16_offense_type %>% 
#   select(ARRESTCHARGEDESCRIPTION, ARRESTCHARGE) %>% 
#   filter(ARRESTCHARGE != "") %>% 
#   distinct(ARRESTCHARGEDESCRIPTION, ARRESTCHARGE) %>% 
#   rename(CHARGEDESCRIPTION=ARRESTCHARGEDESCRIPTION,CHARGE=ARRESTCHARGE)
# #create a key for codes and descriptions: next get all unique final codes and descriptions
# final_codes <- master_fy16_offense_type %>% 
#   select(FINALCHARGEDESCRIPTION, FINALCHARGE) %>% 
#   filter(FINALCHARGE != "") %>% 
#   distinct(FINALCHARGEDESCRIPTION, FINALCHARGE) %>% 
#   rename(CHARGEDESCRIPTION=FINALCHARGEDESCRIPTION,CHARGE=FINALCHARGE)
# 
# #bind the codes/descriptions and collapse any duplicates, now we have a key that maps charge codes to descriptions that we can use to recover charge codes for the offense descriptions
# charge_codes_key <- arrest_codes %>% 
#   bind_rows(final_codes) %>% 
#   distinct(CHARGEDESCRIPTION, CHARGE)
# 
# #for each category, get the unique offense descriptions, and then join them to the charge code key to recover their charge codes
# felony_violent_offenses <- master_fy16_offense_type %>% 
#   distinct(MRrecoded_offtype,OffenseDescription) %>% 
#   filter(MRrecoded_offtype == "Felony Violent") %>% 
#   mutate(OffenseDescription = toupper(OffenseDescription)) %>% 
#   # pull(OffenseDescription)
#   left_join(charge_codes_key, by = c("OffenseDescription" = "CHARGEDESCRIPTION"))
# 
# felony_sex_offenses <- master_fy16_offense_type %>% 
#   distinct(MRrecoded_offtype,OffenseDescription) %>% 
#   filter(MRrecoded_offtype == "Felony Sex") %>% 
#   mutate(OffenseDescription = toupper(OffenseDescription)) %>% 
#   # pull(OffenseDescription)
#   left_join(charge_codes_key, by = c("OffenseDescription" = "CHARGEDESCRIPTION"))
# 
# felony_property_offenses <- master_fy16_offense_type %>% 
#   distinct(MRrecoded_offtype,OffenseDescription) %>% 
#   filter(MRrecoded_offtype == "Felony Property") %>% 
#   mutate(OffenseDescription = toupper(OffenseDescription)) %>% 
#   # pull(OffenseDescription)
#   left_join(charge_codes_key, by = c("OffenseDescription" = "CHARGEDESCRIPTION"))
# 
# felony_drug_offenses <- master_fy16_offense_type %>% 
#   distinct(MRrecoded_offtype,OffenseDescription) %>% 
#   filter(MRrecoded_offtype == "Felony Drug") %>% 
#   mutate(OffenseDescription = toupper(OffenseDescription)) %>% 
#   # pull(OffenseDescription)
#   left_join(charge_codes_key, by = c("OffenseDescription" = "CHARGEDESCRIPTION"))
# 
# felony_other_offenses <- master_fy16_offense_type %>% 
#   distinct(MRrecoded_offtype,OffenseDescription) %>% 
#   filter(MRrecoded_offtype == "Felony Other") %>% 
#   mutate(OffenseDescription = toupper(OffenseDescription)) %>% 
#   # pull(OffenseDescription)
#   left_join(charge_codes_key, by = c("OffenseDescription" = "CHARGEDESCRIPTION"))
# 
# md_other_offenses <- master_fy16_offense_type %>% 
#   distinct(MRrecoded_offtype,OffenseDescription) %>% 
#   filter(MRrecoded_offtype == "Misdemeanor and Other") %>% 
#   mutate(OffenseDescription = toupper(OffenseDescription)) %>% 
#   # pull(OffenseDescription)
#   left_join(charge_codes_key, by = c("OffenseDescription" = "CHARGEDESCRIPTION"))
# #MAIN RESULT - many of the offense descriptions could not be mapped; many show up under multiple categories
# #unfortunately, the FY16 is too messy to use to determine which charge codes/descriptions fall under each category of crime

#___initial and most recent supervision level----
#In discussion with Tim, 2023/05/05, he did not use CX at all for determining risk level, only LSI date from CYZAP, so hit the breaks on this analysis
#first explore breakdown of first and most recent
# tmp3 <- master_acsb_jr %>% 
#   count(SupervisionLevel, InitialSupervisionLevel)
# 
# tmp4 <- master_df_3 %>% 
#   distinct(most_recent_supervision_level) %>% 
#   arrange(most_recent_supervision_level) %>% 
#   apply(., 1, function(x){cat(x); cat("\n")})
# 
# cat(unique(master_df_3$most_recent_supervision_level), sep = "\n")

#recode
#2023/05/04: Wait for now since the supervision level values in CX are not the same as the actual risk scores from CYZAP
# master_df_4 <- master_df_3 %>% 
#   mutate()

#---------------------------------------------------------------------------------#
#7. One-off exclusions, fixes----
#---------------------------------------------------------------------------------#
master_df_3a <-  master_df_3 %>% 
  #exclude this person -> CE shows offense severity level as FA but it's a typo and should be IN
  filter((FirstName != "Kang" & LastName != "An" & DOB != "1989-04-14") | is.na(FirstName)) 

#---------------------------------------------------------------------------------#
#8. SAVE MASTER LIST TO RDS/CSV----
#---------------------------------------------------------------------------------#
saveRDS(master_df_3a, paste0("H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\FY17_recidivism_study\\recidivism_study_FY", fy,"_master_sample_", format(Sys.time(), "%Y-%m-%d_%H%M"), ".rds"))
write.csv(master_df_3a, paste0("H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\FY17_recidivism_study\\recidivism_study_FY", fy,"_master_sample_", format(Sys.time(), "%Y-%m-%d_%H%M"), ".csv"), row.names = FALSE)

#save probation stats for report
saveRDS(probation_stats, file = paste0("H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\FY17_recidivism_study\\recidivism_study_FY", fy,"_probation_stats_", format(Sys.time(), "%Y-%m-%d_%H%M"), ".rds"))
#---------------------------------------------------------------------------------#
#_######### LIST FOR CJIS/CYZAP ######### ----
#---------------------------------------------------------------------------------#
master_cjis <- master_df_3a %>% 
  select(SID, DOB, LastName, FirstName, followup_date)

write.csv(master_cjis, paste0("H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\FY17_recidivism_study\\recidivism_study_FY", fy,"_master_sample_abbrev_", format(Sys.time(), "%Y-%m-%d_%H%M"), ".csv"), row.names = FALSE)

#---------------------------------------------------------------------------------#
#QA: compare first list for CJIS with newest list----
#---------------------------------------------------------------------------------#
# master_cjis_old <- read.csv("H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\FY17_recidivism_study\\archives\\recidivism_study_FY2017_full_sample_CJIS_2023-03-15_1331.csv") %>% 
#   mutate(DOB = as.Date(DOB),
#          follow_up_date = as.Date(follow_up_date))
#   
# master_cjis_new <- readRDS("H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\FY17_recidivism_study\\recidivism_study_FY2017_master_sample_2023-05-05_1325.rds")
# 
# 
# diff_new_minus_old <- master_cjis_new %>% 
#   anti_join(master_cjis_old, by = c("SID", "DOB"))
# #O rows, good
# 
# diff_new_minus_old_left_join <- master_cjis_new %>% 
#   left_join(master_cjis_old, by = c("SID", "DOB")) %>% #get all old follow-up dates for every entry in new list
#   mutate(followup_date_compare = if_else(followup_date == follow_up_date, "match", "ERROR")) %>% 
#   filter(followup_date_compare == "ERROR") %>% #isolate mismatches
#   select(1:17) %>% #remove master_cjis_old cols to more easily rejoin
#   distinct() %>% #collapse any identical rows, important so that the left_join doesn't incorrectly multiply rows
#   left_join(master_cjis_old, by = c("SID", "DOB")) %>% #join back on old follow-up dates to get all follow-up dates for the mismatches, assuming these people had multiple followup dates and one of them should match with new follow up date
#   mutate(followup_date_compare = if_else(followup_date == follow_up_date, "match", "ERROR")) 
# #there are 269 follow-up dates that changed between old and new lists
# #all new follow-up dates that changed can be mapped to a matching old follow up and at least one non-matching old follow up date
