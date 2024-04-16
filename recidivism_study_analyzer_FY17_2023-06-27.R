#---------------------------------------------------------------------------------#
#recidivism_study_FY16_reproduction_[YYYY-MM-DD].R
#forked from recidivism_study_2023-04-21.R on 2023/04/21 by Adam S. Cohen
#Last modified on 2023/04/21 by Adam S. Cohen
#---------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------#
#clear workspace----
#---------------------------------------------------------------------------------#
rm(list = ls())

#---------------------------------------------------------------------------------#
#load libraries----
#---------------------------------------------------------------------------------#
library(readxl)
library(dplyr)
library(tidyr)
#https://stackoverflow.com/questions/33026167/reading-sav-file-into-r
library(haven)
library(scales)
library(janitor)
library(report)
library(ggplot2)
library(lubridate)
library(stringr)
library(zipcodeR)
library(effectsize)

#---------------------------------------------------------------------------------#
#set constants----
#---------------------------------------------------------------------------------#
fy <- "2017"
#prevent county code for displaying in scientific notation
options(scipen=999)

#---------------------------------------------------------------------------------#
#Import data----
#---------------------------------------------------------------------------------#
# CJIS_FY16 <- read_sav("H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\Tim_Wong_files\\from_tim_flash\\FY 2016 Recidivism Study\\Master Recidivism Study FY 2016.sav")
# CJIS_FY16 <- read_sav("H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\Tim_Wong_files\\from_Erin_Tim_Wong_files\\Master Recidivism Study_rev FY 2016.sav")

#FY2017
CJIS_FY17 <- read.csv("H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\FY17_recidivism_study\\CJIS\\RecidivismExtract_2023-05-30.csv") #note, need to make sure every column in first row of data underneath col heads has data, otherwise get a error that "duplicate 'row.names' are not allowed"
CJIS_FY17_redo <- read.csv("H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\FY17_recidivism_study\\CJIS\\RecidivismExtract_2023_June Redo.csv") #note, need to make sure every column in first row of data underneath col heads has data, otherwise get a error that "duplicate 'row.names' are not allowed"
CJIS_FY17 <- CJIS_FY17 %>% 
  bind_rows(CJIS_FY17_redo)
master_df_3 <- readRDS("H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\FY17_recidivism_study\\recidivism_study_FY2017_master_sample_2023-05-19_1245.rds")

#---------------------------------------------------------------------------------#
#WRANGLE CJIS DATA----
#---------------------------------------------------------------------------------#
#___QA: Check everyone on CJIS file is on master file and vice versa----
#---------------------------------------------------------------------------------#
#collapse mutiple charges, get unique SIDs for CJIS
CJIS_distinct <- CJIS_FY17 %>% 
  mutate(DOB = as.Date(SID = toupper(SID),
                       DOB, "%m/%d/%Y")) %>% 
  distinct(SID, FIRST.NAME, LAST.NAME, DOB)

CJIS_distinct_sid <- CJIS_FY17 %>% 
  mutate(DOB = as.Date(SID = toupper(SID),
                       DOB, "%m/%d/%Y")) %>% 
  filter(SID != "NULL") %>% 
  distinct(SID)
nrow(CJIS_distinct) == nrow(CJIS_distinct_sid) #check no missing SIDs
#CJIS_FY17_distinct = 3983 obs, CJIS_distinct_sid = 3983 obs
#---------------------------#
#create two master_dfs, one for SIDs one for NULL
master_df_3_distinct_sid <- master_df_3 %>% 
  filter(SID != "NULL") %>% 
  mutate(SID = toupper(SID),
         LastName = toupper(LastName),
         FirstName = toupper(FirstName),
         DOB = as.Date(DOB)) %>% 
  distinct(SID, .keep_all = TRUE)

master_df_3_distinct_null <- master_df_3 %>% 
  filter(SID == "NULL") %>% 
  mutate(SID = toupper(SID),
         LastName = toupper(LastName),
         FirstName = toupper(FirstName),
         DOB = as.Date(DOB)) %>% 
  distinct(SID, DOB, LastName, FirstName, .keep_all = TRUE)
#---------------------------#
#Does everyone on master df show up on CJIS?
#anti-join each master_df with CJIS_distinct to see persons missing from CJIS extract
master_anti_cjis_sid <- master_df_3_distinct_sid %>% 
  filter(SID != "NULL") %>% 
  anti_join(CJIS_distinct, by = c("SID"))
#47 parolees, 1 prob, 1 MTR 

master_anti_cjis_null <- master_df_3_distinct_null %>% 
  filter(SID == "NULL") %>% 
  anti_join(CJIS_distinct, by = c("DOB", "LastName" = "FIRST.NAME"))
#22 probationers with NULL SIDs

#join and send to CJIS
cjis_fy17_missing <- master_anti_cjis_sid %>% 
  bind_rows(master_anti_cjis_null) %>% 
  select(SID, DOB, LastName, FirstName, followup_date)
write.csv(cjis_fy17_missing, "H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\FY17_recidivism_study\\recidivism_study_FY2017_missing_from_CJIS_2023-06-01_1400.csv", row.names = FALSE)
#---------------------------#
#Does everyone on CJIS show up on master_df?
cjis_anti_master_sid <- CJIS_distinct %>% 
  anti_join(master_df_3_distinct_sid, by = c("SID"))
#10 not on master list of SIDs, but check those without SIDs...

#cjis_anti_master_sid are those persons on CJIS not on master_df with SIDs, check now if the remaining are on master_df no SIDs
cjis_anti_master_null <- cjis_anti_master_sid %>% 
  anti_join(master_df_3_distinct_null, by =  c("DOB", "FIRST.NAME" = "LastName"))
#all there except St Laurent, but only not showing up b/c CJIS puts a "." before St

#---------------------------------------------------------------------------------#
#___QA: Check severity levels, make sure all are factorized----
#---------------------------------------------------------------------------------#
CJIS_FY17 %>% count(ARREST.SEV)

#---------------------------------------------------------------------------------#
#___RENAME VARS, FORMAT DATES, factorize ----
#---------------------------------------------------------------------------------#
CJIS_data_1 <-  CJIS_FY17 %>% 
  rename(ARRESTCHARGEDESCRIPTION = ARREST.CHG.DESC) %>% 
  mutate(ARRESTDATE = as.Date(ARREST.DATE, "%m/%d/%Y"),
         DOB = as.Date(DOB, "%m/%d/%Y"),
         ARRESTSEV = if_else(ARREST.SEV == "F" & grepl("murder", ARRESTCHARGEDESCRIPTION, ignore.case = TRUE), "M", ARREST.SEV),
         ARRESTSEV = factor(ARRESTSEV, levels = c('XX','VL','V','PM','MD', "F",'FC','FB','FA',"M"))) #set up as a factor in order to slice max charge)

#___REMOVE EXTRA/INCOMPLETE DATA----
#______FILTER NO SIDs/person not found----
CJIS_data_2 <- CJIS_data_1  %>% 
  filter(SID != "")

#______FILTER FOR FIRST REARREST with highest charge severity (no ties) or keep if no rearrest----
CJIS_data_3 <- CJIS_data_2 %>% 
  group_by(SID) %>% 
  slice_min(ARRESTDATE) %>% 
  slice_max(ARRESTSEV, with_ties = FALSE) %>% 
  #get county from zipcode -> reverse_zipcode does not work in other places, investigating; opened issue on github
  mutate(zipcode = str_extract(ADDRESS, "[0-9][0-9][0-9][0-9][0-9]$"),
         zipcode2 = if_else(is.na(zipcode), "00000", zipcode), #reverse_zipcode complains if there are NAs, so fill in with 00000
         # county_cjis = if_else(!is.na(zipcode2) & nchar(zipcode2) == 5, reverse_zipcode(zipcode)$county, NA_character_),
         county_cjis = reverse_zipcode(zipcode2)$county)

#---------------------------------------------------------------------------------#
#RECODE VARIABLES FY16
#---------------------------------------------------------------------------------#
#For FY2017, all recoding done in master_df_6

#First QA for FY2016: check accuracy of my recoding of Rearrest, ICIS_arrest, and current_age
# CJIS_FY16_rc <- CJIS_FY16 %>% 
#   mutate(Rearrest_ac = case_when(!is.na(ARRESTDATE) ~ 1,
#                                  is.na(ARRESTDATE) & !is.na(Followup_date) ~ 0),
#          #If ARRESTDATE is NA and Followup_date is NA, then assign NA
#          ICIS_arrest_ac = case_when(grepl("probation|parole", ARRESTCHARGEDESCRIPTION, ignore.case = TRUE) ~ "Probation Revocations/Parole Violations",
#                                     grepl("contempt", ARRESTCHARGEDESCRIPTION, ignore.case = TRUE) ~ "Criminal Contempt of Court",
#                                     grepl("DAG|DNC|condition(al|s)(\\sof)? rel|misc public outside|MOPEDS|GENERAL OPERATION OF VEHICLE|INATTENTION TO DRIVING", ARRESTCHARGEDESCRIPTION, ignore.case = TRUE) ~ "Other Violations",
#                                     grepl("[A-Z]", ARRESTCHARGEDESCRIPTION, ignore.case = TRUE) & !grepl("probation|parole|contempt|DAG|DNC|conditional release|conditions rel", ARRESTCHARGEDESCRIPTION, ignore.case = TRUE) ~ "Criminal Arrest",
#                                     TRUE ~ "No Rearrest"),
#          Rearrest_comp = if_else(Rearrest == Rearrest_ac, "good", "ERROR"),
#          ICIS_arrest_attr = as_factor(ICIS_arrest),
#          ICIS_arrest_comp = if_else(ICIS_arrest_attr == ICIS_arrest_ac, "good", "ERROR"),
#          current_age_ac = (ARRESTDATE - DOB)/365,
#          current_age_comp = if_else(current_age == current_age_ac, "good", "ERROR"),
#          mystery_date = DOB + (current_age * 365) #appears Tim used 2020/11/18
#   ) %>% 
#   # select(SID, cohort, Rearrest, Rearrest_ac, Rearrest_comp, ARRESTCHARGEDESCRIPTION, ARRESTSEVERITY, ICIS_arrest, ICIS_arrest_attr, ICIS_arrest_ac, ICIS_arrest_comp)
#   select(SID, cohort, ARRESTDATE, DOB, current_age, current_age_ac, current_age_comp, mystery_date) %>% 
#   filter(!is.na(ARRESTDATE))
# 
# CJIS_FY16 <- CJIS_FY16 %>% 
#   mutate(Rearrest_ac = case_when(!is.na(ARRESTDATE) ~ 1,
#                                  is.na(ARRESTDATE) & !is.na(Followup_date) ~ 0),
#          #If ARRESTDATE is NA and Followup_date is NA, then assign NA
#          ICIS_arrest_ac = case_when(grepl("probation|parole", ARRESTCHARGEDESCRIPTION, ignore.case = TRUE) ~ "Probation Revocations/Parole Violations",
#                                     grepl("contempt", ARRESTCHARGEDESCRIPTION, ignore.case = TRUE) ~ "Criminal Contempt of Court",
#                                     grepl("DAG|DNC|condition(al|s)(\\sof)? rel|misc public outside|MOPEDS|GENERAL OPERATION OF VEHICLE|INATTENTION TO DRIVING", ARRESTCHARGEDESCRIPTION, ignore.case = TRUE) ~ "Other Violations",
#                                     grepl("[A-Z]", ARRESTCHARGEDESCRIPTION, ignore.case = TRUE) & !grepl("probation|parole|contempt|DAG|DNC|conditional release|conditions rel", ARRESTCHARGEDESCRIPTION, ignore.case = TRUE) ~ "Criminal Arrest",
#                                     TRUE ~ "No Rearrest"),
#          current_age_ac = (ARRESTDATE - DOB)/365
#   ) 

#---------------------------------------------------------------------------------#
#JOIN Master DF with ICIS REARREST DATA----
#also check arrest dates are not before follow-up dates and not beyond 3 years
#---------------------------------------------------------------------------------#
##first filter out MD probation
master_df_4 <- master_df_3 %>% 
  filter(probation_type == "felony_probation" | is.na(probation_type))

#separate master df into block with SIDs and block without SIDs, since they need to be joined separately
master_df_4_SIDs <- master_df_4 %>% 
  filter(nchar(SID) == 8)

master_df_4_NO_SIDs <- master_df_4 %>% 
  filter(nchar(SID) != 8)

#join each block of the master df with the CJIS data
master_df_5_SIDs <- master_df_4_SIDs %>% 
  inner_join(CJIS_data_3, by = "SID", keep = TRUE) #keep join cols so that row bind is easier

master_df_5_NO_SIDs <- master_df_4_NO_SIDs %>% 
  inner_join(CJIS_data_3, by = c("DOB", "LastName" = "LAST.NAME"), keep = TRUE) #keep join cols so that row bind is easier

#---------------------------------------------------------------------------------#
#RECODE VARIABLES FY17----
#---------------------------------------------------------------------------------#
master_df_6 <- master_df_5_SIDs %>% 
  bind_rows(master_df_5_NO_SIDs) %>% 
  #create new ARRESTDATE col with only 0<= arrest dates - followup_date <= 3, otherwise code as NA
  mutate(ARRESTDATE_full = ARRESTDATE,
         ARRESTDATE = as.POSIXct(ARRESTDATE_full, tz = "UCT"),
         # date_diff = interval(ARRESTDATE_full, followup_date)/years(1)
         ARRESTDATE = if_else(0 <= interval(followup_date, ARRESTDATE_full)/years(1) & interval(followup_date, ARRESTDATE_full)/years(1) <= 3, 
                              ARRESTDATE_full, NA_POSIXct_),
         Rearrest = case_when(!is.na(ARRESTDATE) ~ 1,
                              is.na(ARRESTDATE) ~ 0), #& !is.na(Followup_date) ~ 0),
         #If ARRESTDATE is NA and Followup_date is NA, then assign NA
         #DISPOSITION RECODING; see Analysis 7, 8, 9 for investigation of dispo codes
         #FY16 - long dispo description column from CJIS
         #DISPOSITIONDESCRIPTION_rc = case_when(grepl("Found Guilty|Sentenced|Parole Was Revoked|Sentence was Resumed|Committed", DISPOSITION, ignore.case = TRUE) ~ "guilty",
         #                                              grepl("Not Guilty|Discharged|Prosecution was Declined|Nolle|(Released.*(No Charge|Prosecution Declined))|Dismissed|Acquitted|Not Contested|Stricken|No Action|Case-in-chief|prosecution was deferred|case is inactive|moot", DISPOSITION, ignore.case = TRUE) ~ "not_guilty",
         #                                              grepl("Continu|(Release on|Revocation of) bail|Released.*(Bail|Pending Further Investigation|Own Recognizance|Cell\\s?Block)|bench|summon|Didn't Appear|Remanded|Plea|merged|subject was released|pending arrest disposition|processed for honolulu county", DISPOSITION, ignore.case = TRUE) ~ "pendingA",
         #                                              grepl("was taken|TURNED OVER TO ANOTHER AGENCY|bail was reset|prosecutors added|complaint was filed", DISPOSITION, ignore.case = TRUE) ~ "pendingB",
         #                                              DISPOSITION == "" ~ "none",
         #                                              TRUE ~ "other"),
         #FY17 - short dispo description column from CJIS
         DISPOSITIONDESCRIPTION_rc = case_when(Rearrest == 0 ~ "none",
                                               grepl("^GUILTY$|PROB REV|PAROLE REVOKED|Sentenced|Parole Was Revoked|Sentence was Resumed|Committed", DISPOSITION, ignore.case = TRUE) ~ "Guilty",
                                               grepl("DSM W/O PREJ|REL-NO CHARGE|Not Guilty|NO ACTION|DECL TO PROS|LACK OF PROS|Discharged|Prosecution was Declined|Nolle|(Released.*(No Charge|Prosecution Declined))|Dismissed|Acquitted|Not Contested|Stricken|No Action|Case-in-chief|prosecution was deferred|case is inactive|moot", DISPOSITION, ignore.case = TRUE) ~ "Acquitted",
                                               grepl("REL PEND INV|REL-BAIL/BOND|NO SHOW APPR|REL TO DIST CT|Continu|(Release on|Revocation of) bail|Released.*(Bail|Pending Further Investigation|Own Recognizance|Cell\\s?Block)|bench|summon|Didn't Appear|Remanded|Plea|merged|subject was released|pending arrest disposition|processed for honolulu county", DISPOSITION, ignore.case = TRUE) ~ "Pending - Late pretrial",
                                               grepl("INTAKE SVC CTR|ISC VIA CIRC CT|OUTSIDE AGENCY|BAIL RESET DRUG|TURNED OVER TO ANOTHER AGENCY|bail was reset|prosecutors added|complaint was filed", DISPOSITION, ignore.case = TRUE) ~ "Pending - Early pretrial",
                                               DISPOSITION == "" ~ "none",
                                               TRUE ~ "Other"),
         #ICIS_arrest coding for FY16 -> CJIS extract using different language for FY17, talking with HCJDC about this
         # ICIS_arrest = case_when(grepl("probation|parole", ARRESTCHARGEDESCRIPTION, ignore.case = TRUE) ~ "Probation Revocations/Parole Violations",
         #                         grepl("contempt", ARRESTCHARGEDESCRIPTION, ignore.case = TRUE) ~ "Criminal Contempt of Court",
         #                         grepl("DAG|DNC|condition(al|s)(\\sof)? rel|misc public outside|MOPEDS|GENERAL OPERATION OF VEHICLE|INATTENTION TO DRIVING", ARRESTCHARGEDESCRIPTION, ignore.case = TRUE) ~ "Other Violations",
         #                         ARRESTCHARGEDESCRIPTION == "" & Rearrest == 1 ~ "Other Violations",
         #                         grepl("[A-Z]", ARRESTCHARGEDESCRIPTION, ignore.case = TRUE) & !grepl("probation|parole|contempt|DAG|DNC|conditional release|conditions rel", ARRESTCHARGEDESCRIPTION, ignore.case = TRUE) ~ "Criminal Arrest",
         #                         TRUE ~ "No Rearrest"))
         #ICIS_arrest codings for FY17
         ICIS_arrest = case_when(grepl("PROB|parole", ARRESTCHARGEDESCRIPTION, ignore.case = TRUE) ~ "Probation Revocations/Parole Violations",
                                 grepl("CRIM CONTMP CRT|CRIMINAL CONTEM", ARRESTCHARGEDESCRIPTION, ignore.case = TRUE) ~ "Criminal Contempt of Court",
                                 grepl("DAG|DNC|condition(al|s)(\\sof)? rel|misc public outside|MOPEDS|GENERAL OPERATION OF VEHICLE|INATTENTION TO DRIVING", ARRESTCHARGEDESCRIPTION, ignore.case = TRUE) ~ "Other Violations",
                                 ARRESTCHARGEDESCRIPTION == "" & Rearrest == 1 ~ "Other Violations",
                                 grepl("[A-Z]", ARRESTCHARGEDESCRIPTION, ignore.case = TRUE) & !grepl("PROB|parole|CRIM CONTMP CRT|CRIMINAL CONTEM|DAG|DNC|conditional release|conditions rel", ARRESTCHARGEDESCRIPTION, ignore.case = TRUE) ~ "Criminal Arrest",
                                 TRUE ~ "No Rearrest"),
         ICIS_arrest = factor(ICIS_arrest, levels = c("Criminal Arrest", "Probation Revocations/Parole Violations", "Criminal Contempt of Court", "Other Violations")),
         #if rearrest is before or 3 years after followup, turn to NA
         ICIS_arrest = if_else(0 <= interval(followup_date, ARRESTDATE_full)/years(1) & interval(followup_date, ARRESTDATE_full)/years(1) <= 3, 
                               ICIS_arrest, NA),
         agency = factor(agency, levels = c("Probationers", "Parolees", "MTR-Prisoners")),
         county = factor(county, levels = c("Kauai", "Maui", "Honolulu", "Hawaii", "Other", "Unknown")),
         county_cjis = case_when(county_cjis %in% c("Kauai County", "Maui County", "Honolulu County", "Hawaii County") ~ county_cjis,
                                 is.na(county_cjis) ~ "Unknown",
                                 TRUE ~ "Other"),
         county_cjis = factor(county_cjis, levels = c("Kauai County", "Maui County", "Honolulu County", "Hawaii County", "Other", "Unknown")),
         county_cjis_rc = case_when(county_cjis == "Kauai County" ~ 100000,
                                    county_cjis == "Maui County" ~ 10000,
                                    county_cjis == "Honolulu County" ~ 1000,
                                    county_cjis == "Hawaii County" ~ 100,
                                    county_cjis == "Other" ~ 10,
                                    county_cjis == "Unknown" ~ 1),
         county_rc = case_when(county == "Kauai" ~ 200000,
                                    county == "Maui" ~ 20000,
                                    county == "Honolulu" ~ 2000,
                                    county == "Hawaii" ~ 200,
                                    county == "Other" ~ 20,
                                    county == "Unknown" ~ 2),
         county_agg = county_rc + county_cjis_rc,
         #when county_agg contains a 3, then source and CJIS agree; position index of "3" determines county
         #when county_agg contains a 2 and 1, then there is disagreement; 
         #      index of "2" is location according to source
         #      index of "1" is location according to CJIS
         #      if 3, assign [county]_high confidence; if 2, assign [county]_med confidence; else assign low_confidence
         county_confidence = case_when(str_locate(county_agg, pattern = "3")[, 1] + (6 - nchar(county_agg)) == 1 ~ "Kauai_high",
                                       str_locate(county_agg, pattern = "3")[, 1] + (6 - nchar(county_agg)) == 2 ~ "Maui_high", 
                                       str_locate(county_agg, pattern = "3")[, 1] + (6 - nchar(county_agg)) == 3 ~ "Honolulu_high", 
                                       str_locate(county_agg, pattern = "3")[, 1] + (6 - nchar(county_agg)) == 4 ~ "Hawaii_high", 
                                       str_locate(county_agg, pattern = "3")[, 1] + (6 - nchar(county_agg)) == 5 ~ "Other_high", 
                                       str_locate(county_agg, pattern = "3")[, 1] + (6 - nchar(county_agg)) == 6 ~ "Unknown_high",
                                       str_locate(county_agg, pattern = "2")[, 1] + (6 - nchar(county_agg)) == 1 ~ "Kauai_med",
                                       str_locate(county_agg, pattern = "2")[, 1] + (6 - nchar(county_agg)) == 2 ~ "Maui_med", 
                                       str_locate(county_agg, pattern = "2")[, 1] + (6 - nchar(county_agg)) == 3 ~ "Honolulu_med", 
                                       str_locate(county_agg, pattern = "2")[, 1] + (6 - nchar(county_agg)) == 4 ~ "Hawaii_med",
                                       TRUE ~ "low_confidence" #source was either other or unknown, and CJIS had something else
         ),
         #if confidence is high or med, assign status, otherwise assign low_confidence
         county_infer = case_when(grepl("Kauai", county_confidence) ~ "Kauai",
                                  grepl("Maui", county_confidence) ~ "Maui",
                                  grepl("Honolulu", county_confidence) ~ "Honolulu",
                                  grepl("Hawaii", county_confidence) ~ "Hawaii",
                                  grepl("Other", county_confidence) ~ "Other",
                                  grepl("Unknown", county_confidence) ~ "Unknown",
                                  grepl("low_confidence", county_confidence) ~ "low_confidence"
                                  )
         ) %>% 
  mutate(days_rearrest = interval(followup_date, ARRESTDATE)/days(1),
         years_rearrest = case_when(days_rearrest <= 365 ~ "one",
                                    365 < days_rearrest & days_rearrest <= 365*2 ~ "two",
                                    365*2 < days_rearrest & days_rearrest <= 365*3 ~ "three",
                                    365*3 < days_rearrest  ~ "three_plus"),
         years_rearrest = factor(years_rearrest, levels = c("one","two","three","three_plus"))) %>% 
  #following Tim, first recode CJIS into numeric labels
  mutate(race_rc = case_when(RACE == "BLACK" ~ 1,
                             RACE == "CHINESE" ~ 2,
                             RACE == "FILIPINO" ~ 3,
                             RACE == "HAWAIIAN" ~ 4,
                             RACE == "HISPANIC" ~ 5,
                             RACE == "JAPANESE" ~ 6,
                             RACE == "KOREAN" ~ 7,
                             RACE == "MICRONESIAN" ~ 8,
                             RACE == "NATIVE" ~ 9,
                             RACE == "OTHER" ~ 10,
                             RACE == "SAMOAN" ~ 11,
                             RACE == "TONGAN" ~ 12,
                             RACE == "UNKNOWN" | RACE == "" ~ 13,
                             RACE == "WHITE" ~ 14
                             ),
         #then recode into fig 19 categories (see Tim's FY16 SPSS sps and sav files
         Ethnicity = case_when(race_rc == 4 ~ "Hawaiian-Part Hawn",
                               race_rc == 14 ~ "Caucasian",
                               race_rc == 3 ~ "Filipino",
                               race_rc == 6 ~ "Japanese",
                               race_rc == 11 ~ "Samoan",
                               race_rc == 1 ~ "African-American",
                               TRUE ~ "All Others"),
         Ethnicity = factor(Ethnicity, levels = c("Hawaiian-Part Hawn", "Caucasian", "Filipino", "Japanese", 
                                                  "Samoan", "African-American", "All Others")),
         SEX = case_when(SEX %in% c("FEMALE", "MALE", "UNKNOWN") ~ SEX,
                         TRUE ~ "UNKNOWN"),
         SEX = factor(SEX, levels = c("MALE", "FEMALE", "UNKNOWN")),
         age_range = factor(age_range, levels = c("< 20 years old", "20-29 years old", "30-39 years old", 
                                                  "40-49 years old", "50-59 years old", "+60 years old")),
         initial_offense_category = factor(initial_offense_category, levels = c("Non-sex Violent Offenses", "Sex Offenses", "Property Offenses", "Drug Offenses", "Felony Other", "Misdemeanor and other"))
         ) %>% 
  relocate(c(SID.y:DOB.y,DAG.DANC.DATE:CUSTODY.REL.DATE),.after = last_col()) #reorder df for easier viewing

# #QA: troubleshooting issues with reverse_zipcode
# tmp <- master_df_5_SIDs[1:13,]
# 
# tmp2 <- tmp %>% 
#   mutate(zipcode = str_extract(ADDRESS, "[0-9][0-9][0-9][0-9][0-9]$"),
#          zipcode2 = if_else(is.na(zipcode), "00000", zipcode),
#          county_cjis = reverse_zipcode(zipcode2)$county)
# tmp3 <- tmp2 %>% 
#   mutate(county_cjis = reverse_zipcode(zipcode2)$county)
# 
# library(dplyr)
# library(zipcodeR)
# df <- data.frame(id = 1:13,
#                  zipcode = c("96753", "00000", "96744", "96782", "00000", "96720", "96813", "96712", "96817", "96818", "96822", "00000", "96817"))
# df2 <- df %>% 
#   mutate(county = reverse_zipcode(zipcode)$county)

#---------------------------------------------------------------------------------#
#QA CHECK CJIS DATA----
#---------------------------------------------------------------------------------#
#QA: No arrests before start/release dates
CJIS_QA_arrest_check <- master_df_6 %>% 
  mutate(arrest_date_all = as.POSIXct(ARRESTDATE_full, tz = "UCT"), #don't use ARRESTDATE because arrests before start/release date were turned to NA; ARRESTDATE_full has all arrest dates
         days_rearrest_all = interval(followup_date, arrest_date_all)/days(1),
         years_rearrest_all = case_when(days_rearrest_all < 0 ~ "negative",
                                    0 <= days_rearrest_all & days_rearrest_all <= 365 ~ "one",
                                    365 < days_rearrest_all & days_rearrest_all <= 365*2 ~ "two",
                                    365*2 < days_rearrest_all & days_rearrest_all <= 365*3 ~ "three",
                                    365*3 < days_rearrest_all  ~ "three_plus"),
         years_rearrest_all = factor(years_rearrest_all, levels = c("negative","one","two","three","three_plus"))) %>% 
  count(years_rearrest_all)
#QA: Check max arrest date, make sure arrest dates weren't cut off early (e.g. no arrests after 12/31/19)

#QA: Check that people who haven't recidivated have no dispositions listed
CJIS_QA_dispo_check <- master_df_6 %>% 
  filter(Rearrest == 0) %>% 
  count(DISPOSITIONDESCRIPTION_rc)

CJIS_QA_dispo_check_examples <- master_df_6 %>% 
  filter(Rearrest == 0, !DISPOSITIONDESCRIPTION_rc %in% c("","none")) 

#---------------------------------------------------------------------------------#
#___prepare fully crossed factors for filling in any missing levels----
#---------------------------------------------------------------------------------#
#when a agency is missing a county level (e.g., Other or Unknown), fill it in with blank data -> important for creating nice tables and figures
agency_factors <- data.frame(agency = factor(c("Probationers", "Parolees", "MTR-Prisoners"),
                                            levels = c("Probationers", "Parolees", "MTR-Prisoners")))

county_factors <- data.frame(county = factor(c("Kauai", "Maui", "Honolulu", "Hawaii", "Other", "Unknown"),
                                            levels = c("Kauai", "Maui", "Honolulu", "Hawaii", "Other", "Unknown")))

#when an ICIS_arrest is missing a county level (e.g., Other or Unknown), fill it in wtih blank data -> important for creating nice tables and figures
ICIS_arrest_factors <- data.frame(ICIS_arrest = factor(levels(master_df_6$ICIS_arrest),
                                                      levels = levels(master_df_6$ICIS_arrest)))

initial_offense_factors <- data.frame(initial_offense_category = factor(levels(master_df_6$initial_offense_category),
                                                                       levels = levels(master_df_6$initial_offense_category)))

sex_factors <- data.frame(SEX = factor(levels(master_df_6$SEX),
                                      levels = levels(master_df_6$SEX)))

age_range_factors <- data.frame(age_range = factor(levels(master_df_6$age_range),
                                       levels = levels(master_df_6$age_range)))
#Table/Fig 11
agency_x_county_factors <- crossing(agency_factors,county_factors)

#Table/Fig 12, 13 and 15
ICIS_arrest_x_county_factors <- crossing(county_factors,ICIS_arrest_factors)

#Table/Fig 14
agency_factors_x_ICIS_arrest_factors <- crossing(agency_factors, ICIS_arrest_factors)

#Table/Fig 17
initial_offense_x_ICIS_arrest_factors <- crossing(initial_offense_factors, ICIS_arrest_factors)

#Table/Fig 18
agency_x_sex_factors <- crossing(agency_factors, sex_factors, Rearrest = c(0,1)) #add rearrest so that unknowns show up for rearrest even if there are no recidivists; need to include them so that total sample size adds up since it include recidivists and non-recidivists

#Table/Fig 20
agency_x_age_range_factors <- crossing(agency_factors, age_range_factors) #add rearrest so that unknowns show up for rearrest even if there are no recidivists; need to include them so that total sample size adds up since it include recidivists and non-recidivists

#---------------------------------------------------------------------------------#
#ANALYSIS 1 RR by Offender Type----
#NOTES: p-value incorrect in Tim's 2016 report - the recidivism rate is NOT sig different by agency
#---------------------------------------------------------------------------------#
table1_recidivism_rate_agency <- master_df_6 %>%
  count(agency, Rearrest, name = "r") %>%
  group_by(agency) %>%
  mutate(sample_size = sum(r)) %>% 
  ungroup() %>%
  filter(Rearrest == 1) %>%
  mutate(prop_recidivate = r/sample_size,
         prop_recidivate_pretty = percent(r/sample_size, accuracy = 0.1)) 

table1 <- table1_recidivism_rate_agency %>% 
  select(1,3:4,6) %>% 
  rename("Offender type" = agency, "Sample size" = sample_size, "Recidivism rate" = prop_recidivate_pretty) %>% 
  adorn_totals() %>% 
  mutate(`Recidivism rate` = if_else(`Offender type` == "Total", percent(r/`Sample size`, accuracy = 0.1), `Recidivism rate`))

table1_recidivism_rate_total <- sum(table1_recidivism_rate_agency$r)/sum(table1_recidivism_rate_agency$sample_size)

stat1_chi_agency_rearrest <- chisq.test(master_df_6$agency, master_df_6$Rearrest)
# stat1_phi_agency_rearrest <- sqrt(stat1_chi_agency_rearrest$statistic/sum(table1_recidivism_rate_agency$r))
#use this to double check N
#https://statsandr.com/blog/chi-square-test-of-independence-in-r/
summary(table(master_df_6$agency, master_df_6$Rearrest)) 
stat1_p_value <- ifelse(signif_half_up(stat1_chi_agency_rearrest$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat1_chi_agency_rearrest$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat1_description <- if_else(stat1_chi_agency_rearrest$p.value < .05,
                             "There was a statistically significant difference in recidivism rates between offender types, ",
                             "The difference in recidivism rates between offender types was not statistically significant, ")
stat1 <- paste0(stat1_description, 
                "$\\chi^2$(", stat1_chi_agency_rearrest$parameter, ", <em>N</em> = ", 
                sum(table1_recidivism_rate_agency$sample_size),
                ") = ", round_half_up(stat1_chi_agency_rearrest$statistic, digits = 2), 
                stat1_p_value)

figure1 <- ggplot(table1_recidivism_rate_agency, aes(x = agency, y = prop_recidivate, text = paste0("Agency: ", agency, "\n",
                                                                                                "Recidivism rate: ", prop_recidivate_pretty))) +
  geom_col(fill = "gray81") +
  scale_x_discrete(name = "", labels = paste0(table1_recidivism_rate_agency$agency, " (n = ", table1_recidivism_rate_agency$sample_size, ")", 
                                              "\n Recidivism = ",table1_recidivism_rate_agency$prop_recidivate_pretty)) +
  scale_y_continuous(name = "Recidivism Rate", labels = percent_format(accuracy = 0.1), breaks = seq(0, 1, by = .1), limits = c(0,1)) +
  ggtitle(paste0("Recidivism Rates by Offender Type, FY ", fy, " Cohort")) +
  # geom_text(aes(label = prop_recidivate_pretty), nudge_y = -0.05, color = "white") +
  geom_hline(yintercept = table1_recidivism_rate_total, color = "red", linewidth = 1, linetype = 2) +
  annotate("text", label = paste0("FY", fy, " Total Recividism Rate (", 
                                  percent(table1_recidivism_rate_total, accuracy = 0.1),")"), x = 2, y = table1_recidivism_rate_total + 0.03, color = "black") +
  theme_bw() +
  theme(axis.text = element_text(size = 11)) +
  NULL

#---------------------------------------------------------------------------------#
#ANALYSIS 2 RR by Follow-up Period----
#NOTES: 
#1. total sample sizes include NAs that were excluded from Fig 1, so probation n = 1304 for Fig 2 but n = 1301 for Fig 1
#2. slight discrepancy in year 1 and year 2 due to Tim calculating months (30 days = month) since rearrest and I calculated years (1 year = 365 days)
#---------------------------------------------------------------------------------#
#test to figure out if Tim was using full sample (n = 2175 = # rows in master_df_6, or n = 2162 that excludes missing follow-up dates)
# total_sample <- master_df_6 %>%
#   tally()

table2_time_to_rearrest_agency <- master_df_6 %>%
  count(agency, years_rearrest, name = "r") %>% 
  group_by(years_rearrest) %>% 
  mutate(r_year = sum(r)) %>% 
  ungroup() %>%   group_by(agency) %>%
  mutate(r_year_cum = cumsum(r_year),
         sample_size_agency = sum(r),
         prop_recidivate = r/sample_size_agency,
         prop_recidivate_pretty = percent(r/sample_size_agency, accuracy = 0.1),
         r_cum_agency = cumsum(r),
         prop_recidivate_cum = r_cum_agency/sample_size_agency,
         prop_recidivate_cum_pretty = percent(r_cum_agency/sample_size_agency, accuracy = 0.1)
  ) %>% 
  ungroup() %>%
  filter(!is.na(years_rearrest))

table2 <- table2_time_to_rearrest_agency %>% 
  select(agency, years_rearrest, r, sample_size_agency, r_cum_agency, prop_recidivate_cum, prop_recidivate_cum_pretty,
         r_year, r_year_cum) %>% 
  pivot_wider(id_cols = agency, names_from = years_rearrest, values_from = c(prop_recidivate_cum_pretty, r_cum_agency, sample_size_agency)) %>% 
  adorn_totals() %>% 
  mutate(prop_recidivate_cum_pretty_one = if_else(agency == "Total", 
                                                  percent(r_cum_agency_one/sample_size_agency_one, accuracy = 0.1), 
                                                  prop_recidivate_cum_pretty_one),
         prop_recidivate_cum_pretty_two = if_else(agency == "Total", 
                                                  percent(r_cum_agency_two/sample_size_agency_two, accuracy = 0.1), 
                                                  prop_recidivate_cum_pretty_two),
         prop_recidivate_cum_pretty_three = if_else(agency == "Total", 
                                                  percent(r_cum_agency_three/sample_size_agency_three, accuracy = 0.1), 
                                                  prop_recidivate_cum_pretty_three),
         "1-Year Follow-up Period" = paste0(prop_recidivate_cum_pretty_one, " (r = ", r_cum_agency_one, ", n = ", sample_size_agency_one,")"),
         "2-Year Follow-up Period" = paste0(prop_recidivate_cum_pretty_two, " (r = ", r_cum_agency_two, ", n = ", sample_size_agency_two,")"),
         "3-Year Follow-up Period" = paste0(prop_recidivate_cum_pretty_three, " (r = ", r_cum_agency_three, ", n = ", sample_size_agency_three,")")) %>% 
  select(agency, `1-Year Follow-up Period`, `2-Year Follow-up Period`, `3-Year Follow-up Period`)

table2_time_to_rearrest_year <- master_df_6 %>%
  count(agency, years_rearrest, name = "r") %>% 
  group_by(years_rearrest) %>%
  summarize(r_total_year = sum(r)) %>% 
  ungroup() %>% 
  mutate(r_total_year_cum = cumsum(r_total_year),
         total_sample_size_na = nrow(master_df_6),#this should match total_sample_size
         total_sample_size = sum(r_total_year), #this should match total_sample_size_na
         prop_recidivate_year = r_total_year/total_sample_size,
         prop_recidivate_year_cum = r_total_year_cum/total_sample_size) %>%
  filter(!is.na(years_rearrest))

# table2a <- table2_time_to_rearrest_year %>% 
#   mutate("Time-period" = case_when(years_rearrest == "one" ~ "< 12 months",
#                                     years_rearrest == "two" ~ "12-24 months",
#                                     years_rearrest == "three" ~ "24-36 months"),
#          Recidivists = r_total_year,
#          Percent = percent(prop_recidivate_year, accuracy = 0.1),
#          "Cumulative percent" = percent(prop_recidivate_year_cum, accuracy = 0.1)) %>% 
#   select(`Time-period`, Recidivists, Percent, `Cumulative percent`) %>% 
#   adorn_totals()

# table2a_caption <- paste0("Note: ", percent(table2a$Recidivists[1]/table2a$Recidivists[4], accuracy = 0.1),
#                           " of all offenders recidivated within 12 months from the start of supervision/release.")
table2a_caption <- paste0("Note: ", 
                          percent(table2_time_to_rearrest_year$r_total_year[1]/sum(table2_time_to_rearrest_year$r_total_year), accuracy = 0.1),
                          " of all offenders recidivated within 12 months from the start of supervision/release.")

figure2 <- ggplot() +
  geom_col(table2_time_to_rearrest_agency, mapping = aes(x = years_rearrest, y = prop_recidivate_cum, fill = agency), position = position_dodge()) +
  scale_x_discrete(name = "", labels = paste0(stringr::str_to_title(table2_time_to_rearrest_agency$years_rearrest), "-Year Follow-up Period",
                                              "\n # Recid = ",table2_time_to_rearrest_year$r_total_year_cum)) +
  scale_y_continuous(name = "Recidivism Rate", labels = percent_format(accuracy = 0.1), breaks = seq(0, 1, by = .1), limits = c(0,1)) +
  scale_fill_brewer(name = "Offender Type", type = "qual", palette = "Set3") +
  ggtitle(paste0("Recidivism Rates by Time-Period and Offender Type, FY ", fy, " Cohort")) +
  # geom_text(aes(label = prop_recidivate_pretty), nudge_y = -0.05, color = "white") +
  geom_errorbar(data = table2_time_to_rearrest_year, 
                mapping = aes(x = years_rearrest, ymin = prop_recidivate_year_cum, ymax = prop_recidivate_year_cum),
                linetype = 2, color = "red", linewidth = 1) +
  geom_text(data = table2_time_to_rearrest_year, 
            mapping = aes(x = years_rearrest, y = prop_recidivate_year_cum,
                          label = paste0("Avg. ", stringr::str_to_title(years_rearrest), 
                                         "-Year\nRecidivism Rate:", 
                                         percent(prop_recidivate_year_cum, accuracy = 0.1),")")), 
            nudge_y = .06,
            color = "black") +
  theme_bw() +
  NULL

#---------------------------------------------------------------------------------#
#ANALYSIS 3, 4, 5, 6 RR by time----
#NOTES: 
#1. see historical data extracted from Recidivism_study_2016_Final.pptx.ppt and stored in 
#---------------------------------------------------------------------------------#
hist_data <- read.csv("H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\FY17_recidivism_study\\historical_data_for_figs_3-6_2023-03-10.csv", header = FALSE)

hist_data_prob_par <- hist_data %>% 
  slice(1:2) %>%
  mutate(V1 = if_else(row_number() == 1, "year", V1)) %>%
  pivot_longer(!V1, names_to = "cat_test", values_to = "val_test")
hist_data_prob_par2 <- hist_data_prob_par[1:13,]
hist_data_prob_par3 <- hist_data_prob_par[14:26,] %>%
  inner_join(hist_data_prob_par2, by = "cat_test") %>%
  select(1, recid_rate = 3, 5) %>%
  separate(val_test.y, into = c("junk", "year", "n_tmp"), sep = " ", extra = "merge") %>%
  mutate(n = as.numeric(gsub("\\D","",n_tmp)),
         year = as.numeric(year),
         recid_rate = as.numeric(gsub("%","",recid_rate))) %>%
  select(1, 2, 4, 6)

hist_data_prob <- hist_data %>% 
  slice(4:5) %>%
  mutate(V1 = if_else(row_number() == 1, "year", V1)) %>%
  pivot_longer(!V1, names_to = "cat_test", values_to = "val_test")
hist_data_prob2 <- hist_data_prob[1:13,]
hist_data_prob3 <- hist_data_prob[14:26,] %>%
  inner_join(hist_data_prob2, by = "cat_test") %>%
  select(1, recid_rate = 3, 5) %>%
  separate(val_test.y, into = c("junk", "year", "n_tmp"), sep = " ", extra = "merge") %>%
  mutate(n = as.numeric(gsub("\\D","",n_tmp)),
         year = as.numeric(year),
         recid_rate = as.numeric(gsub("%","",recid_rate))) %>%
  select(1, 2, 4, 6)

hist_data_par <- hist_data %>% 
  slice(7:8) %>%
  mutate(V1 = if_else(row_number() == 1, "year", V1)) %>%
  pivot_longer(!V1, names_to = "cat_test", values_to = "val_test")
hist_data_par2 <- hist_data_par[1:13,]
hist_data_par3 <- hist_data_par[14:26,] %>%
  inner_join(hist_data_par2, by = "cat_test") %>%
  select(1, recid_rate = 3, 5) %>%
  separate(val_test.y, into = c("junk", "year", "n_tmp"), sep = " ", extra = "merge") %>%
  mutate(V1.x = gsub("\\*","",V1.x),
         n = as.numeric(gsub("\\D","",n_tmp)),
         year = as.numeric(year),
         recid_rate = as.numeric(gsub("%","",recid_rate))) %>%
  select(1, 2, 4, 6)

hist_data_max <- hist_data %>% 
  slice(10:11) %>%
  mutate(V1 = if_else(row_number() == 1, "year", V1)) %>%
  pivot_longer(!V1, names_to = "cat_test", values_to = "val_test")
hist_data_max2 <- hist_data_max[1:12,]
hist_data_max3 <- hist_data_max[14:25,] %>%
  inner_join(hist_data_max2, by = "cat_test") %>%
  select(1, recid_rate = 3, 5) %>%
  separate(val_test.y, into = c("junk", "year", "n_tmp"), sep = " ", extra = "merge") %>%
  mutate(n = as.numeric(gsub("\\D","",n_tmp)),
         year = as.numeric(year),
         recid_rate = as.numeric(gsub("%","",recid_rate))) %>%
  select(1, 2, 4, 6) 

hist_data_agg <- hist_data_prob_par3 %>% 
  bind_rows(hist_data_prob3, hist_data_par3, hist_data_max3) %>%
  rename(agency = V1.x)

rm(list = ls(pattern = "prob|par|max"))

new_data <- table1_recidivism_rate_agency %>%
  mutate(year = as.numeric(fy),
         prop_recidivate = round_half_up(100 * prop_recidivate, digits = 1)) %>%
  select(agency, n = sample_size, year, recid_rate = prop_recidivate)

new_data_prob_par <- new_data %>% 
  filter(agency %in% c("Probationers", "Parolees")) %>%
  summarize(agency = "Probation/Parole", n1 = sum(n), year = first(year), 
            recid_rate = round_half_up(sum(recid_rate*(n/n1)), digits = 1)) %>%
  rename(n = n1)

hist_and_new_data <- hist_data_agg %>%
  bind_rows(new_data, new_data_prob_par)

#method A
table3_2004 <- data.frame(year = 2004,
                          agency = "Probation/Parole")
#method B
# table3_2004 <- filter(hist_and_new_data, agency == "Probation/Parole") %>% 
#   slice(rep(1, 1)) %>% 
#   mutate(recid_rate = NA, 
#          year = 2004,
#          n = NA)
table3 <- filter(hist_and_new_data, agency == "Probation/Parole") %>% 
  bind_rows(table3_2004) %>% 
  arrange(year)

table4_2004 <- data.frame(year = 2004,
                          agency = "Probationers")
table4 <- filter(hist_and_new_data, agency == "Probationers") %>% 
  bind_rows(table4_2004) %>% 
  arrange(year)

table5_2004 <- data.frame(year = 2004,
                          agency = "Parolees")
table5 <- filter(hist_and_new_data, agency == "Parolees") %>% 
  bind_rows(table5_2004) %>% 
  arrange(year)

table6_2004 <- data.frame(year = c(2003,2004),
                          agency = "Maximum-Term Released Prisoners",
                          recid_rate = c(0,0))
table6 <- filter(hist_and_new_data, agency %in% c("Maximum-Term Released Prisoners","MTR-Prisoners")) %>% 
  bind_rows(table6_2004) %>% 
  mutate(agency = "Maximum-Term Released Prisoners") %>% 
  arrange(year)

figure3 <- ggplot(table3,
                  aes(x = year, y = recid_rate)) +
  geom_col(fill = "gray81") +
  scale_x_continuous(name = NULL, breaks = c(2003:2017), labels = paste0("FY",c(2003:2017),"\n",
                                                            "N = ", table3$n, "\n",
                                                            table3$recid_rate, "%")) +
  scale_y_continuous(name = "Recidivism Rate", labels = percent_format(scale = 1, accuracy = 0.1, suffix = "%"), breaks = seq(0, 100, by = 10), limits = c(0,100)) +
  ggtitle(paste0("Recidivism Rates for Probationers and Parolees, FY 2005 - ", fy, " Cohorts")) +
  geom_hline(yintercept = 63.3, color = "red", linewidth = 1, linetype = 2) +
  geom_hline(yintercept = 44.3, color = "black", linewidth = 1, linetype = 2) +
  annotate("text", label = paste0("FY 1999 Baseline Recividism Rate for Probationers/Parolees (63.3%)"),
           x = (max(hist_data_agg$year) + min(hist_data_agg$year))/2, y = 63.3 + 9, color = "black") +
  annotate("text", label = paste0("30% targeted reduction in recividism rate since the FY 1999 baseline (44.3%)"),
           x = (max(hist_data_agg$year) + min(hist_data_agg$year))/2, y = 44.3 + 9, color = "black") +
  theme_bw() +
  NULL

figure4 <- ggplot(table4,aes(x = year, y = recid_rate)) +
  geom_col(fill = "gray81") +
  scale_x_continuous(name = NULL, breaks = c(2003:2017), labels = paste0("FY",c(2003:2017),"\n",
                                                                         "N = ", table4$n, "\n",
                                                                         table4$recid_rate, "%")) +
  scale_y_continuous(name = "Recidivism Rate", labels = percent_format(scale = 1, accuracy = 0.1, suffix = "%"), breaks = seq(0, 100, by = 10), limits = c(0,100)) +
  ggtitle(paste0("Recidivism Rates for Probationers, FY 2005 - ", fy, " Cohorts")) +
  geom_hline(yintercept = 53.7, color = "red", linewidth = 1, linetype = 2) +
  geom_hline(yintercept = 37.6, color = "black", linewidth = 1, linetype = 2) +
  annotate("text", label = paste0("FY 1999 Baseline Recividism Rate for Probationers (53.7%)"),
           x = (max(hist_data_agg$year) + min(hist_data_agg$year))/2, y = 53.7 + 9, color = "black") +
  annotate("text", label = paste0("30% targeted reduction in recividism rate since the FY 1999 baseline (37.6%)"),
           x = (max(hist_data_agg$year) + min(hist_data_agg$year))/2, y = 37.6 + 9, color = "black") +
  theme_bw() +
  NULL

figure5 <- ggplot(table5, aes(x = year, y = recid_rate)) +
  geom_col(fill = "gray81") +
  scale_x_continuous(name = NULL, breaks = c(2003:2017), labels = paste0("FY",c(2003:2017),"\n",
                                                                         "N = ", table5$n, "\n",
                                                                         table5$recid_rate, "%")) +
  scale_y_continuous(name = "Recidivism Rate", labels = percent_format(scale = 1, accuracy = 0.1, suffix = "%"), breaks = seq(0, 100, by = 10), limits = c(0,100)) +
  ggtitle(paste0("Recidivism Rates for Parolees, FY 2005 - ", fy, " Cohorts")) +
  geom_hline(yintercept = 72.9, color = "red", linewidth = 1, linetype = 2) +
  geom_hline(yintercept = 51.0, color = "black", linewidth = 1, linetype = 2) +
  annotate("text", label = paste0("FY 1999 Baseline Recividism Rate for Parolees (72.9%)"),
           x = (max(hist_data_agg$year) + min(hist_data_agg$year))/2, y = 72.9 + 9, color = "black") +
  annotate("text", label = paste0("30% targeted reduction in recividism rate since the FY 1999 baseline (51.0%)"),
           x = (max(hist_data_agg$year) + min(hist_data_agg$year))/2, y = 51.0 + 9, color = "black") +
  theme_bw() +
  NULL

figure6 <- ggplot(table6, aes(x = year, y = recid_rate)) +
  geom_col(fill = "gray81") +
  scale_x_continuous(name = NULL, breaks = c(2003:2017), labels = paste0("FY",c(2003:2017),"\n",
                                                                         "N = ", table6$n, "\n",
                                                                         table6$recid_rate, "%")) +
  scale_y_continuous(name = "Recidivism Rate", labels = percent_format(scale = 1, accuracy = 0.1, suffix = "%"), breaks = seq(0, 100, by = 10), limits = c(0,100)) +
  ggtitle(paste0("Recidivism Rates for Maximum Term Released Prisoners, FY 2005 - ", fy, " Cohorts")) +
  geom_hline(yintercept = 76.1, color = "red", linewidth = 1, linetype = 2) +
  geom_hline(yintercept = 53.3, color = "black", linewidth = 1, linetype = 2) +
  annotate("text", label = paste0("FY 1999 Baseline Recividism Rate for Maximum Term Released Prisoners (76.1%)"),
           x = (max(hist_data_agg$year) + min(hist_data_agg$year))/2, y = 76.1 + 9, color = "black") +
  annotate("text", label = paste0("30% targeted reduction in recividism rate since the FY 1999 baseline (53.3%)"),
           x = (max(hist_data_agg$year) + min(hist_data_agg$year))/2, y = 53.3 + 9, color = "black") +
  theme_bw() +
  NULL

#---------------------------------------------------------------------------------#
#ANALYSIS 7, 8, 9 Disposition Status and Criminal Reconviction Rate----
#NOTES:
#1.
#---------------------------------------------------------------------------------#
# #investigate dispo codes
# tmp <- master_df_6 %>%
#   distinct(DISPOSITION) %>%
#   arrange(DISPOSITION)
# 
# #compare to FY16
# tmp_fy16 <- CJIS_FY16 %>%
#   count(DISPOSITIONDESCRIPTION)
# 
# #investigate dispo codes, with counts
# tmp <- master_df_6 %>%
#   count(DISPOSITION)
# 
# #investigate dispo codes, by agency
# tmp2 <- distinct(master_df_6,agency,DISPOSITION)

table7_8_9_dispo <- master_df_6 %>%
  count(agency, DISPOSITIONDESCRIPTION_rc, name = "r") %>%
  group_by(agency) %>%
  mutate(sample_size = sum(r),
         DISPOSITIONDESCRIPTION_rc = factor(DISPOSITIONDESCRIPTION_rc, levels = rev(c("Guilty", "Pending - Late pretrial", "Pending - Early pretrial", "Acquitted", "Other", "none")))) %>%
  ungroup() %>%
  # filter(!DISPOSITIONDESCRIPTION_rc %in% c("none", "other")) %>%
  filter(!DISPOSITIONDESCRIPTION_rc %in% c("none")) %>% #nones are non-recidivists, exclude
  mutate(freq = round_half_up(r/sample_size, digits = 3),
         freq_pretty = percent(freq, accuracy = .1))

table7_8_9 <- table7_8_9_dispo %>% 
  pivot_wider(id_cols = DISPOSITIONDESCRIPTION_rc, names_from = agency, values_from = c(r, sample_size, freq)) %>% 
  mutate(Probationers = paste0(percent(freq_Probationers, accuracy = 0.1), " (r = ", r_Probationers,")"),
         Parolees = paste0(percent(freq_Parolees, accuracy = 0.1), " (r = ", r_Parolees,")"),
         `MTR-Prisoners` = paste0(percent(`freq_MTR-Prisoners`, accuracy = 0.1), " (r = ", `r_MTR-Prisoners`,")")
         ) %>% 
  select(Disposition = DISPOSITIONDESCRIPTION_rc,Probationers, Parolees, `MTR-Prisoners`) %>% 
  arrange(Disposition) %>% 
  mutate(Disposition = case_when(Disposition == "pendingB" ~ "pending - late pretrial",
                                 Disposition == "pendingA" ~ "pending - early pretrial",
                                 TRUE ~ Disposition))
  
table7_8_9_dispo_agency <- table7_8_9_dispo %>%
  group_by(agency) %>%
  summarize(sample_size_r = sum(r),
            sample_size_n = first(sample_size)) %>% 
  ungroup()

figure7_8_9 <- ggplot(table7_8_9_dispo, aes(x = agency, y = freq, fill = DISPOSITIONDESCRIPTION_rc)) +
  # geom_col(position = position_dodge2()) + #use this to create regular bar chart
  geom_col() + #use this to create stacked bar chart
  scale_x_discrete(name = NULL, labels = paste0(table7_8_9_dispo_agency$agency, 
                                                "\n(r = ", table7_8_9_dispo_agency$sample_size_r, ", n = ", table7_8_9_dispo_agency$sample_size_n,")")) +
  scale_y_continuous(name = "% of Reoffenders", labels = percent_format(accuracy = 0.1), breaks = seq(0, 1, by = .1), limits = c(0,1.1)) +
  scale_fill_brewer(name = "Disposition", type = "seq", palette = 8) +
  # scale_fill_brewer(name = "Disposition", labels = c("other", "acquitted", "pending - late pretrial", "pending - early pretrial", "guility"), type = "seq", palette = 8) +
  ggtitle(paste0("Percent of Reoffenders by Disposition Status and Offender Type, FY ", fy, " Cohort")) +
  theme_bw() + #facet_wrap(~agency) 
  NULL

caption7_8_9_dispo_collapsed <- master_df_6 %>% 
  distinct(DISPOSITIONDESCRIPTION_rc, DISPOSITION) %>% #collapse duplicate disposition values within each category
  group_by(DISPOSITIONDESCRIPTION_rc) %>% 
  summarize(disposition_list = paste0(DISPOSITION, collapse = "; "))

caption7_8_9 <- paste0("In previous recidivism reports, dispositions were categorized manually. In this report, ",
                            "pattern matching was used to categorize automatically, which was then checked for accuracy. ",
                            "The following rules were used:<br><br>",
                       caption7_8_9_dispo_collapsed[1,1],": ",caption7_8_9_dispo_collapsed[1,2],"<br>",
                       caption7_8_9_dispo_collapsed[2,1],": ",caption7_8_9_dispo_collapsed[2,2],"<br>",
                       caption7_8_9_dispo_collapsed[3,1],": ",caption7_8_9_dispo_collapsed[3,2],"<br>",
                       caption7_8_9_dispo_collapsed[4,1],": ",caption7_8_9_dispo_collapsed[4,2],"<br>",
                       caption7_8_9_dispo_collapsed[5,1],": ",caption7_8_9_dispo_collapsed[5,2],"<br>")
                       #---------------------------------------------------------------------------------#
#ANALYSIS 10 RR by Offender Type and Recidivism Type----
#NOTES:
#1. Tim's phi coefff for arrests seems off?
#---------------------------------------------------------------------------------#
table10_recidivism_type <- master_df_6 %>%
  count(agency, ICIS_arrest, name = "r") %>%
  full_join(agency_factors_x_ICIS_arrest_factors) %>% 
  replace_na(list(r = 0)) %>% 
  group_by(agency) %>%
  mutate(sample_size = sum(r, na.rm = TRUE)) %>% 
  ungroup() %>%
  filter(ICIS_arrest != "No Rearrest") %>%
  mutate(prop_recidivate = round_half_up(r/sample_size, digits = 3),
         prop_recidivate_pretty = percent(r/sample_size, accuracy = 0.1))

table10 <- table10_recidivism_type %>% 
  pivot_wider(id_cols = ICIS_arrest, names_from = agency, values_from = c(prop_recidivate, r, sample_size)) %>% 
  adorn_totals() %>% 
  # adorn_totals("row",,,,-c(sample_size_Probationers, sample_size_Parolees, `sample_size_MTR-Prisoners`)) %>% 
  mutate(prop_recidivate_Probationers = if_else(ICIS_arrest == "Total", 
                                                r_Probationers/sample_size_Probationers[1],
                                                prop_recidivate_Probationers),
         prop_recidivate_Parolees = if_else(ICIS_arrest == "Total", 
                                            r_Parolees/sample_size_Parolees[1],
                                            prop_recidivate_Parolees),
         `prop_recidivate_MTR-Prisoners` = if_else(ICIS_arrest == "Total", 
                                                   `r_MTR-Prisoners`/`sample_size_MTR-Prisoners`[1],
                                                   `prop_recidivate_MTR-Prisoners`),
         Probationers = paste0(percent(prop_recidivate_Probationers, accuracy = 0.1), " (r = ", r_Probationers,")"),
         Parolees = paste0(percent(prop_recidivate_Parolees, accuracy = 0.1), " (r = ", r_Parolees,")"),
         `MTR-Prisoners` = paste0(percent(`prop_recidivate_MTR-Prisoners`, accuracy = 0.1), " (r = ", `r_MTR-Prisoners`,")")) %>% 
  select(ICIS_arrest, Probationers, Parolees, `MTR-Prisoners`)

table10_recidivism_type_agency <- table10_recidivism_type %>%
  group_by(agency) %>%
  summarize(r = sum(r), sample_size = first(sample_size), prop_recidivate = r/sample_size, prop_recidivate_pretty = percent(prop_recidivate, accuracy = 0.1))

#___QA check table1_recidivism_rate_agency and table10_recidivism_type_agency 10 match----
table1_recidivism_rate_agency %>% 
  select(agency, r, sample_size, prop_recidivate, prop_recidivate_pretty) %>% 
  all_equal(table10_recidivism_type_agency)

table10a <- table10_recidivism_type %>% 
  group_by(ICIS_arrest) %>%
  summarize(r = sum(r), sample_size = sum(table10_recidivism_type_agency$sample_size), recidivism_rate = round_half_up(r/sample_size, digits = 3), recidivism_rate_pretty = percent(recidivism_rate, accuracy = 0.1)) %>% 
  #potentially need to fix Other Violations if sample_size doesn't include all 4 recidivism types (e.g., if "Other Violations" has 0 cases for an agency)
  select(`Recidivism type` = ICIS_arrest, r, sample_size, Recidivism.rate = recidivism_rate_pretty)

stat10_recidivism_type_rearrest <- master_df_6 %>%
  mutate(ICIS_arrest = if_else(ICIS_arrest == "Criminal Arrest", 1, 0))
stat10_chi_recidivism_type_rearrest <- chisq.test(stat10_recidivism_type_rearrest$agency, stat10_recidivism_type_rearrest$ICIS_arrest)
stat10_phi_recidivism_type_rearrest <- sqrt(stat10_chi_recidivism_type_rearrest$statistic/sum(table10_recidivism_type_agency$sample_size))
summary(table(master_df_6$ICIS_arrest, master_df_6$Rearrest)) 
stat10a_p_value <- ifelse(signif_half_up(stat10_chi_recidivism_type_rearrest$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat10_chi_recidivism_type_rearrest$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat10a_description <- if_else(stat10_chi_recidivism_type_rearrest$p.value < .05,
                             "There was a statistically significant difference in criminal arrests between offender types, ",
                             "The difference in criminal arrests between offender types was not statistically significant, ")
stat10a <- paste0(stat10a_description,
                  "$\\chi^2$(", stat10_chi_recidivism_type_rearrest$parameter, ", <em>N</em> = ", sum(table10_recidivism_type_agency$sample_size),
                ") = ", round_half_up(stat10_chi_recidivism_type_rearrest$statistic, digits = 2), 
                stat10a_p_value)

# chisq_to_fei(chisq = stat10_chi_recidivism_type_rearrest$statistic, p = c(.33, .33, .33), n = sum(table10_recidivism_type_agency$sample_size), nrow = 1, ncol = 3)

stat10_recidivism_type_revoc <- master_df_6 %>%
  mutate(ICIS_arrest = if_else(ICIS_arrest == "Probation Revocations/Parole Violations", 2, 0))
stat10_chi_recidivism_type_revoc <- chisq.test(stat10_recidivism_type_revoc$agency, stat10_recidivism_type_revoc$ICIS_arrest)
# stat10_phi_recidivism_type_revoc <- sqrt(stat10_chi_recidivism_type_revoc$statistic/sum(table10_recidivism_type_agency$sample_size))
summary(table(master_df_6$ICIS_arrest, master_df_6$Rearrest)) 
stat10b_p_value <- ifelse(signif_half_up(stat10_chi_recidivism_type_revoc$p.value, digits = 3) >= .001, 
                         paste0(", <em>p</em> = ", signif_half_up(stat10_chi_recidivism_type_revoc$p.value, digits = 3)), 
                         paste0(", <em>p</em> < .001"))
stat10b_description <- if_else(stat10_chi_recidivism_type_revoc$p.value < .05,
                               "There was a statistically significant difference in probation revocations/parole violations between offender types, ",
                               "The difference in probation revocations/parole violations between offender types was not statistically significant, ")
stat10b <- paste0(stat10b_description,
                  "$\\chi^2$(", stat10_chi_recidivism_type_revoc$parameter, ", <em>N</em> = ", sum(table10_recidivism_type_agency$sample_size),
                  ") = ", round_half_up(stat10_chi_recidivism_type_revoc$statistic, digits = 2), 
                  stat10b_p_value)

stat10_recidivism_type_contempt <- master_df_6 %>%
  mutate(ICIS_arrest = if_else(ICIS_arrest == "Criminal Contempt of Court", 3, 0))
stat10_chi_recidivism_type_contempt <- chisq.test(stat10_recidivism_type_contempt$agency, stat10_recidivism_type_contempt$ICIS_arrest)
# stat10_phi_recidivism_type_contempt <- sqrt(stat10_chi_recidivism_type_contempt$statistic/sum(table10_recidivism_type_agency$sample_size))
summary(table(master_df_6$ICIS_arrest, master_df_6$Rearrest)) 
stat10c_p_value <- ifelse(signif_half_up(stat10_chi_recidivism_type_contempt$p.value, digits = 3) >= .001, 
                         paste0(", <em>p</em> = ", signif_half_up(stat10_chi_recidivism_type_contempt$p.value, digits = 3)), 
                         paste0(", <em>p</em> < .001"))
stat10c_description <- if_else(stat10_chi_recidivism_type_contempt$p.value < .05,
                               "There was a statistically significant difference in criminal contempt of court between offender types, ",
                               "The difference in criminal contempt of court between offender types was not statistically significant, ")
stat10c <- paste0(stat10c_description,
                  "$\\chi^2$(", stat10_chi_recidivism_type_contempt$parameter, ", <em>N</em> = ", sum(table10_recidivism_type_agency$sample_size),
                  ") = ", round_half_up(stat10_chi_recidivism_type_contempt$statistic, digits = 2), 
                  stat10c_p_value)

figure10 <- ggplot(table10_recidivism_type, aes(x = agency, y = prop_recidivate, fill = ICIS_arrest)) +
  geom_col() +
  # geom_text(data = recidivism_type_agency,
  #           mapping = aes(x = agency, y = prop_recidivate,
  #                         label =  percent(prop_recidivate, accuracy = 0.1)),
  #           nudge_y = .06,
  #           color = "black") +
  annotate(geom = "text", x = 1, y = table10_recidivism_type_agency$prop_recidivate[1] + .05, label = percent(table10_recidivism_type_agency$prop_recidivate[1], accuracy = 0.1, suffix = "%")) +
  annotate(geom = "text", x = 2, y = table10_recidivism_type_agency$prop_recidivate[2] + .05, label = percent(table10_recidivism_type_agency$prop_recidivate[2], accuracy = 0.1, suffix = "%")) +
  annotate(geom = "text", x = 3, y = table10_recidivism_type_agency$prop_recidivate[3] + .05, label = percent(table10_recidivism_type_agency$prop_recidivate[3], accuracy = 0.1, suffix = "%")) +
  scale_x_discrete(name = NULL, labels = paste0(table10_recidivism_type_agency$agency, " (n = ", table10_recidivism_type_agency$sample_size, ")")) +
  scale_y_continuous(name = "Recidivism Rate", labels = percent_format(accuracy = 0.1), breaks = seq(0, 1, by = .1), limits = c(0,1)) +
  scale_fill_brewer(name = "Recidivism Type", type = "seq", direction = -1) +
  ggtitle(paste0("Recidivism Rates by Recidivism Type and Offender Type, FY ", fy, " Cohort")) +
  theme_bw() +
  NULL

caption10 <- paste0("The recidivism types were determined by reverse engineering the mappings from the FY 2016 report. ",
                    "The following mappings were used:")
#---------------------------------------------------------------------------------#
#ANALYSIS 11 RR by Offender Type and County----
#NOTES:
#1. Tim's County total Ns are incorrect (confirmed by script but also by adding together the n's from the agencys != county n)
#2. Tim's phi coefficient for parolees is incorrect (appears to be copying error from max terms?); p-value also incorrect
#3. The statewide RR is different because of 17 individuals without a County designation, so use the statewide # from Fig 1
#---------------------------------------------------------------------------------#
table11_recidivism_rate_agency_county <- master_df_6 %>%
  count(agency, Rearrest, county, name = "r")  %>% 
  full_join(agency_x_county_factors) %>%  #add any missing levels of agency or county
  replace_na(list(Rearrest = 1, r = 0)) %>% 
  group_by(agency) %>%
  mutate(sample_size_agency = sum(r, na.rm = TRUE)) %>% 
  ungroup() %>%
  group_by(county) %>%
  mutate(sample_size_county = sum(r)) %>%
  ungroup() %>%
  group_by(agency, county) %>%
  mutate(sample_size_agency_county = sum(r)) %>%
  ungroup() %>%
  filter(Rearrest == 1) %>%
  mutate(recidivism_rate = round_half_up(r/sample_size_agency_county, digits = 3),
         recidivism_rate_pretty = percent(recidivism_rate, accuracy = 0.1))

table11 <- table11_recidivism_rate_agency_county %>% 
  pivot_wider(id_cols = county, names_from = agency, values_from = c(r, sample_size_county, sample_size_agency_county, recidivism_rate_pretty)) %>% 
  adorn_totals %>% 
  mutate(recidivism_rate_pretty_Probationers = if_else(county == "Total", 
                                                percent(r_Probationers/sample_size_agency_county_Probationers, accuracy = 0.1),
                                                recidivism_rate_pretty_Probationers),
         recidivism_rate_pretty_Parolees = if_else(county == "Total", 
                                            percent(r_Parolees/sample_size_agency_county_Parolees, accuracy = 0.1),
                                            recidivism_rate_pretty_Parolees),
         `recidivism_rate_pretty_MTR-Prisoners` = if_else(county == "Total", 
                                                   percent(`r_MTR-Prisoners`/`sample_size_agency_county_MTR-Prisoners`, accuracy = 0.1),
                                                   `recidivism_rate_pretty_MTR-Prisoners`),
         #to get sample size for counties, look across 3 agency cols to find non-NAs, and also sum these to get Total
         sample_size_county = case_when(county == "Total" ~ 0,
                                        !is.na(sample_size_county_Probationers) ~ sample_size_county_Probationers,
                                        is.na(sample_size_county_Probationers) ~ sample_size_county_Parolees
                                        ),
         sample_size_county = if_else(county == "Total", sum(sample_size_county), sample_size_county),
         Probationers = paste0(recidivism_rate_pretty_Probationers, " (r = ", r_Probationers,", n = ",sample_size_agency_county_Probationers,")"),
         Parolees = paste0(recidivism_rate_pretty_Parolees, " (r = ", r_Parolees,", n = ",sample_size_agency_county_Parolees,")"),
         `MTR-Prisoners` = paste0(`recidivism_rate_pretty_MTR-Prisoners`, " (r = ", `r_MTR-Prisoners`,", n = ",`sample_size_agency_county_MTR-Prisoners`,")"),
         county_factor = county, #keep this so that we can sort the df by county and it appears in the proper order (otherwise Unknown appears before Other)
         county = paste0(county, " (N = ", sample_size_county, ")")) %>% 
  arrange(county_factor) %>% 
  select(county, Probationers, Parolees, `MTR-Prisoners`)

table11_recidivism_rate_agency <- table11_recidivism_rate_agency_county %>%
  group_by(agency) %>%
  summarize(r = sum(r, na.rm = TRUE), sample_size = first(sample_size_agency), recidivism_rate = round_half_up(r/sample_size, digits = 3))
table11a <- table11_recidivism_rate_agency_county %>%
  group_by(county) %>%
  summarize(r = sum(r, na.rm = TRUE), sample_size = sum(sample_size_agency_county, na.rm = TRUE), recidivism_rate = round_half_up(r/sample_size, digits = 3)) %>%
  adorn_totals() %>%
  #this is the correct formula, but because of 17 individuals without a County designation, there is a shift in the statewide % so use the statewide # from Fig 1
  # mutate(recidivism_rate = percent(if_else(County == "Total", round_half_up(n/sample_size, digits = 3), recidivism_rate), accuracy = 0.1))
  mutate(recidivism_rate = percent(if_else(county == "Total", r/sample_size, recidivism_rate), accuracy = 0.1))

stat11_county_all <- master_df_6 %>%
  filter(!is.na(Rearrest))
stat11_chi_agency_county_all <- chisq.test(stat11_county_all$Rearrest, stat11_county_all$county)
# phi_agency_county_all <- sqrt(chi_agency_county_all$statistic/table11a$sample_size[table11a$county == "Total"])
stat11_n_all <- table11a$sample_size[table11a$county == "Total"]
stat11aa_p_value <- ifelse(signif_half_up(stat11_chi_agency_county_all$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat11_chi_agency_county_all$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat11aa_description <- if_else(stat11_chi_agency_county_all$p.value < .05,
                               "There was a statistically significant difference in overall recidivism rates between counties, ",
                               "The difference in overall recidivism rates between counties was not statistically significant, ")
stat11aa <- paste0(stat11aa_description,
                  "$\\chi^2$(", stat11_chi_agency_county_all$parameter, ", <em>N</em> = ", stat11_n_all,
                  ") = ", round_half_up(stat11_chi_agency_county_all$statistic, digits = 2), 
                  stat11aa_p_value)

stat11_county_prob <- master_df_6 %>%
  filter(!is.na(Rearrest), agency == "Probationers")
stat11_chi_agency_county_prob <- chisq.test(stat11_county_prob$Rearrest, stat11_county_prob$county)
stat11_n_prob <- table11_recidivism_rate_agency$sample_size[table11_recidivism_rate_agency$agency == "Probationers"]
# stat11_phi_agency_county_prob <- sqrt(stat11_chi_agency_county_prob$statistic/stat11_n_prob)
stat11a_p_value <- ifelse(signif_half_up(stat11_chi_agency_county_prob$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat11_chi_agency_county_prob$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat11a_description <- if_else(stat11_chi_agency_county_prob$p.value < .05,
                               "There was a statistically significant difference in recidivism rates between counties for probationers, ",
                               "The difference in recidivism rates between counties for probationers was not statistically significant, ")
stat11a <- paste0(stat11a_description,
                  "$\\chi^2$(", stat11_chi_agency_county_prob$parameter, ", <em>N</em> = ", stat11_n_prob,
                  ") = ", round_half_up(stat11_chi_agency_county_prob$statistic, digits = 2), 
                  stat11a_p_value)

stat11_county_parole <- master_df_6 %>%
  filter(!is.na(Rearrest), agency == "Parolees")
stat11_chi_agency_county_parole <- chisq.test(stat11_county_parole$Rearrest, stat11_county_parole$county)
stat11_n_par <- table11_recidivism_rate_agency$sample_size[table11_recidivism_rate_agency$agency == "Parolees"]
# phi_agency_county_parole <- sqrt(chi_agency_county_parole$statistic/531)
stat11b_p_value <- ifelse(signif_half_up(stat11_chi_agency_county_parole$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat11_chi_agency_county_parole$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat11b_description <- if_else(stat11_chi_agency_county_parole$p.value < .05,
                               "There was a statistically significant difference in recidivism rates between counties for parolees, ",
                               "The difference in recidivism rates between counties for parolees was not statistically significant, ")
stat11b <- paste0(stat11b_description,
                  "$\\chi^2$(", stat11_chi_agency_county_parole$parameter, ", <em>N</em> = ", stat11_n_par,
                  ") = ", round_half_up(stat11_chi_agency_county_parole$statistic, digits = 2), 
                  stat11b_p_value)

stat11_county_max <- master_df_6 %>%
  filter(!is.na(Rearrest), agency == "MTR-Prisoners")
stat11_chi_agency_county_max <- chisq.test(stat11_county_max$Rearrest, stat11_county_max$county)
stat11_n_max <- table11_recidivism_rate_agency$sample_size[table11_recidivism_rate_agency$agency == "MTR-Prisoners"]
# phi_agency_county_max <- sqrt(chi_agency_county_max$statistic/317)
stat11c_p_value <- ifelse(signif_half_up(stat11_chi_agency_county_max$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat11_chi_agency_county_max$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat11c_description <- if_else(stat11_chi_agency_county_max$p.value < .05,
                               "There was a statistically significant difference in recidivism rates between counties for MTR-prisoners, ",
                               "The difference in recidivism rates between counties for MTR-prisoners was not statistically significant, ")
stat11c <- paste0(stat11c_description,
                  "$\\chi^2$(", stat11_chi_agency_county_max$parameter, ", <em>N</em> = ", stat11_n_max,
                  ") = ", round_half_up(stat11_chi_agency_county_max$statistic, digits = 2), 
                  stat11c_p_value)

figure11 <- ggplot() +
  geom_col(table11_recidivism_rate_agency_county, mapping = aes(x = agency, y = recidivism_rate, fill = county), position = position_dodge()) +
  geom_errorbar(data = table11_recidivism_rate_agency,
                mapping = aes(x = agency, ymin = recidivism_rate, ymax = recidivism_rate),
                linetype = 2, color = "red", linewidth = 1) +
  annotate(geom = "text", x = 1, y = table11_recidivism_rate_agency$recidivism_rate[1] + .05, label = percent(table11_recidivism_rate_agency$recidivism_rate[1], accuracy = 0.1, suffix = "%")) +
  annotate(geom = "text", x = 2, y = table11_recidivism_rate_agency$recidivism_rate[2] + .05, label = percent(table11_recidivism_rate_agency$recidivism_rate[2], accuracy = 0.1, suffix = "%")) +
  annotate(geom = "text", x = 3, y = table11_recidivism_rate_agency$recidivism_rate[3] + .05, label = percent(table11_recidivism_rate_agency$recidivism_rate[3], accuracy = 0.1, suffix = "%")) +
  scale_x_discrete(name = NULL, labels = paste0(table11_recidivism_rate_agency$agency, " (n = ", table11_recidivism_rate_agency$sample_size, ")")) +
  scale_y_continuous(name = "Recidivism Rate", labels = percent_format(accuracy = 0.1), breaks = seq(0, 1, by = .1), limits = c(0,1)) +
  scale_fill_brewer(name = "County", type = "qual") +
  ggtitle(paste0("Recidivism Rates by County and Offender Type, FY ", fy, " Cohort")) +
  theme_bw() +
  NULL

caption11 <- paste0("County was determined by using information from the source agency. ", 
                    "Location data from CJIS was not used because it reflected the most recent address on record for the",
                    "defendant and is likely less accurate than the data from the source agencies, which reflects the",
                    "the location at the time of their supervision or release. ",
                    "The ACSB/probation database stored the county at the time of supervision. ",
                    "HPA provided the county or city where the offender was released. Cities were recoded to counties. ",
                    "Finally, DPS provided the last correctional center the defendant was located prior to release, which",
                    "were recoded to counties.")
                    # "County information from source agencies were weighted more heavily (2:1) than information from CJIS because",
                    # "CJIS zipcodes come from the most current address on record whereas the source agency information is close",
                    # "to the time of release. If the source agency and CJIS agreed, then that location was used. If they disagreed, then")
#---------------------------------------------------------------------------------#
#ANALYSIS 12 RR by County and Recidivism Type for Probationers----
#NOTES:
#1. #s match Tim's SPSS output but not his ICIS report
#---------------------------------------------------------------------------------#
table12_recidivism_type_county_prob <- master_df_6 %>%
  filter(!is.na(Rearrest), agency == "Probationers") %>% #must remove NAs so that totals match Tim's -> SPSS rmoves the missing values (see "Missing Value Handling -> Definition of Missing in spv file)
  count(county, ICIS_arrest, name = "r") %>% 
  full_join(ICIS_arrest_x_county_factors) %>% #add any missing levels of ICIS_arrest or county
  replace_na(list(r = 0)) %>% 
  group_by(county) %>%
  mutate(sample_size = sum(r)) %>% 
  ungroup() %>%
  # filter(ICIS_arrest != "No Rearrest") %>% #FY16
  filter(!is.na(ICIS_arrest)) %>% #FY17
  mutate(prop_recidivate = round_half_up(r/sample_size, digits = 3),
         prop_recidivate_pretty = percent(r/sample_size, accuracy = 0.1)) 

table12_county_prob <- table12_recidivism_type_county_prob %>%
  group_by(county) %>%
  summarize(r = sum(r, na.rm = TRUE), sample_size = first(sample_size), prop_recidivate = sum(prop_recidivate, na.rm = TRUE), prop_recidivate_pretty = percent(prop_recidivate, accuracy = 0.1)) %>%
  adorn_totals() %>%
  mutate(prop_recidivate = if_else(county == "Total", r/sample_size, prop_recidivate))

#extract prob sample size
# #method A
# table12_probation_sample_size <- table12_county_prob %>% 
#   filter(county=="Total") %>% 
#   select(sample_size)
# #method B
# table12_probation_sample_size <- table12_county_prob[table12_county_prob$county=="Total","sample_size"] 
#method C
table12_probation_sample_size <- table12_county_prob$sample_size[table12_county_prob$county=="Total"]

table12 <- table12_recidivism_type_county_prob %>% 
  pivot_wider(id_cols = ICIS_arrest, names_from = county, values_from = c(r, sample_size, prop_recidivate_pretty)) %>% 
  adorn_totals() %>% 
  mutate(prop_recidivate_pretty_Kauai = if_else(ICIS_arrest == "Total", 
                                                       percent(r_Kauai/sample_size_Kauai[1], accuracy = 0.1),
                                                prop_recidivate_pretty_Kauai),
         prop_recidivate_pretty_Maui = if_else(ICIS_arrest == "Total", 
                                                percent(r_Maui/sample_size_Maui[1], accuracy = 0.1),
                                               prop_recidivate_pretty_Maui),
         prop_recidivate_pretty_Honolulu = if_else(ICIS_arrest == "Total", 
                                                percent(r_Honolulu/sample_size_Honolulu[1], accuracy = 0.1),
                                                prop_recidivate_pretty_Honolulu),
         prop_recidivate_pretty_Hawaii = if_else(ICIS_arrest == "Total", 
                                                percent(r_Hawaii/sample_size_Hawaii[1], accuracy = 0.1),
                                                prop_recidivate_pretty_Hawaii),
         prop_recidivate_pretty_Other = if_else(ICIS_arrest == "Total", 
                                                percent(r_Other/sample_size_Other[1], accuracy = 0.1),
                                                prop_recidivate_pretty_Other),
         prop_recidivate_pretty_Unknown = if_else(ICIS_arrest == "Total", 
                                                percent(r_Unknown/sample_size_Unknown[1], accuracy = 0.1),
                                                prop_recidivate_pretty_Unknown),
         Kauai = paste0(prop_recidivate_pretty_Kauai, " (r = ", r_Kauai,", n = ",sample_size_Kauai[1],")"),
         Maui = paste0(prop_recidivate_pretty_Maui, " (r = ", r_Maui,", n = ",sample_size_Maui[1],")"),
         Honolulu = paste0(prop_recidivate_pretty_Honolulu, " (r = ", r_Honolulu,", n = ",sample_size_Honolulu[1],")"),
         Hawaii = paste0(prop_recidivate_pretty_Hawaii, " (r = ", r_Hawaii,", n = ",sample_size_Hawaii[1],")"),
         Other = paste0(prop_recidivate_pretty_Other, " (r = ", r_Other,", n = ",sample_size_Other[1],")"),
         Unknown = paste0(prop_recidivate_pretty_Unknown, " (r = ", r_Unknown,", n = ",sample_size_Unknown[1],")")) %>% 
  rowwise(ICIS_arrest) %>% 
  mutate(ICIS_arrest_R = sum(c_across(r_Kauai:r_Unknown), na.rm = TRUE),
         ICIS_arrest_N = table12_probation_sample_size,
         ICIS_arrest = paste0(ICIS_arrest, 
                              " (R = ", ICIS_arrest_R,
                              ", N = ", ICIS_arrest_N,")")) %>% 
  ungroup() %>% 
  select(ICIS_arrest, Kauai, Maui, Honolulu, Hawaii, Other, Unknown)

table12a <- table12_recidivism_type_county_prob %>%
  group_by(ICIS_arrest) %>%
  summarize(r = sum(r, na.rm = TRUE), sample_size = sum(sample_size, na.rm = TRUE), recidivism_rate = round_half_up(r/sample_size, digits = 3), recidivism_rate_pretty = percent(recidivism_rate, accuracy = 0.1))
#potentially need to fix Other Violations if sample_size doesn't include all 4 recidivism types (e.g., if "Other Violations" has 0 cases for an agency)

#this matches the crosstab for County * ICIS_arrest in Tim's Recidivism Study Tables 2019 output
stat12aa_county_ICIS_arrest_prob <- master_df_6 %>%
  filter(!is.na(Rearrest), agency == "Probationers") # %>%
# mutate(ICIS_arrest = if_else(ICIS_arrest == 1, 1, 0))
stat12aa_chi_recidivism_type_county_ICIS_arrest_prob <- chisq.test(stat12aa_county_ICIS_arrest_prob$county, stat12aa_county_ICIS_arrest_prob$ICIS_arrest)
# phi_recidivism_type_county_ICIS_arrest_prob <- sqrt(stat12aa_chi_recidivism_type_county_ICIS_arrest_prob$statistic/table12_probation_sample_size)
stat12aa_n <- table12_probation_sample_size
stat12aa_p_value <- ifelse(signif_half_up(stat12aa_chi_recidivism_type_county_ICIS_arrest_prob$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat12aa_chi_recidivism_type_county_ICIS_arrest_prob$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat12aa_description <- if_else(stat12aa_chi_recidivism_type_county_ICIS_arrest_prob$p.value < .05,
                               "There was a statistically significant difference in recidivism rates between counties and types of recidivism for probationers, ",
                               "The difference in recidivism rates between counties and types of recidivism for probationers was not statistically significant, ")
stat12aa <- paste0(stat12aa_description,
                  "$\\chi^2$(", stat12aa_chi_recidivism_type_county_ICIS_arrest_prob$parameter, ", <em>N</em> = ", stat12aa_n,
                  ") = ", round_half_up(stat12aa_chi_recidivism_type_county_ICIS_arrest_prob$statistic, digits = 2), 
                  stat12aa_p_value)

#this matches the crosstab for County * Rearrest in Tim's Recidivism Study Tables 2019 output
stat12a_county_rearrest_prob <- master_df_6 %>%
  filter(!is.na(Rearrest), agency == "Probationers") # %>%
# mutate(ICIS_arrest = if_else(ICIS_arrest == 1, 1, 0))
stat12a_chi_recidivism_type_county_rearrest_prob <- chisq.test(stat12a_county_rearrest_prob$county, stat12a_county_rearrest_prob$Rearrest)
# phi_recidivism_type_county_rearrest_prob <- sqrt(stat12a_chi_recidivism_type_county_rearrest_prob$statistic/table12_probation_sample_size)
stat12a_n <- table12_probation_sample_size
stat12a_p_value <- ifelse(signif_half_up(stat12a_chi_recidivism_type_county_rearrest_prob$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat12a_chi_recidivism_type_county_rearrest_prob$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat12a_description <- if_else(stat12a_chi_recidivism_type_county_rearrest_prob$p.value < .05,
                               "There was a statistically significant difference in overall recidivism rates between counties for probationers, ",
                               "The difference in overall recidivism rates between counties for probationers was not statistically significant, ")
stat12a <- paste0(stat12a_description,
                  "$\\chi^2$(", stat12a_chi_recidivism_type_county_rearrest_prob$parameter, ", <em>N</em> = ", stat12a_n,
                  ") = ", round_half_up(stat12a_chi_recidivism_type_county_rearrest_prob$statistic, digits = 2), 
                  stat12a_p_value)

#this *nearly* matches the crosstab for County * crim_arrest in Tim's Recidivism Study Tables 2019 output
stat12b_county_crim_arrest_prob <- master_df_6 %>%
  filter(!is.na(Rearrest), agency == "Probationers") %>%
  mutate(ICIS_arrest = if_else(ICIS_arrest == "Criminal Arrest", 1, 0))
stat12b_chi_recidivism_type_county_crim_arrest_prob <- chisq.test(stat12b_county_crim_arrest_prob$county, stat12b_county_crim_arrest_prob$ICIS_arrest)
# stat12b_phi_recidivism_type_county_crim_arrest_prob <- sqrt(stat12b_chi_recidivism_type_county_crim_arrest_prob$statistic/table12_probation_sample_size)
stat12b_n <- table12_probation_sample_size
stat12b_p_value <- ifelse(signif_half_up(stat12b_chi_recidivism_type_county_crim_arrest_prob$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat12b_chi_recidivism_type_county_crim_arrest_prob$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat12b_description <- if_else(stat12b_chi_recidivism_type_county_crim_arrest_prob$p.value < .05,
                               "There was a statistically significant difference in criminal arrests between counties for probationers, ",
                               "The difference in criminal arrests between counties for probationers was not statistically significant, ")
stat12b <- paste0(stat12b_description,
                  "$\\chi^2$(", stat12b_chi_recidivism_type_county_crim_arrest_prob$parameter, ", <em>N</em> = ", stat12b_n,
                  ") = ", round_half_up(stat12b_chi_recidivism_type_county_crim_arrest_prob$statistic, digits = 2), 
                  stat12b_p_value)

stat12c_county_revoc_prob <- master_df_6 %>%
  filter(!is.na(Rearrest), agency == "Probationers") %>%
  mutate(ICIS_arrest = if_else(ICIS_arrest == "Probation Revocations/Parole Violations", 1, 0))
stat12c_chi_recidivism_type_county_revoc_prob <- chisq.test(stat12c_county_revoc_prob$county, stat12c_county_revoc_prob$ICIS_arrest)
# stat12c_phi_recidivism_type_county_crim_arrest_prob <- sqrt(stat12c_chi_recidivism_type_county_revoc_prob$statistic/table12_probation_sample_size)
stat12c_n <- table12_probation_sample_size
stat12c_p_value <- ifelse(signif_half_up(stat12c_chi_recidivism_type_county_revoc_prob$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat12c_chi_recidivism_type_county_revoc_prob$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat12c_description <- if_else(stat12c_chi_recidivism_type_county_revoc_prob$p.value < .05,
                               "There was a statistically significant difference in probation revocations between counties for probationers, ",
                               "The difference in probation revocations between counties for probationers was not statistically significant, ")
stat12c <- paste0(stat12c_description,
                  "$\\chi^2$(", stat12c_chi_recidivism_type_county_revoc_prob$parameter, ", <em>N</em> = ", stat12b_n,
                  ") = ", round_half_up(stat12c_chi_recidivism_type_county_revoc_prob$statistic, digits = 2), 
                  stat12c_p_value)

stat12d_county_crim_contempt_prob <- master_df_6 %>%
  filter(!is.na(Rearrest), agency == "Probationers") %>%
  mutate(ICIS_arrest = if_else(ICIS_arrest == "Criminal Contempt of Court", 1, 0))
stat12d_chi_recidivism_type_county_crim_contempt_prob <- chisq.test(stat12d_county_crim_contempt_prob$county, stat12d_county_crim_contempt_prob$ICIS_arrest)
# stat12d_phi_recidivism_type_county_crim_arrest_prob <- sqrt(stat12d_chi_recidivism_type_county_crim_contempt_prob$statistic/table12_probation_sample_size)
stat12d_n <- table12_probation_sample_size
stat12d_p_value <- ifelse(signif_half_up(stat12d_chi_recidivism_type_county_crim_contempt_prob$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat12d_chi_recidivism_type_county_crim_contempt_prob$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat12d_description <- if_else(stat12d_chi_recidivism_type_county_crim_contempt_prob$p.value < .05,
                               "There was a statistically significant difference in criminal contempt of court between counties for probationers, ",
                               "The difference in criminal contempt of court between counties for probationers was not statistically significant, ")
stat12d <- paste0(stat12d_description,
                  "$\\chi^2$(", stat12d_chi_recidivism_type_county_crim_contempt_prob$parameter, ", <em>N</em> = ", stat12d_n,
                  ") = ", round_half_up(stat12d_chi_recidivism_type_county_crim_contempt_prob$statistic, digits = 2), 
                  stat12d_p_value)

stat12e_county_other_vio_prob <- master_df_6 %>%
  filter(!is.na(Rearrest), agency == "Probationers") %>%
  mutate(ICIS_arrest = if_else(ICIS_arrest == "Other Violations", 1, 0))
stat12e_chi_recidivism_type_county_other_vio_prob <- chisq.test(stat12e_county_other_vio_prob$county, stat12e_county_other_vio_prob$ICIS_arrest)
# stat12e_phi_recidivism_type_county_crim_arrest_prob <- sqrt(stat12e_chi_recidivism_type_county_other_vio_prob$statistic/table12_probation_sample_size)
stat12e_n <- table12_probation_sample_size
stat12e_p_value <- ifelse(signif_half_up(stat12e_chi_recidivism_type_county_other_vio_prob$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat12e_chi_recidivism_type_county_other_vio_prob$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat12e_description <- if_else(stat12e_chi_recidivism_type_county_other_vio_prob$p.value < .05,
                               "There was a statistically significant difference in other violations between counties for probationers, ",
                               "The difference in other violations between counties for probationers was not statistically significant, ")
stat12e <- paste0(stat12e_description,
                  "$\\chi^2$(", stat12e_chi_recidivism_type_county_other_vio_prob$parameter, ", <em>N</em> = ", stat12e_n,
                  ") = ", round_half_up(stat12e_chi_recidivism_type_county_other_vio_prob$statistic, digits = 2), 
                  stat12e_p_value)

figure12 <- ggplot(table12_recidivism_type_county_prob, aes(x = county, y = prop_recidivate, fill = ICIS_arrest)) +
  geom_col() +
  # geom_text(data = recidivism_type_agency,
  #           mapping = aes(x = agency, y = prop_recidivate,
  #                         label =  percent(prop_recidivate, accuracy = 0.1)),
  #           nudge_y = .06,
  #           color = "black") +
  annotate(geom = "text", x = 1, y = table12_county_prob$prop_recidivate[table12_county_prob$county == "Kauai"] + .05, label = percent(table12_county_prob$prop_recidivate[table12_county_prob$county == "Kauai"], accuracy = 0.1, suffix = "%")) +
  annotate(geom = "text", x = 2, y = table12_county_prob$prop_recidivate[table12_county_prob$county == "Maui"] + .05, label = percent(table12_county_prob$prop_recidivate[table12_county_prob$county == "Maui"], accuracy = 0.1, suffix = "%")) +
  annotate(geom = "text", x = 3, y = table12_county_prob$prop_recidivate[table12_county_prob$county == "Honolulu"] + .05, label = percent(table12_county_prob$prop_recidivate[table12_county_prob$county == "Honolulu"], accuracy = 0.1, suffix = "%")) +
  annotate(geom = "text", x = 4, y = table12_county_prob$prop_recidivate[table12_county_prob$county == "Hawaii"] + .05, label = percent(table12_county_prob$prop_recidivate[table12_county_prob$county == "Hawaii"], accuracy = 0.1, suffix = "%")) +
  annotate(geom = "text", x = 5, y = table12_county_prob$prop_recidivate[table12_county_prob$county == "Other"] + .05, label = percent(table12_county_prob$prop_recidivate[table12_county_prob$county == "Other"], accuracy = 0.1, suffix = "%")) +
  annotate(geom = "text", x = 6, y = table12_county_prob$prop_recidivate[table12_county_prob$county == "Unknown"] + .05, label = percent(table12_county_prob$prop_recidivate[table12_county_prob$county == "Unknown"], accuracy = 0.1, suffix = "%")) +
  geom_hline(yintercept = table12_county_prob$prop_recidivate[table12_county_prob$county == "Total"], color = "red", linewidth = 1, linetype = 2) +
  annotate(geom = "text", label = paste0("FY", fy, " Total Recividism Rate (",
                                         percent(table12_county_prob$prop_recidivate[table12_county_prob$county == "Total"], accuracy = 0.1),")"), x = 3.5, y = table12_county_prob$prop_recidivate[table12_county_prob$county == "Total"] - 0.05, color = "black") +
  scale_x_discrete(name = NULL, labels = paste0(table12_county_prob$county, " (n = ", table12_county_prob$sample_size, ")"),
                   guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(name = "Recidivism Rate", labels = percent_format(accuracy = 0.1), breaks = seq(0, 1, by = .1), limits = c(0,1)) +
  scale_fill_brewer(name = "Recidivism Type", type = "seq", direction = -1) +
  ggtitle(paste0("Recidivism Rates for Probationers by County and Recidivism Type, FY ", fy, " Cohort")) +
  theme_bw() +
  NULL

#---------------------------------------------------------------------------------#
#ANALYSIS 13 RR by County and Recidivism Type for Parolees----
#NOTES:
#1. Tim's calculations for the R's are incorrect
#2. Types of Recidivism  Rates (Parolees) Box appears to be incorrect -> divided by 531 but should be 517.
#---------------------------------------------------------------------------------#
table13_recidivism_type_county_par <- master_df_6 %>%
  filter(!is.na(Rearrest), agency == "Parolees") %>% #must remove NAs so that totals match Tim's -> SPSS rmoves the missing values (see "Missing Value Handling -> Definition of Missing in spv file)
  count(county, ICIS_arrest, name = "r") %>%
  full_join(ICIS_arrest_x_county_factors) %>% #add any missing levels of ICIS_arrest or county
  replace_na(list(r = 0)) %>% 
  group_by(county) %>%
  mutate(sample_size = sum(r)) %>% 
  ungroup() %>%
  # filter(ICIS_arrest != "No Rearrest") %>% #FY16
  filter(!is.na(ICIS_arrest)) %>% #FY17
  mutate(prop_recidivate = round_half_up(r/sample_size, digits = 3),
         prop_recidivate_pretty = percent(r/sample_size, accuracy = 0.1)) 

table13_county_par <- table13_recidivism_type_county_par %>%
  group_by(county) %>%
  summarize(r = sum(r, na.rm = TRUE), sample_size = first(sample_size), prop_recidivate = sum(prop_recidivate, na.rm = TRUE), prop_recidivate_pretty = percent(prop_recidivate, accuracy = 0.1)) %>%
  adorn_totals() %>%
  #this is the correct formula, but because of 14 individuals in this analysis didn't a County designation, there is a shift in the statewide % so use the statewide # from Fig 1
  mutate(prop_recidivate = if_else(county == "Total", r/sample_size, prop_recidivate))

table13_sample_size_par <- table13_county_par$sample_size[table13_county_par$county=="Total"]

table13a <- table13_recidivism_type_county_par %>%
  group_by(ICIS_arrest) %>%
  summarize(r_icis_arrest = sum(r, na.rm = TRUE), n_icis_arrest = table13_sample_size_par, 
            recidivism_rate = round_half_up(r_icis_arrest/table13_sample_size_par, digits = 3), 
            recidivism_rate_pretty = percent(recidivism_rate, accuracy = 0.1)) #%>% 
  #neither add_row or rbind works, but they should...h/t: https://stackoverflow.com/questions/33565949/add-row-to-data-frame-with-dplyr
  # add_row(ICIS_arrest = "Total", r_icis_arrest = sum(r_icis_arrest), n_icis_arrest = table13_sample_size_par, 
  #         recidivism_rate = sum(recidivism_rate), recidivism_rate_pretty = ""))
  # rbind(c(ICIS_arrest = "Total", r_icis_arrest = sum(r_icis_arrest), n_icis_arrest = table13_sample_size_par, 
  #         recidivism_rate = sum(recidivism_rate), recidivism_rate_pretty = ""))
  # adorn_totals("row",,,, -n_icis_arrest)

table13 <- table13_recidivism_type_county_par %>% 
  pivot_wider(id_cols = ICIS_arrest, names_from = county, values_from = c(r, sample_size, prop_recidivate_pretty)) %>% 
  mutate(Kauai = paste0(prop_recidivate_pretty_Kauai, " (r = ", r_Kauai, ", n = ", sample_size_Kauai, ")"),
         Maui = paste0(prop_recidivate_pretty_Maui, " (r = ", r_Maui, ", n = ", sample_size_Maui, ")"),
         Honolulu = paste0(prop_recidivate_pretty_Honolulu, " (r = ", r_Honolulu, ", n = ", sample_size_Honolulu, ")"),
         Hawaii = paste0(prop_recidivate_pretty_Hawaii, " (r = ", r_Hawaii, ", n = ", sample_size_Hawaii, ")"),
         Other = paste0(prop_recidivate_pretty_Other, " (r = ", r_Other, ", n = ", sample_size_Other, ")"),
         Unknown = paste0(prop_recidivate_pretty_Unknown, " (r = ", r_Unknown, ", n = ", sample_size_Unknown, ")")) %>% 
  inner_join(table13a) %>% 
  mutate(ICIS_arrest = paste0(ICIS_arrest, " (r = ", r_icis_arrest, ", n = ", n_icis_arrest, ")")) %>% 
  select(ICIS_arrest, Kauai, Maui, Honolulu, Hawaii, Other, Unknown)

#this matches the crosstab for County * ICIS_arrest in Tim's Recidivism Study Tables 2019 output
stat13aa_county_ICIS_arrest_par <- master_df_6 %>%
  filter(!is.na(Rearrest), agency == "Parolees") # %>%
# mutate(ICIS_arrest = if_else(ICIS_arrest == 1, 1, 0))
stat13aa_chi_recidivism_type_county_ICIS_arrest_par <- chisq.test(stat13aa_county_ICIS_arrest_par$county, stat13aa_county_ICIS_arrest_par$ICIS_arrest)
# stat13a_phi_recidivism_type_county_ICIS_arrest_par <- sqrt(stat13aa_chi_recidivism_type_county_ICIS_arrest_par$statistic/table13_sample_size_par)
# stat13a <- paste0("phi = ", round_half_up(phi_recidivism_type_county_ICIS_arrest_par, digits = 3), ", <em>p</em> < ", signif_half_up(chi_recidivism_type_county_ICIS_arrest_par$p.value, digits = 3))
stat13aa_n <- table13_sample_size_par
stat13aa_p_value <- ifelse(signif_half_up(stat13aa_chi_recidivism_type_county_ICIS_arrest_par$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat13aa_chi_recidivism_type_county_ICIS_arrest_par$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat13aa_description <- if_else(stat13aa_chi_recidivism_type_county_ICIS_arrest_par$p.value < .05,
                               "There was a statistically significant difference in recidivism rates between counties and types of recidivism for parolees, ",
                               "The difference in recidivism rates between counties and types of recidivism for parolees was not statistically significant, ")
stat13aa <- paste0(stat13aa_description,
                  "$\\chi^2$(", stat13aa_chi_recidivism_type_county_ICIS_arrest_par$parameter, ", <em>N</em> = ", stat13aa_n,
                  ") = ", round_half_up(stat13aa_chi_recidivism_type_county_ICIS_arrest_par$statistic, digits = 2), 
                  stat13aa_p_value)

#this matches the crosstab for County * Rearrest in Tim's Recidivism Study Tables 2019 output
state13a_county_rearrest_par <- master_df_6 %>%
  filter(!is.na(Rearrest), agency == "Parolees") # %>%
# mutate(ICIS_arrest = if_else(ICIS_arrest == 1, 1, 0))
stat13a_chi_recidivism_type_county_rearrest_par <- chisq.test(state13a_county_rearrest_par$county, state13a_county_rearrest_par$Rearrest)
# stat13f_phi_recidivism_type_county_rearrest_par <- sqrt(stat13a_chi_recidivism_type_county_rearrest_par$statistic/table13_sample_size_par)
# stat13f <- paste0("phi = ", round_half_up(phi_recidivism_type_county_rearrest_par, digits = 3), ", <em>p</em> < ", signif_half_up(chi_recidivism_type_county_rearrest_par$p.value, digits = 3))
stat13a_n <- table13_sample_size_par
stat13a_p_value <- ifelse(signif_half_up(stat13a_chi_recidivism_type_county_rearrest_par$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat13a_chi_recidivism_type_county_rearrest_par$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat13a_description <- if_else(stat13a_chi_recidivism_type_county_rearrest_par$p.value < .05,
                               "There was a statistically significant difference in overall recidivism rates between counties for parolees, ",
                               "The difference in overall recidivism rates between counties for parolees was not statistically significant, ")
stat13a <- paste0(stat13a_description,
                  "$\\chi^2$(", stat13a_chi_recidivism_type_county_rearrest_par$parameter, ", <em>N</em> = ", stat13a_n,
                  ") = ", round_half_up(stat13a_chi_recidivism_type_county_rearrest_par$statistic, digits = 2), 
                  stat13a_p_value)

#this *nearly* matches the crosstab for County * crim_arrest in Tim's Recidivism Study Tables 2019 output
stat13b_county_crim_arrest_par <- master_df_6 %>%
  filter(!is.na(Rearrest), agency == "Parolees") %>%
  mutate(ICIS_arrest = if_else(ICIS_arrest == "Criminal Arrest", 1, 0))
stat13b_chi_recidivism_type_county_crim_arrest_par <- chisq.test(stat13b_county_crim_arrest_par$county, stat13b_county_crim_arrest_par$ICIS_arrest)
# stat13b_phi_recidivism_type_county_crim_arrest_par <- sqrt(stat13b_chi_recidivism_type_county_crim_arrest_par$statistic/table13_sample_size_par)
# stat13b <- paste0("phi = ", round_half_up(phi_recidivism_type_county_crim_arrest_par, digits = 3), ", <em>p</em> < ", signif_half_up(chi_recidivism_type_county_crim_arrest_par$p.value, digits = 3))
stat13b_n <- table13_sample_size_par
stat13b_p_value <- ifelse(signif_half_up(stat13b_chi_recidivism_type_county_crim_arrest_par$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat13b_chi_recidivism_type_county_crim_arrest_par$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat13b_description <- if_else(stat13b_chi_recidivism_type_county_crim_arrest_par$p.value < .05,
                               "There was a statistically significant difference in criminal arrests between counties for parolees, ",
                               "The difference in criminal arrests between counties for parolees was not statistically significant, ")
stat13b <- paste0(stat13b_description,
                  "$\\chi^2$(", stat13b_chi_recidivism_type_county_crim_arrest_par$parameter, ", <em>N</em> = ", stat13b_n,
                  ") = ", round_half_up(stat13b_chi_recidivism_type_county_crim_arrest_par$statistic, digits = 2), 
                  stat13b_p_value)

#this *nearly* matches the crosstab for County * revoc in Tim's Recidivism Study Tables 2019 output
stat13c_county_revoc_par <- master_df_6 %>%
  filter(!is.na(Rearrest), agency == "Parolees") %>%
  mutate(ICIS_arrest = if_else(ICIS_arrest == "Probation Revocations/Parole Violations", 1, 0))
stat13c_chi_recidivism_type_county_revoc_par <- chisq.test(stat13c_county_revoc_par$county, stat13c_county_revoc_par$ICIS_arrest)
# stat13c_phi_recidivism_type_county_revoc_par <- sqrt(stat13c_chi_recidivism_type_county_revoc_par$statistic/table13_sample_size_par)
# stat13c <- paste0("phi = ", round_half_up(phi_recidivism_type_county_revoc_par, digits = 3), ", <em>p</em> < ", signif_half_up(stat13c_chi_recidivism_type_county_revoc_par$p.value, digits = 3))
stat13c_n <- table13_sample_size_par
stat13c_p_value <- ifelse(signif_half_up(stat13c_chi_recidivism_type_county_revoc_par$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat13c_chi_recidivism_type_county_revoc_par$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat13c_description <- if_else(stat13c_chi_recidivism_type_county_revoc_par$p.value < .05,
                               "There was a statistically significant difference in parole violations between counties for parolees, ",
                               "The difference in parole violations between counties for parolees was not statistically significant, ")
stat13c <- paste0(stat13c_description,
                  "$\\chi^2$(", stat13c_chi_recidivism_type_county_revoc_par$parameter, ", <em>N</em> = ", stat13c_n,
                  ") = ", round_half_up(stat13c_chi_recidivism_type_county_revoc_par$statistic, digits = 2), 
                  stat13c_p_value)

stat13d_county_crim_contempt_par <- master_df_6 %>%
  filter(!is.na(Rearrest), agency == "Parolees") %>%
  mutate(ICIS_arrest = if_else(ICIS_arrest == "Criminal Contempt of Court", 1, 0))
stat13d_chi_recidivism_type_county_crim_contempt_par <- chisq.test(stat13d_county_crim_contempt_par$county, stat13d_county_crim_contempt_par$ICIS_arrest)
# stat13c_phi_recidivism_type_county_revoc_par <- sqrt(stat13d_chi_recidivism_type_county_crim_contempt_par$statistic/table13_sample_size_par)
# stat13c <- paste0("phi = ", round_half_up(phi_recidivism_type_county_revoc_par, digits = 3), ", <em>p</em> < ", signif_half_up(stat13d_chi_recidivism_type_county_crim_contempt_par$p.value, digits = 3))
stat13d_n <- table13_sample_size_par
stat13d_p_value <- ifelse(signif_half_up(stat13d_chi_recidivism_type_county_crim_contempt_par$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat13d_chi_recidivism_type_county_crim_contempt_par$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat13d_description <- if_else(stat13d_chi_recidivism_type_county_crim_contempt_par$p.value < .05,
                               "There was a statistically significant difference in criminal contempt of court between counties for parolees, ",
                               "The difference in criminal contempt of court between counties for parolees was not statistically significant, ")
stat13d <- paste0(stat13d_description,
                  "$\\chi^2$(", stat13d_chi_recidivism_type_county_crim_contempt_par$parameter, ", <em>N</em> = ", stat13d_n,
                  ") = ", round_half_up(stat13d_chi_recidivism_type_county_crim_contempt_par$statistic, digits = 2), 
                  stat13d_p_value)

# FY17 no "Other Violations" for parolees
# stat13e_county_other_vio_par <- master_df_6 %>%
#   filter(!is.na(Rearrest), agency == "Parolees") %>%
#   mutate(ICIS_arrest = if_else(ICIS_arrest == "Other Violations", 1, 0))
# stat13e_chi_recidivism_type_county_other_vio_par <- chisq.test(stat13e_county_other_vio_par$county, stat13e_county_other_vio_par$ICIS_arrest)
# # stat13c_phi_recidivism_type_county_revoc_par <- sqrt(stat13e_chi_recidivism_type_county_other_vio_par$statistic/table13_sample_size_par)
# # stat13c <- paste0("phi = ", round_half_up(phi_recidivism_type_county_revoc_par, digits = 3), ", <em>p</em> < ", signif_half_up(stat13e_chi_recidivism_type_county_other_vio_par$p.value, digits = 3))
# stat13e_n <- table13_sample_size_par
# stat13e_p_value <- ifelse(signif_half_up(stat13e_chi_recidivism_type_county_other_vio_par$p.value, digits = 3) >= .001, 
#                           paste0(", <em>p</em> = ", signif_half_up(stat13e_chi_recidivism_type_county_other_vio_par$p.value, digits = 3)), 
#                           paste0(", <em>p</em> < .001"))
# stat13e_description <- if_else(stat13e_chi_recidivism_type_county_other_vio_par$p.value < .05,
#                                "There was a statistically significant difference in other violations between counties for parolees, ",
#                                "The difference in other violations between counties for parolees was not statistically significant, ")
# stat13e <- paste0("Other Violations: ",
#                   "$\\chi^2$(", stat13e_chi_recidivism_type_county_other_vio_par$parameter, ", <em>N</em> = ", stat13e_n,
#                   ") = ", round_half_up(stat13e_chi_recidivism_type_county_other_vio_par$statistic, digits = 2), 
#                   stat13e_p_value)

figure13 <- ggplot(table13_recidivism_type_county_par, aes(x = county, y = prop_recidivate, fill = ICIS_arrest)) +
  geom_col() +
  # geom_text(data = recidivism_type_agency,
  #           mapping = aes(x = agency, y = prop_recidivate,
  #                         label =  percent(prop_recidivate, accuracy = 0.1)),
  #           nudge_y = .06,
  #           color = "black") +
  annotate(geom = "text", x = 1, y = table13_county_par$prop_recidivate[1] + .05, label = percent(table13_county_par$prop_recidivate[1], accuracy = 0.1, suffix = "%")) +
  annotate(geom = "text", x = 2, y = table13_county_par$prop_recidivate[2] + .05, label = percent(table13_county_par$prop_recidivate[2], accuracy = 0.1, suffix = "%")) +
  annotate(geom = "text", x = 3, y = table13_county_par$prop_recidivate[3] + .05, label = percent(table13_county_par$prop_recidivate[3], accuracy = 0.1, suffix = "%")) +
  annotate(geom = "text", x = 4, y = table13_county_par$prop_recidivate[4] + .05, label = percent(table13_county_par$prop_recidivate[4], accuracy = 0.1, suffix = "%")) +
  annotate(geom = "text", x = 5, y = table13_county_par$prop_recidivate[5] + .05, label = percent(table13_county_par$prop_recidivate[5], accuracy = 0.1, suffix = "%")) +
  annotate(geom = "text", x = 6, y = table13_county_par$prop_recidivate[6] + .05, label = percent(table13_county_par$prop_recidivate[6], accuracy = 0.1, suffix = "%")) +
  geom_hline(yintercept = table13_county_par$prop_recidivate[7], color = "red", size = 1, linetype = 2) +
  annotate(geom = "text", label = paste0("FY", fy, " Total Recividism Rate (",
                                         percent(table13_county_par$prop_recidivate[7], accuracy = 0.1),")"), x = 3.5, y = table13_county_par$prop_recidivate[7] - 0.05, color = "black") +
  scale_x_discrete(name = NULL, labels = paste0(table13_county_par$county, " (n = ", table13_county_par$sample_size, ")"),
                   guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(name = "Recidivism Rate", labels = percent_format(accuracy = 0.1), breaks = seq(0, 1, by = .1), limits = c(0,1)) +
  scale_fill_brewer(name = "Recidivism Type", type = "seq", direction = -1) +
  ggtitle(paste0("Recidivism Rates for Parolees by County and Recidivism Type, FY ", fy, " Cohort")) +
  theme_bw() +
  NULL

#---------------------------------------------------------------------------------#
#ANALYSIS 14 Time by Offender Type and Recidivism Type----
#NOTES:
#1. # of reoffenders within offender type (R value in labels along x-axis) is not the same as the sum of the individual Rs for each recidivism type in table b/c other violations excluded
#2. the agency means and R counts (counts of reoffenders) include "Other Violation" but table below doesn't
#---------------------------------------------------------------------------------#
table14_avg_time_recidivism_type_agency <- master_df_6 %>%
  group_by(agency, ICIS_arrest) %>%
  summarize(r = n(), avg_days_recidivate = mean(days_rearrest, na.rm = TRUE), 
            avg_months_recidivate = round(as.numeric(mean(days_rearrest, na.rm = TRUE))/30, digits = 1),
            # sd_months_recidivate = round(sd(days_rearrest, na.rm = TRUE)/30, digits = 1)
            ) %>%
  ungroup() %>%
  full_join(agency_factors_x_ICIS_arrest_factors) %>% #add any missing levels of ICIS_arrest or county
  replace_na(list(r = 0)) %>% 
  group_by(agency) %>%
  mutate(sample_size = sum(r)) %>% 
  ungroup() %>%
  # filter(ICIS_arrest != "No Rearrest")#FY16
  filter(!is.na(ICIS_arrest)) #FY17

table14 <- table14_avg_time_recidivism_type_agency %>% 
  pivot_wider(id_cols = ICIS_arrest, names_from = agency, values_from = c(avg_months_recidivate, r)) %>% 
  mutate(Probationers = paste0(avg_months_recidivate_Probationers, " (r = ", r_Probationers, ")"),
         Parolees = paste0(avg_months_recidivate_Parolees, " (r = ", r_Parolees, ")"),
         `MTR-Prisoners` = paste0(`avg_months_recidivate_MTR-Prisoners`, " (r = ", `r_MTR-Prisoners`, ")")) %>% 
  select(ICIS_arrest, Probationers, Parolees, `MTR-Prisoners`)

table14_avg_time_agency <- table14_avg_time_recidivism_type_agency %>%
  # filter(ICIS_arrest != "Other Violations") %>%
  group_by(agency) %>%
  summarize(r_agency = sum(r, na.rm = TRUE),
            avg_days_recidivate = sum((r/r_agency)*avg_days_recidivate, na.rm = TRUE),#take the weighted average
            avg_months_recidivate = round(as.numeric(sum((r/r_agency)*avg_days_recidivate)/30), digits = 1)) %>% #take the weighted average 
  ungroup()

table14a <- table14_avg_time_recidivism_type_agency %>%
  group_by(ICIS_arrest) %>%
  summarize(r_recid_type = sum(r, na.rm = TRUE),
            avg_days_recidivate = sum((r/r_recid_type)*avg_days_recidivate, na.rm = TRUE),
            avg_months_recidivate = round(as.numeric(sum((r/r_recid_type)*avg_days_recidivate)/30), digits = 1)) %>% 
  select(ICIS_arrest, r_recid_type, avg_months_recidivate)

figure14 <- ggplot() +
  geom_col(filter(table14_avg_time_recidivism_type_agency),
           mapping = aes(x = agency, y = avg_months_recidivate, fill = ICIS_arrest),
           position = position_dodge2()) +
  geom_errorbar(data = table14_avg_time_agency,
                mapping = aes(x = agency, ymin = avg_months_recidivate, ymax = avg_months_recidivate),
                linetype = 2, color = "red", size = 1) +
  annotate(geom = "text", x = 1, y = table14_avg_time_agency$avg_months_recidivate[1] + 1, label = paste0("Avg time = ", table14_avg_time_agency$avg_months_recidivate[1], " months")) +
  annotate(geom = "text", x = 2, y = table14_avg_time_agency$avg_months_recidivate[2] + 1, label = paste0("Avg time = ", table14_avg_time_agency$avg_months_recidivate[2], " months")) +
  annotate(geom = "text", x = 3, y = table14_avg_time_agency$avg_months_recidivate[3] + 1, label = paste0("Avg time = ", table14_avg_time_agency$avg_months_recidivate[3], " months")) +
  scale_x_discrete(name = NULL, labels = paste0(table14_avg_time_agency$agency, " (R = ", table14_avg_time_agency$r_agency, ")")) +
  scale_y_continuous(name = "Time to Recidivism (months)", breaks = seq(0, 36, by = 4), limits = c(0,36)) +
  scale_fill_brewer(name = "Recidivism Type", type = "seq", direction = -1) +
  ggtitle(paste0("Average Time to Recidivism by Recidivism Type and Offender Type, FY ", fy, " Cohort")) +
  theme_bw() +
  NULL

#ANOVAs
#FY16: note that aov function needs SPSS variables to be converted with as_factor, unable to process recodes
stat14_avg_time_agency_rearrest <- master_df_6 %>%
  filter(ICIS_arrest == "Criminal Arrest") %>%
  mutate(days_rearrest = as.numeric(days_rearrest),
         agency = as_factor(agency))

#One-Way ANOVA for rearrests
oneway.test(formula = days_rearrest ~ agency,
            data = stat14_avg_time_agency_rearrest,
            var.equal = TRUE)

stat14_res_aov_arrest <- aov(formula = days_rearrest ~ agency,
               data = stat14_avg_time_agency_rearrest,
               projections = TRUE,
               qr = TRUE,
               contrasts = TRUE)

summary(stat14_res_aov_arrest)
report(stat14_res_aov_arrest)
report_parameters(stat14_res_aov_arrest)
stat14a <- paste0("Criminal Arrest: ", report_parameters(stat14_res_aov_arrest))
report_table(stat14_res_aov_arrest)

stat14a_post_test <- multcomp::glht(stat14_res_aov_arrest,
                            linfct = multcomp::mcp(agency = 'Tukey'))

#FY16: note that aov function needs SPSS variables to be converted with as_factor, unable to process recodes
#FY17: not enough observations of revocs for MTR-Prisoners (since they aren't under supervision)
stat14_avg_time_agency_revoc <- master_df_6 %>%
  filter(ICIS_arrest == "Probation Revocations/Parole Violations") %>%
  mutate(days_rearrest = as.numeric(days_rearrest),
         agency = as_factor(agency))
# 
# #One-Way ANOVA for rearrests
# oneway.test(formula = days_rearrest ~ agency,
#             data = stat14_avg_time_agency_revoc,
#             var.equal = TRUE)

stat14_res_aov_revoc <- aov(formula = days_rearrest ~ agency,
               data = stat14_avg_time_agency_revoc,
               projections = TRUE,
               qr = TRUE,
               contrasts = TRUE)

summary(stat14_res_aov_revoc)
report(stat14_res_aov_revoc)
report_parameters(stat14_res_aov_revoc)
stat14b <- paste0("Probation Revocations/Parole Violations: ", report_parameters(stat14_res_aov_revoc))
report_table(stat14_res_aov_revoc)

stat14b_post_test <- multcomp::glht(stat14_res_aov_revoc,
                            linfct = multcomp::mcp(agency = 'Tukey'))

#FY16: note that aov function needs SPSS variables to be converted with as_factor, unable to process recodes
stat14_avg_time_agency_crim_contempt <- master_df_6 %>%
  filter(ICIS_arrest == "Criminal Contempt of Court") %>%
  mutate(days_rearrest = as.numeric(days_rearrest),
         agency = as_factor(agency))

#One-Way ANOVA for rearrests
oneway.test(formula = days_rearrest ~ agency,
            data = stat14_avg_time_agency_crim_contempt,
            var.equal = TRUE)

stat14_res_aov_crim_contempt <- aov(formula = days_rearrest ~ agency,
                     data = stat14_avg_time_agency_crim_contempt,
                     projections = TRUE,
                     qr = TRUE,
                     contrasts = TRUE)

summary(stat14_res_aov_crim_contempt)
report(stat14_res_aov_crim_contempt)
report_parameters(stat14_res_aov_crim_contempt)
stat14c <- paste0("Criminal Contempt of Court: ", report_parameters(stat14_res_aov_crim_contempt))
report_table(stat14_res_aov_crim_contempt)

stat14c_post_test <- multcomp::glht(stat14_res_aov_crim_contempt,
                            linfct = multcomp::mcp(agency = 'Tukey'))

#FY16: note that aov function needs SPSS variables to be converted with as_factor, unable to process recodes
#FY17: not enough observations of other vios for parolees and MTR-Prisoners 
# stat14_avg_time_agency_other_vio <- master_df_6 %>%
#   filter(ICIS_arrest == "Other Violations") %>%
#   mutate(days_rearrest = as.numeric(days_rearrest),
#          agency = as_factor(agency))
# 
# #One-Way ANOVA for rearrests
# oneway.test(formula = days_rearrest ~ agency,
#             data = stat14_avg_time_agency_other_vio,
#             var.equal = TRUE)
# 
# stat14_res_aov_other_vio <- aov(formula = days_rearrest ~ agency,
#                      data = stat14_avg_time_agency_other_vio,
#                      projections = TRUE,
#                      qr = TRUE,
#                      contrasts = TRUE)
# 
# summary(stat14_res_aov_other_vio)
# report(stat14_res_aov_other_vio)
# report_parameters(stat14_res_aov_other_vio)
# stat14d <- paste0("Other Violations: ", report_parameters(stat14_res_aov_other_vio))
# report_table(stat14_res_aov_other_vio)
# 
# stat14d_post_test <- multcomp::glht(stat14_res_aov_other_vio,
#                             linfct = multcomp::mcp(agency = 'Tukey'))
#---------------------------------------------------------------------------------#
#ANALYSIS 15 Time by County and Recidivism Type----
#NOTES:
#1. inconsistency with Fig 14 -> avg_time_county filters out Other Violations but avg_time_agency in Fig 14 includes them
#2. Values in "Average Elapsed Times to Types of Recidivism" box are incorrect -> can confirm simply by looking at table showing avg times disaggregated by county
#     the approx avg of the rearrests is clearly way above 7.2
#     can also confirm by looking at same box in Fig 14 (compare figure14a and figure 15a) - the numbers should be identical but are not
#---------------------------------------------------------------------------------#
table15_avg_time_recidivism_type_county <- master_df_6 %>%
  group_by(county, ICIS_arrest) %>%
  summarize(r = n(), avg_days_recidivate = mean(days_rearrest, na.rm = TRUE), avg_months_recidivate = round(as.numeric(mean(days_rearrest, na.rm = TRUE))/30, digits = 1)) %>%
  ungroup() %>%
  full_join(ICIS_arrest_x_county_factors) %>% #add any missing levels of ICIS_arrest or county
  replace_na(list(r = 0)) %>% 
  group_by(county) %>%
  mutate(sample_size = sum(r)) %>% 
  ungroup() %>%
  # filter(ICIS_arrest != "No Rearrest") %>% #FY16
  filter(!is.na(ICIS_arrest)) #FY17

table15 <- table15_avg_time_recidivism_type_county %>% 
  pivot_wider(id_cols = ICIS_arrest, names_from = county, values_from = c(avg_months_recidivate, r)) %>% 
  mutate(Kauai = paste0(avg_months_recidivate_Kauai, " (r = ", r_Kauai, ")"),
         Maui = paste0(avg_months_recidivate_Maui, " (r = ", r_Maui, ")"),
         Honolulu = paste0(avg_months_recidivate_Honolulu, " (r = ", r_Honolulu, ")"),
         Hawaii = paste0(avg_months_recidivate_Hawaii, " (r = ", r_Hawaii, ")"),
         Other = paste0(avg_months_recidivate_Other, " (r = ", r_Other, ")"),
         Unknown = paste0(avg_months_recidivate_Unknown, " (r = ", r_Unknown, ")")) %>% 
  select(ICIS_arrest, Kauai, Maui, Honolulu, Hawaii, Other, Unknown)

table15_avg_time_county <- table15_avg_time_recidivism_type_county %>%
  filter(ICIS_arrest != "Other Violations") %>%
  group_by(county) %>%
  summarize(r_county = sum(r, na.rm = TRUE),
            avg_days_recidivate = sum((r/r_county)*avg_days_recidivate, na.rm = TRUE),#take the weighted average
            avg_months_recidivate = round(as.numeric(sum((r/r_county)*avg_days_recidivate)/30), digits = 1))#take the weighted average

table15a <- table15_avg_time_recidivism_type_county %>%
  group_by(ICIS_arrest) %>%
  summarize(r_recidivism_type = sum(r, na.rm = TRUE),
            avg_days_recidivate = sum((r/r_recidivism_type)*avg_days_recidivate, na.rm = TRUE),#take the weighted average
            avg_months_recidivate = round(as.numeric(sum((r/r_recidivism_type)*avg_days_recidivate)/30), digits = 1)) %>% #take the weighted average
  select(ICIS_arrest, r_recidivism_type, avg_months_recidivate)

figure15 <- ggplot() +
  geom_col(table15_avg_time_recidivism_type_county,
           mapping = aes(x = county, y = avg_months_recidivate, fill = ICIS_arrest),
           position = position_dodge2()) +
  geom_errorbar(data = table15_avg_time_county,
                mapping = aes(x = county, ymin = avg_months_recidivate, ymax = avg_months_recidivate),
                linetype = 2, color = "red", linewidth = 1) +
  annotate(geom = "text", x = 1, y = table15_avg_time_county$avg_months_recidivate[1] + 3, label = paste0("Avg time = \n", table15_avg_time_county$avg_months_recidivate[1], " months")) +
  annotate(geom = "text", x = 2, y = table15_avg_time_county$avg_months_recidivate[2] + 3, label = paste0("Avg time = \n", table15_avg_time_county$avg_months_recidivate[2], " months")) +
  annotate(geom = "text", x = 3, y = table15_avg_time_county$avg_months_recidivate[3] + 3, label = paste0("Avg time = \n", table15_avg_time_county$avg_months_recidivate[3], " months")) +
  annotate(geom = "text", x = 4, y = table15_avg_time_county$avg_months_recidivate[4] + 3, label = paste0("Avg time = \n", table15_avg_time_county$avg_months_recidivate[4], " months")) +
  annotate(geom = "text", x = 5, y = table15_avg_time_county$avg_months_recidivate[5] + 3, label = paste0("Avg time = \n", table15_avg_time_county$avg_months_recidivate[5], " months")) +
  annotate(geom = "text", x = 6, y = table15_avg_time_county$avg_months_recidivate[6] + 3, label = paste0("Avg time = \n", table15_avg_time_county$avg_months_recidivate[6], " months")) +
  scale_x_discrete(name = NULL, labels = paste0(table15_avg_time_county$county, " (R = ", table15_avg_time_county$r_county, ")"),
                   guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(name = "Time to Recidivism (months)", breaks = seq(0, 36, by = 4), limits = c(0,36)) +
  scale_fill_brewer(name = "Recidivism Type", type = "seq", direction = -1) +
  ggtitle(paste0("Average Time to Recidivism by County and Recidivism Type, FY ", fy, " Cohort")) +
  theme_bw() +
  NULL

#ANOVAs
#note that aov function needs SPSS variables to be converted with as_factor, unable to process recodes
#rearrest
stat15_avg_time_county_rearrest <- master_df_6 %>%
  filter(ICIS_arrest == "Criminal Arrest") %>%
  mutate(days_rearrest = as.numeric(days_rearrest))

#One-Way ANOVA for rearrests
oneway.test(formula = days_rearrest ~ county,
            data = stat15_avg_time_county_rearrest,
            var.equal = TRUE)

stat15_res_aov_rearrest <- aov(formula = days_rearrest ~ county,
               data = stat15_avg_time_county_rearrest,
               projections = TRUE,
               qr = TRUE,
               contrasts = TRUE)

summary(stat15_res_aov_rearrest)
report(stat15_res_aov_rearrest)
stat15a <- paste0("Criminal Arrest: ", report_parameters(stat15_res_aov_rearrest))
report_table(stat15_res_aov_rearrest)

stat15a_post_test <- multcomp::glht(stat15_res_aov_rearrest,
                            linfct = multcomp::mcp(county = 'Tukey'))

#revoc
stat15_avg_time_county_revoc <- master_df_6 %>%
  filter(ICIS_arrest == "Probation Revocations/Parole Violations") %>%
  mutate(days_rearrest = as.numeric(days_rearrest))

#One-Way ANOVA for rearrests
# oneway.test(formula = days_rearrest ~ county,
#             data = stat15_avg_time_county_revoc,
#             var.equal = TRUE)

stat15_res_aov_revoc <- aov(formula = days_rearrest ~ county,
                     data = stat15_avg_time_county_revoc,
                     projections = TRUE,
                     qr = TRUE,
                     contrasts = TRUE)

summary(stat15_res_aov_revoc)
report(stat15_res_aov_revoc)
stat15b <- paste0("Probation Revocations/Parole Violations: ", report_parameters(stat15_res_aov_revoc))
report_table(stat15_res_aov_revoc)

stat15b_post_test <- multcomp::glht(stat15_res_aov_revoc,
                            linfct = multcomp::mcp(county = 'Tukey'))

#crim contempt of court
stat15_avg_time_county_ccc <- master_df_6 %>%
  filter(ICIS_arrest == "Criminal Contempt of Court") %>%
  mutate(days_rearrest = as.numeric(days_rearrest))

#One-Way ANOVA for rearrests
oneway.test(formula = days_rearrest ~ county,
            data = stat15_avg_time_county_ccc,
            var.equal = TRUE)

stat15_res_aov_ccc <- aov(formula = days_rearrest ~ county,
                   data = stat15_avg_time_county_ccc,
                   projections = TRUE,
                   qr = TRUE,
                   contrasts = TRUE)

summary(stat15_res_aov_ccc)
report(stat15_res_aov_ccc)
stat15c <- paste0("Criminal Contempt of Court: ", report_parameters(stat15_res_aov_ccc))
report_table(stat15_res_aov_ccc)

stat15c_post_test <- multcomp::glht(stat15_res_aov_ccc,
                            linfct = multcomp::mcp(county = 'Tukey'))

#---------------------------------------------------------------------------------#
#ANALYSIS 16 RR by Initial Offense Type----
#NOTES:
#1. there are 797 offenders (391 of whom are rearrested) who's initial charge doesn't fall within one of the categories
#     -> they aren't included in report, but there should be a mention that these exist (for completeness but also b/c the total recidivism rate no longer matches Fig 1)
#2 total recidivism rate line is incorrect -> it's 53.8%, see Fig 1
#---------------------------------------------------------------------------------#
table16_recidivism_rate_initial <- master_df_6 %>%
  # filter(!is.na(Rearrest)) %>% #must remove NAs so that totals match Tim's -> SPSS rmoves the missing values (see "Missing Value Handling -> Definition of Missing in spv file)
  count(initial_offense_category, Rearrest, name = "r") %>%
  group_by(initial_offense_category) %>%
  mutate(sample_size = sum(r)) %>% 
  ungroup() %>%
  filter(Rearrest == 1) %>%
  mutate(prop_recidivate = r/sample_size,
         prop_recidivate_pretty = percent(r/sample_size, accuracy = 0.1))

table16 <- table16_recidivism_rate_initial %>% 
  select(initial_offense_category, r, sample_size, prop_recidivate_pretty)

stat16_chi_initial_offense_rearrest <- chisq.test(master_df_6$initial_offense_category, master_df_6$Rearrest)
# stat16_phi_initial_offense_rearrest <- sqrt(stat16_chi_initial_offense_rearrest$statistic/sum(table16_recidivism_rate_initial$sample_size)) #exclude the NAs to be consistent with Tim
# stat16 <- paste0("phi = ", round_half_up(phi_initial_offense_rearrest, digits = 3), ", <em>p</em> < ", signif_half_up(chi_initial_offense_rearrest$p.value, digits = 3))
stat16_n <- sum(table16_recidivism_rate_initial$sample_size)
stat16_p_value <- ifelse(signif_half_up(stat16_chi_initial_offense_rearrest$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat16_chi_initial_offense_rearrest$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat16_description <- if_else(stat16_chi_initial_offense_rearrest$p.value < .05,
                               "There was a statistically significant difference in recidivism rates between initial offense types, ",
                               "The difference in recidivism rates between initial offense types was not statistically significant, ")
stat16 <- paste0(stat16_description,
                  "$\\chi^2$(", stat16_chi_initial_offense_rearrest$parameter, ", <em>N</em> = ", stat16_n,
                  ") = ", round_half_up(stat16_chi_initial_offense_rearrest$statistic, digits = 2), 
                  stat16_p_value)

table16_recidivism_rate_total <- sum(table16_recidivism_rate_initial$r)/stat16_n

figure16 <- ggplot(table16_recidivism_rate_initial, aes(x = initial_offense_category, y = prop_recidivate)) +
  geom_col(fill = "gray81") +
  scale_x_discrete(name = NULL, labels = paste0(table16_recidivism_rate_initial$initial_offense_category, " (n = ", table16_recidivism_rate_initial$sample_size, ")",
                                                "\n Recidivism = ",table16_recidivism_rate_initial$prop_recidivate_pretty),
                   guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(name = "Recidivism Rate", labels = percent_format(accuracy = 0.1), breaks = seq(0, 1, by = .1), limits = c(0,1)) +
  ggtitle(paste0("Recidivism Rates by Initial Offense Type, FY ", fy, " Cohort")) +
  # geom_text(aes(label = prop_recidivate_pretty), nudge_y = -0.05, color = "white") +
  geom_hline(yintercept = table16_recidivism_rate_total, color = "red", linewidth = 1, linetype = 2) +
  annotate("text", label = paste0("FY", fy, " Total Recividism Rate (",
                                  percent(table16_recidivism_rate_total, accuracy = 0.1),")"), x = 4, y = table16_recidivism_rate_total + 0.05, color = "black") +
  theme_bw() +
  # theme(axis.text.x=element_text(angle=15, hjust=1)) +
  NULL
#see link for description of other felony offenses
#https://www.bjs.gov/index.cfm/dataonline/index.cfm?ty=tdtp&tid=2
caption16 <- paste0("The initial offense category was determined for probationers and MTR-prisoners only because we did not have
                    data for parolees. ",
                    "The initial offense categories were determined by the following rules: ",
                    'If the offense_degree was a misdemeanor or petty misdemeanor, it was categorized as "Misdemeanor and other." ',
                    'If the offense chapter began with "707" and the section was 7[01256][0-9], then it was categorized as "Non-sex Violent Offenses." ',
                    'Robbery (708-84[0-2]) and abuse of family or household members (709-906) were also categorized as "Non-sex Violent Offenses." ', 
                    'If the offense chapter began with "707" and the section was 7[3-4][0-49], then it was categorized as "Sex Offenses." ',
                    'If the offense chapter began with "708" and the section was 8[0-35-9][0-9], then it was categorized as "Property Offenses." ',
                    'If the offense chapter began with "712" and the section was 12[45][0-9], then it was categorized as "Drug Offenses," ',
                    'If the offense chapter began with "329" (Uniformed Controlled Substances Act) then it was also categorized as "Drug Offenses." ',
                    'If it fell in non of these categories, then it was categorized as "Felony Other," ',
                    'which included weapons offenses, driving-related offenses, and other public-order offenses (flight/escape, parole or probation violations, prison contraband, habitual offender).')
#---------------------------------------------------------------------------------#
#ANALYSIS 17 RR by Initial Offense Type and Recidivism Type----
#NOTES:
#1. the N's for each category of offense are incorrect; can check by looking at Fig 16
#2. for recidivism type, the overall recid rate includes misdemeanor category even though plot doesn't
#3. for recidivism type, the overall recid rate is divided by 1365 for rearrests but 1352 for revoc and ccc, leads my rearrests to slightly differ from Tim's b/c I used 1352 for all (1352 is total when excluding NAs from master_df_6$rearrest)
#4. total recidivism rate line is incorrect -> it's 53.8%, see Fig 1
#5. phi coefficients for crim_arrest slightly off but the output crosstabs are the same in mine vs Tim's, so possibly a change to the SPSS code?
#---------------------------------------------------------------------------------#
table17_recidivism_type_initial_offense_ICIS_arrest <- master_df_6 %>%
  # filter(!is.na(Rearrest)) %>% #must remove NAs so that totals match Tim's -> SPSS rmoves the missing values (see "Missing Value Handling -> Definition of Missing in spv file)
  count(initial_offense_category, ICIS_arrest, name = "r") %>%
  full_join(initial_offense_x_ICIS_arrest_factors) %>% 
  replace_na(list(r = 0)) %>% 
  group_by(initial_offense_category) %>%
  mutate(sample_size = sum(r)) %>% 
  ungroup() %>%
  # filter(ICIS_arrest != "No Rearrest") %>% #FY16
  filter(!is.na(ICIS_arrest)) %>% #FY17 %>% 
  mutate(prop_recidivate = round_half_up(r/sample_size, digits = 3),
         prop_recidivate_pretty = percent(r/sample_size, accuracy = 0.1))

table17 <- table17_recidivism_type_initial_offense_ICIS_arrest %>% 
  pivot_wider(id_cols = ICIS_arrest, names_from = initial_offense_category, values_from = c(r, sample_size, prop_recidivate_pretty)) %>% 
  mutate(`Non-sex Violent Offenses` = paste0(`prop_recidivate_pretty_Non-sex Violent Offenses`, " (r = ", `r_Non-sex Violent Offenses`, ", n = ", `sample_size_Non-sex Violent Offenses`, ")"),
         `Sex Offenses` = paste0(`prop_recidivate_pretty_Sex Offenses`, " (r = ", `r_Sex Offenses`, ", n = ", `sample_size_Sex Offenses`, ")"),
         `Property Offenses` = paste0(`prop_recidivate_pretty_Property Offenses`, " (r = ", `r_Property Offenses`, ", n = ", `sample_size_Property Offenses`, ")"),
         `Drug Offenses` = paste0(`prop_recidivate_pretty_Drug Offenses`, " (r = ", `r_Drug Offenses`, ", n = ", `sample_size_Drug Offenses`, ")"),
         `Felony Other` = paste0(`prop_recidivate_pretty_Felony Other`, " (r = ", `r_Felony Other`, ", n = ", `sample_size_Felony Other`, ")"),
         `Misdemeanor and other` = paste0(`prop_recidivate_pretty_Misdemeanor and other`, " (r = ", `r_Misdemeanor and other`, ", n = ", `sample_size_Misdemeanor and other`, ")")
         ) %>% 
  # inner_join(table13a) %>% 
  # mutate(ICIS_arrest = paste0(ICIS_arrest, " (R = ", R_icis_arrest, ", n = ", n_icis_arrest, ")")) %>% 
  select(ICIS_arrest, `Non-sex Violent Offenses`, `Sex Offenses`, `Property Offenses`, `Drug Offenses`, `Felony Other`, `Misdemeanor and other`)

table17_recidivism_rate_initial_offense <- table17_recidivism_type_initial_offense_ICIS_arrest %>%
  group_by(initial_offense_category) %>%
  summarize(r = sum(r), sample_size = first(sample_size), prop_recidivate = sum(prop_recidivate), prop_recidivate_pretty = percent(prop_recidivate, accuracy = 0.1)) %>% 
  adorn_totals()

table17_sample_size <- table17_recidivism_rate_initial_offense$sample_size[table17_recidivism_rate_initial_offense$initial_offense_category=="Total"]

table17a <- table17_recidivism_type_initial_offense_ICIS_arrest %>%
  # filter(!is.na(initial_offense_category)) %>%
  group_by(ICIS_arrest) %>%
  summarize(r_icis_arrest = sum(r, na.rm = TRUE), 
            n_icis_arrest = table17_sample_size, 
            recidivism_rate = round_half_up(r_icis_arrest/table17_sample_size, digits = 3), 
            recidivism_rate_pretty = percent(recidivism_rate, accuracy = 0.1))

table17_recidivism_rate_total <- sum(table17_recidivism_rate_initial_offense$r)/sum(table17_recidivism_rate_initial_offense$sample_size)

stat17a_initial_offense_crim_arrest <- master_df_6 %>%
  # filter(!is.na(Rearrest)) %>%
  mutate(ICIS_arrest = if_else(ICIS_arrest == "Criminal Arrest", 1, 0))
stat17a_chi_initial_offense_crim_arrest <- chisq.test(stat17a_initial_offense_crim_arrest$initial_offense_category, stat17a_initial_offense_crim_arrest$ICIS_arrest)
# stat17_phi_initial_offense_crim_arrest <- sqrt(stat17a_chi_initial_offense_crim_arrest$statistic/1365)
# stat17a <- paste0("phi = ", round_half_up(phi_initial_offense_crim_arrest, digits = 3), ", <em>p</em> < ", signif_half_up(chi_initial_offense_crim_arrest$p.value, digits = 3))
stat17a_n <- table17_sample_size
stat17a_p_value <- ifelse(signif_half_up(stat17a_chi_initial_offense_crim_arrest$p.value, digits = 3) >= .001, 
                         paste0(", <em>p</em> = ", signif_half_up(stat17a_chi_initial_offense_crim_arrest$p.value, digits = 3)), 
                         paste0(", <em>p</em> < .001"))
stat17a_description <- if_else(stat17a_chi_initial_offense_crim_arrest$p.value < .05,
                              "There was a statistically significant difference in criminal arrests between initial offense types, ",
                              "The difference in criminal arrests between initial offense types was not statistically significant, ")
stat17a <- paste0(stat17a_description,
                 "$\\chi^2$(", stat17a_chi_initial_offense_crim_arrest$parameter, ", <em>N</em> = ", stat17a_n,
                 ") = ", round_half_up(stat17a_chi_initial_offense_crim_arrest$statistic, digits = 2), 
                 stat17a_p_value)

stat17b_initial_offense_revoc <- master_df_6 %>%
  filter(!is.na(Rearrest)) %>%
  mutate(ICIS_arrest = if_else(ICIS_arrest == "Probation Revocations/Parole Violations", 2, 0))
stat17b_chi_initial_offense_revoc <- chisq.test(stat17b_initial_offense_revoc$initial_offense_category, stat17b_initial_offense_revoc$ICIS_arrest)
# stat17b_phi_initial_offense_revoc <- sqrt(chi_initial_offense_revoc$statistic/1352)
# stat17b <- paste0("phi = ", round_half_up(phi_initial_offense_revoc, digits = 3), ", <em>p</em> < ", signif_half_up(chi_initial_offense_revoc$p.value, digits = 3))
stat17b_n <- table17_sample_size
stat17b_p_value <- ifelse(signif_half_up(stat17b_chi_initial_offense_revoc$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat17b_chi_initial_offense_revoc$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat17b_description <- if_else(stat17b_chi_initial_offense_revoc$p.value < .05,
                               "There was a statistically significant difference in probation revocations/parole violations between initial offense types, ",
                               "The difference in probation revocations/parole violations between initial offense types was not statistically significant, ")
stat17b <- paste0(stat17b_description,
                  "$\\chi^2$(", stat17b_chi_initial_offense_revoc$parameter, ", <em>N</em> = ", stat17b_n,
                  ") = ", round_half_up(stat17b_chi_initial_offense_revoc$statistic, digits = 2), 
                  stat17b_p_value)

stat17c_initial_offense_ccc <- master_df_6 %>%
  filter(!is.na(Rearrest)) %>%
  mutate(ICIS_arrest = if_else(ICIS_arrest == "Criminal Contempt of Court", 3, 0))
stat17c_chi_initial_offense_ccc <- chisq.test(stat17c_initial_offense_ccc$initial_offense_category, stat17c_initial_offense_ccc$ICIS_arrest)
# stat17c_phi_initial_offense_ccc <- sqrt(chi_initial_offense_ccc$statistic/1352)
# stat17c <- paste0("phi = ", round_half_up(phi_initial_offense_ccc, digits = 3), ", <em>p</em> < ", signif_half_up(chi_initial_offense_ccc$p.value, digits = 3))
stat17c_n <- table17_sample_size
stat17c_p_value <- ifelse(signif_half_up(stat17c_chi_initial_offense_ccc$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat17c_chi_initial_offense_ccc$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat17c_description <- if_else(stat17c_chi_initial_offense_ccc$p.value < .05,
                               "There was a statistically significant difference in criminal contempt of court between initial offense types, ",
                               "The difference in criminal contempt of court between initial offense types was not statistically significant, ")
stat17c <- paste0(stat17c_description,
                  "$\\chi^2$(", stat17c_chi_initial_offense_ccc$parameter, ", <em>N</em> = ", stat17c_n,
                  ") = ", round_half_up(stat17c_chi_initial_offense_ccc$statistic, digits = 2), 
                  stat17c_p_value)

# #repeat of stat16
# stat17d_initial_offense_total <- master_df_6 %>%
#   filter(!is.na(Rearrest)) #%>%
# # mutate(ICIS_arrest = if_else(ICIS_arrest == 3, 3, 0))
# stat17d_chi_initial_offense_total <- chisq.test(stat17d_initial_offense_total$initial_offense_category, stat17d_initial_offense_total$Rearrest)
# # stat17d_phi_initial_offense_total <- sqrt(chi_initial_offense_total$statistic/1352)
# # stat17d <- paste0("phi = ", round_half_up(phi_initial_offense_total, digits = 3), ", <em>p</em> < ", signif_half_up(chi_initial_offense_total$p.value, digits = 3))
# stat17d_n <- table17_sample_size
# stat17d_p_value <- ifelse(signif_half_up(stat17d_chi_initial_offense_total$p.value, digits = 3) >= .001, 
#                           paste0(", <em>p</em> = ", signif_half_up(stat17d_chi_initial_offense_total$p.value, digits = 3)), 
#                           paste0(", <em>p</em> < .001"))
# stat17d_description <- if_else(stat17d_chi_initial_offense_total$p.value < .05,
#                                "There was a statistically significant difference in recidivism rates between initial offense types, ",
#                                "The difference in recidivism rates between initial offense types was not statistically significant, ")
# stat17d <- paste0(stat17d_description,
#                   "$\\chi^2$(", stat17d_chi_initial_offense_total$parameter, ", <em>N</em> = ", stat17d_n,
#                   ") = ", round_half_up(stat17d_chi_initial_offense_total$statistic, digits = 2), 
#                   stat17d_p_value)

figure17 <- ggplot(table17_recidivism_type_initial_offense_ICIS_arrest, aes(x = initial_offense_category, y = prop_recidivate, fill = ICIS_arrest)) +
  geom_col() +
  # geom_text(data = recidivism_type_agency,
  #           mapping = aes(x = agency, y = prop_recidivate,
  #                         label =  percent(prop_recidivate, accuracy = 0.1)),
  #           nudge_y = .06,
  #           color = "black") +
  geom_hline(yintercept = table17_recidivism_rate_total, color = "red", linewidth = 1, linetype = 2) +
  annotate("text", label = paste0("FY", fy, " Total Recividism Rate (",
                                  percent(table17_recidivism_rate_total, accuracy = 0.1),")"), x = 4, y = table17_recidivism_rate_total - 0.05, color = "black") +
  annotate(geom = "text", x = 1, y = table17_recidivism_rate_initial_offense$prop_recidivate[1] + .05,
           label = percent(table17_recidivism_rate_initial_offense$prop_recidivate[1], accuracy = 0.1, suffix = "%")) +
  annotate(geom = "text", x = 2, y = table17_recidivism_rate_initial_offense$prop_recidivate[2] + .05,
           label = percent(table17_recidivism_rate_initial_offense$prop_recidivate[2], accuracy = 0.1, suffix = "%")) +
  annotate(geom = "text", x = 3, y = table17_recidivism_rate_initial_offense$prop_recidivate[3] + .05,
           label = percent(table17_recidivism_rate_initial_offense$prop_recidivate[3], accuracy = 0.1, suffix = "%")) +
  annotate(geom = "text", x = 4, y = table17_recidivism_rate_initial_offense$prop_recidivate[4] + .05,
           label = percent(table17_recidivism_rate_initial_offense$prop_recidivate[4], accuracy = 0.1, suffix = "%")) +
  annotate(geom = "text", x = 5, y = table17_recidivism_rate_initial_offense$prop_recidivate[5] + .05,
           label = percent(table17_recidivism_rate_initial_offense$prop_recidivate[5], accuracy = 0.1, suffix = "%")) +
  annotate(geom = "text", x = 6, y = table17_recidivism_rate_initial_offense$prop_recidivate[6] + .05,
           label = percent(table17_recidivism_rate_initial_offense$prop_recidivate[6], accuracy = 0.1, suffix = "%")) +
  annotate(geom = "text", x = 7, y = table17_recidivism_rate_initial_offense$prop_recidivate[7] + .05,
           label = percent(table17_recidivism_rate_initial_offense$prop_recidivate[7], accuracy = 0.1, suffix = "%")) +
  scale_x_discrete(name = NULL, labels = paste0(table17_recidivism_rate_initial_offense$initial_offense_category, " (n = ", table17_recidivism_rate_initial_offense$sample_size, ")"),
                   guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(name = "Recidivism Rate", labels = percent_format(accuracy = 0.1), breaks = seq(0, 1, by = .1), limits = c(0,1)) +
  scale_fill_brewer(name = "Recidivism Type", type = "seq", direction = -1) +
  ggtitle(paste0("Recidivism Rates by Initial Offense Type and Recidivism Type, FY ", fy, " Cohort")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  NULL

#---------------------------------------------------------------------------------#
#ANALYSIS 18 RR by Offender Type and Sex----
#NOTES:
#1.
#---------------------------------------------------------------------------------#
table18_recidivism_rate_offender_type_sex <- master_df_6 %>%
  # filter(!is.na(Rearrest)) %>% #must remove NAs so that totals match Tim's -> SPSS rmoves the missing values (see "Missing Value Handling -> Definition of Missing in spv file)
  count(agency, Rearrest, SEX, name = "r") %>%
  full_join(agency_x_sex_factors) %>% 
  replace_na(list(r = 0)) %>% 
  group_by(agency) %>%
  mutate(sample_size_agency = sum(r)) %>% 
  ungroup() %>%
  group_by(SEX) %>%
  mutate(sample_size_sex = sum(r)) %>%
  ungroup() %>%
  group_by(agency, SEX) %>%
  mutate(sample_size_agency_sex = sum(r)) %>%
  ungroup() %>%
  filter(Rearrest != "No Rearrest") %>% #FY16
  filter(Rearrest == 1) %>% #FY17
  mutate(recidivism_rate = round_half_up(r/sample_size_agency_sex, digits = 3),
         recidivism_rate_pretty = percent(recidivism_rate, accuracy = 0.1))

table18 <- table18_recidivism_rate_offender_type_sex %>% 
  pivot_wider(id_cols = SEX, names_from = agency, values_from = c(r, sample_size_agency_sex, recidivism_rate_pretty)) %>% 
  mutate(Probationers = paste0(recidivism_rate_pretty_Probationers, " (r = ", r_Probationers,", n = ",sample_size_agency_sex_Probationers,")"),
         Parolees = paste0(recidivism_rate_pretty_Parolees, " (r = ", r_Parolees,", n = ",sample_size_agency_sex_Parolees,")"),
         `MTR-Prisoners` = paste0(`recidivism_rate_pretty_MTR-Prisoners`, " (r = ", `r_MTR-Prisoners`,", n = ",`sample_size_agency_sex_MTR-Prisoners`,")")) %>% 
  select(SEX, Probationers, Parolees, `MTR-Prisoners`) 

table18_recidivism_rate_agency <- table18_recidivism_rate_offender_type_sex %>%
  group_by(agency) %>%
  summarize(r = sum(r), sample_size = first(sample_size_agency), recidivism_rate = round_half_up(r/sample_size, digits = 3))

table18a <- table18_recidivism_rate_offender_type_sex %>%
  group_by(SEX) %>%
  summarize(r = sum(r), sample_size = first(sample_size_sex), `Recidivism Rate` = percent(round_half_up(r/sample_size, digits = 3), accuracy = 0.1))

table18_recidivism_rate_total <- sum(table18_recidivism_rate_agency$r)/sum(table18_recidivism_rate_agency$sample_size)

stat18a_sex_prob <- master_df_6 %>%
  filter(agency == "Probationers") #%>%
# mutate(SEX = if_else(SEX == 1, 1, 0))
stat18a_chi_initial_sex_prob <- chisq.test(stat18a_sex_prob$Rearrest, stat18a_sex_prob$SEX)
# phi_initial_sex_prob <- sqrt(chi_initial_sex_prob$statistic/1301)
# stat18a <- paste0("phi = ", round_half_up(phi_initial_sex_prob, digits = 3), ", <em>p</em> < ", signif_half_up(chi_initial_sex_prob$p.value, digits = 3))
stat18a_n <- table18_recidivism_rate_agency$sample_size[table18_recidivism_rate_agency$agency == "Probationers"]
stat18a_p_value <- ifelse(signif_half_up(stat18a_chi_initial_sex_prob$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat18a_chi_initial_sex_prob$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat18a_description <- if_else(stat18a_chi_initial_sex_prob$p.value < .05,
                               "There was a statistically significant difference in recidivism rates between male and female probationers, ",
                               "The difference in recidivism rates between male and female probationers was not statistically significant, ")
stat18a <- paste0(stat18a_description,
                  "$\\chi^2$(", stat18a_chi_initial_sex_prob$parameter, ", <em>N</em> = ", stat18a_n,
                  ") = ", round_half_up(stat18a_chi_initial_sex_prob$statistic, digits = 2), 
                  stat18a_p_value)

stat18b_sex_par <- master_df_6 %>%
  filter(agency == "Parolees") #%>%
# mutate(SEX = if_else(SEX == 1, 1, 0))
stat18b_chi_initial_sex_par <- chisq.test(stat18b_sex_par$Rearrest, stat18b_sex_par$SEX)
# stat18b_phi_initial_sex_par <- sqrt(chi_initial_sex_par$statistic/531)
# stat18b <- paste0("phi = ", round_half_up(phi_initial_sex_par, digits = 3), ", <em>p</em> < ", signif_half_up(chi_initial_sex_par$p.value, digits = 3))
stat18b_n <- table18_recidivism_rate_agency$sample_size[table18_recidivism_rate_agency$agency == "Parolees"]
stat18b_p_value <- ifelse(signif_half_up(stat18b_chi_initial_sex_par$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat18b_chi_initial_sex_par$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat18b_description <- if_else(stat18b_chi_initial_sex_par$p.value < .05,
                               "There was a statistically significant difference in recidivism rates between male and female parolees, ",
                               "The difference in recidivism rates between male and female parolees was not statistically significant, ")
stat18b <- paste0(stat18b_description,
                  "$\\chi^2$(", stat18b_chi_initial_sex_par$parameter, ", <em>N</em> = ", stat18b_n,
                  ") = ", round_half_up(stat18b_chi_initial_sex_par$statistic, digits = 2), 
                  stat18b_p_value)

stat18c_sex_max <- master_df_6 %>%
  filter(agency == "MTR-Prisoners") #%>%
# mutate(SEX = if_else(SEX == 1, 1, 0))
stat18c_chi_initial_sex_max <- chisq.test(stat18c_sex_max$Rearrest, stat18c_sex_max$SEX)
# stat18c_phi_initial_sex_max <- sqrt(chi_initial_sex_max$statistic/317)
# stat18c <- paste0("phi = ", round_half_up(phi_initial_sex_max, digits = 3), ", <em>p</em> < ", signif_half_up(chi_initial_sex_max$p.value, digits = 3))
stat18c_n <- table18_recidivism_rate_agency$sample_size[table18_recidivism_rate_agency$agency == "MTR-Prisoners"]
stat18c_p_value <- ifelse(signif_half_up(stat18c_chi_initial_sex_max$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat18c_chi_initial_sex_max$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat18c_description <- if_else(stat18c_chi_initial_sex_max$p.value < .05,
                               "There was a statistically significant difference in recidivism rates between male and female MTR-prisoners, ",
                               "The difference in recidivism rates between male and female MTR-prisoners was not statistically significant, ")
stat18c <- paste0(stat18c_description,
                  "$\\chi^2$(", stat18c_chi_initial_sex_max$parameter, ", <em>N</em> = ", stat18c_n,
                  ") = ", round_half_up(stat18c_chi_initial_sex_max$statistic, digits = 2), 
                  stat18c_p_value)

stat18d_sex_tot <- master_df_6 #%>%
# filter(agency == 1) #%>%
# mutate(SEX = if_else(SEX == 1, 1, 0))
stat18d_chi_initial_sex_tot <- chisq.test(stat18d_sex_tot$Rearrest, stat18d_sex_tot$SEX)
# stat18d_phi_initial_sex_tot <- sqrt(chi_initial_sex_tot$statistic/2149)
# stat18d <- paste0("phi = ", round_half_up(phi_initial_sex_tot, digits = 3), ", <em>p</em> < ", signif_half_up(chi_initial_sex_tot$p.value, digits = 3))
stat18d_n <- table18_recidivism_rate_agency$sample_size[table18_recidivism_rate_agency$agency == "MTR-Prisoners"]
stat18d_p_value <- ifelse(signif_half_up(stat18d_chi_initial_sex_tot$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat18d_chi_initial_sex_tot$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat18d_description <- if_else(stat18d_chi_initial_sex_tot$p.value < .05,
                               "There was a statistically significant difference in recidivism rates between all males and all females, ",
                               "The difference in recidivism rates between all males and all female was not statistically significant, ")
stat18d <- paste0(stat18d_description,
                  "$\\chi^2$(", stat18d_chi_initial_sex_tot$parameter, ", <em>N</em> = ", stat18d_n,
                  ") = ", round_half_up(stat18d_chi_initial_sex_tot$statistic, digits = 2), 
                  stat18d_p_value)

figure18 <- ggplot(table18_recidivism_rate_offender_type_sex, aes(x = agency, y = recidivism_rate, fill = SEX)) +
  geom_col(position = position_dodge2()) +
  geom_hline(yintercept = table18_recidivism_rate_total, color = "red", linewidth = 1, linetype = 2) +
  annotate("text", label = paste0("FY", fy, " Total Recividism Rate (",
                                  percent(table18_recidivism_rate_total, accuracy = 0.1),")"), x = 2, y = table18_recidivism_rate_total + 0.05, color = "black") +
  scale_x_discrete(name = NULL, labels = paste0(table18_recidivism_rate_agency$agency, " (n = ", table18_recidivism_rate_agency$sample_size, ")")) +
  scale_y_continuous(name = "Recidivism Rate", labels = percent_format(accuracy = 0.1), breaks = seq(0, 1, by = .1), limits = c(0,1)) +
  scale_fill_brewer(name = "Sex", type = "qual") +
  ggtitle(paste0("Recidivism Rates by Sex and Offender Type, FY ", fy, " Cohort")) +
  theme_bw() +
  NULL

#---------------------------------------------------------------------------------#
#ANALYSIS 19 RR by Offender Type and Ethnicity----
#NOTES:
#1.
#---------------------------------------------------------------------------------#
table19_recidivism_rate_offender_type_race <- master_df_6 %>%
  # filter(!is.na(Rearrest)) %>% #must remove NAs so that totals match Tim's -> SPSS rmoves the missing values (see "Missing Value Handling -> Definition of Missing in spv file)
  count(agency, Rearrest, Ethnicity, name = "r") %>%
  group_by(agency) %>%
  mutate(sample_size_agency = sum(r)) %>% 
  ungroup() %>%
  group_by(Ethnicity) %>%
  mutate(sample_size_race = sum(r)) %>%
  ungroup() %>%
  group_by(agency, Ethnicity) %>%
  mutate(sample_size_agency_race = sum(r)) %>%
  ungroup() %>%
  filter(Rearrest != "No Rearrest") %>% #FY16
  filter(Rearrest == 1) %>% #FY17
  mutate(recidivism_rate = round_half_up(r/sample_size_agency_race, digits = 3),
         recidivism_rate_pretty = percent(recidivism_rate, accuracy = 0.1))

table19 <- table19_recidivism_rate_offender_type_race %>% 
  pivot_wider(id_cols = agency, names_from = Ethnicity, values_from = c(r, sample_size_agency_race, recidivism_rate_pretty)) %>% 
  mutate(`Hawaiian-Part Hawn` = paste0(`recidivism_rate_pretty_Hawaiian-Part Hawn`, " (r = ", `r_Hawaiian-Part Hawn`, ", n = ", `sample_size_agency_race_Hawaiian-Part Hawn`, ")"),
         Caucasian = paste0(recidivism_rate_pretty_Caucasian, " (r = ", r_Caucasian, ", n = ", sample_size_agency_race_Caucasian, ")"),
         Filipino = paste0(recidivism_rate_pretty_Filipino, " (r = ", r_Filipino, ", n = ", sample_size_agency_race_Filipino, ")"),
         Japanese = paste0(recidivism_rate_pretty_Japanese, " (r = ", r_Japanese, ", n = ", sample_size_agency_race_Japanese, ")"),
         Samoan = paste0(recidivism_rate_pretty_Samoan, " (r = ", r_Samoan, ", n = ", sample_size_agency_race_Samoan, ")"),
         `African-American` = paste0(`recidivism_rate_pretty_African-American`, " (r = ", `r_African-American`, ", n = ", `sample_size_agency_race_African-American`, ")"),
         `All Others` = paste0(`recidivism_rate_pretty_All Others`, " (r = ", `r_All Others`, ", n = ", `sample_size_agency_race_All Others`, ")")
  ) %>% 
  select(agency, `Hawaiian-Part Hawn`, Caucasian, Filipino, Japanese, Samoan, `African-American`, `All Others`)

table19_recidivism_rate_agency <- table19_recidivism_rate_offender_type_race %>%
  group_by(agency) %>%
  summarize(r = sum(r), sample_size = first(sample_size_agency), recidivism_rate = round_half_up(r/sample_size, digits = 3))

table19a <- table19_recidivism_rate_offender_type_race %>%
  group_by(Ethnicity) %>%
  summarize(r = sum(r), sample_size = first(sample_size_race), "Recidivism Rate" = percent(round_half_up(r/sample_size, digits = 3), accuracy = 0.1))

table19_recidivism_rate_total <- sum(table19_recidivism_rate_agency$r)/sum(table19_recidivism_rate_agency$sample_size)

figure19 <- ggplot(table19_recidivism_rate_offender_type_race, aes(x = Ethnicity, y = recidivism_rate, fill = agency)) +
  geom_col(position = position_dodge2()) +
  geom_hline(yintercept = table19_recidivism_rate_total, color = "red", linewidth = 1, linetype = 2) +
  annotate("text", label = paste0("FY", fy, " Total Recividism Rate (",
                                  percent(table19_recidivism_rate_total, accuracy = 0.1),")"), x = 4, y = table19_recidivism_rate_total + 0.05, color = "black") +
  scale_x_discrete(name = NULL, labels = paste0(table19a$Ethnicity, " (n = ", table19a$sample_size, ")"),
                   guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(name = "Recidivism Rate", labels = percent_format(accuracy = 0.1), breaks = seq(0, 1, by = .1), limits = c(0,1)) +
  # scale_fill_brewer(name = "Offender Type", type = "seq") +
  scale_fill_brewer(name = "Offender Type", type = "qual", palette = "Set3") +
  ggtitle(paste0("Recidivism Rates by Race and Offender Type, FY ", fy, " Cohort")) +
  theme_bw() +
  NULL

#---------------------------------------------------------------------------------#
#ANALYSIS 20 RR by Offender Type and Age----
#NOTES:
#1. FY16 total recidivism rate is incorrect in Tim's graph
#2. max-term release N's and %'s incorrect in Tim's table and graph -> accidentally copied and pasted from Parolees
#---------------------------------------------------------------------------------#
table20_recidivism_rate_offender_type_age_range <- master_df_6 %>%
  # filter(!is.na(Rearrest)) %>% #must remove NAs so that totals match Tim's -> SPSS rmoves the missing values (see "Missing Value Handling -> Definition of Missing in spv file)
  count(agency, Rearrest, age_range, name = "r") %>%
  full_join(agency_x_age_range_factors) %>% 
  replace_na(list(r = 0, Rearrest = 1)) %>% 
  group_by(agency) %>%
  mutate(sample_size_agency = sum(r)) %>% 
  ungroup() %>%
  group_by(age_range) %>%
  mutate(sample_size_age_range = sum(r)) %>%
  ungroup() %>%
  group_by(agency, age_range) %>%
  mutate(sample_size_agency_age_range = sum(r)) %>%
  ungroup() %>%
  filter(Rearrest != "No Rearrest") %>% #FY16
  filter(Rearrest == 1) %>% #FY17
  mutate(recidivism_rate = round_half_up(r/sample_size_agency_age_range, digits = 3),
         recidivism_rate_pretty = percent(recidivism_rate, accuracy = 0.1)) %>% 
  arrange(agency, age_range)

table20 <- table20_recidivism_rate_offender_type_age_range %>% 
  pivot_wider(id_cols = agency, names_from = age_range, values_from = c(r, sample_size_agency_age_range, recidivism_rate_pretty)) %>% 
  mutate(`< 20 years old` = paste0(`recidivism_rate_pretty_< 20 years old`, " (r = ", `r_< 20 years old`, ", n = ", `sample_size_agency_age_range_< 20 years old`, ")"),
         `20-29 years old` = paste0(`recidivism_rate_pretty_20-29 years old`, " (r = ", `r_20-29 years old`, ", n = ", `sample_size_agency_age_range_20-29 years old`, ")"),
         `30-39 years old` = paste0(`recidivism_rate_pretty_30-39 years old`, " (r = ", `r_30-39 years old`, ", n = ", `sample_size_agency_age_range_30-39 years old`, ")"),
         `40-49 years old` = paste0(`recidivism_rate_pretty_40-49 years old`, " (r = ", `r_40-49 years old`, ", n = ", `sample_size_agency_age_range_40-49 years old`, ")"),
         `50-59 years old` = paste0(`recidivism_rate_pretty_50-59 years old`, " (r = ", `r_50-59 years old`, ", n = ", `sample_size_agency_age_range_50-59 years old`, ")"),
         `+60 years old` = paste0(`recidivism_rate_pretty_+60 years old`, " (r = ", `r_+60 years old`, ", n = ", `sample_size_agency_age_range_+60 years old`, ")")  
         ) %>% 
  select(agency, `< 20 years old`, `20-29 years old`, `30-39 years old`, `40-49 years old`, `50-59 years old`, `+60 years old`)

table20_recidivism_rate_agency <- table20_recidivism_rate_offender_type_age_range %>%
  group_by(agency) %>%
  summarize(r = sum(r), sample_size = first(sample_size_agency), recidivism_rate = round_half_up(r/sample_size, digits = 3))

table20a <- table20_recidivism_rate_offender_type_age_range %>%
  group_by(age_range) %>%
  summarize(r = sum(r), sample_size = first(sample_size_age_range), recidivism_rate = round_half_up(r/sample_size, digits = 3))

table20_recidivism_rate_total <- sum(table20_recidivism_rate_agency$r)/sum(table20_recidivism_rate_agency$sample_size)

stat20a_age_prob <- master_df_6 %>%
  filter(agency == "Probationers") #%>%
# mutate(SEX = if_else(SEX == 1, 1, 0))
stat20a_chi_initial_age_prob <- chisq.test(stat20a_age_prob$Rearrest, stat20a_age_prob$age_range)
# phi_initial_age_prob <- sqrt(chi_initial_age_prob$statistic/1301)
# stat20a <- paste0("phi = ", round_half_up(phi_initial_age_prob, digits = 3), ", <em>p</em> < ", signif_half_up(chi_initial_age_prob$p.value, digits = 3))
stat20a_n <- table20_recidivism_rate_agency$sample_size[table20_recidivism_rate_agency$agency == "Probationers"]
stat20a_p_value <- ifelse(signif_half_up(stat20a_chi_initial_age_prob$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat20a_chi_initial_age_prob$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat20a_description <- if_else(stat20a_chi_initial_age_prob$p.value < .05,
                               "There was a statistically significant difference in recidivism rates between age groups among probationers, ",
                               "The difference in recidivism rates between age groups among probationers was not statistically significant, ")
stat20a <- paste0(stat20a_description,
                  "$\\chi^2$(", stat20a_chi_initial_age_prob$parameter, ", <em>N</em> = ", stat20a_n,
                  ") = ", round_half_up(stat20a_chi_initial_age_prob$statistic, digits = 2), 
                  stat18a_p_value)

stat20b_age_par <- master_df_6 %>%
  filter(agency == "Parolees") #%>%
# mutate(SEX = if_else(SEX == 1, 1, 0))
stat20b_chi_initial_age_par <- chisq.test(stat20b_age_par$Rearrest, stat20b_age_par$age_range)
# phi_initial_age_par <- sqrt(chi_initial_age_par$statistic/531)
# stat20b <- paste0("phi = ", round_half_up(phi_initial_age_par, digits = 3), ", <em>p</em> < ", signif_half_up(chi_initial_age_par$p.value, digits = 3))
stat20b_n <- table20_recidivism_rate_agency$sample_size[table20_recidivism_rate_agency$agency == "Parolees"]
stat20b_p_value <- ifelse(signif_half_up(stat20b_chi_initial_age_par$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat20b_chi_initial_age_par$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat20b_description <- if_else(stat20b_chi_initial_age_par$p.value < .05,
                               "There was a statistically significant difference in recidivism rates between age groups among parolees, ",
                               "The difference in recidivism rates between age groups among parolees was not statistically significant, ")
stat20b <- paste0(stat20b_description,
                  "$\\chi^2$(", stat20b_chi_initial_age_par$parameter, ", <em>N</em> = ", stat20b_n,
                  ") = ", round_half_up(stat20b_chi_initial_age_par$statistic, digits = 2), 
                  stat20b_p_value)

stat20c_age_max <- master_df_6 %>%
  filter(agency == "MTR-Prisoners") #%>%
# mutate(SEX = if_else(SEX == 1, 1, 0))
stat20c_chi_initial_age_max <- chisq.test(stat20c_age_max$Rearrest, stat20c_age_max$age_range)
# phi_initial_age_max <- sqrt(chi_initial_age_max$statistic/317)
# stat20c <- paste0("phi = ", round_half_up(phi_initial_age_max, digits = 3), ", <em>p</em> < ", signif_half_up(chi_initial_age_max$p.value, digits = 3))
stat20c_n <- table20_recidivism_rate_agency$sample_size[table20_recidivism_rate_agency$agency == "MTR-Prisoners"]
stat20c_p_value <- ifelse(signif_half_up(stat20c_chi_initial_age_max$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat20c_chi_initial_age_max$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat20c_description <- if_else(stat20c_chi_initial_age_max$p.value < .05,
                               "There was a statistically significant difference in recidivism rates between age groups among MTR-prisoners, ",
                               "The difference in recidivism rates between age groups among MTR-prisoners was not statistically significant, ")
stat20c <- paste0(stat20c_description,
                  "$\\chi^2$(", stat20c_chi_initial_age_max$parameter, ", <em>N</em> = ", stat20c_n,
                  ") = ", round_half_up(stat20c_chi_initial_age_max$statistic, digits = 2), 
                  stat20c_p_value)

stat20d_age_tot <- master_df_6 #%>%
# filter(agency == 1) #%>%
# mutate(SEX = if_else(SEX == 1, 1, 0))
stat20d_chi_initial_age_tot <- chisq.test(stat20d_age_tot$Rearrest, stat20d_age_tot$age_range)
# phi_initial_age_tot <- sqrt(chi_initial_age_tot$statistic/2149)
# stat20d <- paste0("phi = ", round_half_up(phi_initial_age_tot, digits = 3), ", <em>p</em> < ", signif_half_up(chi_initial_age_tot$p.value, digits = 3))
stat20d_n <- sum(table20_recidivism_rate_agency$sample_size)
stat20d_p_value <- ifelse(signif_half_up(stat20d_chi_initial_age_tot$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat20d_chi_initial_age_tot$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat20d_description <- if_else(stat20d_chi_initial_age_tot$p.value < .05,
                               "There was a statistically significant difference in recidivism rates between age groups among all ofenders, ",
                               "The difference in recidivism rates between age groups among all offenders was not statistically significant, ")
stat20d <- paste0(stat20d_description,
                  "$\\chi^2$(", stat20d_chi_initial_age_tot$parameter, ", <em>N</em> = ", stat20d_n,
                  ") = ", round_half_up(stat20d_chi_initial_age_tot$statistic, digits = 2), 
                  stat20d_p_value)

figure20 <- ggplot(table20_recidivism_rate_offender_type_age_range, aes(x = age_range, y = recidivism_rate, fill = agency)) +
  geom_col(position = position_dodge2()) +
  geom_hline(yintercept = table20_recidivism_rate_total, color = "red", linewidth = 1, linetype = 2) +
  annotate("text", label = paste0("FY", fy, " Total Recividism Rate (",
                                  percent(table20_recidivism_rate_total, accuracy = 0.1),")"), x = 3, y = table20_recidivism_rate_total + 0.05, color = "black") +
  scale_x_discrete(name = NULL, labels = paste0(table20a$age_range, " (n = ", table20a$sample_size, ")"),
                   guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(name = "Recidivism Rate", labels = percent_format(accuracy = 0.1), breaks = seq(0, 1, by = .1), limits = c(0,1)) +
  # scale_fill_brewer(name = "Offender Type", type = "seq") +
  scale_fill_brewer(name = "Offender Type", type = "qual", palette = "Set3") +
  ggtitle(paste0("Recidivism Rates by Age Range and Offender Type, FY ", fy, " Cohort")) +
  theme_bw() +
  NULL

#---------------------------------------------------------------------------------#
#ANALYSIS 21 RR by Type of Max-term released----
#NOTES:
#1. max term includes about 17 non-max term probationers and parolees with max term data
#2. the RR for max term is incorrect in Tim's graph, even accounting for the fact that his analysis swallows up probationers and parolees with max term data
#3. my max term total RR in this fig is slightly higher than in Fig 1 b/c it includes the 17 from probation and parolee most of whom reoffended
#---------------------------------------------------------------------------------#
table21_recidivism_rate_max_term_type <- master_df_6 %>%
  # filter(!is.na(Rearrest)) %>% #must remove NAs so that totals match Tim's -> SPSS rmoves the missing values (see "Missing Value Handling -> Definition of Missing in spv file)
  count(Rearrest, laststat, name = "r") %>%
  group_by(laststat) %>%
  mutate(sample_size_psd = sum(r),
         laststat = factor(laststat, levels = c("SF","PARV"))
         # Rearrest = as_factor(Rearrest),
         ) %>%
  ungroup() %>%
  # filter(Rearrest != "No Rearrest", PSD_laststat != "") %>% #FY16
  filter(Rearrest == 1, laststat != "") %>% #FY17
  mutate(recidivism_rate = round_half_up(r/sample_size_psd, digits = 3),
         recidivism_rate_pretty = percent(recidivism_rate, accuracy = 0.1)) %>% 
  arrange(laststat)

table21 <- table21_recidivism_rate_max_term_type %>% 
  mutate("Last Status" = case_when(laststat == "PARV" ~ "Parole Violators",
                                   laststat == "SF" ~ "Sentenced Felons")) %>% 
  select(`Last Status`, r, sample_size_psd, "Recidivism Rate" = recidivism_rate_pretty)

table21_recidivism_rate_psd_total <- sum(table21_recidivism_rate_max_term_type$r)/sum(table21_recidivism_rate_max_term_type$sample_size_psd)

stat21_DPS <- master_df_6 
stat21_chi_initial_dps <- chisq.test(stat21_DPS$Rearrest, stat21_DPS$laststat)
stat21_n <- sum(table21_recidivism_rate_max_term_type$sample_size_psd)
stat21_p_value <- ifelse(signif_half_up(stat21_chi_initial_dps$p.value, digits = 3) >= .001, 
                          paste0(", <em>p</em> = ", signif_half_up(stat21_chi_initial_dps$p.value, digits = 3)), 
                          paste0(", <em>p</em> < .001"))
stat21_description <- if_else(stat21_chi_initial_dps$p.value < .05,
                               "Among MTR-prisoners, there was a statistically significant difference in recidivism rates between sentenced felons and parole violators, ",
                               "Among MTR-prisoners, the difference in recidivism rates between sentenced felons and parole violators was not statistically significant, ")
stat21 <- paste0(stat21_description,
                  "$\\chi^2$(", stat21_chi_initial_dps$parameter, ", <em>N</em> = ", stat21_n,
                  ") = ", round_half_up(stat21_chi_initial_dps$statistic, digits = 2), 
                  stat21_p_value)

figure21 <- ggplot(table21_recidivism_rate_max_term_type, aes(x = laststat, y = recidivism_rate)) +
  geom_col(fill = "gray81") +
  geom_hline(yintercept = table21_recidivism_rate_psd_total, color = "red", linewidth = 1, linetype = 2) +
  annotate("text", label = paste0("FY", fy, " Total Recividism Rate (",
                                  percent(table21_recidivism_rate_psd_total, accuracy = 0.1),")"), x = 1.5, y = table21_recidivism_rate_psd_total + 0.05, color = "black") +
  scale_x_discrete(name = NULL, labels = paste0(table21_recidivism_rate_max_term_type$laststat, " (n = ", table21_recidivism_rate_max_term_type$sample_size_psd, ")")) +
  scale_y_continuous(name = "Recidivism Rate", labels = percent_format(accuracy = 0.1), breaks = seq(0, 1, by = .1), limits = c(0,1)) +
  ggtitle(paste0("Recidivism Rates between Max-term Sentenced Felons and Max-term Parole Violators, FY ", fy, " Cohort")) +
  theme_bw() +
  NULL

# #---------------------------------------------------------------------------------#
# #Appendix A Initial LSI-R Risk Classifications, FY 2015 and FY 2016 Probationer Cohorts----
# #NOTES: 
# #1. 
# #---------------------------------------------------------------------------------#
# recidivism_rate_lsi_initial <- master_df_6 %>%
#   filter(!is.na(Rearrest)) %>% #must remove NAs so that totals match Tim's -> SPSS rmoves the missing values (see "Missing Value Handling -> Definition of Missing in spv file)
#   filter(agency == 1) %>% #must remove NAs so that totals match Tim's -> SPSS rmoves the missing values (see "Missing Value Handling -> Definition of Missing in spv file)
#   count(IAriskcategories, Rearrest) %>%
#   filter(!is.na(IAriskcategories)) %>%
#   group_by(IAriskcategories) %>%
#   mutate(sample_size_IAriskcategories = sum(n),
#          Rearrest = as_factor(Rearrest),
#          IAriskcategories = as_factor(IAriskcategories)) %>% #h/t: https://stackoverflow.com/questions/39671621/extract-the-labels-attribute-from-labeled-tibble-columns-from-a-haven-import-f
#   ungroup() %>%
#   mutate(total_risk_category = sum(n)) %>% 
#   filter(Rearrest != "No Rearrest") %>%
#   mutate(recidivism_rate = round_half_up(n/sample_size_IAriskcategories, digits = 3),
#          recidivism_rate_pretty = percent(recidivism_rate, accuracy = 0.1),
#          classification_prop = round_half_up(sample_size_IAriskcategories/total_risk_category, digits = 3))
# 
# avg_time_recidivism_lsi_initial <- time_period_rearrest %>%
#   filter(!is.na(Rearrest)) %>% #must remove NAs so that totals match Tim's -> SPSS rmoves the missing values (see "Missing Value Handling -> Definition of Missing in spv file)
#   filter(agency == 1) %>% #must remove NAs so that totals match Tim's -> SPSS rmoves the missing values (see "Missing Value Handling -> Definition of Missing in spv file)
#   group_by(IAriskcategories) %>%
#   summarize(n = n(), avg_days_recidivate = mean(days_rearrest, na.rm = TRUE), avg_months_recidivate = round(as.numeric(mean(days_rearrest, na.rm = TRUE))/30, digits = 1)) %>% 
#   ungroup() %>% 
#   filter(!is.na(IAriskcategories)) %>%
#   mutate(IAriskcategories = as_factor(IAriskcategories)) %>% 
#   rename(sample_size_IAriskcategories = n)
# 
# recidivism_rate_avg_time_lsi_initial <- recidivism_rate_lsi_initial %>% 
#   inner_join(avg_time_recidivism_lsi_initial)
# 
# appA <- ggplot(recidivism_rate_avg_time_lsi_initial, aes(x = IAriskcategories, y = classification_prop)) +
#   geom_col() +
#   geom_text(aes(label = paste0(percent(classification_prop, accuracy = 0.1))),
#             vjust = 1.3, size = 5, color = "white") +
#   scale_x_discrete(name = NULL, labels = paste0(recidivism_rate_avg_time_lsi_initial$IAriskcategories, "\n",
#                                                 "recid rate = ", percent(recidivism_rate_avg_time_lsi_initial$recidivism_rate, accuracy = 0.1), "\n",
#                                                 "avg time to recid = ", recidivism_rate_avg_time_lsi_initial$avg_months_recidivate, " mo.")) +
#   scale_y_continuous(name = "% of cohort", labels = percent_format(accuracy = 0.1), breaks = seq(0, 1, by = .1), limits = c(0,1)) +
#   ggtitle(paste0("Appendix A:\nInitial LSI-R Risk Classifications\nFY", fy, " Probationer Cohort")) +
#   theme_bw() +
#   NULL 
# #---------------------------------------------------------------------------------#
# #Appendix B Most Recent LSI-R Risk Classifications, FY 2015 and FY 2016 Probationer Cohorts----
# #NOTES: 
# #1. 
# #---------------------------------------------------------------------------------#
# recidivism_rate_lsi_recent <- master_df_6 %>%
#   filter(!is.na(Rearrest)) %>% #must remove NAs so that totals match Tim's -> SPSS rmoves the missing values (see "Missing Value Handling -> Definition of Missing in spv file)
#   filter(agency == 1) %>% #must remove NAs so that totals match Tim's -> SPSS rmoves the missing values (see "Missing Value Handling -> Definition of Missing in spv file)
#   count(MRrisk_categories, Rearrest) %>%
#   filter(!is.na(MRrisk_categories)) %>%
#   group_by(MRrisk_categories) %>%
#   mutate(sample_size_MRriskcategories = sum(n),
#          Rearrest = as_factor(Rearrest),
#          MRrisk_categories = as_factor(MRrisk_categories)) %>% #h/t: https://stackoverflow.com/questions/39671621/extract-the-labels-attribute-from-labeled-tibble-columns-from-a-haven-import-f
#   ungroup() %>%
#   mutate(total_risk_category = sum(n)) %>% 
#   filter(Rearrest != "No Rearrest") %>%
#   mutate(recidivism_rate = round_half_up(n/sample_size_MRriskcategories, digits = 3),
#          recidivism_rate_pretty = percent(recidivism_rate, accuracy = 0.1),
#          classification_prop = round_half_up(sample_size_MRriskcategories/total_risk_category, digits = 3))
# 
# avg_time_recidivism_lsi_recent <- time_period_rearrest %>%
#   filter(!is.na(Rearrest)) %>% #must remove NAs so that totals match Tim's -> SPSS rmoves the missing values (see "Missing Value Handling -> Definition of Missing in spv file)
#   filter(agency == 1) %>% #must remove NAs so that totals match Tim's -> SPSS rmoves the missing values (see "Missing Value Handling -> Definition of Missing in spv file)
#   group_by(MRrisk_categories) %>%
#   summarize(n = n(), avg_days_recidivate = mean(days_rearrest, na.rm = TRUE), avg_months_recidivate = round(as.numeric(mean(days_rearrest, na.rm = TRUE))/30, digits = 1)) %>% 
#   ungroup() %>% 
#   filter(!is.na(MRrisk_categories)) %>%
#   mutate(MRrisk_categories = as_factor(MRrisk_categories)) %>% 
#   rename(sample_size_MRriskcategories = n)
# 
# recidivism_rate_avg_time_lsi_recent <- recidivism_rate_lsi_recent %>% 
#   inner_join(avg_time_recidivism_lsi_recent)
# 
# appB <- ggplot(recidivism_rate_avg_time_lsi_recent, aes(x = MRrisk_categories, y = classification_prop)) +
#   geom_col() +
#   geom_text(aes(label = paste0(percent(classification_prop, accuracy = 0.1))),
#             vjust = 1.3, size = 5, color = "white") +
#   scale_x_discrete(name = NULL, labels = paste0(recidivism_rate_avg_time_lsi_recent$MRrisk_categories, "\n",
#                                                 "recid rate = ", percent(recidivism_rate_avg_time_lsi_recent$recidivism_rate, accuracy = 0.1), "\n",
#                                                 "avg time to recid = ", recidivism_rate_avg_time_lsi_recent$avg_months_recidivate, " mo.")) +
#   scale_y_continuous(name = "% of cohort", labels = percent_format(accuracy = 0.1), breaks = seq(0, 1, by = .1), limits = c(0,1)) +
#   ggtitle(paste0("Appendix A:\nInitial LSI-R Risk Classifications\nFY", fy, " Probationer Cohort")) +
#   theme_bw() +
#   NULL 
# 
#---------------------------------------------------------------------------------#
#Bundle tables, stats, and figures/plots/graphs----
#---------------------------------------------------------------------------------#
table_list <- mget(ls(pattern = "table.*(1|2)?[0-9](a?(_caption)?)$"))
stat_list <- mget(ls(pattern = "stat(1|2)?[0-9][a-f]?a?$"))
figure_list <- mget(ls(pattern = "figure(1|2)?[0-9]([a-c]?|_8_9)$"))
caption_list <- mget(ls(pattern = "caption(1|2)?[0-9]([a-c]?|_8_9)$"))
rmarkdown_list <- list(tables = table_list, stats = stat_list, figures = figure_list)
saveRDS(rmarkdown_list, paste0("H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\FY17_recidivism_study\\recidivism_study_FY2017_rmarkdown_lists_", format(Sys.time(), "%Y-%m-%d_%H%M"), ".rds"))
saveRDS(caption_list, paste0("H:\\p17_CJRI\\p17s1_CJIS_recidivism_study\\FY17_recidivism_study\\recidivism_study_FY2017_caption_lists_", format(Sys.time(), "%Y-%m-%d_%H%M"), ".rds"))
