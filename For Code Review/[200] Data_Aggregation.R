
### PURPOSE:  The purpose of this program is to create a final dataset for modeling
### AUTHOR:   Jet Wang ###
### DATE:     June 1, 2017 ###

# Use package dplyr to run descriptive statistics
# install.packages("RODBC")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("data.table")
# install.packages('sqldf')
# install.packages('gmodels')
# install.packages('descr')
# install.packages('stringr')
# install.packages('pROC')
# install.packages('PRROC')
# install.packages("caret")
# install.packages("tidyr")
# install.packages("glmnet")
# install.packages("splitstackshape")
# install.packages('VennDiagram')
# install.packages("stringi")

library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)
library(sqldf)
library(RODBC)
library(gmodels)
library(descr)
library(stringr)
library(pROC)
library(PRROC)
library(caret)
library(tidyr)
library(glmnet)
library(ade4)
library(splitstackshape)
# library(VennDiagram)

setwd("T:/Sepsis Non-POA/Data") #set work library

## Load all data that were imported from Clarity
load("pat_enc.RData") 
load("DEMO.RData") 
load("Diagnosis.RData")
load("antibio.RData")
load("INP_ADM.RData") 
load("DID_details.RData") 
load("treatment.RData") 
load("Order_Results.RData")
load("DRG.RData") 

## Load all data that required further calculations
load("HISTORY.RData") # History data
load("OR_ALL.RData") # Order results
load("Vitals_ALL.RData") # Vital signs

## Load the final modeling data I created
# load("SEPSIS_FINAL.RData")

# remove(list = ls())



## Get IV ABX DTM with some specific statuses
IV_ABX <- antibio%>%
  filter(ORDER_STATUS %in% c(2,5,9,10,11))%>%
  mutate(HSP_ACCOUNT_ID = as.numeric(HSP_ACCOUNT_ID),
         IV_ABX_DTM = as.POSIXct(IV_ABX_DTM,format='%Y-%m-%d %H:%M:%S'),
         ORDER_START_DTM = as.POSIXct(ORDER_START_DTM,format='%Y-%m-%d %H:%M:%S'),
         ORDER_END_DTM = as.POSIXct(ORDER_END_DTM,format='%Y-%m-%d %H:%M:%S'))%>%
  distinct(HSP_ACCOUNT_ID, IV_ABX_DTM, ORDER_START_DTM, ORDER_END_DTM, .keep_all=FALSE)%>%
  arrange(HSP_ACCOUNT_ID, IV_ABX_DTM)%>%
  select(HSP_ACCOUNT_ID, IV_ABX_DTM, ORDER_START_DTM, ORDER_END_DTM)


## Check INP ADM missing dates
# check1 <- INP_ADM%>%
#   filter(is.na(INP_ADM_DTM))
## No missing INP dates -- GOOD

## Read in INP ADM time
INP_ADM_DTM <- left_join(INP_ADM,IV_ABX, by = "HSP_ACCOUNT_ID")%>%
  mutate(INP=1,
         INP_ADM_3 = INP_ADM_DTM + hours(3),
         INP_ADM_6 = INP_ADM_DTM + hours(6),
         INP_ADM_9 = INP_ADM_DTM + hours(9),
         INP_ADM_12 = INP_ADM_DTM + hours(12),
         INP_ADM_1D = INP_ADM_DTM + hours(24),
         INP_ADM_2D = INP_ADM_DTM + hours(48),
         INP_ADM_3D = INP_ADM_DTM + hours(72),
         INP_ADM_4D = INP_ADM_DTM + hours(96),
         INP_ABX_DTM = case_when( IV_ABX_DTM > INP_ADM_DTM ~ IV_ABX_DTM  ),
         INP_ABX_IND = case_when( IV_ABX_DTM > INP_ADM_DTM ~ 1, TRUE ~ 0  ))%>%
  arrange(HSP_ACCOUNT_ID, desc(INP_ABX_IND),INP_ABX_DTM )%>% #select the earliest ABX AFTER inpatient admission
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)%>%
  select(HSP_ACCOUNT_ID, INP_ADM_DTM, INP_ADM_3, INP_ADM_6, INP_ADM_9,INP_ADM_12,
         INP_ADM_1D, INP_ADM_2D, INP_ADM_3D, INP_ADM_4D,
         INP_ABX_DTM, INP_ABX_IND, DIS_DTM )

### Calculate the number of ABX orders during the ED stay

IV_ABX_Day0_COUNT <- left_join(INP_ADM, IV_ABX, by = "HSP_ACCOUNT_ID")%>%
  mutate(ED_ABX = case_when( IV_ABX_DTM < INP_ADM_DTM  ~ 1, TRUE ~ 0 ))
IV_ABX_Day0_COUNT <- as.data.frame(aggregate(IV_ABX_Day0_COUNT$ED_ABX, by=list(HSP_ACCOUNT_ID=IV_ABX_Day0_COUNT$HSP_ACCOUNT_ID), FUN=sum))%>%
  rename(IV_ABX_Day0_COUNT = x)

# IV_ABX_Day2_COUNT <- left_join(INP_ADM, IV_ABX, by = "HSP_ACCOUNT_ID")%>%
#   mutate(ED_ABX = case_when( IV_ABX_DTM < (INP_ADM_DTM + hours(24))  ~ 1, TRUE ~ 0 ))
# IV_ABX_Day2_COUNT <- as.data.frame(aggregate(IV_ABX_Day2_COUNT$ED_ABX, by=list(HSP_ACCOUNT_ID=IV_ABX_Day2_COUNT$HSP_ACCOUNT_ID), FUN=sum))%>%
#   rename(IV_ABX_Day2_COUNT = x)
# 
# IV_ABX_Day3_COUNT <- left_join(INP_ADM, IV_ABX, by = "HSP_ACCOUNT_ID")%>%
#   mutate(ED_ABX = case_when( IV_ABX_DTM < (INP_ADM_DTM + hours(48))  ~ 1, TRUE ~ 0 ))
# IV_ABX_Day3_COUNT <- as.data.frame(aggregate(IV_ABX_Day3_COUNT$ED_ABX, by=list(HSP_ACCOUNT_ID=IV_ABX_Day3_COUNT$HSP_ACCOUNT_ID), FUN=sum))%>%
#   rename(IV_ABX_Day3_COUNT = x)
# 
# IV_ABX_Day4_COUNT <- left_join(INP_ADM, IV_ABX, by = "HSP_ACCOUNT_ID")%>%
#   mutate(ED_ABX = case_when( IV_ABX_DTM < (INP_ADM_DTM + hours(72))  ~ 1, TRUE ~ 0 ))
# IV_ABX_Day4_COUNT <- as.data.frame(aggregate(IV_ABX_Day4_COUNT$ED_ABX, by=list(HSP_ACCOUNT_ID=IV_ABX_Day4_COUNT$HSP_ACCOUNT_ID), FUN=sum))%>%
#   rename(IV_ABX_Day4_COUNT = x)

# IV_ABX_DayX_COUNT <- left_join(INP_ADM, IV_ABX, by = "HSP_ACCOUNT_ID")%>%
#   mutate(ED_ABX = case_when( !is.na(IV_ABX_DTM)  ~ 1, TRUE ~ 0 ))
# IV_ABX_DayX_COUNT <- as.data.frame(aggregate(IV_ABX_DayX_COUNT$ED_ABX, by=list(HSP_ACCOUNT_ID=IV_ABX_DayX_COUNT$HSP_ACCOUNT_ID), FUN=sum))%>%
#   rename(IV_ABX_DayX_COUNT = x)

# sum(IV_ABX_Day1_COUNT$IV_ABX_Day1_COUNT)
# sum(IV_ABX_Day2_COUNT$IV_ABX_Day2_COUNT)
# sum(IV_ABX_Day3_COUNT$IV_ABX_Day3_COUNT)
# sum(IV_ABX_Day4_COUNT$IV_ABX_Day4_COUNT)
# sum(IV_ABX_DayX_COUNT$IV_ABX_DayX_COUNT)

### Created ABX indicators for those inpatients
INP_ABX <- left_join(INP_ADM_DTM, IV_ABX_Day0_COUNT, by = "HSP_ACCOUNT_ID")%>%
  # left_join(.,IV_ABX_Day2_COUNT, by = "HSP_ACCOUNT_ID")%>%
  # left_join(.,IV_ABX_Day3_COUNT, by = "HSP_ACCOUNT_ID")%>%
  # left_join(.,IV_ABX_Day4_COUNT, by = "HSP_ACCOUNT_ID")%>%
  # left_join(.,IV_ABX_DayX_COUNT, by = "HSP_ACCOUNT_ID")%>%
  mutate(ED_ABX_IND = case_when( IV_ABX_Day0_COUNT >= 1 ~ 1, TRUE ~ 0 ))

# any(data.frame(table(INP_ABX$HSP_ACCOUNT_ID))$Freq > 1)

### Get ICD-10 discharges only
INP_ADM_DTM_I10 <- INP_ABX%>%filter(DIS_DTM >= as.POSIXct("2015-10-01 00:00:00") & DIS_DTM < as.POSIXct("2017-10-01 00:00:00"))



# check1 <- left_join(INP_ADM_DTM_I10, sepsis%>%select(HSP_ACCOUNT_ID, ACTUAL, Type),by="HSP_ACCOUNT_ID")%>%
#   filter(ACTUAL==1 & Type=='NPOA')
# CrossTable(check1$ED_ABX_IND, check1$INP_ABX_IND, prop.chisq=FALSE, chisq = FALSE, format = 'SAS')

remove(INP_ADM, INP_ADM_DTM, IV_ABX_Day0_COUNT,IV_ABX_Day1_COUNT, IV_ABX_Day2_COUNT, IV_ABX_Day3_COUNT, IV_ABX_Day4_COUNT, IV_ABX_DayX_COUNT)


#Vitals <- Vitals%>%
# distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)

### Create demographics information
DEMOX <- left_join(DEMO, pat_enc%>%select(HSP_ACCOUNT_ID, HOSP_ADMSN_TIME_NEW), by = "HSP_ACCOUNT_ID")%>%
  arrange(HSP_ACCOUNT_ID, desc(ADM_TYPE))%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)%>%
  mutate(MARITAL_STATUS_C = case_when(
    MARITAL_STATUS == 1 ~ 'Single',
    MARITAL_STATUS == 2 ~ 'Married',
    TRUE ~ 'Other'),
    GENDER = case_when(SEX==1 ~ 'Female', TRUE ~ 'Male'),
    AGE = floor(new_interval(start = BIRTH_DATE, end = HOSP_ADMSN_TIME_NEW) / duration(num = 1, units = "years")))%>%
  select(-MARITAL_STATUS, -ADM_TYPE, -SEX,-HOSP_ADMSN_TIME_NEW,-BIRTH_DATE)
DEMOX$MARITAL_STATUS_C <- as.character(DEMOX$MARITAL_STATUS_C)
DEMOX$ADM_TYPE_NAME <- as.character(DEMOX$ADM_TYPE_NAME)
remove(DEMO)


# The complete ICD-10 code list to define sepsis cases, based on CMS definition
sepsis_ICD10_code <- 
  c('A02.1', 'A22.7', 'A26.7', 'A32.7', 'A40.0', 'A40.1', 'A40.3', 'A40.8', 'A40.9', 
    'A41.01', 'A41.02', 'A41.1', 'A41.2', 'A41.3', 'A41.4', 'A41.50', 'A41.51', 'A41.52', 
    'A41.53', 'A41.59', 'A41.81', 'A41.89', 'A41.9', 'A42.7', 'A54.86','B37.7','R65.20', 'R65.21')

# Find out what encounters have POA Sepsis 
# POA_sepsis_ID <- as.character(distinct(filter(diagnosis,POA == 1 & ICD10 %in%  sepsis_ICD10_code),HSP_ACCOUNT_ID, .keep_all = FALSE) )

### Split ICD10 combination to individual codes
diagnosis1 <- cSplit(diagnosis, "ICD10", ",")
# Maximum 7 codes for one account
remove(diagnosis)

# Create sepsis information
SEPSIS <- diagnosis1%>%
  mutate(ACTUAL = case_when( (ICD10_1 %in% sepsis_ICD10_code | ICD10_2 %in% sepsis_ICD10_code |
                                ICD10_3 %in% sepsis_ICD10_code | ICD10_4 %in% sepsis_ICD10_code |
                                ICD10_5 %in% sepsis_ICD10_code | ICD10_6 %in% sepsis_ICD10_code |
                                ICD10_7 %in% sepsis_ICD10_code ) ~ 1, TRUE ~ 0),
         Type = case_when(ACTUAL == 1 & POA == '1' ~ 'POA', ACTUAL == 1 & POA != '1' ~ 'NPOA', TRUE ~ NA_character_))%>%
  arrange(PAT_ID, HSP_ACCOUNT_ID, desc(ACTUAL), desc(Type))%>%
  distinct(PAT_ID, HSP_ACCOUNT_ID, .keep_all = TRUE)%>%
  select(PAT_ID, HSP_ACCOUNT_ID, ADM_DTM, DIS_DTM, ACTUAL, Type)
# No duplicate HSP_ACCOUNT_IDs--GOOD!


## Get Department ID information
DID <- DID_details%>%distinct(HSP_ACCOUNT_ID, DEPARTMENT_ID)
## Select Internal Medicine HSP_ACCOUNT_ID
# Import the treatment team list provided by Shelley
tteam_include_list <- read.csv(file='T:/Sepsis Non-POA/Files/From Shelley/TREATMENT_TEAM_INCLUDE.csv', header=TRUE)[,'TeamName']
# Import the department ID list provided by Shelley
DID_exclude_list <- read.csv(file='T:/Sepsis Non-POA/Files/From Shelley/DEPARTMENT_ID_EXCLUDE.csv', header=TRUE)[,'DEPARTMENT_ID']

# Use TREATMENT_NAME, TREATMENT_TITLE, and TREATMENT_ABBR to identify Internal Medicine teams
temp <- treatment%>%
  filter(toupper(TREATMENT_NAME) %in% tteam_include_list | toupper(TREATMENT_TITLE) %in% tteam_include_list | toupper(TREATMENT_ABBR) %in% tteam_include_list)%>%
  arrange(HSP_ACCOUNT_ID, TREATMENT_DTM)%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
all_IM_ID <- 
  left_join(temp, DID, by='HSP_ACCOUNT_ID', copy=F)%>%
  filter(!(DEPARTMENT_ID %in% DID_exclude_list) )%>% # The department ID should not be in the exclusion list
  arrange(HSP_ACCOUNT_ID, TREATMENT_DTM)%>% # Select the earliest DTM treated by IM teams
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)%>%
  select(HSP_ACCOUNT_ID, TREATMENT_DTM)%>%
  mutate(IM=1)
remove('tteam_include_list','DID_exclude_list','temp')

## Combine those indicators
SEPSIS2 <- 
  left_join(INP_ADM_DTM_I10%>%select(-DIS_DTM), SEPSIS, by = 'HSP_ACCOUNT_ID')%>% # Add INP ADM DTM
  left_join(., all_IM_ID, by = 'HSP_ACCOUNT_ID')%>% # Add Internal Medicine indicator
  left_join(., IV_ABX, by = 'HSP_ACCOUNT_ID')%>% # Add IV ABX time
  mutate(ACTUAL = case_when( is.na(ACTUAL) ~ 0, TRUE ~ ACTUAL ))%>% # Impute few cases who did not have case status
  filter(IM==1)

## Apply the 3h-12h criterion. Specifically, if the IV ABX ordering and the ending time overlaps 
## with the 3h-13 range, then this case has been suspected by the inpatient team.
## SUSP stands for "suspected"
SUSP <- SEPSIS2%>%
  filter( (IV_ABX_DTM <= INP_ADM_3 & ORDER_END_DTM >= INP_ADM_3 & ORDER_END_DTM <= INP_ADM_12  ) |
            (IV_ABX_DTM >= INP_ADM_3 & IV_ABX_DTM <= INP_ADM_12 & ORDER_END_DTM >= INP_ADM_3 & ORDER_END_DTM <= INP_ADM_12  ) | 
            (IV_ABX_DTM >= INP_ADM_3 & IV_ABX_DTM <= INP_ADM_12 & ORDER_END_DTM >= INP_ADM_12  ) |
            (IV_ABX_DTM <= INP_ADM_3 & ORDER_END_DTM >= INP_ADM_12  )  )%>%
  mutate(SUSP=1)%>%
  distinct(HSP_ACCOUNT_ID, .keep_all = TRUE)


## Create baseline data, one row for one HSP_ACCOUNT_ID
Baseline <- SEPSIS2%>%
  distinct(HSP_ACCOUNT_ID, .keep_all = TRUE)%>%
  select(-IV_ABX_DTM, -ORDER_START_DTM, -ORDER_END_DTM)
# freq(Baseline$ACTUAL)


## Combine those indicators
SEPSIS_FINAL <- left_join(Baseline, DEMOX, by="HSP_ACCOUNT_ID")%>%
  left_join(., SUSP%>%select(HSP_ACCOUNT_ID, SUSP), by="HSP_ACCOUNT_ID")%>%
  left_join(., HISTORY, by="HSP_ACCOUNT_ID")%>%
  left_join(., Vitals_ALL, by="HSP_ACCOUNT_ID")%>%
  left_join(., OR_ALL, by='HSP_ACCOUNT_ID')%>%

  mutate(SUSP = case_when(SUSP==1 ~ 1, TRUE ~ 0),
         ADM_TYPE_NAME = case_when(ADM_TYPE_NAME == 'EMERGENT' ~ 'URGENT', TRUE ~ ADM_TYPE_NAME),#Combine EMERGENT and URGENT admission type together
         Last_Visit_Type = case_when((is.na(Last_Visit_Type) | Last_Visit_Type == "Unknown") ~ 'Medical', TRUE ~ Last_Visit_Type ), #Impute missing Last_Visit_Type
         M6_ED_Visits = case_when(is.na(M6_ED_Visits) ~ 0, TRUE ~ M6_ED_Visits ),#Impute missing values for the visit history
         M12_ED_Visits = case_when(is.na(M12_ED_Visits) ~ 0, TRUE ~ M12_ED_Visits ),
         Y2_ED_Visits = case_when(is.na(Y2_ED_Visits) ~ 0, TRUE ~ Y2_ED_Visits ),
         Y1_Sepsis_Visits = case_when(is.na(Y1_Sepsis_Visits) ~ 0, TRUE ~ Y1_Sepsis_Visits ),
         Y2_Sepsis_Visits = case_when(is.na(Y2_Sepsis_Visits) ~ 0, TRUE ~ Y2_Sepsis_Visits ),
         IV_ABX_Day0_COUNT = case_when(IV_ABX_Day0_COUNT >= 7 ~ 7, #If it's >7 then it's considered the same as 7
                                       is.na(IV_ABX_Day0_COUNT) ~ 0,
                                       TRUE ~ IV_ABX_Day0_COUNT),
         LOS_Days = (as.numeric(difftime(DIS_DTM, INP_ADM_DTM, units = "hours")))/24 #LOS since inpatient admission
         )%>%
  filter( IM==1 & !((ADM_TYPE_NAME %in% c("NEWBORN", "INFORMATION NOT AVAILABLE", "TRAUMA CENTER")) | is.na(ADM_TYPE_NAME) |
                      DIS_DTM >= as.POSIXct("2017-10-01 00:00:00")) & LOS_Days <= 100 ) # Add a filter here


SEPSIS_FINAL$RowID <- as.numeric(rownames(SEPSIS_FINAL)) #Create a unique RowID for each row.
### Convert all characters to factors ###
SEPSIS_FINAL[sapply(SEPSIS_FINAL, is.character)] <- lapply(SEPSIS_FINAL[sapply(SEPSIS_FINAL, is.character)], as.factor)
SEPSIS_FINAL$CHF_L	<-	as.factor(	SEPSIS_FINAL$CHF_L	)
SEPSIS_FINAL$VALVE_L	<-	as.factor(	SEPSIS_FINAL$VALVE_L	)
SEPSIS_FINAL$PULMCIRC_L	<-	as.factor(	SEPSIS_FINAL$PULMCIRC_L	)
SEPSIS_FINAL$PERIVASC_L	<-	as.factor(	SEPSIS_FINAL$PERIVASC_L	)
SEPSIS_FINAL$HTN_L	<-	as.factor(	SEPSIS_FINAL$HTN_L	)
SEPSIS_FINAL$HTNCX_L	<-	as.factor(	SEPSIS_FINAL$HTNCX_L	)
SEPSIS_FINAL$PARA_L	<-	as.factor(	SEPSIS_FINAL$PARA_L	)
SEPSIS_FINAL$NEURO_L	<-	as.factor(	SEPSIS_FINAL$NEURO_L	)
SEPSIS_FINAL$CHRNLUNG_L	<-	as.factor(	SEPSIS_FINAL$CHRNLUNG_L	)
SEPSIS_FINAL$DM_L	<-	as.factor(	SEPSIS_FINAL$DM_L	)
SEPSIS_FINAL$DMCX_L	<-	as.factor(	SEPSIS_FINAL$DMCX_L	)
SEPSIS_FINAL$HYPOTHY_L	<-	as.factor(	SEPSIS_FINAL$HYPOTHY_L	)
SEPSIS_FINAL$RENLFAIL_L	<-	as.factor(	SEPSIS_FINAL$RENLFAIL_L	)
SEPSIS_FINAL$LIVER_L	<-	as.factor(	SEPSIS_FINAL$LIVER_L	)
SEPSIS_FINAL$ULCER_L	<-	as.factor(	SEPSIS_FINAL$ULCER_L	)
SEPSIS_FINAL$AIDS_L	<-	as.factor(	SEPSIS_FINAL$AIDS_L	)
SEPSIS_FINAL$LYMPH_L	<-	as.factor(	SEPSIS_FINAL$LYMPH_L	)
SEPSIS_FINAL$METS_L	<-	as.factor(	SEPSIS_FINAL$METS_L	)
SEPSIS_FINAL$TUMOR_L	<-	as.factor(	SEPSIS_FINAL$TUMOR_L	)
SEPSIS_FINAL$ARTH_L	<-	as.factor(	SEPSIS_FINAL$ARTH_L	)
SEPSIS_FINAL$COAG_L	<-	as.factor(	SEPSIS_FINAL$COAG_L	)
SEPSIS_FINAL$OBESE_L	<-	as.factor(	SEPSIS_FINAL$OBESE_L	)
SEPSIS_FINAL$WGHTLOSS_L	<-	as.factor(	SEPSIS_FINAL$WGHTLOSS_L	)
SEPSIS_FINAL$LYTES_L	<-	as.factor(	SEPSIS_FINAL$LYTES_L	)
SEPSIS_FINAL$BLDLOSS_L	<-	as.factor(	SEPSIS_FINAL$BLDLOSS_L	)
SEPSIS_FINAL$ANEMDEF_L	<-	as.factor(	SEPSIS_FINAL$ANEMDEF_L	)
SEPSIS_FINAL$ALCOHOL_L	<-	as.factor(	SEPSIS_FINAL$ALCOHOL_L	)
SEPSIS_FINAL$DRUG_L	<-	as.factor(	SEPSIS_FINAL$DRUG_L	)
SEPSIS_FINAL$PSYCH_L	<-	as.factor(	SEPSIS_FINAL$PSYCH_L	)
SEPSIS_FINAL$DEPRESS_L	<-	as.factor(	SEPSIS_FINAL$DEPRESS_L	)
SEPSIS_FINAL$SUSP	<-	as.factor(	SEPSIS_FINAL$SUSP	)

# any(data.frame(table(SEPSIS_FINAL$HSP_ACCOUNT_ID))$Freq > 1) # Make sure no duplicate records

save(SEPSIS_FINAL,file = "SEPSIS_FINAL.RData")


# to_Shelley <- SEPSIS_FINAL%>%
#   filter(ACTUAL1D==1)%>%
#   select(HSP_ACCOUNT_ID, Type, ADM_DTM, INP_ADM_DTM, DIS_DTM, IV_ABX_DTM, INP_ABX_DTM, ED_ABX_IND, INP_ABX_IND, LOS_Days)
# to_Shelley <- left_join(to_Shelley, pat_enc%>%select(HSP_ACCOUNT_ID, PAT_ENC_CSN_ID), by = "HSP_ACCOUNT_ID")%>%
#   select(HSP_ACCOUNT_ID, PAT_ENC_CSN_ID, Type, ADM_DTM, INP_ADM_DTM, DIS_DTM, INP_ABX_DTM, ED_ABX_IND, INP_ABX_IND, LOS_Days)%>%
#   arrange(desc(LOS_Days), HSP_ACCOUNT_ID, PAT_ENC_CSN_ID)
# write.csv(to_Shelley, file="T:/Sepsis Non-POA/Files/To Shelley/Sepsis_Cases_Chart_Review_09292017.csv")


# CrossTable(to_Shelley$ED_ABX_IND, to_Shelley$INP_ABX_IND, prop.chisq=FALSE, chisq = FALSE, format = 'SAS')

# Check the distribution of continuous variables
# ggplot(SEPSIS_FINAL, aes(AGE)) +  geom_histogram()
# ggplot(SEPSIS_FINAL, aes(TEMPERATURE)) +  geom_histogram()
# ggplot(SEPSIS_FINAL, aes(BP_SYSTOLIC)) +  geom_histogram()
# ggplot(SEPSIS_FINAL, aes(BP_DIASTOLIC)) +  geom_histogram()
# ggplot(SEPSIS_FINAL, aes(PULSE)) +  geom_histogram()
# ggplot(SEPSIS_FINAL, aes(RESPIRATIONS)) +  geom_histogram()



