

### The purpose of this program is to create patients' history -------------
### Such as the MS-DRG type of the last visit, -----------------------------
### number of ED visits in the past 6 months, 12 months, -------------------
### number of SEPSIS onsets in the past 1 year, 2 years.--------------------


load("pat_enc.RData") 

### Identify whether the last inpatient visit (within 3 years) was Medical or Surgical 
### If not, then use "Unknown" 
MSDRG <- DRG%>%
  filter( substr(DRG_NUMBER, 1, 2) == 'MS' )%>%
  mutate(MSDRG=substr(DRG_NUMBER,3,5))%>%
  select(HSP_ACCOUNT_ID, MSDRG)%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)

# Read in the list of Medical DRGs
MDRG_List <- read.csv(file='T:/Sepsis Non-POA/Files/Medical_DRG.csv',header=T)
MDRG_List <- (MDRG_List%>%mutate(MDRG_List = substr(Medical_DRG,2,4)))$MDRG_List

# Read in the list of Surgical DRGs
SDRG_List <- read.csv(file='T:/Sepsis Non-POA/Files/Surgical_DRG.csv',header=T)
SDRG_List <- (SDRG_List%>%mutate(SDRG_List = substr(Surgical_DRG,2,4)))$SDRG_List

# Select distinct hospital accounts
pat_enc_new <- pat_enc%>%
  select(PAT_ID,HSP_ACCOUNT_ID,HOSP_DISCH_TIME_NEW)%>%
  distinct(PAT_ID,HSP_ACCOUNT_ID,HOSP_DISCH_TIME_NEW)

# Create Last Visit Type (Medical or Surgical)
DRG_ID <- left_join(MSDRG, pat_enc_new, by = "HSP_ACCOUNT_ID")%>%
  # left_join(.,INP_ADM_DTM, by = "HSP_ACCOUNT_ID")%>%
  arrange(PAT_ID, HOSP_DISCH_TIME_NEW)%>%
  select(PAT_ID, HSP_ACCOUNT_ID, MSDRG, HOSP_DISCH_TIME_NEW)
Last_Visit_Type <- DRG_ID%>%
  group_by(PAT_ID)%>%
  mutate(DRG_L = lag(MSDRG),
         DIS_L = lag(HOSP_DISCH_TIME_NEW),
         timediff = as.numeric(difftime(as.Date(HOSP_DISCH_TIME_NEW), as.Date(DIS_L), units = "weeks"))/52.25,
         Last_Visit_Type = case_when(timediff <= 3 & DRG_L %in% MDRG_List ~ 'Medical', #Within the last 3 years
                                     timediff <= 3 & DRG_L %in% SDRG_List ~ 'Surgical', 
                                     TRUE ~ 'Unknown'  ))%>%
  ungroup(PAT_ID)%>%
  select(HSP_ACCOUNT_ID, Last_Visit_Type)

# Save it as a permanent dataset
save(Last_Visit_Type,file = "Last_Visit_Type.RData")
remove(DRG_ID, pat_enc_new )

pat_enc_new <- pat_enc%>%
  mutate(ADM_DTM = HOSP_ADMSN_TIME)%>%
  select(PAT_ID,HSP_ACCOUNT_ID, ADM_DTM)

## Create # of ED visits in the last 6 months
All_ED_Visits <- inner_join(DID_details, pat_enc_new, by="HSP_ACCOUNT_ID")%>%
  filter(DEPARTMENT_ID %in% c(160000, 230733, 250004, 260003, 540905, 560000, 560003, 570000, 570002))%>%
  mutate(ED=1,
         ED_DTM = ADM_DTM)%>% #Set the hospital admission time as the ED time
  distinct(PAT_ID, HSP_ACCOUNT_ID, .keep_all=TRUE)%>%
  arrange(PAT_ID, HSP_ACCOUNT_ID)%>%
  select(PAT_ID, HSP_ACCOUNT_ID, ED_DTM)

All_ED_Visits2 <- right_join(All_ED_Visits, pat_enc_new, by = c("PAT_ID","HSP_ACCOUNT_ID") )%>%
  arrange(HSP_ACCOUNT_ID, ADM_DTM )%>%
  mutate(M6 = ADM_DTM - days(180))%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
dups <- All_ED_Visits2%>%
  mutate(HSP_ACCOUNT_ID1=HSP_ACCOUNT_ID, ED_DTM1=ED_DTM)%>%
  select(PAT_ID, HSP_ACCOUNT_ID1, ED_DTM1)
All_ED_Visits2 <- left_join(All_ED_Visits2, dups, by="PAT_ID")%>%
  arrange(PAT_ID, HSP_ACCOUNT_ID, HSP_ACCOUNT_ID1)%>%
  filter(HSP_ACCOUNT_ID != HSP_ACCOUNT_ID1)%>%
  mutate(ED_Visit = case_when( !is.na(ADM_DTM) & !is.na(M6) & ED_DTM1 < ADM_DTM & ED_DTM1 > M6 ~ 1, TRUE ~ 0  ))%>%
  filter( ED_Visit != 0 )
M6_ED_Visits <- All_ED_Visits2%>%
  group_by(HSP_ACCOUNT_ID)%>%
  mutate(ED_Visit_Total = sum(ED_Visit))%>%
  select(HSP_ACCOUNT_ID, ED_Visit_Total)%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)%>%
  mutate( M6_ED_Visits = case_when(is.na(ED_Visit_Total) ~ 0, TRUE ~ ED_Visit_Total)  )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)%>%
  select(HSP_ACCOUNT_ID, M6_ED_Visits)

remove('All_ED_Visits2','dups')
save(M6_ED_Visits, file = "M6_ED_Visits.RData")

### NOTE:
### I randomly select 5 patients and manually checked their ED visit history
### All the results match the algorithm results





## Create # of ED visits in the last 12 months
All_ED_Visits2 <- right_join(All_ED_Visits, pat_enc_new, by = c("PAT_ID","HSP_ACCOUNT_ID") )%>%
  arrange(HSP_ACCOUNT_ID, ADM_DTM )%>%
  mutate(M12 = ADM_DTM - years(1))%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)%>%
  arrange(PAT_ID, HSP_ACCOUNT_ID, ED_DTM )
dups <- All_ED_Visits2%>%
  mutate(HSP_ACCOUNT_ID1=HSP_ACCOUNT_ID, ED_DTM1=ED_DTM)%>%
  select(PAT_ID, HSP_ACCOUNT_ID1, ED_DTM1)
All_ED_Visits2 <- left_join(All_ED_Visits2, dups, by="PAT_ID")%>%
  arrange(PAT_ID, HSP_ACCOUNT_ID, HSP_ACCOUNT_ID1)%>%
  filter(HSP_ACCOUNT_ID != HSP_ACCOUNT_ID1)%>%
  mutate(ED_Visit = case_when( !is.na(ADM_DTM) & !is.na(M12) & ED_DTM1 < ADM_DTM & ED_DTM1 > M12 ~ 1, TRUE ~ 0  ))%>%
  filter( ED_Visit != 0 )
M12_ED_Visits <- All_ED_Visits2%>%
  group_by(HSP_ACCOUNT_ID)%>%
  mutate(ED_Visit_Total = sum(ED_Visit))%>%
  select(HSP_ACCOUNT_ID, ED_Visit_Total)%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)%>%
  mutate( M12_ED_Visits = case_when(is.na(ED_Visit_Total) ~ 0, TRUE ~ ED_Visit_Total)  )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)%>%
  select(HSP_ACCOUNT_ID, M12_ED_Visits)

remove('All_ED_Visits2','dups')
save(M12_ED_Visits, file = "M12_ED_Visits.RData")

### NOTE:
### I randomly select 5 patients and manually checked their ED visit history
### All the results match the algorithm results



## Create # of ED visits in the last 2 years
All_ED_Visits2 <- right_join(All_ED_Visits, pat_enc_new, by = c("PAT_ID","HSP_ACCOUNT_ID") )%>%
  arrange(HSP_ACCOUNT_ID, ADM_DTM )%>%
  mutate(Y2 = ADM_DTM - years(2))%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)%>%
  arrange(PAT_ID, HSP_ACCOUNT_ID, ED_DTM )
dups <- All_ED_Visits2%>%
  mutate(HSP_ACCOUNT_ID1=HSP_ACCOUNT_ID, ED_DTM1=ED_DTM)%>%
  select(PAT_ID, HSP_ACCOUNT_ID1, ED_DTM1)
All_ED_Visits2 <- left_join(All_ED_Visits2, dups, by="PAT_ID")%>%
  arrange(PAT_ID, HSP_ACCOUNT_ID, HSP_ACCOUNT_ID1)%>%
  filter(HSP_ACCOUNT_ID != HSP_ACCOUNT_ID1)%>%
  mutate(ED_Visit = case_when( !is.na(ADM_DTM) & !is.na(Y2) & ED_DTM1 < ADM_DTM & ED_DTM1 > Y2 ~ 1, TRUE ~ 0  ))%>%
  filter( ED_Visit != 0 )
Y2_ED_Visits <- All_ED_Visits2%>%
  group_by(HSP_ACCOUNT_ID)%>%
  mutate(ED_Visit_Total = sum(ED_Visit))%>%
  select(HSP_ACCOUNT_ID, ED_Visit_Total)%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)%>%
  mutate( Y2_ED_Visits = case_when(is.na(ED_Visit_Total) ~ 0, TRUE ~ ED_Visit_Total)  )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)%>%
  select(HSP_ACCOUNT_ID, Y2_ED_Visits)

remove('All_ED_Visits','All_ED_Visits2','dups')
save(Y2_ED_Visits, file = "Y2_ED_Visits.RData")

### NOTE:
### I randomly select 5 patients and manually checked their ED visit history
### All the results match the algorithm results



## Create 1-year SEPSIS diagnosis information
All_Sepsis_Visits <- SEPSIS%>%
  #filter(SEPSIS == 1)%>%
  mutate(Y1 = DIS_DTM - years(1))%>%
  distinct(PAT_ID, HSP_ACCOUNT_ID, .keep_all=TRUE)%>%
  arrange(PAT_ID, HSP_ACCOUNT_ID)%>%
  select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, Y1, ACTUAL)
dups <- All_Sepsis_Visits%>%
  filter(ACTUAL == 1)%>%
  mutate(HSP_ACCOUNT_ID1=HSP_ACCOUNT_ID, DIS_DTM1=DIS_DTM)%>%
  select(PAT_ID, HSP_ACCOUNT_ID1, DIS_DTM1)
All_Sepsis_Visits2 <- left_join(All_Sepsis_Visits, dups, by="PAT_ID")%>%
  arrange(PAT_ID, HSP_ACCOUNT_ID, HSP_ACCOUNT_ID1)%>%
  filter(HSP_ACCOUNT_ID != HSP_ACCOUNT_ID1)%>%
  mutate(Sepsis_Visit = case_when( !is.na(DIS_DTM) & !is.na(Y1) & DIS_DTM1 < DIS_DTM & DIS_DTM1 > Y1 ~ 1, TRUE ~ 0  ))#%>%
#filter( Sepsis_Visit != 0 )
Y1_Sepsis_Visits <- All_Sepsis_Visits2%>%
  group_by(HSP_ACCOUNT_ID)%>%
  mutate(Sepsis_Visit_Total = sum(Sepsis_Visit))%>%
  select(HSP_ACCOUNT_ID, Sepsis_Visit_Total)%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)%>%
  mutate( Y1_Sepsis_Visits = case_when(is.na(Sepsis_Visit_Total) ~ 0, TRUE ~ Sepsis_Visit_Total)  )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)%>%
  select(HSP_ACCOUNT_ID, Y1_Sepsis_Visits)
remove('All_Sepsis_Visits','All_Sepsis_Visits2','dups')

### NOTE:
### I randomly select 5 patients and manually checked their ED visit history
### All the results match the algorithm results
save(Y1_Sepsis_Visits, file = "Y1_Sepsis_Visits.RData")


## Create 2-year SEPSIS diagnosis information
All_Sepsis_Visits <- SEPSIS%>%
  #filter(SEPSIS == 1)%>%
  mutate(Y2 = DIS_DTM - years(2))%>%
  distinct(PAT_ID, HSP_ACCOUNT_ID, .keep_all=TRUE)%>%
  arrange(PAT_ID, HSP_ACCOUNT_ID)%>%
  select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, Y2, ACTUAL)
dups <- All_Sepsis_Visits%>%
  filter(ACTUAL == 1)%>%
  mutate(HSP_ACCOUNT_ID1=HSP_ACCOUNT_ID, DIS_DTM1=DIS_DTM)%>%
  select(PAT_ID, HSP_ACCOUNT_ID1, DIS_DTM1)
All_Sepsis_Visits2 <- left_join(All_Sepsis_Visits, dups, by="PAT_ID")%>%
  arrange(PAT_ID, HSP_ACCOUNT_ID, HSP_ACCOUNT_ID1)%>%
  filter(HSP_ACCOUNT_ID != HSP_ACCOUNT_ID1)%>%
  mutate(Sepsis_Visit = case_when( !is.na(DIS_DTM) & !is.na(Y2) & DIS_DTM1 < DIS_DTM & DIS_DTM1 > Y2 ~ 1, TRUE ~ 0  ))#%>%
#filter( Sepsis_Visit != 0 )
Y2_Sepsis_Visits <- All_Sepsis_Visits2%>%
  group_by(HSP_ACCOUNT_ID)%>%
  mutate(Sepsis_Visit_Total = sum(Sepsis_Visit))%>%
  select(HSP_ACCOUNT_ID, Sepsis_Visit_Total)%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)%>%
  mutate( Y2_Sepsis_Visits = case_when(is.na(Sepsis_Visit_Total) ~ 0, TRUE ~ Sepsis_Visit_Total)  )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)%>%
  select(HSP_ACCOUNT_ID, Y2_Sepsis_Visits)
remove('All_Sepsis_Visits','All_Sepsis_Visits2','dups')

### NOTE:
### I randomly select 5 patients and manually checked their ED visit history
### All the results match the algorithm results
save(Y2_Sepsis_Visits, file = "Y2_Sepsis_Visits.RData")


## Combine all history data together
HISTORY <- left_join(COMORB_ALL, Last_Visit_Type, by = "HSP_ACCOUNT_ID")%>%
  left_join(., M6_ED_Visits, by = "HSP_ACCOUNT_ID")%>%
  left_join(., M12_ED_Visits, by = "HSP_ACCOUNT_ID")%>%
  left_join(., Y2_ED_Visits, by = "HSP_ACCOUNT_ID")%>%
  left_join(., Y1_Sepsis_Visits, by = "HSP_ACCOUNT_ID")%>%
  left_join(., Y2_Sepsis_Visits, by = "HSP_ACCOUNT_ID")

save(HISTORY, file = "HISTORY.RData")
  
  
  
  
  
  
  
  
  
  









