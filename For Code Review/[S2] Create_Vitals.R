
### PURPOSE:      The purpose of this program is to create two types of vital sign values:
#                   1. Average value before inpatient admission
#                   2. Average value 12 hours before inpatient IV ABX was ordered

### UPDATED ON:   11/28/2017


#Load raw vital sign values that were extracted from flowsheets
load("Vitals_BP.RData") 
load("Vitals_TEMP.RData") 
load("Vitals_PULSE.RData") 
load("Vitals_RESP.RData") 
load("Vitals_O2SAT.RData") 

## Get the modeling population encounters
## INP_ADM_DTM_I10 is from program [200] Data_Aggregation
INP_I10_ID <- INP_ADM_DTM_I10%>%
  select(HSP_ACCOUNT_ID, INP_ADM_DTM, INP_ADM_6, INP_ABX_DTM)



### Get systolic BP ### 
Vitals_BPX <- Vitals_BP%>%
  arrange(HSP_ACCOUNT_ID, BP_DTM)%>%
  distinct(HSP_ACCOUNT_ID, BP_DTM, .keep_all=TRUE)
remove(Vitals_BP)

Vitals_SBP <- separate(data = Vitals_BPX, col = BP, into = c("SBP", "DBP"))%>% #Split SBP and DBP from one value
  rename(SBP_DTM = BP_DTM)%>%
  select(HSP_ACCOUNT_ID, SBP_DTM, SBP)
Vitals_SBP$SBP <- as.numeric(Vitals_SBP$SBP)
Vitals_SBP <- Vitals_SBP%>%
  filter(SBP != 0 & SBP <= 250)%>% #Remove values=0 and outliers
  mutate(SBP_NORMAL = case_when(SBP > 120 ~ "NO", TRUE ~ "YES"))

# SBP BEFORE IP admission (use IP+6 as the IP admission time to allow some cushion time)
Vitals_SBP_ZZZ <- left_join(INP_I10_ID, Vitals_SBP, by="HSP_ACCOUNT_ID")%>%
  filter(SBP_DTM < INP_ADM_6 & !is.na(SBP))%>%
  arrange(HSP_ACCOUNT_ID, SBP_DTM)%>%
  select(HSP_ACCOUNT_ID, SBP)
SBP_Day0_AVG <- as.data.frame(aggregate(Vitals_SBP_ZZZ$SBP, 
                                             by=list(HSP_ACCOUNT_ID=Vitals_SBP_ZZZ$HSP_ACCOUNT_ID), FUN=mean))%>% #Get the mean value
  rename(SBP_Day0_AVG = x)

# SBP BEFORE IV ABX
# EPIC APIs only have vital signs extracted in the past 72 hours,
# Here we use vital signs in the past 12 hours prior to IV ABX
Vitals_SBP_ZZZ <- left_join(INP_I10_ID, Vitals_SBP, by="HSP_ACCOUNT_ID")%>%
  filter(!is.na(SBP) & ( (SBP_DTM < INP_ABX_DTM & SBP_DTM > (INP_ABX_DTM - hours(12))) | is.na(INP_ABX_DTM))    )%>%
  arrange(HSP_ACCOUNT_ID, SBP_DTM)%>%
  select(HSP_ACCOUNT_ID, SBP)
SBP_DayX_AVG <- as.data.frame(aggregate(Vitals_SBP_ZZZ$SBP, 
                                        by=list(HSP_ACCOUNT_ID=Vitals_SBP_ZZZ$HSP_ACCOUNT_ID), FUN=mean))%>% #Get the mean value
  rename(SBP_DayX_AVG = x)



### Get diastolic BP ### 
Vitals_DBP <- separate(data = Vitals_BPX, col = BP, into = c("SBP", "DBP"))%>%
  rename(DBP_DTM = BP_DTM)%>%
  select(HSP_ACCOUNT_ID, DBP_DTM, DBP)
Vitals_DBP$DBP <- as.numeric(Vitals_DBP$DBP)
Vitals_DBP <- Vitals_DBP%>% #Remove values=0
  mutate(DBP_NORMAL = case_when(DBP > 80 ~ "NO", TRUE ~ "YES"))

Vitals_DBP_ZZZ <- left_join(INP_I10_ID, Vitals_DBP, by="HSP_ACCOUNT_ID")%>%
  filter(DBP_DTM < INP_ADM_6 & !is.na(DBP))%>%
  arrange(HSP_ACCOUNT_ID, DBP_DTM)%>%
  select(HSP_ACCOUNT_ID, DBP)
DBP_Day0_AVG <- as.data.frame(aggregate(Vitals_DBP_ZZZ$DBP, 
                                        by=list(HSP_ACCOUNT_ID=Vitals_DBP_ZZZ$HSP_ACCOUNT_ID), FUN=mean))%>%
  rename(DBP_Day0_AVG = x)

Vitals_DBP_ZZZ <- left_join(INP_I10_ID, Vitals_DBP, by="HSP_ACCOUNT_ID")%>%
  filter(!is.na(DBP) & ( (DBP_DTM < INP_ABX_DTM & DBP_DTM > (INP_ABX_DTM - hours(12))) | is.na(INP_ABX_DTM))    )%>%
  arrange(HSP_ACCOUNT_ID, DBP_DTM)%>%
  select(HSP_ACCOUNT_ID, DBP)
DBP_DayX_AVG <- as.data.frame(aggregate(Vitals_DBP_ZZZ$DBP, 
                                        by=list(HSP_ACCOUNT_ID=Vitals_DBP_ZZZ$HSP_ACCOUNT_ID), FUN=mean))%>%
  rename(DBP_DayX_AVG = x)





### Get Temperature ### 
Vitals_TEMPX <- Vitals_TEMP%>%
  arrange(HSP_ACCOUNT_ID, TEMP_DTM)%>%
  distinct(HSP_ACCOUNT_ID, TEMP_DTM, .keep_all=TRUE)%>%
  mutate( TEMP_NORMAL = case_when(TEMP < 96.8 | TEMP > 101   ~ "NO", TRUE ~ "YES"))%>%
  select(HSP_ACCOUNT_ID, TEMP, TEMP_DTM, TEMP_NORMAL)%>%
  filter(TEMP != 0)
remove(Vitals_TEMP)

Vitals_TEMPX_ZZZ <- left_join(INP_I10_ID, Vitals_TEMPX, by="HSP_ACCOUNT_ID")%>%
  filter(TEMP_DTM < INP_ADM_6 & !is.na(TEMP))%>%
  arrange(HSP_ACCOUNT_ID, TEMP_DTM)%>%
  select(HSP_ACCOUNT_ID, TEMP)
TEMP_Day0_AVG <- as.data.frame(aggregate(Vitals_TEMPX_ZZZ$TEMP, 
                                        by=list(HSP_ACCOUNT_ID=Vitals_TEMPX_ZZZ$HSP_ACCOUNT_ID), FUN=mean))%>%
  rename(TEMP_Day0_AVG = x)

Vitals_TEMPX_ZZZ <- left_join(INP_I10_ID, Vitals_TEMPX, by="HSP_ACCOUNT_ID")%>%
  filter(!is.na(TEMP) & ( (TEMP_DTM < INP_ABX_DTM & TEMP_DTM > (INP_ABX_DTM - hours(12))) | is.na(INP_ABX_DTM))    )%>%
  arrange(HSP_ACCOUNT_ID, TEMP_DTM)%>%
  select(HSP_ACCOUNT_ID, TEMP)
TEMP_DayX_AVG <- as.data.frame(aggregate(Vitals_TEMPX_ZZZ$TEMP, 
                                        by=list(HSP_ACCOUNT_ID=Vitals_TEMPX_ZZZ$HSP_ACCOUNT_ID), FUN=mean))%>%
  rename(TEMP_DayX_AVG = x)



### Get Pulse ### 
# freq(Vitals_PULSE$PULSE_NAME)
Vitals_PULSEX <- Vitals_PULSE%>%
  arrange(HSP_ACCOUNT_ID, PULSE_DTM)%>%
  distinct(HSP_ACCOUNT_ID, PULSE_DTM, .keep_all=TRUE)%>%
  mutate( PULSE_NORMAL = case_when(PULSE > 90 ~ "NO", TRUE ~ "YES"))%>%
  select(HSP_ACCOUNT_ID, PULSE, PULSE_DTM, PULSE_NORMAL)%>%
  filter(PULSE != 0)
remove(Vitals_PULSE)

Vitals_PULSEX_ZZZ <- left_join(INP_I10_ID, Vitals_PULSEX, by="HSP_ACCOUNT_ID")%>%
  filter(PULSE_DTM < INP_ADM_6 & !is.na(PULSE))%>%
  arrange(HSP_ACCOUNT_ID, PULSE_DTM)%>%
  select(HSP_ACCOUNT_ID, PULSE)
PULSE_Day0_AVG <- as.data.frame(aggregate(Vitals_PULSEX_ZZZ$PULSE, 
                                        by=list(HSP_ACCOUNT_ID=Vitals_PULSEX_ZZZ$HSP_ACCOUNT_ID), FUN=mean))%>%
  rename(PULSE_Day0_AVG = x)

Vitals_PULSEX_ZZZ <- left_join(INP_I10_ID, Vitals_PULSEX, by="HSP_ACCOUNT_ID")%>%
  filter(!is.na(PULSE) & ( (PULSE_DTM < INP_ABX_DTM & PULSE_DTM > (INP_ABX_DTM - hours(12))) | is.na(INP_ABX_DTM))    )%>%
  arrange(HSP_ACCOUNT_ID, PULSE_DTM)%>%
  select(HSP_ACCOUNT_ID, PULSE)
PULSE_DayX_AVG <- as.data.frame(aggregate(Vitals_PULSEX_ZZZ$PULSE, 
                                        by=list(HSP_ACCOUNT_ID=Vitals_PULSEX_ZZZ$HSP_ACCOUNT_ID), FUN=mean))%>%
  rename(PULSE_DayX_AVG = x)





### Get Respirations ### 
# freq(Vitals_RESP$RESP_NAME)
Vitals_RESPX <- Vitals_RESP%>%
  arrange(HSP_ACCOUNT_ID, RESP_DTM)%>%
  distinct(HSP_ACCOUNT_ID, RESP_DTM, .keep_all=TRUE)%>%
  mutate( RESP_NORMAL = case_when(RESP > 20 ~ "NO", TRUE ~ "YES"))%>%
  select(HSP_ACCOUNT_ID, RESP, RESP_DTM, RESP_NORMAL)%>%
  filter(RESP != 0)
remove(Vitals_RESP)

Vitals_RESP_ZZZ <- left_join(INP_I10_ID, Vitals_RESPX, by="HSP_ACCOUNT_ID")%>%
  filter(RESP_DTM < INP_ADM_6 & !is.na(RESP))%>%
  arrange(HSP_ACCOUNT_ID, RESP_DTM)%>%
  select(HSP_ACCOUNT_ID, RESP)
RESP_Day0_AVG <- as.data.frame(aggregate(Vitals_RESP_ZZZ$RESP, 
                                        by=list(HSP_ACCOUNT_ID=Vitals_RESP_ZZZ$HSP_ACCOUNT_ID), FUN=mean))%>%
  rename(RESP_Day0_AVG = x)

Vitals_RESP_ZZZ <- left_join(INP_I10_ID, Vitals_RESPX, by="HSP_ACCOUNT_ID")%>%
  filter(!is.na(RESP) & ( (RESP_DTM < INP_ABX_DTM & RESP_DTM > (INP_ABX_DTM - hours(12))) | is.na(INP_ABX_DTM))    )%>%
  arrange(HSP_ACCOUNT_ID, RESP_DTM)%>%
  select(HSP_ACCOUNT_ID, RESP)
RESP_DayX_AVG <- as.data.frame(aggregate(Vitals_RESP_ZZZ$RESP, 
                                        by=list(HSP_ACCOUNT_ID=Vitals_RESP_ZZZ$HSP_ACCOUNT_ID), FUN=mean))%>%
  rename(RESP_DayX_AVG = x)






### Get O2SAT ### 
# freq(Vitals_O2SAT$O2SAT_NAME)
# freq(Vitals_O2SAT$O2SAT2)

Vitals_O2SATX <- Vitals_O2SAT%>%
  arrange(HSP_ACCOUNT_ID, O2SAT_DTM)%>%
  distinct(HSP_ACCOUNT_ID, O2SAT_DTM, .keep_all=TRUE)%>%
  mutate( O2SAT_NORMAL = case_when(O2SAT > 100 | O2SAT < 95 ~ "NO", TRUE ~ "YES"))%>%
  select(HSP_ACCOUNT_ID, O2SAT, O2SAT_DTM, O2SAT_NORMAL)%>%
  filter(O2SAT != 0)
remove(Vitals_O2SAT)

Vitals_O2SAT_ZZZ <- left_join(INP_I10_ID, Vitals_O2SATX, by="HSP_ACCOUNT_ID")%>%
  filter(O2SAT_DTM < INP_ADM_6 & !is.na(O2SAT))%>%
  arrange(HSP_ACCOUNT_ID, O2SAT_DTM)%>%
  select(HSP_ACCOUNT_ID, O2SAT)
O2SAT_Day0_AVG <- as.data.frame(aggregate(Vitals_O2SAT_ZZZ$O2SAT, 
                                        by=list(HSP_ACCOUNT_ID=Vitals_O2SAT_ZZZ$HSP_ACCOUNT_ID), FUN=mean))%>%
  rename(O2SAT_Day0_AVG = x)

Vitals_O2SAT_ZZZ <- left_join(INP_I10_ID, Vitals_O2SATX, by="HSP_ACCOUNT_ID")%>%
  filter(!is.na(O2SAT) & ( (O2SAT_DTM < INP_ABX_DTM & O2SAT_DTM > (INP_ABX_DTM - hours(12))) | is.na(INP_ABX_DTM))    )%>%
  arrange(HSP_ACCOUNT_ID, O2SAT_DTM)%>%
  select(HSP_ACCOUNT_ID, O2SAT)
O2SAT_DayX_AVG <- as.data.frame(aggregate(Vitals_O2SAT_ZZZ$O2SAT, 
                                        by=list(HSP_ACCOUNT_ID=Vitals_O2SAT_ZZZ$HSP_ACCOUNT_ID), FUN=mean))%>%
  rename(O2SAT_DayX_AVG = x)


Vitals_ALL <- left_join(INP_ADM_DTM_I10, SBP_Day0_AVG, by = "HSP_ACCOUNT_ID")%>%
  left_join(., DBP_Day0_AVG, by = "HSP_ACCOUNT_ID")%>%
  left_join(., TEMP_Day0_AVG, by = "HSP_ACCOUNT_ID")%>%
  left_join(., PULSE_Day0_AVG, by = "HSP_ACCOUNT_ID")%>%
  left_join(., RESP_Day0_AVG, by = "HSP_ACCOUNT_ID")%>%
  left_join(., O2SAT_Day0_AVG, by = "HSP_ACCOUNT_ID")%>%

  left_join(., SBP_DayX_AVG, by = "HSP_ACCOUNT_ID")%>%
  left_join(., DBP_DayX_AVG, by = "HSP_ACCOUNT_ID")%>%
  left_join(., TEMP_DayX_AVG, by = "HSP_ACCOUNT_ID")%>%
  left_join(., PULSE_DayX_AVG, by = "HSP_ACCOUNT_ID")%>%
  left_join(., RESP_DayX_AVG, by = "HSP_ACCOUNT_ID")%>%
  left_join(., O2SAT_DayX_AVG, by = "HSP_ACCOUNT_ID")%>%
  
  mutate( SBP_Day0_AVG = case_when(is.na(SBP_Day0_AVG) ~ 135, TRUE ~ SBP_Day0_AVG), #Impute missing values with the average
          SBP_NORMAL_AVG_Day0 = case_when(SBP_Day0_AVG > 120 ~ "NO", TRUE ~ "YES"), #Assign the abnormal value
          
          DBP_Day0_AVG = case_when(is.na(DBP_Day0_AVG) ~ 80, TRUE ~ DBP_Day0_AVG),
          DBP_NORMAL_AVG_Day0 = case_when(DBP_Day0_AVG > 80   ~ "NO", TRUE ~ "YES"),
          
          TEMP_Day0_AVG = case_when(is.na(TEMP_Day0_AVG) ~ 98, TRUE ~ TEMP_Day0_AVG),
          TEMP_NORMAL_AVG_Day0 = case_when(TEMP_Day0_AVG < 96.8 | TEMP_Day0_AVG > 101   ~ "NO", TRUE ~ "YES"),
          
          PULSE_Day0_AVG = case_when(is.na(PULSE_Day0_AVG) ~ 85, TRUE ~ PULSE_Day0_AVG),
          PULSE_NORMAL_AVG_Day0 = case_when(PULSE_Day0_AVG > 90   ~ "NO", TRUE ~ "YES"),
          
          RESP_Day0_AVG = case_when(is.na(RESP_Day0_AVG) ~ 18, TRUE ~ RESP_Day0_AVG),
          RESP_NORMAL_AVG_Day0 = case_when(RESP_Day0_AVG > 20   ~ "NO", TRUE ~ "YES"),
          
          O2SAT_Day0_AVG = case_when(is.na(O2SAT_Day0_AVG) ~ 98, TRUE ~ O2SAT_Day0_AVG),
          O2SAT_NORMAL_AVG_Day0 = case_when(O2SAT_Day0_AVG > 100 | O2SAT_Day0_AVG < 95   ~ "NO", TRUE ~ "YES"),
          
          SBP_DayX_AVG = case_when(is.na(SBP_DayX_AVG) ~ 135, TRUE ~ SBP_DayX_AVG),
          SBP_NORMAL_AVG_DayX = case_when(SBP_DayX_AVG > 120 ~ "NO", TRUE ~ "YES"), 
          
          DBP_DayX_AVG = case_when(is.na(DBP_DayX_AVG) ~ 80, TRUE ~ DBP_DayX_AVG),
          DBP_NORMAL_AVG_DayX = case_when(DBP_DayX_AVG > 80   ~ "NO", TRUE ~ "YES"),
          
          TEMP_DayX_AVG = case_when(is.na(TEMP_DayX_AVG) ~ 98, TRUE ~ TEMP_DayX_AVG),
          TEMP_NORMAL_AVG_DayX = case_when(TEMP_DayX_AVG < 96.8 | TEMP_DayX_AVG > 101   ~ "NO", TRUE ~ "YES"),
          
          PULSE_DayX_AVG = case_when(is.na(PULSE_DayX_AVG) ~ 85, TRUE ~ PULSE_DayX_AVG),
          PULSE_NORMAL_AVG_DayX = case_when(PULSE_DayX_AVG > 90   ~ "NO", TRUE ~ "YES"),
          
          RESP_DayX_AVG = case_when(is.na(RESP_DayX_AVG) ~ 18, TRUE ~ RESP_DayX_AVG),
          RESP_NORMAL_AVG_DayX = case_when(RESP_DayX_AVG > 20   ~ "NO", TRUE ~ "YES"),
          
          O2SAT_DayX_AVG = case_when(is.na(O2SAT_DayX_AVG) ~ 98, TRUE ~ O2SAT_DayX_AVG),
          O2SAT_NORMAL_AVG_DayX = case_when(O2SAT_DayX_AVG > 100 | O2SAT_DayX_AVG < 95   ~ "NO", TRUE ~ "YES"))%>%
  select(HSP_ACCOUNT_ID, SBP_Day0_AVG:O2SAT_NORMAL_AVG_DayX)
  

save(Vitals_ALL,file = "Vitals_ALL.RData")


rm(INP_I10_ID)
rm(list=ls(pattern="Vitals_BP"))
rm(list=ls(pattern="Vitals_SBP"))
rm(list=ls(pattern="Vitals_DBP"))
rm(list=ls(pattern="Vitals_TEMP"))
rm(list=ls(pattern="Vitals_PULSE"))
rm(list=ls(pattern="Vitals_RESP"))
rm(list=ls(pattern="Vitals_BMI"))
rm(list=ls(pattern="Vitals_O2SAT"))

rm(list=ls(pattern="SBP_"))
rm(list=ls(pattern="DBP_"))
rm(list=ls(pattern="TEMP_"))
rm(list=ls(pattern="PULSE_"))
rm(list=ls(pattern="RESP_"))
rm(list=ls(pattern="O2SAT_"))


