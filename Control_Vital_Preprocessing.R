library(dplyr)
library(reshape2)
ConVital <- read.csv("Control_Vital_20190601_20200101.csv") %>% select(-X)
ConVital_wide <- ConVital %>% reshape2::dcast(HSP_ACCOUNT_ID + FSD_ID + RECORDED_TIME + CareTeamKey + PAT_ENC_CSN_ID + Role + TeamType + EncounterKey + DateKey ~ Vital, value.var = 'MEAS_VALUE') 
