
### PURPOSE:      The purpose of this program is to pull vital sign data from flowsheets by breaking months
### UPDATED ON:   10/13/2017


library(RODBC)
PW <- read.csv('C:/PW/PW.txt',as.is=TRUE,header = T,sep="\t")
ch <- odbcConnect("CIS-Parkland",uid=PW[1,1],pwd=PW[1,2],believeNRows=F)

#set work library
setwd("T:/Sepsis Non-POA/Data") 


# Get the list of months
dates = seq(as.Date("2015/1/1"), as.Date("2017/10/1"), "month")
# Setting the flowsheet recording start date as 1/1/2015 was to cover all patients who were discharged after 10/01/2015

### IMPORT BLOOD PRESSURE
for (i in seq(1, length(dates)-1)) # all but last
{   
  print(i)
  sqlcode = paste0("
                   SELECT DISTINCT 
                   hsp.HSP_ACCOUNT_ID,  
                   flo.RECORDED_TIME,
                   flo.MEAS_VALUE
                   
                   FROM PG_PIE.DM_CLA_PAT_ENC_HSP  hsp
                   
                   RIGHT JOIN PG_PIE.DM_CLA_IP_FLWSHT_REC  rec
                   ON rec.INPATIENT_DATA_ID = hsp.INPATIENT_DATA_ID
                   
                   LEFT JOIN PG_PIE.DM_CLA_IP_FLWSHT_MEAS flo
                   ON flo.FSD_ID = rec.FSD_ID
                   
                   where flo.FLO_MEAS_ID  = '5'
                   
                   and flo.RECORDED_TIME  <  {d '",dates[i+1], "'} AND
                   flo.RECORDED_TIME  >=  {d '", dates[i] , "'} 
                   ")
  #data1 <-  sqlQuery(ch, sqlcode)
  assign(paste0("Vitals_BP_", as.character(i)), sqlQuery(ch, sqlcode))
  #paste0("Vitals_BP_", as.character(i)) <- data1
}

Vitals_BP <- do.call(rbind, lapply( ls(patt="Vitals_BP_"), get))%>%
  rename(BP_DTM = RECORDED_TIME,
         BP = MEAS_VALUE)

save(Vitals_BP,file = "Vitals_BP.RData")

rm(list=paste0("Vitals_BP_",1:33))




### IMPORT TEMPERATURE
ch <- odbcConnect("CIS-Parkland",uid=PW[1,1],pwd=PW[1,2],believeNRows=F)

for (i in seq(1, length(dates)-1)) # all but last
{   
  print(i)
  sqlcode = paste0("
                   SELECT DISTINCT 
                   hsp.HSP_ACCOUNT_ID,  
                   flo.RECORDED_TIME,
                   flo.MEAS_VALUE
                   
                   FROM PG_PIE.DM_CLA_PAT_ENC_HSP  hsp
                   
                   RIGHT JOIN PG_PIE.DM_CLA_IP_FLWSHT_REC  rec
                   ON rec.INPATIENT_DATA_ID = hsp.INPATIENT_DATA_ID
                   
                   LEFT JOIN PG_PIE.DM_CLA_IP_FLWSHT_MEAS flo
                   ON flo.FSD_ID = rec.FSD_ID
                   
                   where flo.FLO_MEAS_ID  = '6'
                   
                   and flo.RECORDED_TIME  <  {d '",dates[i+1], "'} AND
                   flo.RECORDED_TIME  >=  {d '", dates[i] , "'} 
                   ")
  #data1 <-  sqlQuery(ch, sqlcode)
  assign(paste0("Vitals_TEMP_", as.character(i)), sqlQuery(ch, sqlcode))
  #paste0("Vitals_BP_", as.character(i)) <- data1
}

Vitals_TEMP <- do.call(rbind, lapply( ls(patt="Vitals_TEMP_"), get))%>%
  rename(TEMP_DTM = RECORDED_TIME,
         TEMP = MEAS_VALUE)%>%
  filter(!is.na(HSP_ACCOUNT_ID))%>%
  arrange(HSP_ACCOUNT_ID, TEMP_DTM)

save(Vitals_TEMP,file = "Vitals_TEMP.RData")

rm(list=paste0("Vitals_TEMP_",1:33))








### IMPORT PULSE
ch <- odbcConnect("CIS-Parkland",uid=PW[1,1],pwd=PW[1,2],believeNRows=F)

for (i in seq(1, length(dates)-1)) # all but last
{   
  print(i)
  sqlcode = 
    paste0("
                   SELECT DISTINCT 
           hsp.HSP_ACCOUNT_ID,  
           flo.RECORDED_TIME,
           flo.MEAS_VALUE
           
           FROM PG_PIE.DM_CLA_PAT_ENC_HSP  hsp
           
           RIGHT JOIN PG_PIE.DM_CLA_IP_FLWSHT_REC  rec
           ON rec.INPATIENT_DATA_ID = hsp.INPATIENT_DATA_ID
           
           LEFT JOIN PG_PIE.DM_CLA_IP_FLWSHT_MEAS flo
           ON flo.FSD_ID = rec.FSD_ID
           
           where flo.FLO_MEAS_ID  = '8'
           
           and flo.RECORDED_TIME  <  {d '",dates[i+1], "'} AND
           flo.RECORDED_TIME  >=  {d '", dates[i] , "'} 
           ")
  #data1 <-  sqlQuery(ch, sqlcode)
  assign(paste0("Vitals_PULSE_", as.character(i)), sqlQuery(ch, sqlcode))
  #paste0("Vitals_BP_", as.character(i)) <- data1
}

Vitals_PULSE <- do.call(rbind, lapply( ls(patt="Vitals_PULSE_"), get))%>%
  rename(PULSE_DTM = RECORDED_TIME,
         PULSE = MEAS_VALUE)%>%
  filter(!is.na(HSP_ACCOUNT_ID))%>%
  arrange(HSP_ACCOUNT_ID, PULSE_DTM)

save(Vitals_PULSE,file = "Vitals_PULSE.RData")

rm(list=paste0("Vitals_PULSE_",1:33))







### IMPORT Respiration
ch <- odbcConnect("CIS-Parkland",uid=PW[1,1],pwd=PW[1,2],believeNRows=F)

for (i in seq(1, length(dates)-1)) # all but last
{   
  print(i)
  sqlcode = paste0("
                   SELECT DISTINCT 
                   hsp.HSP_ACCOUNT_ID,  
                   flo.RECORDED_TIME,
                   flo.MEAS_VALUE
                   
                   FROM PG_PIE.DM_CLA_PAT_ENC_HSP  hsp
                   
                   RIGHT JOIN PG_PIE.DM_CLA_IP_FLWSHT_REC  rec
                   ON rec.INPATIENT_DATA_ID = hsp.INPATIENT_DATA_ID
                   
                   LEFT JOIN PG_PIE.DM_CLA_IP_FLWSHT_MEAS flo
                   ON flo.FSD_ID = rec.FSD_ID
                   
                   where flo.FLO_MEAS_ID  = '9'
                   
                   and flo.RECORDED_TIME  <  {d '",dates[i+1], "'} AND
                   flo.RECORDED_TIME  >=  {d '", dates[i] , "'} 
                   ")
  #data1 <-  sqlQuery(ch, sqlcode)
  assign(paste0("Vitals_RESP_", as.character(i)), sqlQuery(ch, sqlcode))
  #paste0("Vitals_BP_", as.character(i)) <- data1
}

Vitals_RESP <- do.call(rbind, lapply( ls(patt="Vitals_RESP_"), get))%>%
  rename(RESP_DTM = RECORDED_TIME,
         RESP = MEAS_VALUE)%>%
  filter(!is.na(HSP_ACCOUNT_ID))%>%
  arrange(HSP_ACCOUNT_ID, RESP_DTM)

save(Vitals_RESP,file = "Vitals_RESP.RData")

rm(list=paste0("Vitals_RESP_",1:33))






### IMPORT O2 Saturation
ch <- odbcConnect("CIS-Parkland",uid=PW[1,1],pwd=PW[1,2],believeNRows=F)

for (i in seq(1, length(dates)-1)) # all but last
{   
  print(i)
  sqlcode = paste0("
                   SELECT DISTINCT 
                   hsp.HSP_ACCOUNT_ID,  
                   flo.RECORDED_TIME,
                   flo.MEAS_VALUE
                   
                   FROM PG_PIE.DM_CLA_PAT_ENC_HSP  hsp
                   
                   RIGHT JOIN PG_PIE.DM_CLA_IP_FLWSHT_REC  rec
                   ON rec.INPATIENT_DATA_ID = hsp.INPATIENT_DATA_ID
                   
                   LEFT JOIN PG_PIE.DM_CLA_IP_FLWSHT_MEAS flo
                   ON flo.FSD_ID = rec.FSD_ID
                   
                   where flo.FLO_MEAS_ID  = '10'
                   
                   and flo.RECORDED_TIME  <  {d '",dates[i+1], "'} AND
                   flo.RECORDED_TIME  >=  {d '", dates[i] , "'} 
                   ")
  #data1 <-  sqlQuery(ch, sqlcode)
  assign(paste0("Vitals_O2SAT_", as.character(i)), sqlQuery(ch, sqlcode))
  #paste0("Vitals_BP_", as.character(i)) <- data1
}

Vitals_O2SAT <- do.call(rbind, lapply( ls(patt="Vitals_O2SAT_"), get))%>%
  rename(O2SAT_DTM = RECORDED_TIME,
         O2SAT = MEAS_VALUE)%>%
  filter(!is.na(HSP_ACCOUNT_ID))%>%
  arrange(HSP_ACCOUNT_ID, O2SAT_DTM)

save(Vitals_O2SAT,file = "Vitals_O2SAT.RData")

rm(list=paste0("Vitals_O2SAT_",1:33))
