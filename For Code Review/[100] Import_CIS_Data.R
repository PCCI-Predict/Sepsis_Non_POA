
### PURPOSE:  To import all data via CIS Composite ###
### AUTHOR:   Jet Wang ###
### DATE:     8/11/2017 ###


# Build RODBC Connection
library(RODBC)
PW <- read.csv('C:/PW/PW.txt',as.is=TRUE,header = T,sep="\t")
ch <- odbcConnect("CIS-Parkland",uid=PW[1,1],pwd=PW[1,2],believeNRows=F)

#set work library
setwd("T:/Sepsis Non-POA/Data") 



## Import patient encounter data
sqlcode <- "select * from PIE_SYC_NONPOA_SEPSIS_PAT_ENC;"
pat_enc <- sqlQuery(ch,sqlcode)
save(pat_enc,file = "pat_enc.RData")


## Import diagnosis data
sqlcode <- "select * from PIE_SYC_NONPOA_SEPSIS_DIAG;"
diagnosis <- sqlQuery(ch,sqlcode)
save(diagnosis,file = "diagnosis.RData")


## Import demographics data
sqlcode <- "select * from PIE_SYC_NONPOA_SEPSIS_DEMO;"
DEMO <- sqlQuery(ch,sqlcode)
save(DEMO,file = "DEMO.RData")


## Import antibiotics data
sqlcode <- "select * from PIE_SYC_NONPOA_SEPSIS_ANTIBIO;"
antibio <- sqlQuery(ch,sqlcode, as.is = TRUE)
save(antibio,file = "antibio.RData")


#table(antibio$MED_ROUTE) #INJECTION and INTRAVENOUS only
# antibio1 <- antibio%>%
#   filter(!is.na(MED_ROUTE) )%>%
#   arrange(HSP_ACCOUNT_ID, ORDER_DTM)%>%
#   distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)%>%
#   select(HSP_ACCOUNT_ID, ORDER_DTM, MED_ROUTE) #select the first order date&time

## Read in Inpatient indicator
# sqlcode <- "select * from PIE_SYC_NONPOA_SEPSIS_INPATIENT;"
# inp <- sqlQuery(ch,sqlcode)
# save(inp,file = "inp.RData")

## Import Treatment Team data
sqlcode <- "select * from PIE_SYC_NONPOA_SEPSIS_TREATMENT;"
treatment <- sqlQuery(ch,sqlcode)
save(treatment,file = "treatment.RData")


## Import Department ID Details
sqlcode <- "select * from PIE_SYC_NONPOA_SEPSIS_DID;"
DID_details <- sqlQuery(ch,sqlcode)
save(DID_details,file = "DID_details.RData")


## Read in sepsis activation data
# sqlcode <- "select * from PIE_SYC_NONPOA_SEPSIS_ACTIVATE;"
# active <- sqlQuery(ch,sqlcode) 
# save(active,file = "active.RData")
# active1 <- active%>%
#   arrange(HSP_ACCOUNT_ID, ACTIVATE_DTM)%>%
#   distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)

## Import Inpatient Admission Date & Time data
sqlcode <- "select * from PIE_SYC_NONPOA_SEPSIS_INP_ADM;"
INP_ADM <- sqlQuery(ch,sqlcode)
save(INP_ADM,file = "INP_ADM.RData")


## Import Order Results data
sqlcode <- "select * from PIE_SYC_NONPOA_SEPSIS_ORDER_RESULTS;"
Order_Results <- sqlQuery(ch,sqlcode)
save(Order_Results,file = "Order_Results.RData")


## Import DRG data
sqlcode <- "select * from PIE_SYC_NONPOA_SEPSIS_DRG;"
DRG <- sqlQuery(ch,sqlcode)
save(DRG,file = "DRG.RData")

