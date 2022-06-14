
### PURPOSE:  The purpose of this program is to 
#             1. Read in the raw order result values pulled from Clarity;
#             2. Create one dataset for each order type
#             3. Remove outliers
#             4. Impute the missing values by either the average values, or by the randomly selected values from the statistical distribution.
#             5. Create the latest order results by two cutoff time: Inpatient Admission time (for the Base Model), and the IV ABX Ordering time (for the Inpatient Model)

### AUTHOR:   Jet Wang ###
### DATE:     9/11/2017 ###

## Order_Results is directly from CIS Composite
## Check program [100] Import_CIS_Data for details.
load("Order_Results.RData")

Order_Results$NAME <- as.character(Order_Results$NAME)
Order_Results <- Order_Results%>%
  filter(RESULT_STATUS_C %in% c(3,4)) #Order results must be 3-FINAL or 4-CORRECTED


#########################
#         ALBUMIN       #
#########################

ALBUMIN <- Order_Results[grep("ALBUMIN LEVEL", Order_Results$NAME), ]%>%
  filter(NAME == "ALBUMIN LEVEL")%>%
  arrange(HSP_ACCOUNT_ID, NAME, RESULT_TIME)%>%
  distinct(HSP_ACCOUNT_ID, NAME, RESULT_TIME, .keep_all=TRUE)%>%
  mutate(ALBUMIN = ORD_NUM_VALUE )%>%
  select(HSP_ACCOUNT_ID, RESULT_TIME, ALBUMIN, REFERENCE_LOW, REFERENCE_HIGH)
ALBUMIN <- ALBUMIN%>%filter( !is.na(ALBUMIN) & ALBUMIN > 0  & ALBUMIN <= 8 ) # Remove the outliers

lowr <- mean(ALBUMIN$REFERENCE_LOW, na.rm=TRUE) # Get the LOW reference value
highr <- mean(ALBUMIN$REFERENCE_HIGH, na.rm=TRUE) # Get the HIGH reference value
mean1 <- mean(ALBUMIN$ALBUMIN[ALBUMIN$ALBUMIN>=lowr & ALBUMIN$ALBUMIN<=highr], na.rm=TRUE) # Get the AVERAGE value within the normal range
sd1 <- sd(ALBUMIN$ALBUMIN[ALBUMIN$ALBUMIN>=lowr & ALBUMIN$ALBUMIN<=highr], na.rm=TRUE) # Get the STD value
ALBUMIN_Day0 <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6','INP_ABX_DTM')], ALBUMIN, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ADM_6 )%>% # The result time must be before 6 hours prior to the floor admission
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>% # Select the most recent order
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
ALBUMIN_Day0 <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), ALBUMIN_Day0, by = "HSP_ACCOUNT_ID")%>%
  mutate(ALBUMIN_Day0_Missing = case_when(is.na(ALBUMIN) ~ 'YES', TRUE ~ 'NO'),
         ALBUMIN_Day0_Orig = ALBUMIN, #Original value
         ALBUMIN_Day0_ImpN = ALBUMIN, #Pre-set the imputed values based on AVERAGE VALUES
         ALBUMIN_Day0_ImpA = ALBUMIN) #Pre-set the imputed values based on DATA DISTRIBUTIONS
ALBUMIN_Day0$ALBUMIN_Day0_ImpN[is.na(ALBUMIN_Day0$ALBUMIN_Day0_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(ALBUMIN_Day0$ALBUMIN_Day0_ImpN)), replace=T)
ALBUMIN_Day0$ALBUMIN_Day0_ImpA[is.na(ALBUMIN_Day0$ALBUMIN_Day0_ImpA)] <- mean1
ALBUMIN_Day0 <- ALBUMIN_Day0%>%
  mutate(ALBUMIN_Day0_Orig_NORMAL = case_when( is.na(ALBUMIN_Day0_Orig) ~ 'Unknown',
                                               ALBUMIN_Day0_Orig>=lowr & ALBUMIN_Day0_Orig<=highr ~ "YES", 
                                               TRUE ~ "NO"),
         ALBUMIN_Day0_ImpN_NORMAL = case_when( is.na(ALBUMIN_Day0_ImpN) ~ 'Unknown',
                                               ALBUMIN_Day0_ImpN>=lowr & ALBUMIN_Day0_ImpN<=highr ~ "YES", 
                                               TRUE ~ "NO"),
         ALBUMIN_Day0_ImpA_NORMAL = case_when( is.na(ALBUMIN_Day0_ImpA) ~ 'Unknown',
                                               ALBUMIN_Day0_ImpA>=lowr & ALBUMIN_Day0_ImpA<=highr ~ "YES", 
                                               TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, ALBUMIN_Day0_Orig, ALBUMIN_Day0_Orig_NORMAL, ALBUMIN_Day0_ImpN, ALBUMIN_Day0_ImpA, 
         ALBUMIN_Day0_ImpN_NORMAL, ALBUMIN_Day0_ImpA_NORMAL, ALBUMIN_Day0_Missing)

ALBUMIN_DayX <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ABX_DTM')], ALBUMIN, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ABX_DTM | is.na(INP_ABX_DTM) )%>% # The order result must be BEFORE IV ABX is ordered
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
ALBUMIN_DayX <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), ALBUMIN_DayX, by = "HSP_ACCOUNT_ID")%>%
  mutate(ALBUMIN_DayX_Missing = case_when(is.na(ALBUMIN) ~ 'YES', TRUE ~ 'NO'),
         ALBUMIN_DayX_Orig = ALBUMIN,
         ALBUMIN_DayX_ImpN = ALBUMIN,
         ALBUMIN_DayX_ImpA = ALBUMIN)
ALBUMIN_DayX$ALBUMIN_DayX_ImpN[is.na(ALBUMIN_DayX$ALBUMIN_DayX_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(ALBUMIN_DayX$ALBUMIN_DayX_ImpN)), replace=T)
ALBUMIN_DayX$ALBUMIN_DayX_ImpA[is.na(ALBUMIN_DayX$ALBUMIN_DayX_ImpA)] <- mean1
ALBUMIN_DayX <- ALBUMIN_DayX%>%
  mutate(ALBUMIN_DayX_Orig_NORMAL = case_when( is.na(ALBUMIN_DayX_Orig) ~ 'Unknown',
                                               ALBUMIN_DayX_Orig>=lowr & ALBUMIN_DayX_Orig<=highr ~ "YES", 
                                               TRUE ~ "NO"),
         ALBUMIN_DayX_ImpN_NORMAL = case_when( is.na(ALBUMIN_DayX_ImpN) ~ 'Unknown',
                                               ALBUMIN_DayX_ImpN>=lowr & ALBUMIN_DayX_ImpN<=highr ~ "YES", 
                                               TRUE ~ "NO"),
         ALBUMIN_DayX_ImpA_NORMAL = case_when( is.na(ALBUMIN_DayX_ImpA) ~ 'Unknown',
                                               ALBUMIN_DayX_ImpA>=lowr & ALBUMIN_DayX_ImpA<=highr ~ "YES", 
                                               TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, ALBUMIN_DayX_Orig, ALBUMIN_DayX_Orig_NORMAL, ALBUMIN_DayX_ImpN, ALBUMIN_DayX_ImpA, 
         ALBUMIN_DayX_ImpN_NORMAL, ALBUMIN_DayX_ImpA_NORMAL, ALBUMIN_DayX_Missing)









#########################
#         PTT           #
#########################

PTT <- Order_Results[grep("PARTIAL THROMBOPLASTIN TIME", Order_Results$NAME), ]%>%
  filter(NAME == "PARTIAL THROMBOPLASTIN TIME")%>%
  arrange(HSP_ACCOUNT_ID, NAME, RESULT_TIME)%>%
  distinct(HSP_ACCOUNT_ID, NAME, RESULT_TIME, .keep_all=TRUE)%>%
  mutate(REFERENCE_LOW = 23.5,
         REFERENCE_HIGH = 33.5,
         PTT_NORMAL = case_when( ORD_NUM_VALUE>=REFERENCE_LOW & ORD_NUM_VALUE<=REFERENCE_HIGH ~ "YES", TRUE ~ "NO"),
         PTT = ORD_NUM_VALUE)%>%
  select(HSP_ACCOUNT_ID, RESULT_TIME, PTT, PTT_NORMAL, REFERENCE_LOW, REFERENCE_HIGH)
PTT <- PTT%>%filter( !is.na(PTT) & PTT > 0  & PTT <= 200 ) # Remove the outliers

lowr <- mean(PTT$REFERENCE_LOW, na.rm=TRUE)
highr <- mean(PTT$REFERENCE_HIGH, na.rm=TRUE)
mean1 <- mean(PTT$PTT[PTT$PTT>=lowr & PTT$PTT<=highr], na.rm=TRUE)
sd1 <- sd(PTT$PTT[PTT$PTT>=lowr & PTT$PTT<=highr], na.rm=TRUE)

PTT_Day0 <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6','INP_ABX_DTM')], PTT, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ADM_6 )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
PTT_Day0 <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), PTT_Day0, by = "HSP_ACCOUNT_ID")%>%
  mutate(PTT_Day0_Missing = case_when(is.na(PTT) ~ 'YES', TRUE ~ 'NO'),
         PTT_Day0_Orig = PTT,
         PTT_Day0_ImpN = PTT,
         PTT_Day0_ImpA = PTT)
PTT_Day0$PTT_Day0_ImpN[is.na(PTT_Day0$PTT_Day0_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(PTT_Day0$PTT_Day0_ImpN)), replace=T)
PTT_Day0$PTT_Day0_ImpA[is.na(PTT_Day0$PTT_Day0_ImpA)] <- mean1
PTT_Day0 <- PTT_Day0%>%
  mutate(PTT_Day0_Orig_NORMAL = case_when( is.na(PTT_Day0_Orig) ~ 'Unknown',
                                           PTT_Day0_Orig>=lowr & PTT_Day0_Orig<=highr ~ "YES", 
                                           TRUE ~ "NO"),
         PTT_Day0_ImpN_NORMAL = case_when( is.na(PTT_Day0_ImpN) ~ 'Unknown',
                                           PTT_Day0_ImpN>=lowr & PTT_Day0_ImpN<=highr ~ "YES", 
                                           TRUE ~ "NO"),
         PTT_Day0_ImpA_NORMAL = case_when( is.na(PTT_Day0_ImpA) ~ 'Unknown',
                                           PTT_Day0_ImpA>=lowr & PTT_Day0_ImpA<=highr ~ "YES", 
                                           TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, PTT_Day0_Orig, PTT_Day0_Orig_NORMAL, PTT_Day0_ImpN, PTT_Day0_ImpA, 
         PTT_Day0_ImpN_NORMAL, PTT_Day0_ImpA_NORMAL, PTT_Day0_Missing)

PTT_DayX <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ABX_DTM')], PTT, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ABX_DTM | is.na(INP_ABX_DTM) )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
PTT_DayX <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), PTT_DayX, by = "HSP_ACCOUNT_ID")%>%
  mutate(PTT_DayX_Missing = case_when(is.na(PTT) ~ 'YES', TRUE ~ 'NO'),
         PTT_DayX_Orig = PTT,
         PTT_DayX_ImpN = PTT,
         PTT_DayX_ImpA = PTT)
PTT_DayX$PTT_DayX_ImpN[is.na(PTT_DayX$PTT_DayX_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(PTT_DayX$PTT_DayX_ImpN)), replace=T)
PTT_DayX$PTT_DayX_ImpA[is.na(PTT_DayX$PTT_DayX_ImpA)] <- mean1
PTT_DayX <- PTT_DayX%>%
  mutate(PTT_DayX_Orig_NORMAL = case_when( is.na(PTT_DayX_Orig) ~ 'Unknown',
                                           PTT_DayX_Orig>=lowr & PTT_DayX_Orig<=highr ~ "YES", 
                                           TRUE ~ "NO"),
         PTT_DayX_ImpN_NORMAL = case_when( is.na(PTT_DayX_ImpN) ~ 'Unknown',
                                           PTT_DayX_ImpN>=lowr & PTT_DayX_ImpN<=highr ~ "YES", 
                                           TRUE ~ "NO"),
         PTT_DayX_ImpA_NORMAL = case_when( is.na(PTT_DayX_ImpA) ~ 'Unknown',
                                           PTT_DayX_ImpA>=lowr & PTT_DayX_ImpA<=highr ~ "YES", 
                                           TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, PTT_DayX_Orig, PTT_DayX_Orig_NORMAL, PTT_DayX_ImpN, PTT_DayX_ImpA, 
         PTT_DayX_ImpN_NORMAL, PTT_DayX_ImpA_NORMAL, PTT_DayX_Missing)





#########################
#       BILIRUBIN       #
#########################

BILIRUBIN <- Order_Results[grep("BILIRUBIN, TOTAL", Order_Results$NAME), ]%>%
  filter(REFERENCE_LOW == 0.2 & REFERENCE_HIGH == 1.3)%>%
  arrange(HSP_ACCOUNT_ID, NAME, RESULT_TIME)%>%
  distinct(HSP_ACCOUNT_ID, NAME, RESULT_TIME, .keep_all=TRUE)%>%
  mutate(BILIRUBIN_NORMAL = case_when( ORD_NUM_VALUE>=REFERENCE_LOW & ORD_NUM_VALUE<=REFERENCE_HIGH ~ "YES", TRUE ~ "NO"),
         BILIRUBIN = ORD_NUM_VALUE  )%>%
  select(HSP_ACCOUNT_ID, RESULT_TIME, BILIRUBIN, BILIRUBIN_NORMAL, REFERENCE_LOW, REFERENCE_HIGH)
# check1 <- as.data.frame(freq(BILIRUBIN$BILIRUBIN)) # Check the outliers
BILIRUBIN <- BILIRUBIN%>%filter( !is.na(BILIRUBIN) & BILIRUBIN > 0  & BILIRUBIN <= 65 ) # Remove the outliers

lowr <- mean(BILIRUBIN$REFERENCE_LOW, na.rm=TRUE)
highr <- mean(BILIRUBIN$REFERENCE_HIGH, na.rm=TRUE)
mean1 <- mean(BILIRUBIN$BILIRUBIN[BILIRUBIN$BILIRUBIN>=lowr & BILIRUBIN$BILIRUBIN<=highr], na.rm=TRUE)
sd1 <- sd(BILIRUBIN$BILIRUBIN[BILIRUBIN$BILIRUBIN>=lowr & BILIRUBIN$BILIRUBIN<=highr], na.rm=TRUE)

BILIRUBIN_Day0 <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6','INP_ABX_DTM')], BILIRUBIN, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ADM_6 )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
BILIRUBIN_Day0 <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), BILIRUBIN_Day0, by = "HSP_ACCOUNT_ID")%>%
  mutate(BILIRUBIN_Day0_Missing = case_when(is.na(BILIRUBIN) ~ 'YES', TRUE ~ 'NO'),
         BILIRUBIN_Day0_Orig = BILIRUBIN,
         BILIRUBIN_Day0_ImpN = BILIRUBIN,
         BILIRUBIN_Day0_ImpA = BILIRUBIN)
BILIRUBIN_Day0$BILIRUBIN_Day0_ImpN[is.na(BILIRUBIN_Day0$BILIRUBIN_Day0_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(BILIRUBIN_Day0$BILIRUBIN_Day0_ImpN)), replace=T)
BILIRUBIN_Day0$BILIRUBIN_Day0_ImpA[is.na(BILIRUBIN_Day0$BILIRUBIN_Day0_ImpA)] <- mean1
BILIRUBIN_Day0 <- BILIRUBIN_Day0%>%
  mutate(BILIRUBIN_Day0_Orig_NORMAL = case_when( is.na(BILIRUBIN_Day0_Orig) ~ 'Unknown',
                                                 BILIRUBIN_Day0_Orig>=lowr & BILIRUBIN_Day0_Orig<=highr ~ "YES", 
                                                 TRUE ~ "NO"),
         BILIRUBIN_Day0_ImpN_NORMAL = case_when( is.na(BILIRUBIN_Day0_ImpN) ~ 'Unknown',
                                                 BILIRUBIN_Day0_ImpN>=lowr & BILIRUBIN_Day0_ImpN<=highr ~ "YES", 
                                                 TRUE ~ "NO"),
         BILIRUBIN_Day0_ImpA_NORMAL = case_when( is.na(BILIRUBIN_Day0_ImpA) ~ 'Unknown',
                                                 BILIRUBIN_Day0_ImpA>=lowr & BILIRUBIN_Day0_ImpA<=highr ~ "YES", 
                                                 TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, BILIRUBIN_Day0_Orig, BILIRUBIN_Day0_Orig_NORMAL, BILIRUBIN_Day0_ImpN, BILIRUBIN_Day0_ImpA, 
         BILIRUBIN_Day0_ImpN_NORMAL, BILIRUBIN_Day0_ImpA_NORMAL, BILIRUBIN_Day0_Missing)

BILIRUBIN_DayX <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ABX_DTM')], BILIRUBIN, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ABX_DTM | is.na(INP_ABX_DTM) )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
BILIRUBIN_DayX <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), BILIRUBIN_DayX, by = "HSP_ACCOUNT_ID")%>%
  mutate(BILIRUBIN_DayX_Missing = case_when(is.na(BILIRUBIN) ~ 'YES', TRUE ~ 'NO'),
         BILIRUBIN_DayX_Orig = BILIRUBIN,
         BILIRUBIN_DayX_ImpN = BILIRUBIN,
         BILIRUBIN_DayX_ImpA = BILIRUBIN)
BILIRUBIN_DayX$BILIRUBIN_DayX_ImpN[is.na(BILIRUBIN_DayX$BILIRUBIN_DayX_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(BILIRUBIN_DayX$BILIRUBIN_DayX_ImpN)), replace=T)
BILIRUBIN_DayX$BILIRUBIN_DayX_ImpA[is.na(BILIRUBIN_DayX$BILIRUBIN_DayX_ImpA)] <- mean1
BILIRUBIN_DayX <- BILIRUBIN_DayX%>%
  mutate(BILIRUBIN_DayX_Orig_NORMAL = case_when( is.na(BILIRUBIN_DayX_Orig) ~ 'Unknown',
                                                 BILIRUBIN_DayX_Orig>=lowr & BILIRUBIN_DayX_Orig<=highr ~ "YES", 
                                                 TRUE ~ "NO"),
         BILIRUBIN_DayX_ImpN_NORMAL = case_when( is.na(BILIRUBIN_DayX_ImpN) ~ 'Unknown',
                                                 BILIRUBIN_DayX_ImpN>=lowr & BILIRUBIN_DayX_ImpN<=highr ~ "YES", 
                                                 TRUE ~ "NO"),
         BILIRUBIN_DayX_ImpA_NORMAL = case_when( is.na(BILIRUBIN_DayX_ImpA) ~ 'Unknown',
                                                 BILIRUBIN_DayX_ImpA>=lowr & BILIRUBIN_DayX_ImpA<=highr ~ "YES", 
                                                 TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, BILIRUBIN_DayX_Orig, BILIRUBIN_DayX_Orig_NORMAL, BILIRUBIN_DayX_ImpN, BILIRUBIN_DayX_ImpA, 
         BILIRUBIN_DayX_ImpN_NORMAL, BILIRUBIN_DayX_ImpA_NORMAL, BILIRUBIN_DayX_Missing)





#########################
#       POTASSIUM       #
#########################

POTASSIUM <- Order_Results[grep("POTASSIUM", Order_Results$NAME), ]%>%
  filter(NAME %in% c('POC POTASSIUM', 'POC POTASSIUM ARTERIAL', 'POC POTASSIUM CAPILLARY', 'POTASSIUM LEVEL'))%>%
  arrange(HSP_ACCOUNT_ID, NAME, RESULT_TIME)%>%
  distinct(HSP_ACCOUNT_ID, NAME, RESULT_TIME, .keep_all=TRUE)%>%
  mutate(REFERENCE_LOW = 3.6,
         REFERENCE_HIGH = 5,
         POTASSIUM_NORMAL = case_when( ORD_NUM_VALUE>=REFERENCE_LOW & ORD_NUM_VALUE<=REFERENCE_HIGH ~ "YES", TRUE ~ "NO"),
         POTASSIUM = ORD_NUM_VALUE  )%>%
  select(HSP_ACCOUNT_ID, RESULT_TIME, POTASSIUM, POTASSIUM_NORMAL, REFERENCE_LOW, REFERENCE_HIGH)
# check1 <- as.data.frame(freq(POTASSIUM$POTASSIUM)) # Check the outliers
POTASSIUM <- POTASSIUM%>%filter( !is.na(POTASSIUM) & POTASSIUM >= 0  & POTASSIUM <= 9 ) # Remove the outliers

lowr <- mean(POTASSIUM$REFERENCE_LOW, na.rm=TRUE)
highr <- mean(POTASSIUM$REFERENCE_HIGH, na.rm=TRUE)
mean1 <- mean(POTASSIUM$POTASSIUM[POTASSIUM$POTASSIUM>=lowr & POTASSIUM$POTASSIUM<=highr], na.rm=TRUE)
sd1 <- sd(POTASSIUM$POTASSIUM[POTASSIUM$POTASSIUM>=lowr & POTASSIUM$POTASSIUM<=highr], na.rm=TRUE)

POTASSIUM_Day0 <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6','INP_ABX_DTM')], POTASSIUM, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ADM_6 )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
POTASSIUM_Day0 <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), POTASSIUM_Day0, by = "HSP_ACCOUNT_ID")%>%
  mutate(POTASSIUM_Day0_Missing = case_when(is.na(POTASSIUM) ~ 'YES', TRUE ~ 'NO'),
         POTASSIUM_Day0_Orig = POTASSIUM,
         POTASSIUM_Day0_ImpN = POTASSIUM,
         POTASSIUM_Day0_ImpA = POTASSIUM)
POTASSIUM_Day0$POTASSIUM_Day0_ImpN[is.na(POTASSIUM_Day0$POTASSIUM_Day0_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(POTASSIUM_Day0$POTASSIUM_Day0_ImpN)), replace=T)
POTASSIUM_Day0$POTASSIUM_Day0_ImpA[is.na(POTASSIUM_Day0$POTASSIUM_Day0_ImpA)] <- mean1
POTASSIUM_Day0 <- POTASSIUM_Day0%>%
  mutate(POTASSIUM_Day0_Orig_NORMAL = case_when( is.na(POTASSIUM_Day0_Orig) ~ 'Unknown',
                                                 POTASSIUM_Day0_Orig>=lowr & POTASSIUM_Day0_Orig<=highr ~ "YES", 
                                                 TRUE ~ "NO"),
         POTASSIUM_Day0_ImpN_NORMAL = case_when( is.na(POTASSIUM_Day0_ImpN) ~ 'Unknown',
                                                 POTASSIUM_Day0_ImpN>=lowr & POTASSIUM_Day0_ImpN<=highr ~ "YES", 
                                                 TRUE ~ "NO"),
         POTASSIUM_Day0_ImpA_NORMAL = case_when( is.na(POTASSIUM_Day0_ImpA) ~ 'Unknown',
                                                 POTASSIUM_Day0_ImpA>=lowr & POTASSIUM_Day0_ImpA<=highr ~ "YES", 
                                                 TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, POTASSIUM_Day0_Orig, POTASSIUM_Day0_Orig_NORMAL, POTASSIUM_Day0_ImpN, POTASSIUM_Day0_ImpA, 
         POTASSIUM_Day0_ImpN_NORMAL, POTASSIUM_Day0_ImpA_NORMAL, POTASSIUM_Day0_Missing)

POTASSIUM_DayX <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ABX_DTM')], POTASSIUM, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ABX_DTM | is.na(INP_ABX_DTM) )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
POTASSIUM_DayX <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), POTASSIUM_DayX, by = "HSP_ACCOUNT_ID")%>%
  mutate(POTASSIUM_DayX_Missing = case_when(is.na(POTASSIUM) ~ 'YES', TRUE ~ 'NO'),
         POTASSIUM_DayX_Orig = POTASSIUM,
         POTASSIUM_DayX_ImpN = POTASSIUM,
         POTASSIUM_DayX_ImpA = POTASSIUM)
POTASSIUM_DayX$POTASSIUM_DayX_ImpN[is.na(POTASSIUM_DayX$POTASSIUM_DayX_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(POTASSIUM_DayX$POTASSIUM_DayX_ImpN)), replace=T)
POTASSIUM_DayX$POTASSIUM_DayX_ImpA[is.na(POTASSIUM_DayX$POTASSIUM_DayX_ImpA)] <- mean1
POTASSIUM_DayX <- POTASSIUM_DayX%>%
  mutate(POTASSIUM_DayX_Orig_NORMAL = case_when( is.na(POTASSIUM_DayX_Orig) ~ 'Unknown',
                                                 POTASSIUM_DayX_Orig>=lowr & POTASSIUM_DayX_Orig<=highr ~ "YES", 
                                                 TRUE ~ "NO"),
         POTASSIUM_DayX_ImpN_NORMAL = case_when( is.na(POTASSIUM_DayX_ImpN) ~ 'Unknown',
                                                 POTASSIUM_DayX_ImpN>=lowr & POTASSIUM_DayX_ImpN<=highr ~ "YES", 
                                                 TRUE ~ "NO"),
         POTASSIUM_DayX_ImpA_NORMAL = case_when( is.na(POTASSIUM_DayX_ImpA) ~ 'Unknown',
                                                 POTASSIUM_DayX_ImpA>=lowr & POTASSIUM_DayX_ImpA<=highr ~ "YES", 
                                                 TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, POTASSIUM_DayX_Orig, POTASSIUM_DayX_Orig_NORMAL, POTASSIUM_DayX_ImpN, POTASSIUM_DayX_ImpA, 
         POTASSIUM_DayX_ImpN_NORMAL, POTASSIUM_DayX_ImpA_NORMAL, POTASSIUM_DayX_Missing)





#########################
#          CRP          #
#########################

CRP <- Order_Results[grep("C-REACTIVE PROTEIN", Order_Results$NAME), ]%>%
  filter(NAME == "C-REACTIVE PROTEIN")%>%
  arrange(HSP_ACCOUNT_ID, NAME, RESULT_TIME)%>%
  distinct(HSP_ACCOUNT_ID, NAME, RESULT_TIME, .keep_all=TRUE)%>%
  mutate(CRP_NORMAL = case_when( ORD_NUM_VALUE>=REFERENCE_LOW & ORD_NUM_VALUE<=REFERENCE_HIGH ~ "YES", TRUE ~ "NO"),
         CRP = ORD_NUM_VALUE)%>%
  select(HSP_ACCOUNT_ID, RESULT_TIME, CRP, CRP_NORMAL, REFERENCE_LOW, REFERENCE_HIGH)
# check1 <- as.data.frame(freq(CRP$CRP)) # Check the outliers
CRP <- CRP%>%filter( !is.na(CRP) & CRP > 0  & CRP <= 83 ) # Remove the outliers

lowr <- mean(CRP$REFERENCE_LOW, na.rm=TRUE)
highr <- mean(CRP$REFERENCE_HIGH, na.rm=TRUE)
mean1 <- mean(CRP$CRP[CRP$CRP>=lowr & CRP$CRP<=highr], na.rm=TRUE)
sd1 <- sd(CRP$CRP[CRP$CRP>=lowr & CRP$CRP<=highr], na.rm=TRUE)

CRP_Day0 <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6','INP_ABX_DTM')], CRP, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ADM_6 )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
CRP_Day0 <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), CRP_Day0, by = "HSP_ACCOUNT_ID")%>%
  mutate(CRP_Day0_Missing = case_when(is.na(CRP) ~ 'YES', TRUE ~ 'NO'),
         CRP_Day0_Orig = CRP,
         CRP_Day0_ImpN = CRP,
         CRP_Day0_ImpA = CRP)
CRP_Day0$CRP_Day0_ImpN[is.na(CRP_Day0$CRP_Day0_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(CRP_Day0$CRP_Day0_ImpN)), replace=T)
CRP_Day0$CRP_Day0_ImpA[is.na(CRP_Day0$CRP_Day0_ImpA)] <- mean1
CRP_Day0 <- CRP_Day0%>%
  mutate(CRP_Day0_Orig_NORMAL = case_when( is.na(CRP_Day0_Orig) ~ 'Unknown',
                                           CRP_Day0_Orig>=lowr & CRP_Day0_Orig<=highr ~ "YES", 
                                           TRUE ~ "NO"),
         CRP_Day0_ImpN_NORMAL = case_when( is.na(CRP_Day0_ImpN) ~ 'Unknown',
                                           CRP_Day0_ImpN>=lowr & CRP_Day0_ImpN<=highr ~ "YES", 
                                           TRUE ~ "NO"),
         CRP_Day0_ImpA_NORMAL = case_when( is.na(CRP_Day0_ImpA) ~ 'Unknown',
                                           CRP_Day0_ImpA>=lowr & CRP_Day0_ImpA<=highr ~ "YES", 
                                           TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, CRP_Day0_Orig, CRP_Day0_Orig_NORMAL, CRP_Day0_ImpN, CRP_Day0_ImpA, 
         CRP_Day0_ImpN_NORMAL, CRP_Day0_ImpA_NORMAL, CRP_Day0_Missing)

CRP_DayX <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ABX_DTM')], CRP, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ABX_DTM | is.na(INP_ABX_DTM) )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
CRP_DayX <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), CRP_DayX, by = "HSP_ACCOUNT_ID")%>%
  mutate(CRP_DayX_Missing = case_when(is.na(CRP) ~ 'YES', TRUE ~ 'NO'),
         CRP_DayX_Orig = CRP,
         CRP_DayX_ImpN = CRP,
         CRP_DayX_ImpA = CRP)
CRP_DayX$CRP_DayX_ImpN[is.na(CRP_DayX$CRP_DayX_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(CRP_DayX$CRP_DayX_ImpN)), replace=T)
CRP_DayX$CRP_DayX_ImpA[is.na(CRP_DayX$CRP_DayX_ImpA)] <- mean1
CRP_DayX <- CRP_DayX%>%
  mutate(CRP_DayX_Orig_NORMAL = case_when( is.na(CRP_DayX_Orig) ~ 'Unknown',
                                           CRP_DayX_Orig>=lowr & CRP_DayX_Orig<=highr ~ "YES", 
                                           TRUE ~ "NO"),
         CRP_DayX_ImpN_NORMAL = case_when( is.na(CRP_DayX_ImpN) ~ 'Unknown',
                                           CRP_DayX_ImpN>=lowr & CRP_DayX_ImpN<=highr ~ "YES", 
                                           TRUE ~ "NO"),
         CRP_DayX_ImpA_NORMAL = case_when( is.na(CRP_DayX_ImpA) ~ 'Unknown',
                                           CRP_DayX_ImpA>=lowr & CRP_DayX_ImpA<=highr ~ "YES", 
                                           TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, CRP_DayX_Orig, CRP_DayX_Orig_NORMAL, CRP_DayX_ImpN, CRP_DayX_ImpA, 
         CRP_DayX_ImpN_NORMAL, CRP_DayX_ImpA_NORMAL, CRP_DayX_Missing)





#########################
#      CREATININE       #
#########################

CREATININE <- Order_Results[grep("CREATININE LEVEL", Order_Results$NAME), ]%>%
  filter( (REFERENCE_LOW==0.51&REFERENCE_HIGH==0.95) | (REFERENCE_LOW==0.67&REFERENCE_HIGH==1.17) )%>%
  arrange(HSP_ACCOUNT_ID, NAME, RESULT_TIME)%>%
  distinct(HSP_ACCOUNT_ID, NAME, RESULT_TIME, .keep_all=TRUE)%>%
  mutate(CREATININE_NORMAL = case_when( ORD_NUM_VALUE>=REFERENCE_LOW & ORD_NUM_VALUE<=REFERENCE_HIGH ~ "YES", TRUE ~ "NO"),
         CREATININE = ORD_NUM_VALUE)%>%
  select(HSP_ACCOUNT_ID, RESULT_TIME, CREATININE, CREATININE_NORMAL, REFERENCE_LOW, REFERENCE_HIGH)
#check1 <- as.data.frame(freq(CREATININE$CREATININE)) # Check the outliers
CREATININE <- CREATININE%>%filter( !is.na(CREATININE) & CREATININE > 0  & CREATININE <= 50 ) # Remove the outliers

lowr <- mean(CREATININE$REFERENCE_LOW, na.rm=TRUE)
highr <- mean(CREATININE$REFERENCE_HIGH, na.rm=TRUE)
mean1 <- mean(CREATININE$CREATININE[CREATININE$CREATININE>=lowr & CREATININE$CREATININE<=highr], na.rm=TRUE)
sd1 <- sd(CREATININE$CREATININE[CREATININE$CREATININE>=lowr & CREATININE$CREATININE<=highr], na.rm=TRUE)

CREATININE_Day0 <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6','INP_ABX_DTM')], CREATININE, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ADM_6 )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
CREATININE_Day0 <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), CREATININE_Day0, by = "HSP_ACCOUNT_ID")%>%
  mutate(CREATININE_Day0_Missing = case_when(is.na(CREATININE) ~ 'YES', TRUE ~ 'NO'),
         CREATININE_Day0_Orig = CREATININE,
         CREATININE_Day0_ImpN = CREATININE,
         CREATININE_Day0_ImpA = CREATININE)
CREATININE_Day0$CREATININE_Day0_ImpN[is.na(CREATININE_Day0$CREATININE_Day0_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(CREATININE_Day0$CREATININE_Day0_ImpN)), replace=T)
CREATININE_Day0$CREATININE_Day0_ImpA[is.na(CREATININE_Day0$CREATININE_Day0_ImpA)] <- mean1
CREATININE_Day0 <- CREATININE_Day0%>%
  mutate(CREATININE_Day0_Orig_NORMAL = case_when( is.na(CREATININE_Day0_Orig) ~ 'Unknown',
                                                  CREATININE_Day0_Orig>=lowr & CREATININE_Day0_Orig<=highr ~ "YES", 
                                                  TRUE ~ "NO"),
         CREATININE_Day0_ImpN_NORMAL = case_when( is.na(CREATININE_Day0_ImpN) ~ 'Unknown',
                                                  CREATININE_Day0_ImpN>=lowr & CREATININE_Day0_ImpN<=highr ~ "YES", 
                                                  TRUE ~ "NO"),
         CREATININE_Day0_ImpA_NORMAL = case_when( is.na(CREATININE_Day0_ImpA) ~ 'Unknown',
                                                  CREATININE_Day0_ImpA>=lowr & CREATININE_Day0_ImpA<=highr ~ "YES", 
                                                  TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, CREATININE_Day0_Orig, CREATININE_Day0_Orig_NORMAL, CREATININE_Day0_ImpN, CREATININE_Day0_ImpA, 
         CREATININE_Day0_ImpN_NORMAL, CREATININE_Day0_ImpA_NORMAL, CREATININE_Day0_Missing)

CREATININE_DayX <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ABX_DTM')], CREATININE, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ABX_DTM | is.na(INP_ABX_DTM) )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
CREATININE_DayX <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), CREATININE_DayX, by = "HSP_ACCOUNT_ID")%>%
  mutate(CREATININE_DayX_Missing = case_when(is.na(CREATININE) ~ 'YES', TRUE ~ 'NO'),
         CREATININE_DayX_Orig = CREATININE,
         CREATININE_DayX_ImpN = CREATININE,
         CREATININE_DayX_ImpA = CREATININE)
CREATININE_DayX$CREATININE_DayX_ImpN[is.na(CREATININE_DayX$CREATININE_DayX_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(CREATININE_DayX$CREATININE_DayX_ImpN)), replace=T)
CREATININE_DayX$CREATININE_DayX_ImpA[is.na(CREATININE_DayX$CREATININE_DayX_ImpA)] <- mean1
CREATININE_DayX <- CREATININE_DayX%>%
  mutate(CREATININE_DayX_Orig_NORMAL = case_when( is.na(CREATININE_DayX_Orig) ~ 'Unknown',
                                                  CREATININE_DayX_Orig>=lowr & CREATININE_DayX_Orig<=highr ~ "YES", 
                                                  TRUE ~ "NO"),
         CREATININE_DayX_ImpN_NORMAL = case_when( is.na(CREATININE_DayX_ImpN) ~ 'Unknown',
                                                  CREATININE_DayX_ImpN>=lowr & CREATININE_DayX_ImpN<=highr ~ "YES", 
                                                  TRUE ~ "NO"),
         CREATININE_DayX_ImpA_NORMAL = case_when( is.na(CREATININE_DayX_ImpA) ~ 'Unknown',
                                                  CREATININE_DayX_ImpA>=lowr & CREATININE_DayX_ImpA<=highr ~ "YES", 
                                                  TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, CREATININE_DayX_Orig, CREATININE_DayX_Orig_NORMAL, CREATININE_DayX_ImpN, CREATININE_DayX_ImpA, 
         CREATININE_DayX_ImpN_NORMAL, CREATININE_DayX_ImpA_NORMAL, CREATININE_DayX_Missing)






#########################
#      HEMOGLOBIN       #
#########################

HEMOGLOBIN <- Order_Results[grep("HEMOGLOBIN", Order_Results$NAME), ]%>%
  filter(NAME == "HEMOGLOBIN" &
           !((REFERENCE_LOW==10 & REFERENCE_HIGH==18) | 
               (REFERENCE_LOW==13.5 & REFERENCE_HIGH==21.5)  | 
               (REFERENCE_LOW==14.5 & REFERENCE_HIGH==22.5) |
               ( is.na(REFERENCE_LOW) & is.na(REFERENCE_HIGH) ) ) )%>% # Remove some wrong ranges
  arrange(HSP_ACCOUNT_ID, NAME, RESULT_TIME)%>%
  distinct(HSP_ACCOUNT_ID, NAME, RESULT_TIME, .keep_all=TRUE)%>%
  mutate(HEMOGLOBIN_NORMAL = case_when( ORD_NUM_VALUE>=REFERENCE_LOW & ORD_NUM_VALUE<=REFERENCE_HIGH ~ "YES", TRUE ~ "NO"),
         HEMOGLOBIN = ORD_NUM_VALUE)%>%
  select(HSP_ACCOUNT_ID, RESULT_TIME, HEMOGLOBIN, HEMOGLOBIN_NORMAL, REFERENCE_LOW, REFERENCE_HIGH)
# check1 <- as.data.frame(freq(HEMOGLOBIN$HEMOGLOBIN)) # Check the outliers
HEMOGLOBIN <- HEMOGLOBIN%>%filter( !is.na(HEMOGLOBIN) & HEMOGLOBIN >= 1.5  & HEMOGLOBIN <= 22 ) # Remove the outliers

lowr <- mean(HEMOGLOBIN$REFERENCE_LOW, na.rm=TRUE)
highr <- mean(HEMOGLOBIN$REFERENCE_HIGH, na.rm=TRUE)
mean1 <- mean(HEMOGLOBIN$HEMOGLOBIN[HEMOGLOBIN$HEMOGLOBIN>=lowr & HEMOGLOBIN$HEMOGLOBIN<=highr], na.rm=TRUE)
sd1 <- sd(HEMOGLOBIN$HEMOGLOBIN[HEMOGLOBIN$HEMOGLOBIN>=lowr & HEMOGLOBIN$HEMOGLOBIN<=highr], na.rm=TRUE)

HEMOGLOBIN_Day0 <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6','INP_ABX_DTM')], HEMOGLOBIN, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ADM_6 )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
HEMOGLOBIN_Day0 <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), HEMOGLOBIN_Day0, by = "HSP_ACCOUNT_ID")%>%
  mutate(HEMOGLOBIN_Day0_Missing = case_when(is.na(HEMOGLOBIN) ~ 'YES', TRUE ~ 'NO'),
         HEMOGLOBIN_Day0_Orig = HEMOGLOBIN,
         HEMOGLOBIN_Day0_ImpN = HEMOGLOBIN,
         HEMOGLOBIN_Day0_ImpA = HEMOGLOBIN)
HEMOGLOBIN_Day0$HEMOGLOBIN_Day0_ImpN[is.na(HEMOGLOBIN_Day0$HEMOGLOBIN_Day0_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(HEMOGLOBIN_Day0$HEMOGLOBIN_Day0_ImpN)), replace=T)
HEMOGLOBIN_Day0$HEMOGLOBIN_Day0_ImpA[is.na(HEMOGLOBIN_Day0$HEMOGLOBIN_Day0_ImpA)] <- mean1
HEMOGLOBIN_Day0 <- HEMOGLOBIN_Day0%>%
  mutate(HEMOGLOBIN_Day0_Orig_NORMAL = case_when( is.na(HEMOGLOBIN_Day0_Orig) ~ 'Unknown',
                                                  HEMOGLOBIN_Day0_Orig>=lowr & HEMOGLOBIN_Day0_Orig<=highr ~ "YES", 
                                                  TRUE ~ "NO"),
         HEMOGLOBIN_Day0_ImpN_NORMAL = case_when( is.na(HEMOGLOBIN_Day0_ImpN) ~ 'Unknown',
                                                  HEMOGLOBIN_Day0_ImpN>=lowr & HEMOGLOBIN_Day0_ImpN<=highr ~ "YES", 
                                                  TRUE ~ "NO"),
         HEMOGLOBIN_Day0_ImpA_NORMAL = case_when( is.na(HEMOGLOBIN_Day0_ImpA) ~ 'Unknown',
                                                  HEMOGLOBIN_Day0_ImpA>=lowr & HEMOGLOBIN_Day0_ImpA<=highr ~ "YES", 
                                                  TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, HEMOGLOBIN_Day0_Orig, HEMOGLOBIN_Day0_Orig_NORMAL, HEMOGLOBIN_Day0_ImpN, HEMOGLOBIN_Day0_ImpA, 
         HEMOGLOBIN_Day0_ImpN_NORMAL, HEMOGLOBIN_Day0_ImpA_NORMAL, HEMOGLOBIN_Day0_Missing)

HEMOGLOBIN_DayX <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ABX_DTM')], HEMOGLOBIN, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ABX_DTM | is.na(INP_ABX_DTM) )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
HEMOGLOBIN_DayX <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), HEMOGLOBIN_DayX, by = "HSP_ACCOUNT_ID")%>%
  mutate(HEMOGLOBIN_DayX_Missing = case_when(is.na(HEMOGLOBIN) ~ 'YES', TRUE ~ 'NO'),
         HEMOGLOBIN_DayX_Orig = HEMOGLOBIN,
         HEMOGLOBIN_DayX_ImpN = HEMOGLOBIN,
         HEMOGLOBIN_DayX_ImpA = HEMOGLOBIN)
HEMOGLOBIN_DayX$HEMOGLOBIN_DayX_ImpN[is.na(HEMOGLOBIN_DayX$HEMOGLOBIN_DayX_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(HEMOGLOBIN_DayX$HEMOGLOBIN_DayX_ImpN)), replace=T)
HEMOGLOBIN_DayX$HEMOGLOBIN_DayX_ImpA[is.na(HEMOGLOBIN_DayX$HEMOGLOBIN_DayX_ImpA)] <- mean1
HEMOGLOBIN_DayX <- HEMOGLOBIN_DayX%>%
  mutate(HEMOGLOBIN_DayX_Orig_NORMAL = case_when( is.na(HEMOGLOBIN_DayX_Orig) ~ 'Unknown',
                                                  HEMOGLOBIN_DayX_Orig>=lowr & HEMOGLOBIN_DayX_Orig<=highr ~ "YES", 
                                                  TRUE ~ "NO"),
         HEMOGLOBIN_DayX_ImpN_NORMAL = case_when( is.na(HEMOGLOBIN_DayX_ImpN) ~ 'Unknown',
                                                  HEMOGLOBIN_DayX_ImpN>=lowr & HEMOGLOBIN_DayX_ImpN<=highr ~ "YES", 
                                                  TRUE ~ "NO"),
         HEMOGLOBIN_DayX_ImpA_NORMAL = case_when( is.na(HEMOGLOBIN_DayX_ImpA) ~ 'Unknown',
                                                  HEMOGLOBIN_DayX_ImpA>=lowr & HEMOGLOBIN_DayX_ImpA<=highr ~ "YES", 
                                                  TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, HEMOGLOBIN_DayX_Orig, HEMOGLOBIN_DayX_Orig_NORMAL, HEMOGLOBIN_DayX_ImpN, HEMOGLOBIN_DayX_ImpA, 
         HEMOGLOBIN_DayX_ImpN_NORMAL, HEMOGLOBIN_DayX_ImpA_NORMAL, HEMOGLOBIN_DayX_Missing)





#########################
#          MCH          #
#########################

MCH <- Order_Results[grep("HEMOGLOBIN", Order_Results$NAME), ]%>%
  filter(NAME == "MEAN CELL HEMOGLOBIN"&
           !((REFERENCE_LOW==28 & REFERENCE_HIGH==40) | 
               (REFERENCE_LOW==31 & REFERENCE_HIGH==37)  | 
               ( is.na(REFERENCE_LOW) & is.na(REFERENCE_HIGH) ) ) )%>%
  arrange(HSP_ACCOUNT_ID, NAME, RESULT_TIME)%>%
  distinct(HSP_ACCOUNT_ID, NAME, RESULT_TIME, .keep_all=TRUE)%>%
  mutate(MCH_NORMAL = case_when( ORD_NUM_VALUE>=REFERENCE_LOW & ORD_NUM_VALUE<=REFERENCE_HIGH ~ "YES", TRUE ~ "NO"),
         MCH = ORD_NUM_VALUE)%>%
  select(HSP_ACCOUNT_ID, RESULT_TIME, MCH, MCH_NORMAL, REFERENCE_LOW, REFERENCE_HIGH)
# check1 <- as.data.frame(freq(MCH$MCH)) # Check the outliers
MCH <- MCH%>%filter( !is.na(MCH) & MCH >= 13  & MCH <= 50 ) # Remove the outliers

lowr <- mean(MCH$REFERENCE_LOW, na.rm=TRUE)
highr <- mean(MCH$REFERENCE_HIGH, na.rm=TRUE)
mean1 <- mean(MCH$MCH[MCH$MCH>=lowr & MCH$MCH<=highr], na.rm=TRUE)
sd1 <- sd(MCH$MCH[MCH$MCH>=lowr & MCH$MCH<=highr], na.rm=TRUE)

MCH_Day0 <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6','INP_ABX_DTM')], MCH, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ADM_6 )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
MCH_Day0 <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), MCH_Day0, by = "HSP_ACCOUNT_ID")%>%
  mutate(MCH_Day0_Missing = case_when(is.na(MCH) ~ 'YES', TRUE ~ 'NO'),
         MCH_Day0_Orig = MCH,
         MCH_Day0_ImpN = MCH,
         MCH_Day0_ImpA = MCH)
MCH_Day0$MCH_Day0_ImpN[is.na(MCH_Day0$MCH_Day0_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(MCH_Day0$MCH_Day0_ImpN)), replace=T)
MCH_Day0$MCH_Day0_ImpA[is.na(MCH_Day0$MCH_Day0_ImpA)] <- mean1
MCH_Day0 <- MCH_Day0%>%
  mutate(MCH_Day0_Orig_NORMAL = case_when( is.na(MCH_Day0_Orig) ~ 'Unknown',
                                           MCH_Day0_Orig>=lowr & MCH_Day0_Orig<=highr ~ "YES", 
                                           TRUE ~ "NO"),
         MCH_Day0_ImpN_NORMAL = case_when( is.na(MCH_Day0_ImpN) ~ 'Unknown',
                                           MCH_Day0_ImpN>=lowr & MCH_Day0_ImpN<=highr ~ "YES", 
                                           TRUE ~ "NO"),
         MCH_Day0_ImpA_NORMAL = case_when( is.na(MCH_Day0_ImpA) ~ 'Unknown',
                                           MCH_Day0_ImpA>=lowr & MCH_Day0_ImpA<=highr ~ "YES", 
                                           TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, MCH_Day0_Orig, MCH_Day0_Orig_NORMAL, MCH_Day0_ImpN, MCH_Day0_ImpA, 
         MCH_Day0_ImpN_NORMAL, MCH_Day0_ImpA_NORMAL, MCH_Day0_Missing)

MCH_DayX <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ABX_DTM')], MCH, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ABX_DTM | is.na(INP_ABX_DTM) )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
MCH_DayX <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), MCH_DayX, by = "HSP_ACCOUNT_ID")%>%
  mutate(MCH_DayX_Missing = case_when(is.na(MCH) ~ 'YES', TRUE ~ 'NO'),
         MCH_DayX_Orig = MCH,
         MCH_DayX_ImpN = MCH,
         MCH_DayX_ImpA = MCH)
MCH_DayX$MCH_DayX_ImpN[is.na(MCH_DayX$MCH_DayX_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(MCH_DayX$MCH_DayX_ImpN)), replace=T)
MCH_DayX$MCH_DayX_ImpA[is.na(MCH_DayX$MCH_DayX_ImpA)] <- mean1
MCH_DayX <- MCH_DayX%>%
  mutate(MCH_DayX_Orig_NORMAL = case_when( is.na(MCH_DayX_Orig) ~ 'Unknown',
                                           MCH_DayX_Orig>=lowr & MCH_DayX_Orig<=highr ~ "YES", 
                                           TRUE ~ "NO"),
         MCH_DayX_ImpN_NORMAL = case_when( is.na(MCH_DayX_ImpN) ~ 'Unknown',
                                           MCH_DayX_ImpN>=lowr & MCH_DayX_ImpN<=highr ~ "YES", 
                                           TRUE ~ "NO"),
         MCH_DayX_ImpA_NORMAL = case_when( is.na(MCH_DayX_ImpA) ~ 'Unknown',
                                           MCH_DayX_ImpA>=lowr & MCH_DayX_ImpA<=highr ~ "YES", 
                                           TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, MCH_DayX_Orig, MCH_DayX_Orig_NORMAL, MCH_DayX_ImpN, MCH_DayX_ImpA, 
         MCH_DayX_ImpN_NORMAL, MCH_DayX_ImpA_NORMAL, MCH_DayX_Missing)





#########################
#         MCHC          #
#########################

MCHC <- Order_Results[grep("HEMOGLOBIN", Order_Results$NAME), ]%>%
  filter(NAME == "MEAN CELL HEMOGLOBIN CONCENTRATION")%>%
  arrange(HSP_ACCOUNT_ID, NAME, RESULT_TIME)%>%
  distinct(HSP_ACCOUNT_ID, NAME, RESULT_TIME, .keep_all=TRUE)%>%
  mutate(MCHC_NORMAL = case_when( ORD_NUM_VALUE>=REFERENCE_LOW & ORD_NUM_VALUE<=REFERENCE_HIGH ~ "YES", TRUE ~ "NO"),
         MCHC = ORD_NUM_VALUE)%>%
  select(HSP_ACCOUNT_ID, RESULT_TIME, MCHC, MCHC_NORMAL, REFERENCE_LOW, REFERENCE_HIGH)
# check1 <- as.data.frame(freq(MCHC$MCHC)) # Check the outliers
MCHC <- MCHC%>%filter( !is.na(MCHC) & MCHC >= 20  & MCHC <= 50 ) # Remove the outliers

lowr <- mean(MCHC$REFERENCE_LOW, na.rm=TRUE)
highr <- mean(MCHC$REFERENCE_HIGH, na.rm=TRUE)
mean1 <- mean(MCHC$MCHC[MCHC$MCHC>=lowr & MCHC$MCHC<=highr], na.rm=TRUE)
sd1 <- sd(MCHC$MCHC[MCHC$MCHC>=lowr & MCHC$MCHC<=highr], na.rm=TRUE)

MCHC_Day0 <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6','INP_ABX_DTM')], MCHC, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ADM_6 )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
MCHC_Day0 <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), MCHC_Day0, by = "HSP_ACCOUNT_ID")%>%
  mutate(MCHC_Day0_Missing = case_when(is.na(MCHC) ~ 'YES', TRUE ~ 'NO'),
         MCHC_Day0_Orig = MCHC,
         MCHC_Day0_ImpN = MCHC,
         MCHC_Day0_ImpA = MCHC)
MCHC_Day0$MCHC_Day0_ImpN[is.na(MCHC_Day0$MCHC_Day0_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(MCHC_Day0$MCHC_Day0_ImpN)), replace=T)
MCHC_Day0$MCHC_Day0_ImpA[is.na(MCHC_Day0$MCHC_Day0_ImpA)] <- mean1
MCHC_Day0 <- MCHC_Day0%>%
  mutate(MCHC_Day0_Orig_NORMAL = case_when( is.na(MCHC_Day0_Orig) ~ 'Unknown',
                                            MCHC_Day0_Orig>=lowr & MCHC_Day0_Orig<=highr ~ "YES", 
                                            TRUE ~ "NO"),
         MCHC_Day0_ImpN_NORMAL = case_when( is.na(MCHC_Day0_ImpN) ~ 'Unknown',
                                            MCHC_Day0_ImpN>=lowr & MCHC_Day0_ImpN<=highr ~ "YES", 
                                            TRUE ~ "NO"),
         MCHC_Day0_ImpA_NORMAL = case_when( is.na(MCHC_Day0_ImpA) ~ 'Unknown',
                                            MCHC_Day0_ImpA>=lowr & MCHC_Day0_ImpA<=highr ~ "YES", 
                                            TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, MCHC_Day0_Orig, MCHC_Day0_Orig_NORMAL, MCHC_Day0_ImpN, MCHC_Day0_ImpA, 
         MCHC_Day0_ImpN_NORMAL, MCHC_Day0_ImpA_NORMAL, MCHC_Day0_Missing)

MCHC_DayX <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ABX_DTM')], MCHC, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ABX_DTM | is.na(INP_ABX_DTM) )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
MCHC_DayX <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), MCHC_DayX, by = "HSP_ACCOUNT_ID")%>%
  mutate(MCHC_DayX_Missing = case_when(is.na(MCHC) ~ 'YES', TRUE ~ 'NO'),
         MCHC_DayX_Orig = MCHC,
         MCHC_DayX_ImpN = MCHC,
         MCHC_DayX_ImpA = MCHC)
MCHC_DayX$MCHC_DayX_ImpN[is.na(MCHC_DayX$MCHC_DayX_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(MCHC_DayX$MCHC_DayX_ImpN)), replace=T)
MCHC_DayX$MCHC_DayX_ImpA[is.na(MCHC_DayX$MCHC_DayX_ImpA)] <- mean1
MCHC_DayX <- MCHC_DayX%>%
  mutate(MCHC_DayX_Orig_NORMAL = case_when( is.na(MCHC_DayX_Orig) ~ 'Unknown',
                                            MCHC_DayX_Orig>=lowr & MCHC_DayX_Orig<=highr ~ "YES", 
                                            TRUE ~ "NO"),
         MCHC_DayX_ImpN_NORMAL = case_when( is.na(MCHC_DayX_ImpN) ~ 'Unknown',
                                            MCHC_DayX_ImpN>=lowr & MCHC_DayX_ImpN<=highr ~ "YES", 
                                            TRUE ~ "NO"),
         MCHC_DayX_ImpA_NORMAL = case_when( is.na(MCHC_DayX_ImpA) ~ 'Unknown',
                                            MCHC_DayX_ImpA>=lowr & MCHC_DayX_ImpA<=highr ~ "YES", 
                                            TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, MCHC_DayX_Orig, MCHC_DayX_Orig_NORMAL, MCHC_DayX_ImpN, MCHC_DayX_ImpA, 
         MCHC_DayX_ImpN_NORMAL, MCHC_DayX_ImpA_NORMAL, MCHC_DayX_Missing)




#########################
#          INR          #
#########################

INR <- Order_Results[grep("INTERNATIONAL NORMALIZATION RATIO", Order_Results$NAME), ]%>%
  filter(NAME == 'INTERNATIONAL NORMALIZATION RATIO')%>%
  arrange(HSP_ACCOUNT_ID, NAME, RESULT_TIME)%>%
  distinct(HSP_ACCOUNT_ID, NAME, RESULT_TIME, .keep_all=TRUE)%>%
  mutate(INR_NORMAL = case_when( ORD_NUM_VALUE>=REFERENCE_LOW & ORD_NUM_VALUE<=REFERENCE_HIGH ~ "YES", TRUE ~ "NO"),
         INR = ORD_NUM_VALUE)%>%
  select(HSP_ACCOUNT_ID, NAME, RESULT_TIME, INR_NORMAL, INR, REFERENCE_LOW, REFERENCE_HIGH)
# check1 <- as.data.frame(freq(INR$INR)) # Check the outliers
INR <- INR%>%filter( !is.na(INR) & INR >= 0.7  & INR <= 15 ) # Remove the outliers

lowr <- mean(INR$REFERENCE_LOW, na.rm=TRUE)
highr <- mean(INR$REFERENCE_HIGH, na.rm=TRUE)
mean1 <- mean(INR$INR[INR$INR>=lowr & INR$INR<=highr], na.rm=TRUE)
sd1 <- sd(INR$INR[INR$INR>=lowr & INR$INR<=highr], na.rm=TRUE)

INR_Day0 <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6','INP_ABX_DTM')], INR, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ADM_6 )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
INR_Day0 <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), INR_Day0, by = "HSP_ACCOUNT_ID")%>%
  mutate(INR_Day0_Missing = case_when(is.na(INR) ~ 'YES', TRUE ~ 'NO'),
         INR_Day0_Orig = INR,
         INR_Day0_ImpN = INR,
         INR_Day0_ImpA = INR)
INR_Day0$INR_Day0_ImpN[is.na(INR_Day0$INR_Day0_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(INR_Day0$INR_Day0_ImpN)), replace=T)
INR_Day0$INR_Day0_ImpA[is.na(INR_Day0$INR_Day0_ImpA)] <- mean1
INR_Day0 <- INR_Day0%>%
  mutate(INR_Day0_Orig_NORMAL = case_when( is.na(INR_Day0_Orig) ~ 'Unknown',
                                           INR_Day0_Orig>=lowr & INR_Day0_Orig<=highr ~ "YES", 
                                           TRUE ~ "NO"),
         INR_Day0_ImpN_NORMAL = case_when( is.na(INR_Day0_ImpN) ~ 'Unknown',
                                           INR_Day0_ImpN>=lowr & INR_Day0_ImpN<=highr ~ "YES", 
                                           TRUE ~ "NO"),
         INR_Day0_ImpA_NORMAL = case_when( is.na(INR_Day0_ImpA) ~ 'Unknown',
                                           INR_Day0_ImpA>=lowr & INR_Day0_ImpA<=highr ~ "YES", 
                                           TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, INR_Day0_Orig, INR_Day0_Orig_NORMAL, INR_Day0_ImpN, INR_Day0_ImpA, 
         INR_Day0_ImpN_NORMAL, INR_Day0_ImpA_NORMAL, INR_Day0_Missing)

INR_DayX <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ABX_DTM')], INR, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ABX_DTM | is.na(INP_ABX_DTM) )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
INR_DayX <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), INR_DayX, by = "HSP_ACCOUNT_ID")%>%
  mutate(INR_DayX_Missing = case_when(is.na(INR) ~ 'YES', TRUE ~ 'NO'),
         INR_DayX_Orig = INR,
         INR_DayX_ImpN = INR,
         INR_DayX_ImpA = INR)
INR_DayX$INR_DayX_ImpN[is.na(INR_DayX$INR_DayX_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(INR_DayX$INR_DayX_ImpN)), replace=T)
INR_DayX$INR_DayX_ImpA[is.na(INR_DayX$INR_DayX_ImpA)] <- mean1
INR_DayX <- INR_DayX%>%
  mutate(INR_DayX_Orig_NORMAL = case_when( is.na(INR_DayX_Orig) ~ 'Unknown',
                                           INR_DayX_Orig>=lowr & INR_DayX_Orig<=highr ~ "YES", 
                                           TRUE ~ "NO"),
         INR_DayX_ImpN_NORMAL = case_when( is.na(INR_DayX_ImpN) ~ 'Unknown',
                                           INR_DayX_ImpN>=lowr & INR_DayX_ImpN<=highr ~ "YES", 
                                           TRUE ~ "NO"),
         INR_DayX_ImpA_NORMAL = case_when( is.na(INR_DayX_ImpA) ~ 'Unknown',
                                           INR_DayX_ImpA>=lowr & INR_DayX_ImpA<=highr ~ "YES", 
                                           TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, INR_DayX_Orig, INR_DayX_Orig_NORMAL, INR_DayX_ImpN, INR_DayX_ImpA, 
         INR_DayX_ImpN_NORMAL, INR_DayX_ImpA_NORMAL, INR_DayX_Missing)




#########################
#        LACTATE        #
#########################

LACTATE <- Order_Results[grep("LACTATE", Order_Results$NAME), ]%>%
  filter(REFERENCE_LOW==0.5 & REFERENCE_HIGH==2.2)%>%
  arrange(HSP_ACCOUNT_ID, NAME, RESULT_TIME)%>%
  distinct(HSP_ACCOUNT_ID, NAME, RESULT_TIME, .keep_all=TRUE)%>%
  mutate(LACTATE_NORMAL = case_when( ORD_NUM_VALUE>=REFERENCE_LOW & ORD_NUM_VALUE<=REFERENCE_HIGH ~ "YES", TRUE ~ "NO"),
         LACTATE = ORD_NUM_VALUE)%>%
  select(HSP_ACCOUNT_ID, RESULT_TIME, LACTATE_NORMAL, LACTATE, REFERENCE_LOW, REFERENCE_HIGH)
# check1 <- as.data.frame(freq(LACTATE$LACTATE)) # Check the outliers
LACTATE <- LACTATE%>%filter( !is.na(LACTATE) & LACTATE > 0  & LACTATE <= 38 ) # Remove the outliers

lowr <- mean(LACTATE$REFERENCE_LOW, na.rm=TRUE)
highr <- mean(LACTATE$REFERENCE_HIGH, na.rm=TRUE)
mean1 <- mean(LACTATE$LACTATE[LACTATE$LACTATE>=lowr & LACTATE$LACTATE<=highr], na.rm=TRUE)
sd1 <- sd(LACTATE$LACTATE[LACTATE$LACTATE>=lowr & LACTATE$LACTATE<=highr], na.rm=TRUE)

LACTATE_Day0 <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6','INP_ABX_DTM')], LACTATE, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ADM_6 )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
LACTATE_Day0 <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), LACTATE_Day0, by = "HSP_ACCOUNT_ID")%>%
  mutate(LACTATE_Day0_Missing = case_when(is.na(LACTATE) ~ 'YES', TRUE ~ 'NO'),
         LACTATE_Day0_Orig = LACTATE,
         LACTATE_Day0_ImpN = LACTATE,
         LACTATE_Day0_ImpA = LACTATE)
LACTATE_Day0$LACTATE_Day0_ImpN[is.na(LACTATE_Day0$LACTATE_Day0_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(LACTATE_Day0$LACTATE_Day0_ImpN)), replace=T)
LACTATE_Day0$LACTATE_Day0_ImpA[is.na(LACTATE_Day0$LACTATE_Day0_ImpA)] <- mean1
LACTATE_Day0 <- LACTATE_Day0%>%
  mutate(LACTATE_Day0_Orig_NORMAL = case_when( is.na(LACTATE_Day0_Orig) ~ 'Unknown',
                                               LACTATE_Day0_Orig>=lowr & LACTATE_Day0_Orig<=highr ~ "YES", 
                                               TRUE ~ "NO"),
         LACTATE_Day0_ImpN_NORMAL = case_when( is.na(LACTATE_Day0_ImpN) ~ 'Unknown',
                                               LACTATE_Day0_ImpN>=lowr & LACTATE_Day0_ImpN<=highr ~ "YES", 
                                               TRUE ~ "NO"),
         LACTATE_Day0_ImpA_NORMAL = case_when( is.na(LACTATE_Day0_ImpA) ~ 'Unknown',
                                               LACTATE_Day0_ImpA>=lowr & LACTATE_Day0_ImpA<=highr ~ "YES", 
                                               TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, LACTATE_Day0_Orig, LACTATE_Day0_Orig_NORMAL, LACTATE_Day0_ImpN, LACTATE_Day0_ImpA, 
         LACTATE_Day0_ImpN_NORMAL, LACTATE_Day0_ImpA_NORMAL, LACTATE_Day0_Missing)

LACTATE_DayX <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ABX_DTM')], LACTATE, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ABX_DTM | is.na(INP_ABX_DTM) )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
LACTATE_DayX <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), LACTATE_DayX, by = "HSP_ACCOUNT_ID")%>%
  mutate(LACTATE_DayX_Missing = case_when(is.na(LACTATE) ~ 'YES', TRUE ~ 'NO'),
         LACTATE_DayX_Orig = LACTATE,
         LACTATE_DayX_ImpN = LACTATE,
         LACTATE_DayX_ImpA = LACTATE)
LACTATE_DayX$LACTATE_DayX_ImpN[is.na(LACTATE_DayX$LACTATE_DayX_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(LACTATE_DayX$LACTATE_DayX_ImpN)), replace=T)
LACTATE_DayX$LACTATE_DayX_ImpA[is.na(LACTATE_DayX$LACTATE_DayX_ImpA)] <- mean1
LACTATE_DayX <- LACTATE_DayX%>%
  mutate(LACTATE_DayX_Orig_NORMAL = case_when( is.na(LACTATE_DayX_Orig) ~ 'Unknown',
                                               LACTATE_DayX_Orig>=lowr & LACTATE_DayX_Orig<=highr ~ "YES", 
                                               TRUE ~ "NO"),
         LACTATE_DayX_ImpN_NORMAL = case_when( is.na(LACTATE_DayX_ImpN) ~ 'Unknown',
                                               LACTATE_DayX_ImpN>=lowr & LACTATE_DayX_ImpN<=highr ~ "YES", 
                                               TRUE ~ "NO"),
         LACTATE_DayX_ImpA_NORMAL = case_when( is.na(LACTATE_DayX_ImpA) ~ 'Unknown',
                                               LACTATE_DayX_ImpA>=lowr & LACTATE_DayX_ImpA<=highr ~ "YES", 
                                               TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, LACTATE_DayX_Orig, LACTATE_DayX_Orig_NORMAL, LACTATE_DayX_ImpN, LACTATE_DayX_ImpA, 
         LACTATE_DayX_ImpN_NORMAL, LACTATE_DayX_ImpA_NORMAL, LACTATE_DayX_Missing)





#########################
#     LYMPHOCYTES       #
#########################

LYMPHOCYTES <- Order_Results[grep("LYMPHOCYTES", Order_Results$NAME), ]%>%
  filter(NAME == "LYMPHOCYTES ABSOLUTE COUNT" & REFERENCE_LOW==1.3 & REFERENCE_HIGH==4.07)%>%
  arrange(HSP_ACCOUNT_ID, NAME, RESULT_TIME)%>%
  distinct(HSP_ACCOUNT_ID, NAME, RESULT_TIME, .keep_all=TRUE)%>%
  mutate(LYMPHOCYTES_NORMAL = case_when( ORD_NUM_VALUE>=REFERENCE_LOW & ORD_NUM_VALUE<=REFERENCE_HIGH ~ "YES", TRUE ~ "NO"),
         LYMPHOCYTES = ORD_NUM_VALUE)%>%
  select(HSP_ACCOUNT_ID, RESULT_TIME, LYMPHOCYTES_NORMAL, LYMPHOCYTES, REFERENCE_LOW, REFERENCE_HIGH)
# check1 <- as.data.frame(freq(LYMPHOCYTES$LYMPHOCYTES)) # Check the outliers
LYMPHOCYTES <- LYMPHOCYTES%>%filter( !is.na(LYMPHOCYTES) & LYMPHOCYTES > 0 & LYMPHOCYTES <= 20 ) # Remove the outliers

lowr <- mean(LYMPHOCYTES$REFERENCE_LOW, na.rm=TRUE)
highr <- mean(LYMPHOCYTES$REFERENCE_HIGH, na.rm=TRUE)
mean1 <- mean(LYMPHOCYTES$LYMPHOCYTES[LYMPHOCYTES$LYMPHOCYTES>=lowr & LYMPHOCYTES$LYMPHOCYTES<=highr], na.rm=TRUE)
sd1 <- sd(LYMPHOCYTES$LYMPHOCYTES[LYMPHOCYTES$LYMPHOCYTES>=lowr & LYMPHOCYTES$LYMPHOCYTES<=highr], na.rm=TRUE)

LYMPHOCYTES_Day0 <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6','INP_ABX_DTM')], LYMPHOCYTES, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ADM_6 )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
LYMPHOCYTES_Day0 <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), LYMPHOCYTES_Day0, by = "HSP_ACCOUNT_ID")%>%
  mutate(LYMPHOCYTES_Day0_Missing = case_when(is.na(LYMPHOCYTES) ~ 'YES', TRUE ~ 'NO'),
         LYMPHOCYTES_Day0_Orig = LYMPHOCYTES,
         LYMPHOCYTES_Day0_ImpN = LYMPHOCYTES,
         LYMPHOCYTES_Day0_ImpA = LYMPHOCYTES)
LYMPHOCYTES_Day0$LYMPHOCYTES_Day0_ImpN[is.na(LYMPHOCYTES_Day0$LYMPHOCYTES_Day0_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(LYMPHOCYTES_Day0$LYMPHOCYTES_Day0_ImpN)), replace=T)
LYMPHOCYTES_Day0$LYMPHOCYTES_Day0_ImpA[is.na(LYMPHOCYTES_Day0$LYMPHOCYTES_Day0_ImpA)] <- mean1
LYMPHOCYTES_Day0 <- LYMPHOCYTES_Day0%>%
  mutate(LYMPHOCYTES_Day0_Orig_NORMAL = case_when( is.na(LYMPHOCYTES_Day0_Orig) ~ 'Unknown',
                                                   LYMPHOCYTES_Day0_Orig>=lowr & LYMPHOCYTES_Day0_Orig<=highr ~ "YES", 
                                                   TRUE ~ "NO"),
         LYMPHOCYTES_Day0_ImpN_NORMAL = case_when( is.na(LYMPHOCYTES_Day0_ImpN) ~ 'Unknown',
                                                   LYMPHOCYTES_Day0_ImpN>=lowr & LYMPHOCYTES_Day0_ImpN<=highr ~ "YES", 
                                                   TRUE ~ "NO"),
         LYMPHOCYTES_Day0_ImpA_NORMAL = case_when( is.na(LYMPHOCYTES_Day0_ImpA) ~ 'Unknown',
                                                   LYMPHOCYTES_Day0_ImpA>=lowr & LYMPHOCYTES_Day0_ImpA<=highr ~ "YES", 
                                                   TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, LYMPHOCYTES_Day0_Orig, LYMPHOCYTES_Day0_Orig_NORMAL, LYMPHOCYTES_Day0_ImpN, LYMPHOCYTES_Day0_ImpA, 
         LYMPHOCYTES_Day0_ImpN_NORMAL, LYMPHOCYTES_Day0_ImpA_NORMAL, LYMPHOCYTES_Day0_Missing)

LYMPHOCYTES_DayX <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ABX_DTM')], LYMPHOCYTES, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ABX_DTM | is.na(INP_ABX_DTM) )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
LYMPHOCYTES_DayX <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), LYMPHOCYTES_DayX, by = "HSP_ACCOUNT_ID")%>%
  mutate(LYMPHOCYTES_DayX_Missing = case_when(is.na(LYMPHOCYTES) ~ 'YES', TRUE ~ 'NO'),
         LYMPHOCYTES_DayX_Orig = LYMPHOCYTES,
         LYMPHOCYTES_DayX_ImpN = LYMPHOCYTES,
         LYMPHOCYTES_DayX_ImpA = LYMPHOCYTES)
LYMPHOCYTES_DayX$LYMPHOCYTES_DayX_ImpN[is.na(LYMPHOCYTES_DayX$LYMPHOCYTES_DayX_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(LYMPHOCYTES_DayX$LYMPHOCYTES_DayX_ImpN)), replace=T)
LYMPHOCYTES_DayX$LYMPHOCYTES_DayX_ImpA[is.na(LYMPHOCYTES_DayX$LYMPHOCYTES_DayX_ImpA)] <- mean1
LYMPHOCYTES_DayX <- LYMPHOCYTES_DayX%>%
  mutate(LYMPHOCYTES_DayX_Orig_NORMAL = case_when( is.na(LYMPHOCYTES_DayX_Orig) ~ 'Unknown',
                                                   LYMPHOCYTES_DayX_Orig>=lowr & LYMPHOCYTES_DayX_Orig<=highr ~ "YES", 
                                                   TRUE ~ "NO"),
         LYMPHOCYTES_DayX_ImpN_NORMAL = case_when( is.na(LYMPHOCYTES_DayX_ImpN) ~ 'Unknown',
                                                   LYMPHOCYTES_DayX_ImpN>=lowr & LYMPHOCYTES_DayX_ImpN<=highr ~ "YES", 
                                                   TRUE ~ "NO"),
         LYMPHOCYTES_DayX_ImpA_NORMAL = case_when( is.na(LYMPHOCYTES_DayX_ImpA) ~ 'Unknown',
                                                   LYMPHOCYTES_DayX_ImpA>=lowr & LYMPHOCYTES_DayX_ImpA<=highr ~ "YES", 
                                                   TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, LYMPHOCYTES_DayX_Orig, LYMPHOCYTES_DayX_Orig_NORMAL, LYMPHOCYTES_DayX_ImpN, LYMPHOCYTES_DayX_ImpA, 
         LYMPHOCYTES_DayX_ImpN_NORMAL, LYMPHOCYTES_DayX_ImpA_NORMAL, LYMPHOCYTES_DayX_Missing)



# Microbiological Culture NOT FOUND in Clarity




# For New Borns
# NRBC <- Order_Results[grep("NUCLEATED RED BLOOD CELLS", Order_Results$NAME), ]%>%
#   filter(NAME == "NUCLEATED RED BLOOD CELLS" )%>%
#   arrange(HSP_ACCOUNT_ID, NAME, RESULT_TIME)%>%
#   distinct(HSP_ACCOUNT_ID, NAME, RESULT_TIME, .keep_all=TRUE)%>%
#   mutate(NRBC_NORMAL = case_when( ORD_NUM_VALUE>=REFERENCE_LOW & ORD_NUM_VALUE<=REFERENCE_HIGH ~ "YES", TRUE ~ "NO"),
#          NRBC = ORD_NUM_VALUE  )%>%
#   select(HSP_ACCOUNT_ID, RESULT_TIME, NRBC_NORMAL, NRBC, REFERENCE_LOW, REFERENCE_HIGH)
# # check1 <- as.data.frame(freq(NRBC$NRBC)) # Check the outliers
# NRBC <- NRBC%>%filter( !is.na(NRBC) & NRBC >= 0 & NRBC <= 100 ) # Remove the outliers
# 
# NRBC <- left_join(INP_ADM_DTM_I10, NRBC, by = "HSP_ACCOUNT_ID")%>%
#   filter( RESULT_TIME <= INP_ADM_6 )%>%
#   arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
#   distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
# NRBC <- left_join(INP_ADM_DTM_I10, NRBC, by = "HSP_ACCOUNT_ID")%>%
#   mutate(NRBC_M = case_when(is.na(NRBC) ~ 'YES', TRUE ~ 'NO'))
# lowr <- mean(NRBC$REFERENCE_LOW, na.rm=TRUE)
# highr <- mean(NRBC$REFERENCE_HIGH, na.rm=TRUE)
# mean1 <- mean(NRBC$NRBC[NRBC$NRBC>=lowr & NRBC$NRBC<=highr], na.rm=TRUE)
# sd1 <- sd(NRBC$NRBC[NRBC$NRBC>=lowr & NRBC$NRBC<=highr], na.rm=TRUE)
# NRBC$NRBC[is.na(NRBC$NRBC)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(NRBC$NRBC)), replace=T)
# NRBC <- NRBC%>%
#   mutate(NRBC_NORMAL = case_when( NRBC>=lowr & NRBC<=highr ~ "YES", TRUE ~ "NO"))%>%
#   select(HSP_ACCOUNT_ID, NRBC, NRBC_NORMAL, NRBC_M)



#########################
#      NEUTROPHIL       #
#########################

NEUTROPHIL <- Order_Results[grep("NEUTROPHIL", Order_Results$NAME), ]%>%
  filter(REFERENCE_LOW == 1.98 & REFERENCE_HIGH == 6.59 )%>%
  arrange(HSP_ACCOUNT_ID, NAME, RESULT_TIME)%>%
  distinct(HSP_ACCOUNT_ID, NAME, RESULT_TIME, .keep_all=TRUE)%>%
  mutate(NEUTROPHIL_NORMAL = case_when( ORD_NUM_VALUE>=REFERENCE_LOW & ORD_NUM_VALUE<=REFERENCE_HIGH ~ "YES", TRUE ~ "NO"),
         NEUTROPHIL = ORD_NUM_VALUE  )%>%
  select(HSP_ACCOUNT_ID, RESULT_TIME, NEUTROPHIL_NORMAL, NEUTROPHIL, REFERENCE_LOW, REFERENCE_HIGH)
# check1 <- as.data.frame(freq(NEUTROPHIL$NEUTROPHIL)) # Check the outliers
NEUTROPHIL <- NEUTROPHIL%>%filter( !is.na(NEUTROPHIL) & NEUTROPHIL > 0 & NEUTROPHIL <= 20 ) # Remove the outliers

lowr <- mean(NEUTROPHIL$REFERENCE_LOW, na.rm=TRUE)
highr <- mean(NEUTROPHIL$REFERENCE_HIGH, na.rm=TRUE)
mean1 <- mean(NEUTROPHIL$NEUTROPHIL[NEUTROPHIL$NEUTROPHIL>=lowr & NEUTROPHIL$NEUTROPHIL<=highr], na.rm=TRUE)
sd1 <- sd(NEUTROPHIL$NEUTROPHIL[NEUTROPHIL$NEUTROPHIL>=lowr & NEUTROPHIL$NEUTROPHIL<=highr], na.rm=TRUE)

NEUTROPHIL_Day0 <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6','INP_ABX_DTM')], NEUTROPHIL, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ADM_6 )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
NEUTROPHIL_Day0 <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), NEUTROPHIL_Day0, by = "HSP_ACCOUNT_ID")%>%
  mutate(NEUTROPHIL_Day0_Missing = case_when(is.na(NEUTROPHIL) ~ 'YES', TRUE ~ 'NO'),
         NEUTROPHIL_Day0_Orig = NEUTROPHIL,
         NEUTROPHIL_Day0_ImpN = NEUTROPHIL,
         NEUTROPHIL_Day0_ImpA = NEUTROPHIL)
NEUTROPHIL_Day0$NEUTROPHIL_Day0_ImpN[is.na(NEUTROPHIL_Day0$NEUTROPHIL_Day0_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(NEUTROPHIL_Day0$NEUTROPHIL_Day0_ImpN)), replace=T)
NEUTROPHIL_Day0$NEUTROPHIL_Day0_ImpA[is.na(NEUTROPHIL_Day0$NEUTROPHIL_Day0_ImpA)] <- mean1
NEUTROPHIL_Day0 <- NEUTROPHIL_Day0%>%
  mutate(NEUTROPHIL_Day0_Orig_NORMAL = case_when( is.na(NEUTROPHIL_Day0_Orig) ~ 'Unknown',
                                                  NEUTROPHIL_Day0_Orig>=lowr & NEUTROPHIL_Day0_Orig<=highr ~ "YES", 
                                                  TRUE ~ "NO"),
         NEUTROPHIL_Day0_ImpN_NORMAL = case_when( is.na(NEUTROPHIL_Day0_ImpN) ~ 'Unknown',
                                                  NEUTROPHIL_Day0_ImpN>=lowr & NEUTROPHIL_Day0_ImpN<=highr ~ "YES", 
                                                  TRUE ~ "NO"),
         NEUTROPHIL_Day0_ImpA_NORMAL = case_when( is.na(NEUTROPHIL_Day0_ImpA) ~ 'Unknown',
                                                  NEUTROPHIL_Day0_ImpA>=lowr & NEUTROPHIL_Day0_ImpA<=highr ~ "YES", 
                                                  TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, NEUTROPHIL_Day0_Orig, NEUTROPHIL_Day0_Orig_NORMAL, NEUTROPHIL_Day0_ImpN, NEUTROPHIL_Day0_ImpA, 
         NEUTROPHIL_Day0_ImpN_NORMAL, NEUTROPHIL_Day0_ImpA_NORMAL, NEUTROPHIL_Day0_Missing)

NEUTROPHIL_DayX <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ABX_DTM')], NEUTROPHIL, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ABX_DTM | is.na(INP_ABX_DTM) )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
NEUTROPHIL_DayX <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), NEUTROPHIL_DayX, by = "HSP_ACCOUNT_ID")%>%
  mutate(NEUTROPHIL_DayX_Missing = case_when(is.na(NEUTROPHIL) ~ 'YES', TRUE ~ 'NO'),
         NEUTROPHIL_DayX_Orig = NEUTROPHIL,
         NEUTROPHIL_DayX_ImpN = NEUTROPHIL,
         NEUTROPHIL_DayX_ImpA = NEUTROPHIL)
NEUTROPHIL_DayX$NEUTROPHIL_DayX_ImpN[is.na(NEUTROPHIL_DayX$NEUTROPHIL_DayX_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(NEUTROPHIL_DayX$NEUTROPHIL_DayX_ImpN)), replace=T)
NEUTROPHIL_DayX$NEUTROPHIL_DayX_ImpA[is.na(NEUTROPHIL_DayX$NEUTROPHIL_DayX_ImpA)] <- mean1
NEUTROPHIL_DayX <- NEUTROPHIL_DayX%>%
  mutate(NEUTROPHIL_DayX_Orig_NORMAL = case_when( is.na(NEUTROPHIL_DayX_Orig) ~ 'Unknown',
                                                  NEUTROPHIL_DayX_Orig>=lowr & NEUTROPHIL_DayX_Orig<=highr ~ "YES", 
                                                  TRUE ~ "NO"),
         NEUTROPHIL_DayX_ImpN_NORMAL = case_when( is.na(NEUTROPHIL_DayX_ImpN) ~ 'Unknown',
                                                  NEUTROPHIL_DayX_ImpN>=lowr & NEUTROPHIL_DayX_ImpN<=highr ~ "YES", 
                                                  TRUE ~ "NO"),
         NEUTROPHIL_DayX_ImpA_NORMAL = case_when( is.na(NEUTROPHIL_DayX_ImpA) ~ 'Unknown',
                                                  NEUTROPHIL_DayX_ImpA>=lowr & NEUTROPHIL_DayX_ImpA<=highr ~ "YES", 
                                                  TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, NEUTROPHIL_DayX_Orig, NEUTROPHIL_DayX_Orig_NORMAL, NEUTROPHIL_DayX_ImpN, NEUTROPHIL_DayX_ImpA, 
         NEUTROPHIL_DayX_ImpN_NORMAL, NEUTROPHIL_DayX_ImpA_NORMAL, NEUTROPHIL_DayX_Missing)





#########################
#          MPV          #
#########################

MPV <- Order_Results[grep("MEAN PLATELET VOLUME", Order_Results$NAME), ]%>%
  filter(REFERENCE_LOW == 8.8 & REFERENCE_HIGH == 12.2 )%>%
  arrange(HSP_ACCOUNT_ID, NAME, RESULT_TIME)%>%
  distinct(HSP_ACCOUNT_ID, NAME, RESULT_TIME, .keep_all=TRUE)%>%
  mutate(MPV_NORMAL = case_when( ORD_NUM_VALUE>=REFERENCE_LOW & ORD_NUM_VALUE<=REFERENCE_HIGH ~ "YES", TRUE ~ "NO"),
         MPV = ORD_NUM_VALUE  )%>%
  select(HSP_ACCOUNT_ID, RESULT_TIME, MPV_NORMAL, MPV, REFERENCE_LOW, REFERENCE_HIGH)
# check1 <- as.data.frame(freq(MPV$MPV)) # Check the outliers
MPV <- MPV%>%filter( !is.na(MPV) & MPV > 0 & MPV <= 16 ) # Remove the outliers

lowr <- mean(MPV$REFERENCE_LOW, na.rm=TRUE)
highr <- mean(MPV$REFERENCE_HIGH, na.rm=TRUE)
mean1 <- mean(MPV$MPV[MPV$MPV>=lowr & MPV$MPV<=highr], na.rm=TRUE)
sd1 <- sd(MPV$MPV[MPV$MPV>=lowr & MPV$MPV<=highr], na.rm=TRUE)

MPV_Day0 <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6','INP_ABX_DTM')], MPV, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ADM_6 )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
MPV_Day0 <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), MPV_Day0, by = "HSP_ACCOUNT_ID")%>%
  mutate(MPV_Day0_Missing = case_when(is.na(MPV) ~ 'YES', TRUE ~ 'NO'),
         MPV_Day0_Orig = MPV,
         MPV_Day0_ImpN = MPV,
         MPV_Day0_ImpA = MPV)
MPV_Day0$MPV_Day0_ImpN[is.na(MPV_Day0$MPV_Day0_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(MPV_Day0$MPV_Day0_ImpN)), replace=T)
MPV_Day0$MPV_Day0_ImpA[is.na(MPV_Day0$MPV_Day0_ImpA)] <- mean1
MPV_Day0 <- MPV_Day0%>%
  mutate(MPV_Day0_Orig_NORMAL = case_when( is.na(MPV_Day0_Orig) ~ 'Unknown',
                                           MPV_Day0_Orig>=lowr & MPV_Day0_Orig<=highr ~ "YES", 
                                           TRUE ~ "NO"),
         MPV_Day0_ImpN_NORMAL = case_when( is.na(MPV_Day0_ImpN) ~ 'Unknown',
                                           MPV_Day0_ImpN>=lowr & MPV_Day0_ImpN<=highr ~ "YES", 
                                           TRUE ~ "NO"),
         MPV_Day0_ImpA_NORMAL = case_when( is.na(MPV_Day0_ImpA) ~ 'Unknown',
                                           MPV_Day0_ImpA>=lowr & MPV_Day0_ImpA<=highr ~ "YES", 
                                           TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, MPV_Day0_Orig, MPV_Day0_Orig_NORMAL, MPV_Day0_ImpN, MPV_Day0_ImpA, 
         MPV_Day0_ImpN_NORMAL, MPV_Day0_ImpA_NORMAL, MPV_Day0_Missing)

MPV_DayX <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ABX_DTM')], MPV, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ABX_DTM | is.na(INP_ABX_DTM) )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
MPV_DayX <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), MPV_DayX, by = "HSP_ACCOUNT_ID")%>%
  mutate(MPV_DayX_Missing = case_when(is.na(MPV) ~ 'YES', TRUE ~ 'NO'),
         MPV_DayX_Orig = MPV,
         MPV_DayX_ImpN = MPV,
         MPV_DayX_ImpA = MPV)
MPV_DayX$MPV_DayX_ImpN[is.na(MPV_DayX$MPV_DayX_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(MPV_DayX$MPV_DayX_ImpN)), replace=T)
MPV_DayX$MPV_DayX_ImpA[is.na(MPV_DayX$MPV_DayX_ImpA)] <- mean1
MPV_DayX <- MPV_DayX%>%
  mutate(MPV_DayX_Orig_NORMAL = case_when( is.na(MPV_DayX_Orig) ~ 'Unknown',
                                           MPV_DayX_Orig>=lowr & MPV_DayX_Orig<=highr ~ "YES", 
                                           TRUE ~ "NO"),
         MPV_DayX_ImpN_NORMAL = case_when( is.na(MPV_DayX_ImpN) ~ 'Unknown',
                                           MPV_DayX_ImpN>=lowr & MPV_DayX_ImpN<=highr ~ "YES", 
                                           TRUE ~ "NO"),
         MPV_DayX_ImpA_NORMAL = case_when( is.na(MPV_DayX_ImpA) ~ 'Unknown',
                                           MPV_DayX_ImpA>=lowr & MPV_DayX_ImpA<=highr ~ "YES", 
                                           TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, MPV_DayX_Orig, MPV_DayX_Orig_NORMAL, MPV_DayX_ImpN, MPV_DayX_ImpA, 
         MPV_DayX_ImpN_NORMAL, MPV_DayX_ImpA_NORMAL, MPV_DayX_Missing)




### PROCALCITONIN NOT FOUND in Clarity




#########################
#           WBC         #
#########################

WBC <- Order_Results[grep("WHITE BLOOD COUNT", Order_Results$NAME), ]%>%
  filter(NAME == "WHITE BLOOD COUNT" & ((REFERENCE_LOW==4.5&REFERENCE_HIGH==11)|(REFERENCE_LOW==4.22&REFERENCE_HIGH==10.33)))%>%
  arrange(HSP_ACCOUNT_ID, NAME, RESULT_TIME)%>%
  distinct(HSP_ACCOUNT_ID, NAME, RESULT_TIME, .keep_all=TRUE)%>%
  mutate(WBC_NORMAL = case_when( ORD_NUM_VALUE>=REFERENCE_LOW & ORD_NUM_VALUE<=REFERENCE_HIGH ~ "YES", TRUE ~ "NO"),
         WBC = ORD_NUM_VALUE )%>%
  select(HSP_ACCOUNT_ID, RESULT_TIME, WBC_NORMAL, WBC, REFERENCE_LOW, REFERENCE_HIGH)
# check1 <- as.data.frame(freq(WBC$WBC)) # Check the outliers
WBC <- WBC%>%filter( !is.na(WBC) & WBC > 0 & WBC <= 600 ) # Remove the outliers

lowr <- mean(WBC$REFERENCE_LOW, na.rm=TRUE)
highr <- mean(WBC$REFERENCE_HIGH, na.rm=TRUE)
mean1 <- mean(WBC$WBC[WBC$WBC>=lowr & WBC$WBC<=highr], na.rm=TRUE)
sd1 <- sd(WBC$WBC[WBC$WBC>=lowr & WBC$WBC<=highr], na.rm=TRUE)

WBC_Day0 <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6','INP_ABX_DTM')], WBC, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ADM_6 )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
WBC_Day0 <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), WBC_Day0, by = "HSP_ACCOUNT_ID")%>%
  mutate(WBC_Day0_Missing = case_when(is.na(WBC) ~ 'YES', TRUE ~ 'NO'),
         WBC_Day0_Orig = WBC,
         WBC_Day0_ImpN = WBC,
         WBC_Day0_ImpA = WBC)
WBC_Day0$WBC_Day0_ImpN[is.na(WBC_Day0$WBC_Day0_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(WBC_Day0$WBC_Day0_ImpN)), replace=T)
WBC_Day0$WBC_Day0_ImpA[is.na(WBC_Day0$WBC_Day0_ImpA)] <- mean1
WBC_Day0 <- WBC_Day0%>%
  mutate(WBC_Day0_Orig_NORMAL = case_when( is.na(WBC_Day0_Orig) ~ 'Unknown',
                                           WBC_Day0_Orig>=lowr & WBC_Day0_Orig<=highr ~ "YES", 
                                           TRUE ~ "NO"),
         WBC_Day0_ImpN_NORMAL = case_when( is.na(WBC_Day0_ImpN) ~ 'Unknown',
                                           WBC_Day0_ImpN>=lowr & WBC_Day0_ImpN<=highr ~ "YES", 
                                           TRUE ~ "NO"),
         WBC_Day0_ImpA_NORMAL = case_when( is.na(WBC_Day0_ImpA) ~ 'Unknown',
                                           WBC_Day0_ImpA>=lowr & WBC_Day0_ImpA<=highr ~ "YES", 
                                           TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, WBC_Day0_Orig, WBC_Day0_Orig_NORMAL, WBC_Day0_ImpN, WBC_Day0_ImpA, 
         WBC_Day0_ImpN_NORMAL, WBC_Day0_ImpA_NORMAL, WBC_Day0_Missing)

WBC_DayX <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ABX_DTM')], WBC, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ABX_DTM | is.na(INP_ABX_DTM) )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
WBC_DayX <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), WBC_DayX, by = "HSP_ACCOUNT_ID")%>%
  mutate(WBC_DayX_Missing = case_when(is.na(WBC) ~ 'YES', TRUE ~ 'NO'),
         WBC_DayX_Orig = WBC,
         WBC_DayX_ImpN = WBC,
         WBC_DayX_ImpA = WBC)
WBC_DayX$WBC_DayX_ImpN[is.na(WBC_DayX$WBC_DayX_ImpN)] <- sample(rnorm(180000,mean1,sd1), size=sum(is.na(WBC_DayX$WBC_DayX_ImpN)), replace=T)
WBC_DayX$WBC_DayX_ImpA[is.na(WBC_DayX$WBC_DayX_ImpA)] <- mean1
WBC_DayX <- WBC_DayX%>%
  mutate(WBC_DayX_Orig_NORMAL = case_when( is.na(WBC_DayX_Orig) ~ 'Unknown',
                                           WBC_DayX_Orig>=lowr & WBC_DayX_Orig<=highr ~ "YES", 
                                           TRUE ~ "NO"),
         WBC_DayX_ImpN_NORMAL = case_when( is.na(WBC_DayX_ImpN) ~ 'Unknown',
                                           WBC_DayX_ImpN>=lowr & WBC_DayX_ImpN<=highr ~ "YES", 
                                           TRUE ~ "NO"),
         WBC_DayX_ImpA_NORMAL = case_when( is.na(WBC_DayX_ImpA) ~ 'Unknown',
                                           WBC_DayX_ImpA>=lowr & WBC_DayX_ImpA<=highr ~ "YES", 
                                           TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, WBC_DayX_Orig, WBC_DayX_Orig_NORMAL, WBC_DayX_ImpN, WBC_DayX_ImpA, 
         WBC_DayX_ImpN_NORMAL, WBC_DayX_ImpA_NORMAL, WBC_DayX_Missing)





#########################
#       CHLORIDE        #
#########################

CHLORIDE <- Order_Results[grep("CHLORIDE LEVEL", Order_Results$NAME), ]%>%
  arrange(HSP_ACCOUNT_ID, NAME, RESULT_TIME)%>%
  distinct(HSP_ACCOUNT_ID, NAME, RESULT_TIME, .keep_all=TRUE)%>%
  mutate(CHLORIDE_NORMAL = case_when( ORD_NUM_VALUE>=REFERENCE_LOW & ORD_NUM_VALUE<=REFERENCE_HIGH ~ "YES", TRUE ~ "NO"),
         CHLORIDE = ORD_NUM_VALUE  )%>%
  select(HSP_ACCOUNT_ID, RESULT_TIME, CHLORIDE, CHLORIDE_NORMAL, REFERENCE_LOW, REFERENCE_HIGH)
# check1 <- as.data.frame(freq(CHLORIDE$CHLORIDE)) # Check the outliers
CHLORIDE <- CHLORIDE%>%filter( !is.na(CHLORIDE) & CHLORIDE >= 60  & CHLORIDE <= 140 ) # Remove the outliers

lowr <- floor(mean(CHLORIDE$REFERENCE_LOW, na.rm=TRUE))
highr <- ceiling(mean(CHLORIDE$REFERENCE_HIGH, na.rm=TRUE))
mean1 <- floor(mean(CHLORIDE$CHLORIDE[CHLORIDE$CHLORIDE>=lowr & CHLORIDE$CHLORIDE<=highr], na.rm=TRUE))
sd1 <- sd(CHLORIDE$CHLORIDE[CHLORIDE$CHLORIDE>=lowr & CHLORIDE$CHLORIDE<=highr], na.rm=TRUE)

# hist(CHLORIDE_Day0[CHLORIDE_Day0$CHLORIDE_Day0_ImpN_NORMAL=='YES',]$CHLORIDE_Day0_ImpN)
# hist(CHLORIDE_Day0[CHLORIDE_Day0$CHLORIDE_Day0_ImpA_NORMAL=='YES',]$CHLORIDE_Day0_ImpA)

CHLORIDE_Day0 <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6')], CHLORIDE, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ADM_6 )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
CHLORIDE_Day0 <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), CHLORIDE_Day0, by = "HSP_ACCOUNT_ID")%>%
  mutate(CHLORIDE_Day0_Missing = case_when(is.na(CHLORIDE) ~ 'YES', TRUE ~ 'NO'),
         CHLORIDE_Day0_Orig = CHLORIDE,
         CHLORIDE_Day0_ImpN = CHLORIDE,
         CHLORIDE_Day0_ImpA = CHLORIDE)
CHLORIDE_Day0$CHLORIDE_Day0_ImpN[is.na(CHLORIDE_Day0$CHLORIDE_Day0_ImpN)] <- sample(floor(rnorm(180000,mean1,sd1)), size=sum(is.na(CHLORIDE_Day0$CHLORIDE_Day0_ImpN)), replace=T)
CHLORIDE_Day0$CHLORIDE_Day0_ImpA[is.na(CHLORIDE_Day0$CHLORIDE_Day0_ImpA)] <- floor(mean1)
CHLORIDE_Day0 <- CHLORIDE_Day0%>%
  mutate(CHLORIDE_Day0_Orig_NORMAL = case_when( is.na(CHLORIDE_Day0_Orig) ~ 'Unknown',
                                                CHLORIDE_Day0_Orig>=lowr & CHLORIDE_Day0_Orig<=highr ~ "YES", 
                                                TRUE ~ "NO"),
         CHLORIDE_Day0_ImpN_NORMAL = case_when( is.na(CHLORIDE_Day0_ImpN) ~ 'Unknown',
                                                CHLORIDE_Day0_ImpN>=lowr & CHLORIDE_Day0_ImpN<=highr ~ "YES", 
                                                TRUE ~ "NO"),
         CHLORIDE_Day0_ImpA_NORMAL = case_when( is.na(CHLORIDE_Day0_ImpA) ~ 'Unknown',
                                                CHLORIDE_Day0_ImpA>=lowr & CHLORIDE_Day0_ImpA<=highr ~ "YES", 
                                                TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, CHLORIDE_Day0_Orig, CHLORIDE_Day0_Orig_NORMAL, CHLORIDE_Day0_ImpN, CHLORIDE_Day0_ImpA, 
         CHLORIDE_Day0_ImpN_NORMAL, CHLORIDE_Day0_ImpA_NORMAL, CHLORIDE_Day0_Missing)

CHLORIDE_DayX <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6','INP_ABX_DTM')], CHLORIDE, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ABX_DTM | is.na(INP_ABX_DTM) )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
CHLORIDE_DayX <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), CHLORIDE_DayX, by = "HSP_ACCOUNT_ID")%>%
  mutate(CHLORIDE_DayX_Missing = case_when(is.na(CHLORIDE) ~ 'YES', TRUE ~ 'NO'),
         CHLORIDE_DayX_Orig = CHLORIDE,
         CHLORIDE_DayX_ImpN = CHLORIDE,
         CHLORIDE_DayX_ImpA = CHLORIDE)
CHLORIDE_DayX$CHLORIDE_DayX_ImpN[is.na(CHLORIDE_DayX$CHLORIDE_DayX_ImpN)] <- sample(floor(rnorm(180000,mean1,sd1)), size=sum(is.na(CHLORIDE_DayX$CHLORIDE_DayX_ImpN)), replace=T)
CHLORIDE_DayX$CHLORIDE_DayX_ImpA[is.na(CHLORIDE_DayX$CHLORIDE_DayX_ImpA)] <- floor(mean1)
CHLORIDE_DayX <- CHLORIDE_DayX%>%
  mutate(CHLORIDE_DayX_Orig_NORMAL = case_when( is.na(CHLORIDE_DayX_Orig) ~ 'Unknown',
                                                CHLORIDE_DayX_Orig>=lowr & CHLORIDE_DayX_Orig<=highr ~ "YES", 
                                                TRUE ~ "NO"),
         CHLORIDE_DayX_ImpN_NORMAL = case_when( is.na(CHLORIDE_DayX_ImpN) ~ 'Unknown',
                                                CHLORIDE_DayX_ImpN>=lowr & CHLORIDE_DayX_ImpN<=highr ~ "YES", 
                                                TRUE ~ "NO"),
         CHLORIDE_DayX_ImpA_NORMAL = case_when( is.na(CHLORIDE_DayX_ImpA) ~ 'Unknown',
                                                CHLORIDE_DayX_ImpA>=lowr & CHLORIDE_DayX_ImpA<=highr ~ "YES", 
                                                TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, CHLORIDE_DayX_Orig, CHLORIDE_DayX_Orig_NORMAL, CHLORIDE_DayX_ImpN, CHLORIDE_DayX_ImpA, 
         CHLORIDE_DayX_ImpN_NORMAL, CHLORIDE_DayX_ImpA_NORMAL, CHLORIDE_DayX_Missing)





#########################
#         SODIUM        #
#########################

SODIUM <- Order_Results[grep("SODIUM LEVEL", Order_Results$NAME), ]%>%
  arrange(HSP_ACCOUNT_ID, NAME, RESULT_TIME)%>%
  distinct(HSP_ACCOUNT_ID, NAME, RESULT_TIME, .keep_all=TRUE)%>%
  mutate(SODIUM_NORMAL = case_when( ORD_NUM_VALUE>=REFERENCE_LOW & ORD_NUM_VALUE<=REFERENCE_HIGH ~ "YES", TRUE ~ "NO"),
         SODIUM = ORD_NUM_VALUE  )%>%
  select(HSP_ACCOUNT_ID, RESULT_TIME, SODIUM, SODIUM_NORMAL, REFERENCE_LOW, REFERENCE_HIGH)
# check1 <- as.data.frame(freq(SODIUM$SODIUM)) # Check the outliers
SODIUM <- SODIUM%>%filter( !is.na(SODIUM) & SODIUM > 0  & SODIUM <= 180 ) # Remove the outliers

lowr <- floor(mean(SODIUM$REFERENCE_LOW, na.rm=TRUE))
highr <- ceiling(mean(SODIUM$REFERENCE_HIGH, na.rm=TRUE))
mean1 <- floor(mean(SODIUM$SODIUM[SODIUM$SODIUM>=lowr & SODIUM$SODIUM<=highr], na.rm=TRUE))
sd1 <- sd(SODIUM$SODIUM[SODIUM$SODIUM>=lowr & SODIUM$SODIUM<=highr], na.rm=TRUE)

SODIUM_Day0 <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6')], SODIUM, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ADM_6 )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
SODIUM_Day0 <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), SODIUM_Day0, by = "HSP_ACCOUNT_ID")%>%
  mutate(SODIUM_Day0_Missing = case_when(is.na(SODIUM) ~ 'YES', TRUE ~ 'NO'),
         SODIUM_Day0_Orig = SODIUM,
         SODIUM_Day0_ImpN = SODIUM,
         SODIUM_Day0_ImpA = SODIUM)
SODIUM_Day0$SODIUM_Day0_ImpN[is.na(SODIUM_Day0$SODIUM_Day0_ImpN)] <- sample(floor(rnorm(180000,mean1,sd1)), size=sum(is.na(SODIUM_Day0$SODIUM_Day0_ImpN)), replace=T)
SODIUM_Day0$SODIUM_Day0_ImpA[is.na(SODIUM_Day0$SODIUM_Day0_ImpA)] <- floor(mean1)
SODIUM_Day0 <- SODIUM_Day0%>%
  mutate(SODIUM_Day0_Orig_NORMAL = case_when( is.na(SODIUM_Day0_Orig) ~ 'Unknown',
                                              SODIUM_Day0_Orig>=lowr & SODIUM_Day0_Orig<=highr ~ "YES", 
                                              TRUE ~ "NO"),
         SODIUM_Day0_ImpN_NORMAL = case_when( is.na(SODIUM_Day0_ImpN) ~ 'Unknown',
                                              SODIUM_Day0_ImpN>=lowr & SODIUM_Day0_ImpN<=highr ~ "YES", 
                                              TRUE ~ "NO"),
         SODIUM_Day0_ImpA_NORMAL = case_when( is.na(SODIUM_Day0_ImpA) ~ 'Unknown',
                                              SODIUM_Day0_ImpA>=lowr & SODIUM_Day0_ImpA<=highr ~ "YES", 
                                              TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, SODIUM_Day0_Orig, SODIUM_Day0_Orig_NORMAL, SODIUM_Day0_ImpN, SODIUM_Day0_ImpA, 
         SODIUM_Day0_ImpN_NORMAL, SODIUM_Day0_ImpA_NORMAL, SODIUM_Day0_Missing)

SODIUM_DayX <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6','INP_ABX_DTM')], SODIUM, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ABX_DTM | is.na(INP_ABX_DTM) )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
SODIUM_DayX <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), SODIUM_DayX, by = "HSP_ACCOUNT_ID")%>%
  mutate(SODIUM_DayX_Missing = case_when(is.na(SODIUM) ~ 'YES', TRUE ~ 'NO'),
         SODIUM_DayX_Orig = SODIUM,
         SODIUM_DayX_ImpN = SODIUM,
         SODIUM_DayX_ImpA = SODIUM)
SODIUM_DayX$SODIUM_DayX_ImpN[is.na(SODIUM_DayX$SODIUM_DayX_ImpN)] <- sample(floor(rnorm(180000,mean1,sd1)), size=sum(is.na(SODIUM_DayX$SODIUM_DayX_ImpN)), replace=T)
SODIUM_DayX$SODIUM_DayX_ImpA[is.na(SODIUM_DayX$SODIUM_DayX_ImpA)] <- floor(mean1)
SODIUM_DayX <- SODIUM_DayX%>%
  mutate(SODIUM_DayX_Orig_NORMAL = case_when( is.na(SODIUM_DayX_Orig) ~ 'Unknown',
                                              SODIUM_DayX_Orig>=lowr & SODIUM_DayX_Orig<=highr ~ "YES", 
                                              TRUE ~ "NO"),
         SODIUM_DayX_ImpN_NORMAL = case_when( is.na(SODIUM_DayX_ImpN) ~ 'Unknown',
                                              SODIUM_DayX_ImpN>=lowr & SODIUM_DayX_ImpN<=highr ~ "YES", 
                                              TRUE ~ "NO"),
         SODIUM_DayX_ImpA_NORMAL = case_when( is.na(SODIUM_DayX_ImpA) ~ 'Unknown',
                                              SODIUM_DayX_ImpA>=lowr & SODIUM_DayX_ImpA<=highr ~ "YES", 
                                              TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, SODIUM_DayX_Orig, SODIUM_DayX_Orig_NORMAL, SODIUM_DayX_ImpN, SODIUM_DayX_ImpA, 
         SODIUM_DayX_ImpN_NORMAL, SODIUM_DayX_ImpA_NORMAL, SODIUM_DayX_Missing)






#########################
#       GLUCOSE         #
#########################

GLUCOSE <- Order_Results[grep("GLUCOSE", Order_Results$NAME),]%>%
  filter(REFERENCE_LOW == 65 & REFERENCE_HIGH == 200)%>%
  arrange(HSP_ACCOUNT_ID, NAME, RESULT_TIME)%>%
  distinct(HSP_ACCOUNT_ID, NAME, RESULT_TIME, .keep_all=TRUE)%>%
  mutate(GLUCOSE_NORMAL = case_when( ORD_NUM_VALUE>=REFERENCE_LOW & ORD_NUM_VALUE<=REFERENCE_HIGH ~ "YES", TRUE ~ "NO"),
         GLUCOSE = ORD_NUM_VALUE)%>%
  select(HSP_ACCOUNT_ID, RESULT_TIME, GLUCOSE, GLUCOSE_NORMAL, REFERENCE_LOW, REFERENCE_HIGH)
# check1 <- as.data.frame(freq(GLUCOSE$GLUCOSE)) # Check the outliers
GLUCOSE <- GLUCOSE%>%filter( !is.na(GLUCOSE) & GLUCOSE >= 3  & GLUCOSE <= 1460 ) # Remove the outliers

lowr <- floor(mean(GLUCOSE$REFERENCE_LOW, na.rm=TRUE))
highr <- ceiling(mean(GLUCOSE$REFERENCE_HIGH, na.rm=TRUE))
mean1 <- floor(mean(GLUCOSE$GLUCOSE[GLUCOSE$GLUCOSE>=lowr & GLUCOSE$GLUCOSE<=highr], na.rm=TRUE))
sd1 <- sd(GLUCOSE$GLUCOSE[GLUCOSE$GLUCOSE>=lowr & GLUCOSE$GLUCOSE<=highr], na.rm=TRUE)

GLUCOSE_Day0 <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6')], GLUCOSE, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ADM_6 )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
GLUCOSE_Day0 <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), GLUCOSE_Day0, by = "HSP_ACCOUNT_ID")%>%
  mutate(GLUCOSE_Day0_Missing = case_when(is.na(GLUCOSE) ~ 'YES', TRUE ~ 'NO'),
         GLUCOSE_Day0_Orig = GLUCOSE,
         GLUCOSE_Day0_ImpN = GLUCOSE,
         GLUCOSE_Day0_ImpA = GLUCOSE)
GLUCOSE_Day0$GLUCOSE_Day0_ImpN[is.na(GLUCOSE_Day0$GLUCOSE_Day0_ImpN)] <- sample(floor(rnorm(180000,mean1,sd1)), size=sum(is.na(GLUCOSE_Day0$GLUCOSE_Day0_ImpN)), replace=T)
GLUCOSE_Day0$GLUCOSE_Day0_ImpA[is.na(GLUCOSE_Day0$GLUCOSE_Day0_ImpA)] <- floor(mean1)
GLUCOSE_Day0 <- GLUCOSE_Day0%>%
  mutate(GLUCOSE_Day0_Orig_NORMAL = case_when( is.na(GLUCOSE_Day0_Orig) ~ 'Unknown',
                                               GLUCOSE_Day0_Orig>=lowr & GLUCOSE_Day0_Orig<=highr ~ "YES", 
                                               TRUE ~ "NO"),
         GLUCOSE_Day0_ImpN_NORMAL = case_when( is.na(GLUCOSE_Day0_ImpN) ~ 'Unknown',
                                               GLUCOSE_Day0_ImpN>=lowr & GLUCOSE_Day0_ImpN<=highr ~ "YES", 
                                               TRUE ~ "NO"),
         GLUCOSE_Day0_ImpA_NORMAL = case_when( is.na(GLUCOSE_Day0_ImpA) ~ 'Unknown',
                                               GLUCOSE_Day0_ImpA>=lowr & GLUCOSE_Day0_ImpA<=highr ~ "YES", 
                                               TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, GLUCOSE_Day0_Orig, GLUCOSE_Day0_Orig_NORMAL, GLUCOSE_Day0_ImpN, GLUCOSE_Day0_ImpA, 
         GLUCOSE_Day0_ImpN_NORMAL, GLUCOSE_Day0_ImpA_NORMAL, GLUCOSE_Day0_Missing)

GLUCOSE_DayX <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6','INP_ABX_DTM')], GLUCOSE, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ABX_DTM | is.na(INP_ABX_DTM) )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
GLUCOSE_DayX <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), GLUCOSE_DayX, by = "HSP_ACCOUNT_ID")%>%
  mutate(GLUCOSE_DayX_Missing = case_when(is.na(GLUCOSE) ~ 'YES', TRUE ~ 'NO'),
         GLUCOSE_DayX_Orig = GLUCOSE,
         GLUCOSE_DayX_ImpN = GLUCOSE,
         GLUCOSE_DayX_ImpA = GLUCOSE)
GLUCOSE_DayX$GLUCOSE_DayX_ImpN[is.na(GLUCOSE_DayX$GLUCOSE_DayX_ImpN)] <- sample(floor(rnorm(180000,mean1,sd1)), size=sum(is.na(GLUCOSE_DayX$GLUCOSE_DayX_ImpN)), replace=T)
GLUCOSE_DayX$GLUCOSE_DayX_ImpA[is.na(GLUCOSE_DayX$GLUCOSE_DayX_ImpA)] <- floor(mean1)
GLUCOSE_DayX <- GLUCOSE_DayX%>%
  mutate(GLUCOSE_DayX_Orig_NORMAL = case_when( is.na(GLUCOSE_DayX_Orig) ~ 'Unknown',
                                               GLUCOSE_DayX_Orig>=lowr & GLUCOSE_DayX_Orig<=highr ~ "YES", 
                                               TRUE ~ "NO"),
         GLUCOSE_DayX_ImpN_NORMAL = case_when( is.na(GLUCOSE_DayX_ImpN) ~ 'Unknown',
                                               GLUCOSE_DayX_ImpN>=lowr & GLUCOSE_DayX_ImpN<=highr ~ "YES", 
                                               TRUE ~ "NO"),
         GLUCOSE_DayX_ImpA_NORMAL = case_when( is.na(GLUCOSE_DayX_ImpA) ~ 'Unknown',
                                               GLUCOSE_DayX_ImpA>=lowr & GLUCOSE_DayX_ImpA<=highr ~ "YES", 
                                               TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, GLUCOSE_DayX_Orig, GLUCOSE_DayX_Orig_NORMAL, GLUCOSE_DayX_ImpN, GLUCOSE_DayX_ImpA, 
         GLUCOSE_DayX_ImpN_NORMAL, GLUCOSE_DayX_ImpA_NORMAL, GLUCOSE_DayX_Missing)






#########################
#          PCO2         #
#########################

PCO2 <- Order_Results[grep("PCO2", Order_Results$NAME), ]%>%
  filter(REFERENCE_LOW == 35 & REFERENCE_HIGH == 45 )%>%
  arrange(HSP_ACCOUNT_ID, NAME, RESULT_TIME)%>%
  distinct(HSP_ACCOUNT_ID, NAME, RESULT_TIME, .keep_all=TRUE)%>%
  mutate(PCO2_NORMAL = case_when( ORD_NUM_VALUE>=REFERENCE_LOW & ORD_NUM_VALUE<=REFERENCE_HIGH ~ "YES", TRUE ~ "NO"),
         PCO2 = ORD_NUM_VALUE  )%>%
  select(HSP_ACCOUNT_ID, RESULT_TIME, PCO2_NORMAL, PCO2, REFERENCE_LOW, REFERENCE_HIGH)
# check1 <- as.data.frame(freq(PCO2$PCO2)) # Check the outliers
PCO2 <- PCO2%>%filter( !is.na(PCO2) & PCO2 > 0 & PCO2 < 120 ) # Remove the outliers

lowr <- floor(mean(PCO2$REFERENCE_LOW, na.rm=TRUE))
highr <- ceiling(mean(PCO2$REFERENCE_HIGH, na.rm=TRUE))
mean1 <- floor(mean(PCO2$PCO2[PCO2$PCO2>=lowr & PCO2$PCO2<=highr], na.rm=TRUE))
sd1 <- sd(PCO2$PCO2[PCO2$PCO2>=lowr & PCO2$PCO2<=highr], na.rm=TRUE)

PCO2_Day0 <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6')], PCO2, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ADM_6 )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
PCO2_Day0 <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), PCO2_Day0, by = "HSP_ACCOUNT_ID")%>%
  mutate(PCO2_Day0_Missing = case_when(is.na(PCO2) ~ 'YES', TRUE ~ 'NO'),
         PCO2_Day0_Orig = PCO2,
         PCO2_Day0_ImpN = PCO2,
         PCO2_Day0_ImpA = PCO2)
PCO2_Day0$PCO2_Day0_ImpN[is.na(PCO2_Day0$PCO2_Day0_ImpN)] <- sample(floor(rnorm(180000,mean1,sd1)), size=sum(is.na(PCO2_Day0$PCO2_Day0_ImpN)), replace=T)
PCO2_Day0$PCO2_Day0_ImpA[is.na(PCO2_Day0$PCO2_Day0_ImpA)] <- floor(mean1)
PCO2_Day0 <- PCO2_Day0%>%
  mutate(PCO2_Day0_Orig_NORMAL = case_when( is.na(PCO2_Day0_Orig) ~ 'Unknown',
                                            PCO2_Day0_Orig>=lowr & PCO2_Day0_Orig<=highr ~ "YES", 
                                            TRUE ~ "NO"),
         PCO2_Day0_ImpN_NORMAL = case_when( is.na(PCO2_Day0_ImpN) ~ 'Unknown',
                                            PCO2_Day0_ImpN>=lowr & PCO2_Day0_ImpN<=highr ~ "YES", 
                                            TRUE ~ "NO"),
         PCO2_Day0_ImpA_NORMAL = case_when( is.na(PCO2_Day0_ImpA) ~ 'Unknown',
                                            PCO2_Day0_ImpA>=lowr & PCO2_Day0_ImpA<=highr ~ "YES", 
                                            TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, PCO2_Day0_Orig, PCO2_Day0_Orig_NORMAL, PCO2_Day0_ImpN, PCO2_Day0_ImpA, 
         PCO2_Day0_ImpN_NORMAL, PCO2_Day0_ImpA_NORMAL, PCO2_Day0_Missing)

PCO2_DayX <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6','INP_ABX_DTM')], PCO2, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ABX_DTM | is.na(INP_ABX_DTM) )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
PCO2_DayX <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), PCO2_DayX, by = "HSP_ACCOUNT_ID")%>%
  mutate(PCO2_DayX_Missing = case_when(is.na(PCO2) ~ 'YES', TRUE ~ 'NO'),
         PCO2_DayX_Orig = PCO2,
         PCO2_DayX_ImpN = PCO2,
         PCO2_DayX_ImpA = PCO2)
PCO2_DayX$PCO2_DayX_ImpN[is.na(PCO2_DayX$PCO2_DayX_ImpN)] <- sample(floor(rnorm(180000,mean1,sd1)), size=sum(is.na(PCO2_DayX$PCO2_DayX_ImpN)), replace=T)
PCO2_DayX$PCO2_DayX_ImpA[is.na(PCO2_DayX$PCO2_DayX_ImpA)] <- floor(mean1)
PCO2_DayX <- PCO2_DayX%>%
  mutate(PCO2_DayX_Orig_NORMAL = case_when( is.na(PCO2_DayX_Orig) ~ 'Unknown',
                                            PCO2_DayX_Orig>=lowr & PCO2_DayX_Orig<=highr ~ "YES", 
                                            TRUE ~ "NO"),
         PCO2_DayX_ImpN_NORMAL = case_when( is.na(PCO2_DayX_ImpN) ~ 'Unknown',
                                            PCO2_DayX_ImpN>=lowr & PCO2_DayX_ImpN<=highr ~ "YES", 
                                            TRUE ~ "NO"),
         PCO2_DayX_ImpA_NORMAL = case_when( is.na(PCO2_DayX_ImpA) ~ 'Unknown',
                                            PCO2_DayX_ImpA>=lowr & PCO2_DayX_ImpA<=highr ~ "YES", 
                                            TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, PCO2_DayX_Orig, PCO2_DayX_Orig_NORMAL, PCO2_DayX_ImpN, PCO2_DayX_ImpA, 
         PCO2_DayX_ImpN_NORMAL, PCO2_DayX_ImpA_NORMAL, PCO2_DayX_Missing)






#########################
#       PLATELET        #
#########################

PLATELET <- Order_Results[grep("PLATELET", Order_Results$NAME), ]%>%
  filter(REFERENCE_LOW == 160 & REFERENCE_HIGH == 383 )%>%
  arrange(HSP_ACCOUNT_ID, NAME, RESULT_TIME)%>%
  distinct(HSP_ACCOUNT_ID, NAME, RESULT_TIME, .keep_all=TRUE)%>%
  mutate(PLATELET_NORMAL = case_when( ORD_NUM_VALUE>=REFERENCE_LOW & ORD_NUM_VALUE<=REFERENCE_HIGH ~ "YES", TRUE ~ "NO"),
         PLATELET = ORD_NUM_VALUE  )%>%
  select(HSP_ACCOUNT_ID, RESULT_TIME, PLATELET_NORMAL, PLATELET, REFERENCE_LOW, REFERENCE_HIGH)
# check1 <- as.data.frame(freq(PLATELET$PLATELET)) # Check the outliers
PLATELET <- PLATELET%>%filter( !is.na(PLATELET) & PLATELET >= 5 & PLATELET <= 2300 ) # Remove the outliers

lowr <- floor(mean(PLATELET$REFERENCE_LOW, na.rm=TRUE))
highr <- ceiling(mean(PLATELET$REFERENCE_HIGH, na.rm=TRUE))
mean1 <- floor(mean(PLATELET$PLATELET[PLATELET$PLATELET>=lowr & PLATELET$PLATELET<=highr], na.rm=TRUE))
sd1 <- sd(PLATELET$PLATELET[PLATELET$PLATELET>=lowr & PLATELET$PLATELET<=highr], na.rm=TRUE)

PLATELET_Day0 <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6')], PLATELET, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ADM_6 )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
PLATELET_Day0 <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), PLATELET_Day0, by = "HSP_ACCOUNT_ID")%>%
  mutate(PLATELET_Day0_Missing = case_when(is.na(PLATELET) ~ 'YES', TRUE ~ 'NO'),
         PLATELET_Day0_Orig = PLATELET,
         PLATELET_Day0_ImpN = PLATELET,
         PLATELET_Day0_ImpA = PLATELET)
PLATELET_Day0$PLATELET_Day0_ImpN[is.na(PLATELET_Day0$PLATELET_Day0_ImpN)] <- sample(floor(rnorm(180000,mean1,sd1)), size=sum(is.na(PLATELET_Day0$PLATELET_Day0_ImpN)), replace=T)
PLATELET_Day0$PLATELET_Day0_ImpA[is.na(PLATELET_Day0$PLATELET_Day0_ImpA)] <- floor(mean1)
PLATELET_Day0 <- PLATELET_Day0%>%
  mutate(PLATELET_Day0_Orig_NORMAL = case_when( is.na(PLATELET_Day0_Orig) ~ 'Unknown',
                                                PLATELET_Day0_Orig>=lowr & PLATELET_Day0_Orig<=highr ~ "YES", 
                                                TRUE ~ "NO"),
         PLATELET_Day0_ImpN_NORMAL = case_when( is.na(PLATELET_Day0_ImpN) ~ 'Unknown',
                                                PLATELET_Day0_ImpN>=lowr & PLATELET_Day0_ImpN<=highr ~ "YES", 
                                                TRUE ~ "NO"),
         PLATELET_Day0_ImpA_NORMAL = case_when( is.na(PLATELET_Day0_ImpA) ~ 'Unknown',
                                                PLATELET_Day0_ImpA>=lowr & PLATELET_Day0_ImpA<=highr ~ "YES", 
                                                TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, PLATELET_Day0_Orig, PLATELET_Day0_Orig_NORMAL, PLATELET_Day0_ImpN, PLATELET_Day0_ImpA, 
         PLATELET_Day0_ImpN_NORMAL, PLATELET_Day0_ImpA_NORMAL, PLATELET_Day0_Missing)

PLATELET_DayX <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6','INP_ABX_DTM')], PLATELET, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ABX_DTM | is.na(INP_ABX_DTM) )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
PLATELET_DayX <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), PLATELET_DayX, by = "HSP_ACCOUNT_ID")%>%
  mutate(PLATELET_DayX_Missing = case_when(is.na(PLATELET) ~ 'YES', TRUE ~ 'NO'),
         PLATELET_DayX_Orig = PLATELET,
         PLATELET_DayX_ImpN = PLATELET,
         PLATELET_DayX_ImpA = PLATELET)
PLATELET_DayX$PLATELET_DayX_ImpN[is.na(PLATELET_DayX$PLATELET_DayX_ImpN)] <- sample(floor(rnorm(180000,mean1,sd1)), size=sum(is.na(PLATELET_DayX$PLATELET_DayX_ImpN)), replace=T)
PLATELET_DayX$PLATELET_DayX_ImpA[is.na(PLATELET_DayX$PLATELET_DayX_ImpA)] <- floor(mean1)
PLATELET_DayX <- PLATELET_DayX%>%
  mutate(PLATELET_DayX_Orig_NORMAL = case_when( is.na(PLATELET_DayX_Orig) ~ 'Unknown',
                                                PLATELET_DayX_Orig>=lowr & PLATELET_DayX_Orig<=highr ~ "YES", 
                                                TRUE ~ "NO"),
         PLATELET_DayX_ImpN_NORMAL = case_when( is.na(PLATELET_DayX_ImpN) ~ 'Unknown',
                                                PLATELET_DayX_ImpN>=lowr & PLATELET_DayX_ImpN<=highr ~ "YES", 
                                                TRUE ~ "NO"),
         PLATELET_DayX_ImpA_NORMAL = case_when( is.na(PLATELET_DayX_ImpA) ~ 'Unknown',
                                                PLATELET_DayX_ImpA>=lowr & PLATELET_DayX_ImpA<=highr ~ "YES", 
                                                TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, PLATELET_DayX_Orig, PLATELET_DayX_Orig_NORMAL, PLATELET_DayX_ImpN, PLATELET_DayX_ImpA, 
         PLATELET_DayX_ImpN_NORMAL, PLATELET_DayX_ImpA_NORMAL, PLATELET_DayX_Missing)




#########################
#         WBCUA         #
#########################

WBCUA <- Order_Results[grep("WBC, UA", Order_Results$NAME), ]%>%
  filter(NAME == "WBC, UA")%>%
  arrange(HSP_ACCOUNT_ID, NAME, RESULT_TIME)%>%
  distinct(HSP_ACCOUNT_ID, NAME, RESULT_TIME, .keep_all=TRUE)%>%
  mutate(WBCUA_NORMAL = case_when( ORD_NUM_VALUE>=REFERENCE_LOW & ORD_NUM_VALUE<=REFERENCE_HIGH ~ "YES", TRUE ~ "NO"),
         WBCUA = ORD_NUM_VALUE )%>%
  select(HSP_ACCOUNT_ID, RESULT_TIME, WBCUA_NORMAL, WBCUA, REFERENCE_LOW, REFERENCE_HIGH)
# check1 <- as.data.frame(freq(WBCUA$WBCUA)) # Check the outliers
WBCUA <- WBCUA%>%filter( !is.na(WBCUA) & WBCUA > 0 & WBCUA <= 1000 ) # Remove the outliers

lowr <- floor(mean(WBCUA$REFERENCE_LOW, na.rm=TRUE))
highr <- ceiling(mean(WBCUA$REFERENCE_HIGH, na.rm=TRUE))
mean1 <- floor(mean(WBCUA$WBCUA[WBCUA$WBCUA>=lowr & WBCUA$WBCUA<=highr], na.rm=TRUE))
sd1 <- sd(WBCUA$WBCUA[WBCUA$WBCUA>=lowr & WBCUA$WBCUA<=highr], na.rm=TRUE)

WBCUA_Day0 <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6')], WBCUA, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ADM_6 )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
WBCUA_Day0 <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), WBCUA_Day0, by = "HSP_ACCOUNT_ID")%>%
  mutate(WBCUA_Day0_Missing = case_when(is.na(WBCUA) ~ 'YES', TRUE ~ 'NO'),
         WBCUA_Day0_Orig = WBCUA,
         WBCUA_Day0_ImpN = WBCUA,
         WBCUA_Day0_ImpA = WBCUA)
WBCUA_Day0$WBCUA_Day0_ImpN[is.na(WBCUA_Day0$WBCUA_Day0_ImpN)] <- sample(floor(rnorm(180000,mean1,sd1)), size=sum(is.na(WBCUA_Day0$WBCUA_Day0_ImpN)), replace=T)
WBCUA_Day0$WBCUA_Day0_ImpA[is.na(WBCUA_Day0$WBCUA_Day0_ImpA)] <- floor(mean1)
WBCUA_Day0 <- WBCUA_Day0%>%
  mutate(WBCUA_Day0_Orig_NORMAL = case_when( is.na(WBCUA_Day0_Orig) ~ 'Unknown',
                                             WBCUA_Day0_Orig>=lowr & WBCUA_Day0_Orig<=highr ~ "YES", 
                                             TRUE ~ "NO"),
         WBCUA_Day0_ImpN_NORMAL = case_when( is.na(WBCUA_Day0_ImpN) ~ 'Unknown',
                                             WBCUA_Day0_ImpN>=lowr & WBCUA_Day0_ImpN<=highr ~ "YES", 
                                             TRUE ~ "NO"),
         WBCUA_Day0_ImpA_NORMAL = case_when( is.na(WBCUA_Day0_ImpA) ~ 'Unknown',
                                             WBCUA_Day0_ImpA>=lowr & WBCUA_Day0_ImpA<=highr ~ "YES", 
                                             TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, WBCUA_Day0_Orig, WBCUA_Day0_Orig_NORMAL, WBCUA_Day0_ImpN, WBCUA_Day0_ImpA, 
         WBCUA_Day0_ImpN_NORMAL, WBCUA_Day0_ImpA_NORMAL, WBCUA_Day0_Missing)

WBCUA_DayX <- left_join(INP_ADM_DTM_I10[,c('HSP_ACCOUNT_ID','INP_ADM_6','INP_ABX_DTM')], WBCUA, by = "HSP_ACCOUNT_ID")%>%
  filter( RESULT_TIME <= INP_ABX_DTM | is.na(INP_ABX_DTM) )%>%
  arrange(HSP_ACCOUNT_ID, desc(RESULT_TIME) )%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
WBCUA_DayX <- left_join(INP_ADM_DTM_I10%>%select(HSP_ACCOUNT_ID), WBCUA_DayX, by = "HSP_ACCOUNT_ID")%>%
  mutate(WBCUA_DayX_Missing = case_when(is.na(WBCUA) ~ 'YES', TRUE ~ 'NO'),
         WBCUA_DayX_Orig = WBCUA,
         WBCUA_DayX_ImpN = WBCUA,
         WBCUA_DayX_ImpA = WBCUA)
WBCUA_DayX$WBCUA_DayX_ImpN[is.na(WBCUA_DayX$WBCUA_DayX_ImpN)] <- sample(floor(rnorm(180000,mean1,sd1)), size=sum(is.na(WBCUA_DayX$WBCUA_DayX_ImpN)), replace=T)
WBCUA_DayX$WBCUA_DayX_ImpA[is.na(WBCUA_DayX$WBCUA_DayX_ImpA)] <- floor(mean1)
WBCUA_DayX <- WBCUA_DayX%>%
  mutate(WBCUA_DayX_Orig_NORMAL = case_when( is.na(WBCUA_DayX_Orig) ~ 'Unknown',
                                             WBCUA_DayX_Orig>=lowr & WBCUA_DayX_Orig<=highr ~ "YES", 
                                             TRUE ~ "NO"),
         WBCUA_DayX_ImpN_NORMAL = case_when( is.na(WBCUA_DayX_ImpN) ~ 'Unknown',
                                             WBCUA_DayX_ImpN>=lowr & WBCUA_DayX_ImpN<=highr ~ "YES", 
                                             TRUE ~ "NO"),
         WBCUA_DayX_ImpA_NORMAL = case_when( is.na(WBCUA_DayX_ImpA) ~ 'Unknown',
                                             WBCUA_DayX_ImpA>=lowr & WBCUA_DayX_ImpA<=highr ~ "YES", 
                                             TRUE ~ "NO"))%>%
  select(HSP_ACCOUNT_ID, WBCUA_DayX_Orig, WBCUA_DayX_Orig_NORMAL, WBCUA_DayX_ImpN, WBCUA_DayX_ImpA, 
         WBCUA_DayX_ImpN_NORMAL, WBCUA_DayX_ImpA_NORMAL, WBCUA_DayX_Missing)



# OR_list <- c('ALBUMIN', 'PTT',	'BILIRUBIN',	'SODIUM',	'CHLORIDE',	'POTASSIUM',	'CRP',	'CREATININE',	'GLUCOSE',	
#              'HEMOGLOBIN',	'MCH',	'MCHC',	'INR',	'LACTATE',	'LYMPHOCYTES',	'NEUTROPHIL',	
#              'PCO2',	'PLATELET',	'MPV',	'WBCUA', 'WBC')


OR_ALL <- left_join(ALBUMIN_Day0, PTT_Day0, by = "HSP_ACCOUNT_ID")%>%
  left_join(., BILIRUBIN_Day0 , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., SODIUM_Day0 , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., CHLORIDE_Day0 , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., POTASSIUM_Day0 , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., CRP_Day0 , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., CREATININE_Day0 , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., GLUCOSE_Day0 , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., HEMOGLOBIN_Day0 , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., MCH_Day0 , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., MCHC_Day0 , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., INR_Day0 , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., LACTATE_Day0 , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., LYMPHOCYTES_Day0 , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., NEUTROPHIL_Day0 , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., PCO2_Day0 , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., PLATELET_Day0 , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., MPV_Day0 , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., WBCUA_Day0 , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., WBC_Day0 , by = 'HSP_ACCOUNT_ID')%>%
  
  left_join(., ALBUMIN_DayX, by = "HSP_ACCOUNT_ID")%>%
  left_join(., PTT_DayX, by = "HSP_ACCOUNT_ID")%>%
  left_join(., BILIRUBIN_DayX , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., SODIUM_DayX , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., CHLORIDE_DayX , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., POTASSIUM_DayX , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., CRP_DayX , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., CREATININE_DayX , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., GLUCOSE_DayX , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., HEMOGLOBIN_DayX , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., MCH_DayX , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., MCHC_DayX , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., INR_DayX , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., LACTATE_DayX , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., LYMPHOCYTES_DayX , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., NEUTROPHIL_DayX , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., PCO2_DayX , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., PLATELET_DayX , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., MPV_DayX , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., WBCUA_DayX , by = 'HSP_ACCOUNT_ID')%>%
  left_join(., WBC_DayX , by = 'HSP_ACCOUNT_ID')


save(OR_ALL,file = "OR_ALL.RData")


rm(Order_Results)
rm(list=ls(pattern="ALBUMIN"))
rm(list=ls(pattern="PTT"))
rm(list=ls(pattern="BILIRUBIN"))
rm(list=ls(pattern="SODIUM"))
rm(list=ls(pattern="CHLORIDE"))
rm(list=ls(pattern="POTASSIUM"))
rm(list=ls(pattern="CRP"))
rm(list=ls(pattern="CREATININE"))
rm(list=ls(pattern="GLUCOSE"))
rm(list=ls(pattern="HEMOGLOBIN"))
rm(list=ls(pattern="MCH"))
rm(list=ls(pattern="MCHC"))
rm(list=ls(pattern="INR"))
rm(list=ls(pattern="LACTATE"))
rm(list=ls(pattern="LYMPHOCYTES"))
rm(list=ls(pattern="NEUTROPHIL"))
rm(list=ls(pattern="PCO2"))
rm(list=ls(pattern="PLATELET"))
rm(list=ls(pattern="MPV"))
rm(list=ls(pattern="WBCUA"))
rm(list=ls(pattern="WBC"))

