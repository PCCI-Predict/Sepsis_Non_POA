
### PURPOSE:  The purpose of this program is to train and test the inpatient sepsis model
### AUTHOR:   Jet Wang ###
### DATE:     11/01/2017 ###

load("SEPSIS_FINAL.RData")

LASSO_DayX <- SEPSIS_FINAL%>%
  filter(SUSP != 1)%>% #Remove those patients who have been suspected
  select(ACTUAL, 
         
         ADM_TYPE_NAME	,
         PAYOR	,
         MARITAL_STATUS_C	,
         GENDER	,
         AGE	,
         
         Last_Visit_Type	,
         M6_ED_Visits	,
         M12_ED_Visits	,
         Y2_ED_Visits ,
         Y1_Sepsis_Visits	,
         Y2_Sepsis_Visits	,
    
         
         SBP_DayX_AVG	,
         SBP_NORMAL_AVG_DayX	,
         DBP_DayX_AVG	,
         DBP_NORMAL_AVG_DayX	,
         TEMP_DayX_AVG	,
         TEMP_NORMAL_AVG_DayX	,
         PULSE_DayX_AVG	,
         PULSE_NORMAL_AVG_DayX	,
         RESP_DayX_AVG	,
         RESP_NORMAL_AVG_DayX	,
         O2SAT_DayX_AVG	,
         O2SAT_NORMAL_AVG_DayX	,

         
         ALBUMIN_DayX_ImpA	,
         ALBUMIN_DayX_ImpA_NORMAL	,
         #	         ALBUMIN_DayX_Missing	,
         PTT_DayX_ImpA	,
         PTT_DayX_ImpA_NORMAL	,
         #	         PTT_DayX_Missing	,
         BILIRUBIN_DayX_ImpA	,
         BILIRUBIN_DayX_ImpA_NORMAL	,
         #	         BILIRUBIN_DayX_Missing	,
         SODIUM_DayX_ImpA	,
         SODIUM_DayX_ImpA_NORMAL	,
         #	         SODIUM_DayX_Missing	,
         CHLORIDE_DayX_ImpA	,
         CHLORIDE_DayX_ImpA_NORMAL	,
         #	         CHLORIDE_DayX_Missing	,
         POTASSIUM_DayX_ImpA	,
         POTASSIUM_DayX_ImpA_NORMAL	,
         #	         POTASSIUM_DayX_Missing	,
         CRP_DayX_ImpA	,
         CRP_DayX_ImpA_NORMAL	,
         #	         CRP_DayX_Missing	,
         CREATININE_DayX_ImpA	,
         CREATININE_DayX_ImpA_NORMAL	,
         #	         CREATININE_DayX_Missing	,
         GLUCOSE_DayX_ImpA	,
         GLUCOSE_DayX_ImpA_NORMAL	,
         #	         GLUCOSE_DayX_Missing	,
         HEMOGLOBIN_DayX_ImpA	,
         HEMOGLOBIN_DayX_ImpA_NORMAL	,
         #	         HEMOGLOBIN_DayX_Missing	,
         MCH_DayX_ImpA	,
         MCH_DayX_ImpA_NORMAL	,
         #	         MCH_DayX_Missing	,
         MCHC_DayX_ImpA	,
         MCHC_DayX_ImpA_NORMAL	,
         #	         MCHC_DayX_Missing	,
         INR_DayX_ImpA	,
         INR_DayX_ImpA_NORMAL	,
         #	         INR_DayX_Missing	,
         LACTATE_DayX_ImpA	,
         LACTATE_DayX_ImpA_NORMAL	,
         #	         LACTATE_DayX_Missing	,
         LYMPHOCYTES_DayX_ImpA	,
         LYMPHOCYTES_DayX_ImpA_NORMAL	,
         #	         LYMPHOCYTES_DayX_Missing	,
         NEUTROPHIL_DayX_ImpA	,
         NEUTROPHIL_DayX_ImpA_NORMAL	,
         #	         NEUTROPHIL_DayX_Missing	,
         PCO2_DayX_ImpA	,
         PCO2_DayX_ImpA_NORMAL	,
         #	         PCO2_DayX_Missing	,
         PLATELET_DayX_ImpA	,
         PLATELET_DayX_ImpA_NORMAL	,
         #	         PLATELET_DayX_Missing	,
         MPV_DayX_ImpA	,
         MPV_DayX_ImpA_NORMAL	,
         #	         MPV_DayX_Missing	,
         WBCUA_DayX_ImpA	,
         WBCUA_DayX_ImpA_NORMAL	,
         #	         WBCUA_DayX_Missing	,
         WBC_DayX_ImpA	,
         WBC_DayX_ImpA_NORMAL	,
         #	         WBC_DayX_Missing	,
         
         CHF_L	,
         VALVE_L	,
         PULMCIRC_L	,
         PERIVASC_L	,
         HTN_L	,
         HTNCX_L	,
         PARA_L	,
         NEURO_L	,
         CHRNLUNG_L	,
         DM_L	,
         DMCX_L	,
         HYPOTHY_L	,
         RENLFAIL_L	,
         LIVER_L	,
         ULCER_L	,
         AIDS_L	,
         LYMPH_L	,
         METS_L	,
         TUMOR_L	,
         ARTH_L	,
         COAG_L	,
         OBESE_L	,
         WGHTLOSS_L	,
         LYTES_L	,
         BLDLOSS_L	,
         ANEMDEF_L	,
         ALCOHOL_L	,
         DRUG_L	,
         PSYCH_L	,
         DEPRESS_L 
         
         # IV_ABX_Day0_COUNT

  )


set.seed(123456)
LASSO_Training_DayX <- sample_frac(LASSO_DayX, 0.7)
sid<-as.numeric(rownames(LASSO_Training_DayX))
LASSO_TESTING_DayX<-LASSO_DayX[-sid,]

freq(LASSO_Training_DayX$ACTUAL)
freq(LASSO_TESTING_DayX$ACTUAL)


formula <- as.formula(ACTUAL ~ .)
X <- model.matrix(formula, LASSO_Training_DayX) # The predictors
Y <- as.double(as.matrix(LASSO_Training_DayX[, 1])) # Only class

# Fitting the model (Lasso: Alpha = 1)
LASSO_Model_DayX <- cv.glmnet(X, Y, family='binomial', alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc') #Use AUC as the parameter

coef(LASSO_Model_DayX) #Review the coefficients

# best.lambda <- LASSO_Model_DayX$lambda.min #Get the best lambda
# 
# formula <- as.formula(ACTUAL ~ .)
# Xnew <- model.matrix(formula, LASSO_TESTING_DayX)
# LASSO_PRED <-predict(LASSO_Model_DayX, s=best.lambda, newx=Xnew, type="response")
# 
# LASSO_DayX_Eval <- cbind(LASSO_TESTING_DayX, LASSO_PRED)
# LASSO_DayX_Eval <- LASSO_DayX_Eval[,c(1,ncol(LASSO_DayX_Eval))]
# colnames(LASSO_DayX_Eval) <- c('ACTUAL','PROB')
# LASSO_DayX_Eval$RowID <- as.numeric(rownames(LASSO_DayX_Eval))
# 
# cutoff <- seq(0.01, 0.99, by=0.001)
# datalist = list()
# for (i in 1:length(cutoff)) {
#   testing1 <- LASSO_DayX_Eval%>%
#     mutate(PREDICT = case_when(PROB > cutoff[i] ~ 'YES', TRUE ~ 'NO'))
#   CUTOFF <- cutoff[i]
#   PPV <- nrow(testing1[which(testing1$ACTUAL==1 & testing1$PREDICT=='YES'),])/nrow(testing1[which(testing1$PREDICT=='YES'),])
#   SEN <- nrow(testing1[which(testing1$ACTUAL==1 & testing1$PREDICT=='YES'),])/nrow(testing1[which(testing1$ACTUAL==1),])
#   datalist[[i]] <- cbind(cutoff[i], PPV, SEN)
# }
# CF_Table = as.data.frame(do.call(rbind, datalist))%>%rename(CutOffProb=V1)
# 
# ggplot(CF_Table, aes(CutOffProb)) + 
#   geom_line(aes(y = PPV, colour = "PPV")) + 
#   geom_line(aes(y = SEN, colour = "SEN")) + ylab("")





### Extract the variables from Lasso and apply them on traditional Logit Regression ###

Logit_Model1 <- glm(formula = ACTUAL ~ 
              # Y2_Sepsis_Visits	+
              SBP_DayX_AVG	+
              DBP_DayX_AVG	+ I(DBP_DayX_AVG*DBP_DayX_AVG) +
              TEMP_DayX_AVG	+ 
              TEMP_NORMAL_AVG_DayX	+
              PULSE_DayX_AVG	+ #I(PULSE_DayX_AVG*PULSE_DayX_AVG) +
              # RESP_DayX_AVG	+
              RESP_NORMAL_AVG_DayX	+
              O2SAT_DayX_AVG	+
                 # O2SAT_NORMAL_AVG_DayX	+
              ALBUMIN_DayX_ImpA	+
              SODIUM_DayX_ImpA_NORMAL	+
              POTASSIUM_DayX_ImpA_NORMAL	+
                 # CRP_DayX_ImpA	+
              CRP_DayX_ImpA_NORMAL	+
              CREATININE_DayX_ImpA	+
                 # LACTATE_DayX_ImpA	+
                 LACTATE_DayX_ImpA_NORMAL	+
                 # LYMPHOCYTES_DayX_ImpA	+
              NEUTROPHIL_DayX_ImpA	+
                 # NEUTROPHIL_DayX_ImpA_NORMAL	+
              # PCO2_DayX_ImpA_NORMAL	+
                 PLATELET_DayX_ImpA	+
              # PLATELET_DayX_ImpA_NORMAL	+
              # WBCUA_DayX_ImpA	+
              # WBCUA_DayX_ImpA_NORMAL	+
              WBC_DayX_ImpA_NORMAL	#+
              # AIDS_L	
           
	,
    data=LASSO_Training_DayX, family=binomial)
summary(Logit_Model1)


### Get the probability table in the training dataset
### Then determine the optimal cutoff point
Logit_Model1_Training_Result <- as.data.frame(cbind( LASSO_Training_DayX[,"ACTUAL"],predict(Logit_Model1, LASSO_Training_DayX, type="response") ))%>%
  rename(ACTUAL = V1,
         DayXPROB = V2)
cutoff <- seq(0.01, 0.99, by=0.001)
datalist = list()
for (i in 1:length(cutoff)) {
  testing1 <- Logit_Model1_Training_Result%>%
    mutate(PREDICT = case_when(DayXPROB > cutoff[i] ~ 'YES', TRUE ~ 'NO'))
  CUTOFF <- cutoff[i]
  PPV <- nrow(testing1[which(testing1$ACTUAL==1 & testing1$PREDICT=='YES'),])/nrow(testing1[which(testing1$PREDICT=='YES'),])
  SEN <- nrow(testing1[which(testing1$ACTUAL==1 & testing1$PREDICT=='YES'),])/nrow(testing1[which(testing1$ACTUAL==1),])
  datalist[[i]] <- cbind(cutoff[i], PPV, SEN)}
CF_Table = as.data.frame(do.call(rbind, datalist))%>%rename(CutOffProb=V1)
### Optimal prob cutoff = 0.081


Logit_Model1_Testing_Result <- as.data.frame(cbind( LASSO_TESTING_DayX[,"ACTUAL"],predict(Logit_Model1, LASSO_TESTING_DayX, type="response") ))%>%
  rename(ACTUAL = V1,
         DayXPROB = V2)
# cutoff <- seq(0.01, 0.99, by=0.001)
# datalist = list()
# for (i in 1:length(cutoff)) {
#   testing1 <- Logit_Model1_Testing_Result%>%
#     mutate(PREDICT = case_when(DayXPROB > cutoff[i] ~ 'YES', TRUE ~ 'NO'))
#   CUTOFF <- cutoff[i]
#   PPV <- nrow(testing1[which(testing1$ACTUAL==1 & testing1$PREDICT=='YES'),])/nrow(testing1[which(testing1$PREDICT=='YES'),])
#   SEN <- nrow(testing1[which(testing1$ACTUAL==1 & testing1$PREDICT=='YES'),])/nrow(testing1[which(testing1$ACTUAL==1),])
#   datalist[[i]] <- cbind(cutoff[i], PPV, SEN)}
# CF_Table = as.data.frame(do.call(rbind, datalist))%>%rename(CutOffProb=V1)

### Apply the prob cutoff 0.089 on the testing dataset
### Get the final PPV and Sensitivity
CM <- Logit_Model1_Testing_Result%>%
  mutate(PREDICT = case_when(DayXPROB > 0.081 ~ 'YES', TRUE ~ 'NO'))
CrossTable(CM$PREDICT, CM$ACTUAL, prop.chisq=FALSE, chisq = FALSE, format = 'SAS')

# ===================================
#               CM$ACTUAL
# CM$PREDICT        0       1   Total
# -----------------------------------
# NO             8942     116    9058
#               0.987   0.013   0.947
#               0.960   0.458        
#               0.935   0.012        
# -----------------------------------
# YES             371     137     508
#               0.730   0.270   0.053
#               0.040   0.542        
#               0.039   0.014        
# -----------------------------------
# Total          9313     253    9566
#               0.974   0.026        
# ===================================

### PPV = 0.27
### SEN = 0.54
### NPV = 0.99
### SPE = 0.96
### ACC = 95%


