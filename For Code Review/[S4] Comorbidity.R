
### PURPOSE:  The purpose of this program is to create comorbidity variables 
### AUTHOR:   Jet Wang ###
### DATE:     09/01/2017 ###



### Import MS-DRG information
Comorb_DRG <- read.csv(file='T:/Sepsis Non-POA/Files/Comorb_Diag_Code.csv', header=TRUE)

CHF <- as.character(Comorb_DRG$CHF)[nchar(as.character(Comorb_DRG$CHF))>1]
CHF <- trimws(CHF, which = c("both"))

VALVE <- as.character(Comorb_DRG$VALVE)[nchar(as.character(Comorb_DRG$VALVE))>1]
VALVE <- trimws(VALVE, which = c("both"))

PULMCIRC <- as.character(Comorb_DRG$PULMCIRC)[nchar(as.character(Comorb_DRG$PULMCIRC))>1]
PULMCIRC <- trimws(PULMCIRC, which = c("both"))

PERIVASC <- as.character(Comorb_DRG$PERIVASC)[nchar(as.character(Comorb_DRG$PERIVASC))>1]
PERIVASC <- trimws(PERIVASC, which = c("both"))

HTN <- as.character(Comorb_DRG$HTN)[nchar(as.character(Comorb_DRG$HTN))>1]
HTN <- trimws(HTN, which = c("both"))

HTNCX <- as.character(Comorb_DRG$HTNCX)[nchar(as.character(Comorb_DRG$HTNCX))>1]
HTNCX <- trimws(HTNCX, which = c("both"))

PARA <- as.character(Comorb_DRG$PARA)[nchar(as.character(Comorb_DRG$PARA))>1]
PARA <- trimws(PARA, which = c("both"))

NEURO <- as.character(Comorb_DRG$NEURO)[nchar(as.character(Comorb_DRG$NEURO))>1]
NEURO <- trimws(NEURO, which = c("both"))

CHRNLUNG <- as.character(Comorb_DRG$CHRNLUNG)[nchar(as.character(Comorb_DRG$CHRNLUNG))>1]
CHRNLUNG <- trimws(CHRNLUNG, which = c("both"))

DM <- as.character(Comorb_DRG$DM)[nchar(as.character(Comorb_DRG$DM))>1]
DM <- trimws(DM, which = c("both"))

DMCX <- as.character(Comorb_DRG$DMCX)[nchar(as.character(Comorb_DRG$DMCX))>1]
DMCX <- trimws(DMCX, which = c("both"))

HYPOTHY <- as.character(Comorb_DRG$HYPOTHY)[nchar(as.character(Comorb_DRG$HYPOTHY))>1]
HYPOTHY <- trimws(HYPOTHY, which = c("both"))

RENLFAIL <- as.character(Comorb_DRG$RENLFAIL)[nchar(as.character(Comorb_DRG$RENLFAIL))>1]
RENLFAIL <- trimws(RENLFAIL, which = c("both"))

LIVER <- as.character(Comorb_DRG$LIVER)[nchar(as.character(Comorb_DRG$LIVER))>1]
LIVER <- trimws(LIVER, which = c("both"))

ULCER <- as.character(Comorb_DRG$ULCER)[nchar(as.character(Comorb_DRG$ULCER))>1]
ULCER <- trimws(ULCER, which = c("both"))

AIDS <- as.character(Comorb_DRG$AIDS)[nchar(as.character(Comorb_DRG$AIDS))>1]
AIDS <- trimws(AIDS, which = c("both"))

LYMPH <- as.character(Comorb_DRG$LYMPH)[nchar(as.character(Comorb_DRG$LYMPH))>1]
LYMPH <- trimws(LYMPH, which = c("both"))

METS <- as.character(Comorb_DRG$METS)[nchar(as.character(Comorb_DRG$METS))>1]
METS <- trimws(METS, which = c("both"))

TUMOR <- as.character(Comorb_DRG$TUMOR)[nchar(as.character(Comorb_DRG$TUMOR))>1]
TUMOR <- trimws(TUMOR, which = c("both"))

ARTH <- as.character(Comorb_DRG$ARTH)[nchar(as.character(Comorb_DRG$ARTH))>1]
ARTH <- trimws(ARTH, which = c("both"))

COAG <- as.character(Comorb_DRG$COAG)[nchar(as.character(Comorb_DRG$COAG))>1]
COAG <- trimws(COAG, which = c("both"))

OBESE <- as.character(Comorb_DRG$OBESE)[nchar(as.character(Comorb_DRG$OBESE))>1]
OBESE <- trimws(OBESE, which = c("both"))

WGHTLOSS <- as.character(Comorb_DRG$WGHTLOSS)[nchar(as.character(Comorb_DRG$WGHTLOSS))>1]
WGHTLOSS <- trimws(WGHTLOSS, which = c("both"))

LYTES <- as.character(Comorb_DRG$LYTES)[nchar(as.character(Comorb_DRG$LYTES))>1]
LYTES <- trimws(LYTES, which = c("both"))

BLDLOSS <- as.character(Comorb_DRG$BLDLOSS)[nchar(as.character(Comorb_DRG$BLDLOSS))>1]
BLDLOSS <- trimws(BLDLOSS, which = c("both"))

ANEMDEF <- as.character(Comorb_DRG$ANEMDEF)[nchar(as.character(Comorb_DRG$ANEMDEF))>1]
ANEMDEF <- trimws(ANEMDEF, which = c("both"))

ALCOHOL <- as.character(Comorb_DRG$ALCOHOL)[nchar(as.character(Comorb_DRG$ALCOHOL))>1]
ALCOHOL <- trimws(ALCOHOL, which = c("both"))

DRUG <- as.character(Comorb_DRG$DRUG)[nchar(as.character(Comorb_DRG$DRUG))>1]
DRUG <- trimws(DRUG, which = c("both"))

PSYCH <- as.character(Comorb_DRG$PSYCH)[nchar(as.character(Comorb_DRG$PSYCH))>1]
PSYCH <- trimws(PSYCH, which = c("both"))

DEPRESS <- as.character(Comorb_DRG$DEPRESS)[nchar(as.character(Comorb_DRG$DEPRESS))>1]
DEPRESS <- trimws(DEPRESS, which = c("both"))


COMORB <- diagnosis%>%
  mutate(ICD10X =str_replace_all(ICD10, "[^[:alnum:]]", ""),
         CHF = case_when(ICD10X %in% CHF ~ 1, TRUE ~ 0),
         VALVE	= case_when(ICD10X %in% 	VALVE	~ 1, TRUE ~ 0),
         PULMCIRC	= case_when(ICD10X %in% 	PULMCIRC	~ 1, TRUE ~ 0),
         PERIVASC	= case_when(ICD10X %in% 	PERIVASC	~ 1, TRUE ~ 0),
         HTN	= case_when(ICD10X %in% 	HTN	~ 1, TRUE ~ 0),
         HTNCX	= case_when(ICD10X %in% 	HTNCX	~ 1, TRUE ~ 0),
         PARA	= case_when(ICD10X %in% 	PARA	~ 1, TRUE ~ 0),
         NEURO	= case_when(ICD10X %in% 	NEURO	~ 1, TRUE ~ 0),
         CHRNLUNG	= case_when(ICD10X %in% 	CHRNLUNG	~ 1, TRUE ~ 0),
         DM	= case_when(ICD10X %in% 	DM	~ 1, TRUE ~ 0),
         DMCX	= case_when(ICD10X %in% 	DMCX	~ 1, TRUE ~ 0),
         HYPOTHY	= case_when(ICD10X %in% 	HYPOTHY	~ 1, TRUE ~ 0),
         RENLFAIL	= case_when(ICD10X %in% 	RENLFAIL	~ 1, TRUE ~ 0),
         LIVER	= case_when(ICD10X %in% 	LIVER	~ 1, TRUE ~ 0),
         ULCER	= case_when(ICD10X %in% 	ULCER	~ 1, TRUE ~ 0),
         AIDS	= case_when(ICD10X %in% 	AIDS	~ 1, TRUE ~ 0),
         LYMPH	= case_when(ICD10X %in% 	LYMPH	~ 1, TRUE ~ 0),
         METS	= case_when(ICD10X %in% 	METS	~ 1, TRUE ~ 0),
         TUMOR	= case_when(ICD10X %in% 	TUMOR	~ 1, TRUE ~ 0),
         ARTH	= case_when(ICD10X %in% 	ARTH	~ 1, TRUE ~ 0),
         COAG	= case_when(ICD10X %in% 	COAG	~ 1, TRUE ~ 0),
         OBESE	= case_when(ICD10X %in% 	OBESE	~ 1, TRUE ~ 0),
         WGHTLOSS	= case_when(ICD10X %in% 	WGHTLOSS	~ 1, TRUE ~ 0),
         LYTES	= case_when(ICD10X %in% 	LYTES	~ 1, TRUE ~ 0),
         BLDLOSS	= case_when(ICD10X %in% 	BLDLOSS	~ 1, TRUE ~ 0),
         ANEMDEF	= case_when(ICD10X %in% 	ANEMDEF	~ 1, TRUE ~ 0),
         ALCOHOL	= case_when(ICD10X %in% 	ALCOHOL	~ 1, TRUE ~ 0),
         DRUG	= case_when(ICD10X %in% 	DRUG	~ 1, TRUE ~ 0),
         PSYCH	= case_when(ICD10X %in% 	PSYCH	~ 1, TRUE ~ 0),
         DEPRESS	= case_when(ICD10X %in% 	DEPRESS	~ 1, TRUE ~ 0),
         
         CHF_exclude	= case_when( 	CHF	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         VALVE_exclude	= case_when( 	VALVE	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         PULMCIRC_exclude	= case_when( 	PULMCIRC	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         PERIVASC_exclude	= case_when( 	PERIVASC	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         HTN_exclude	= case_when( 	HTN	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         HTNCX_exclude	= case_when( 	HTNCX	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         PARA_exclude	= case_when( 	PARA	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         NEURO_exclude	= case_when( 	NEURO	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         CHRNLUNG_exclude	= case_when( 	CHRNLUNG	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         DM_exclude	= case_when( 	DM	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         DMCX_exclude	= case_when( 	DMCX	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         HYPOTHY_exclude	= case_when( 	HYPOTHY	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         RENLFAIL_exclude	= case_when( 	RENLFAIL	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         LIVER_exclude	= case_when( 	LIVER	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         ULCER_exclude	= case_when( 	ULCER	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         AIDS_exclude	= case_when( 	AIDS	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         LYMPH_exclude	= case_when( 	LYMPH	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         METS_exclude	= case_when( 	METS	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         TUMOR_exclude	= case_when( 	TUMOR	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         ARTH_exclude	= case_when( 	ARTH	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         COAG_exclude	= case_when( 	COAG	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         OBESE_exclude	= case_when( 	OBESE	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         WGHTLOSS_exclude	= case_when( 	WGHTLOSS	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         LYTES_exclude	= case_when( 	LYTES	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         BLDLOSS_exclude	= case_when( 	BLDLOSS	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         ANEMDEF_exclude	= case_when( 	ANEMDEF	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         ALCOHOL_exclude	= case_when( 	ALCOHOL	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         DRUG_exclude	= case_when( 	DRUG	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         PSYCH_exclude	= case_when( 	PSYCH	== 1 & LINE == 1 ~ 1, TRUE ~ 0),
         DEPRESS_exclude	= case_when( 	DEPRESS	== 1 & LINE == 1 ~ 1, TRUE ~ 0)
  )


# CHF_exclude_ID <- as.character(COMORB%>%filter(CHF_exclude==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
CHF_exclude_ID	<- as.character(COMORB%>%filter(	CHF_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
VALVE_exclude_ID	<- as.character(COMORB%>%filter(	VALVE_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
PULMCIRC_exclude_ID	<- as.character(COMORB%>%filter(	PULMCIRC_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
PERIVASC_exclude_ID	<- as.character(COMORB%>%filter(	PERIVASC_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
HTN_exclude_ID	<- as.character(COMORB%>%filter(	HTN_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
HTNCX_exclude_ID	<- as.character(COMORB%>%filter(	HTNCX_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
PARA_exclude_ID	<- as.character(COMORB%>%filter(	PARA_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
NEURO_exclude_ID	<- as.character(COMORB%>%filter(	NEURO_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
CHRNLUNG_exclude_ID	<- as.character(COMORB%>%filter(	CHRNLUNG_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
DM_exclude_ID	<- as.character(COMORB%>%filter(	DM_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
DMCX_exclude_ID	<- as.character(COMORB%>%filter(	DMCX_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
HYPOTHY_exclude_ID	<- as.character(COMORB%>%filter(	HYPOTHY_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
RENLFAIL_exclude_ID	<- as.character(COMORB%>%filter(	RENLFAIL_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
LIVER_exclude_ID	<- as.character(COMORB%>%filter(	LIVER_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
ULCER_exclude_ID	<- as.character(COMORB%>%filter(	ULCER_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
AIDS_exclude_ID	<- as.character(COMORB%>%filter(	AIDS_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
LYMPH_exclude_ID	<- as.character(COMORB%>%filter(	LYMPH_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
METS_exclude_ID	<- as.character(COMORB%>%filter(	METS_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
TUMOR_exclude_ID	<- as.character(COMORB%>%filter(	TUMOR_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
ARTH_exclude_ID	<- as.character(COMORB%>%filter(	ARTH_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
COAG_exclude_ID	<- as.character(COMORB%>%filter(	COAG_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
OBESE_exclude_ID	<- as.character(COMORB%>%filter(	OBESE_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
WGHTLOSS_exclude_ID	<- as.character(COMORB%>%filter(	WGHTLOSS_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
LYTES_exclude_ID	<- as.character(COMORB%>%filter(	LYTES_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
BLDLOSS_exclude_ID	<- as.character(COMORB%>%filter(	BLDLOSS_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
ANEMDEF_exclude_ID	<- as.character(COMORB%>%filter(	ANEMDEF_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
ALCOHOL_exclude_ID	<- as.character(COMORB%>%filter(	ALCOHOL_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
DRUG_exclude_ID	<- as.character(COMORB%>%filter(	DRUG_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
PSYCH_exclude_ID	<- as.character(COMORB%>%filter(	PSYCH_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))
DEPRESS_exclude_ID	<- as.character(COMORB%>%filter(	DEPRESS_exclude	==1)%>%select(HSP_ACCOUNT_ID)%>%distinct(HSP_ACCOUNT_ID))


# CHF_ID<-COMORB%>%
#   arrange(HSP_ACCOUNT_ID, desc(CHF))%>%
#   distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%
#   mutate(CHF=case_when(HSP_ACCOUNT_ID %in% CHF_exclude_ID ~ 0, TRUE~CHF))%>%
#   select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, CHF)%>%
#   arrange(PAT_ID, DIS_DTM)%>%
#   group_by(PAT_ID)%>% 
#   mutate(CHF_L = lag(CHF))%>%select(HSP_ACCOUNT_ID,CHF_L) 

CHF_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(CHF))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(CHF=case_when(HSP_ACCOUNT_ID %in% CHF_exclude_ID~ 0, TRUE~CHF))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, CHF)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(CHF_L= lag(CHF)) %>%select(PAT_ID,HSP_ACCOUNT_ID,CHF_L)
VALVE_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(VALVE))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(VALVE=case_when(HSP_ACCOUNT_ID %in% VALVE_exclude_ID~ 0, TRUE~VALVE))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, VALVE)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(VALVE_L= lag(VALVE)) %>%select(PAT_ID,HSP_ACCOUNT_ID,VALVE_L)
PULMCIRC_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(PULMCIRC))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(PULMCIRC=case_when(HSP_ACCOUNT_ID %in% PULMCIRC_exclude_ID~ 0, TRUE~PULMCIRC))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, PULMCIRC)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(PULMCIRC_L= lag(PULMCIRC)) %>%select(PAT_ID,HSP_ACCOUNT_ID,PULMCIRC_L)
PERIVASC_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(PERIVASC))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(PERIVASC=case_when(HSP_ACCOUNT_ID %in% PERIVASC_exclude_ID~ 0, TRUE~PERIVASC))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, PERIVASC)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(PERIVASC_L= lag(PERIVASC)) %>%select(PAT_ID,HSP_ACCOUNT_ID,PERIVASC_L)
HTN_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(HTN))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(HTN=case_when(HSP_ACCOUNT_ID %in% HTN_exclude_ID~ 0, TRUE~HTN))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, HTN)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(HTN_L= lag(HTN)) %>%select(PAT_ID,HSP_ACCOUNT_ID,HTN_L)
HTNCX_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(HTNCX))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(HTNCX=case_when(HSP_ACCOUNT_ID %in% HTNCX_exclude_ID~ 0, TRUE~HTNCX))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, HTNCX)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(HTNCX_L= lag(HTNCX)) %>%select(PAT_ID,HSP_ACCOUNT_ID,HTNCX_L)
PARA_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(PARA))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(PARA=case_when(HSP_ACCOUNT_ID %in% PARA_exclude_ID~ 0, TRUE~PARA))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, PARA)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(PARA_L= lag(PARA)) %>%select(PAT_ID,HSP_ACCOUNT_ID,PARA_L)
NEURO_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(NEURO))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(NEURO=case_when(HSP_ACCOUNT_ID %in% NEURO_exclude_ID~ 0, TRUE~NEURO))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, NEURO)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(NEURO_L= lag(NEURO)) %>%select(PAT_ID,HSP_ACCOUNT_ID,NEURO_L)
CHRNLUNG_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(CHRNLUNG))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(CHRNLUNG=case_when(HSP_ACCOUNT_ID %in% CHRNLUNG_exclude_ID~ 0, TRUE~CHRNLUNG))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, CHRNLUNG)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(CHRNLUNG_L= lag(CHRNLUNG)) %>%select(PAT_ID,HSP_ACCOUNT_ID,CHRNLUNG_L)
DM_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(DM))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(DM=case_when(HSP_ACCOUNT_ID %in% DM_exclude_ID~ 0, TRUE~DM))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, DM)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(DM_L= lag(DM)) %>%select(PAT_ID,HSP_ACCOUNT_ID,DM_L)
DMCX_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(DMCX))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(DMCX=case_when(HSP_ACCOUNT_ID %in% DMCX_exclude_ID~ 0, TRUE~DMCX))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, DMCX)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(DMCX_L= lag(DMCX)) %>%select(PAT_ID,HSP_ACCOUNT_ID,DMCX_L)
HYPOTHY_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(HYPOTHY))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(HYPOTHY=case_when(HSP_ACCOUNT_ID %in% HYPOTHY_exclude_ID~ 0, TRUE~HYPOTHY))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, HYPOTHY)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(HYPOTHY_L= lag(HYPOTHY)) %>%select(PAT_ID,HSP_ACCOUNT_ID,HYPOTHY_L)
RENLFAIL_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(RENLFAIL))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(RENLFAIL=case_when(HSP_ACCOUNT_ID %in% RENLFAIL_exclude_ID~ 0, TRUE~RENLFAIL))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, RENLFAIL)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(RENLFAIL_L= lag(RENLFAIL)) %>%select(PAT_ID,HSP_ACCOUNT_ID,RENLFAIL_L)
LIVER_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(LIVER))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(LIVER=case_when(HSP_ACCOUNT_ID %in% LIVER_exclude_ID~ 0, TRUE~LIVER))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, LIVER)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(LIVER_L= lag(LIVER)) %>%select(PAT_ID,HSP_ACCOUNT_ID,LIVER_L)
ULCER_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(ULCER))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(ULCER=case_when(HSP_ACCOUNT_ID %in% ULCER_exclude_ID~ 0, TRUE~ULCER))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, ULCER)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(ULCER_L= lag(ULCER)) %>%select(PAT_ID,HSP_ACCOUNT_ID,ULCER_L)
AIDS_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(AIDS))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(AIDS=case_when(HSP_ACCOUNT_ID %in% AIDS_exclude_ID~ 0, TRUE~AIDS))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, AIDS)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(AIDS_L= lag(AIDS)) %>%select(PAT_ID,HSP_ACCOUNT_ID,AIDS_L)
LYMPH_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(LYMPH))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(LYMPH=case_when(HSP_ACCOUNT_ID %in% LYMPH_exclude_ID~ 0, TRUE~LYMPH))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, LYMPH)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(LYMPH_L= lag(LYMPH)) %>%select(PAT_ID,HSP_ACCOUNT_ID,LYMPH_L)
METS_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(METS))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(METS=case_when(HSP_ACCOUNT_ID %in% METS_exclude_ID~ 0, TRUE~METS))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, METS)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(METS_L= lag(METS)) %>%select(PAT_ID,HSP_ACCOUNT_ID,METS_L)
TUMOR_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(TUMOR))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(TUMOR=case_when(HSP_ACCOUNT_ID %in% TUMOR_exclude_ID~ 0, TRUE~TUMOR))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, TUMOR)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(TUMOR_L= lag(TUMOR)) %>%select(PAT_ID,HSP_ACCOUNT_ID,TUMOR_L)
ARTH_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(ARTH))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(ARTH=case_when(HSP_ACCOUNT_ID %in% ARTH_exclude_ID~ 0, TRUE~ARTH))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, ARTH)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(ARTH_L= lag(ARTH)) %>%select(PAT_ID,HSP_ACCOUNT_ID,ARTH_L)
COAG_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(COAG))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(COAG=case_when(HSP_ACCOUNT_ID %in% COAG_exclude_ID~ 0, TRUE~COAG))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, COAG)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(COAG_L= lag(COAG)) %>%select(PAT_ID,HSP_ACCOUNT_ID,COAG_L)
OBESE_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(OBESE))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(OBESE=case_when(HSP_ACCOUNT_ID %in% OBESE_exclude_ID~ 0, TRUE~OBESE))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, OBESE)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(OBESE_L= lag(OBESE)) %>%select(PAT_ID,HSP_ACCOUNT_ID,OBESE_L)
WGHTLOSS_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(WGHTLOSS))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(WGHTLOSS=case_when(HSP_ACCOUNT_ID %in% WGHTLOSS_exclude_ID~ 0, TRUE~WGHTLOSS))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, WGHTLOSS)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(WGHTLOSS_L= lag(WGHTLOSS)) %>%select(PAT_ID,HSP_ACCOUNT_ID,WGHTLOSS_L)
LYTES_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(LYTES))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(LYTES=case_when(HSP_ACCOUNT_ID %in% LYTES_exclude_ID~ 0, TRUE~LYTES))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, LYTES)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(LYTES_L= lag(LYTES)) %>%select(PAT_ID,HSP_ACCOUNT_ID,LYTES_L)
BLDLOSS_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(BLDLOSS))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(BLDLOSS=case_when(HSP_ACCOUNT_ID %in% BLDLOSS_exclude_ID~ 0, TRUE~BLDLOSS))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, BLDLOSS)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(BLDLOSS_L= lag(BLDLOSS)) %>%select(PAT_ID,HSP_ACCOUNT_ID,BLDLOSS_L)
ANEMDEF_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(ANEMDEF))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(ANEMDEF=case_when(HSP_ACCOUNT_ID %in% ANEMDEF_exclude_ID~ 0, TRUE~ANEMDEF))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, ANEMDEF)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(ANEMDEF_L= lag(ANEMDEF)) %>%select(PAT_ID,HSP_ACCOUNT_ID,ANEMDEF_L)
ALCOHOL_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(ALCOHOL))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(ALCOHOL=case_when(HSP_ACCOUNT_ID %in% ALCOHOL_exclude_ID~ 0, TRUE~ALCOHOL))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, ALCOHOL)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(ALCOHOL_L= lag(ALCOHOL)) %>%select(PAT_ID,HSP_ACCOUNT_ID,ALCOHOL_L)
DRUG_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(DRUG))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(DRUG=case_when(HSP_ACCOUNT_ID %in% DRUG_exclude_ID~ 0, TRUE~DRUG))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, DRUG)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(DRUG_L= lag(DRUG)) %>%select(PAT_ID,HSP_ACCOUNT_ID,DRUG_L)
PSYCH_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(PSYCH))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(PSYCH=case_when(HSP_ACCOUNT_ID %in% PSYCH_exclude_ID~ 0, TRUE~PSYCH))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, PSYCH)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(PSYCH_L= lag(PSYCH)) %>%select(PAT_ID,HSP_ACCOUNT_ID,PSYCH_L)
DEPRESS_ID<-COMORB%>%arrange(HSP_ACCOUNT_ID, desc(DEPRESS))%>%distinct(HSP_ACCOUNT_ID,.keep_all=TRUE)%>%mutate(DEPRESS=case_when(HSP_ACCOUNT_ID %in% DEPRESS_exclude_ID~ 0, TRUE~DEPRESS))%>%select(PAT_ID, HSP_ACCOUNT_ID, DIS_DTM, DEPRESS)%>%arrange(PAT_ID, DIS_DTM)%>%group_by(PAT_ID)%>%mutate(DEPRESS_L= lag(DEPRESS)) %>%select(PAT_ID,HSP_ACCOUNT_ID,DEPRESS_L)



CHF_ID<- subset(CHF_ID, select = -c(PAT_ID))%>%mutate(CHF_L=case_when(is.na(CHF_L) ~ 0, TRUE ~ CHF_L))
VALVE_ID<- subset(VALVE_ID, select = -c(PAT_ID))%>%mutate(VALVE_L=case_when(is.na(VALVE_L) ~ 0, TRUE ~ VALVE_L))
PULMCIRC_ID<- subset(PULMCIRC_ID, select = -c(PAT_ID))%>%mutate(PULMCIRC_L=case_when(is.na(PULMCIRC_L) ~ 0, TRUE ~ PULMCIRC_L))
PERIVASC_ID<- subset(PERIVASC_ID, select = -c(PAT_ID))%>%mutate(PERIVASC_L=case_when(is.na(PERIVASC_L) ~ 0, TRUE ~ PERIVASC_L))
HTN_ID<- subset(HTN_ID, select = -c(PAT_ID))%>%mutate(HTN_L=case_when(is.na(HTN_L) ~ 0, TRUE ~ HTN_L))
HTNCX_ID<- subset(HTNCX_ID, select = -c(PAT_ID))%>%mutate(HTNCX_L=case_when(is.na(HTNCX_L) ~ 0, TRUE ~ HTNCX_L))
PARA_ID<- subset(PARA_ID, select = -c(PAT_ID))%>%mutate(PARA_L=case_when(is.na(PARA_L) ~ 0, TRUE ~ PARA_L))
NEURO_ID<- subset(NEURO_ID, select = -c(PAT_ID))%>%mutate(NEURO_L=case_when(is.na(NEURO_L) ~ 0, TRUE ~ NEURO_L))
CHRNLUNG_ID<- subset(CHRNLUNG_ID, select = -c(PAT_ID))%>%mutate(CHRNLUNG_L=case_when(is.na(CHRNLUNG_L) ~ 0, TRUE ~ CHRNLUNG_L))
DM_ID<- subset(DM_ID, select = -c(PAT_ID))%>%mutate(DM_L=case_when(is.na(DM_L) ~ 0, TRUE ~ DM_L))
DMCX_ID<- subset(DMCX_ID, select = -c(PAT_ID))%>%mutate(DMCX_L=case_when(is.na(DMCX_L) ~ 0, TRUE ~ DMCX_L))
HYPOTHY_ID<- subset(HYPOTHY_ID, select = -c(PAT_ID))%>%mutate(HYPOTHY_L=case_when(is.na(HYPOTHY_L) ~ 0, TRUE ~ HYPOTHY_L))
RENLFAIL_ID<- subset(RENLFAIL_ID, select = -c(PAT_ID))%>%mutate(RENLFAIL_L=case_when(is.na(RENLFAIL_L) ~ 0, TRUE ~ RENLFAIL_L))
LIVER_ID<- subset(LIVER_ID, select = -c(PAT_ID))%>%mutate(LIVER_L=case_when(is.na(LIVER_L) ~ 0, TRUE ~ LIVER_L))
ULCER_ID<- subset(ULCER_ID, select = -c(PAT_ID))%>%mutate(ULCER_L=case_when(is.na(ULCER_L) ~ 0, TRUE ~ ULCER_L))
AIDS_ID<- subset(AIDS_ID, select = -c(PAT_ID))%>%mutate(AIDS_L=case_when(is.na(AIDS_L) ~ 0, TRUE ~ AIDS_L))
LYMPH_ID<- subset(LYMPH_ID, select = -c(PAT_ID))%>%mutate(LYMPH_L=case_when(is.na(LYMPH_L) ~ 0, TRUE ~ LYMPH_L))
METS_ID<- subset(METS_ID, select = -c(PAT_ID))%>%mutate(METS_L=case_when(is.na(METS_L) ~ 0, TRUE ~ METS_L))
TUMOR_ID<- subset(TUMOR_ID, select = -c(PAT_ID))%>%mutate(TUMOR_L=case_when(is.na(TUMOR_L) ~ 0, TRUE ~ TUMOR_L))
ARTH_ID<- subset(ARTH_ID, select = -c(PAT_ID))%>%mutate(ARTH_L=case_when(is.na(ARTH_L) ~ 0, TRUE ~ ARTH_L))
COAG_ID<- subset(COAG_ID, select = -c(PAT_ID))%>%mutate(COAG_L=case_when(is.na(COAG_L) ~ 0, TRUE ~ COAG_L))
OBESE_ID<- subset(OBESE_ID, select = -c(PAT_ID))%>%mutate(OBESE_L=case_when(is.na(OBESE_L) ~ 0, TRUE ~ OBESE_L))
WGHTLOSS_ID<- subset(WGHTLOSS_ID, select = -c(PAT_ID))%>%mutate(WGHTLOSS_L=case_when(is.na(WGHTLOSS_L) ~ 0, TRUE ~ WGHTLOSS_L))
LYTES_ID<- subset(LYTES_ID, select = -c(PAT_ID))%>%mutate(LYTES_L=case_when(is.na(LYTES_L) ~ 0, TRUE ~ LYTES_L))
BLDLOSS_ID<- subset(BLDLOSS_ID, select = -c(PAT_ID))%>%mutate(BLDLOSS_L=case_when(is.na(BLDLOSS_L) ~ 0, TRUE ~ BLDLOSS_L))
ANEMDEF_ID<- subset(ANEMDEF_ID, select = -c(PAT_ID))%>%mutate(ANEMDEF_L=case_when(is.na(ANEMDEF_L) ~ 0, TRUE ~ ANEMDEF_L))
ALCOHOL_ID<- subset(ALCOHOL_ID, select = -c(PAT_ID))%>%mutate(ALCOHOL_L=case_when(is.na(ALCOHOL_L) ~ 0, TRUE ~ ALCOHOL_L))
DRUG_ID<- subset(DRUG_ID, select = -c(PAT_ID))%>%mutate(DRUG_L=case_when(is.na(DRUG_L) ~ 0, TRUE ~ DRUG_L))
PSYCH_ID<- subset(PSYCH_ID, select = -c(PAT_ID))%>%mutate(PSYCH_L=case_when(is.na(PSYCH_L) ~ 0, TRUE ~ PSYCH_L))
DEPRESS_ID<- subset(DEPRESS_ID, select = -c(PAT_ID))%>%mutate(DEPRESS_L=case_when(is.na(DEPRESS_L) ~ 0, TRUE ~ DEPRESS_L))



COMORB_ALL <- left_join(CHF_ID,VALVE_ID,by="HSP_ACCOUNT_ID")%>%
  left_join(.,PULMCIRC_ID,by="HSP_ACCOUNT_ID")%>%
  left_join(.,PERIVASC_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,HTN_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,HTNCX_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,PARA_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,NEURO_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,CHRNLUNG_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,DM_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,DMCX_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,HYPOTHY_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,RENLFAIL_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,LIVER_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,ULCER_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,AIDS_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,LYMPH_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,METS_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,TUMOR_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,ARTH_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,COAG_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,OBESE_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,WGHTLOSS_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,LYTES_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,BLDLOSS_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,ANEMDEF_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,ALCOHOL_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,DRUG_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,PSYCH_ID,by='HSP_ACCOUNT_ID')%>%
  left_join(.,DEPRESS_ID,by='HSP_ACCOUNT_ID')

save(COMORB_ALL,file = "COMORB_ALL.RData")

remove(CHF	,
       VALVE	,
       PULMCIRC	,
       PERIVASC	,
       HTN	,
       HTNCX	,
       PARA	,
       NEURO	,
       CHRNLUNG	,
       DM	,
       DMCX	,
       HYPOTHY	,
       RENLFAIL	,
       LIVER	,
       ULCER	,
       AIDS	,
       LYMPH	,
       METS	,
       TUMOR	,
       ARTH	,
       COAG	,
       OBESE	,
       WGHTLOSS	,
       LYTES	,
       BLDLOSS	,
       ANEMDEF	,
       ALCOHOL	,
       DRUG	,
       PSYCH	,
       DEPRESS	,
       CHF_ID	,
       VALVE_ID	,
       PULMCIRC_ID	,
       PERIVASC_ID	,
       HTN_ID	,
       HTNCX_ID	,
       PARA_ID	,
       NEURO_ID	,
       CHRNLUNG_ID	,
       DM_ID	,
       DMCX_ID	,
       HYPOTHY_ID	,
       RENLFAIL_ID	,
       LIVER_ID	,
       ULCER_ID	,
       AIDS_ID	,
       LYMPH_ID	,
       METS_ID	,
       TUMOR_ID	,
       ARTH_ID	,
       COAG_ID	,
       OBESE_ID	,
       WGHTLOSS_ID	,
       LYTES_ID	,
       BLDLOSS_ID	,
       ANEMDEF_ID	,
       ALCOHOL_ID	,
       DRUG_ID	,
       PSYCH_ID	,
       DEPRESS_ID	,
       CHF_exclude_ID	,
       VALVE_exclude_ID	,
       PULMCIRC_exclude_ID	,
       PERIVASC_exclude_ID	,
       HTN_exclude_ID	,
       HTNCX_exclude_ID	,
       PARA_exclude_ID	,
       NEURO_exclude_ID	,
       CHRNLUNG_exclude_ID	,
       DM_exclude_ID	,
       DMCX_exclude_ID	,
       HYPOTHY_exclude_ID	,
       RENLFAIL_exclude_ID	,
       LIVER_exclude_ID	,
       ULCER_exclude_ID	,
       AIDS_exclude_ID	,
       LYMPH_exclude_ID	,
       METS_exclude_ID	,
       TUMOR_exclude_ID	,
       ARTH_exclude_ID	,
       COAG_exclude_ID	,
       OBESE_exclude_ID	,
       WGHTLOSS_exclude_ID	,
       LYTES_exclude_ID	,
       BLDLOSS_exclude_ID	,
       ANEMDEF_exclude_ID	,
       ALCOHOL_exclude_ID	,
       DRUG_exclude_ID	,
       PSYCH_exclude_ID	,
       DEPRESS_exclude_ID	
       
)



