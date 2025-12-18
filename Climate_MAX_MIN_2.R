library(tidyverse)
library(gridExtra)
library(lubridate)
library(corrplot)
library(car)

AVG_MAX_45_Tualatin <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/Climate/AVG_MAX_45_Tualatin.csv", stringsAsFactors = FALSE, header=FALSE)
AVG_MAX_85_Tualatin <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/Climate/AVG_MAX_85_Tualatin.csv", stringsAsFactors = FALSE, header=FALSE)
AVG_MIN_45_Tualatin <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/Climate/AVG_MIN_45_Tualatin.csv", stringsAsFactors = FALSE, header=FALSE)
AVG_MIN_85_Tualatin <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/Climate/AVG_MIN_85_Tualatin.csv", stringsAsFactors = FALSE, header=FALSE)
AVG_MAX_45_Lafayette <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/Climate/AVG_MAX_45_Lafayette.csv", stringsAsFactors = FALSE, header=FALSE)
AVG_MAX_85_Lafayette <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/Climate/AVG_MAX_85_Lafayette.csv", stringsAsFactors = FALSE, header=FALSE)
AVG_MIN_45_Lafayette <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/Climate/AVG_MIN_45_Lafayette.csv", stringsAsFactors = FALSE, header=FALSE)
AVG_MIN_85_Lafayette <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/Climate/AVG_MIN_85_Lafayette.csv", stringsAsFactors = FALSE, header=FALSE)

colnames(AVG_MAX_45_Tualatin) <- c("doy","year","month","dom","bcc_csm1_1","bcc_csm1_1_m","BNU_ESM","CanESM2","CCSM4","CNRM_CM5","CSIRO_Mk_3_6_0","GFDL_ESM2M","GFDL_ESM2G","HadGEM2_CC365","HadGEM2_ES365","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC5","MIROC_ESM","MIROC_ESM_CHEM","MRI_CGCM3","NorESM1_M")
colnames(AVG_MAX_85_Tualatin) <- c("doy","year","month","dom","bcc_csm1_1","bcc_csm1_1_m","BNU_ESM","CanESM2","CCSM4","CNRM_CM5","CSIRO_Mk_3_6_0","GFDL_ESM2M","GFDL_ESM2G","HadGEM2_CC365","HadGEM2_ES365","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC5","MIROC_ESM","MIROC_ESM_CHEM","MRI_CGCM3","NorESM1_M")
colnames(AVG_MIN_45_Tualatin) <- c("doy","year","month","dom","bcc_csm1_1","bcc_csm1_1_m","BNU_ESM","CanESM2","CCSM4","CNRM_CM5","CSIRO_Mk_3_6_0","GFDL_ESM2M","GFDL_ESM2G","HadGEM2_CC365","HadGEM2_ES365","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC5","MIROC_ESM","MIROC_ESM_CHEM","MRI_CGCM3","NorESM1_M")
colnames(AVG_MIN_85_Tualatin) <- c("doy","year","month","dom","bcc_csm1_1","bcc_csm1_1_m","BNU_ESM","CanESM2","CCSM4","CNRM_CM5","CSIRO_Mk_3_6_0","GFDL_ESM2M","GFDL_ESM2G","HadGEM2_CC365","HadGEM2_ES365","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC5","MIROC_ESM","MIROC_ESM_CHEM","MRI_CGCM3","NorESM1_M")
colnames(AVG_MAX_45_Lafayette) <- c("doy","year","month","dom","bcc_csm1_1","bcc_csm1_1_m","BNU_ESM","CanESM2","CCSM4","CNRM_CM5","CSIRO_Mk_3_6_0","GFDL_ESM2M","GFDL_ESM2G","HadGEM2_CC365","HadGEM2_ES365","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC5","MIROC_ESM","MIROC_ESM_CHEM","MRI_CGCM3","NorESM1_M")
colnames(AVG_MAX_85_Lafayette) <- c("doy","year","month","dom","bcc_csm1_1","bcc_csm1_1_m","BNU_ESM","CanESM2","CCSM4","CNRM_CM5","CSIRO_Mk_3_6_0","GFDL_ESM2M","GFDL_ESM2G","HadGEM2_CC365","HadGEM2_ES365","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC5","MIROC_ESM","MIROC_ESM_CHEM","MRI_CGCM3","NorESM1_M")
colnames(AVG_MIN_45_Lafayette) <- c("doy","year","month","dom","bcc_csm1_1","bcc_csm1_1_m","BNU_ESM","CanESM2","CCSM4","CNRM_CM5","CSIRO_Mk_3_6_0","GFDL_ESM2M","GFDL_ESM2G","HadGEM2_CC365","HadGEM2_ES365","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC5","MIROC_ESM","MIROC_ESM_CHEM","MRI_CGCM3","NorESM1_M")
colnames(AVG_MIN_85_Lafayette) <- c("doy","year","month","dom","bcc_csm1_1","bcc_csm1_1_m","BNU_ESM","CanESM2","CCSM4","CNRM_CM5","CSIRO_Mk_3_6_0","GFDL_ESM2M","GFDL_ESM2G","HadGEM2_CC365","HadGEM2_ES365","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC5","MIROC_ESM","MIROC_ESM_CHEM","MRI_CGCM3","NorESM1_M")

AVG_MAX_45_Tualatin <- AVG_MAX_45_Tualatin %>% filter(month > 5) %>% filter(month < 9)
AVG_MAX_85_Tualatin <- AVG_MAX_85_Tualatin %>% filter(month > 5) %>% filter(month < 9)
AVG_MAX_45_Lafayette <- AVG_MAX_45_Tualatin %>% filter(month > 5) %>% filter(month < 9)
AVG_MAX_85_Lafayette <- AVG_MAX_85_Tualatin %>% filter(month > 5) %>% filter(month < 9)


AVG_MAX_45_Tualatin_AVG <- AVG_MAX_45_Tualatin %>%
  group_by(year) %>%
  summarize(avg_bcc_csm1_1 = mean(bcc_csm1_1, na.rm = TRUE),
#            sd_bcc_csm1_1 = sd(bcc_csm1_1, na.rm = TRUE),
#            max_bcc_csm1_1 = max(bcc_csm1_1, na.rm = TRUE),
#            min_bcc_csm1_1 = min(bcc_csm1_1, na.rm = TRUE),
		avg_bcc_csm1_1_m = mean(bcc_csm1_1_m, na.rm = TRUE),
#            sd_bcc_csm1_1_m = sd(bcc_csm1_1_m, na.rm = TRUE),
#            max_bcc_csm1_1_m = max(bcc_csm1_1_m, na.rm = TRUE),
#            min_bcc_csm1_1_m = min(bcc_csm1_1_m, na.rm = TRUE),
		avg_BNU_ESM = mean(BNU_ESM, na.rm = TRUE),
#            sd_BNU_ESM = sd(BNU_ESM, na.rm = TRUE),
#            max_BNU_ESM = max(BNU_ESM, na.rm = TRUE),
#            min_BNU_ESM = min(BNU_ESM, na.rm = TRUE),
		avg_CanESM2 = mean(CanESM2, na.rm = TRUE),
#            sd_CanESM2 = sd(CanESM2, na.rm = TRUE),
#            max_CanESM2 = max(CanESM2, na.rm = TRUE),
#            min_CanESM2 = min(CanESM2, na.rm = TRUE),
		avg_CCSM4 = mean(CCSM4, na.rm = TRUE),
#            sd_CCSM4 = sd(CCSM4, na.rm = TRUE),
#            max_CCSM4 = max(CCSM4, na.rm = TRUE),
#            min_CCSM4 = min(CCSM4, na.rm = TRUE),
		avg_CNRM_CM5 = mean(CNRM_CM5, na.rm = TRUE),
#            sd_CNRM_CM5 = sd(CNRM_CM5, na.rm = TRUE),
#            max_CNRM_CM5 = max(CNRM_CM5, na.rm = TRUE),
#            min_CNRM_CM5 = min(CNRM_CM5, na.rm = TRUE),
		avg_CSIRO_Mk_3_6_0 = mean(CSIRO_Mk_3_6_0, na.rm = TRUE),
#            sd_CSIRO_Mk_3_6_0 = sd(CSIRO_Mk_3_6_0, na.rm = TRUE),
#            max_CSIRO_Mk_3_6_0 = max(CSIRO_Mk_3_6_0, na.rm = TRUE),
#            min_CSIRO_Mk_3_6_0 = min(CSIRO_Mk_3_6_0, na.rm = TRUE),
		avg_GFDL_ESM2M = mean(GFDL_ESM2M, na.rm = TRUE),
#            sd_GFDL_ESM2M = sd(GFDL_ESM2M, na.rm = TRUE),
#            max_GFDL_ESM2M = max(GFDL_ESM2M, na.rm = TRUE),
#            min_GFDL_ESM2M = min(GFDL_ESM2M, na.rm = TRUE),
		avg_GFDL_ESM2G = mean(GFDL_ESM2G, na.rm = TRUE),
#            sd_GFDL_ESM2G = sd(GFDL_ESM2G, na.rm = TRUE),
#            max_GFDL_ESM2G = max(GFDL_ESM2G, na.rm = TRUE),
#            min_GFDL_ESM2G = min(GFDL_ESM2G, na.rm = TRUE),
		avg_HadGEM2_CC365 = mean(HadGEM2_CC365, na.rm = TRUE),
#            sd_HadGEM2_CC365 = sd(HadGEM2_CC365, na.rm = TRUE),
#            max_HadGEM2_CC365 = max(HadGEM2_CC365, na.rm = TRUE),
#            min_HadGEM2_CC365 = min(HadGEM2_CC365, na.rm = TRUE),
		avg_HadGEM2_ES365 = mean(HadGEM2_ES365, na.rm = TRUE),
#            sd_HadGEM2_ES365 = sd(HadGEM2_ES365, na.rm = TRUE),
#            max_HadGEM2_ES365 = max(HadGEM2_ES365, na.rm = TRUE),
#            min_HadGEM2_ES365 = min(HadGEM2_ES365, na.rm = TRUE),
		avg_inmcm4 = mean(inmcm4, na.rm = TRUE),
#            sd_inmcm4 = sd(inmcm4, na.rm = TRUE),
#            max_inmcm4 = max(inmcm4, na.rm = TRUE),
#            min_inmcm4 = min(inmcm4, na.rm = TRUE),
		avg_IPSL_CM5A_LR = mean(IPSL_CM5A_LR, na.rm = TRUE),
#            sd_IPSL_CM5A_LR = sd(IPSL_CM5A_LR, na.rm = TRUE),
#            max_IPSL_CM5A_LR = max(IPSL_CM5A_LR, na.rm = TRUE),
#            min_IPSL_CM5A_LR = min(IPSL_CM5A_LR, na.rm = TRUE),
		avg_IPSL_CM5A_MR = mean(IPSL_CM5A_MR, na.rm = TRUE),
#            sd_IPSL_CM5A_MR = sd(IPSL_CM5A_MR, na.rm = TRUE),
#            max_IPSL_CM5A_MR = max(IPSL_CM5A_MR, na.rm = TRUE),
#            min_IPSL_CM5A_MR = min(IPSL_CM5A_MR, na.rm = TRUE),
		avg_IPSL_CM5B_LR = mean(IPSL_CM5B_LR, na.rm = TRUE),
#            sd_IPSL_CM5B_LR = sd(IPSL_CM5B_LR, na.rm = TRUE),
#            max_IPSL_CM5B_LR = max(IPSL_CM5B_LR, na.rm = TRUE),
#            min_IPSL_CM5B_LR = min(IPSL_CM5B_LR, na.rm = TRUE),
		avg_MIROC5 = mean(MIROC5, na.rm = TRUE),
#            sd_MIROC5 = sd(MIROC5, na.rm = TRUE),
#            max_MIROC5 = max(MIROC5, na.rm = TRUE),
#            min_MIROC5 = min(MIROC5, na.rm = TRUE),
		avg_MIROC_ESM = mean(MIROC_ESM, na.rm = TRUE),
#            sd_MIROC_ESM = sd(MIROC_ESM, na.rm = TRUE),
#            max_MIROC_ESM = max(MIROC_ESM, na.rm = TRUE),
#            min_MIROC_ESM = min(MIROC_ESM, na.rm = TRUE),
		avg_MIROC_ESM_CHEM = mean(MIROC_ESM_CHEM, na.rm = TRUE),
#            sd_MIROC_ESM_CHEM = sd(MIROC_ESM_CHEM, na.rm = TRUE),
#            max_MIROC_ESM_CHEM = max(MIROC_ESM_CHEM, na.rm = TRUE),
#            min_MIROC_ESM_CHEM = min(MIROC_ESM_CHEM, na.rm = TRUE),
		avg_MRI_CGCM3 = mean(MRI_CGCM3, na.rm = TRUE),
#            sd_MRI_CGCM3 = sd(MRI_CGCM3, na.rm = TRUE),
#            max_MRI_CGCM3 = max(MRI_CGCM3, na.rm = TRUE),
#            min_MRI_CGCM3 = min(MRI_CGCM3, na.rm = TRUE),
		avg_NorESM1_M = mean(NorESM1_M, na.rm = TRUE),
#            sd_NorESM1_M = sd(NorESM1_M, na.rm = TRUE),
#            max_NorESM1_M = max(NorESM1_M, na.rm = TRUE),
#            min_NorESM1_M = min(NorESM1_M, na.rm = TRUE)
)


AVG_MAX_85_Tualatin_AVG <- AVG_MAX_85_Tualatin %>%
  group_by(year) %>%
  summarize(avg_bcc_csm1_1 = mean(bcc_csm1_1, na.rm = TRUE),
#            sd_bcc_csm1_1 = sd(bcc_csm1_1, na.rm = TRUE),
#            max_bcc_csm1_1 = max(bcc_csm1_1, na.rm = TRUE),
#            min_bcc_csm1_1 = min(bcc_csm1_1, na.rm = TRUE),
		avg_bcc_csm1_1_m = mean(bcc_csm1_1_m, na.rm = TRUE),
#            sd_bcc_csm1_1_m = sd(bcc_csm1_1_m, na.rm = TRUE),
#            max_bcc_csm1_1_m = max(bcc_csm1_1_m, na.rm = TRUE),
#            min_bcc_csm1_1_m = min(bcc_csm1_1_m, na.rm = TRUE),
		avg_BNU_ESM = mean(BNU_ESM, na.rm = TRUE),
#            sd_BNU_ESM = sd(BNU_ESM, na.rm = TRUE),
#            max_BNU_ESM = max(BNU_ESM, na.rm = TRUE),
#            min_BNU_ESM = min(BNU_ESM, na.rm = TRUE),
		avg_CanESM2 = mean(CanESM2, na.rm = TRUE),
#            sd_CanESM2 = sd(CanESM2, na.rm = TRUE),
#            max_CanESM2 = max(CanESM2, na.rm = TRUE),
#            min_CanESM2 = min(CanESM2, na.rm = TRUE),
		avg_CCSM4 = mean(CCSM4, na.rm = TRUE),
#            sd_CCSM4 = sd(CCSM4, na.rm = TRUE),
#            max_CCSM4 = max(CCSM4, na.rm = TRUE),
#            min_CCSM4 = min(CCSM4, na.rm = TRUE),
		avg_CNRM_CM5 = mean(CNRM_CM5, na.rm = TRUE),
#            sd_CNRM_CM5 = sd(CNRM_CM5, na.rm = TRUE),
#            max_CNRM_CM5 = max(CNRM_CM5, na.rm = TRUE),
#            min_CNRM_CM5 = min(CNRM_CM5, na.rm = TRUE),
		avg_CSIRO_Mk_3_6_0 = mean(CSIRO_Mk_3_6_0, na.rm = TRUE),
#            sd_CSIRO_Mk_3_6_0 = sd(CSIRO_Mk_3_6_0, na.rm = TRUE),
#            max_CSIRO_Mk_3_6_0 = max(CSIRO_Mk_3_6_0, na.rm = TRUE),
#            min_CSIRO_Mk_3_6_0 = min(CSIRO_Mk_3_6_0, na.rm = TRUE),
		avg_GFDL_ESM2M = mean(GFDL_ESM2M, na.rm = TRUE),
#            sd_GFDL_ESM2M = sd(GFDL_ESM2M, na.rm = TRUE),
#            max_GFDL_ESM2M = max(GFDL_ESM2M, na.rm = TRUE),
#            min_GFDL_ESM2M = min(GFDL_ESM2M, na.rm = TRUE),
		avg_GFDL_ESM2G = mean(GFDL_ESM2G, na.rm = TRUE),
#            sd_GFDL_ESM2G = sd(GFDL_ESM2G, na.rm = TRUE),
#            max_GFDL_ESM2G = max(GFDL_ESM2G, na.rm = TRUE),
#            min_GFDL_ESM2G = min(GFDL_ESM2G, na.rm = TRUE),
		avg_HadGEM2_CC365 = mean(HadGEM2_CC365, na.rm = TRUE),
#            sd_HadGEM2_CC365 = sd(HadGEM2_CC365, na.rm = TRUE),
#            max_HadGEM2_CC365 = max(HadGEM2_CC365, na.rm = TRUE),
#            min_HadGEM2_CC365 = min(HadGEM2_CC365, na.rm = TRUE),
		avg_HadGEM2_ES365 = mean(HadGEM2_ES365, na.rm = TRUE),
#            sd_HadGEM2_ES365 = sd(HadGEM2_ES365, na.rm = TRUE),
#            max_HadGEM2_ES365 = max(HadGEM2_ES365, na.rm = TRUE),
#            min_HadGEM2_ES365 = min(HadGEM2_ES365, na.rm = TRUE),
		avg_inmcm4 = mean(inmcm4, na.rm = TRUE),
#            sd_inmcm4 = sd(inmcm4, na.rm = TRUE),
#            max_inmcm4 = max(inmcm4, na.rm = TRUE),
#            min_inmcm4 = min(inmcm4, na.rm = TRUE),
		avg_IPSL_CM5A_LR = mean(IPSL_CM5A_LR, na.rm = TRUE),
#            sd_IPSL_CM5A_LR = sd(IPSL_CM5A_LR, na.rm = TRUE),
#            max_IPSL_CM5A_LR = max(IPSL_CM5A_LR, na.rm = TRUE),
#            min_IPSL_CM5A_LR = min(IPSL_CM5A_LR, na.rm = TRUE),
		avg_IPSL_CM5A_MR = mean(IPSL_CM5A_MR, na.rm = TRUE),
#            sd_IPSL_CM5A_MR = sd(IPSL_CM5A_MR, na.rm = TRUE),
#            max_IPSL_CM5A_MR = max(IPSL_CM5A_MR, na.rm = TRUE),
#            min_IPSL_CM5A_MR = min(IPSL_CM5A_MR, na.rm = TRUE),
		avg_IPSL_CM5B_LR = mean(IPSL_CM5B_LR, na.rm = TRUE),
#            sd_IPSL_CM5B_LR = sd(IPSL_CM5B_LR, na.rm = TRUE),
#            max_IPSL_CM5B_LR = max(IPSL_CM5B_LR, na.rm = TRUE),
#            min_IPSL_CM5B_LR = min(IPSL_CM5B_LR, na.rm = TRUE),
		avg_MIROC5 = mean(MIROC5, na.rm = TRUE),
#            sd_MIROC5 = sd(MIROC5, na.rm = TRUE),
#            max_MIROC5 = max(MIROC5, na.rm = TRUE),
#            min_MIROC5 = min(MIROC5, na.rm = TRUE),
		avg_MIROC_ESM = mean(MIROC_ESM, na.rm = TRUE),
#            sd_MIROC_ESM = sd(MIROC_ESM, na.rm = TRUE),
#            max_MIROC_ESM = max(MIROC_ESM, na.rm = TRUE),
#            min_MIROC_ESM = min(MIROC_ESM, na.rm = TRUE),
		avg_MIROC_ESM_CHEM = mean(MIROC_ESM_CHEM, na.rm = TRUE),
#            sd_MIROC_ESM_CHEM = sd(MIROC_ESM_CHEM, na.rm = TRUE),
#            max_MIROC_ESM_CHEM = max(MIROC_ESM_CHEM, na.rm = TRUE),
#            min_MIROC_ESM_CHEM = min(MIROC_ESM_CHEM, na.rm = TRUE),
		avg_MRI_CGCM3 = mean(MRI_CGCM3, na.rm = TRUE),
#            sd_MRI_CGCM3 = sd(MRI_CGCM3, na.rm = TRUE),
#            max_MRI_CGCM3 = max(MRI_CGCM3, na.rm = TRUE),
#            min_MRI_CGCM3 = min(MRI_CGCM3, na.rm = TRUE),
		avg_NorESM1_M = mean(NorESM1_M, na.rm = TRUE),
#            sd_NorESM1_M = sd(NorESM1_M, na.rm = TRUE),
#            max_NorESM1_M = max(NorESM1_M, na.rm = TRUE),
#            min_NorESM1_M = min(NorESM1_M, na.rm = TRUE)
)


AVG_MAX_45_Lafayette_AVG <- AVG_MAX_45_Lafayette %>%
  group_by(year) %>%
  summarize(avg_bcc_csm1_1 = mean(bcc_csm1_1, na.rm = TRUE),
#            sd_bcc_csm1_1 = sd(bcc_csm1_1, na.rm = TRUE),
#            max_bcc_csm1_1 = max(bcc_csm1_1, na.rm = TRUE),
#            min_bcc_csm1_1 = min(bcc_csm1_1, na.rm = TRUE),
		avg_bcc_csm1_1_m = mean(bcc_csm1_1_m, na.rm = TRUE),
#            sd_bcc_csm1_1_m = sd(bcc_csm1_1_m, na.rm = TRUE),
#            max_bcc_csm1_1_m = max(bcc_csm1_1_m, na.rm = TRUE),
#            min_bcc_csm1_1_m = min(bcc_csm1_1_m, na.rm = TRUE),
		avg_BNU_ESM = mean(BNU_ESM, na.rm = TRUE),
#            sd_BNU_ESM = sd(BNU_ESM, na.rm = TRUE),
#            max_BNU_ESM = max(BNU_ESM, na.rm = TRUE),
#            min_BNU_ESM = min(BNU_ESM, na.rm = TRUE),
		avg_CanESM2 = mean(CanESM2, na.rm = TRUE),
#            sd_CanESM2 = sd(CanESM2, na.rm = TRUE),
#            max_CanESM2 = max(CanESM2, na.rm = TRUE),
#            min_CanESM2 = min(CanESM2, na.rm = TRUE),
		avg_CCSM4 = mean(CCSM4, na.rm = TRUE),
#            sd_CCSM4 = sd(CCSM4, na.rm = TRUE),
#            max_CCSM4 = max(CCSM4, na.rm = TRUE),
#            min_CCSM4 = min(CCSM4, na.rm = TRUE),
		avg_CNRM_CM5 = mean(CNRM_CM5, na.rm = TRUE),
#            sd_CNRM_CM5 = sd(CNRM_CM5, na.rm = TRUE),
#            max_CNRM_CM5 = max(CNRM_CM5, na.rm = TRUE),
#            min_CNRM_CM5 = min(CNRM_CM5, na.rm = TRUE),
		avg_CSIRO_Mk_3_6_0 = mean(CSIRO_Mk_3_6_0, na.rm = TRUE),
#            sd_CSIRO_Mk_3_6_0 = sd(CSIRO_Mk_3_6_0, na.rm = TRUE),
#            max_CSIRO_Mk_3_6_0 = max(CSIRO_Mk_3_6_0, na.rm = TRUE),
#            min_CSIRO_Mk_3_6_0 = min(CSIRO_Mk_3_6_0, na.rm = TRUE),
		avg_GFDL_ESM2M = mean(GFDL_ESM2M, na.rm = TRUE),
#            sd_GFDL_ESM2M = sd(GFDL_ESM2M, na.rm = TRUE),
#            max_GFDL_ESM2M = max(GFDL_ESM2M, na.rm = TRUE),
#            min_GFDL_ESM2M = min(GFDL_ESM2M, na.rm = TRUE),
		avg_GFDL_ESM2G = mean(GFDL_ESM2G, na.rm = TRUE),
#            sd_GFDL_ESM2G = sd(GFDL_ESM2G, na.rm = TRUE),
#            max_GFDL_ESM2G = max(GFDL_ESM2G, na.rm = TRUE),
#            min_GFDL_ESM2G = min(GFDL_ESM2G, na.rm = TRUE),
		avg_HadGEM2_CC365 = mean(HadGEM2_CC365, na.rm = TRUE),
#            sd_HadGEM2_CC365 = sd(HadGEM2_CC365, na.rm = TRUE),
#            max_HadGEM2_CC365 = max(HadGEM2_CC365, na.rm = TRUE),
#            min_HadGEM2_CC365 = min(HadGEM2_CC365, na.rm = TRUE),
		avg_HadGEM2_ES365 = mean(HadGEM2_ES365, na.rm = TRUE),
#            sd_HadGEM2_ES365 = sd(HadGEM2_ES365, na.rm = TRUE),
#            max_HadGEM2_ES365 = max(HadGEM2_ES365, na.rm = TRUE),
#            min_HadGEM2_ES365 = min(HadGEM2_ES365, na.rm = TRUE),
		avg_inmcm4 = mean(inmcm4, na.rm = TRUE),
#            sd_inmcm4 = sd(inmcm4, na.rm = TRUE),
#            max_inmcm4 = max(inmcm4, na.rm = TRUE),
#            min_inmcm4 = min(inmcm4, na.rm = TRUE),
		avg_IPSL_CM5A_LR = mean(IPSL_CM5A_LR, na.rm = TRUE),
#            sd_IPSL_CM5A_LR = sd(IPSL_CM5A_LR, na.rm = TRUE),
#            max_IPSL_CM5A_LR = max(IPSL_CM5A_LR, na.rm = TRUE),
#            min_IPSL_CM5A_LR = min(IPSL_CM5A_LR, na.rm = TRUE),
		avg_IPSL_CM5A_MR = mean(IPSL_CM5A_MR, na.rm = TRUE),
#            sd_IPSL_CM5A_MR = sd(IPSL_CM5A_MR, na.rm = TRUE),
#            max_IPSL_CM5A_MR = max(IPSL_CM5A_MR, na.rm = TRUE),
#            min_IPSL_CM5A_MR = min(IPSL_CM5A_MR, na.rm = TRUE),
		avg_IPSL_CM5B_LR = mean(IPSL_CM5B_LR, na.rm = TRUE),
#            sd_IPSL_CM5B_LR = sd(IPSL_CM5B_LR, na.rm = TRUE),
#            max_IPSL_CM5B_LR = max(IPSL_CM5B_LR, na.rm = TRUE),
#            min_IPSL_CM5B_LR = min(IPSL_CM5B_LR, na.rm = TRUE),
		avg_MIROC5 = mean(MIROC5, na.rm = TRUE),
#            sd_MIROC5 = sd(MIROC5, na.rm = TRUE),
#            max_MIROC5 = max(MIROC5, na.rm = TRUE),
#            min_MIROC5 = min(MIROC5, na.rm = TRUE),
		avg_MIROC_ESM = mean(MIROC_ESM, na.rm = TRUE),
#            sd_MIROC_ESM = sd(MIROC_ESM, na.rm = TRUE),
#            max_MIROC_ESM = max(MIROC_ESM, na.rm = TRUE),
#            min_MIROC_ESM = min(MIROC_ESM, na.rm = TRUE),
		avg_MIROC_ESM_CHEM = mean(MIROC_ESM_CHEM, na.rm = TRUE),
#            sd_MIROC_ESM_CHEM = sd(MIROC_ESM_CHEM, na.rm = TRUE),
#            max_MIROC_ESM_CHEM = max(MIROC_ESM_CHEM, na.rm = TRUE),
#            min_MIROC_ESM_CHEM = min(MIROC_ESM_CHEM, na.rm = TRUE),
		avg_MRI_CGCM3 = mean(MRI_CGCM3, na.rm = TRUE),
#            sd_MRI_CGCM3 = sd(MRI_CGCM3, na.rm = TRUE),
#            max_MRI_CGCM3 = max(MRI_CGCM3, na.rm = TRUE),
#            min_MRI_CGCM3 = min(MRI_CGCM3, na.rm = TRUE),
		avg_NorESM1_M = mean(NorESM1_M, na.rm = TRUE),
#            sd_NorESM1_M = sd(NorESM1_M, na.rm = TRUE),
#            max_NorESM1_M = max(NorESM1_M, na.rm = TRUE),
#            min_NorESM1_M = min(NorESM1_M, na.rm = TRUE)
)


AVG_MAX_85_Lafayette_AVG <- AVG_MAX_85_Lafayette %>%
  group_by(year) %>%
  summarize(avg_bcc_csm1_1 = mean(bcc_csm1_1, na.rm = TRUE),
#            sd_bcc_csm1_1 = sd(bcc_csm1_1, na.rm = TRUE),
#            max_bcc_csm1_1 = max(bcc_csm1_1, na.rm = TRUE),
#            min_bcc_csm1_1 = min(bcc_csm1_1, na.rm = TRUE),
		avg_bcc_csm1_1_m = mean(bcc_csm1_1_m, na.rm = TRUE),
#            sd_bcc_csm1_1_m = sd(bcc_csm1_1_m, na.rm = TRUE),
#            max_bcc_csm1_1_m = max(bcc_csm1_1_m, na.rm = TRUE),
#            min_bcc_csm1_1_m = min(bcc_csm1_1_m, na.rm = TRUE),
		avg_BNU_ESM = mean(BNU_ESM, na.rm = TRUE),
#            sd_BNU_ESM = sd(BNU_ESM, na.rm = TRUE),
#            max_BNU_ESM = max(BNU_ESM, na.rm = TRUE),
#            min_BNU_ESM = min(BNU_ESM, na.rm = TRUE),
		avg_CanESM2 = mean(CanESM2, na.rm = TRUE),
#            sd_CanESM2 = sd(CanESM2, na.rm = TRUE),
#            max_CanESM2 = max(CanESM2, na.rm = TRUE),
#            min_CanESM2 = min(CanESM2, na.rm = TRUE),
		avg_CCSM4 = mean(CCSM4, na.rm = TRUE),
#            sd_CCSM4 = sd(CCSM4, na.rm = TRUE),
#            max_CCSM4 = max(CCSM4, na.rm = TRUE),
#            min_CCSM4 = min(CCSM4, na.rm = TRUE),
		avg_CNRM_CM5 = mean(CNRM_CM5, na.rm = TRUE),
#            sd_CNRM_CM5 = sd(CNRM_CM5, na.rm = TRUE),
#            max_CNRM_CM5 = max(CNRM_CM5, na.rm = TRUE),
#            min_CNRM_CM5 = min(CNRM_CM5, na.rm = TRUE),
		avg_CSIRO_Mk_3_6_0 = mean(CSIRO_Mk_3_6_0, na.rm = TRUE),
#            sd_CSIRO_Mk_3_6_0 = sd(CSIRO_Mk_3_6_0, na.rm = TRUE),
#            max_CSIRO_Mk_3_6_0 = max(CSIRO_Mk_3_6_0, na.rm = TRUE),
#            min_CSIRO_Mk_3_6_0 = min(CSIRO_Mk_3_6_0, na.rm = TRUE),
		avg_GFDL_ESM2M = mean(GFDL_ESM2M, na.rm = TRUE),
#            sd_GFDL_ESM2M = sd(GFDL_ESM2M, na.rm = TRUE),
#            max_GFDL_ESM2M = max(GFDL_ESM2M, na.rm = TRUE),
#            min_GFDL_ESM2M = min(GFDL_ESM2M, na.rm = TRUE),
		avg_GFDL_ESM2G = mean(GFDL_ESM2G, na.rm = TRUE),
#            sd_GFDL_ESM2G = sd(GFDL_ESM2G, na.rm = TRUE),
#            max_GFDL_ESM2G = max(GFDL_ESM2G, na.rm = TRUE),
#            min_GFDL_ESM2G = min(GFDL_ESM2G, na.rm = TRUE),
		avg_HadGEM2_CC365 = mean(HadGEM2_CC365, na.rm = TRUE),
#            sd_HadGEM2_CC365 = sd(HadGEM2_CC365, na.rm = TRUE),
#            max_HadGEM2_CC365 = max(HadGEM2_CC365, na.rm = TRUE),
#            min_HadGEM2_CC365 = min(HadGEM2_CC365, na.rm = TRUE),
		avg_HadGEM2_ES365 = mean(HadGEM2_ES365, na.rm = TRUE),
#            sd_HadGEM2_ES365 = sd(HadGEM2_ES365, na.rm = TRUE),
#            max_HadGEM2_ES365 = max(HadGEM2_ES365, na.rm = TRUE),
#            min_HadGEM2_ES365 = min(HadGEM2_ES365, na.rm = TRUE),
		avg_inmcm4 = mean(inmcm4, na.rm = TRUE),
#            sd_inmcm4 = sd(inmcm4, na.rm = TRUE),
#            max_inmcm4 = max(inmcm4, na.rm = TRUE),
#            min_inmcm4 = min(inmcm4, na.rm = TRUE),
		avg_IPSL_CM5A_LR = mean(IPSL_CM5A_LR, na.rm = TRUE),
#            sd_IPSL_CM5A_LR = sd(IPSL_CM5A_LR, na.rm = TRUE),
#            max_IPSL_CM5A_LR = max(IPSL_CM5A_LR, na.rm = TRUE),
#            min_IPSL_CM5A_LR = min(IPSL_CM5A_LR, na.rm = TRUE),
		avg_IPSL_CM5A_MR = mean(IPSL_CM5A_MR, na.rm = TRUE),
#            sd_IPSL_CM5A_MR = sd(IPSL_CM5A_MR, na.rm = TRUE),
#            max_IPSL_CM5A_MR = max(IPSL_CM5A_MR, na.rm = TRUE),
#            min_IPSL_CM5A_MR = min(IPSL_CM5A_MR, na.rm = TRUE),
		avg_IPSL_CM5B_LR = mean(IPSL_CM5B_LR, na.rm = TRUE),
#            sd_IPSL_CM5B_LR = sd(IPSL_CM5B_LR, na.rm = TRUE),
#            max_IPSL_CM5B_LR = max(IPSL_CM5B_LR, na.rm = TRUE),
#            min_IPSL_CM5B_LR = min(IPSL_CM5B_LR, na.rm = TRUE),
		avg_MIROC5 = mean(MIROC5, na.rm = TRUE),
#            sd_MIROC5 = sd(MIROC5, na.rm = TRUE),
#            max_MIROC5 = max(MIROC5, na.rm = TRUE),
#            min_MIROC5 = min(MIROC5, na.rm = TRUE),
		avg_MIROC_ESM = mean(MIROC_ESM, na.rm = TRUE),
#            sd_MIROC_ESM = sd(MIROC_ESM, na.rm = TRUE),
#            max_MIROC_ESM = max(MIROC_ESM, na.rm = TRUE),
#            min_MIROC_ESM = min(MIROC_ESM, na.rm = TRUE),
		avg_MIROC_ESM_CHEM = mean(MIROC_ESM_CHEM, na.rm = TRUE),
#            sd_MIROC_ESM_CHEM = sd(MIROC_ESM_CHEM, na.rm = TRUE),
#            max_MIROC_ESM_CHEM = max(MIROC_ESM_CHEM, na.rm = TRUE),
#            min_MIROC_ESM_CHEM = min(MIROC_ESM_CHEM, na.rm = TRUE),
		avg_MRI_CGCM3 = mean(MRI_CGCM3, na.rm = TRUE),
#            sd_MRI_CGCM3 = sd(MRI_CGCM3, na.rm = TRUE),
#            max_MRI_CGCM3 = max(MRI_CGCM3, na.rm = TRUE),
#            min_MRI_CGCM3 = min(MRI_CGCM3, na.rm = TRUE),
		avg_NorESM1_M = mean(NorESM1_M, na.rm = TRUE),
#            sd_NorESM1_M = sd(NorESM1_M, na.rm = TRUE),
#            max_NorESM1_M = max(NorESM1_M, na.rm = TRUE),
#            min_NorESM1_M = min(NorESM1_M, na.rm = TRUE)
)


# OR_ALL_410670005_DAY_SUM all 2017 is missing due to missing PM25
# OR_ALL_410510080_DAY_SUM

OR_ALL_410510080_DAY_SUM_T <- OR_ALL_410510080_DAY_SUM %>% select(year,month,dom,avg_T,max_T,min_T,sd_T) %>% filter(!is.infinite(max_T))
OR_ALL_410510080_DAY_SUM_WS <- OR_ALL_410510080_DAY_SUM %>% select(year,month,dom,avg_WS,max_WS,min_WS,sd_WS) %>% filter(!is.infinite(max_WS))
OR_ALL_410510080_DAY_SUM_WX <- OR_ALL_410510080_DAY_SUM %>% select(year,month,dom,avg_WX,max_WX,min_WX,sd_WX) %>% filter(!is.infinite(max_WX))
OR_ALL_410510080_DAY_SUM_WY <- OR_ALL_410510080_DAY_SUM %>% select(year,month,dom,avg_WY,max_WY,min_WY,sd_WY) %>% filter(!is.infinite(max_WY))
OR_ALL_410510080_DAY_SUM_NO2 <- OR_ALL_410510080_DAY_SUM %>% select(year,month,dom,avg_NO2,max_NO2,min_NO2,sd_NO2) %>% filter(!is.infinite(max_NO2))
OR_ALL_410510080_DAY_SUM_NO <- OR_ALL_410510080_DAY_SUM %>% select(year,month,dom,avg_NO,max_NO,min_NO,sd_NO) %>% filter(!is.infinite(max_NO))
OR_ALL_410510080_DAY_SUM_O3 <- OR_ALL_410510080_DAY_SUM %>% select(year,month,dom,avg_O3,max_O3,min_O3,sd_O3) %>% filter(!is.infinite(max_O3))
OR_ALL_410510080_DAY_SUM_CO <- OR_ALL_410510080_DAY_SUM %>% select(year,month,dom,avg_CO,max_CO,min_CO,sd_CO) %>% filter(!is.infinite(max_CO))
OR_ALL_410510080_DAY_SUM_PM25 <- OR_ALL_410510080_DAY_SUM %>% select(year,month,dom,avg_PM25,max_PM25,min_PM25,sd_PM25) %>% filter(!is.infinite(max_PM25))

OR_ALL_410670005_DAY_SUM_T <- OR_ALL_410670005_DAY_SUM %>% select(year,month,dom,avg_T,max_T,min_T,sd_T) %>% filter(!is.infinite(max_T))
OR_ALL_410670005_DAY_SUM_WS <- OR_ALL_410670005_DAY_SUM %>% select(year,month,dom,avg_WS,max_WS,min_WS,sd_WS) %>% filter(!is.infinite(max_WS))
OR_ALL_410670005_DAY_SUM_WX <- OR_ALL_410670005_DAY_SUM %>% select(year,month,dom,avg_WX,max_WX,min_WX,sd_WX) %>% filter(!is.infinite(max_WX))
OR_ALL_410670005_DAY_SUM_WY <- OR_ALL_410670005_DAY_SUM %>% select(year,month,dom,avg_WY,max_WY,min_WY,sd_WY) %>% filter(!is.infinite(max_WY))
OR_ALL_410670005_DAY_SUM_NO2 <- OR_ALL_410670005_DAY_SUM %>% select(year,month,dom,avg_NO2,max_NO2,min_NO2,sd_NO2) %>% filter(!is.infinite(max_NO2))
OR_ALL_410670005_DAY_SUM_NO <- OR_ALL_410670005_DAY_SUM %>% select(year,month,dom,avg_NO,max_NO,min_NO,sd_NO) %>% filter(!is.infinite(max_NO))
OR_ALL_410670005_DAY_SUM_O3 <- OR_ALL_410670005_DAY_SUM %>% select(year,month,dom,avg_O3,max_O3,min_O3,sd_O3) %>% filter(!is.infinite(max_O3))
OR_ALL_410670005_DAY_SUM_CO <- OR_ALL_410670005_DAY_SUM %>% select(year,month,dom,avg_CO,max_CO,min_CO,sd_CO) %>% filter(!is.infinite(max_CO))
OR_ALL_410670005_DAY_SUM_PM25 <- OR_ALL_410670005_DAY_SUM %>% select(year,month,dom,avg_PM25,max_PM25,min_PM25,sd_PM25) %>% filter(!is.infinite(max_PM25))

OR_ALL_410510080_DAY_SUM_merge <- merge(x=OR_ALL_410510080_DAY_SUM_T, y=OR_ALL_410510080_DAY_SUM_WS, by = c("year", "month", "dom"), all = TRUE)
OR_ALL_410510080_DAY_SUM_merge <- merge(x=OR_ALL_410510080_DAY_SUM_merge, y=OR_ALL_410510080_DAY_SUM_WX, by = c("year", "month", "dom"), all = TRUE)
OR_ALL_410510080_DAY_SUM_merge <- merge(x=OR_ALL_410510080_DAY_SUM_merge, y=OR_ALL_410510080_DAY_SUM_WY, by = c("year", "month", "dom"), all = TRUE)
OR_ALL_410510080_DAY_SUM_merge <- merge(x=OR_ALL_410510080_DAY_SUM_merge, y=OR_ALL_410510080_DAY_SUM_NO2, by = c("year", "month", "dom"), all = TRUE)
OR_ALL_410510080_DAY_SUM_merge <- merge(x=OR_ALL_410510080_DAY_SUM_merge, y=OR_ALL_410510080_DAY_SUM_NO, by = c("year", "month", "dom"), all = TRUE)
OR_ALL_410510080_DAY_SUM_merge <- merge(x=OR_ALL_410510080_DAY_SUM_merge, y=OR_ALL_410510080_DAY_SUM_CO, by = c("year", "month", "dom"), all = TRUE)
OR_ALL_410510080_DAY_SUM_merge <- merge(x=OR_ALL_410510080_DAY_SUM_merge, y=OR_ALL_410510080_DAY_SUM_O3, by = c("year", "month", "dom"), all = TRUE)
OR_ALL_410510080_DAY_SUM_merge <- merge(x=OR_ALL_410510080_DAY_SUM_merge, y=OR_ALL_410510080_DAY_SUM_PM25, by = c("year", "month", "dom"), all = TRUE)

OR_ALL_410670005_DAY_SUM_merge <- merge(x=OR_ALL_410670005_DAY_SUM_T, y=OR_ALL_410670005_DAY_SUM_WS, by = c("year", "month", "dom"), all = TRUE)
OR_ALL_410670005_DAY_SUM_merge <- merge(x=OR_ALL_410670005_DAY_SUM_merge, y=OR_ALL_410670005_DAY_SUM_WX, by = c("year", "month", "dom"), all = TRUE)
OR_ALL_410670005_DAY_SUM_merge <- merge(x=OR_ALL_410670005_DAY_SUM_merge, y=OR_ALL_410670005_DAY_SUM_WY, by = c("year", "month", "dom"), all = TRUE)
OR_ALL_410670005_DAY_SUM_merge <- merge(x=OR_ALL_410670005_DAY_SUM_merge, y=OR_ALL_410670005_DAY_SUM_NO2, by = c("year", "month", "dom"), all = TRUE)
OR_ALL_410670005_DAY_SUM_merge <- merge(x=OR_ALL_410670005_DAY_SUM_merge, y=OR_ALL_410670005_DAY_SUM_NO, by = c("year", "month", "dom"), all = TRUE)
OR_ALL_410670005_DAY_SUM_merge <- merge(x=OR_ALL_410670005_DAY_SUM_merge, y=OR_ALL_410670005_DAY_SUM_CO, by = c("year", "month", "dom"), all = TRUE)
OR_ALL_410670005_DAY_SUM_merge <- merge(x=OR_ALL_410670005_DAY_SUM_merge, y=OR_ALL_410670005_DAY_SUM_O3, by = c("year", "month", "dom"), all = TRUE)
OR_ALL_410670005_DAY_SUM_merge <- merge(x=OR_ALL_410670005_DAY_SUM_merge, y=OR_ALL_410670005_DAY_SUM_PM25, by = c("year", "month", "dom"), all = TRUE)


OR_ALL_410670005_DAY_SUM_YEAR <- OR_ALL_410670005_DAY_SUM_merge %>%
  group_by(year) %>%
  summarize(avg_max_T = mean(max_T, na.rm = TRUE),
		avg_avg_WS = mean(avg_WS, na.rm = TRUE),
		avg_max_O3 = mean(max_O3, na.rm = TRUE),
		avg_max_NO = mean(max_NO, na.rm = TRUE),
		avg_max_NO2 = mean(max_NO2, na.rm = TRUE),
		avg_avg_CO = mean(avg_CO, na.rm = TRUE),
		avg_avg_PM25 = mean(avg_PM25, na.rm = TRUE),
		avg_avg_WX = mean(avg_WX, na.rm = TRUE),
		avg_avg_WY = mean(avg_WY, na.rm = TRUE))

OR_ALL_410510080_DAY_SUM_YEAR <- OR_ALL_410510080_DAY_SUM_merge %>%
  group_by(year) %>%
  summarize(avg_max_T = mean(max_T, na.rm = TRUE),
		avg_avg_WS = mean(avg_WS, na.rm = TRUE),
		avg_max_O3 = mean(max_O3, na.rm = TRUE),
		avg_max_NO = mean(max_NO, na.rm = TRUE),
		avg_max_NO2 = mean(max_NO2, na.rm = TRUE),
		avg_avg_CO = mean(avg_CO, na.rm = TRUE),
		avg_avg_PM25 = mean(avg_PM25, na.rm = TRUE),
		avg_avg_WX = mean(avg_WX, na.rm = TRUE),
		avg_avg_WY = mean(avg_WY, na.rm = TRUE))

#OR_ALL_410670005_DAY_SUM_YEAR <- OR_ALL_410670005_DAY_SUM_YEAR %>% filter(year != 2020)
#OR_ALL_410510080_DAY_SUM_YEAR <- OR_ALL_410510080_DAY_SUM_YEAR %>% filter(year != 2020)
#Should I include 2020 or not in my baseline??? I currently include it

OR_ALL_410670005_DAY_SUM_YEAR_AVG <- OR_ALL_410670005_DAY_SUM_YEAR %>%
  summarize(max_T = mean(avg_max_T, na.rm = TRUE),
		avg_WS = mean(avg_avg_WS, na.rm = TRUE),
		max_O3 = mean(avg_max_O3, na.rm = TRUE),
		max_NO = mean(avg_max_NO, na.rm = TRUE),
		max_NO2 = mean(avg_max_NO2, na.rm = TRUE),
		avg_CO = mean(avg_avg_CO, na.rm = TRUE),
		avg_PM25 = mean(avg_avg_PM25, na.rm = TRUE),
		avg_WX = mean(avg_avg_WX, na.rm = TRUE),
		avg_WY = mean(avg_avg_WY, na.rm = TRUE))

OR_ALL_410510080_DAY_SUM_YEAR_AVG <- OR_ALL_410510080_DAY_SUM_YEAR %>%
  summarize(max_T = mean(avg_max_T, na.rm = TRUE),
		avg_WS = mean(avg_avg_WS, na.rm = TRUE),
		max_O3 = mean(avg_max_O3, na.rm = TRUE),
		max_NO = mean(avg_max_NO, na.rm = TRUE),
		max_NO2 = mean(avg_max_NO2, na.rm = TRUE),
		avg_CO = mean(avg_avg_CO, na.rm = TRUE),
		avg_PM25 = mean(avg_avg_PM25, na.rm = TRUE),
		avg_WX = mean(avg_avg_WX, na.rm = TRUE),
		avg_WY = mean(avg_avg_WY, na.rm = TRUE))

OR_ALL_410670005_DAY_SUM_YEAR_AVG <- OR_ALL_410670005_DAY_SUM_YEAR_AVG %>% select(max_T,avg_WS,max_NO,max_NO2,avg_CO,avg_PM25,avg_WX,avg_WY)
Tualatin_base_r <- predict(res_410670005, newdata = OR_ALL_410670005_DAY_SUM_YEAR_AVG)

OR_ALL_410510080_DAY_SUM_YEAR_AVG <- OR_ALL_410510080_DAY_SUM_YEAR_AVG %>% select(max_T,avg_WS,max_NO,max_NO2,avg_CO,avg_PM25,avg_WX,avg_WY)
Lafayette_base_r <- predict(res_410510080, newdata = OR_ALL_410510080_DAY_SUM_YEAR_AVG)


#write.csv(OR_ALL_410670005_DAY_SUM_YEAR_AVG ,file="C://Users/treeo/OneDrive/Documents/R/data/Climate/OR_ALL_410670005_DAY_SUM_YEAR_AVGoMAT.csv")
#write.csv(OR_ALL_410510080_DAY_SUM_YEAR_AVG ,file="C://Users/treeo/OneDrive/Documents/R/data/Climate/OR_ALL_410510080_DAY_SUM_YEAR_AVGoMAT.csv")
#write.csv(AVG_MAX_45_Tualatin_AVG ,file="C://Users/treeo/OneDrive/Documents/R/data/Climate/AVG_MAX_45_Tualatin_AVGoMAT.csv")
#write.csv(AVG_MAX_85_Tualatin_AVG ,file="C://Users/treeo/OneDrive/Documents/R/data/Climate/AVG_MAX_85_Tualatin_AVGoMAT.csv")
#write.csv(AVG_MAX_45_Lafayette_AVG ,file="C://Users/treeo/OneDrive/Documents/R/data/Climate/AVG_MAX_45_Lafayette_AVGoMAT.csv")
#write.csv(AVG_MAX_85_Lafayette_AVG ,file="C://Users/treeo/OneDrive/Documents/R/data/Climate/AVG_MAX_85_Lafayette_AVGoMAT.csv")

AVG_MAX_45_Tualatin_AVG <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/Climate/AVG_MAX_45_Tualatin_AVGoR.csv", stringsAsFactors = FALSE, header=FALSE)
AVG_MAX_85_Tualatin_AVG <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/Climate/AVG_MAX_85_Tualatin_AVGoR.csv", stringsAsFactors = FALSE, header=FALSE)
AVG_MAX_45_Lafayette_AVG <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/Climate/AVG_MAX_45_Lafayette_AVGoR.csv", stringsAsFactors = FALSE, header=FALSE)
AVG_MAX_85_Lafayette_AVG <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/Climate/AVG_MAX_85_Lafayette_AVGoR.csv", stringsAsFactors = FALSE, header=FALSE)

colnames(AVG_MAX_45_Tualatin_AVG) <- c("year","bcc_csm1_1","bcc_csm1_1_m","BNU_ESM","CanESM2","CCSM4","CNRM_CM5","CSIRO_Mk_3_6_0","GFDL_ESM2M","GFDL_ESM2G","HadGEM2_CC365","HadGEM2_ES365","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC5","MIROC_ESM","MIROC_ESM_CHEM","MRI_CGCM3","NorESM1_M","avg_WS","max_NO","max_NO2","avg_CO","avg_PM25","avg_WX","avg_WY")
colnames(AVG_MAX_85_Tualatin_AVG) <- c("year","bcc_csm1_1","bcc_csm1_1_m","BNU_ESM","CanESM2","CCSM4","CNRM_CM5","CSIRO_Mk_3_6_0","GFDL_ESM2M","GFDL_ESM2G","HadGEM2_CC365","HadGEM2_ES365","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC5","MIROC_ESM","MIROC_ESM_CHEM","MRI_CGCM3","NorESM1_M","avg_WS","max_NO","max_NO2","avg_CO","avg_PM25","avg_WX","avg_WY")
colnames(AVG_MAX_45_Lafayette_AVG) <- c("year","bcc_csm1_1","bcc_csm1_1_m","BNU_ESM","CanESM2","CCSM4","CNRM_CM5","CSIRO_Mk_3_6_0","GFDL_ESM2M","GFDL_ESM2G","HadGEM2_CC365","HadGEM2_ES365","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC5","MIROC_ESM","MIROC_ESM_CHEM","MRI_CGCM3","NorESM1_M","avg_WS","max_NO","max_NO2","avg_CO","avg_PM25","avg_WX","avg_WY")
colnames(AVG_MAX_85_Lafayette_AVG) <- c("year","bcc_csm1_1","bcc_csm1_1_m","BNU_ESM","CanESM2","CCSM4","CNRM_CM5","CSIRO_Mk_3_6_0","GFDL_ESM2M","GFDL_ESM2G","HadGEM2_CC365","HadGEM2_ES365","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC5","MIROC_ESM","MIROC_ESM_CHEM","MRI_CGCM3","NorESM1_M","avg_WS","max_NO","max_NO2","avg_CO","avg_PM25","avg_WX","avg_WY")

##Regression
#AVG_MAX_45_Tualatin_AVG
AVG_MAX_45_Tualatin_AVG_r <- data.frame(matrix(ncol=9,nrow=41))
colnames(AVG_MAX_45_Tualatin_AVG_r) <- c("year","avg_WS","max_NO","max_NO2","avg_CO","avg_PM25","avg_WX","avg_WY","max_T")
AVG_MAX_45_Tualatin_AVG_r$year <- AVG_MAX_45_Tualatin_AVG$year
AVG_MAX_45_Tualatin_AVG_r$avg_WS <- AVG_MAX_45_Tualatin_AVG$avg_WS
AVG_MAX_45_Tualatin_AVG_r$max_NO <- AVG_MAX_45_Tualatin_AVG$max_NO
AVG_MAX_45_Tualatin_AVG_r$max_NO2 <- AVG_MAX_45_Tualatin_AVG$max_NO2
AVG_MAX_45_Tualatin_AVG_r$avg_CO <- AVG_MAX_45_Tualatin_AVG$avg_CO
AVG_MAX_45_Tualatin_AVG_r$avg_PM25 <- AVG_MAX_45_Tualatin_AVG$avg_PM25
AVG_MAX_45_Tualatin_AVG_r$avg_WX <- AVG_MAX_45_Tualatin_AVG$avg_WX
AVG_MAX_45_Tualatin_AVG_r$avg_WY <- AVG_MAX_45_Tualatin_AVG$avg_WY

Tualatin_45_r <- data.frame(matrix(ncol = 21, nrow=41))
colnames(Tualatin_45_r) <- c("year","bcc_csm1_1","bcc_csm1_1_m","BNU_ESM","CanESM2","CCSM4","CNRM_CM5","CSIRO_Mk_3_6_0","GFDL_ESM2M","GFDL_ESM2G","HadGEM2_CC365","HadGEM2_ES365","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC5","MIROC_ESM","MIROC_ESM_CHEM","MRI_CGCM3","NorESM1_M")
Tualatin_45_r$year <- AVG_MAX_45_Tualatin_AVG$year
	
AVG_MAX_45_Tualatin_AVG_r$max_T <- AVG_MAX_45_Tualatin_AVG$bcc_csm1_1
Tualatin_45_r$bcc_csm1_1 <- predict(res_410670005, newdata = AVG_MAX_45_Tualatin_AVG_r)
AVG_MAX_45_Tualatin_AVG_r$max_T <- AVG_MAX_45_Tualatin_AVG$bcc_csm1_1_m
Tualatin_45_r$bcc_csm1_1_m <- predict(res_410670005, newdata = AVG_MAX_45_Tualatin_AVG_r)
AVG_MAX_45_Tualatin_AVG_r$max_T <- AVG_MAX_45_Tualatin_AVG$BNU_ESM
Tualatin_45_r$BNU_ESM <- predict(res_410670005, newdata = AVG_MAX_45_Tualatin_AVG_r)
AVG_MAX_45_Tualatin_AVG_r$max_T <- AVG_MAX_45_Tualatin_AVG$CanESM2
Tualatin_45_r$CanESM2 <- predict(res_410670005, newdata = AVG_MAX_45_Tualatin_AVG_r)
AVG_MAX_45_Tualatin_AVG_r$max_T <- AVG_MAX_45_Tualatin_AVG$CCSM4
Tualatin_45_r$CCSM4 <- predict(res_410670005, newdata = AVG_MAX_45_Tualatin_AVG_r)
AVG_MAX_45_Tualatin_AVG_r$max_T <- AVG_MAX_45_Tualatin_AVG$CNRM_CM5
Tualatin_45_r$CNRM_CM5 <- predict(res_410670005, newdata = AVG_MAX_45_Tualatin_AVG_r)
AVG_MAX_45_Tualatin_AVG_r$max_T <- AVG_MAX_45_Tualatin_AVG$CSIRO_Mk_3_6_0
Tualatin_45_r$CSIRO_Mk_3_6_0 <- predict(res_410670005, newdata = AVG_MAX_45_Tualatin_AVG_r)
AVG_MAX_45_Tualatin_AVG_r$max_T <- AVG_MAX_45_Tualatin_AVG$GFDL_ESM2M
Tualatin_45_r$GFDL_ESM2M <- predict(res_410670005, newdata = AVG_MAX_45_Tualatin_AVG_r)
AVG_MAX_45_Tualatin_AVG_r$max_T <- AVG_MAX_45_Tualatin_AVG$GFDL_ESM2G
Tualatin_45_r$GFDL_ESM2G <- predict(res_410670005, newdata = AVG_MAX_45_Tualatin_AVG_r)
AVG_MAX_45_Tualatin_AVG_r$max_T <- AVG_MAX_45_Tualatin_AVG$HadGEM2_CC365
Tualatin_45_r$HadGEM2_CC365 <- predict(res_410670005, newdata = AVG_MAX_45_Tualatin_AVG_r)
AVG_MAX_45_Tualatin_AVG_r$max_T <- AVG_MAX_45_Tualatin_AVG$HadGEM2_ES365
Tualatin_45_r$HadGEM2_ES365 <- predict(res_410670005, newdata = AVG_MAX_45_Tualatin_AVG_r)
AVG_MAX_45_Tualatin_AVG_r$max_T <- AVG_MAX_45_Tualatin_AVG$inmcm4
Tualatin_45_r$inmcm4 <- predict(res_410670005, newdata = AVG_MAX_45_Tualatin_AVG_r)
AVG_MAX_45_Tualatin_AVG_r$max_T <- AVG_MAX_45_Tualatin_AVG$IPSL_CM5A_LR
Tualatin_45_r$IPSL_CM5A_LR <- predict(res_410670005, newdata = AVG_MAX_45_Tualatin_AVG_r)
AVG_MAX_45_Tualatin_AVG_r$max_T <- AVG_MAX_45_Tualatin_AVG$IPSL_CM5A_MR
Tualatin_45_r$IPSL_CM5A_MR <- predict(res_410670005, newdata = AVG_MAX_45_Tualatin_AVG_r)
AVG_MAX_45_Tualatin_AVG_r$max_T <- AVG_MAX_45_Tualatin_AVG$IPSL_CM5B_LR
Tualatin_45_r$IPSL_CM5B_LR <- predict(res_410670005, newdata = AVG_MAX_45_Tualatin_AVG_r)
AVG_MAX_45_Tualatin_AVG_r$max_T <- AVG_MAX_45_Tualatin_AVG$MIROC5
Tualatin_45_r$MIROC5 <- predict(res_410670005, newdata = AVG_MAX_45_Tualatin_AVG_r)
AVG_MAX_45_Tualatin_AVG_r$max_T <- AVG_MAX_45_Tualatin_AVG$MIROC_ESM
Tualatin_45_r$MIROC_ESM <- predict(res_410670005, newdata = AVG_MAX_45_Tualatin_AVG_r)
AVG_MAX_45_Tualatin_AVG_r$max_T <- AVG_MAX_45_Tualatin_AVG$MIROC_ESM_CHEM
Tualatin_45_r$MIROC_ESM_CHEM <- predict(res_410670005, newdata = AVG_MAX_45_Tualatin_AVG_r)
AVG_MAX_45_Tualatin_AVG_r$max_T <- AVG_MAX_45_Tualatin_AVG$MRI_CGCM3
Tualatin_45_r$MRI_CGCM3 <- predict(res_410670005, newdata = AVG_MAX_45_Tualatin_AVG_r)
AVG_MAX_45_Tualatin_AVG_r$max_T <- AVG_MAX_45_Tualatin_AVG$NorESM1_M
Tualatin_45_r$NorESM1_M <- predict(res_410670005, newdata = AVG_MAX_45_Tualatin_AVG_r)

#AVG_MAX_85_Tualatin_AVG
AVG_MAX_85_Tualatin_AVG_r <- data.frame(matrix(ncol=9,nrow=41))
colnames(AVG_MAX_85_Tualatin_AVG_r) <- c("year","avg_WS","max_NO","max_NO2","avg_CO","avg_PM25","avg_WX","avg_WY","max_T")
AVG_MAX_85_Tualatin_AVG_r$year <- AVG_MAX_85_Tualatin_AVG$year
AVG_MAX_85_Tualatin_AVG_r$avg_WS <- AVG_MAX_85_Tualatin_AVG$avg_WS
AVG_MAX_85_Tualatin_AVG_r$max_NO <- AVG_MAX_85_Tualatin_AVG$max_NO
AVG_MAX_85_Tualatin_AVG_r$max_NO2 <- AVG_MAX_85_Tualatin_AVG$max_NO2
AVG_MAX_85_Tualatin_AVG_r$avg_CO <- AVG_MAX_85_Tualatin_AVG$avg_CO
AVG_MAX_85_Tualatin_AVG_r$avg_PM25 <- AVG_MAX_85_Tualatin_AVG$avg_PM25
AVG_MAX_85_Tualatin_AVG_r$avg_WX <- AVG_MAX_85_Tualatin_AVG$avg_WX
AVG_MAX_85_Tualatin_AVG_r$avg_WY <- AVG_MAX_85_Tualatin_AVG$avg_WY

Tualatin_85_r <- data.frame(matrix(ncol = 21, nrow=41))
colnames(Tualatin_85_r) <- c("year","bcc_csm1_1","bcc_csm1_1_m","BNU_ESM","CanESM2","CCSM4","CNRM_CM5","CSIRO_Mk_3_6_0","GFDL_ESM2M","GFDL_ESM2G","HadGEM2_CC365","HadGEM2_ES365","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC5","MIROC_ESM","MIROC_ESM_CHEM","MRI_CGCM3","NorESM1_M")
Tualatin_85_r$year <- AVG_MAX_85_Tualatin_AVG$year
	
AVG_MAX_85_Tualatin_AVG_r$max_T <- AVG_MAX_85_Tualatin_AVG$bcc_csm1_1
Tualatin_85_r$bcc_csm1_1 <- predict(res_410670005, newdata = AVG_MAX_85_Tualatin_AVG_r)
AVG_MAX_85_Tualatin_AVG_r$max_T <- AVG_MAX_85_Tualatin_AVG$bcc_csm1_1_m
Tualatin_85_r$bcc_csm1_1_m <- predict(res_410670005, newdata = AVG_MAX_85_Tualatin_AVG_r)
AVG_MAX_85_Tualatin_AVG_r$max_T <- AVG_MAX_85_Tualatin_AVG$BNU_ESM
Tualatin_85_r$BNU_ESM <- predict(res_410670005, newdata = AVG_MAX_85_Tualatin_AVG_r)
AVG_MAX_85_Tualatin_AVG_r$max_T <- AVG_MAX_85_Tualatin_AVG$CanESM2
Tualatin_85_r$CanESM2 <- predict(res_410670005, newdata = AVG_MAX_85_Tualatin_AVG_r)
AVG_MAX_85_Tualatin_AVG_r$max_T <- AVG_MAX_85_Tualatin_AVG$CCSM4
Tualatin_85_r$CCSM4 <- predict(res_410670005, newdata = AVG_MAX_85_Tualatin_AVG_r)
AVG_MAX_85_Tualatin_AVG_r$max_T <- AVG_MAX_85_Tualatin_AVG$CNRM_CM5
Tualatin_85_r$CNRM_CM5 <- predict(res_410670005, newdata = AVG_MAX_85_Tualatin_AVG_r)
AVG_MAX_85_Tualatin_AVG_r$max_T <- AVG_MAX_85_Tualatin_AVG$CSIRO_Mk_3_6_0
Tualatin_85_r$CSIRO_Mk_3_6_0 <- predict(res_410670005, newdata = AVG_MAX_85_Tualatin_AVG_r)
AVG_MAX_85_Tualatin_AVG_r$max_T <- AVG_MAX_85_Tualatin_AVG$GFDL_ESM2M
Tualatin_85_r$GFDL_ESM2M <- predict(res_410670005, newdata = AVG_MAX_85_Tualatin_AVG_r)
AVG_MAX_85_Tualatin_AVG_r$max_T <- AVG_MAX_85_Tualatin_AVG$GFDL_ESM2G
Tualatin_85_r$GFDL_ESM2G <- predict(res_410670005, newdata = AVG_MAX_85_Tualatin_AVG_r)
AVG_MAX_85_Tualatin_AVG_r$max_T <- AVG_MAX_85_Tualatin_AVG$HadGEM2_CC365
Tualatin_85_r$HadGEM2_CC365 <- predict(res_410670005, newdata = AVG_MAX_85_Tualatin_AVG_r)
AVG_MAX_85_Tualatin_AVG_r$max_T <- AVG_MAX_85_Tualatin_AVG$HadGEM2_ES365
Tualatin_85_r$HadGEM2_ES365 <- predict(res_410670005, newdata = AVG_MAX_85_Tualatin_AVG_r)
AVG_MAX_85_Tualatin_AVG_r$max_T <- AVG_MAX_85_Tualatin_AVG$inmcm4
Tualatin_85_r$inmcm4 <- predict(res_410670005, newdata = AVG_MAX_85_Tualatin_AVG_r)
AVG_MAX_85_Tualatin_AVG_r$max_T <- AVG_MAX_85_Tualatin_AVG$IPSL_CM5A_LR
Tualatin_85_r$IPSL_CM5A_LR <- predict(res_410670005, newdata = AVG_MAX_85_Tualatin_AVG_r)
AVG_MAX_85_Tualatin_AVG_r$max_T <- AVG_MAX_85_Tualatin_AVG$IPSL_CM5A_MR
Tualatin_85_r$IPSL_CM5A_MR <- predict(res_410670005, newdata = AVG_MAX_85_Tualatin_AVG_r)
AVG_MAX_85_Tualatin_AVG_r$max_T <- AVG_MAX_85_Tualatin_AVG$IPSL_CM5B_LR
Tualatin_85_r$IPSL_CM5B_LR <- predict(res_410670005, newdata = AVG_MAX_85_Tualatin_AVG_r)
AVG_MAX_85_Tualatin_AVG_r$max_T <- AVG_MAX_85_Tualatin_AVG$MIROC5
Tualatin_85_r$MIROC5 <- predict(res_410670005, newdata = AVG_MAX_85_Tualatin_AVG_r)
AVG_MAX_85_Tualatin_AVG_r$max_T <- AVG_MAX_85_Tualatin_AVG$MIROC_ESM
Tualatin_85_r$MIROC_ESM <- predict(res_410670005, newdata = AVG_MAX_85_Tualatin_AVG_r)
AVG_MAX_85_Tualatin_AVG_r$max_T <- AVG_MAX_85_Tualatin_AVG$MIROC_ESM_CHEM
Tualatin_85_r$MIROC_ESM_CHEM <- predict(res_410670005, newdata = AVG_MAX_85_Tualatin_AVG_r)
AVG_MAX_85_Tualatin_AVG_r$max_T <- AVG_MAX_85_Tualatin_AVG$MRI_CGCM3
Tualatin_85_r$MRI_CGCM3 <- predict(res_410670005, newdata = AVG_MAX_85_Tualatin_AVG_r)
AVG_MAX_85_Tualatin_AVG_r$max_T <- AVG_MAX_85_Tualatin_AVG$NorESM1_M
Tualatin_85_r$NorESM1_M <- predict(res_410670005, newdata = AVG_MAX_85_Tualatin_AVG_r)

#AVG_MAX_45_Lafayette_AVG
AVG_MAX_45_Lafayette_AVG_r <- data.frame(matrix(ncol=9,nrow=41))
colnames(AVG_MAX_45_Lafayette_AVG_r) <- c("year","avg_WS","max_NO","max_NO2","avg_CO","avg_PM25","avg_WX","avg_WY","max_T")
AVG_MAX_45_Lafayette_AVG_r$year <- AVG_MAX_45_Lafayette_AVG$year
AVG_MAX_45_Lafayette_AVG_r$avg_WS <- AVG_MAX_45_Lafayette_AVG$avg_WS
AVG_MAX_45_Lafayette_AVG_r$max_NO <- AVG_MAX_45_Lafayette_AVG$max_NO
AVG_MAX_45_Lafayette_AVG_r$max_NO2 <- AVG_MAX_45_Lafayette_AVG$max_NO2
AVG_MAX_45_Lafayette_AVG_r$avg_CO <- AVG_MAX_45_Lafayette_AVG$avg_CO
AVG_MAX_45_Lafayette_AVG_r$avg_PM25 <- AVG_MAX_45_Lafayette_AVG$avg_PM25
AVG_MAX_45_Lafayette_AVG_r$avg_WX <- AVG_MAX_45_Lafayette_AVG$avg_WX
AVG_MAX_45_Lafayette_AVG_r$avg_WY <- AVG_MAX_45_Lafayette_AVG$avg_WY

Lafayette_45_r <- data.frame(matrix(ncol = 21, nrow=41))
colnames(Lafayette_45_r) <- c("year","bcc_csm1_1","bcc_csm1_1_m","BNU_ESM","CanESM2","CCSM4","CNRM_CM5","CSIRO_Mk_3_6_0","GFDL_ESM2M","GFDL_ESM2G","HadGEM2_CC365","HadGEM2_ES365","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC5","MIROC_ESM","MIROC_ESM_CHEM","MRI_CGCM3","NorESM1_M")
Lafayette_45_r$year <- AVG_MAX_45_Lafayette_AVG$year
	
AVG_MAX_45_Lafayette_AVG_r$max_T <- AVG_MAX_45_Lafayette_AVG$bcc_csm1_1
Lafayette_45_r$bcc_csm1_1 <- predict(res_410510080, newdata = AVG_MAX_45_Lafayette_AVG_r)
AVG_MAX_45_Lafayette_AVG_r$max_T <- AVG_MAX_45_Lafayette_AVG$bcc_csm1_1_m
Lafayette_45_r$bcc_csm1_1_m <- predict(res_410510080, newdata = AVG_MAX_45_Lafayette_AVG_r)
AVG_MAX_45_Lafayette_AVG_r$max_T <- AVG_MAX_45_Lafayette_AVG$BNU_ESM
Lafayette_45_r$BNU_ESM <- predict(res_410510080, newdata = AVG_MAX_45_Lafayette_AVG_r)
AVG_MAX_45_Lafayette_AVG_r$max_T <- AVG_MAX_45_Lafayette_AVG$CanESM2
Lafayette_45_r$CanESM2 <- predict(res_410510080, newdata = AVG_MAX_45_Lafayette_AVG_r)
AVG_MAX_45_Lafayette_AVG_r$max_T <- AVG_MAX_45_Lafayette_AVG$CCSM4
Lafayette_45_r$CCSM4 <- predict(res_410510080, newdata = AVG_MAX_45_Lafayette_AVG_r)
AVG_MAX_45_Lafayette_AVG_r$max_T <- AVG_MAX_45_Lafayette_AVG$CNRM_CM5
Lafayette_45_r$CNRM_CM5 <- predict(res_410510080, newdata = AVG_MAX_45_Lafayette_AVG_r)
AVG_MAX_45_Lafayette_AVG_r$max_T <- AVG_MAX_45_Lafayette_AVG$CSIRO_Mk_3_6_0
Lafayette_45_r$CSIRO_Mk_3_6_0 <- predict(res_410510080, newdata = AVG_MAX_45_Lafayette_AVG_r)
AVG_MAX_45_Lafayette_AVG_r$max_T <- AVG_MAX_45_Lafayette_AVG$GFDL_ESM2M
Lafayette_45_r$GFDL_ESM2M <- predict(res_410510080, newdata = AVG_MAX_45_Lafayette_AVG_r)
AVG_MAX_45_Lafayette_AVG_r$max_T <- AVG_MAX_45_Lafayette_AVG$GFDL_ESM2G
Lafayette_45_r$GFDL_ESM2G <- predict(res_410510080, newdata = AVG_MAX_45_Lafayette_AVG_r)
AVG_MAX_45_Lafayette_AVG_r$max_T <- AVG_MAX_45_Lafayette_AVG$HadGEM2_CC365
Lafayette_45_r$HadGEM2_CC365 <- predict(res_410510080, newdata = AVG_MAX_45_Lafayette_AVG_r)
AVG_MAX_45_Lafayette_AVG_r$max_T <- AVG_MAX_45_Lafayette_AVG$HadGEM2_ES365
Lafayette_45_r$HadGEM2_ES365 <- predict(res_410510080, newdata = AVG_MAX_45_Lafayette_AVG_r)
AVG_MAX_45_Lafayette_AVG_r$max_T <- AVG_MAX_45_Lafayette_AVG$inmcm4
Lafayette_45_r$inmcm4 <- predict(res_410510080, newdata = AVG_MAX_45_Lafayette_AVG_r)
AVG_MAX_45_Lafayette_AVG_r$max_T <- AVG_MAX_45_Lafayette_AVG$IPSL_CM5A_LR
Lafayette_45_r$IPSL_CM5A_LR <- predict(res_410510080, newdata = AVG_MAX_45_Lafayette_AVG_r)
AVG_MAX_45_Lafayette_AVG_r$max_T <- AVG_MAX_45_Lafayette_AVG$IPSL_CM5A_MR
Lafayette_45_r$IPSL_CM5A_MR <- predict(res_410510080, newdata = AVG_MAX_45_Lafayette_AVG_r)
AVG_MAX_45_Lafayette_AVG_r$max_T <- AVG_MAX_45_Lafayette_AVG$IPSL_CM5B_LR
Lafayette_45_r$IPSL_CM5B_LR <- predict(res_410510080, newdata = AVG_MAX_45_Lafayette_AVG_r)
AVG_MAX_45_Lafayette_AVG_r$max_T <- AVG_MAX_45_Lafayette_AVG$MIROC5
Lafayette_45_r$MIROC5 <- predict(res_410510080, newdata = AVG_MAX_45_Lafayette_AVG_r)
AVG_MAX_45_Lafayette_AVG_r$max_T <- AVG_MAX_45_Lafayette_AVG$MIROC_ESM
Lafayette_45_r$MIROC_ESM <- predict(res_410510080, newdata = AVG_MAX_45_Lafayette_AVG_r)
AVG_MAX_45_Lafayette_AVG_r$max_T <- AVG_MAX_45_Lafayette_AVG$MIROC_ESM_CHEM
Lafayette_45_r$MIROC_ESM_CHEM <- predict(res_410510080, newdata = AVG_MAX_45_Lafayette_AVG_r)
AVG_MAX_45_Lafayette_AVG_r$max_T <- AVG_MAX_45_Lafayette_AVG$MRI_CGCM3
Lafayette_45_r$MRI_CGCM3 <- predict(res_410510080, newdata = AVG_MAX_45_Lafayette_AVG_r)
AVG_MAX_45_Lafayette_AVG_r$max_T <- AVG_MAX_45_Lafayette_AVG$NorESM1_M
Lafayette_45_r$NorESM1_M <- predict(res_410510080, newdata = AVG_MAX_45_Lafayette_AVG_r)

#AVG_MAX_85_Lafayette_AVG
AVG_MAX_85_Lafayette_AVG_r <- data.frame(matrix(ncol=9,nrow=41))
colnames(AVG_MAX_85_Lafayette_AVG_r) <- c("year","avg_WS","max_NO","max_NO2","avg_CO","avg_PM25","avg_WX","avg_WY","max_T")
AVG_MAX_85_Lafayette_AVG_r$year <- AVG_MAX_85_Lafayette_AVG$year
AVG_MAX_85_Lafayette_AVG_r$avg_WS <- AVG_MAX_85_Lafayette_AVG$avg_WS
AVG_MAX_85_Lafayette_AVG_r$max_NO <- AVG_MAX_85_Lafayette_AVG$max_NO
AVG_MAX_85_Lafayette_AVG_r$max_NO2 <- AVG_MAX_85_Lafayette_AVG$max_NO2
AVG_MAX_85_Lafayette_AVG_r$avg_CO <- AVG_MAX_85_Lafayette_AVG$avg_CO
AVG_MAX_85_Lafayette_AVG_r$avg_PM25 <- AVG_MAX_85_Lafayette_AVG$avg_PM25
AVG_MAX_85_Lafayette_AVG_r$avg_WX <- AVG_MAX_85_Lafayette_AVG$avg_WX
AVG_MAX_85_Lafayette_AVG_r$avg_WY <- AVG_MAX_85_Lafayette_AVG$avg_WY

Lafayette_85_r <- data.frame(matrix(ncol = 21, nrow=41))
colnames(Lafayette_85_r) <- c("year","bcc_csm1_1","bcc_csm1_1_m","BNU_ESM","CanESM2","CCSM4","CNRM_CM5","CSIRO_Mk_3_6_0","GFDL_ESM2M","GFDL_ESM2G","HadGEM2_CC365","HadGEM2_ES365","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC5","MIROC_ESM","MIROC_ESM_CHEM","MRI_CGCM3","NorESM1_M")
Lafayette_85_r$year <- AVG_MAX_85_Lafayette_AVG$year
	
AVG_MAX_85_Lafayette_AVG_r$max_T <- AVG_MAX_85_Lafayette_AVG$bcc_csm1_1
Lafayette_85_r$bcc_csm1_1 <- predict(res_410510080, newdata = AVG_MAX_85_Lafayette_AVG_r)
AVG_MAX_85_Lafayette_AVG_r$max_T <- AVG_MAX_85_Lafayette_AVG$bcc_csm1_1_m
Lafayette_85_r$bcc_csm1_1_m <- predict(res_410510080, newdata = AVG_MAX_85_Lafayette_AVG_r)
AVG_MAX_85_Lafayette_AVG_r$max_T <- AVG_MAX_85_Lafayette_AVG$BNU_ESM
Lafayette_85_r$BNU_ESM <- predict(res_410510080, newdata = AVG_MAX_85_Lafayette_AVG_r)
AVG_MAX_85_Lafayette_AVG_r$max_T <- AVG_MAX_85_Lafayette_AVG$CanESM2
Lafayette_85_r$CanESM2 <- predict(res_410510080, newdata = AVG_MAX_85_Lafayette_AVG_r)
AVG_MAX_85_Lafayette_AVG_r$max_T <- AVG_MAX_85_Lafayette_AVG$CCSM4
Lafayette_85_r$CCSM4 <- predict(res_410510080, newdata = AVG_MAX_85_Lafayette_AVG_r)
AVG_MAX_85_Lafayette_AVG_r$max_T <- AVG_MAX_85_Lafayette_AVG$CNRM_CM5
Lafayette_85_r$CNRM_CM5 <- predict(res_410510080, newdata = AVG_MAX_85_Lafayette_AVG_r)
AVG_MAX_85_Lafayette_AVG_r$max_T <- AVG_MAX_85_Lafayette_AVG$CSIRO_Mk_3_6_0
Lafayette_85_r$CSIRO_Mk_3_6_0 <- predict(res_410510080, newdata = AVG_MAX_85_Lafayette_AVG_r)
AVG_MAX_85_Lafayette_AVG_r$max_T <- AVG_MAX_85_Lafayette_AVG$GFDL_ESM2M
Lafayette_85_r$GFDL_ESM2M <- predict(res_410510080, newdata = AVG_MAX_85_Lafayette_AVG_r)
AVG_MAX_85_Lafayette_AVG_r$max_T <- AVG_MAX_85_Lafayette_AVG$GFDL_ESM2G
Lafayette_85_r$GFDL_ESM2G <- predict(res_410510080, newdata = AVG_MAX_85_Lafayette_AVG_r)
AVG_MAX_85_Lafayette_AVG_r$max_T <- AVG_MAX_85_Lafayette_AVG$HadGEM2_CC365
Lafayette_85_r$HadGEM2_CC365 <- predict(res_410510080, newdata = AVG_MAX_85_Lafayette_AVG_r)
AVG_MAX_85_Lafayette_AVG_r$max_T <- AVG_MAX_85_Lafayette_AVG$HadGEM2_ES365
Lafayette_85_r$HadGEM2_ES365 <- predict(res_410510080, newdata = AVG_MAX_85_Lafayette_AVG_r)
AVG_MAX_85_Lafayette_AVG_r$max_T <- AVG_MAX_85_Lafayette_AVG$inmcm4
Lafayette_85_r$inmcm4 <- predict(res_410510080, newdata = AVG_MAX_85_Lafayette_AVG_r)
AVG_MAX_85_Lafayette_AVG_r$max_T <- AVG_MAX_85_Lafayette_AVG$IPSL_CM5A_LR
Lafayette_85_r$IPSL_CM5A_LR <- predict(res_410510080, newdata = AVG_MAX_85_Lafayette_AVG_r)
AVG_MAX_85_Lafayette_AVG_r$max_T <- AVG_MAX_85_Lafayette_AVG$IPSL_CM5A_MR
Lafayette_85_r$IPSL_CM5A_MR <- predict(res_410510080, newdata = AVG_MAX_85_Lafayette_AVG_r)
AVG_MAX_85_Lafayette_AVG_r$max_T <- AVG_MAX_85_Lafayette_AVG$IPSL_CM5B_LR
Lafayette_85_r$IPSL_CM5B_LR <- predict(res_410510080, newdata = AVG_MAX_85_Lafayette_AVG_r)
AVG_MAX_85_Lafayette_AVG_r$max_T <- AVG_MAX_85_Lafayette_AVG$MIROC5
Lafayette_85_r$MIROC5 <- predict(res_410510080, newdata = AVG_MAX_85_Lafayette_AVG_r)
AVG_MAX_85_Lafayette_AVG_r$max_T <- AVG_MAX_85_Lafayette_AVG$MIROC_ESM
Lafayette_85_r$MIROC_ESM <- predict(res_410510080, newdata = AVG_MAX_85_Lafayette_AVG_r)
AVG_MAX_85_Lafayette_AVG_r$max_T <- AVG_MAX_85_Lafayette_AVG$MIROC_ESM_CHEM
Lafayette_85_r$MIROC_ESM_CHEM <- predict(res_410510080, newdata = AVG_MAX_85_Lafayette_AVG_r)
AVG_MAX_85_Lafayette_AVG_r$max_T <- AVG_MAX_85_Lafayette_AVG$MRI_CGCM3
Lafayette_85_r$MRI_CGCM3 <- predict(res_410510080, newdata = AVG_MAX_85_Lafayette_AVG_r)
AVG_MAX_85_Lafayette_AVG_r$max_T <- AVG_MAX_85_Lafayette_AVG$NorESM1_M
Lafayette_85_r$NorESM1_M <- predict(res_410510080, newdata = AVG_MAX_85_Lafayette_AVG_r)

Tualatin_45_r$Scenario <- c("RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5")
Tualatin_85_r$Scenario <- c("RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5")
Lafayette_45_r$Scenario <- c("RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5","RCP 4.5")
Lafayette_85_r$Scenario <- c("RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5","RCP 8.5")

Tualatin_Projections <- rbind(Tualatin_45_r, Tualatin_85_r)
Lafayette_Projections <- rbind(Lafayette_45_r, Lafayette_85_r)

My_Theme2 = theme(
  title = element_text(size = 22),
  axis.title.x = element_text(size = 17),
  axis.text.x = element_text(size = 13),
  axis.title.y = element_text(size = 17),
  axis.text.y = element_text(size = 13),
  legend.title = element_text(size = 17),
  legend.text = element_text(size = 13),
  strip.text.x = element_text(size = 17))

colors <- c("bcc_csm1_1"="gold2","bcc_csm1_1_m"="gold3","BNU_ESM"="gold4","CanESM2"="red","CCSM4"="blue","CNRM_CM5"="orange","CSIRO_Mk_3_6_0"="green","GFDL_ESM2M"="cyan2","GFDL_ESM2G"="cyan3","HadGEM2_CC365"="slateblue2","HadGEM2_ES365"="slateblue3","inmcm4"="ivory4","IPSL_CM5A_LR"="aquamarine2","IPSL_CM5A_MR"="aquamarine3","IPSL_CM5B_LR"="aquamarine4","MIROC5"="maroon1","MIROC_ESM"="maroon2","MIROC_ESM_CHEM"="maroon3","MRI_CGCM3"="maroon4","NorESM1_M"="red3")
p_Tualatin_Projections <- (ggplot(Tualatin_Projections, aes(x = year)) 
	+ geom_line(aes(y = bcc_csm1_1, color = 'bcc_csm1_1'),size=1)
	+ geom_line(aes(y = bcc_csm1_1_m, color = 'bcc_csm1_1_m'),size=1)
	+ geom_line(aes(y = BNU_ESM, color = 'BNU_ESM'),size=1)
	+ geom_line(aes(y = CanESM2, color = 'CanESM2'),size=1)
	+ geom_line(aes(y = CCSM4, color = 'CCSM4'),size=1)
	+ geom_line(aes(y = CNRM_CM5, color = 'CNRM_CM5'),size=1)
	+ geom_line(aes(y = CSIRO_Mk_3_6_0, color = 'CSIRO_Mk_3_6_0'),size=1)
	+ geom_line(aes(y = GFDL_ESM2M, color = 'GFDL_ESM2M'),size=1)
	+ geom_line(aes(y = GFDL_ESM2G, color = 'GFDL_ESM2G'),size=1)
	+ geom_line(aes(y = HadGEM2_CC365, color = 'HadGEM2_CC365'),size=1)
	+ geom_line(aes(y = HadGEM2_ES365, color = 'HadGEM2_ES365'),size=1)
	+ geom_line(aes(y = inmcm4, color = 'inmcm4'),size=1)
	+ geom_line(aes(y = IPSL_CM5A_LR, color = 'IPSL_CM5A_LR'),size=1)
	+ geom_line(aes(y = IPSL_CM5A_MR, color = 'IPSL_CM5A_MR'),size=1)
	+ geom_line(aes(y = IPSL_CM5B_LR, color = 'IPSL_CM5B_LR'),size=1)
	+ geom_line(aes(y = MIROC5, color = 'MIROC5'),size=1)
	+ geom_line(aes(y = MIROC_ESM, color = 'MIROC_ESM'),size=1)
	+ geom_line(aes(y = MIROC_ESM_CHEM, color = 'MIROC_ESM_CHEM'),size=1)
	+ geom_line(aes(y = MRI_CGCM3, color = 'MRI_CGCM3'),size=1)
	+ geom_line(aes(y = NorESM1_M, color = 'NorESM1_M'),size=1)
	+ scale_y_continuous(sec.axis = sec_axis(~ (. - 0.0004207759437+0.3656)/.001337, name = "Temperature (K)"))
	+ facet_wrap(~Scenario)
	+ ggtitle("Tualatin Surface Level Ozone Projections")
	+ xlab("Year")
	+ ylab("Ozone (ppm)")
	+ labs(color = "Model")
#	+ labs(caption = "Surface level ozone projections by year, for changes in average daily maximum temperature. Temperature projections for IPCC scenarios RCP 4.5 and 8.5, are obtained from statistically downscaled climate models")
      + scale_color_manual(values = colors)
	+ My_Theme2
)
print(p_Tualatin_Projections)

p_Lafayette_Projections <- (ggplot(Lafayette_Projections, aes(x = year)) 
	+ geom_line(aes(y = bcc_csm1_1, color = 'bcc_csm1_1'),size=1)
	+ geom_line(aes(y = bcc_csm1_1_m, color = 'bcc_csm1_1_m'),size=1)
	+ geom_line(aes(y = BNU_ESM, color = 'BNU_ESM'),size=1)
	+ geom_line(aes(y = CanESM2, color = 'CanESM2'),size=1)
	+ geom_line(aes(y = CCSM4, color = 'CCSM4'),size=1)
	+ geom_line(aes(y = CNRM_CM5, color = 'CNRM_CM5'),size=1)
	+ geom_line(aes(y = CSIRO_Mk_3_6_0, color = 'CSIRO_Mk_3_6_0'),size=1)
	+ geom_line(aes(y = GFDL_ESM2M, color = 'GFDL_ESM2M'),size=1)
	+ geom_line(aes(y = GFDL_ESM2G, color = 'GFDL_ESM2G'),size=1)
	+ geom_line(aes(y = HadGEM2_CC365, color = 'HadGEM2_CC365'),size=1)
	+ geom_line(aes(y = HadGEM2_ES365, color = 'HadGEM2_ES365'),size=1)
	+ geom_line(aes(y = inmcm4, color = 'inmcm4'),size=1)
	+ geom_line(aes(y = IPSL_CM5A_LR, color = 'IPSL_CM5A_LR'),size=1)
	+ geom_line(aes(y = IPSL_CM5A_MR, color = 'IPSL_CM5A_MR'),size=1)
	+ geom_line(aes(y = IPSL_CM5B_LR, color = 'IPSL_CM5B_LR'),size=1)
	+ geom_line(aes(y = MIROC5, color = 'MIROC5'),size=1)
	+ geom_line(aes(y = MIROC_ESM, color = 'MIROC_ESM'),size=1)
	+ geom_line(aes(y = MIROC_ESM_CHEM, color = 'MIROC_ESM_CHEM'),size=1)
	+ geom_line(aes(y = MRI_CGCM3, color = 'MRI_CGCM3'),size=1)
	+ geom_line(aes(y = NorESM1_M, color = 'NorESM1_M'),size=1)
	+ scale_y_continuous(sec.axis = sec_axis(~ (. - 0.0031530692+0.5929945)/.0021149, name = "Temperature (K)"))
	+ facet_wrap(~Scenario)
	+ ggtitle("SE Lafayette Surface Level Ozone Projections")
	+ xlab("Year") 
	+ ylab("Ozone (ppm)")
	+ labs(color = "Model")
#	+ labs(caption = "Surface level ozone projections by year, for changes in average daily maximum temperature. Temperature projections for IPCC scenarios RCP 4.5 and 8.5, are obtained from statistically downscaled climate models")
      + scale_color_manual(values = colors)
	+ My_Theme2
)
print(p_Lafayette_Projections)

library(colorRamps)
#pivot_long
