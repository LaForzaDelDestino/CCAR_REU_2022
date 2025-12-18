library(tidyverse)
library(gridExtra)
library(lubridate)
library(corrplot)
library(car)

#stringrpackage str_replace()
#res <- lm(formula = x ~ y, data = z)
#summary(res)
#plot(res)
#predict(restest, newdata = ztest)

#Tualatin P DOE and is proxied from Lafayette
#Lafayette 21T DOE and is proxied from PDX
#Tualatin 17PM25 DOE

NEWMETdate <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/Met_2018_delivery.csv", stringsAsFactors = FALSE)
NEWPM <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/pm_2017_2021_delivered_20220811.csv", stringsAsFactors = FALSE)
NEWGAS <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/gasses_delivered_20220811.csv", stringsAsFactors = FALSE)

NEWMET <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/NEWMET.csv", stringsAsFactors = FALSE, header=FALSE)
colnames(NEWMET) <- c("site_id", "T", "WD", "WS", "P", "H")
NEWMETdate <- NEWMETdate %>% select(dt_local)
NEWMET <- cbind(NEWMETdate, NEWMET)
NEWMET$dt_local <- mdy_hm(NEWMET$dt_local)
NEWMET$year <- year(NEWMET$dt_local)
NEWMET$month <- month(NEWMET$dt_local)
NEWMET$dom <- day(NEWMET$dt_local)
NEWMET$hour <- hour(NEWMET$dt_local)

NEWMETTdate <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/MET_Tualatin_2018.csv", stringsAsFactors = FALSE)
NEWMETT <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/NEWMETT.csv", stringsAsFactors = FALSE, header=FALSE)
colnames(NEWMETT) <- c("site_id", "T", "WD", "WS", "H")
NEWMETTdate <- NEWMETTdate %>% select(dt_local)
NEWMETT <- cbind(NEWMETTdate, NEWMETT)
NEWMETT$dt_local <- mdy_hm(NEWMETT$dt_local)
NEWMETT$year <- year(NEWMETT$dt_local)
NEWMETT$month <- month(NEWMETT$dt_local)
NEWMETT$dom <- day(NEWMETT$dt_local)
NEWMETT$hour <- hour(NEWMETT$dt_local)

NEW_Weather_P_410510080_18 <- NEWMET %>% select(site_id, year, month, dom, hour, P)
NEW_Weather_T_410510080_18 <- NEWMET %>% select(site_id, year, month, dom, hour, T)
NEW_Weather_H_410510080_18 <- NEWMET %>% select(site_id, year, month, dom, hour, H)
NEW_Weather_WS_410510080_18 <- NEWMET %>% select(site_id, year, month, dom, hour, WS)
NEW_Weather_WD_410510080_18 <- NEWMET %>% select(site_id, year, month, dom, hour, WD)

NEW_Weather_T_410670005_18 <- NEWMETT %>% select(site_id, year, month, dom, hour, T)
NEW_Weather_H_410670005_18 <- NEWMETT %>% select(site_id, year, month, dom, hour, H)
NEW_Weather_WS_410670005_18 <- NEWMETT %>% select(site_id, year, month, dom, hour, WS)
NEW_Weather_WD_410670005_18 <- NEWMETT %>% select(site_id, year, month, dom, hour, WD)

#Met data from ODEQ site
#NEWMETT <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/NEWMETT.csv", stringsAsFactors = FALSE, header=FALSE)
#colnames(NEWMETT) <- c("site_id", "T", "H", "WD", "WS")
#NEWMETT$dt_local <- NEWMET$dt_local
#NEWMETT$year <- year(NEWMETT$dt_local)
#NEWMETT$month <- month(NEWMETT$dt_local)
#NEWMETT$dom <- day(NEWMETT$dt_local)
#NEWMETT$hour <- hour(NEWMETT$dt_local)
#NEW_Weather_T_410670005_18 <- NEWMETT %>% select(site_id, year, month, dom, hour, T)
#NEW_Weather_H_410670005_18 <- NEWMETT %>% select(site_id, year, month, dom, hour, H)
#NEW_Weather_WS_410670005_18 <- NEWMETT %>% select(site_id, year, month, dom, hour, WS)
#NEW_Weather_WD_410670005_18 <- NEWMETT %>% select(site_id, year, month, dom, hour, WD)


NEWPM$date <- mdy(NEWPM$date)
NEWPM$start_time <- hm(NEWPM$start_time)
NEWPM$year <- year(NEWPM$date)
NEWPM$month <- month(NEWPM$date)
NEWPM$dom <- day(NEWPM$date)
NEWPM$hour <- hour(NEWPM$start_time)

NEWPM25 <- NEWPM %>% filter(parameter == "Acceptable PM2.5 AQI & Speciation Mass") %>% select(epa_id, year, month, dom, hour, sample_value) %>% rename(PM25 = sample_value) %>% rename(site_id = epa_id)
NEWPM10 <- NEWPM %>% filter(parameter == "PM10 Total 0-10um STP") %>% select(epa_id, year, month, dom, hour, sample_value) %>% rename(PM10 = sample_value) %>% rename(site_id = epa_id)

NEWPM25_410510080 <- NEWPM25 %>% filter(site_id == 410510080)
NEWPM25_410510080_21 <- NEWPM25_410510080 %>% filter(year == 2021)
NEWPM25_410510080_20 <- NEWPM25_410510080 %>% filter(year == 2020)
NEWPM25_410510080_19 <- NEWPM25_410510080 %>% filter(year == 2019)
NEWPM25_410510080_18 <- NEWPM25_410510080 %>% filter(year == 2018)
NEWPM25_410510080_17 <- NEWPM25_410510080 %>% filter(year == 2017)

NEWPM25_410670005 <- NEWPM25 %>% filter(site_id == 410670005)
NEWPM25_410670005_21 <- NEWPM25_410670005 %>% filter(year == 2021)
NEWPM25_410670005_20 <- NEWPM25_410670005 %>% filter(year == 2020)
NEWPM25_410670005_19 <- NEWPM25_410670005 %>% filter(year == 2019)
NEWPM25_410670005_18 <- NEWPM25_410670005 %>% filter(year == 2018)
NEWPM25_410670005_17 <- NEWPM25_410670005 %>% filter(year == 2017)

NEWPM10_410510080 <- NEWPM10 %>% filter(site_id == 410510080)
#DOE
NEWPM10_410510080_21 <- NEWPM10_410510080 %>% filter(year == 2021)
NEWPM10_410510080_20 <- NEWPM10_410510080 %>% filter(year == 2020)
NEWPM10_410510080_19 <- NEWPM10_410510080 %>% filter(year == 2019)
NEWPM10_410510080_18 <- NEWPM10_410510080 %>% filter(year == 2018)
NEWPM10_410510080_17 <- NEWPM10_410510080 %>% filter(year == 2017)

NEWPM10_410670005 <- NEWPM10 %>% filter(site_id == 410670005)
NEWPM10_410670005_21 <- NEWPM10_410670005 %>% filter(year == 2021)
NEWPM10_410670005_20 <- NEWPM10_410670005 %>% filter(year == 2020)
NEWPM10_410670005_19 <- NEWPM10_410670005 %>% filter(year == 2019)
NEWPM10_410670005_18 <- NEWPM10_410670005 %>% filter(year == 2018)
NEWPM10_410670005_17 <- NEWPM10_410670005 %>% filter(year == 2017)


NEWGAS$date <- mdy(NEWGAS$date)
NEWGAS$start_time <- hm(NEWGAS$start_time)
NEWGAS$year <- year(NEWGAS$date)
NEWGAS$month <- month(NEWGAS$date)
NEWGAS$dom <- day(NEWGAS$date)
NEWGAS$hour <- hour(NEWGAS$start_time)

NEWCO <- NEWGAS %>% filter(parameter == "Carbon monoxide") %>% select(epa_id, year, month, dom, hour, sample_value) %>% rename(CO = sample_value) %>% rename(site_id = epa_id)
NEWNO <- NEWGAS %>% filter(parameter == "Nitric oxide (NO)") %>% select(epa_id, year, month, dom, hour, sample_value) %>% rename(NO = sample_value) %>% rename(site_id = epa_id)
NEWNO2 <- NEWGAS %>% filter(parameter == "Nitrogen dioxide (NO2)") %>% select(epa_id, year, month, dom, hour, sample_value) %>% rename(NO2 = sample_value) %>% rename(site_id = epa_id)
NEWNOX <- NEWGAS %>% filter(parameter == "Oxides of nitrogen (NOx)") %>% select(epa_id, year, month, dom, hour, sample_value) %>% rename(NOX = sample_value) %>% rename(site_id = epa_id)
NEWO3 <- NEWGAS %>% filter(parameter == "Ozone") %>% select(epa_id, year, month, dom, hour, sample_value) %>% rename(O3 = sample_value) %>% rename(site_id = epa_id)

#ALL CO UNITS ARE QUESTIONABLE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
NEWCO_410510080 <- NEWCO %>% filter(site_id == 410510080)
NEWCO_410510080_21 <- NEWCO_410510080 %>% filter(year == 2021)
NEWCO_410510080_20 <- NEWCO_410510080 %>% filter(year == 2020)
NEWCO_410510080_19 <- NEWCO_410510080 %>% filter(year == 2019)
NEWCO_410510080_18 <- NEWCO_410510080 %>% filter(year == 2018)
NEWCO_410510080_17 <- NEWCO_410510080 %>% filter(year == 2017)

NEWCO_410670005 <- NEWCO %>% filter(site_id == 410670005)
NEWCO_410670005_21 <- NEWCO_410670005 %>% filter(year == 2021)
NEWCO_410670005_20 <- NEWCO_410670005 %>% filter(year == 2020)
NEWCO_410670005_19 <- NEWCO_410670005 %>% filter(year == 2019)
NEWCO_410670005_18 <- NEWCO_410670005 %>% filter(year == 2018)
NEWCO_410670005_17 <- NEWCO_410670005 %>% filter(year == 2017)

NEWNO_410510080 <- NEWNO %>% filter(site_id == 410510080)
NEWNO_410510080_21 <- NEWNO_410510080 %>% filter(year == 2021)
NEWNO_410510080_20 <- NEWNO_410510080 %>% filter(year == 2020)
NEWNO_410510080_19 <- NEWNO_410510080 %>% filter(year == 2019)
NEWNO_410510080_18 <- NEWNO_410510080 %>% filter(year == 2018)
NEWNO_410510080_17 <- NEWNO_410510080 %>% filter(year == 2017)

NEWNO_410670005 <- NEWNO %>% filter(site_id == 410670005)
NEWNO_410670005_21 <- NEWNO_410670005 %>% filter(year == 2021)
NEWNO_410670005_20 <- NEWNO_410670005 %>% filter(year == 2020)
NEWNO_410670005_19 <- NEWNO_410670005 %>% filter(year == 2019)
NEWNO_410670005_18 <- NEWNO_410670005 %>% filter(year == 2018)
NEWNO_410670005_17 <- NEWNO_410670005 %>% filter(year == 2017)

NEWNO2_410510080 <- NEWNO2 %>% filter(site_id == 410510080)
NEWNO2_410510080_21 <- NEWNO2_410510080 %>% filter(year == 2021)
NEWNO2_410510080_20 <- NEWNO2_410510080 %>% filter(year == 2020)
NEWNO2_410510080_19 <- NEWNO2_410510080 %>% filter(year == 2019)
NEWNO2_410510080_18 <- NEWNO2_410510080 %>% filter(year == 2018)
NEWNO2_410510080_17 <- NEWNO2_410510080 %>% filter(year == 2017)

NEWNO2_410670005 <- NEWNO2 %>% filter(site_id == 410670005)
NEWNO2_410670005_21 <- NEWNO2_410670005 %>% filter(year == 2021)
NEWNO2_410670005_20 <- NEWNO2_410670005 %>% filter(year == 2020)
NEWNO2_410670005_19 <- NEWNO2_410670005 %>% filter(year == 2019)
NEWNO2_410670005_18 <- NEWNO2_410670005 %>% filter(year == 2018)
NEWNO2_410670005_17 <- NEWNO2_410670005 %>% filter(year == 2017)

NEWNOX_410510080 <- NEWNOX %>% filter(site_id == 410510080)
NEWNOX_410510080_21 <- NEWNOX_410510080 %>% filter(year == 2021)
NEWNOX_410510080_20 <- NEWNOX_410510080 %>% filter(year == 2020)
NEWNOX_410510080_19 <- NEWNOX_410510080 %>% filter(year == 2019)
NEWNOX_410510080_18 <- NEWNOX_410510080 %>% filter(year == 2018)
NEWNOX_410510080_17 <- NEWNOX_410510080 %>% filter(year == 2017)

NEWNOX_410670005 <- NEWNOX %>% filter(site_id == 410670005)
NEWNOX_410670005_21 <- NEWNOX_410670005 %>% filter(year == 2021)
NEWNOX_410670005_20 <- NEWNOX_410670005 %>% filter(year == 2020)
NEWNOX_410670005_19 <- NEWNOX_410670005 %>% filter(year == 2019)
NEWNOX_410670005_18 <- NEWNOX_410670005 %>% filter(year == 2018)
NEWNOX_410670005_17 <- NEWNOX_410670005 %>% filter(year == 2017)

#ALL O3 UNITS ARE QUESTIONABLE - 2017 differs!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
NEWO3_410510080 <- NEWO3 %>% filter(site_id == 410510080)
NEWO3_410510080_21 <- NEWO3_410510080 %>% filter(year == 2021)
NEWO3_410510080_20 <- NEWO3_410510080 %>% filter(year == 2020)
NEWO3_410510080_19 <- NEWO3_410510080 %>% filter(year == 2019)
NEWO3_410510080_18 <- NEWO3_410510080 %>% filter(year == 2018)
NEWO3_410510080_17 <- NEWO3_410510080 %>% filter(year == 2017)

NEWO3_410670005 <- NEWO3 %>% filter(site_id == 410670005)
NEWO3_410670005_21 <- NEWO3_410670005 %>% filter(year == 2021)
NEWO3_410670005_20 <- NEWO3_410670005 %>% filter(year == 2020)
NEWO3_410670005_19 <- NEWO3_410670005 %>% filter(year == 2019)
NEWO3_410670005_18 <- NEWO3_410670005 %>% filter(year == 2018)
NEWO3_410670005_17 <- NEWO3_410670005 %>% filter(year == 2017)


OR_Weather_W_21 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_Weather_W_21.csv", stringsAsFactors = FALSE)
OR_Weather_W_20 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_Weather_W_20.csv", stringsAsFactors = FALSE)
OR_Weather_W_19 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_Weather_W_19.csv", stringsAsFactors = FALSE)
#OR_Weather_W_18 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_Weather_W_18.csv", stringsAsFactors = FALSE)
OR_Weather_W_17 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_Weather_W_17.csv", stringsAsFactors = FALSE)

OR_Weather_W_21$year <- year(OR_Weather_W_21$dt_local)
OR_Weather_W_21$month <- month(OR_Weather_W_21$dt_local)
OR_Weather_W_21$dom <- day(OR_Weather_W_21$dt_local)
OR_Weather_W_21$hour <- hour(OR_Weather_W_21$dt_local)

OR_Weather_W_20$year <- year(OR_Weather_W_20$dt_local)
OR_Weather_W_20$month <- month(OR_Weather_W_20$dt_local)
OR_Weather_W_20$dom <- day(OR_Weather_W_20$dt_local)
OR_Weather_W_20$hour <- hour(OR_Weather_W_20$dt_local)

OR_Weather_W_19$year <- year(OR_Weather_W_19$dt_local)
OR_Weather_W_19$month <- month(OR_Weather_W_19$dt_local)
OR_Weather_W_19$dom <- day(OR_Weather_W_19$dt_local)
OR_Weather_W_19$hour <- hour(OR_Weather_W_19$dt_local)

#OR_Weather_W_18$year <- year(OR_Weather_W_18$dt_local)
#OR_Weather_W_18$month <- month(OR_Weather_W_18$dt_local)
#OR_Weather_W_18$dom <- day(OR_Weather_W_18$dt_local)
#OR_Weather_W_18$hour <- hour(OR_Weather_W_18$dt_local)

OR_Weather_W_17$year <- year(OR_Weather_W_17$dt_local)
OR_Weather_W_17$month <- month(OR_Weather_W_17$dt_local)
OR_Weather_W_17$dom <- day(OR_Weather_W_17$dt_local)
OR_Weather_W_17$hour <- hour(OR_Weather_W_17$dt_local)

OR_Weather_T_21 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_Weather_T_21.csv", stringsAsFactors = FALSE)
OR_Weather_T_20 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_Weather_T_20.csv", stringsAsFactors = FALSE)
OR_Weather_T_19 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_Weather_T_19.csv", stringsAsFactors = FALSE)
#OR_Weather_T_18 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_Weather_T_18.csv", stringsAsFactors = FALSE)
OR_Weather_T_17 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_Weather_T_17.csv", stringsAsFactors = FALSE)

NTEMPT21 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/NTEMPT21.csv", stringsAsFactors = FALSE, header=FALSE)
NTEMPT20 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/NTEMPT20.csv", stringsAsFactors = FALSE, header=FALSE)
NTEMPT19 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/NTEMPT19.csv", stringsAsFactors = FALSE, header=FALSE)
NTEMPT17 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/NTEMPT17.csv", stringsAsFactors = FALSE, header=FALSE)
colnames(NTEMPT21) <- c("ignore", "Sample.Measurement_K")
colnames(NTEMPT20) <- c("ignore", "Sample.Measurement_K")
colnames(NTEMPT19) <- c("ignore", "Sample.Measurement_K")
colnames(NTEMPT17) <- c("ignore", "Sample.Measurement_K")
OR_Weather_T_21$Sample.Measurement_K <- NTEMPT21$Sample.Measurement_K
OR_Weather_T_20$Sample.Measurement_K <- NTEMPT20$Sample.Measurement_K
OR_Weather_T_19$Sample.Measurement_K <- NTEMPT19$Sample.Measurement_K
OR_Weather_T_17$Sample.Measurement_K <- NTEMPT17$Sample.Measurement_K

OR_Weather_T_21$year <- year(OR_Weather_T_21$dt_local)
OR_Weather_T_21$month <- month(OR_Weather_T_21$dt_local)
OR_Weather_T_21$dom <- day(OR_Weather_T_21$dt_local)
OR_Weather_T_21$hour <- hour(OR_Weather_T_21$dt_local)

OR_Weather_T_20$year <- year(OR_Weather_T_20$dt_local)
OR_Weather_T_20$month <- month(OR_Weather_T_20$dt_local)
OR_Weather_T_20$dom <- day(OR_Weather_T_20$dt_local)
OR_Weather_T_20$hour <- hour(OR_Weather_T_20$dt_local)

OR_Weather_T_19$year <- year(OR_Weather_T_19$dt_local)
OR_Weather_T_19$month <- month(OR_Weather_T_19$dt_local)
OR_Weather_T_19$dom <- day(OR_Weather_T_19$dt_local)
OR_Weather_T_19$hour <- hour(OR_Weather_T_19$dt_local)

#OR_Weather_T_18$year <- year(OR_Weather_T_18$dt_local)
#OR_Weather_T_18$month <- month(OR_Weather_T_18$dt_local)
#OR_Weather_T_18$dom <- day(OR_Weather_T_18$dt_local)
#OR_Weather_T_18$hour <- hour(OR_Weather_T_18$dt_local)

OR_Weather_T_17$year <- year(OR_Weather_T_17$dt_local)
OR_Weather_T_17$month <- month(OR_Weather_T_17$dt_local)
OR_Weather_T_17$dom <- day(OR_Weather_T_17$dt_local)
OR_Weather_T_17$hour <- hour(OR_Weather_T_17$dt_local)

OR_Weather_H_21 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_Weather_H_21.csv", stringsAsFactors = FALSE)
OR_Weather_H_20 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_Weather_H_20.csv", stringsAsFactors = FALSE)
OR_Weather_H_19 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_Weather_H_19.csv", stringsAsFactors = FALSE)
#OR_Weather_H_18 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_Weather_H_18.csv", stringsAsFactors = FALSE)
OR_Weather_H_17 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_Weather_H_17.csv", stringsAsFactors = FALSE)

OR_Weather_H_21$year <- year(OR_Weather_H_21$dt_local)
OR_Weather_H_21$month <- month(OR_Weather_H_21$dt_local)
OR_Weather_H_21$dom <- day(OR_Weather_H_21$dt_local)
OR_Weather_H_21$hour <- hour(OR_Weather_H_21$dt_local)

OR_Weather_H_20$year <- year(OR_Weather_H_20$dt_local)
OR_Weather_H_20$month <- month(OR_Weather_H_20$dt_local)
OR_Weather_H_20$dom <- day(OR_Weather_H_20$dt_local)
OR_Weather_H_20$hour <- hour(OR_Weather_H_20$dt_local)

OR_Weather_H_19$year <- year(OR_Weather_H_19$dt_local)
OR_Weather_H_19$month <- month(OR_Weather_H_19$dt_local)
OR_Weather_H_19$dom <- day(OR_Weather_H_19$dt_local)
OR_Weather_H_19$hour <- hour(OR_Weather_H_19$dt_local)

#OR_Weather_H_18$year <- year(OR_Weather_H_18$dt_local)
#OR_Weather_H_18$month <- month(OR_Weather_H_18$dt_local)
#OR_Weather_H_18$dom <- day(OR_Weather_H_18$dt_local)
#OR_Weather_H_18$hour <- hour(OR_Weather_H_18$dt_local)

OR_Weather_H_17$year <- year(OR_Weather_H_17$dt_local)
OR_Weather_H_17$month <- month(OR_Weather_H_17$dt_local)
OR_Weather_H_17$dom <- day(OR_Weather_H_17$dt_local)
OR_Weather_H_17$hour <- hour(OR_Weather_H_17$dt_local)

OR_Weather_P_21 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_Weather_P_21.csv", stringsAsFactors = FALSE)
OR_Weather_P_20 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_Weather_P_20.csv", stringsAsFactors = FALSE)
OR_Weather_P_19 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_Weather_P_19.csv", stringsAsFactors = FALSE)
OR_Weather_P_18 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_Weather_P_18.csv", stringsAsFactors = FALSE)
OR_Weather_P_17 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_Weather_P_17.csv", stringsAsFactors = FALSE)

OR_Weather_P_21$year <- year(OR_Weather_P_21$dt_local)
OR_Weather_P_21$month <- month(OR_Weather_P_21$dt_local)
OR_Weather_P_21$dom <- day(OR_Weather_P_21$dt_local)
OR_Weather_P_21$hour <- hour(OR_Weather_P_21$dt_local)

OR_Weather_P_20$year <- year(OR_Weather_P_20$dt_local)
OR_Weather_P_20$month <- month(OR_Weather_P_20$dt_local)
OR_Weather_P_20$dom <- day(OR_Weather_P_20$dt_local)
OR_Weather_P_20$hour <- hour(OR_Weather_P_20$dt_local)

OR_Weather_P_19$year <- year(OR_Weather_P_19$dt_local)
OR_Weather_P_19$month <- month(OR_Weather_P_19$dt_local)
OR_Weather_P_19$dom <- day(OR_Weather_P_19$dt_local)
OR_Weather_P_19$hour <- hour(OR_Weather_P_19$dt_local)

OR_Weather_P_18$year <- year(OR_Weather_P_18$dt_local)
OR_Weather_P_18$month <- month(OR_Weather_P_18$dt_local)
OR_Weather_P_18$dom <- day(OR_Weather_P_18$dt_local)
OR_Weather_P_18$hour <- hour(OR_Weather_P_18$dt_local)

OR_Weather_P_17$year <- year(OR_Weather_P_17$dt_local)
OR_Weather_P_17$month <- month(OR_Weather_P_17$dt_local)
OR_Weather_P_17$dom <- day(OR_Weather_P_17$dt_local)
OR_Weather_P_17$hour <- hour(OR_Weather_P_17$dt_local)

OR_O3_21 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/or_o3_21.csv", stringsAsFactors = FALSE)
OR_O3_20 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/or_o3_20.csv", stringsAsFactors = FALSE)
OR_O3_19 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/or_o3_19.csv", stringsAsFactors = FALSE)
OR_O3_18 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/or_o3_18.csv", stringsAsFactors = FALSE)
OR_O3_17 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/or_o3_17.csv", stringsAsFactors = FALSE)

OR_O3_21$year <- year(OR_O3_21$dt_local)
OR_O3_21$month <- month(OR_O3_21$dt_local)
OR_O3_21$dom <- day(OR_O3_21$dt_local)
OR_O3_21$hour <- hour(OR_O3_21$dt_local)

OR_O3_20$year <- year(OR_O3_20$dt_local)
OR_O3_20$month <- month(OR_O3_20$dt_local)
OR_O3_20$dom <- day(OR_O3_20$dt_local)
OR_O3_20$hour <- hour(OR_O3_20$dt_local)

OR_O3_19$year <- year(OR_O3_19$dt_local)
OR_O3_19$month <- month(OR_O3_19$dt_local)
OR_O3_19$dom <- day(OR_O3_19$dt_local)
OR_O3_19$hour <- hour(OR_O3_19$dt_local)

OR_O3_18$year <- year(OR_O3_18$dt_local)
OR_O3_18$month <- month(OR_O3_18$dt_local)
OR_O3_18$dom <- day(OR_O3_18$dt_local)
OR_O3_18$hour <- hour(OR_O3_18$dt_local)

OR_O3_17$year <- year(OR_O3_17$dt_local)
OR_O3_17$month <- month(OR_O3_17$dt_local)
OR_O3_17$dom <- day(OR_O3_17$dt_local)
OR_O3_17$hour <- hour(OR_O3_17$dt_local)

OR_CO_21 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_CO_21.csv", stringsAsFactors = FALSE)
OR_CO_20 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_CO_20.csv", stringsAsFactors = FALSE)
OR_CO_19 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_CO_19.csv", stringsAsFactors = FALSE)
OR_CO_18 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_CO_18.csv", stringsAsFactors = FALSE)
OR_CO_17 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_CO_17.csv", stringsAsFactors = FALSE)

OR_CO_21$year <- year(OR_CO_21$dt_local)
OR_CO_21$month <- month(OR_CO_21$dt_local)
OR_CO_21$dom <- day(OR_CO_21$dt_local)
OR_CO_21$hour <- hour(OR_CO_21$dt_local)

OR_CO_20$year <- year(OR_CO_20$dt_local)
OR_CO_20$month <- month(OR_CO_20$dt_local)
OR_CO_20$dom <- day(OR_CO_20$dt_local)
OR_CO_20$hour <- hour(OR_CO_20$dt_local)

OR_CO_19$year <- year(OR_CO_19$dt_local)
OR_CO_19$month <- month(OR_CO_19$dt_local)
OR_CO_19$dom <- day(OR_CO_19$dt_local)
OR_CO_19$hour <- hour(OR_CO_19$dt_local)

OR_CO_18$year <- year(OR_CO_18$dt_local)
OR_CO_18$month <- month(OR_CO_18$dt_local)
OR_CO_18$dom <- day(OR_CO_18$dt_local)
OR_CO_18$hour <- hour(OR_CO_18$dt_local)

OR_CO_17$year <- year(OR_CO_17$dt_local)
OR_CO_17$month <- month(OR_CO_17$dt_local)
OR_CO_17$dom <- day(OR_CO_17$dt_local)
OR_CO_17$hour <- hour(OR_CO_17$dt_local)

OR_SO2_21 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_SO2_21.csv", stringsAsFactors = FALSE)
OR_SO2_20 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_SO2_20.csv", stringsAsFactors = FALSE)
OR_SO2_19 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_SO2_19.csv", stringsAsFactors = FALSE)
OR_SO2_18 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_SO2_18.csv", stringsAsFactors = FALSE)
OR_SO2_17 <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/OR_SO2_17.csv", stringsAsFactors = FALSE)

OR_SO2_21$year <- year(OR_SO2_21$dt_local)
OR_SO2_21$month <- month(OR_SO2_21$dt_local)
OR_SO2_21$dom <- day(OR_SO2_21$dt_local)
OR_SO2_21$hour <- hour(OR_SO2_21$dt_local)

OR_SO2_20$year <- year(OR_SO2_20$dt_local)
OR_SO2_20$month <- month(OR_SO2_20$dt_local)
OR_SO2_20$dom <- day(OR_SO2_20$dt_local)
OR_SO2_20$hour <- hour(OR_SO2_20$dt_local)

OR_SO2_19$year <- year(OR_SO2_19$dt_local)
OR_SO2_19$month <- month(OR_SO2_19$dt_local)
OR_SO2_19$dom <- day(OR_SO2_19$dt_local)
OR_SO2_19$hour <- hour(OR_SO2_19$dt_local)

OR_SO2_18$year <- year(OR_SO2_18$dt_local)
OR_SO2_18$month <- month(OR_SO2_18$dt_local)
OR_SO2_18$dom <- day(OR_SO2_18$dt_local)
OR_SO2_18$hour <- hour(OR_SO2_18$dt_local)

OR_SO2_17$year <- year(OR_SO2_17$dt_local)
OR_SO2_17$month <- month(OR_SO2_17$dt_local)
OR_SO2_17$dom <- day(OR_SO2_17$dt_local)
OR_SO2_17$hour <- hour(OR_SO2_17$dt_local)

##Create station variables
#410670005
OR_Weather_P_410670005_21 <- OR_Weather_P_21 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(P = Sample.Measurement)
OR_Weather_P_410670005_20 <- OR_Weather_P_20 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(P = Sample.Measurement)
OR_Weather_P_410670005_19 <- OR_Weather_P_19 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(P = Sample.Measurement)
OR_Weather_P_410670005_18 <- OR_Weather_P_18 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(P = Sample.Measurement)
OR_Weather_P_410670005_17 <- OR_Weather_P_17 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(P = Sample.Measurement)

OR_Weather_W_410670005_21 <- OR_Weather_W_21 %>% filter(site_id == 410670005)
OR_Weather_WS_410670005_21 <- OR_Weather_W_410670005_21 %>% filter(Parameter.Name == "Wind Speed - Resultant") %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(WS = Sample.Measurement)
OR_Weather_WD_410670005_21 <- OR_Weather_W_410670005_21 %>% filter(Parameter.Name == "Wind Direction - Resultant") %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(WD = Sample.Measurement)
OR_Weather_W_410670005_20 <- OR_Weather_W_20 %>% filter(site_id == 410670005)
OR_Weather_WS_410670005_20 <- OR_Weather_W_410670005_20 %>% filter(Parameter.Name == "Wind Speed - Resultant") %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(WS = Sample.Measurement)
OR_Weather_WD_410670005_20 <- OR_Weather_W_410670005_20 %>% filter(Parameter.Name == "Wind Direction - Resultant") %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(WD = Sample.Measurement)
OR_Weather_W_410670005_19 <- OR_Weather_W_19 %>% filter(site_id == 410670005)
OR_Weather_WS_410670005_19 <- OR_Weather_W_410670005_19 %>% filter(Parameter.Name == "Wind Speed - Resultant") %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(WS = Sample.Measurement)
OR_Weather_WD_410670005_19 <- OR_Weather_W_410670005_19 %>% filter(Parameter.Name == "Wind Direction - Resultant") %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(WD = Sample.Measurement)
#OR_Weather_W_410670005_18 <- OR_Weather_W_18 %>% filter(site_id == 410670005)
#OR_Weather_WS_410670005_18 <- OR_Weather_W_410670005_18 %>% filter(Parameter.Name == "Wind Speed - Resultant") %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(WS = Sample.Measurement)
#OR_Weather_WD_410670005_18 <- OR_Weather_W_410670005_18 %>% filter(Parameter.Name == "Wind Direction - Resultant") %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(WD = Sample.Measurement)
OR_Weather_W_410670005_17 <- OR_Weather_W_17 %>% filter(site_id == 410670005)
OR_Weather_WS_410670005_17 <- OR_Weather_W_410670005_17 %>% filter(Parameter.Name == "Wind Speed - Resultant") %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(WS = Sample.Measurement)
OR_Weather_WD_410670005_17 <- OR_Weather_W_410670005_17 %>% filter(Parameter.Name == "Wind Direction - Resultant") %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(WD = Sample.Measurement)

OR_Weather_T_410670005_21 <- OR_Weather_T_21 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement_K) %>% rename(T = Sample.Measurement_K)
OR_Weather_T_410670005_20 <- OR_Weather_T_20 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement_K) %>% rename(T = Sample.Measurement_K)
OR_Weather_T_410670005_19 <- OR_Weather_T_19 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement_K) %>% rename(T = Sample.Measurement_K)
#OR_Weather_T_410670005_18 <- OR_Weather_T_18 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement_K) %>% rename(T = Sample.Measurement_K)
OR_Weather_T_410670005_17 <- OR_Weather_T_17 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement_K) %>% rename(T = Sample.Measurement_K)

OR_Weather_H_410670005_21 <- OR_Weather_H_21 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(H = Sample.Measurement)
OR_Weather_H_410670005_20 <- OR_Weather_H_20 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(H = Sample.Measurement)
OR_Weather_H_410670005_19 <- OR_Weather_H_19 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(H = Sample.Measurement)
#OR_Weather_H_410670005_18 <- OR_Weather_H_18 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(H = Sample.Measurement)
OR_Weather_H_410670005_17 <- OR_Weather_H_17 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(H = Sample.Measurement)

OR_O3_410670005_21 <- OR_O3_21 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(O3 = Sample.Measurement)
OR_O3_410670005_20 <- OR_O3_20 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(O3 = Sample.Measurement)
OR_O3_410670005_19 <- OR_O3_19 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(O3 = Sample.Measurement)
OR_O3_410670005_18 <- OR_O3_18 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(O3 = Sample.Measurement)
OR_O3_410670005_17 <- OR_O3_17 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(O3 = Sample.Measurement)

OR_CO_410670005_21 <- OR_CO_21 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(CO = Sample.Measurement)
OR_CO_410670005_20 <- OR_CO_20 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(CO = Sample.Measurement)
OR_CO_410670005_19 <- OR_CO_19 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(CO = Sample.Measurement)
OR_CO_410670005_18 <- OR_CO_18 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(CO = Sample.Measurement)
OR_CO_410670005_17 <- OR_CO_17 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(CO = Sample.Measurement)

OR_SO2_410670005_21 <- OR_SO2_21 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(SO2 = Sample.Measurement)
OR_SO2_410670005_20 <- OR_SO2_20 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(SO2 = Sample.Measurement)
OR_SO2_410670005_19 <- OR_SO2_19 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(SO2 = Sample.Measurement)
OR_SO2_410670005_18 <- OR_SO2_18 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(SO2 = Sample.Measurement)
OR_SO2_410670005_17 <- OR_SO2_17 %>% filter(site_id == 410670005) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(SO2 = Sample.Measurement)

#410510080
OR_Weather_P_410510080_21 <- OR_Weather_P_21 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(P = Sample.Measurement)
OR_Weather_P_410510080_20 <- OR_Weather_P_20 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(P = Sample.Measurement)
OR_Weather_P_410510080_19 <- OR_Weather_P_19 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(P = Sample.Measurement)
#OR_Weather_P_410510080_18 <- OR_Weather_P_18 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(P = Sample.Measurement)
OR_Weather_P_410510080_17 <- OR_Weather_P_17 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(P = Sample.Measurement)

OR_Weather_W_410510080_21 <- OR_Weather_W_21 %>% filter(site_id == 410510080)
OR_Weather_WS_410510080_21 <- OR_Weather_W_410510080_21 %>% filter(Parameter.Name == "Wind Speed - Resultant") %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(WS = Sample.Measurement)
OR_Weather_WD_410510080_21 <- OR_Weather_W_410510080_21 %>% filter(Parameter.Name == "Wind Direction - Resultant") %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(WD = Sample.Measurement)
OR_Weather_W_410510080_20 <- OR_Weather_W_20 %>% filter(site_id == 410510080)
OR_Weather_WS_410510080_20 <- OR_Weather_W_410510080_20 %>% filter(Parameter.Name == "Wind Speed - Resultant") %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(WS = Sample.Measurement)
OR_Weather_WD_410510080_20 <- OR_Weather_W_410510080_20 %>% filter(Parameter.Name == "Wind Direction - Resultant") %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(WD = Sample.Measurement)
OR_Weather_W_410510080_19 <- OR_Weather_W_19 %>% filter(site_id == 410510080)
OR_Weather_WS_410510080_19 <- OR_Weather_W_410510080_19 %>% filter(Parameter.Name == "Wind Speed - Resultant") %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(WS = Sample.Measurement)
OR_Weather_WD_410510080_19 <- OR_Weather_W_410510080_19 %>% filter(Parameter.Name == "Wind Direction - Resultant") %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(WD = Sample.Measurement)
#OR_Weather_W_410510080_18 <- OR_Weather_W_18 %>% filter(site_id == 410510080)
#OR_Weather_WS_410510080_18 <- OR_Weather_W_410510080_18 %>% filter(Parameter.Name == "Wind Speed - Resultant") %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(WS = Sample.Measurement)
#OR_Weather_WD_410510080_18 <- OR_Weather_W_410510080_18 %>% filter(Parameter.Name == "Wind Direction - Resultant") %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(WD = Sample.Measurement)
OR_Weather_W_410510080_17 <- OR_Weather_W_17 %>% filter(site_id == 410510080)
OR_Weather_WS_410510080_17 <- OR_Weather_W_410510080_17 %>% filter(Parameter.Name == "Wind Speed - Resultant") %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(WS = Sample.Measurement)
OR_Weather_WD_410510080_17 <- OR_Weather_W_410510080_17 %>% filter(Parameter.Name == "Wind Direction - Resultant") %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(WD = Sample.Measurement)

OR_Weather_T_410510080_21 <- OR_Weather_T_21 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement_K) %>% rename(T = Sample.Measurement_K)
OR_Weather_T_410510080_20 <- OR_Weather_T_20 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement_K) %>% rename(T = Sample.Measurement_K)
OR_Weather_T_410510080_19 <- OR_Weather_T_19 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement_K) %>% rename(T = Sample.Measurement_K)
#OR_Weather_T_410510080_18 <- OR_Weather_T_18 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement_K) %>% rename(T = Sample.Measurement_K)
OR_Weather_T_410510080_17 <- OR_Weather_T_17 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement_K) %>% rename(T = Sample.Measurement_K)

OR_Weather_H_410510080_21 <- OR_Weather_H_21 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(H = Sample.Measurement)
OR_Weather_H_410510080_20 <- OR_Weather_H_20 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(H = Sample.Measurement)
OR_Weather_H_410510080_19 <- OR_Weather_H_19 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(H = Sample.Measurement)
#OR_Weather_H_410510080_18 <- OR_Weather_H_18 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(H = Sample.Measurement)
OR_Weather_H_410510080_17 <- OR_Weather_H_17 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(H = Sample.Measurement)

OR_O3_410510080_21 <- OR_O3_21 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(O3 = Sample.Measurement)
OR_O3_410510080_20 <- OR_O3_20 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(O3 = Sample.Measurement)
OR_O3_410510080_19 <- OR_O3_19 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(O3 = Sample.Measurement)
OR_O3_410510080_18 <- OR_O3_18 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(O3 = Sample.Measurement)
OR_O3_410510080_17 <- OR_O3_17 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(O3 = Sample.Measurement)

OR_CO_410510080_21 <- OR_CO_21 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(CO = Sample.Measurement)
OR_CO_410510080_20 <- OR_CO_20 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(CO = Sample.Measurement)
OR_CO_410510080_19 <- OR_CO_19 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(CO = Sample.Measurement)
OR_CO_410510080_18 <- OR_CO_18 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(CO = Sample.Measurement)
OR_CO_410510080_17 <- OR_CO_17 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(CO = Sample.Measurement)

OR_SO2_410510080_21 <- OR_SO2_21 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(SO2 = Sample.Measurement)
OR_SO2_410510080_20 <- OR_SO2_20 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(SO2 = Sample.Measurement)
OR_SO2_410510080_19 <- OR_SO2_19 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(SO2 = Sample.Measurement)
OR_SO2_410510080_18 <- OR_SO2_18 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(SO2 = Sample.Measurement)
OR_SO2_410510080_17 <- OR_SO2_17 %>% filter(site_id == 410510080) %>% select(site_id, year, month, dom, hour, Sample.Measurement) %>% rename(SO2 = Sample.Measurement)

#LAFYETTE 21 TEMP PROXY
LAFAYETTE_21T_PROXY <- read.csv("C://Users/treeo/OneDrive/Documents/R/data/LAFAYETTE_21T_PROXY.csv", stringsAsFactors = FALSE)
LAFAYETTE_21T_PROXY$dt_local <- mdy_hm(LAFAYETTE_21T_PROXY$dt_local)
LAFAYETTE_21T_PROXY$year <- year(LAFAYETTE_21T_PROXY$dt_local)
LAFAYETTE_21T_PROXY$month <- month(LAFAYETTE_21T_PROXY$dt_local)
LAFAYETTE_21T_PROXY$dom <- day(LAFAYETTE_21T_PROXY$dt_local)
LAFAYETTE_21T_PROXY$hour <- hour(LAFAYETTE_21T_PROXY$dt_local)
LAFAYETTE_21T_PROXY <- LAFAYETTE_21T_PROXY %>% select(site_id, year, month, dom, hour, T)

LAFAYETTE_21T_PROXY_HOUR <- LAFAYETTE_21T_PROXY %>%
  group_by(site_id, year, month, dom, hour) %>%
  summarize(T = mean(T, na.rm = TRUE))

##Merge
#2021
OR_ALL_410670005_21 <- merge(x=OR_Weather_P_410670005_21, y=OR_Weather_T_410670005_21, by = c("site_id", "year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_21 <- merge(x=OR_ALL_410670005_21, y=OR_Weather_H_410670005_21, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_21 <- merge(x=OR_ALL_410670005_21, y=OR_Weather_WS_410670005_21, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_21 <- merge(x=OR_ALL_410670005_21, y=OR_Weather_WD_410670005_21, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_21 <- merge(x=OR_ALL_410670005_21, y=OR_O3_410670005_21, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_21 <- merge(x=OR_ALL_410670005_21, y=NEWNO_410670005_21, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_21 <- merge(x=OR_ALL_410670005_21, y=NEWNOX_410670005_21, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_21 <- merge(x=OR_ALL_410670005_21, y=NEWNO2_410670005_21, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_21 <- merge(x=OR_ALL_410670005_21, y=OR_SO2_410670005_21, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_21 <- merge(x=OR_ALL_410670005_21, y=OR_CO_410670005_21, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_21 <- merge(x=OR_ALL_410670005_21, y=NEWPM25_410670005_21, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_21 <- merge(x=OR_ALL_410670005_21, y=NEWPM10_410670005_21, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_21 <- OR_ALL_410670005_21 %>% filter(month > 5) 
OR_ALL_410670005_21 <- OR_ALL_410670005_21 %>% filter(month < 9)
OR_ALL_410670005_21$WX <- cos(OR_ALL_410670005_21$WD * pi/180)
OR_ALL_410670005_21$WY <- sin(OR_ALL_410670005_21$WD * pi/180)
OR_ALL_410670005_21_DAY <- OR_ALL_410670005_21 %>% filter(hour >= 7) %>% filter(hour <= 19)

OR_ALL_410510080_21 <- merge(x=OR_Weather_P_410510080_21, y=LAFAYETTE_21T_PROXY_HOUR, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_21 <- merge(x=OR_ALL_410510080_21, y=OR_Weather_H_410510080_21, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_21 <- merge(x=OR_ALL_410510080_21, y=OR_Weather_WS_410510080_21, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_21 <- merge(x=OR_ALL_410510080_21, y=OR_Weather_WD_410510080_21, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_21 <- merge(x=OR_ALL_410510080_21, y=OR_O3_410510080_21, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_21 <- merge(x=OR_ALL_410510080_21, y=NEWNO_410510080_21, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_21 <- merge(x=OR_ALL_410510080_21, y=NEWNOX_410510080_21, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_21 <- merge(x=OR_ALL_410510080_21, y=NEWNO2_410510080_21, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_21 <- merge(x=OR_ALL_410510080_21, y=OR_SO2_410510080_21, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_21 <- merge(x=OR_ALL_410510080_21, y=OR_CO_410510080_21, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_21 <- merge(x=OR_ALL_410510080_21, y=NEWPM25_410510080_21, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_21 <- merge(x=OR_ALL_410510080_21, y=NEWPM10_410510080_21, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_21 <- OR_ALL_410510080_21 %>% filter(month > 5) 
OR_ALL_410510080_21 <- OR_ALL_410510080_21 %>% filter(month < 9)
OR_ALL_410510080_21$WX <- cos(OR_ALL_410510080_21$WD * pi/180)
OR_ALL_410510080_21$WY <- sin(OR_ALL_410510080_21$WD * pi/180)
OR_ALL_410510080_21_DAY <- OR_ALL_410510080_21 %>% filter(hour >= 7) %>% filter(hour <= 19)

#2020
OR_ALL_410670005_20 <- merge(x=OR_Weather_P_410670005_20, y=OR_Weather_T_410670005_20, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_20 <- merge(x=OR_ALL_410670005_20, y=OR_Weather_H_410670005_20, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_20 <- merge(x=OR_ALL_410670005_20, y=OR_Weather_WS_410670005_20, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_20 <- merge(x=OR_ALL_410670005_20, y=OR_Weather_WD_410670005_20, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_20 <- merge(x=OR_ALL_410670005_20, y=OR_O3_410670005_20, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_20 <- merge(x=OR_ALL_410670005_20, y=NEWNO_410670005_20, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_20 <- merge(x=OR_ALL_410670005_20, y=NEWNOX_410670005_20, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_20 <- merge(x=OR_ALL_410670005_20, y=NEWNO2_410670005_20, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_20 <- merge(x=OR_ALL_410670005_20, y=OR_SO2_410670005_20, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_20 <- merge(x=OR_ALL_410670005_20, y=OR_CO_410670005_20, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_20 <- merge(x=OR_ALL_410670005_20, y=NEWPM25_410670005_20, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_20 <- merge(x=OR_ALL_410670005_20, y=NEWPM10_410670005_20, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_20 <- OR_ALL_410670005_20 %>% filter(month > 5) 
OR_ALL_410670005_20 <- OR_ALL_410670005_20 %>% filter(month < 9)
OR_ALL_410670005_20$WX <- cos(OR_ALL_410670005_20$WD * pi/180)
OR_ALL_410670005_20$WY <- sin(OR_ALL_410670005_20$WD * pi/180)
OR_ALL_410670005_20_DAY <- OR_ALL_410670005_20 %>% filter(hour >= 7) %>% filter(hour <= 19)

OR_ALL_410510080_20 <- merge(x=OR_Weather_P_410510080_20, y=OR_Weather_T_410510080_20, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_20 <- merge(x=OR_ALL_410510080_20, y=OR_Weather_H_410510080_20, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_20 <- merge(x=OR_ALL_410510080_20, y=OR_Weather_WS_410510080_20, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_20 <- merge(x=OR_ALL_410510080_20, y=OR_Weather_WD_410510080_20, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_20 <- merge(x=OR_ALL_410510080_20, y=OR_O3_410510080_20, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_20 <- merge(x=OR_ALL_410510080_20, y=NEWNO_410510080_20, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_20 <- merge(x=OR_ALL_410510080_20, y=NEWNOX_410510080_20, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_20 <- merge(x=OR_ALL_410510080_20, y=NEWNO2_410510080_20, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_20 <- merge(x=OR_ALL_410510080_20, y=OR_SO2_410510080_20, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_20 <- merge(x=OR_ALL_410510080_20, y=OR_CO_410510080_20, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_20 <- merge(x=OR_ALL_410510080_20, y=NEWPM25_410510080_20, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_20 <- merge(x=OR_ALL_410510080_20, y=NEWPM10_410510080_20, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_20 <- OR_ALL_410510080_20 %>% filter(month > 5) 
OR_ALL_410510080_20 <- OR_ALL_410510080_20 %>% filter(month < 9)
OR_ALL_410510080_20$WX <- cos(OR_ALL_410510080_20$WD * pi/180)
OR_ALL_410510080_20$WY <- sin(OR_ALL_410510080_20$WD * pi/180)
OR_ALL_410510080_20_DAY <- OR_ALL_410510080_20 %>% filter(hour >= 7) %>% filter(hour <= 19)

#2019
OR_ALL_410670005_19 <- merge(x=OR_Weather_P_410670005_19, y=OR_Weather_T_410670005_19, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_19 <- merge(x=OR_ALL_410670005_19, y=OR_Weather_H_410670005_19, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_19 <- merge(x=OR_ALL_410670005_19, y=OR_Weather_WS_410670005_19, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_19 <- merge(x=OR_ALL_410670005_19, y=OR_Weather_WD_410670005_19, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_19 <- merge(x=OR_ALL_410670005_19, y=OR_O3_410670005_19, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_19 <- merge(x=OR_ALL_410670005_19, y=NEWNO_410670005_19, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_19 <- merge(x=OR_ALL_410670005_19, y=NEWNOX_410670005_19, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_19 <- merge(x=OR_ALL_410670005_19, y=NEWNO2_410670005_19, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_19 <- merge(x=OR_ALL_410670005_19, y=OR_SO2_410670005_19, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_19 <- merge(x=OR_ALL_410670005_19, y=OR_CO_410670005_19, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_19 <- merge(x=OR_ALL_410670005_19, y=NEWPM25_410670005_19, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_19 <- merge(x=OR_ALL_410670005_19, y=NEWPM10_410670005_19, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_19 <- OR_ALL_410670005_19 %>% filter(month > 5) 
OR_ALL_410670005_19 <- OR_ALL_410670005_19 %>% filter(month < 9)
OR_ALL_410670005_19$WX <- cos(OR_ALL_410670005_19$WD * pi/180)
OR_ALL_410670005_19$WY <- sin(OR_ALL_410670005_19$WD * pi/180)
OR_ALL_410670005_19_DAY <- OR_ALL_410670005_19 %>% filter(hour >= 7) %>% filter(hour <= 19)

OR_ALL_410510080_19 <- merge(x=OR_Weather_P_410510080_19, y=OR_Weather_T_410510080_19, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_19 <- merge(x=OR_ALL_410510080_19, y=OR_Weather_H_410510080_19, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_19 <- merge(x=OR_ALL_410510080_19, y=OR_Weather_WS_410510080_19, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_19 <- merge(x=OR_ALL_410510080_19, y=OR_Weather_WD_410510080_19, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_19 <- merge(x=OR_ALL_410510080_19, y=OR_O3_410510080_19, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_19 <- merge(x=OR_ALL_410510080_19, y=NEWNO_410510080_19, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_19 <- merge(x=OR_ALL_410510080_19, y=NEWNOX_410510080_19, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_19 <- merge(x=OR_ALL_410510080_19, y=NEWNO2_410510080_19, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_19 <- merge(x=OR_ALL_410510080_19, y=OR_SO2_410510080_19, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_19 <- merge(x=OR_ALL_410510080_19, y=OR_CO_410510080_19, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_19 <- merge(x=OR_ALL_410510080_19, y=NEWPM25_410510080_19, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_19 <- merge(x=OR_ALL_410510080_19, y=NEWPM10_410510080_19, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_19 <- OR_ALL_410510080_19 %>% filter(month > 5) 
OR_ALL_410510080_19 <- OR_ALL_410510080_19 %>% filter(month < 9)
OR_ALL_410510080_19$WX <- cos(OR_ALL_410510080_19$WD * pi/180)
OR_ALL_410510080_19$WY <- sin(OR_ALL_410510080_19$WD * pi/180)
OR_ALL_410510080_19_DAY <- OR_ALL_410510080_19 %>% filter(hour >= 7) %>% filter(hour <= 19)

#2018
OR_ALL_410670005_18 <- merge(x=OR_Weather_P_410670005_18, y=NEW_Weather_T_410670005_18, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_18 <- merge(x=OR_ALL_410670005_18, y=NEW_Weather_H_410670005_18, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_18 <- merge(x=OR_ALL_410670005_18, y=NEW_Weather_WS_410670005_18, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_18 <- merge(x=OR_ALL_410670005_18, y=NEW_Weather_WD_410670005_18, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_18 <- merge(x=OR_ALL_410670005_18, y=OR_O3_410670005_18, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_18 <- merge(x=OR_ALL_410670005_18, y=NEWNO_410670005_18, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_18 <- merge(x=OR_ALL_410670005_18, y=NEWNOX_410670005_18, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_18 <- merge(x=OR_ALL_410670005_18, y=NEWNO2_410670005_18, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_18 <- merge(x=OR_ALL_410670005_18, y=OR_SO2_410670005_18, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_18 <- merge(x=OR_ALL_410670005_18, y=OR_CO_410670005_18, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_18 <- merge(x=OR_ALL_410670005_18, y=NEWPM25_410670005_18, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_18 <- merge(x=OR_ALL_410670005_18, y=NEWPM10_410670005_18, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_18 <- OR_ALL_410670005_18 %>% filter(month > 5) 
OR_ALL_410670005_18 <- OR_ALL_410670005_18 %>% filter(month < 9)
OR_ALL_410670005_18$WX <- cos(OR_ALL_410670005_18$WD * pi/180)
OR_ALL_410670005_18$WY <- sin(OR_ALL_410670005_18$WD * pi/180)
OR_ALL_410670005_18_DAY <- OR_ALL_410670005_18 %>% filter(hour >= 7) %>% filter(hour <= 19)

OR_ALL_410510080_18 <- merge(x=NEW_Weather_P_410510080_18, y=NEW_Weather_T_410510080_18, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_18 <- merge(x=OR_ALL_410510080_18, y=NEW_Weather_H_410510080_18, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_18 <- merge(x=OR_ALL_410510080_18, y=NEW_Weather_WS_410510080_18, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_18 <- merge(x=OR_ALL_410510080_18, y=NEW_Weather_WD_410510080_18, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_18 <- merge(x=OR_ALL_410510080_18, y=OR_O3_410510080_18, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_18 <- merge(x=OR_ALL_410510080_18, y=NEWNO_410510080_18, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_18 <- merge(x=OR_ALL_410510080_18, y=NEWNOX_410510080_18, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_18 <- merge(x=OR_ALL_410510080_18, y=NEWNO2_410510080_18, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_18 <- merge(x=OR_ALL_410510080_18, y=OR_SO2_410510080_18, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_18 <- merge(x=OR_ALL_410510080_18, y=OR_CO_410510080_18, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_18 <- merge(x=OR_ALL_410510080_18, y=NEWPM25_410510080_18, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_18 <- merge(x=OR_ALL_410510080_18, y=NEWPM10_410510080_18, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_18 <- OR_ALL_410510080_18 %>% filter(month > 5) 
OR_ALL_410510080_18 <- OR_ALL_410510080_18 %>% filter(month < 9)
OR_ALL_410510080_18$WX <- cos(OR_ALL_410510080_18$WD * pi/180)
OR_ALL_410510080_18$WY <- sin(OR_ALL_410510080_18$WD * pi/180)
OR_ALL_410510080_18_DAY <- OR_ALL_410510080_18 %>% filter(hour >= 7) %>% filter(hour <= 19)

#2017
OR_ALL_410670005_17 <- merge(x=OR_Weather_P_410670005_17, y=OR_Weather_T_410670005_17, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_17 <- merge(x=OR_ALL_410670005_17, y=OR_Weather_H_410670005_17, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_17 <- merge(x=OR_ALL_410670005_17, y=OR_Weather_WS_410670005_17, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_17 <- merge(x=OR_ALL_410670005_17, y=OR_Weather_WD_410670005_17, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_17 <- merge(x=OR_ALL_410670005_17, y=OR_O3_410670005_17, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_17 <- merge(x=OR_ALL_410670005_17, y=NEWNO_410670005_17, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_17 <- merge(x=OR_ALL_410670005_17, y=NEWNOX_410670005_17, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_17 <- merge(x=OR_ALL_410670005_17, y=NEWNO2_410670005_17, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_17 <- merge(x=OR_ALL_410670005_17, y=OR_SO2_410670005_17, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_17 <- merge(x=OR_ALL_410670005_17, y=OR_CO_410670005_17, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_17 <- merge(x=OR_ALL_410670005_17, y=NEWPM25_410670005_17, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_17 <- merge(x=OR_ALL_410670005_17, y=NEWPM10_410670005_17, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410670005_17 <- OR_ALL_410670005_17 %>% filter(month > 5) 
OR_ALL_410670005_17 <- OR_ALL_410670005_17 %>% filter(month < 9)
OR_ALL_410670005_17$WX <- cos(OR_ALL_410670005_17$WD * pi/180)
OR_ALL_410670005_17$WY <- sin(OR_ALL_410670005_17$WD * pi/180)
OR_ALL_410670005_17_DAY <- OR_ALL_410670005_17 %>% filter(hour >= 7) %>% filter(hour <= 19)

OR_ALL_410510080_17 <- merge(x=OR_Weather_P_410510080_17, y=OR_Weather_T_410510080_17, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_17 <- merge(x=OR_ALL_410510080_17, y=OR_Weather_H_410510080_17, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_17 <- merge(x=OR_ALL_410510080_17, y=OR_Weather_WS_410510080_17, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_17 <- merge(x=OR_ALL_410510080_17, y=OR_Weather_WD_410510080_17, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_17 <- merge(x=OR_ALL_410510080_17, y=OR_O3_410510080_17, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_17 <- merge(x=OR_ALL_410510080_17, y=NEWNO_410510080_17, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_17 <- merge(x=OR_ALL_410510080_17, y=NEWNOX_410510080_17, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_17 <- merge(x=OR_ALL_410510080_17, y=NEWNO2_410510080_17, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_17 <- merge(x=OR_ALL_410510080_17, y=OR_SO2_410510080_17, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_17 <- merge(x=OR_ALL_410510080_17, y=OR_CO_410510080_17, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_17 <- merge(x=OR_ALL_410510080_17, y=NEWPM25_410510080_17, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_17 <- merge(x=OR_ALL_410510080_17, y=NEWPM10_410510080_17, by = c("site_id" ,"year", "month", "dom", "hour"), all = TRUE)
OR_ALL_410510080_17 <- OR_ALL_410510080_17 %>% filter(month > 5) 
OR_ALL_410510080_17 <- OR_ALL_410510080_17 %>% filter(month < 9)
OR_ALL_410510080_17$WX <- cos(OR_ALL_410510080_17$WD * pi/180)
OR_ALL_410510080_17$WY <- sin(OR_ALL_410510080_17$WD * pi/180)
OR_ALL_410510080_17_DAY <- OR_ALL_410510080_17 %>% filter(hour >= 7) %>% filter(hour <= 19)

#Pressure Proxy
OR_ALL_410670005_21$P <- OR_ALL_410510080_21$P
OR_ALL_410670005_21_DAY$P <- OR_ALL_410510080_21_DAY$P
OR_ALL_410670005_20$P <- OR_ALL_410510080_20$P
OR_ALL_410670005_20_DAY$P <- OR_ALL_410510080_20_DAY$P
OR_ALL_410670005_19$P <- OR_ALL_410510080_19$P
OR_ALL_410670005_19_DAY$P <- OR_ALL_410510080_19_DAY$P
OR_ALL_410670005_18$P <- OR_ALL_410510080_18$P
OR_ALL_410670005_18_DAY$P <- OR_ALL_410510080_18_DAY$P
OR_ALL_410670005_17$P <- OR_ALL_410510080_17$P
OR_ALL_410670005_17_DAY$P <- OR_ALL_410510080_17_DAY$P

##rbind
OR_ALL_21 <- rbind(OR_ALL_410670005_21, OR_ALL_410510080_21)
OR_ALL_20 <- rbind(OR_ALL_410670005_20, OR_ALL_410510080_20)
OR_ALL_19 <- rbind(OR_ALL_410670005_19, OR_ALL_410510080_19)
OR_ALL_18 <- rbind(OR_ALL_410670005_18, OR_ALL_410510080_18)
OR_ALL_17 <- rbind(OR_ALL_410670005_17, OR_ALL_410510080_17)
OR_ALL <- rbind(OR_ALL_21, OR_ALL_20, OR_ALL_19, OR_ALL_18, OR_ALL_17)

OR_ALL_410670005 <- rbind(OR_ALL_410670005_21, OR_ALL_410670005_20, OR_ALL_410670005_19, OR_ALL_410670005_18, OR_ALL_410670005_17)
OR_ALL_410510080 <- rbind(OR_ALL_410510080_21, OR_ALL_410510080_20, OR_ALL_410510080_19, OR_ALL_410510080_18, OR_ALL_410510080_17)

OR_ALL_21_DAY <- rbind(OR_ALL_410670005_21_DAY, OR_ALL_410510080_21_DAY)
OR_ALL_20_DAY <- rbind(OR_ALL_410670005_20_DAY, OR_ALL_410510080_20_DAY)
OR_ALL_19_DAY <- rbind(OR_ALL_410670005_19_DAY, OR_ALL_410510080_19_DAY)
OR_ALL_18_DAY <- rbind(OR_ALL_410670005_18_DAY, OR_ALL_410510080_18_DAY)
OR_ALL_17_DAY <- rbind(OR_ALL_410670005_17_DAY, OR_ALL_410510080_17_DAY)
OR_ALL_DAY <- rbind(OR_ALL_21_DAY, OR_ALL_20_DAY, OR_ALL_19_DAY, OR_ALL_18_DAY, OR_ALL_17_DAY)

OR_ALL_410670005_DAY <- rbind(OR_ALL_410670005_21_DAY, OR_ALL_410670005_20_DAY, OR_ALL_410670005_19_DAY, OR_ALL_410670005_18_DAY, OR_ALL_410670005_17_DAY)
OR_ALL_410510080_DAY <- rbind(OR_ALL_410510080_21_DAY, OR_ALL_410510080_20_DAY, OR_ALL_410510080_19_DAY, OR_ALL_410510080_18_DAY, OR_ALL_410510080_17_DAY)

##Create Summaries by day
OR_ALL_DAY_SUM_P <- OR_ALL_DAY %>%
  group_by(site_id, year, month, dom) %>%
  summarize(avg_P = mean(P, na.rm = TRUE),
            sd_P = sd(P, na.rm = TRUE),
            max_P = max(P, na.rm = TRUE),
            min_P = min(P, na.rm = TRUE),
		num_P = sum(is.na(P)))
OR_ALL_DAY_SUM_T <- OR_ALL_DAY %>%
  group_by(site_id, year, month, dom) %>%
  summarize(avg_T = mean(T, na.rm = TRUE),
            sd_T = sd(T, na.rm = TRUE),
            max_T = max(T, na.rm = TRUE),
            min_T = min(T, na.rm = TRUE),
		num_T = sum(is.na(T)))
OR_ALL_DAY_SUM_H <- OR_ALL_DAY %>%
  group_by(site_id, year, month, dom) %>%
  summarize(avg_H = mean(H, na.rm = TRUE),
            sd_H = sd(H, na.rm = TRUE),
            max_H = max(H, na.rm = TRUE),
            min_H = min(H, na.rm = TRUE),
		num_H = sum(is.na(H)))
OR_ALL_DAY_SUM_WS <- OR_ALL_DAY %>%
  group_by(site_id, year, month, dom) %>%
  summarize(avg_WS = mean(WS, na.rm = TRUE),
            sd_WS = sd(WS, na.rm = TRUE),
            max_WS = max(WS, na.rm = TRUE),
            min_WS = min(WS, na.rm = TRUE),
		num_WS = sum(is.na(WS)))
OR_ALL_DAY_SUM_WD <- OR_ALL_DAY %>%
  group_by(site_id, year, month, dom) %>%
  summarize(avg_WD = mean(WD, na.rm = TRUE),
            sd_WD = sd(WD, na.rm = TRUE),
            max_WD = max(WD, na.rm = TRUE),
            min_WD = min(WD, na.rm = TRUE),
		num_WD = sum(is.na(WD)))
OR_ALL_DAY_SUM_O3 <- OR_ALL_DAY %>%
  group_by(site_id, year, month, dom) %>%
  summarize(avg_O3 = mean(O3, na.rm = TRUE),
            sd_O3 = sd(O3, na.rm = TRUE),
            max_O3 = max(O3, na.rm = TRUE),
            min_O3 = min(O3, na.rm = TRUE),
		num_O3 = sum(is.na(O3)))
OR_ALL_DAY_SUM_NO <- OR_ALL_DAY %>%
  group_by(site_id, year, month, dom) %>%
  summarize(avg_NO = mean(NO, na.rm = TRUE),
            sd_NO = sd(NO, na.rm = TRUE),
            max_NO = max(NO, na.rm = TRUE),
            min_NO = min(NO, na.rm = TRUE),
		num_NO = sum(is.na(NO)))
OR_ALL_DAY_SUM_NOX <- OR_ALL_DAY %>%
  group_by(site_id, year, month, dom) %>%
  summarize(avg_NOX = mean(NOX, na.rm = TRUE),
            sd_NOX = sd(NOX, na.rm = TRUE),
            max_NOX = max(NOX, na.rm = TRUE),
            min_NOX = min(NOX, na.rm = TRUE),
		num_NOX = sum(is.na(NOX)))
OR_ALL_DAY_SUM_NO2 <- OR_ALL_DAY %>%
  group_by(site_id, year, month, dom) %>%
  summarize(avg_NO2 = mean(NO2, na.rm = TRUE),
            sd_NO2 = sd(NO2, na.rm = TRUE),
            max_NO2 = max(NO2, na.rm = TRUE),
            min_NO2 = min(NO2, na.rm = TRUE),
		num_NO2 = sum(is.na(NO2)))
OR_ALL_DAY_SUM_SO2 <- OR_ALL_DAY %>%
  group_by(site_id, year, month, dom) %>%
  summarize(avg_SO2 = mean(SO2, na.rm = TRUE),
            sd_SO2 = sd(SO2, na.rm = TRUE),
            max_SO2 = max(SO2, na.rm = TRUE),
            min_SO2 = min(SO2, na.rm = TRUE),
		num_SO2 = sum(is.na(SO2)))
OR_ALL_DAY_SUM_CO <- OR_ALL_DAY %>%
  group_by(site_id, year, month, dom) %>%
  summarize(avg_CO = mean(CO, na.rm = TRUE),
            sd_CO = sd(CO, na.rm = TRUE),
            max_CO = max(CO, na.rm = TRUE),
            min_CO = min(CO, na.rm = TRUE),
		num_CO = sum(is.na(CO)))
OR_ALL_DAY_SUM_PM25 <- OR_ALL_DAY %>%
  group_by(site_id, year, month, dom) %>%
  summarize(avg_PM25 = mean(PM25, na.rm = TRUE),
            sd_PM25 = sd(PM25, na.rm = TRUE),
            max_PM25 = max(PM25, na.rm = TRUE),
            min_PM25 = min(PM25, na.rm = TRUE),
		num_PM25 = sum(is.na(PM25)))
OR_ALL_DAY_SUM_PM10 <- OR_ALL_DAY %>%
  group_by(site_id, year, month, dom) %>%
  summarize(avg_PM10 = mean(PM10, na.rm = TRUE),
            sd_PM10 = sd(PM10, na.rm = TRUE),
            max_PM10 = max(PM10, na.rm = TRUE),
            min_PM10 = min(PM10, na.rm = TRUE),
		num_PM10 = sum(is.na(PM10)))
OR_ALL_DAY_SUM_WX <- OR_ALL_DAY %>%
  group_by(site_id, year, month, dom) %>%
  summarize(avg_WX = mean(WX, na.rm = TRUE),
            sd_WX = sd(WX, na.rm = TRUE),
            max_WX = max(WX, na.rm = TRUE),
            min_WX = min(WX, na.rm = TRUE),
		num_WX = sum(is.na(WX)))
OR_ALL_DAY_SUM_WY <- OR_ALL_DAY %>%
  group_by(site_id, year, month, dom) %>%
  summarize(avg_WY = mean(WY, na.rm = TRUE),
            sd_WY = sd(WY, na.rm = TRUE),
            max_WY = max(WY, na.rm = TRUE),
            min_WY = min(WY, na.rm = TRUE),
		num_WY = sum(is.na(WY)))

#MERGE
OR_ALL_DAY_SUM <- merge(x=OR_ALL_DAY_SUM_P, y=OR_ALL_DAY_SUM_T, by = c("site_id" ,"year", "month", "dom"), all = TRUE)
OR_ALL_DAY_SUM <- merge(x=OR_ALL_DAY_SUM, y=OR_ALL_DAY_SUM_H, by = c("site_id" ,"year", "month", "dom"), all = TRUE)
OR_ALL_DAY_SUM <- merge(x=OR_ALL_DAY_SUM, y=OR_ALL_DAY_SUM_WS, by = c("site_id" ,"year", "month", "dom"), all = TRUE)
OR_ALL_DAY_SUM <- merge(x=OR_ALL_DAY_SUM, y=OR_ALL_DAY_SUM_WD, by = c("site_id" ,"year", "month", "dom"), all = TRUE)
OR_ALL_DAY_SUM <- merge(x=OR_ALL_DAY_SUM, y=OR_ALL_DAY_SUM_O3, by = c("site_id" ,"year", "month", "dom"), all = TRUE)
OR_ALL_DAY_SUM <- merge(x=OR_ALL_DAY_SUM, y=OR_ALL_DAY_SUM_NO, by = c("site_id" ,"year", "month", "dom"), all = TRUE)
OR_ALL_DAY_SUM <- merge(x=OR_ALL_DAY_SUM, y=OR_ALL_DAY_SUM_NOX, by = c("site_id" ,"year", "month", "dom"), all = TRUE)
OR_ALL_DAY_SUM <- merge(x=OR_ALL_DAY_SUM, y=OR_ALL_DAY_SUM_NO2, by = c("site_id" ,"year", "month", "dom"), all = TRUE)
OR_ALL_DAY_SUM <- merge(x=OR_ALL_DAY_SUM, y=OR_ALL_DAY_SUM_SO2, by = c("site_id" ,"year", "month", "dom"), all = TRUE)
OR_ALL_DAY_SUM <- merge(x=OR_ALL_DAY_SUM, y=OR_ALL_DAY_SUM_CO, by = c("site_id" ,"year", "month", "dom"), all = TRUE)
OR_ALL_DAY_SUM <- merge(x=OR_ALL_DAY_SUM, y=OR_ALL_DAY_SUM_PM25, by = c("site_id" ,"year", "month", "dom"), all = TRUE)
OR_ALL_DAY_SUM <- merge(x=OR_ALL_DAY_SUM, y=OR_ALL_DAY_SUM_PM10, by = c("site_id" ,"year", "month", "dom"), all = TRUE)
OR_ALL_DAY_SUM <- merge(x=OR_ALL_DAY_SUM, y=OR_ALL_DAY_SUM_WX, by = c("site_id" ,"year", "month", "dom"), all = TRUE)
OR_ALL_DAY_SUM <- merge(x=OR_ALL_DAY_SUM, y=OR_ALL_DAY_SUM_WY, by = c("site_id" ,"year", "month", "dom"), all = TRUE)

OR_ALL_410510080_DAY_SUM <- OR_ALL_DAY_SUM %>% filter (site_id == 410510080)
OR_ALL_410670005_DAY_SUM <- OR_ALL_DAY_SUM %>% filter (site_id == 410670005)

OR_ALL_410510080_DAY_SUM_r <- OR_ALL_410510080_DAY_SUM %>% filter(!is.infinite(max_T)) %>% filter(!is.infinite(max_WS)) %>% filter(!is.infinite(max_WD)) %>% filter(!is.infinite(max_O3)) %>% filter(!is.infinite(max_NO2)) %>% filter(!is.infinite(max_NO)) %>% filter(!is.infinite(max_CO)) %>% filter(!is.infinite(max_PM25))
OR_ALL_410670005_DAY_SUM_r <- OR_ALL_410670005_DAY_SUM %>% filter(!is.infinite(max_T)) %>% filter(!is.infinite(max_WS)) %>% filter(!is.infinite(max_WD)) %>% filter(!is.infinite(max_O3)) %>% filter(!is.infinite(max_NO2)) %>% filter(!is.infinite(max_NO)) %>% filter(!is.infinite(max_CO)) %>% filter(!is.infinite(max_PM25))


#Correlation Plots
OR_CORR_410670005_DAY_SUM <- OR_ALL_410670005_DAY_SUM_r%>% select(max_O3,max_T,avg_WS,avg_WX,avg_WY,max_NO,max_NO2,avg_CO,avg_PM25)
M_410670005_DAY_SUM=cor(OR_CORR_410670005_DAY_SUM, use = "pairwise.complete.obs")
colnames(M_410670005_DAY_SUM) <- c("Max O3","Max T","Avg WS","Avg WU","Avg WV","Max NO","Max NO2","Avg CO","Avg PM25")
rownames(M_410670005_DAY_SUM) <- c("Max O3","Max T","Avg WS","Avg WU","Avg WV","Max NO","Max NO2","Avg CO","Avg PM25")
CORR_410670005_DAY_SUM <- corrplot(M_410670005_DAY_SUM, is.corr = TRUE, addCoef.col ='black', method = 'shade', type = 'lower')
print(CORR_410670005_DAY_SUM)

OR_CORR_410510080_DAY_SUM <- OR_ALL_410510080_DAY_SUM_r%>% select(max_O3,max_T,avg_WS,avg_WX,avg_WY,max_NO,max_NO2,avg_CO,avg_PM25)
M_410510080_DAY_SUM=cor(OR_CORR_410510080_DAY_SUM, use = "pairwise.complete.obs")
colnames(M_410510080_DAY_SUM) <- c("Max O3","Max T","Avg WS","Avg WU","Avg WV","Max NO","Max NO2","Avg CO","Avg PM25")
rownames(M_410510080_DAY_SUM) <- c("Max O3","Max T","Avg WS","Avg WU","Avg WV","Max NO","Max NO2","Avg CO","Avg PM25")
CORR_410510080_DAY_SUM <- corrplot(M_410510080_DAY_SUM, is.corr = TRUE, addCoef.col ='black', method = 'shade', type = 'lower')
print(CORR_410510080_DAY_SUM)

My_Theme1 = theme(
  title = element_text(size = 22),
  axis.title.x = element_text(size = 17),
  axis.text.x = element_text(size = 13),
  axis.title.y = element_text(size = 17),
  axis.text.y = element_text(size = 13),
  legend.title = element_text(size = 17),
  legend.position = c(.1, .9),
  legend.justification = c("left", "top"),
  legend.box.just = "left",
  legend.margin = margin(6, 6, 6, 6),
  legend.text = element_text(size = 12))

#Prep 410670005
set.seed(1234)
OR_ALL_410670005_DAY_SUM_r <- OR_ALL_410670005_DAY_SUM_r %>% mutate(randnum = runif(316, 0, 1)) %>% arrange(randnum)
train_410670005 <- OR_ALL_410670005_DAY_SUM_r[1:253,]
test_410670005 <- OR_ALL_410670005_DAY_SUM_r[254:316,]
res_410670005 <- lm(data = train_410670005, max_O3 ~ max_T + avg_WS + avg_WX + avg_WY + max_NO + max_NO2 + avg_CO + avg_PM25)
summary(res_410670005)
vif(res_410670005)

Predicted_O3 <- predict(res_410670005, newdata = test_410670005)
asdfg <- cbind(test_410670005, Predicted_O3)
res1 <- lm(data = asdfg, max_O3 ~ Predicted_O3)
summary(res1)
Actual_O3 <- asdfg$max_O3
rplot_Tualatin <- (ggplot() + geom_point(aes(x = Predicted_O3*1000, y = Actual_O3*1000), na.rm = FALSE)
	+ geom_abline(aes(slope = 1, intercept = 0, color = '1 to 1 Relationship\nY = X')) 
	+ geom_abline(aes(slope = 0.950299, intercept = 0.001238, color = 'Line of Best Fit\nY = 0.950*X + 0.00124'), na.rm = FALSE)
	+ labs(color ="Legend")
	+ xlab("Actual O3 (ppb)")
	+ylab("Predicted O3 (ppb)")
	+ggtitle("Tualatin")
	+ My_Theme1
)
print(rplot_Tualatin)

#Prep 410510080
set.seed(1234)
OR_ALL_410510080_DAY_SUM_r <- OR_ALL_410510080_DAY_SUM_r %>% mutate(randnum = runif(316, 0, 1)) %>% arrange(randnum)
train_410510080<- OR_ALL_410510080_DAY_SUM_r[1:253,]
test_410510080<- OR_ALL_410510080_DAY_SUM_r[254:316,]
res_410510080<- lm(data = train_410510080, max_O3 ~ max_T + avg_WS + avg_WX + avg_WY + max_NO + max_NO2 + avg_CO + avg_PM25)
summary(res_410510080)
vif(res_410510080)

Predicted_O3 <- predict(res_410510080, newdata = test_410510080)
asdfg <- cbind(test_410510080, Predicted_O3)
res1 <- lm(data = asdfg, max_O3 ~ Predicted_O3)
summary(res1)
Actual_O3 <- asdfg$max_O3
rplot_Lafayette <- (ggplot() + geom_point(aes(x = Predicted_O3*1000, y = Actual_O3*1000), na.rm = FALSE)
	+ geom_abline(aes(slope = .871405, intercept = .001255, color = 'Line of Best Fit\nY = 0.871*X + 0.00126'), na.rm = FALSE)
	+ geom_abline(aes(slope = 1, intercept = 0, color = '1 to 1 Relationship\nY = X'))
	+ labs(color ="Legend")
	+ xlab("Actual O3 (ppb)")
	+ ylab("Predicted O3 (ppb)")
	+ ggtitle("SE Lafayette")
	+ My_Theme1
)
print(rplot_Lafayette)

#Regressions - excluding SO2, H, P, PM10, NOX
#r_410670005_DAY_SUM <-lm(OR_ALL_410670005_DAY_SUM_r$max_O3 ~ OR_ALL_410670005_DAY_SUM_r$max_T + OR_ALL_410670005_DAY_SUM_r$avg_WS + OR_ALL_410670005_DAY_SUM_r$avg_WX + OR_ALL_410670005_DAY_SUM_r$avg_WY + OR_ALL_410670005_DAY_SUM_r$max_NO2 + OR_ALL_410670005_DAY_SUM_r$max_NO + OR_ALL_410670005_DAY_SUM_r$avg_CO + OR_ALL_410670005_DAY_SUM_r$avg_PM25)
#summary(r_410670005_DAY_SUM)
#vif(r_410670005_DAY_SUM)

#r_410510080_DAY_SUM <-lm(OR_ALL_410510080_DAY_SUM_r$max_O3 ~ OR_ALL_410510080_DAY_SUM_r$max_T + OR_ALL_410510080_DAY_SUM_r$avg_WS + OR_ALL_410510080_DAY_SUM_r$avg_WX + OR_ALL_410510080_DAY_SUM_r$avg_WY + OR_ALL_410510080_DAY_SUM_r$max_NO2 + OR_ALL_410510080_DAY_SUM_r$max_NO + OR_ALL_410510080_DAY_SUM_r$avg_CO + OR_ALL_410510080_DAY_SUM_r$avg_PM25)
#summary(r_410510080_DAY_SUM)
#vif(r_410510080_DAY_SUM)

My_Theme3 = theme(
  title = element_text(size = 22),
  axis.title.x = element_text(size = 17),
  axis.text.x = element_text(size = 13),
  axis.title.y = element_text(size = 17),
  axis.text.y = element_text(size = 13),
  strip.text.x = element_text(size = 15),
  strip.text.y = element_text(size = 15))
day.labs <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
site.labs <- c("SE Lafayette", "Tualatin")
names(day.labs) <- c("1","2","3","4","5","6","7")
names(site.labs) <- c("410510080", "410670005")

AVG_O3_DAY_18 <- OR_O3_18
AVG_O3_DAY_18$day <- wday(AVG_O3_DAY_18$dt_local)
AVG_O3_DAY_18_410510080 <- AVG_O3_DAY_18 %>% filter(site_id == 410510080)
AVG_O3_DAY_18_410670005 <- AVG_O3_DAY_18 %>% filter(site_id == 410670005)
AVG_O3_DAY_18 <- rbind(AVG_O3_DAY_18_410510080,AVG_O3_DAY_18_410670005)
AVG_O3_DAYHOUR_18 <- AVG_O3_DAY_18 %>%
  group_by(site_id, day, hour) %>%
  summarize(avg_O3 = mean(Sample.Measurement, na.rm = TRUE)*1000,
            sd_O3 = sd(Sample.Measurement, na.rm = TRUE)*1000,
            max_O3 = max(Sample.Measurement, na.rm = TRUE)*1000,
            min_O3 = min(Sample.Measurement, na.rm = TRUE)*1000)

OR_ALL_18_O3_p <- (ggplot(AVG_O3_DAYHOUR_18, aes(x=hour))
	+ geom_line(aes(y = avg_O3), color = 'darkred',size=1)
	+ geom_line(aes(y = avg_O3+sd_O3), color = 'steelblue',size=1)
	+ geom_line(aes(y = avg_O3-sd_O3), color = 'steelblue',size=1)
#	+ facet_grid(rows=vars(site_id), cols=vars(day))
	+ facet_grid(site_id ~ day, labeller = labeller(site_id = site.labs, day = day.labs))
	+ ggtitle("Average 2018 Ozone Concentrations by Day")
	+ xlab("Hour")
	+ ylab("Ozone (ppb)")
	+ labs(color = "Model")
	+ My_Theme3
)
print(OR_ALL_18_O3_p)

AVG_NO_DAY_18 <- OR_NOX_18 %>% filter(Parameter.Name == "Oxides of nitrogen (NOx)")
AVG_NO_DAY_18$day <- wday(AVG_NO_DAY_18$dt_local)
AVG_NO_DAY_18$hour <- hour(AVG_NO_DAY_18$dt_local)
AVG_NO_DAY_18_410510080 <- AVG_NO_DAY_18 %>% filter(site_id == 410510080)
AVG_NO_DAY_18_410670005 <- AVG_NO_DAY_18 %>% filter(site_id == 410670005)
AVG_NO_DAY_18 <- rbind(AVG_NO_DAY_18_410510080,AVG_NO_DAY_18_410670005)
AVG_NO_DAYHOUR_18 <- AVG_NO_DAY_18 %>%
  group_by(site_id, day, hour) %>%
  summarize(avg_NO= mean(Sample.Measurement, na.rm = TRUE),
            sd_NO= sd(Sample.Measurement, na.rm = TRUE),
            max_NO= max(Sample.Measurement, na.rm = TRUE),
            min_NO= min(Sample.Measurement, na.rm = TRUE))

OR_ALL_18_NO_p <- (ggplot(AVG_NO_DAYHOUR_18, aes(x=hour))
	+ geom_line(aes(y = avg_NO), color = 'darkred',size=1)
	+ geom_line(aes(y = avg_NO+sd_NO), color = 'steelblue',size=1)
	+ geom_line(aes(y = avg_NO-sd_NO), color = 'steelblue',size=1)
#	+ facet_grid(rows=vars(site_id), cols=vars(day))
	+ facet_grid(site_id ~ day, labeller = labeller(site_id = site.labs, day = day.labs))
	+ ggtitle("Average 2018 Oxides of Nitrogen Concentrations by Day of Week")
	+ xlab("Hour")
	+ ylab("NOx (ppb)")
	+ labs(color = "Model")
	+ My_Theme3
)
print(OR_ALL_18_NO_p)


