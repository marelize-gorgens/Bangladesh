#----------------------------------------------------------------------------------------------------------
# dp_DHS_.R
# Description: Data preprocessing of DHS
# Organization: Health, Nutrition, and Population (HNP) | The World Bank
# Author: Hellen Matarazzo
# Date: Updated on 03-19-2019
#----------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------
# Clear memory
#----------------------------------------------------------------------------------------------------------

rm(list=ls()) # Remove objects
graphics.off # Close graphics
cat("\014") # Clear console

#----------------------------------------------------------------------------------------------------------
# Load packages
#----------------------------------------------------------------------------------------------------------

if(!require(rdhs)) installed.packages("rdhs"); library(rdhs)
if(!require(tidyverse)) installed.packages("tidyverse", dependencies=TRUE); library(tidyverse)
if(!require(haven)) installed.packages("haven", dependencies=TRUE); library(haven)
if(!require(survey)) installed.packages("survey", dependencies=TRUE); library(survey)
if(!require(reshape2)) installed.packages("reshape2", dependencies=TRUE); library(reshape2)
if(!require(plyr)) installed.packages("plyr", dependencies=TRUE); library(plyr)
if(!require(dummies)) installed.packages("dummies");library(dummies)
   
#----------------------------------------------------------------------------------------------------------
# Load datasets
#----------------------------------------------------------------------------------------------------------

dhs11files <- list.files("./data/dhs/datasets",recursive=TRUE, pattern="*6", full.names=TRUE) # List files 
dhs11 <- sapply(dhs11files,readRDS) # Load each dataset as data frame
names(dhs11) <- c("BR61","CR61","HR61","IR61", "KR61", "MR61", "PR61", "SQ61", "VA62") # Rename lists

#----------------------------------------------------------------------------------------------------------
# Survey design
#----------------------------------------------------------------------------------------------------------

# Household member recode dataset
pr <- dhs11$PR61
pr_var <- get_variable_labels(pr) # Get the data dictionary
pr$wgt <- pr$hv005/1000000 # Create the weight variable
prdesign <- svydesign(data=pr, id=~hv021, weight=~wgt, strata=~hv023) # Set survey design for dataset | id: cluster or psu
sum(duplicated(pr)) # Uniqueness: Check for duplicates
sum(is.na(pr)) # Completeness: Check for NAs
na_pr <- data.frame(lapply(pr, function(y) sum(length(which(is.na(y)))))) # Save NAs by column

# Individual recode - Women's (15-49 years) dataset
# ir <- as.data.frame(dhs11$IR61)
# ir_var <- get_variable_labels(ir) # Get the data dictionary 
# ir$wgt <- ir$v005/1000000 # Create the weight variable
# irdesign <- svydesign(data=ir, id=~v021, weight=~wgt, strata=~v023) # Set survey design for dataset | id: cluster or psu

#----------------------------------------------------------------------------------------------------------
# List of varibales of interest
#----------------------------------------------------------------------------------------------------------

### Unit
# hv024	      region (~Division)
# shdistrict	type of district
# shupazila	  upazila
# sh231       sex of household member (1=male, 2=female)
# hv105       age of household members (97=97+, 98=don't know)

### Blood pressure
# sh248   blood pressure ever checked (1=yes)
# sh249   told by a doctor to have high blood pressure (1=yes)
# sh250   taking prescribed medicine to lower blood pressure (1=yes)
# sh241	  consent for blood pressure measurement (1=granted, 2=refused, 3=not present)
# sh246s  systolic blood pressure measurement
# sh246d  diastolic blood pressure measurement
# sh254   consent for second blood pressure test (1=yes)
# sh255s	systolic blood pressure
# sh255d  diastolic blood pressure
# sh263	  consent for third blood pressure test (1=yes)
# sh264s	systolic blood pressure
# sh264d	diastolic blood pressure
# fdysto	valid dystolic blood pressure (996=others, 999=missing)
# sh19	  eligible for blood glucose and blood pressure tests: woman  (1=yes)
# sh20	  eligible for height, weight,blood glucose and blood pressure tests: woman  (1=yes)
# sh22	  eligible for height, weight,blood glucose and blood pressure tests: man  (1=yes)

### Glucose
# sh276	  consent for blood sugar testing
# sh279	  consent for blood sugar testing
# sh284	  glucose testing (mg/dl)
# sh284a	plasma blood glucose (mmol/l)
# sh284g	blood glucose (mmol/dl)
# sh284b	plasma blood glucose (mg/dl)

### Diabetes
# sh257	  heard of illness diabetes
# sh258	  ever told by a doctor/nurse to have diabetes
# sh259	  taking medication for the diabetes

### Antropometry
# shpc1	  height/age percentile
# shsd1	  height/age standard deviations
# shrm1	  height/age percent reference median
# shrm3	  weight/height percent reference median (dhs)
# shrm4	  weight/height percent reference median (fog)
# shrm5	  weight/height percent reference median (who)
# shsd3	  weight/height standard deviations (dhs)
# shbm	  body mass index for respondent

### Anemia
# hv042   household selected for hemoglobin
# ha53	  hemoglobin level (g/dl - 1 decimal)
# ha54	  currently pregnant
# ha55	  result of measurement - hemoglobin
# ha56	  hemoglobin level adjusted for altitude and smoking (g/dl - 1 decimal)
# ha57	  anemia level

### Education
# hv105	  age of household members
# hv106	  highest educational level attained
# hv107	  highest year of education completed

### Others
# hv015	  result of household interview
# hv104	  sex of household member
# hv237	  anything done to water to make safe to drink
# hv252	  frequency household members smoke inside the house
# hv270	  wealth index
# hv271	  wealth index factor score (5 decimals)
# sh08	  current marital status
# sh13	  currently working
# sh18	  eligible for height, weight and anemia tests: woman
# sh19	  eligible for blood glucose and blood pressure tests: woman
# sh20	  eligible for height, weight,blood glucose and blood pressure tests: woman
# sh21	  eligible for height and weight tests: man
# sh22	  eligible for height,weight,blood glucose and blood pressure tests:man
# ha1	    woman's age in years
# ha2	    woman's weight in kilograms (1 decimal)
# ha3	    woman's height in centimeters (1 decimal)
# ha4	    height/age percentile
# ha5	    height/age standard deviation
# ha6	    height/age percent ref. median
# ha11	  weight/height standard deviation (dhs)
# ha12	  weight/height percent ref. median (dhs)
# ha12a	  weight/height percent ref. median (fog)
# ha12b	  weight/height percent ref. median (who)
# ha13	  result of measurement - height/weight
# ha32	  date of birth (cmc)
# ha40	  body mass index

#----------------------------------------------------------------------------------------------------------
# Data transformation
#----------------------------------------------------------------------------------------------------------

# Hypertension --------------------------------------------------------------------------------------------

# Blood pressure variables
pr$blood_pressure_measured <- ifelse((pr$sh19==1 | pr$sh20==1 | pr$sh22==1) & (pr$sh241==1 & pr$sh254==1 & pr$sh263==1) & (pr$sh255s!=0 & pr$sh264s!=0 & pr$sh255d!=0 & pr$sh264d!=0), 1, 0) # Create dummy for eligible, consented and have 2nd and 3rd measurements
pr$systolic_pressure <- ifelse(pr$blood_pressure_measured==1, (pr$sh255s+pr$sh264s)/2,NA) # Average systolic pressure of 2nd and 3rd measurements
pr$diastolic_pressure <- ifelse(pr$blood_pressure_measured==1, (pr$sh255d+pr$sh264d)/2,NA) # Average diastolic pressure of 2nd and 3rd measurements
pr %>% 
  filter(blood_pressure_measured==1) %>%
  select("sh255s","sh264s","sh255d","sh264d") %>%
  summarise_each(funs(min, max)) # Validity check: Max and min of 2nd and 3rd measurements

pr$blood_pressure_cat <- ifelse(pr$sh250==1, "hypertensive_1",
                                ifelse(pr$systolic_pressure<120 & pr$diastolic_pressure <80, "normal", # Define categories
                                       ifelse((pr$systolic_pressure>=120 & pr$systolic_pressure<140) | (pr$diastolic_pressure>=80 & pr$diastolic_pressure<90), "prehypertension",
                                              ifelse((pr$systolic_pressure>=140 & pr$systolic_pressure<160) | (pr$diastolic_pressure>=90 & pr$diastolic_pressure<100), "hypertensive_1",
                                                     ifelse(pr$systolic_pressure>=160 | pr$diastolic_pressure>=100,"hypertensive_2",
                                                            NA)))))
pr$blood_pressure_cat <- factor(pr$blood_pressure_cat, exclude=c(NA,"")) # Convert to factor
pr$blood_pressure_cat_ <- pr$blood_pressure_cat # Duplicate variable
pr <- dummy.data.frame(pr, names="blood_pressure_cat_") # Create a dummy for each level
pr$blood_pressure_cat_NA <- NULL
pr$blood_pressure_cat_hypertensive <- ifelse(pr$blood_pressure_cat_hypertensive_1==1 | pr$blood_pressure_cat_hypertensive_2==1, 1, 0)
pr$blood_pressure_cat_hypertensive[is.na(pr$blood_pressure_cat)] <- NA # Convert to NA all non respondents
pr$blood_pressure_cat_normal[is.na(pr$blood_pressure_cat)] <- NA 
pr$blood_pressure_cat_prehypertension[is.na(pr$blood_pressure_cat)] <- NA
pr$blood_pressure_cat_hypertensive_1[is.na(pr$blood_pressure_cat)] <- NA
pr$blood_pressure_cat_hypertensive_2[is.na(pr$blood_pressure_cat)] <- NA

# Blood pressure by district
final <- ddply(pr, ~shdistrict, summarise, # Summarize by district
               mean_systolic_pressure=weighted.mean(systolic_pressure, wgt, na.rm=TRUE), 
               mean_diastolic_pressure=weighted.mean(diastolic_pressure, wgt, na.rm=TRUE),
               normal=weighted.mean(blood_pressure_cat_normal, wgt, na.rm=TRUE),
               prehypertension=weighted.mean(blood_pressure_cat_prehypertension, wgt, na.rm=TRUE),
               hypertensive_1=weighted.mean(blood_pressure_cat_hypertensive_1, wgt, na.rm=TRUE),
               hypertensive_2=weighted.mean(blood_pressure_cat_hypertensive_2, wgt, na.rm=TRUE),
               hypertensive=weighted.mean(blood_pressure_cat_hypertensive, wgt, na.rm=TRUE))

#----------------------------------------------------------------------------------------------------------
# Save final dataset
#----------------------------------------------------------------------------------------------------------

write.csv(final,"./output/dhs/data/data_dhs_2011.csv", row.names=FALSE) # Save metadata
write.csv(pr,"./output/dhs/data/data_dhs_pr_2011.csv", row.names=FALSE) # Save metadata

#----------------------------------------------------------------------------------------------------------
# Create/save metadata
#----------------------------------------------------------------------------------------------------------

meta_dhs11 <- data.frame()
for (i in seq_along(dhs11)){
  meta <- data.frame("Source"="DHS2011", "File"= names(dhs11[i]),"Variable"=colnames(dhs11[[i]]), "Description"= sapply(dhs11[[i]], function(x) attributes(x)$label))
  meta_dhs11 <- rbind(meta_dhs11,meta)
}
write.csv(meta_dhs11,"./output/dhs/data/metadata_dhs_2011.csv", row.names=FALSE) # Save metadata
