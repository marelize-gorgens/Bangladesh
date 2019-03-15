#----------------------------------------------------------------------------------------------------------
# dp_DHS_.R
# Description: Data preprocessing of DHS
# Organization: Health, Nutrition, and Population (HNP) | The World Bank
# Author: Hellen Matarazzo
# Date: Updated on 03-15-2019
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

#----------------------------------------------------------------------------------------------------------
# Load datasets
#----------------------------------------------------------------------------------------------------------

dhs11files <- list.files("./data/dhs/datasets",recursive=TRUE, pattern="*6", full.names=TRUE) # List files 
dhs11 <- sapply(dhs11files,readRDS) # Load each dataset as data frame
names(dhs11) <- c("BR61","CR61","HR61","IR61", "KR61", "MR61", "PR61", "SQ61", "VA62") # Rename lists

attributes(dhs11$BR61)
head(get_variable_labels(dhs11$BR61)) # Get the data dictionary

#----------------------------------------------------------------------------------------------------------
# Survey design
#----------------------------------------------------------------------------------------------------------

pr <- dhs11$PR61
pr$wgt <- pr$hv005/1000000 # Create the weight variable
prdesign <- svydesign(data=pr, id=~hv021, weight=~wgt, strata=~hv023) # Set survey design for dataset | id: cluster or psu
summary(prdesign)

# "source of drinking water"
str(pr$hv201)
attributes(pr$hv201)
xtabs(~hv201, data=pr) # Total by source of drinking water
prop.table(svytable(~hv201, design = prdesign)) # Sex ratios weighted
svymean(~hv201, prdesign) # Mean for sample surveys
svymean(~factor(hv021), prdesign) # Mean for sample surveys as factor
confint(svymean(~hv201, prdesign)) # Confidence intervals
svyby(~hv201, ~shdistrict, prdesign, svymean) # Mean by subset: district

# "sex of household member": hv104

# Blood pressure
# sh246s  systolic blood pressure measurement
# sh246d  diastolic blood pressure measurement
# sh248   blood pressure ever checked
# sh249   told by a doctor to have high blood pressure
# sh250   taking prescribed medicine to lower blood pressure
# sh253   time for second blood pressure test
# sh254   consent for second blood pressure test
# sh255s	systolic blood pressure
# sh255d  diastolic blood pressure
# sh262   time for third blood pressure test
# sh263	  consent for third blood pressure test
# sh264s	systolic blood pressure
# sh264d	diastolic blood pressure
# fsysto	valid systolic blood presure
# fdysto	valid dystolic blood pressure
# sh276	  consent for blood sugar testing
# sh279	  consent for blood sugar testing
# sh284a	plasma blood glucose (mmol/l)
# sh284g	blood glucose (mmol/dl)
# sh284b	plasma blood glucose (mg/dl)

# Glucose
# sh284	  glucose testing (mg/dl)
# sh284a	plasma blood glucose (mmol/l)
# sh284g	blood glucose (mmol/dl)
# sh284b	plasma blood glucose (mg/dl)

# Diabetes
# sh257	heard of illness diabetes
# sh258	ever told by a doctor/nurse to have diabetes
# sh259	taking medication for the diabetes


#----------------------------------------------------------------------------------------------------------
# Create/save metadata
#----------------------------------------------------------------------------------------------------------

meta_dhs11 <- data.frame()
for (i in seq_along(dhs11)){
  meta <- data.frame("Source"="DHS2011", "File"= names(dhs11[i]),"Variable"=colnames(dhs11[[i]]), "Description"= sapply(dhs11[[i]], function(x) attributes(x)$label))
  meta_dhs11 <- rbind(meta_dhs11,meta)
}
write.csv(meta_dhs11,"./output/dhs/data/metadata_dhs_2011.csv", row.names=FALSE) # Save metadata

write.csv(pr,"./output/dhs/data/test.csv", row.names=FALSE) # Save metadata
