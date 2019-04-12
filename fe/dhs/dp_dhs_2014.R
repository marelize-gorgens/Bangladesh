#----------------------------------------------------------------------------------------------------------
# dp_DHS_.R
# Description: Data preprocessing of DHS
# Organization: Health, Nutrition, and Population (HNP) | The World Bank
# Author: Hellen Matarazzo
# Date: Updated on 04-11-2019
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
if(!require(dummies)) install.packages("dummies");library(dummies)
if(!require(readxl)) installed.packages("readxl", dependencies=TRUE); library(readxl)

#----------------------------------------------------------------------------------------------------------
# Load datasets
#----------------------------------------------------------------------------------------------------------

dhs14files <- list.files("./data/dhs/datasets",recursive=TRUE, pattern="*7", full.names=TRUE) # List files 
dhs14 <- sapply(dhs14files,readRDS) # Load each dataset as data frame
names(dhs14) <- c("BR72","FC7ADTSP","FC7ADTSR","HR72", "IR72", "KR72", "PR72", "PV7ADTSP", "PV7ADTSR", "SL7ADTSP", "SL7ADTSR", "SQ71") # Rename lists

#----------------------------------------------------------------------------------------------------------
# Survey design
#----------------------------------------------------------------------------------------------------------

# PR: Household member recode dataset ---------------------------------------------------------------------
pr <- dhs14$PR72
pr_var <- get_variable_labels(pr) # Get the data dictionary
# sum(duplicated(pr)) # Uniqueness: Check for duplicates
# sum(is.na(pr)) # Completeness: Check for NAs
# na_pr <- data.frame(lapply(pr, function(y) sum(length(which(is.na(y)))))) # Save NAs by column
pr$wgt <- pr$hv005/1000000 # Create the weight variable
# prdesign <- svydesign(data=pr, id=~hv021, weight=~wgt, strata=~hv023) # Set survey design for dataset | id: cluster or psu
districts <- data.frame(pr[c(4,5,131)])
districts <- unique(districts)

# IR: Individual recode - Women's (15-49 years) dataset ---------------------------------------------------
ir <- as.data.frame(dhs14$IR72)
ir$wgt <- ir$v005/1000000 # Create the weight variable
ir_var <- get_variable_labels(ir) # Get the data dictionary 
ir <- merge(ir, districts, all.x=TRUE, by.x=c("v001","v002"), by.y=c("hv001","hv002"))

# BR: Births recode - Everychild <5y from interviewed mother (15-49 years) dataset -----------------------
br <- as.data.frame(dhs14$BR72)
br$wgt <- br$v005/1000000 # Create the weight variable
br_var <- get_variable_labels(br) # Get the data dictionary
br <- merge(br, districts, all.x=TRUE, by.x=c("v001","v002"), by.y=c("hv001","hv002"))

# KR: Childrens recode - Children <5y from interviewed mother (15-49 years) dataset -----------------------
kr <- as.data.frame(dhs14$KR72)
kr$wgt <- kr$v005/1000000 # Create the weight variable
kr_var <- get_variable_labels(kr) # Get the data dictionary
kr <- merge(kr, districts, all.x=TRUE, by.x=c("v001","v002"), by.y=c("hv001","hv002"))

#----------------------------------------------------------------------------------------------------------
# List of varibales of interest
#----------------------------------------------------------------------------------------------------------

# PR: Household member recode dataset ---------------------------------------------------------------------
### Unit
# hv024	      region (~Division)
# shdistrict	type of district
# shupazila	  upazila
# sh231       sex of household member (1=male, 2=female)
# hv105       age of household members (97=97+, 98=don't know)
# hv025       type of place of residence | Rural==1

### Antropometry
# shpc1	  height/age percentile
# shsd1	  height/age standard deviations
# shrm1	  height/age percent reference median
# shrm3	  weight/height percent reference median (dhs)
# shrm4	  weight/height percent reference median (fog)
# shrm5	  weight/height percent reference median (who)
# shsd3	  weight/height standard deviations (dhs)
# shbm	  body mass index for respondent

### Education
# hv106	  highest educational level attained

### Others
# hv015	  result of household interview
# hv104	  sex of household member
# hv237	  anything done to water to make safe to drink
# hv252	  frequency household members smoke inside the house
# hv270	  wealth index
# hv271	  wealth index factor score (5 decimals)
# hv116	  current marital status
# sh13	  currently working
# sh21	  eligible for height and weight tests: man
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
# hv105   age of household members
# hv219   sex of head of household | Female==2
# sh13    currently working

#----------------------------------------------------------------------------------------------------------
# Data transformation
#----------------------------------------------------------------------------------------------------------

# Births
births <- ddply(br, ~shdistrict, summarise, # Summarize by district
                "no_births_last3y"=round(sum(((v008-b3)>=1 & (v008-b3)<36)*wgt)*100,2))
# "rate_under5y_mortality"=round((b7<5)*wgt/))

# Women
women <- ddply(ir, ~shdistrict, summarise, # Summarize by district
               "prop_current_contraceptive"= round(sum((v313!=0 & v502==1)*wgt, na.rm=TRUE)/sum((v502==1)*wgt, na.rm=TRUE)*100,2),
               "prop_unmet_need_family_planing"=round(sum(((v626a == 1 | v626a == 2) & v502==1)*wgt, na.rm=TRUE)/sum((v502==1)*wgt, na.rm=TRUE)*100,2),
               "prop_antenatal_coverage"=round(sum((m2a_1==1)*wgt, na.rm=TRUE)/sum((midx_1==1)*wgt, na.rm=TRUE)*100,2),
               "prop_antenatal_care4+"=round(sum((m14_1>=4 & m14_1<=20)*wgt, na.rm=TRUE)/sum((midx_1==1)*wgt, na.rm=TRUE)*100,2),
               "prop_institutional_delivery"=round(sum((m15_1>=20 & m15_1<=39)*wgt, na.rm=TRUE)/sum((midx_1 == 1)*wgt, na.rm=TRUE)*100,2),
               "prop_attendant_delivery"=round(sum((m3a_1==1 | m3b_1==1 | m3c_1==1 | m3d_1==1 | m3e_1==1)*wgt, na.rm=TRUE)/sum((midx_1 == 1)*wgt, na.rm=TRUE)*100,2),
               "prop_caesarean"=round(sum((m17_1==1)*wgt, na.rm=TRUE)/sum((midx_1 == 1)*wgt, na.rm=TRUE)*100,2))

# Demographic
demo <- ddply(pr, ~shdistrict, summarise, # Summarize by district
              "no_total_pop"=length(hv015),
              "no_pop_>15y"=sum((hv105>15)*wgt, na.rm=TRUE),
              "no_pop_15-19y"=sum((hv105>=15 & hv105<=19)*wgt, na.rm=TRUE),
              "no_pop_>35y"=sum((hv105>35)*wgt, na.rm=TRUE),
              "no_women_15-19y"=sum((hv105>=15 & hv105<=19 & hv104==2)*wgt, na.rm=TRUE),
              "no_women_15-45y"=sum((hv105>=15 & hv105<=45 & hv104==2)*wgt, na.rm=TRUE),
              "no_men_>15y"=sum((hv105>=15 & hv104==1)*wgt, na.rm=TRUE),
              "no_women_15-45_men_>=15y" = sum(((hv105>=15 & hv105<=45 & hv104==2) | (hv105>=15 & hv104==1))*wgt, na.rm=TRUE),
              "no_child_under5y"=sum((hv105<5)*wgt, na.rm=TRUE),
              "no_child_1-4y" = sum((hv105>=1 & hv105<=4)*wgt, na.rm=TRUE),
              "no_child_0-5y" = sum((hv105<=5)*wgt, na.rm=TRUE),
              "no_married_>=15y"=sum((hv105>=15 & hv116==1)*wgt, na.rm=TRUE),
              "sex_ratio"=round(sum((hv104==1)*wgt, na.rm=TRUE)/sum((hv104==2)*wgt, na.rm=TRUE)*100,2),
              "dependency_ratio"=round((sum((hv105<=14)*wgt, na.rm=TRUE) + sum((hv105>=65)*wgt, na.rm=TRUE))/sum((hv105>=15 & hv105<=64)*wgt, na.rm=TRUE)*100,2),
              "prop_pop_rural"=round(sum((hv025==1)*wgt, na.rm=TRUE)/length(hv015)*100,2),
              "prop_pop_women"=round(sum((hv104==2)*wgt, na.rm=TRUE)/length(hv015)*100,2),
              "prop_pop_rural_women"=round(sum((hv104==2 & hv025==1)*wgt, na.rm=TRUE)/length(hv015)*100,2),
              "prop_women_15-45y_overwomen"=round(sum((hv105>=15 & hv105<=45 & hv104==2)*wgt, na.rm=TRUE)/sum((hv104==2)*wgt, na.rm=TRUE)*100,2),
              "prop_women_15-45y_overtotal"=round(sum((hv105>=15 & hv105<=45 & hv104==2)*wgt, na.rm=TRUE)/length(hv015)*100,2),
              "prop_married_women_15-45y"=round(sum((hv105>=15 & hv105<=45 & hv104==2 & hv116==1)*wgt, na.rm=TRUE)/sum((hv105>=15 & hv105<=45 & hv104==2)*wgt, na.rm=TRUE)*100,2),
              "prop_married_>=15y"=round(sum((hv105>=15 & hv116==1)*wgt, na.rm=TRUE)/sum((hv105>=15)*wgt, na.rm=TRUE)*100,2),
              "prop_female_head"=round(sum((hv219==2)*wgt, na.rm=TRUE)/length(hv015)*100,2),
              "prop_registered_under5"=round(sum(((hv140==1 | hv140==2) & hv102==1 & hv105<5)*wgt, na.rm=TRUE)/sum((hv102==1 & hv105<5)*wgt, na.rm=TRUE)*100,2))
# "prop_stunting_under5y"=/sum((hv105<5)*wgt, na.rm=TRUE),
# "prop_overweight_under5y"=/sum((hv105<5)*wgt, na.rm=TRUE),
# "prop_wasting_under5y"=/sum((hv105<5)*wgt, na.rm=TRUE))

# Indicators
# demo$rate_fertility <- round((births$no_births_last3y/demo$`no_women_15-45y`)*1000,2)
demo$year <- 2014

# Merge
final1 <- merge(births, women, all=TRUE)
final <- merge(final1, demo, all=TRUE)

# Rename Zilas
temp <- read_xlsx("./data/geo_files/bbs_geos/geo.xlsx", skip=1)
temp <- data.frame(temp[3], temp[4])
temp <- unique(temp)
final <- merge(temp, final, all.y=TRUE, by.x="District.Code", by.y="shdistrict")
final <- data.frame(final[-c(1)])
names(final)[1] <- "district"
final$district <- gsub(" Zila", "", final$district)

#----------------------------------------------------------------------------------------------------------
# Save final dataset/metadata
#----------------------------------------------------------------------------------------------------------

write.csv(final,"./output/dhs/data/data_dhs_2014.csv", row.names=FALSE) # Save metadata

meta_dhs14 <- data.frame("Source"="DHS2014", "File"= "72DT","Variable"=colnames(final))
write.csv(meta_dhs14,"./output/dhs/data/metadata_dhs_2014.csv", row.names=FALSE) # Save metadata

# Save only ratio variables
write.csv(final[c(1,3:9,22:27,29:33)],"./output/dhs/data/data_dhs_2014_clean.csv", row.names=FALSE) # Save data

#----------------------------------------------------------------------------------------------------------
# Create/save dataset/metadata (raw data)
#----------------------------------------------------------------------------------------------------------
write.csv(pr,"./data/dhs/data_dhs_pr_2014.csv", row.names=FALSE) # Save metadata

meta_dhs14 <- data.frame()
for (i in seq_along(dhs14)){
  meta <- data.frame("Source"="DHS2014", "File"= names(dhs14[i]),"Variable"=colnames(dhs14[[i]]), "Description"= sapply(dhs14[[i]], function(x) attributes(x)$label))
  meta_dhs14 <- rbind(meta_dhs14,meta)
}
write.csv(meta_dhs14,"./data/dhs/metadata_dhs_2014.csv", row.names=FALSE) # Save metadata
