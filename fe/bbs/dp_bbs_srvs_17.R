#----------------------------------------------------------------------------------------------------------
# dp_bbs_srvs_.R
# Description: Data preprocessing of SRVS datasets
# Organization: Health, Nutrition, and Population (HNP) | The World Bank
# Author: Hellen Matarazzo
# Date: Updated on 05-12-2019
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

if(!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE); library(tidyverse)
if(!require(haven)) install.packages("haven", dependencies=TRUE); library(haven)
if(!require(rdhs)) install.packages("rdhs"); library(rdhs)
if(!require(plyr)) install.packages("plyr", dependencies=TRUE); library(plyr)
if(!require(readxl)) install.packages("readxl", dependencies=TRUE); library(readxl)

#----------------------------------------------------------------------------------------------------------
# Load datasets
#----------------------------------------------------------------------------------------------------------

svrs17files <- list.files("./data/bbs/svrs/SVRS_17/", recursive=TRUE, pattern="*.dta", full.names=TRUE) # List files
svrs17 <- sapply(svrs17files, read_dta)
names(svrs17) <- c("tafsil-10","tafsil-3","tafsil-4","tafsil-5","tafsil-6","tafsil-7","tafsil-8","tafsil-9","tafsil-11","tafsil-2h","tafsil-2p") # Rename lists

# Section 2: Household roster and demographic characteristics ---------------------------------------------
tafsil2h <- svrs17$'tafsil-2h' # Load as data frame
tafsil2h_var <- get_variable_labels(tafsil2h) # Get the data dictionary

tafsil2p <- svrs17$'tafsil-2p'
tafsil2p_var <- get_variable_labels(tafsil2p)

# Section 3: Birth ----------------------------------------------------------------------------------------
tafsil3 <- svrs17$'tafsil-3'
tafsil3_var <- get_variable_labels(tafsil3)

# Section 4: Deaths ---------------------------------------------------------------------------------------
tafsil4 <- svrs17$'tafsil-4'
tafsil4_var <- get_variable_labels(tafsil4)

# Section 5: Marriage -------------------------------------------------------------------------------------
tafsil5 <- svrs17$'tafsil-5'
tafsil5_var <- get_variable_labels(tafsil5)

# Section 6: Divorce --------------------------------------------------------------------------------------
tafsil6 <- svrs17$'tafsil-6'
tafsil6_var <- get_variable_labels(tafsil6)
tafsil6 <- subset(tafsil6, !is.na(zila))
tafsil6 <- tafsil6[!(tafsil6$zila==95),]

# Section 7: Out-migration --------------------------------------------------------------------------------
tafsil7 <- svrs17$'tafsil-7'
tafsil7_var <- get_variable_labels(tafsil7)

# Section 8: In-migration ---------------------------------------------------------------------------------
tafsil8 <- svrs17$'tafsil-8'
tafsil8_var <- get_variable_labels(tafsil8)

# Section 9: Use of Contraceptives ------------------------------------------------------------------------
tafsil9 <- svrs17$'tafsil-9'
tafsil9_var <- get_variable_labels(tafsil9)

# Section 10: Disability ----------------------------------------------------------------------------------
tafsil10 <- svrs17$'tafsil-10'
tafsil10_var <- get_variable_labels(tafsil10)

# Section 11: HIV/AIDS ------------------------------------------------------------------------------------
tafsil11 <- svrs17$'tafsil-11'
tafsil11_var <- get_variable_labels(tafsil11)

#----------------------------------------------------------------------------------------------------------
# List of variables of interest
#----------------------------------------------------------------------------------------------------------

# Section 2: Household roster and demographic characteristics
# Section 3: Birth 
# Section 4: Death 
# Section 9: Use of Contraceptives 

# Section 2: Household roster and demographic characteristics ---------------------------------------------
# "tafsil-2H" 
# zila # Zila Code
# upza # Upazila Code
# rmo # Rural Urban Code
# hh_no # Household Number
# q2_1 # Sources of Drinking Water
# q_4 # Sources of light
# q_5 # Sources of Fuel
# q_6 # Toilet Facilities

# "tafsil-2P" 
# zila # Zila Code
# upza # Upazila Code
# rmo # Rural Urban Code
# q_10 # Age
# q_11 # Sex | Men==1
# q_14 # Marital Status | Married==2
# q_16 # Level of Education
# q_19 # Literacy
# q_20 # Education
# hhsize # RECODE of tot_pop
# agecat1 # RECODE of q_10
# agecat2 # RECODE of q_10
# resi # RECODE of rmo | Rural==1
# divn # RECODE of psu_no
# sex # RECODE of q_11
# agecatLT # RECODE of q_10

# Section 3: Birth ----------------------------------------------------------------------------------------
# "tafsil-3" 
# zila # Zila Code
# upza # Upazila Code
# rmo # Rural Urban Code
# q_4 # Birth Registration Yes/No (within 45 days of the birth) | Yes==1
# q_7 # Birth Attendent
      # 1. Trained Doctor, 
      # 2. Nurse, Midwife, paramedic, Family welfare visitors (FVW), 
      # 3. Medical Assistant (MA)/ Sun assistant community medical officer (SACMO)
      # 4. Health Assistant (HA), Family welfare asssntant (FWA)
      # 5. Untrained treaditioanl birth attandent 
      # 6. Untrained village doctor/ QUACK
      # 7. Neighbour/Relatives
      # 9. Others
# q_9 # Live Birth | Yes==1
# q_12 # Mothers Age (in full years)

# Section 4: Deaths ---------------------------------------------------------------------------------------
# q_3y # Age
# q_5 # Causes of Death

# Section 9: Use of Contraceptives ------------------------------------------------------------------------
# "tafsil-9" 
# zila # Zila Code
# upza # Upazila Code
# rmo # Rural Urban Code
# i4 # Current age (in full years)
# i5 # Education
      # 1.	Did not passed 1st grade 
      # 2.	Passed 1st grade 
      # 3.	Passed 2nd grade 
      # 4.	Passed 3rd grade 
      # 5.	Passed 4th grade 
      # 6.	Passed 5th grade 
      # 7.	Passed 6th grade 
      # 8.	Passed 7th grade 
      # 9.	Passed 8th grade 
      # 10.	 Passed 9th grade 
      # 11.	 Secondery or equal 
      # 12.	 Higher secondary or equal
      # 13.	 Undergraduate or equal 
      # 14.	 Graduate or equal 
      # 15.	 Doctor/Engineer/Agriculturist 
      # 16.	 Diploma
      # 17.	 Vocational 
      # 18.	 Others 
# i14 # Currently use contrceptiev techniques | Yes==1

#----------------------------------------------------------------------------------------------------------
# Data transformation
#----------------------------------------------------------------------------------------------------------

# Zila ----------------------------------------------------------------------------------------------------
# Births
births <- ddply(tafsil3, ~zila, summarise, # Summarize by district
                "no_births"=length(q_9),
                "no_live_births"=sum(q_9==1),
                "no_registered_births"=sum(q_4==1),
                "prop_live_births"=round(sum(q_9==1)/length(q_9)*100,2),
                "prop_registered_births"=round(sum(q_4==1)/length(q_9)*100,2),
                "prop_attendant_delivery"=round(sum(q_7 %in% c(1,2,3))/length(q_9)*100,2),
                "mean_mothers_age"=round(mean(q_12),0))

# Deaths
deaths <- ddply(tafsil4, ~zila, summarise, # Summarize by district
                "no_deaths"=length(q_2),
                "no_deaths_rural"=sum(resi==1),
                "no_deaths_under5y"=sum(q_3y<5),
                "no_deaths_1-4y"=sum(q_3y>=1 & q_3y<=4),
                "no_deaths_under1y"=sum(q_3y<1),
                "no_maternal_deaths"=sum(q_5 %in% c(37,38,39,40,41,42,43) & (q_3y>=15 & q_3y<=49) & q_2==2),
                "prop_deaths_rural"=round(sum(resi==1)/length(q_2)*100,2))

# Marriage
marriage <- ddply(tafsil5, ~zila, summarise, # Summarize by district
                "no_marriage"=length(hh_no),
                "mean_age_marriage"=round(mean(q_4),0))

# Divorce
divorce <- ddply(tafsil6, ~zila, summarise, # Summarize by district
                 "no_divorce"=sum(q_1==1, na.rm=TRUE),
                 "no_separation"=sum(q_1==2, na.rm=TRUE))

# Out-migration
outmigration <- ddply(tafsil7, ~zila, summarise,
                    "no_out"=length(hh_no),
                    "no_out_women"=sum(q_2==2),
                    "no_out_men"=sum(q_2==1),
                    "no_out_0to14y" = sum(q_3<15),
                    "no_out_15to44y" = sum(q_3>=15 & q_3<=44),
                    "no_out_45to64y" = sum(q_3>=45 & q_3<=64),
                    "no_out_65yplus" = sum(q_3>=65))

# In-migration
immigration <- ddply(tafsil8, ~zila, summarise,
                     "no_in"=length(hh_no),
                     "no_in_women"=sum(q_2==2),
                     "no_in_men"=sum(q_2==1),
                     "no_in_0to14y" = sum(q_3<15),
                     "no_in_15to44y" = sum(q_3>=15 & q_3<=44),
                     "no_in_45to64y" = sum(q_3>=45 & q_3<=64),
                     "no_in_65yplus" = sum(q_3>=65))

# Household
house <- ddply(tafsil2h, ~zila, summarise,
               "prop_drinking_tubewell_water"=round(sum(q21==2)/length(hh_no)*100,2),
               "prop_source_light_eletricity"=round(sum(q4==1)/length(hh_no)*100,2),
               "prop_sanitary_with_water"=round(sum(q6==1)/length(hh_no)*100,2),
               "prop_sanitary_open"=round(sum(q6==4)/length(hh_no)*100,2),
               "prop_house_building"=round(sum(q1_1a!=0)/length(hh_no)*100,2),
               "prop_house_semipucca"=round(sum(q1_2a!=0)/length(hh_no)*100,2),
               "prop_house_wooden"=round(sum(q1_3a!=0)/length(hh_no)*100,2),
               "prop_house_mud"=round(sum(q1_4a!=0)/length(hh_no)*100,2),
               "prop_house_bamboo"=round(sum(q1_5a!=0)/length(hh_no)*100,2))

# Household members 
demo <- ddply(tafsil2p, ~zila, summarise,
              "total_pop"=length(tot_pop),
              "pop_>15y"=sum(q_10>15),
              "pop_15-19y"=sum(q_10>=15 & q_10<=19),
              "pop_>35y"=sum(q_10>35),
              "women_15-19y"=sum((q_10>=15 & q_10<=19) & q_11==2),
              "women_15-45y"=sum((q_10>=15 & q_10<=45) & q_11==2),
              "men_>15y"=sum(q_10>=15 & q_11==1),
              "women_15-45_men_>=15y" = sum(((q_10>=15 & q_10<=45) & q_11==2) | (q_10>=15 & q_11==1)),
              "child_under5y"=sum(q_10<5),
              "child_1-4y" = sum(q_10>=1 & q_10<=4),
              "child_0-5y" = sum(q_10<=5),
              "no_married_>=15y"=sum(q_10>=15 & q_14==2, na.rm=TRUE),
              "sex_ratio"=round(sum(q_11==1)/sum(q_11==2)*100,2),
              "dependency_ratio"=round((sum(q_10<=14) + sum(q_10>=65))/sum(q_10>=15 & q_10<=64)*100,2),
              "prop_pop_rural"=round(sum(resi==1)/length(tot_pop)*100,2),
              "prop_pop_women"=round(sum(q_11==2)/length(tot_pop)*100,2),
              "prop_pop_rural_women"=round(sum(q_11==2 & resi==1)/length(tot_pop)*100,2),
              "prop_women_15-45y_overwomen"=round(sum((q_10>=15 & q_10<=45) & q_11==2)/sum(q_11==2)*100,2),
              "prop_women_15-45y_overtotal"=round(sum((q_10>=15 & q_10<=45) & q_11==2)/length(tot_pop)*100,2),
              "prop_married_women_15-45y"=round(sum((q_10>=15 & q_10<=45) & q_14==2 & q_11==2, na.rm=TRUE)/sum(q_10>=15 & q_10<=45 & q_11==2)*100,2),
              "prop_married_>=15y"=round(sum(q_10>=15 & q_14==2, na.rm=TRUE)/sum(q_10>=15)*100,2),
              "prop_muslim"=round(sum(religion==1)/length(tot_pop)*100,2),
              "prop_hindu"=round(sum(religion==2)/length(tot_pop)*100,2),
              "mean_age"=round(mean(q_10),2),
              "mean_household_head_age"=round(mean(q_10[q_13==1]),2),
              "prop_household_head_women"=round(sum(q_13==1 & q_11==2)/length(tot_pop)*100,2),
              "prop_primary"=round(sum(q_16 %in% c(1:5))/length(tot_pop)*100,2),
              "prop_secondary_or_higher"=round(sum(q_16 %in% c(10:99))/length(tot_pop)*100,2),
              "rate_literacy_7yplus"=round(sum(q_19==1 & q_10>=7)/sum(q_10>=7)*100,2),
              "ratio_child_women"=round(sum(q_10<5)/sum((q_10>=15 & q_10<=49) & q_11==2)*1000,2))

# Indicators
demo$rate_live_births <- round((births$no_live_births/demo$total_pop)*1000,2) # Crude Birth Rate (CBR)
demo$rate_fertility <- round((births$no_births/demo$`women_15-45y`)*1000,2) # General Fertility Rate (GFR)
demo$rate_death <- round((deaths$no_deaths/demo$total_pop) * 1000,2)
demo$rate_child_death <- round((deaths$`no_deaths_1-4y`/demo$`child_1-4y`) * 1000,2)
demo$rate_under5y_mortality <- round((deaths$`no_deaths_under5y`/births$no_live_births) * 1000,2)
demo$rate_infant_mortality <- round((deaths$`no_deaths_under1y`/births$no_live_births) * 1000,2)
demo$rate_maternal_mortality <- round((deaths$no_maternal_deaths/births$no_live_births) * 100000,2) # Maternal Mortality Ratio (MMR)
demo$rate_out <- round((outmigration$no_out/demo$total_pop)*1000,2)
demo$rate_out_women <- round((outmigration$no_out_women/demo$total_pop)*1000,2)
demo$rate_out_men <- round((outmigration$no_out_men/demo$total_pop)*1000,2)
demo$rate_out_0to14y <- round((outmigration$no_out_0to14y/demo$total_pop)*1000,2)
demo$rate_out_15to44y <- round((outmigration$no_out_15to44y/demo$total_pop)*1000,2)
demo$rate_out_45to64y <- round((outmigration$no_out_45to64y/demo$total_pop)*1000,2)
demo$rate_out_65yplus <- round((outmigration$no_out_65yplus/demo$total_pop)*1000,2)
demo$rate_in <- round((immigration$no_in/demo$total_pop)*1000,2)
demo$rate_in_women <- round((immigration$no_in_women/demo$total_pop)*1000,2)
demo$rate_in_men <- round((immigration$no_in_men/demo$total_pop)*1000,2)
demo$rate_in_0to14y <- round((immigration$no_in_0to14y/demo$total_pop)*1000,2)
demo$rate_in_15to44y <- round((immigration$no_in_15to44y/demo$total_pop)*1000,2)
demo$rate_in_45to64y <- round((immigration$no_in_45to64y/demo$total_pop)*1000,2)
demo$rate_in_65yplus <- round((immigration$no_in_65yplus/demo$total_pop)*1000,2)
demo$rate_marriage <- round((marriage$no_marriage/demo$total_pop)*1000,2)
demo$rate_divorce <- round((divorce$no_divorce/demo$total_pop)*1000,2)
demo$rate_separation <- round((divorce$no_separation/demo$total_pop)*1000,2)
demo$prop_divorce_marriage <- round((divorce$no_divorce/marriage$no_marriage)*100,2)
demo$year <- 2017

# Merge
zila <- merge(births, deaths, all=TRUE)
zila_b <- merge(zila, marriage, all=TRUE)
zila_c <- merge(zila_b, divorce, all=TRUE)
zila_d <- merge(zila_c, outmigration, all=TRUE)
zila_e <- merge(zila_d, immigration, all=TRUE)
zila_f <- merge(zila_e, house, all=TRUE)
zila1 <- merge(zila_f, demo, all=TRUE)

# Rename Zilas
temp <- read_xlsx("./data/bbs/svrs/SVRS_17/MSVSB PSU 2015.xlsx")
temp <- data.frame(temp[4], temp[5])
temp <- unique(temp)
zila1 <- merge(temp, zila1, all.y=TRUE, by.x="zl", by.y="zila")
zila1 <- data.frame(zila1[-c(1)])
names(zila1)[1] <- "district"
zila1 <- subset(zila1, !is.na(zila1$district))

# # Upazila -------------------------------------------------------------------------------------------------
# # Births
# births_uz <- ddply(tafsil3, ~upza, summarise, # Summarize by district
#                 "no_births"=length(q_9),
#                 "no_live_births"=sum(q_9==1),
#                 "no_registered_births"=sum(q_4==1),
#                 "prop_registered_births"=round(sum(q_4==1)/length(q_9),2),
#                 "prop_attendant_delivery"=round(sum(q_7 %in% c(1,2,3))/length(q_9),2))
# 
# # Deaths
# deaths_uz <- ddply(tafsil4, ~upza, summarise, # Summarize by district
#                 "no_deaths"=length(q_2),
#                 "no_deaths_rural"=sum(resi==1),
#                 "prop_deaths_rural"=round(sum(resi==1)/length(q_2),2),
#                 "no_deaths_<5y"=sum(q_3y<5),
#                 "no_deaths_1-4y"=sum(q_3y>=1 & q_3y<=4),
#                 "no_deaths_<1y"=sum(q_3y<1),
#                 "no_maternal_deaths"=sum(q_5 %in% c(37,38,39,40,41,42,43) & q_2==2))
# 
# # Demographic
# demo_uz <- ddply(tafsil2p, ~upza, summarise,
#               "total_pop"=length(tot_pop),
#               "prop_pop_rural"=round(sum(resi==1)/length(tot_pop),2),
#               "prop_pop_women"=round(sum(q_11==2)/length(tot_pop),2),
#               "pop_>15y"=sum(q_10>15),
#               "pop_15-19y"=sum(q_10>=15 & q_10<=19),
#               "pop_>35y"=sum(q_10>35),
#               "women_15-19y"=sum((q_10>=15 & q_10<=19) & q_11==2),
#               "women_15-45y"=sum((q_10>=15 & q_10<=45) & q_11==2),
#               "women_15-49y"=sum((q_10>=15 & q_10<=49) & q_11==2),
#               "men_>15y"=sum(q_10>=15 & q_11==1),
#               "women_15-45_men_>=15y" = sum(((q_10>=15 & q_10<=45) & q_11==2) | (q_10>=15 & q_11==1)),
#               "child_<5y"=sum(q_10<5),
#               "child_1-4y" = sum(q_10>=1 & q_10<=4),
#               "child_0-5y" = sum(q_10<=5),
#               "no_married_>=15y"=sum(q_10>=15 & q_14==2, na.rm=TRUE),
#               "prop_married_>=15y"=round(sum(q_10>=15 & q_14==2, na.rm=TRUE)/sum(q_10>=15),2))
# 
# # Indicators
# demo_uz$rate_live_births <- round((births_uz$no_births/demo_uz$total_pop)*1000,2)
# demo_uz$rate_fertility <- round((births_uz$no_births/demo_uz$`women_15-49y`)*1000,2)
# demo_uz$rate_death <- round((deaths_uz$no_deaths/demo_uz$total_pop) * 1000,2)
# demo_uz$rate_child_death <- round((deaths_uz$`no_deaths_1-4y`/demo_uz$`child_1-4y`) * 1000,2)
# demo_uz$rate_under5y_mortality <- round((deaths_uz$`no_deaths_<5y`/births_uz$no_live_births) * 1000,2)
# demo_uz$rate_infant_mortality <- round((deaths_uz$`no_deaths_<1y`/births_uz$no_live_births) * 1000,2)
# demo_uz$rate_maternal_mortality <- round((deaths_uz$no_maternal_deaths/births_uz$no_live_births) * 1000,2)
# demo$year <- 2017
# 
# upazila <- merge(births_uz, deaths_uz, all=TRUE)
# upazila1 <- merge(upazila, demo_uz, all=TRUE)

#----------------------------------------------------------------------------------------------------------
# Save final dataset/metadata
#----------------------------------------------------------------------------------------------------------

# By Zila
write.csv(zila1,"./output/bbs/data/data_svrs_zila_2017.csv", row.names=FALSE) # Save data

# Save only ratio variables
colnames(zila1)
write.csv(zila1[c(1,5:8,15,17,34:42,55:98)],"./output/bbs/data/data_svrs_zila_2017_clean.csv", row.names=FALSE) # Save data

# # By Upazila
# write.csv(upazila1,"./output/bbs/data/data_svrs_upzila_2017.csv", row.names=FALSE) # Save data

meta_srvs17 <- data.frame("Source"="SRVS", "File"= "SRVS_17","Variable"=colnames(zila1))
write.csv(meta_srvs17,"./output/bbs/data/metadata_bbs_srvs_2017.csv", row.names=FALSE) # Save metadata

#----------------------------------------------------------------------------------------------------------
# Create/save metadata (raw data)
#----------------------------------------------------------------------------------------------------------

# meta_svrs17 <- data.frame()
# for (i in seq_along(svrs17)){
#   meta <- data.frame("Source"="SRVS_17", "File"= names(svrs17[i]),get_variable_labels(svrs17[[i]]))
#   meta_svrs17 <- rbind(meta_svrs17,meta)
# }
# 
# write.csv(meta_svrs17,"./data/bbs/svrs/SVRS_17/metadata_bbs_SRVS_17.csv", row.names=FALSE) # Save metadata

#----------------------------------------------------------------------------------------------------------
# Merge/save metadata from 2017, 2014 and 2013 (raw data)
#----------------------------------------------------------------------------------------------------------

# svrs_17_14 <- merge(meta_svrs17, meta_svrs14, by="variable", all=TRUE)
# svrs_17_14_13 <- merge(svrs_17_14, meta_svrs13, by="variable", all=TRUE)
# 
# write.csv(svrs_17_14_13,"./data/bbs/svrs/metadata_bbs_SRVS_merged_17_14_13.csv", row.names=FALSE) # Save metadata
