#----------------------------------------------------------------------------------------------------------
# dp_bbs_srvs_.R
# Description: Data preprocessing of SRVS datasets
# Organization: Health, Nutrition, and Population (HNP) | The World Bank
# Author: Hellen Matarazzo
# Date: Updated on 05-14-2019
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
if(!require(foreign)) installed.packages("foreign", dependencies=TRUE); library(foreign)
if(!require(rdhs)) installed.packages("rdhs"); library(rdhs)
if(!require(plyr)) installed.packages("plyr", dependencies=TRUE); library(plyr)
if(!require(readxl)) installed.packages("readxl", dependencies=TRUE); library(readxl)

#----------------------------------------------------------------------------------------------------------
# Load datasets
#----------------------------------------------------------------------------------------------------------

svrs12files <- list.files("./data/bbs/svrs/SVRS_12/", recursive=TRUE, pattern="*.dbf", full.names=TRUE) # List files
svrs12 <- sapply(svrs12files, read.dbf)
names(svrs12) <- c("tafsil-10","tafsil-2h","tafsil-2p","tafsil-3","tafsil-4","tafsil-5","tafsil-6","tafsil-7","tafsil-8","tafsil-9") # Rename lists

# Section 2: Household roster and demographic characteristics ---------------------------------------------
tafsil2h <- svrs12$'tafsil-2h' # Load as data frame
colnames(tafsil2h) # Get the data dictionary

tafsil2p <- svrs12$'tafsil-2p'
colnames(tafsil2p)
data.frame(lapply(tafsil2p, function(y) sum(length(which(is.na(y)))))) # NAs by column

# Section 3: Birth ----------------------------------------------------------------------------------------
tafsil3 <- svrs12$'tafsil-3'
colnames(tafsil3)

# Section 4: Deaths ---------------------------------------------------------------------------------------
tafsil4 <- svrs12$'tafsil-4'
colnames(tafsil4)

# Section 5: Marriage -------------------------------------------------------------------------------------
tafsil5 <- svrs12$'tafsil-5'

# Section 6: Divorce --------------------------------------------------------------------------------------
tafsil6 <- svrs12$'tafsil-6'
levels(tafsil6$ZILA) <- c(levels(tafsil6$ZILA),"30")

# Section 7: Out-migration --------------------------------------------------------------------------------
tafsil7 <- svrs12$'tafsil-7'

# Section 8: In-migration ---------------------------------------------------------------------------------
tafsil8 <- svrs12$'tafsil-8'

# Section 9: Use of Contraceptives ------------------------------------------------------------------------
tafsil9 <- svrs12$'tafsil-9'

# Section 10: Disability ----------------------------------------------------------------------------------
tafsil10 <- svrs12$'tafsil-10'

#----------------------------------------------------------------------------------------------------------
# List of variables of interest
#----------------------------------------------------------------------------------------------------------

# Section 2: Household roster and demographic characteristics ---------------------------------------------
# "tafsil-2P" 
# ZILA # District
# UPZA # Upazila
# RMO # RMO | Rural==1
# Q_11 # Age
# Q_12 # Sex | Women==2
# Q_15 # M.Stat. | Married==15

# Section 3: Birth ----------------------------------------------------------------------------------------
# ZILA # District
# UPZA # Upazila
# RMO # RMO | Rural==1
# Q_4 # Birth registration | Yes==1
# Q_8 # Live Birth | Yes==1

# Section 4: Deaths ---------------------------------------------------------------------------------------
# ZILA # District
# UPZA # Upazila
# RMO # RMO | Rural==1
# Q_2 # Sex | Women==2
# Q_3Y # Age
# Q_5 # Causes of Death
    # 37 # Pregnancy related problem
    # 38 # Complex delivary
    # 39 # Bleeding after delivery (PPH)
    # 40 # Complex Abortion
    # 41 # Bleeding during pregnancy (APH)
    # 42 # Sutika
    # 43 # Titanus

#----------------------------------------------------------------------------------------------------------
# Data transformation
#----------------------------------------------------------------------------------------------------------

# Zila ----------------------------------------------------------------------------------------------------
# Births
births <- ddply(tafsil3, ~ZILA, summarise, # Summarize by district
                "no_births"=length(Q_9),
                "no_live_births"=sum(Q_9==1, na.rm=TRUE),
                "no_registered_births"=sum(Q_4==1, na.rm=TRUE),
                "prop_live_births"=round(sum(Q_9==1)/length(Q_9)*100,2),
                "prop_registered_births"=(round(sum(Q_4==1, na.rm=TRUE)/length(Q_9),2))*100,
                "prop_attendant_delivery"=round(sum(Q_7 %in% c(3,4))/length(Q_7),2),
                "mean_mothers_age"=round(mean(Q_11),0))

# Deaths
deaths <- ddply(tafsil4, ~ZILA, summarise, # Summarize by district
                "no_deaths"=length(HH_NO),
                "no_deaths_rural"=sum(RMO==1),
                "no_deaths_under5y"=sum(Q_3Y<5),
                "no_deaths_1-4y"=sum(Q_3Y>=1 & Q_3Y<=4),
                "no_deaths_under1y"=sum(Q_3Y<1),
                "no_maternal_deaths"=sum(Q_5 %in% c(37,38,39,40,41,42,43) & (Q_3Y>=15 & Q_3Y<=49) & Q_2==2),
                "prop_deaths_rural"=(round(sum(RMO==1)/length(HH_NO),2))*100)

# Marriage
marriage <- ddply(tafsil5, ~ZILA, summarise, # Summarize by district
                  "no_marriage"=length(HH_NO),
                  "mean_age_marriage"=round(mean(Q_4),0))

# Divorce
divorce <- ddply(tafsil6, ~ZILA, summarise, # Summarize by district
                 "no_divorce"=sum(Q_1==1, na.rm=TRUE),
                 "no_separation"=sum(Q_1==2, na.rm=TRUE))

# Out-migration
outmigration <- ddply(tafsil7, ~ZILA, summarise,
                      "no_out"=length(HH_NO),
                      "no_out_women"=sum(Q_2==2),
                      "no_out_men"=sum(Q_2==1),
                      "no_out_0to14y" = sum(Q_3<15),
                      "no_out_15to44y" = sum(Q_3>=15 & Q_3<=44),
                      "no_out_45to64y" = sum(Q_3>=45 & Q_3<=64),
                      "no_out_65yplus" = sum(Q_3>=65))
# In-migration
immigration <- ddply(tafsil8, ~ZILA, summarise,
                     "no_in"=length(HH_NO),
                     "no_in_women"=sum(Q_2==2),
                     "no_in_men"=sum(Q_2==1),
                     "no_in_0to14y" = sum(Q_3<15),
                     "no_in_15to44y" = sum(Q_3>=15 & Q_3<=44),
                     "no_in_45to64y" = sum(Q_3>=45 & Q_3<=64),
                     "no_in_65yplus" = sum(Q_3>=65))

# Household
house <- ddply(tafsil2h, ~ZILA, summarise,
               "prop_drinking_tubewell_water"=round(sum(Q3_1==2)/length(HH_NO)*100,2),
               "prop_source_light_eletricity"=round(sum(Q_5==2)/length(HH_NO)*100,2),
               "prop_sanitary_with_water"=round(sum(Q_7==1)/length(HH_NO)*100,2),
               "prop_sanitary_open"=round(sum(Q_7==4)/length(HH_NO)*100,2),
               "prop_house_building"=round(sum(Q2_1A!=0)/length(HH_NO)*100,2),
               "prop_house_semipucca"=round(sum(Q2_2A!=0)/length(HH_NO)*100,2),
               "prop_house_wooden"=round(sum(Q2_3A!=0)/length(HH_NO)*100,2),
               "prop_house_mud"=round(sum(Q2_4A!=0)/length(HH_NO)*100,2),
               "prop_house_bamboo"=round(sum(Q2_5A!=0)/length(HH_NO)*100,2))

# Household members
demo <- ddply(tafsil2p, ~ZILA, summarise,
              "total_pop"=length(HH_NO),
              "pop_>15y"=sum(Q_11>15),
              "pop_15-19y"=sum(Q_11>=15 & Q_11<=19),
              "pop_>35y"=sum(Q_11>35),
              "women_15-19y"=sum((Q_11>=15 & Q_11<=19) & Q_12==2),
              "women_15-45y"=sum((Q_11>=15 & Q_11<=45) & Q_12==2),
              "men_>15y"=sum(Q_11>=15 & Q_12==1),
              "women_15-45_men_>=15y" = sum(((Q_11>=15 & Q_11<=45) & Q_12==2) | (Q_11>=15 & Q_12==1)),
              "child_under5y"=sum(Q_11<5),
              "child_1-4y" = sum(Q_11>=1 & Q_11<=4),
              "child_0-5y" = sum(Q_11<=5),
              "no_married_>=15y"=sum(Q_11>=15 & Q_15==2, na.rm=TRUE),
              "sex_ratio"=round(sum(Q_12==1)/sum(Q_12==2)*100,2),
              "dependency_ratio"=round((sum(Q_11<=14) + sum(Q_11>=65))/sum(Q_11>=15 & Q_11<=64)*100,2),
              "prop_pop_rural"=round(sum(RMO==1)/length(HH_NO)*100,2),
              "prop_pop_women"=round(sum(Q_12==2)/length(HH_NO)*100,2),
              "prop_pop_rural_women"=round(sum(Q_12==2 & RMO==1)/length(HH_NO)*100,2),
              "prop_women_15-45y_overwomen"=round(sum((Q_11>=15 & Q_11<=45) & Q_12==2)/sum(Q_12==2)*100,2),
              "prop_women_15-45y_overtotal"=round(sum((Q_11>=15 & Q_11<=45) & Q_12==2)/length(HH_NO)*100,2),
              "prop_married_women_15-45y"=round(sum((Q_11>=15 & Q_11<=45) & Q_15==2 & Q_12==2, na.rm=TRUE)/sum(Q_11>=15 & Q_11<=45 & Q_12==2)*100,2),
              "prop_married_>=15y"=round(sum(Q_11>=15 & Q_15==2, na.rm=TRUE)/sum(Q_11>=15)*100,2),
              "prop_muslim"=round(sum(Q_13==1)/length(HH_NO)*100,2),
              "prop_hindu"=round(sum(Q_13==2)/length(HH_NO)*100,2),
              "mean_age"=round(mean(Q_11),2),
              "mean_household_head_age"=round(mean(Q_11[Q_14==1]),2),
              "prop_household_head_women"=round(sum(Q_14==1 & Q_12==2)/length(HH_NO)*100,2),
              "prop_primary"=round(sum(Q_16 %in% c(1:5))/length(HH_NO)*100,2),
              "prop_secondary_or_higher"=round(sum(Q_16 %in% c(10:99))/length(HH_NO)*100,2),
              "rate_literacy_7yplus"=round(sum(Q_19==1 & Q_11>=7)/sum(Q_11>=7)*100,2),
              "ratio_child_women"=round(sum(Q_11<5)/sum((Q_11>=15 & Q_11<=49) & Q_12==2)*1000,2))

              
# Indicators
demo$rate_live_births <- round((births$no_live_births/demo$total_pop)*1000,2)
demo$rate_fertility <- round((births$no_births/demo$`women_15-45y`)*1000,2)
demo$rate_death <- round((deaths$no_deaths/demo$total_pop) * 1000,2)
demo$rate_child_death <- round((deaths$`no_deaths_1-4y`/demo$`child_1-4y`) * 1000,2)
demo$rate_under5y_mortality <- round((deaths$`no_deaths_under5y`/births$no_live_births) * 1000,2)
demo$rate_infant_mortality <- round((deaths$`no_deaths_under1y`/births$no_live_births) * 1000,2)
demo$rate_maternal_mortality <- round((deaths$no_maternal_deaths/births$no_live_births) * 100000,2)
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

divorce <- rbind(divorce, c(30, 0, 0))
demo$rate_divorce <- round((divorce$no_divorce/demo$total_pop)*1000,2)
demo$rate_separation <- round((divorce$no_separation/demo$total_pop)*1000,2)
demo$prop_divorce_marriage <- round((divorce$no_divorce/marriage$no_marriage)*100,2)
demo$year <- 2012

# Merge
zila <- merge(births, deaths, all=TRUE)
zila_b <- merge(zila, marriage, all=TRUE)
zila_c <- merge(zila_b, divorce, all=TRUE)
zila_d <- merge(zila_c, outmigration, all=TRUE)
zila_e <- merge(zila_d, immigration, all=TRUE)
zila_f <- merge(zila_e, house, all=TRUE)
zila1 <- merge(zila_f, demo, all=TRUE)

# Rename Zilas
temp <- read_xlsx("./data/geo_files/bbs_geos/geo.xlsx", skip=1)
temp <- data.frame(temp[3], temp[4])
temp <- unique(temp)
zila1$ZILA <- as.numeric(as.character(zila1$ZILA))
zila1 <- merge(temp, zila1, all.y=TRUE, by.x="District.Code", by.y="ZILA")
zila1 <- data.frame(zila1[-c(1)])
names(zila1)[1] <- "district"
zila1$district <- gsub(" Zila", "", zila1$district)

#----------------------------------------------------------------------------------------------------------
# Save final dataset/metadata
#----------------------------------------------------------------------------------------------------------

# By Zila
write.csv(zila1,"./output/bbs/data/data_svrs_zila_2012.csv", row.names=FALSE) # Save data

# Save only ratio variables
colnames(zila1)
write.csv(zila1[c(1,5:8,15,17,34:42,55:98)],"./output/bbs/data/data_svrs_zila_2012_clean.csv", row.names=FALSE) # Save data

meta_srvs12 <- data.frame("Source"="SRVS", "File"= "SRVS_12","Variable"=colnames(zila1))
write.csv(meta_srvs12,"./output/bbs/data/metadata_bbs_srvs_2012.csv", row.names=FALSE) # Save metadata
