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
if(!require(haven)) installed.packages("haven", dependencies=TRUE); library(haven)
if(!require(rdhs)) installed.packages("rdhs"); library(rdhs)
if(!require(plyr)) installed.packages("plyr", dependencies=TRUE); library(plyr)
if(!require(readxl)) installed.packages("readxl", dependencies=TRUE); library(readxl)

#----------------------------------------------------------------------------------------------------------
# Load datasets
#----------------------------------------------------------------------------------------------------------

svrs14files <- list.files("./data/bbs/svrs/SVRS_14", recursive=TRUE, pattern="*.sav", full.names=TRUE) # List files
svrs14 <- sapply(svrs14files, read_sav)
names(svrs14) <- c("tafsil-10","tafsil-11","tafsil-2h","tafsil-2p","tafsil-3","tafsil-4","tafsil-5","tafsil-6","tafsil-7","tafsil-8","tafsil-9") # Rename lists

# Section 2: Household roster and demographic characteristics ---------------------------------------------
tafsil2h <- svrs14$'tafsil-2h' # Load as data frame
tafsil2h_var <- get_variable_labels(tafsil2h) # Get the data dictionary

tafsil2p <- svrs14$'tafsil-2p'
tafsil2p_var <- get_variable_labels(tafsil2p)
data.frame(lapply(tafsil2p, function(y) sum(length(which(is.na(y)))))) # NAs by column

# Section 3: Birth ----------------------------------------------------------------------------------------
tafsil3 <- svrs14$'tafsil-3'
tafsil3_var <- get_variable_labels(tafsil3)

# Section 4: Deaths ---------------------------------------------------------------------------------------
tafsil4 <- svrs14$'tafsil-4'
tafsil4_var <- get_variable_labels(tafsil4)

# Section 5: Marriage -------------------------------------------------------------------------------------
tafsil5 <- svrs14$'tafsil-5'
tafsil5_var <- get_variable_labels(tafsil5)

# Section 6: Divorce --------------------------------------------------------------------------------------
tafsil6 <- svrs14$'tafsil-6'
tafsil6_var <- get_variable_labels(tafsil6)
# tafsil6 <- subset(tafsil6, !is.na(zila))
# tafsil6 <- tafsil6[!(tafsil6$zila==95),]

# Section 7: Out-migration --------------------------------------------------------------------------------
tafsil7 <- svrs14$'tafsil-7'
tafsil7_var <- get_variable_labels(tafsil7)

# Section 8: In-migration ---------------------------------------------------------------------------------
tafsil8 <- svrs14$'tafsil-8'
tafsil8_var <- get_variable_labels(tafsil8)

# Section 9: Use of Contraceptives ------------------------------------------------------------------------
tafsil9 <- svrs14$'tafsil-9'
tafsil9_var <- get_variable_labels(tafsil9)

# Section 10: Disability ----------------------------------------------------------------------------------
tafsil10 <- svrs14$'tafsil-10'
tafsil10_var <- get_variable_labels(tafsil10)

# Section 11: HIV/AIDS ------------------------------------------------------------------------------------
tafsil11 <- svrs14$'tafsil-11'
tafsil11_var <- get_variable_labels(tafsil11)

#----------------------------------------------------------------------------------------------------------
# List of variables of interest
#----------------------------------------------------------------------------------------------------------

# Section 2: Household roster and demographic characteristics ---------------------------------------------
# "tafsil-2P" 
# zila # District
# upz # Upazila
# area # area | Rural==1
# Q11 # Age
# Q12 # Sex | Women==2
# Q15 # M.Stat. | Married==15

# Section 3: Birth ----------------------------------------------------------------------------------------
# zila # District
# upz # Upazila
# area # area | Rural==1
# C4 # Birth registration | Yes==1
# C7 # Birth Attendent
     # 1 # Relative
     # 2 # TBA
     # 3 # Nurse
     # 4 # Doctor
# C10 # Live Birth | Yes==1

# Section 4: Deaths ---------------------------------------------------------------------------------------
# zila # District
# upz # Upazila
# area # area | Rural==1
# D2 # Sex | Women==2
# D5 # Causes of Death
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
births <- ddply(tafsil3, ~zila, summarise, # Summarize by district
                "no_births"=length(C10),
                "no_live_births"=sum(C10==1, na.rm=TRUE),
                "no_registered_births"=sum(C4==1, na.rm=TRUE),
                "prop_live_births"=round(sum(C9==1)/length(C9)*100,2),
                "prop_registered_births"=round(sum(C4==1, na.rm=TRUE)/length(C10),2),
                "prop_attendant_delivery"=round(sum(C7 %in% c(3,4))/length(C10),2),
                "mean_mothers_age"=round(mean(C12),0))

# Deaths
deaths <- ddply(tafsil4, ~zila, summarise, # Summarize by district
                "no_deaths"=length(HH1),
                "no_deaths_rural"=sum(area==1),
                "no_deaths_under5y"=sum(D3Y<5),
                "no_deaths_1-4y"=sum(D3Y>=1 & D3Y<=4),
                "no_deaths_under1y"=sum(D3Y<1),
                "no_maternal_deaths"=sum(D5 %in% c(37,38,39,40,41,42,43) & D2==2),
                "prop_deaths_rural"=round(sum(area==1)/length(HH1),2))

# Marriage
marriage <- ddply(tafsil5, ~zila, summarise, # Summarize by district
                  "no_marriage"=length(HH1),
                  "mean_age_marriage"=round(mean(E4),0))

# Divorce
divorce <- ddply(tafsil6, ~zila, summarise, # Summarize by district
                 "no_divorce"=sum(F1_A==1, na.rm=TRUE),
                 "no_separation"=sum(F1_A==2, na.rm=TRUE))

# Out-migration
outmigration <- ddply(tafsil7, ~zila, summarise,
                      "no_out"=length(HH1),
                      "no_out_women"=sum(G2==2),
                      "no_out_men"=sum(G2==1),
                      "no_out_0to14y" = sum(G3<15),
                      "no_out_15to44y" = sum(G3>=15 & G3<=44),
                      "no_out_45to64y" = sum(G3>=45 & G3<=64),
                      "no_out_65yplus" = sum(G3>=65))
# In-migration
immigration <- ddply(tafsil8, ~zila, summarise,
                     "no_in"=length(HH1),
                     "no_in_women"=sum(H2==2),
                     "no_in_men"=sum(H2==1),
                     "no_in_0to14y" = sum(H3<15),
                     "no_in_15to44y" = sum(H3>=15 & H3<=44),
                     "no_in_45to64y" = sum(H3>=45 & H3<=64),
                     "no_in_65yplus" = sum(H3>=65))

# Household
house <- ddply(tafsil2h, ~ZILA, summarise,
               "prop_drinking_tubewell_water"=round(sum(Q3A==2)/length(HH1)*100,2),
               "prop_source_light_eletricity"=round(sum(Q5==2)/length(HH1)*100,2),
               "prop_sanitary_with_water"=round(sum(Q7==1)/length(HH1)*100,2),
               "prop_sanitary_open"=round(sum(Q7==4)/length(HH1)*100,2),
               "prop_house_building"=round(sum(Q2A2!=0)/length(HH1)*100,2),
               "prop_house_semipucca"=round(sum(Q2B2!=0)/length(HH1)*100,2),
               "prop_house_wooden"=round(sum(Q2C2!=0)/length(HH1)*100,2),
               "prop_house_mud"=round(sum(Q2D2!=0)/length(HH1)*100,2),
               "prop_house_bamboo"=round(sum(Q2E2!=0)/length(HH1)*100,2))

# Household members
demo <- ddply(tafsil2p, ~zila, summarise,
              "total_pop"=length(HH1),
              "pop_>15y"=sum(Q11>15),
              "pop_15-19y"=sum(Q11>=15 & Q11<=19),
              "pop_>35y"=sum(Q11>35),
              "women_15-19y"=sum((Q11>=15 & Q11<=19) & Q12==2),
              "women_15-45y"=sum((Q11>=15 & Q11<=45) & Q12==2),
              "men_>15y"=sum(Q11>=15 & Q12==1),
              "women_15-45_men_>=15y" = sum(((Q11>=15 & Q11<=45) & Q12==2) | (Q11>=15 & Q12==1)),
              "child_under5y"=sum(Q11<5),
              "child_1-4y" = sum(Q11>=1 & Q11<=4),
              "child_0-5y" = sum(Q11<=5),
              "no_married_>=15y"=sum(Q11>=15 & Q15==2, na.rm=TRUE),
              "sex_ratio"=round(sum(Q12==1)/sum(Q12==2)*100,2),
              "dependency_ratio"=round((sum(Q11<=14) + sum(Q11>=65))/sum(Q11>=15 & Q11<=64)*100,2),
              "prop_pop_rural"=round(sum(area==1)/length(HH1)*100,2),
              "prop_pop_women"=round(sum(Q12==2)/length(HH1)*100,2),
              "prop_pop_rural_women"=round(sum(Q12==2 & area==1)/length(HH1)*100,2),
              "prop_women_15-45y_overwomen"=round(sum((Q11>=15 & Q11<=45) & Q12==2)/sum(Q12==2)*100,2),
              "prop_women_15-45y_overtotal"=round(sum((Q11>=15 & Q11<=45) & Q12==2)/length(HH1)*100,2),
              "prop_married_women_15-45y"=round(sum((Q11>=15 & Q11<=45) & Q15==2 & Q12==2, na.rm=TRUE)/sum(Q11>=15 & Q11<=45 & Q12==2)*100,2),
              "prop_married_>=15y"=round(sum(Q11>=15 & Q15==2, na.rm=TRUE)/sum(Q11>=15)*100,2),
              "prop_muslim"=round(sum(religion==1)/length(HH1)*100,2),
              "prop_hindu"=round(sum(religion==2)/length(HH1)*100,2),
              "mean_age"=round(mean(Q11),2),
              "mean_household_head_age"=round(mean(Q11[Q14==1]),2),
              "prop_household_head_women"=round(sum(Q14==1 & Q12==2)/length(HH1)*100,2),
              "prop_primary"=round(sum(Q16 %in% c(1:5))/length(HH1)*100,2),
              "prop_secondary_or_higher"=round(sum(Q16 %in% c(10:99))/length(HH1)*100,2),
              "rate_literacy_7yplus"=round(sum(Q19==1 & Q11>=7)/sum(Q11>=7)*100,2),
              "ratio_child_women"=round(sum(Q11<5)/sum((Q11>=15 & Q11<=49) & Q12==2)*1000,2))

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
demo$rate_divorce <- round((divorce$no_divorce/demo$total_pop)*1000,2)
demo$rate_separation <- round((divorce$no_separation/demo$total_pop)*1000,2)
demo$prop_divorce_marriage <- round((divorce$no_divorce/marriage$no_marriage)*100,2)
demo$year <- 2014

# Merge
zila <- merge(births, deaths, all=TRUE)
zila_b <- merge(zila, marriage, all=TRUE)
zila_c <- merge(zila_b, divorce, all=TRUE)
zila_d <- merge(zila_c, outmigration, all=TRUE)
zila_e <- merge(zila_d, immigration, all=TRUE)
zila_f <- merge(zila_e, house, all=TRUE, by.x="zila", by.y="ZILA")
zila1 <- merge(zila_f, demo, all=TRUE)

# Rename Zilas
temp <- read_xlsx("./data/bbs/svrs/SVRS_14/PSU1500.xlsx")
temp <- data.frame(temp[5], temp[6])
temp <- unique(temp)
zila1 <- merge(temp, zila1, all.y=TRUE, by.x="zl", by.y="zila")
zila1 <- data.frame(zila1[-c(1)])
names(zila1)[1] <- "district"

#----------------------------------------------------------------------------------------------------------
# Save final dataset/metadata
#----------------------------------------------------------------------------------------------------------

# By Zila
write.csv(zila1,"./output/bbs/data/data_svrs_zila_2014.csv", row.names=FALSE) # Save data

# Save only ratio variables
colnames(zila1)
write.csv(zila1[c(1,5:8,15,17,34:42,55:98)],"./output/bbs/data/data_svrs_zila_2017_clean.csv", row.names=FALSE) # Save data

meta_srvs14 <- data.frame("Source"="SRVS", "File"= "SRVS_14","Variable"=colnames(zila1))
write.csv(meta_srvs14,"./output/bbs/data/metadata_bbs_srvs_2014.csv", row.names=FALSE) # Save metadata

#----------------------------------------------------------------------------------------------------------
# Create/save metadata (raw data)
#----------------------------------------------------------------------------------------------------------

meta_svrs14 <- data.frame()
for (i in seq_along(svrs14)){
  meta <- data.frame("Source"="SRVS_14", "File"= names(svrs14[i]),get_variable_labels(svrs14[[i]]))
  meta_svrs14 <- rbind(meta_svrs14,meta)
}

write.csv(meta_svrs14,"./data/bbs/svrs/SVRS_14/metadata_bbs_SRVS_14.csv", row.names=FALSE) # Save metadata
