#----------------------------------------------------------------------------------------------------------
# dp_bbs_srvs_.R
# Description: Data preprocessing of SRVS datasets
# Organization: Health, Nutrition, and Population (HNP) | The World Bank
# Author: Hellen Matarazzo
# Date: Updated on 03-27-2019
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

svrs13files <- list.files("./data/bbs/svrs/SVRS_13/", recursive=TRUE, pattern="*.sav", full.names=TRUE) # List files
svrs13 <- sapply(svrs13files[c(1:2,5:6,11)], read_sav)
names(svrs13) <- c("tafsil-2h", "tafsil-2p", "tafsil-3", "tafsil-4", "tafsil-9") # Rename lists

# Section 2: Household roster and demographic characteristics ---------------------------------------------
tafsil2h <- svrs13$'tafsil-2h' # Load as data frame
tafsil2h_var <- get_variable_labels(tafsil2h) # Get the data dictionary

tafsil2p <- svrs13$'tafsil-2p'
tafsil2p_var <- get_variable_labels(tafsil2p)
data.frame(lapply(tafsil2p, function(y) sum(length(which(is.na(y)))))) # NAs by column

# Section 3: Birth ----------------------------------------------------------------------------------------
tafsil3 <- svrs13$'tafsil-3'
tafsil3_var <- get_variable_labels(tafsil3)
data.frame(lapply(tafsil3, function(y) sum(length(which(is.na(y)))))) # NAs by column

# Section 4: Deaths ---------------------------------------------------------------------------------------
tafsil4 <- svrs13$'tafsil-4'
tafsil4_var <- get_variable_labels(tafsil4)
data.frame(lapply(tafsil4, function(y) sum(length(which(is.na(y)))))) # NAs by column

# Section 9: Use of Contraceptives ------------------------------------------------------------------------
tafsil9 <- svrs13$'tafsil-9'
tafsil9_var <- get_variable_labels(tafsil9)

#----------------------------------------------------------------------------------------------------------
# List of variables of interest
#----------------------------------------------------------------------------------------------------------

# Section 2: Household roster and demographic characteristics ---------------------------------------------
# "tafsil-2P" 
# zila # District
# upz # Upazila
# RMO # RMO | Rural==1
# Q11 # Age
# Q12 # Sex | Women==2
# Q15 # M.Stat. | Married==15

# Section 3: Birth ----------------------------------------------------------------------------------------
# zila # District
# upz # Upazila
# RMO # RMO | Rural==1
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
# RMO # RMO | Rural==1
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
                "prop_registered_births"=round(sum(C4==1, na.rm=TRUE)/length(C10),2),
                "prop_attendant_delivery"=round(sum(C7 %in% c(3,4))/length(C10),2))

# Deaths
deaths <- ddply(tafsil4, ~zila, summarise, # Summarize by district
                "no_deaths"=length(hhno),
                "no_deaths_rural"=sum(RMO==1),
                "prop_deaths_rural"=round(sum(RMO==1)/length(hhno),2),
                "no_deaths_<5y"=sum(D3Y<5),
                "no_deaths_1-4y"=sum(D3Y>=1 & D3Y<=4),
                "no_deaths_<1y"=sum(D3Y<1),
                "no_maternal_deaths"=sum(D5 %in% c(37,38,39,40,41,42,43) & D2==2))

# Demographic
demo <- ddply(tafsil2p, ~zila, summarise,
              "total_pop"=length(HHNO),
              "prop_pop_rural"=round(sum(RMO==1)/length(HHNO),2),
              "prop_pop_women"=round(sum(Q12==2)/length(HHNO),2),
              "pop_>15y"=sum(Q11>15),
              "pop_15-19y"=sum(Q11>=15 & Q11<=19),
              "pop_>35y"=sum(Q11>35),
              "women_15-19y"=sum((Q11>=15 & Q11<=19) & Q12==2),
              "women_15-45y"=sum((Q11>=15 & Q11<=45) & Q12==2),
              "women_15-49y"=sum((Q11>=15 & Q11<=49) & Q12==2),
              "men_>15y"=sum(Q11>=15 & Q12==1),
              "women_15-45_men_>=15y" = sum(((Q11>=15 & Q11<=45) & Q12==2) | (Q11>=15 & Q12==1)),
              "child_<5y"=sum(Q11<5),
              "child_1-4y" = sum(Q11>=1 & Q11<=4),
              "child_0-5y" = sum(Q11<=5),
              "no_married_>=15y"=sum(Q11>=15 & Q15==2, na.rm=TRUE),
              "prop_married_>=15y"=round(sum(Q11>=15 & Q15==2, na.rm=TRUE)/sum(Q11>=15),2))

# Indicators
demo$rate_live_births <- round((births$no_births/demo$total_pop)*1000,2)
demo$rate_fertility <- round((births$no_births/demo$`women_15-49y`)*1000,2)
demo$rate_death <- round((deaths$no_deaths/demo$total_pop) * 1000,2)
demo$rate_child_death <- round((deaths$`no_deaths_1-4y`/demo$`child_1-4y`) * 1000,2)
demo$rate_under5y_mortality <- round((deaths$`no_deaths_<5y`/births$no_live_births) * 1000,2)
demo$rate_infant_mortality <- round((deaths$`no_deaths_<1y`/births$no_live_births) * 1000,2)
demo$rate_maternal_mortality <- round((deaths$no_maternal_deaths/births$no_live_births) * 1000,2)
demo$year <- 2013

# Merge
zila <- merge(births, deaths, all=TRUE)
zila1 <- merge(zila, demo, all=TRUE)

# Rename Zilas
temp <- read_xlsx("./data/geo_files/bbs_geos/geo.xlsx", skip=1)
temp <- data.frame(temp[3], temp[4])
temp <- unique(temp)
zila1 <- merge(temp, zila1, all.y=TRUE, by.x="District.Code", by.y="zila")
zila1 <- data.frame(zila1[-c(1)])
names(zila1)[1] <- "district"

#----------------------------------------------------------------------------------------------------------
# Save final dataset/metadata
#----------------------------------------------------------------------------------------------------------

# By Zila
write.csv(zila1,"./output/bbs/data/data_svrs_zila_2013.csv", row.names=FALSE) # Save data

meta_srvs13 <- data.frame("Source"="SRVS", "File"= "SRVS_13","Variable"=colnames(zila1))
write.csv(meta_srvs13,"./output/bbs/data/metadata_bbs_srvs_2013.csv", row.names=FALSE) # Save metadata

#----------------------------------------------------------------------------------------------------------
# Create/save metadata (raw data)
#----------------------------------------------------------------------------------------------------------

meta_svrs13 <- data.frame()
for (i in seq_along(svrs13)){
  meta <- data.frame("Source"="SRVS_13", "File"= names(svrs13[i]),get_variable_labels(svrs13[[i]]))
  meta_svrs13 <- rbind(meta_svrs13,meta)
}

write.csv(meta_svrs13,"./data/bbs/svrs/SVRS_13/metadata_bbs_SRVS_13.csv", row.names=FALSE) # Save metadata
