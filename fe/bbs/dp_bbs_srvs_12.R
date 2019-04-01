#----------------------------------------------------------------------------------------------------------
# dp_bbs_srvs_.R
# Description: Data preprocessing of SRVS datasets
# Organization: Health, Nutrition, and Population (HNP) | The World Bank
# Author: Hellen Matarazzo
# Date: Updated on 03-29-2019
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
svrs12 <- sapply(svrs12files[c(2:5,10)], read.dbf)
names(svrs12) <- c("tafsil-2h", "tafsil-2p", "tafsil-3", "tafsil-4", "tafsil-9") # Rename lists

# Section 2: Household roster and demographic characteristics ---------------------------------------------
tafsil2h <- svrs12$'tafsil-2h' # Load as data frame

tafsil2p <- svrs12$'tafsil-2p'
colnames(tafsil2p)
data.frame(lapply(tafsil2p, function(y) sum(length(which(is.na(y)))))) # NAs by column

# Section 3: Birth ----------------------------------------------------------------------------------------
tafsil3 <- svrs12$'tafsil-3'
colnames(tafsil3)
data.frame(lapply(tafsil3, function(y) sum(length(which(is.na(y)))))) # NAs by column

# Section 4: Deaths ---------------------------------------------------------------------------------------
tafsil4 <- svrs12$'tafsil-4'
colnames(tafsil4)
data.frame(lapply(tafsil4, function(y) sum(length(which(is.na(y)))))) # NAs by column

# Section 9: Use of Contraceptives ------------------------------------------------------------------------
tafsil9 <- svrs12$'tafsil-9'

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
                "no_births"=length(Q_8),
                "no_live_births"=sum(Q_8==1, na.rm=TRUE),
                "no_registered_births"=sum(Q_4==1, na.rm=TRUE),
                "prop_registered_births"=round(sum(Q_4==1, na.rm=TRUE)/length(Q_8),2))

# Deaths
deaths <- ddply(tafsil4, ~ZILA, summarise, # Summarize by district
                "no_deaths"=length(HH_NO),
                "no_deaths_rural"=sum(RMO==1),
                "prop_deaths_rural"=round(sum(RMO==1)/length(HH_NO),2),
                "no_deaths_<5y"=sum(Q_3Y<5),
                "no_deaths_1-4y"=sum(Q_3Y>=1 & Q_3Y<=4),
                "no_deaths_<1y"=sum(Q_3Y<1),
                "no_maternal_deaths"=sum(Q_5 %in% c(37,38,39,40,41,42,43) & Q_2==2))

# Demographic
demo <- ddply(tafsil2p, ~ZILA, summarise,
              "total_pop"=length(HH_NO),
              "prop_pop_rural"=round(sum(RMO==1)/length(HH_NO),2),
              "prop_pop_women"=round(sum(Q_12==2)/length(HH_NO),2),
              "pop_>15y"=sum(Q_11>15),
              "pop_15-19y"=sum(Q_11>=15 & Q_11<=19),
              "pop_>35y"=sum(Q_11>35),
              "women_15-19y"=sum((Q_11>=15 & Q_11<=19) & Q_12==2),
              "women_15-45y"=sum((Q_11>=15 & Q_11<=45) & Q_12==2),
              "women_15-49y"=sum((Q_11>=15 & Q_11<=49) & Q_12==2),
              "men_>15y"=sum(Q_11>=15 & Q_12==1),
              "women_15-45_men_>=15y" = sum(((Q_11>=15 & Q_11<=45) & Q_12==2) | (Q_11>=15 & Q_12==1)),
              "child_<5y"=sum(Q_11<5),
              "child_1-4y" = sum(Q_11>=1 & Q_11<=4),
              "child_0-5y" = sum(Q_11<=5),
              "no_married_>=15y"=sum(Q_11>=15 & Q_15==2, na.rm=TRUE),
              "prop_married_>=15y"=round(sum(Q_11>=15 & Q_15==2, na.rm=TRUE)/sum(Q_11>=15),2))

# Indicators
demo$rate_live_births <- round((births$no_births/demo$total_pop)*1000,2)
demo$rate_fertility <- round((births$no_births/demo$`women_15-49y`)*1000,2)
demo$rate_death <- round((deaths$no_deaths/demo$total_pop) * 1000,2)
demo$rate_child_death <- round((deaths$`no_deaths_1-4y`/demo$`child_1-4y`) * 1000,2)
demo$rate_under5y_mortality <- round((deaths$`no_deaths_<5y`/births$no_live_births) * 1000,2)
demo$rate_infant_mortality <- round((deaths$`no_deaths_<1y`/births$no_live_births) * 1000,2)
demo$rate_maternal_mortality <- round((deaths$no_maternal_deaths/births$no_live_births) * 1000,2)
demo$year <- 2012

# Merge
zila <- merge(births, deaths, all=TRUE)
zila1 <- merge(zila, demo, all=TRUE)

# Rename Zilas
temp <- read_xlsx("./data/geo_files/bbs_geos/geo.xlsx", skip=1)
temp <- data.frame(temp[3], temp[4])
temp <- unique(temp)
zila1$ZILA <- as.numeric(as.character(zila1$ZILA))
zila1 <- merge(temp, zila1, all.y=TRUE, by.x="District.Code", by.y="ZILA")
zila1 <- data.frame(zila1[-c(1)])
names(zila1)[1] <- "district"

#----------------------------------------------------------------------------------------------------------
# Save final dataset/metadata
#----------------------------------------------------------------------------------------------------------

# By Zila
write.csv(zila1,"./output/bbs/data/data_svrs_zila_2012.csv", row.names=FALSE) # Save data

meta_srvs12 <- data.frame("Source"="SRVS", "File"= "SRVS_12","Variable"=colnames(zila1))
write.csv(meta_srvs12,"./output/bbs/data/metadata_bbs_srvs_2012.csv", row.names=FALSE) # Save metadata

