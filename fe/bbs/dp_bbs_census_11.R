#----------------------------------------------------------------------------------------------------------
# dp_bbs_census_.R
# Description: Data preprocessing of Census datasets
# Organization: Health, Nutrition, and Population (HNP) | The World Bank
# Author: Hellen Matarazzo
# Date: Updated on 04-01-2019
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

#----------------------------------------------------------------------------------------------------------
# Load datasets
#----------------------------------------------------------------------------------------------------------

censusfiles <- list.files("./output/bbs/data/census2011/", pattern=glob2rx("ZilaTotal_C*.csv"), full.names=TRUE) # List files
census <- lapply(censusfiles, read.csv)
census2 <- bind_cols(census) # Combine lists
colnames(census)
census3 <- census2[-c(1:6,8,11:23,27:35,48:55,68:75,87:94,104:111,118)] # Subset dataset
names(census3)[1] <- "district"
census3$district <- gsub(" Zila Total", "", census3$district)

#----------------------------------------------------------------------------------------------------------
# Save final dataset/metadata
#----------------------------------------------------------------------------------------------------------

# By Zila
write.csv(census3,"./output/bbs/data/data_census_2011.csv", row.names=FALSE) # Save data

meta_census3 <- data.frame("Source"="Census", "File"= "ZilaTotal_2011","Variable"=colnames(census3))
write.csv(meta_census3,"./output/bbs/data/metadata_census_2011.csv", row.names=FALSE) # Save metadata

