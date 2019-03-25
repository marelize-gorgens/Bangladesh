#----------------------------------------------------------------------------------------------------------
# dp_bbs_srvs_.R
# Description: Data preprocessing of SRVS datasets
# Organization: Health, Nutrition, and Population (HNP) | The World Bank
# Author: Hellen Matarazzo
# Date: Updated on 03-25-2019
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

#----------------------------------------------------------------------------------------------------------
# Load datasets
#----------------------------------------------------------------------------------------------------------

svrs17files <- list.files("C:/Users/wb531612/Documents/HNP/Bangladesh/data/bbs/svrs/SVRS_17/", recursive=TRUE, pattern="*.dta", full.names=TRUE) # List files
svrs17 <- sapply(svrs17files[c(2,3,8,10,11)], read_dta)
names(svrs17) <- c("tafsil-3", "tafsil-4","tafsil-9","tafsl-2h","tafsl-2p") # Rename lists

#----------------------------------------------------------------------------------------------------------
# List of variables of interest
#----------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------------
# Data transformation
#----------------------------------------------------------------------------------------------------------

attributes(svrs17[[4]])
get_variable_labels(svrs17[[4]]) # Get the data dictionary

#----------------------------------------------------------------------------------------------------------
# Create/save metadata
#----------------------------------------------------------------------------------------------------------

meta_svrs17 <- data.frame()
for (i in seq_along(svrs17)){
  meta <- data.frame("Source"="SRVS_17", "File"= names(svrs17[i]),get_variable_labels(svrs17[[i]]))
  meta_svrs17 <- rbind(meta_svrs17,meta)
}

write.csv(meta_svrs17,"./data/bbs/svrs/SVRS_17/metadata_bbs_SRVS_17.csv", row.names=FALSE) # Save metadata
