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

svrs13files <- list.files("C:/Users/wb531612/Documents/HNP/Bangladesh/data/bbs/svrs/SVRS_13/", recursive=TRUE, pattern="*.sav", full.names=TRUE) # List files
svrs13 <- sapply(svrs13files[c(1:2,5:6,11)], read_sav)
names(svrs13) <- c("tafsil2P", "tafsil2H", "tafsil3", "tafsil4", "tafsil9") # Rename lists

#----------------------------------------------------------------------------------------------------------
# Create/save metadata
#----------------------------------------------------------------------------------------------------------

meta_svrs13 <- data.frame()
for (i in seq_along(svrs13)){
  meta <- data.frame("Source"="SRVS_13", "File"= names(svrs13[i]),get_variable_labels(svrs13[[i]]))
  meta_svrs13 <- rbind(meta_svrs13,meta)
}

write.csv(meta_svrs13,"./data/bbs/svrs/SVRS_13/metadata_bbs_SRVS_13.csv", row.names=FALSE) # Save metadata
