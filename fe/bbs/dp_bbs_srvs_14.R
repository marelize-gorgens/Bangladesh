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

svrs14files <- list.files("C:/Users/wb531612/Documents/HNP/Bangladesh/data/bbs/svrs/SVRS_14/", recursive=TRUE, pattern="*.sav", full.names=TRUE) # List files
svrs14 <- sapply(svrs14files[c(3:6,11)], read_sav)
names(svrs14) <- c("tafsil2H", "tafsil2P", "tafsil3", "tafsil4", "tafsil9") # Rename lists

#----------------------------------------------------------------------------------------------------------
# Create/save metadata
#----------------------------------------------------------------------------------------------------------

meta_svrs14 <- data.frame()
for (i in seq_along(svrs14)){
  meta <- data.frame("Source"="SRVS_14", "File"= names(svrs14[i]),get_variable_labels(svrs14[[i]]))
  meta_svrs14 <- rbind(meta_svrs14,meta)
}

write.csv(meta_svrs14,"./data/bbs/svrs/SVRS_14/metadata_bbs_SRVS_14.csv", row.names=FALSE) # Save metadata
