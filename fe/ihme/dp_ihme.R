#----------------------------------------------------------------------------------------------------------
# dp_ihme.R
# Description: Data preprocessing of IHME datasets
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

if(!require(tidyverse)) installed.packages("tidyverse", dependencies=TRUE); library(tidyverse)
if(!require(WDI)) installed.packages("WDI", dependencies=TRUE); library(WDI)
if(!require(reshape2)) installed.packages("reshape2", dependencies=TRUE); library(reshape2)
if(!require(ggmap)) installed.packages("ggmap", dependencies=TRUE); library(ggmap)

#----------------------------------------------------------------------------------------------------------
# Load datasets 
#----------------------------------------------------------------------------------------------------------

ihmefiles <- list.files("./data/ihme/Data/", pattern="*.csv", full.names=TRUE) # List files
ihme <- lapply(ihmefiles, read.csv)
ihme2 <- bind_rows(ihme) # Combine lists
df <- ihme2[c(4,13,6,8,10,2,14)] # Select variables of interess
sum(is.na(df)) # Check missing

wide <- dcast(df, location_name + year ~ sex_name + age_name + measure_name + cause_name, value.var="val") # Transform long in wide

ncd <- df %>% 
  filter(sex_name=="Both", age_name=="All Ages", cause_name %in% c("Cardiovascular diseases", "Diabetes mellitus"), year==2017)

# WDIsearch('x') # List reports available
# x <- WDI(indicator='x', country="all", start=2005, end=2017, extra=TRUE) # x data

#----------------------------------------------------------------------------------------------------------
# Data analysis 
#----------------------------------------------------------------------------------------------------------  

# Clustering
set.seed(20)
k1 <- kmeans(wide[,3:326], 5, nstart = 25) # K-means clustering (5 clusters)
wide$cluster <- as.factor(k1$cluster)
str(k1)

#----------------------------------------------------------------------------------------------------------
# Save final dataset/metadata
#----------------------------------------------------------------------------------------------------------

write.csv(df,"./output/ihme/data/data_ihme.csv", row.names=FALSE) # Save metadata

meta_ihme <- data.frame("Source"="IHME", "File"= "IHME-GBD","Variable"=colnames(df))
write.csv(meta_ihme,"./output/ihme/data/metadata_ihme.csv", row.names=FALSE) # Save metadata
