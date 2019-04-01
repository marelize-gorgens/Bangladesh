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

if(!require(tidyverse)) install.packages("tidyverse", dependencies=TRUE); library(tidyverse)
if(!require(WDI)) install.packages("WDI", dependencies=TRUE); library(WDI)
if(!require(reshape2)) install.packages("reshape2", dependencies=TRUE); library(reshape2)
if(!require(ggmap)) install.packages("ggmap", dependencies=TRUE); library(ggmap)

#----------------------------------------------------------------------------------------------------------
# Load datasets 
#----------------------------------------------------------------------------------------------------------

ihmefiles <- list.files("./data/ihme/Data/", pattern="*.csv", full.names=TRUE) # List files
ihme <- lapply(ihmefiles, read.csv)
ihme2 <- bind_rows(ihme) # Combine lists
df <- ihme2[c(4,6,8,10,13,2,14)]
summary(df)
sum(is.na(df)) # Check missing

df2 <- dcast(df, location_name + sex_name + age_name + cause_name + year ~ measure_name, value.var="val") # Transform long in wide
str(df2)

# WDIsearch('income') # List CPIA reports available
# cpia <- WDI(indicator='IQ.CPA.ECON.XQ', country="all", start=2005, end=2018, extra=TRUE) # CPIA data

#----------------------------------------------------------------------------------------------------------
# Data analysis 
#----------------------------------------------------------------------------------------------------------  

set.seed(20)
clusters <- kmeans(df2[,6:8], 5) # K-means clustering (5 clusters)
df2$cluster <- as.factor(clusters$cluster)
str(clusters)

get_dist(df2)
