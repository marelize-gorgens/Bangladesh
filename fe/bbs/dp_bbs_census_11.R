#----------------------------------------------------------------------------------------------------------
# dp_bbs_census_.R
# Description: Data preprocessing of Census datasets
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
# if(!require(haven)) installed.packages("haven", dependencies=TRUE); library(haven)
# if(!require(rdhs)) installed.packages("rdhs"); library(rdhs)
# if(!require(plyr)) installed.packages("plyr", dependencies=TRUE); library(plyr)
# if(!require(readxl)) installed.packages("readxl", dependencies=TRUE); library(readxl)

#----------------------------------------------------------------------------------------------------------
# Load datasets
#----------------------------------------------------------------------------------------------------------

censusfiles <- list.files("./data/bbs/census2011_parsed/", pattern="*.csv", full.names=TRUE) # List files
census <- sapply(censusfiles, read.csv)
