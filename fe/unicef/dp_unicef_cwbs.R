#----------------------------------------------------------------------------------------------------------
# dp_unicef_.R
# Description: Data preprocessing of Unicef CWBS datasets
# Organization: Health, Nutrition, and Population (HNP) | The World Bank
# Author: Hellen Matarazzo
# Date: Updated on 02-27-2019
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
if(!require(readxl)) install.packages("readxl", dependencies = TRUE); library(readxl)

#----------------------------------------------------------------------------------------------------------
# Load CWBS datasets
#----------------------------------------------------------------------------------------------------------

list.files("data/unicef/", recursive=T) # List all files on folder 'unicef' and its subfolders

path <- "data/unicef/CWBS/child_wellbeing_survey.xlsx"
sheets <- excel_sheets(path)
xl_list <- lapply(excel_sheets(path), read_excel, path = path)
names(xl_list) <- sheets
names(xl_list)
 
