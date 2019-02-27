#----------------------------------------------------------------------------------------------------------
# dp_unicef_.R
# Description: Data preprocessing of Unicef datasets
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
if(!require(zoo)) install.packages("zoo", dependencies = TRUE); library(zoo)
if(!require(reshape2)) install.packages("reshape2", dependencies = TRUE); library(reshape2)

#----------------------------------------------------------------------------------------------------------
# Load datasets
#----------------------------------------------------------------------------------------------------------

list.files("data/unicef/", recursive=T) # List all files on folder 'unicef' and its subfolders

#----------------------------------------------------------------------------------------------------------
# CBSS dataset
#----------------------------------------------------------------------------------------------------------
path <- "data/unicef/CBSS/factsheet_district_2019_01_21_22_53_59.xlsx"
excel_sheets(path) # Check existing sheets
cbss <- read_excel(path, sheet="factsheet", skip=4, na="") # Load CBSS sheet, filling blanks with 'Nas'

#----------------------------------------------------------------------------------------------------------
# Data transformation
#----------------------------------------------------------------------------------------------------------

# CBSS dataset
#----------------------------------------------------------------------------------------------------------
attach(cbss)
head(cbss)

# Data quality assessment
sum(is.na(cbss)) # Completeness: Check for NAs
na_cbss <- data.frame(lapply(cbss, function(y) sum(length(which(is.na(y)))))) # Show NAs by column
cbss$Programme <- na.locf(cbss$Programme)  # Fill NAs (merged cells in excel) with last value by column
cbss$Module <- na.locf(cbss$Module)
sum(is.na(cbss))

sum(duplicated(cbss)) # Uniqueness: Check for duplicates
cbss <- unique(cbss) # Exclude duplicate rows
sum(duplicated(cbss))

str(cbss) # Validity: Check format and type
cols <- c(4:68)
cbss[,cols] <- apply(cbss[,cols], 2, function(x) as.numeric(as.character(x))) # Set numeric variables
str(cbss)

max(cbss[,cols]) # Show global max # Validity: Check range (between 0 and 100)
max_cbss <- data.frame(lapply(cbss, function(z) max((z)))) # Show max by column

# Feature engeering
cbss$Variable  <- paste("CBSS",1:nrow(cbss), sep=".") # Create short name for indicators

# Create/save metadata
meta_cbss <- cbss[c(69,1:3)] # Select the variables of interest for metadata file
meta_cbss$Type <- "Numeric, %"
write.csv(meta_cbss,"metadata_unicef_cbss.csv", row.names=FALSE) # Save metadata

# Create/save final dataset
cbss2 <- as.data.frame(t(cbss[c(4:68)])) # Transpose only the variables of interest
colnames(cbss2) <- cbss$Variable # Set variables names
Unit <- str_remove_all(rownames(cbss2), "[ (%)]") # Clear Units names
Year <- ""
cbss2 <- data.frame(Unit, Year, cbss2) # Add Units names to the dataset
rownames(cbss2) <- NULL # Delete row names

write.csv(cbss2,"data_unicef_cbss.csv", row.names=FALSE) # Save data
