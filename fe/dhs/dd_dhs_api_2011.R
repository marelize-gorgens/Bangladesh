#----------------------------------------------------------------------------------------------------------
# dp_DHS_.R
# Description: Data preprocessing of DHS api 2011
# Organization: Health, Nutrition, and Population (HNP) | The World Bank
# Author: Hellen Matarazzo
# Date: Updated on 03-15-2019
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

if(!require(httr)) installed.packages("httr", dependencies=TRUE); library(httr)
if(!require(jsonlite)) installed.packages("jsonlite", dependencies=TRUE); library(jsonlite)
if(!require(RJSONIO)) installed.packages("RJSONIO", dependencies=TRUE); library(RJSONIO)

#----------------------------------------------------------------------------------------------------------
# Load datasets
#----------------------------------------------------------------------------------------------------------

api1 <- "https://api.dhsprogram.com/rest/dhs/data?countryIds=BD&surveyIds=BD2011DHS&lang=en&perpage=5000&f=json" # No breakdown and no indicators
api2 <- "https://api.dhsprogram.com/rest/dhs/data?breakdown=subnational&countryIds=BD&surveyIds=BD2011DHS&lang=en&perpage=5000&f=json" # No indicators specified
# "https://api1.dhsprogram.com/rest/dhs/surveycharacteristics/BD" # Edin
# "https://api1.dhsprogram.com/rest/dhs/data?breakdown=subnational&indicatorIds=FE_FRTR_W_TFR,FP_CUSM_W_ANY,FP_CUSM_W_MOD,FP_NADM_W_UNT,FP_NADM_W_PDM,MA_AAFM_W_M2B,SX_AAFS_W_M2B,CM_ECMR_C_IMR,CM_ECMR_C_U5M,MM_MMRO_W_PMR,MM_MMRO_W_MMR,RH_DELP_C_DHF,CH_VACC_C_BAS,CH_DIAT_C_ORT,CN_NUTS_C_HA2,CN_NUTS_C_WH2,CN_NUTS_C_WA2,CN_BFDR_C_MDE,ML_NETC_C_ITN,HA_CPHT_W_T1R,HA_CPHT_M_T1R,HA_HIVP_W_HIV,HA_HIVP_M_HIV,HA_HIVP_B_HIV,FG_PFCC_W_WCC,ED_EDUC_W_SEH,ED_LITR_W_LIT,HC_ELEC_H_ELC&countryIds=BD&surveyIds=BD2011DHS&lang=en&perpage=1000&f=json" # Selected indicators
# "https://api1.dhsprogram.com/rest/dhs/data?countryIds=BD&perpage=1000&f=json"

# National level ------------------------------------------------------------------------------------------
json_file <- RJSONIO::fromJSON(api1) # Import DHS Indicator data for TFR for each survey
pages <- json_file$TotalPages # Save the number of pages
data <- data.frame() 
for(i in 1:pages){ # Loop to create a data frame with all pages
  json_data <- lapply(json_file$Data, function(x) {unlist(x)}) # Unlist the JSON file entries
  apidata <- as.data.frame(do.call("rbind", json_data),stringsAsFactors=FALSE) # Convert JSON input to a data frame
  data <- rbind(data,apidata) # Merge all pages
}

# Save dataset
write.csv(data,"./data/dhs/data_dhs_2011_api_national.csv", row.names=FALSE) # Save data

# Sub-National level ---------------------------------------------------------------------------------------
json_file2 <- RJSONIO::fromJSON(api2) # Import DHS Indicator data for TFR for each survey
pages2 <- json_file2$TotalPages # Save the number of pages
data2 <- data.frame() 
for(i in 1:pages2){ # Loop to create a data frame with all pages
  json_data2 <- lapply(json_file2$Data, function(x) {unlist(x)}) # Unlist the JSON file entries
  apidata2 <- as.data.frame(do.call("rbind", json_data2),stringsAsFactors=FALSE) # Convert JSON input to a data frame
  data2 <- rbind(data2,apidata2) # Merge all pages
}

table(data2$CharacteristicLabel)
  
# Save dataset
write.csv(data2,"./data/dhs/data_dhs_2011_api_subnational.csv", row.names=FALSE) # Save data

