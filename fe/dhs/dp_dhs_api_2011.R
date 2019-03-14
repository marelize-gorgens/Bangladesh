#----------------------------------------------------------------------------------------------------------
# dp_DHS_.R
# Description: Data preprocessing of DHS API
# Organization: Health, Nutrition, and Population (HNP) | The World Bank
# Author: Hellen Matarazzo
# Date: Updated on 03-13-2019
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

api <- "https://api.dhsprogram.com/rest/dhs/data?countryIds=BD&surveyIds=BD2011DHS&lang=en&perpage=1000&f=json" # No breakdown and no indicators
# "https://api.dhsprogram.com/rest/dhs/data?breakdown=subnational&countryIds=BD&surveyIds=BD2011DHS&lang=en&perpage=1000&f=json" # No indicators specified
# "https://api.dhsprogram.com/rest/dhs/surveycharacteristics/BD" # Edin
# "https://api.dhsprogram.com/rest/dhs/data?breakdown=subnational&indicatorIds=FE_FRTR_W_TFR,FP_CUSM_W_ANY,FP_CUSM_W_MOD,FP_NADM_W_UNT,FP_NADM_W_PDM,MA_AAFM_W_M2B,SX_AAFS_W_M2B,CM_ECMR_C_IMR,CM_ECMR_C_U5M,MM_MMRO_W_PMR,MM_MMRO_W_MMR,RH_DELP_C_DHF,CH_VACC_C_BAS,CH_DIAT_C_ORT,CN_NUTS_C_HA2,CN_NUTS_C_WH2,CN_NUTS_C_WA2,CN_BFDR_C_MDE,ML_NETC_C_ITN,HA_CPHT_W_T1R,HA_CPHT_M_T1R,HA_HIVP_W_HIV,HA_HIVP_M_HIV,HA_HIVP_B_HIV,FG_PFCC_W_WCC,ED_EDUC_W_SEH,ED_LITR_W_LIT,HC_ELEC_H_ELC&countryIds=BD&surveyIds=BD2011DHS&lang=en&perpage=1000&f=json" # Selected indicators
# "https://api.dhsprogram.com/rest/dhs/data?countryIds=BD&perpage=1000&f=json"


### Option 1
# Import DHS Indicator data for TFR for each survey
json_file <- fromJSON(api)
# Unlist the JSON file entries
json_data <- lapply(json_file$Data, function(x) {unlist(x)})
# Convert JSON input to a data frame
APIdata <- as.data.frame(do.call("rbind", json_data),stringsAsFactors=FALSE)
# Tabulate the TFR values by the survey IDs
# xtabs(as.numeric(Value) ~ SurveyId, data=APIdata)

### Option 2
# Import DHS Indicator data for TFR for each survey
get_data <- httr::GET(api)
# Start the process of deserialization: Convert the raw data from your API call into JSON format
get_data_text <- content(get_data, "text")
# Parse the JSON
get_data_json <- jsonlite::fromJSON(get_data_text, flatten = TRUE)
# Convert the parsed JSON to a data frame for analysis
get_data_df <- as.data.frame(get_data_json)
# Initialize the pages variable
pages <- get_data_json$TotalPages
# For loop that gets each page of data
for(i in 2:pages){
  #Making an API call that has page_number= at the end. This will increment by 1 in each loop until you have all pages
  #Making the API call
  get_data_2 <- httr::GET(api)
  #Parsing it to JSON
  get_data_text_2 <- content(get_data_2, "text")
  #Converting it from JSON to a list you can use. This actually gives you a list, one item of which is the data, with the rest is information about the API call
  get_data_json_2 <- jsonlite::fromJSON(get_data_text_2, flatten = TRUE)
  #This grabs just the data you want and makes it a data frame
  get_data_df_2 <- as.data.frame(get_data_json_2)
  #Now you add the data to the existing data frame and repeat
  get_data_df <- rbind(get_data_df, get_data_df_2)
}

# Create/save dataset
write.csv(get_data_df,"./data/dhs/data_dhs_2011_national.csv", row.names=FALSE) # Save data
