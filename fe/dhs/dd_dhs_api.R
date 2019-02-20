library(RJSONIO)
# ### Functions -----------------------------------------------------------

explore <- function(data) {
  for (i in seq_along(names(data))) {
    print("---------------------------------------------------------------------")
    print(paste("Variable: ", names(data)[i],  sep = " "))
    print(summary(as.factor(data[,i])))
  }
}
explore(APIdata)


# Fetching data from DHS Indicator Data API
list_BD_surveys <- list('BD1994DHS', 'BD1997DHS', 'BD2000DHS', 'BD2004DHS', 'BD2007DHS', 'BD2011DHS', 'BD2014DHS')
list_df <- list()
for (i in seq_along(list_BD_surveys)) {
  print("--------------------------------------------------------------------------")
  print(paste("Fetching data for survey ", list_BD_surveys[[i]], sep=" "))
  json_file <- fromJSON(paste("https://api.dhsprogram.com/rest/dhs/data?breakdown=all&indicatorIds=FE_FRTR_W_TFR,FP_CUSM_W_ANY,FP_CUSM_W_MOD,FP_NADM_W_UNT,FP_NADM_W_PDM,MA_AAFM_W_M2B,SX_AAFS_W_M2B,CM_ECMR_C_IMR,CM_ECMR_C_U5M,MM_MMRO_W_PMR,MM_MMRO_W_MMR,RH_DELP_C_DHF,CH_VACC_C_BAS,CH_DIAT_C_ORT,CN_NUTS_C_HA2,CN_NUTS_C_WH2,CN_NUTS_C_WA2,CN_BFDR_C_MDE,ML_NETC_C_ITN,HA_CPHT_W_T1R,HA_CPHT_M_T1R,HA_HIVP_W_HIV,HA_HIVP_M_HIV,HA_HIVP_B_HIV,FG_PFCC_W_WCC,ED_EDUC_W_SEH,ED_LITR_W_LIT,HC_ELEC_H_ELC&countryIds=BD&surveyIds=", list_BD_surveys[[i]],"&lang=en&f=json&perpage=5000&apiKey=SYMPH-267391", sep=""))
  json_data <- lapply(json_file$Data, function(x) { unlist(x) })
  api_data <- as.data.frame(do.call("rbind", json_data),stringsAsFactors=FALSE)
  print(paste("Number rows and columns in fetched dataframe: ", dim(api_data), sep=" "))
  list_df[[i]] <- api_data
  names(list_df)[i] <- list_BD_surveys[[i]]
}

for (i in seq_along(list_df)) {
write.csv(list_df[[i]],file = paste(names(list_df)[i], ".csv", sep =""), quote = F, row.names = F)
}




