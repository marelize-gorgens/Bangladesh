# Getting libraries
if (!require('pacman')) install.pacakges('pacman')
pacman::p_load('rdhs')
pacman::p_load('devtools')
library('rdhs')
library('haven')
data = '/Users/edinhamzic/Symphony/wb_bangladesh/Bangladesh/data/dhs/ncd_data'
output = '/Users/edinhamzic/Symphony/wb_bangladesh/Bangladesh/output/dhs/ncd_data'

# Configuration
set_rdhs_config(email = "edin.hamzic@gmail.com",
                project = "WorldBank RMNCAH Bangladesh DataAnalytics",
                cache_path = data,
                config_path = "~/.rdhs.json",
                data_frame = "data.table::as.data.table",
                global = TRUE)

sc <- dhs_survey_characteristics()
sc
# Interesting set of data:
# 23: Blood pressure (measure)
# 68: Blood pressure questions
# 86: Non-communicable diseases
# 75: Breast cancer screening
# 63: Diabetes questions
# 46: Diabetes testing
# 86: Non-communicable diseases

survs2011 <- dhs_surveys(surveyCharacteristicIds = list(23, 68, 86, 75, 63, 46, 86),
                     countryIds = c("BD"),
                     surveyType = "DHS",
                     surveyYearStart = 2011)


datasets2011 <- dhs_datasets(surveyIds = survs2011$SurveyId, 
                         fileFormat = "flat", 
                         fileType = "PR")
str(datasets2011)
downloads2011 <- get_datasets(datasets2011$FileName)
ncd_data <- readRDS(downloads2011$BDPR61FL)
get_variable_labels(ncd_data)
write.csv(get_variable_labels(ncd_data), file = paste0(output, "/metadata_dd_dhs_ncd.csv"))
write.csv(ncd_data, file = paste0(output, "/data_dd_dhs_ncd.csv"))

