
# ### Fetching data using rdhs R packages ---------------------------------

if (!require('pacman')) install.pacakges('pacman')
pacman::p_load('OJWatson/rdhs')
pacman::p_load('devtools')
devtools::install_github("OJWatson/rdhs")
library('rdhs')
library('haven')
set_rdhs_config(email = "edin.hamzic@gmail.com", project = "WorldBank RMNCAH Bangladesh DataAnalytics")
datasets_bd <- dhs_datasets(countryIds='BD')
filter_criteria <- sapply(datasets_bd$FileFormat, function(x) {"Stata dataset (.dta)" == x})
datasets_bd <- datasets_bd[filter_criteria,]
datasets <- get_datasets(datasets_bd$FileName)

Service_Availability_Raw <- readRDS(datasets$BDBR72DT)
Household_Member_Recode <- readRDS(datasets$BDPR72DT)
Childrens_Recode <- readRDS(datasets$BDKR72DT)
Individual_Recode <- readRDS(datasets$BDIR72DT)
Household_Recode <- readRDS(datasets$BDHR72DT)
Births_Recode <- readRDS(datasets$BDBR72DT)

dataBD_2014 <- list(Service_Availability_Raw, Household_Member_Recode, 
                    Childrens_Recode, Individual_Recode, Household_Recode,
                    Births_Recode)
names(dataBD_2014) <- c("Service_Availability_Raw", "Household_Member_Recode", 
                        "Childrens_Recode", "Individual_Recode", "Household_Recode",
                        "Births_Recode")

sapply(dataBD_2014, dim)
View(dataBD_2014[[1]])

grep('district', sapply(data, function(x){attr(x, 'label')}), value = TRUE)
summary(as.factor(data$shdist))
View(data[,'shdist'])
summary(as.factor(data$hwdist))
summary(as.factor(data$hwclust))
sapply(data, function(x) {length(levels(as.factor(x)))})
