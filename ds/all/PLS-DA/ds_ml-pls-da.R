################################################################################
####################### MULTILEVEL PLS-DA ANALYSIS #############################
################################################################################


rm(list=ls()) # Remove objects
graphics.off # Close graphics
cat("\014") # Clear console

################################################################################
####################### LOAD PACKAGES ##########################################
################################################################################

if (!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE);library(tidyverse)
if (!require(evaluate)) install.packages("evaluate"); library(evaluate)
if (!require(digest)) install.packages("digest"); library(digest)
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!require(mixOmics)) install.packages("mixOmics"); library(mixOmics)
if (!require(dplyr)) install.pacakges("dplyr"); library(dplyr)
if (!require(stringr)) install.packages("stringr"); library(stringr)
if (!require(rgl)) install.pacakges("rgl"); library(rgl)
################################################################################
####################### IMPORT DATA ############################################
################################################################################

# Set paths
DATA2011 <- './output/all/all2011_2.csv'
DATA2016  <- './output/all/all2016_2.csv'
DHIS2_VARS  <-  './output/all/DHIS_Rate_Absolute.csv'
var_names <- read_csv("./output/all/IndicatorsNames_2011_2016.csv")
outputpath <- "./ds/all/PLS-DA/"
# Import data
d2011 <- read_csv(DATA2011)
d2016 <- read_csv(DATA2016)
dhis2vars <- read_csv(DHIS2_VARS)

dim_summary <- function(df, string="") {
  print(paste(rep("#",80), collapse = ""))
  print(paste("Number of rows data 2011: ", dim(df)[1], string))
  print(paste("Number of columns data 2011: ", dim(df)[2], string))
  print(paste("Number of rows data 2016: ", dim(df)[1], string))
  print(paste("Number of columns data 2016: ", dim(df)[2], string))
  print(paste(rep("#",80), collapse = ""))
  
} 


################################################################################
####################### DATA PREPROCESSING #####################################
################################################################################

# STEP 1: Uniqueness: Check for duplicates
sum(duplicated(d2011))
sum(duplicated(d2016))

# STEP 2: Validity: Check format and type
which(sapply(d2011, is.numeric)==FALSE)
which(sapply(d2016, is.numeric)==FALSE)

# STEP 3: Completeness: Check for NAs
na_d2011 <- data.frame(number_na = colSums(is.na(d2011)),perc_na = round(colMeans(is.na(d2011))*100,2))
na_d2016 <- data.frame(number_na = colSums(is.na(d2016)),perc_na = round(colMeans(is.na(d2016))*100,2))

# STEP 4: Remove absolute variables
tmp1 <- dhis2vars[dhis2vars$Rate_Absolute == "Absolute",]
d2011 <- d2011[,!names(d2011) %in% tmp1$Full_name] 
d2016 <- d2016[,!names(d2016) %in% tmp1$Full_name]
dim_summary(d2011, string = "After removing absolute variables")
dim_summary(d2016, string = "After removing absolute variables")


# STEP 5: Remove other counting variables
index_balance_d2011 <- grep("*balance", colnames(d2011))
index_balance_d2016 <- grep("*balance", colnames(d2016))
d2011[index_balance_d2011] <- NULL
d2016[index_balance_d2016] <- NULL
dim_summary(d2011, string = "After removing other counting variables")
dim_summary(d2016, string = "After removing other counting variables")


# STEP 6: Remove variables with 0 variance
d2016 <- d2016[-which(apply(d2011, 2, var)==0)]
d2011 <- d2011[-which(apply(d2011, 2, var)==0)]
d2011 <- d2011[-which(apply(d2016, 2, var)==0)]
d2016 <- d2016[-which(apply(d2016, 2, var)==0)]
dim_summary(d2011, string = "After removing variables with 0 variance")
dim_summary(d2016, string = "After removing variables with 0 variance")


# STEP 7: Remove variables with 20% or more missings
missingthreshold <- 20
tmp2 <- na_d2011[na_d2011$perc_na >= missingthreshold,]
d2011 <- d2011[,!names(d2011) %in% rownames(tmp2)] 
d2016 <- d2016[,!names(d2016) %in% rownames(tmp2)]
dim_summary(d2011, string = "After removing variables with missing values")
dim_summary(d2016, string = "After removing variables with missing values")


# STEP 8: Remove geo feature
d2011$DistrictGeo <- NULL
d2016$DistrictGeo <- NULL
dim_summary(d2011, string = "After removing  variables with missing values")
dim_summary(d2016, string = "After removing  variables with missing values")

# STEP 9: Set District to index
d2011 <- as.data.frame(d2011)
d2016 <- as.data.frame(d2016)
rownames(d2011) <- d2011$DistrictName
rownames(d2016) <- d2016$DistrictName
d2011$DistrictName <- NULL
d2016$DistrictName <- NULL
dim_summary(d2011, string = "After removing  variables with missing values")
dim_summary(d2016, string = "After removing variables with missing values")


# STEP 10: Scaling data
# Create dataset for percentage of change between years
df_d2011 <- Filter(is.numeric, d2011)
df_d2016 <- Filter(is.numeric, d2016)
df_change <- round((df_d2016[colnames(df_d2011)]-df_d2011)/df_d2011*100,2)

# Create scale and standardize datasets
s_d2011 <- as.data.frame(scale(df_d2011))
s_d2016 <- as.data.frame(scale(df_d2016))
s_change <- as.data.frame(scale(df_change))

data <- rbind(df_d2011, df_d2016)
s_data <- as.data.frame(scale(data))

# STEP 11: Creating index outcome variable

outcome_variables_5 <- c('prop_antenatal_care4.', 'rate_under5y_mortality',
                        'rate_maternal_mortality', 'rate_death',
                        'prop_unmet_need_family_planing')
outcome_variables_4 <- c('prop_antenatal_care4.', 'rate_under5y_mortality',
                        'rate_death', 'prop_unmet_need_family_planing')

# Proportion of antenatal coverage (4 visits): High==Good, Low==Bad
# Proportion of under 5 mortality: High==Bad, Low==Good
# Proportion of maternal mortality: High==Bad, Low==Good
# Proportion of death rate: High==Bad, Low==Good
# Proportion of unmet need for family planning: High==Bad, Low==Good
# Only to inverse antenatal coverage (4 visits)

# Step 11a: Inverse values so that High means good and Low means bad for all outcome variables
s_data$rate_under5y_mortality <- -s_data$rate_under5y_mortality
s_data$rate_maternal_mortality <- -s_data$rate_maternal_mortality
s_data$rate_death <- -s_data$rate_death
s_data$prop_unmet_need_family_planing <- -s_data$prop_unmet_need_family_planing

# Step 11b: Create two index outcome variables

s_data$outcome_index_4 <- rowSums(s_data[outcome_variables_4])
s_data$outcome_index_5 <- rowSums(s_data[outcome_variables_5]) 

s_data$outcome_index_4_class <- as.factor(ifelse(s_data$outcome_index_4<=-2.5, "Low",
                                 ifelse(s_data$outcome_index_4<=0,"Medium Low",
                                        ifelse(s_data$outcome_index_4>=2.5, "High", "Medium High"))))
s_data$outcome_index_5_class <- as.factor(ifelse(s_data$outcome_index_5<=-2.5, "Low",
                                       ifelse(s_data$outcome_index_5<=0,"Medium Low",
                                              ifelse(s_data$outcome_index_5>=2.5, "High", "Medium High"))))

s_data$outcome_index_4_class <- as.factor(ifelse(s_data$outcome_index_4<=0, "Low", "High"))
s_data$outcome_index_5_class <- as.factor(ifelse(s_data$outcome_index_5<=0, "Low", "High"))
summary(s_data$outcome_index_4_class)
summary(s_data$outcome_index_5_class)

# Step 11c: Remove original variables
print(dim(s_data))
s_data <- s_data[,-which(names(s_data) %in% outcome_variables_5)]
print(dim(s_data))


################################################################################
################### CREATE MASTER FILE AND DESIGN MATRIX IMPORT DATA ###########
################################################################################
# STEP 12: Create design matrix, master file and output file

indicators <- s_data[,-which(x=names(s_data) %in% c("outcome_index_4", 
                                                    "outcome_index_5",
                                                    "outcome_index_4_class",
                                                    "outcome_index_5_class") )]
districts <- as.factor(str_replace(rownames(data), pattern = "1", replacement = ""))
years <- as.factor(c(rep(1,64), rep(2, 64)))
outcome_index_4 <- s_data[,'outcome_index_4_class']
outcome_index_5 <- s_data[,'outcome_index_5_class']
rownames(indicators) <- paste(districts, c(rep(2011,64), rep(2016,64)))
  
################################################################################
################### FINE-TUNING PLS-DA ANALYSIS - DISCRIMINATIVE ###############
################################################################################

# STEP 1: Plain vanilla run
plsda_outcome4 <- plsda(indicators,outcome_index_4,ncomp = 10)
plsda_outcome5 <- plsda(indicators,outcome_index_5,ncomp = 10)
set.seed(84)

# STEP 2: Defining number of variables to keep per iteration and setting seed
list.keepX <- c(seq(2, 100, 5))

# STEP 3: Fine-tunning for outcome_index_4 variable
set.seed(84) 
tune_splda_outcome4 <- tune.splsda(indicators, outcome_index_4, ncomp = 10,
                                   validation = 'Mfold', folds = 5, 
                                   progressBar = T, dist = 'max.dist',
                                   test.keepX = list.keepX, nrepeat = 10) 

outcome4_choice.ncomp <- tune_splda_outcome4$choice.ncomp$ncomp
print(outcome4_choice.ncomp)
outcome4_choice.keepX <- tune_splda_outcome4$choice.keepX[1:outcome4_choice.ncomp]
print(outcome4_choice.keepX)
# STEP 4: Fine-tunning for outcome_index_5 variable
tune_splda_outcome5 <- tune.splsda(indicators, outcome_index_5, ncomp = 10,
                                   validation = 'Mfold', folds = 5, 
                                   progressBar = T, dist = 'max.dist',
                                   test.keepX = list.keepX, nrepeat = 10)
outcome5_choice.ncomp <- tune_splda_outcome5$choice.ncomp$ncomp
print(outcome5_choice.ncomp)
outcome5_choice.keepX <- tune_splda_outcome5$choice.keepX[1:outcome5_choice.ncomp]
print(outcome5_choice.keepX)
# STEP 5: Running the analysis
splsda_outcome4.res <- splsda(indicators, outcome_index_4,
                              ncomp = outcome4_choice.ncomp, 
                              keepX = outcome4_choice.keepX) 
splsda_outcome5.res <- splsda(indicators, outcome_index_5, 
                              ncomp = outcome5_choice.ncomp, 
                              keepX = outcome5_choice.keepX) 

################################################################################
################### FINE-TUNING PLS-DA ANALYSIS - INTEGRATIVE APPROACH #########
################################################################################
# numbers here indicate the ID of the individuals
plotIndiv(splsda_outcome4.res,style = '3d', legend = T)
plotIndiv(splsda_outcome4.res, legend = T)
plotIndiv(splsda_outcome5.res,style = '3d', legend = T)
plotVar(splsda_outcome5.res)
selectVar(splsda_outcome5.res, comp = 1)$value


