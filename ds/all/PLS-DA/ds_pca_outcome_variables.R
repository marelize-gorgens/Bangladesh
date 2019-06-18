################################################################################
####################### PCA ON OUTCOME VARIABLES ################## ############
################################################################################


rm(list=ls()) # Remove objects
graphics.off # Close graphics
cat("\014") # Clear console

################################################################################
####################### LOAD PACKAGES ##########################################
################################################################################

if(!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE);library(tidyverse)
if(!require(evaluate)) install.packages("evaluate"); library(evaluate)
if(!require(digest)) install.packages("digest"); library(digest)
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if(!require(mixOmics)) install.packages("mixOmics"); library(mixOmics)
if(!require(cluster)) install.pacakges('cluster'); library(cluster)
if(!require(factoextra)) install.packages('factoextra'); library(factoextra)



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

################################################################################
####################### PERFORMING PCA ON OUTCOME VARIABLES ####################
################################################################################


# STEP 1: Getting outcome variables
outcome_variables <- c('prop_antenatal_care4.', 'rate_under5y_mortality',
                       'rate_maternal_mortality', 'rate_death',
                       'prop_unmet_need_family_planing')

data <- rbind(d2011[outcome_variables], d2016[outcome_variables])
data_r <- data
pca_data <- pca(data, ncomp = 5, scale = TRUE, center = TRUE)
plotVar(pca_data, comp = c(1,2), title = "PC1 vs PC2 Correlation plot")
plotVar(pca_data, comp = c(1,3), title = "PC1 vs PC3 Correlation plot")
plotVar(pca_data, comp = c(2,3), title = "PC2 vs PC3 Correlation plot")
par(mfrow=c(2,2))
plotLoadings(pca_data,comp = 1, title = "PC1 Loadings", size.title =1)
plotLoadings(pca_data,comp = 2, title = "PC2 Loadings", size.title =1)
plotLoadings(pca_data,comp = 3, title = "PC3 Loadings", size.title =1)
plotLoadings(pca_data,comp = 4, title = "PC4 Loadings", size.title =1)


data$cat_ufmr <- ifelse(data$rate_under5y_mortality<=33, "Low_UFMR", ifelse(data$rate_under5y_mortality>=66, "High_UFMR", "Medium_UFMR"))
data$cat_mmr <- ifelse(data$rate_maternal_mortality<=75, "Low_MMR", ifelse(data$rate_maternal_mortality>=400, "High_MMR", "Medium_MMR"))
#data$cat_death <- ifelse(data$rate_death<=5, "Low_DEATH", ifelse(data$rate_death>=6, "High_DEATH", "Medium_DEATH"))
data$cat_death <- ifelse(data$rate_death<=5, "Low_DEATH", "High_DEATH")
data$cat_anc <- ifelse(data$prop_antenatal_care4.<=15, "Low_DEATH", ifelse(data$rate_death>=30, "High_DEATH", "Medium_DEATH"))
data$cat_unmet <- ifelse(data$rate_death<=3, "Low_DEATH", ifelse(data$rate_death>=6, "High_DEATH", "Medium_DEATH"))

data <- transform(data, target_variable=paste(cat_ufmr, cat_death, cat_anc, cat_unmet))
sort(summary(as.factor(data$target_variable)))
length(sort(summary(as.factor(data$target_variable))))
View(data[, c('target_variable','prop_antenatal_care4.', 'rate_under5y_mortality',
          'rate_death', 'prop_unmet_need_family_planing')])

################################################################################
####################### PERFORMING KMEANS ON OUTCOME VARIABLES #################
################################################################################
set.seed(84)
fviz_nbclust(scale(data_r), kmeans, method = "wss")
set.seed(84)
fviz_nbclust(scale(data_r), kmeans, method = "silhouette")
set.seed(84)
gap_stat <- clusGap(scale(data_r), FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)


kmeans_clustering <- kmeans(scale(data_r), centers = 4, nstart = 25)
kmeans_clustering
data_r$Cluster <- kmeans_clustering$cluster
aggregate(data_r[, 1:5], list(data_r$Cluster), mean)
aggregate(data_r[, 1:5], list(data_r$Cluster), min)
aggregate(data_r[, 1:5], list(data_r$Cluster), max)
