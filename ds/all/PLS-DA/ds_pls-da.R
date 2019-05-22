#----------------------------------------------------------------------------------------------------------
# ds_rmncah_.R
# Description: PLS-DA analysis
# Organization: Health, Nutrition, and Population (HNP) | The World Bank
# Author: Hellen Matarazzo
# Date: Updated on 05-21-2019
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
if(!require(BiocManager)) install.packages("BiocManager"); library(BiocManager)
BiocManager::install("mixOmics"); library(mixOmics)

#----------------------------------------------------------------------------------------------------------
# Load datasets
#----------------------------------------------------------------------------------------------------------

# Import data
DATA2011 <- './output/all/all2011.csv'
DATA2016  <- './output/all/all2016.csv'
DHIS2_VARS  <-  './output/all/DHIS_Rate_Absolute.csv'
d2011 <- read_csv(DATA2011)
d2016 <- read_csv(DATA2016)
dhis2vars <- read_csv(DHIS2_VARS)

#----------------------------------------------------------------------------------------------------------
# Data preprocessing
#----------------------------------------------------------------------------------------------------------

# Remove absolute variables
tmp <- dhis2vars[dhis2vars$Rate_Absolute == "Absolute",]
d2011 <- d2011[,!names(d2011) %in% tmp$Full_name] 
d2016 <- d2016[,!names(d2016) %in% tmp$Full_name]

# Remove other counting variables
index_balance_d2011 <- grep("*balance", colnames(d2011))
index_balance_d2016 <- grep("*balance", colnames(d2016))
d2011[index_balance_d2011] <- NULL
d2016[index_balance_d2016] <- NULL

# Remove variables with 0 variance
d2016 <- d2016[-which(apply(d2011, 2, var)==0)]
d2011 <- d2011[-which(apply(d2011, 2, var)==0)]
d2011 <- d2011[-which(apply(d2016, 2, var)==0)]
d2016 <- d2016[-which(apply(d2016, 2, var)==0)]

# Remove geo feature
d2011$DistrictGeo <- NULL
d2016$DistrictGeo <- NULL

# Set District to index
df_d2011 <- as.data.frame(d2011[-c(25)])
rownames(df_d2011) <- d2011$DistrictName
df_d2016 <- as.data.frame(d2016[-c(25)])
rownames(df_d2016) <- d2016$DistrictName

# Create scale and standardize dataset
s_d2011 <- as.data.frame(scale(df_d2011))
s_d2016 <- as.data.frame(scale(df_d2016))

# Renaming variables
var_names <- read_csv("./output/all/IndicatorsNames_2011_2016.csv")
names(s_d2011) <- var_names$Description[match(names(s_d2011), var_names$`Indicators 2011`)]
names(s_d2016) <- var_names$Description[match(names(s_d2016), var_names$`Indicators 2011`)]

# Create dataset for percentage of change between years
df_change <- round((df_d2016[colnames(df_d2011)]-df_d2011)/df_d2011*100,2)

#----------------------------------------------------------------------------------------------------------
# Set outcome variables
#----------------------------------------------------------------------------------------------------------

# Maternal mortality rate categories
index_mmr <- grep("*maternal_mortality", colnames(df_d2011))
outcome_mmr <- df_d2011[index_mmr]
summary(outcome_mmr)
outcome_mmr$cat <- ifelse(outcome_mmr<=3, "Low-MMR", ifelse(outcome_mmr>=5, "Hight-MMR", "Medium-MMR"))
ggplot(outcome_mmr, aes(x=rate_maternal_mortality)) + 
  geom_histogram(aes(y=..density..), colour="darkgray", fill="darkgray") +
  geom_density(col=1, alpha=.2) +
  geom_vline(data=outcome_mmr, aes(xintercept=median(rate_maternal_mortality)), linetype="dashed") +
  labs(title="Maternal Mortality Rate Histogram", x="Rate", y="Count")

# Maternal mortality rate categories
index_ufmr <- grep("*under5y_mortality", colnames(df_d2011))
outcome_ufmr <- df_d2011[index_ufmr]
summary(outcome_ufmr)
outcome_ufmr$cat <- ifelse(outcome_ufmr<=42, "Low-U5MR", ifelse(outcome_ufmr>=50, "Hight-U5MR", "Medium-U5MR"))
ggplot(outcome_ufmr, aes(x=rate_under5y_mortality)) + 
  geom_histogram(aes(y=..density..), colour="darkgray", fill="darkgray") +
  geom_density(col=1, alpha=.2) +
  geom_vline(data=outcome_ufmr, aes(xintercept=median(rate_under5y_mortality)), linetype="dashed") +
  labs(title="Under 5 Mortality Rate Histogram", x="Rate", y="Count")

#----------------------------------------------------------------------------------------------------------
# Data quality assessment
#----------------------------------------------------------------------------------------------------------

# Validity: Check format and type
which(sapply(d2011, is.numeric)==FALSE)
which(sapply(d2016, is.numeric)==FALSE)

# Uniqueness: Check for duplicates
sum(duplicated(d2011))
sum(duplicated(d2016))

# Completeness: Check for NAs
na_d2011 <- data.frame(number_na = colSums(is.na(d2011)),perc_na = round(colMeans(is.na(d2011))*100,2))
na_d2016 <- data.frame(number_na = colSums(is.na(d2016)),perc_na = round(colMeans(is.na(d2016))*100,2))

#----------------------------------------------------------------------------------------------------------
# Data Analysis
#----------------------------------------------------------------------------------------------------------

##### Maternal mortality rate 2011
X <- as.matrix(s_d2011[-c(index_mmr)])
Y <- as.factor(outcome_mmr$cat)

# sPLS-DA analysis
plsda.mmr <- plsda(X, Y, # Model with 2 components
      ncomp = 5,
      scale = FALSE,
      mode = "regression",
      tol = 1e-06,
      max.iter = 100,
      near.zero.var = FALSE,
      logratio="none",  
      multilevel=NULL,
      all.outputs = TRUE)

plotIndiv(plsda.mmr, # Plot the samples
          group = outcome_mmr$cat, 
          ind.names = FALSE, 
          legend = TRUE, 
          title = 'PLSDA on MMR - 2011', 
          ellipse = TRUE, 
          comp = 1:2)

plotVar(plsda.mmr, 
        comp = 1:2, 
        rad.in = 0.5, 
        cex = 2, 
        var.names = FALSE) # Plot the variables

### NOT WORKING
# selectVar(plsda.mmr, comp=1)$value
# plotLoadings(plsda.mmr, contrib = 'max', method = 'mean')
# plotLoadings(plsda.mmr, comp = 1, size.name = rel(0.5))

# Evaluate a PLS-DA performance
perf.plsda.mmr <- perf(plsda.mmr, # 5-fold cross-validation, repeated 10 times
                       validation = "Mfold", 
                       folds = 5, 
                       progressBar = FALSE, 
                       nrepeat = 10, 
                       auc = TRUE)

plot(perf.plsda.mmr, 
     col = color.mixo(1:3), 
     sd = TRUE, 
     legend.position = "horizontal")

matplot(perf.plsda.mmr$error.rate$BER, type = 'l', lty = 1, 
        col = color.mixo(1:3), 
        main = 'Balanced Error rate')

auroc(plsda.mmr) # ROC curve ! ONLY for sPLS-DA



