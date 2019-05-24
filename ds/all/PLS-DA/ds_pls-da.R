#----------------------------------------------------------------------------------------------------------
# ds_rmncah_.R
# Description: PLS-DA analysis
# Organization: Health, Nutrition, and Population (HNP) | The World Bank
# Author: Hellen Matarazzo
# Date: Updated on 05-24-2019
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
if(!require(evaluate)) install.packages("evaluate"); library(evaluate)
if(!require(digest)) install.packages("digest"); library(digest)
if(!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install("mixOmics")
# BiocManager::install()
if(!require(mixOmics)) install.packages("mixOmics"); library(mixOmics)

#----------------------------------------------------------------------------------------------------------
# Load datasets
#----------------------------------------------------------------------------------------------------------

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

#----------------------------------------------------------------------------------------------------------
# Data quality assessment - Pre-preprocessing
#----------------------------------------------------------------------------------------------------------

# Uniqueness: Check for duplicates
sum(duplicated(d2011))
sum(duplicated(d2016))

# Validity: Check format and type
which(sapply(d2011, is.numeric)==FALSE)
which(sapply(d2016, is.numeric)==FALSE)

# Completeness: Check for NAs
na_d2011 <- data.frame(number_na = colSums(is.na(d2011)),perc_na = round(colMeans(is.na(d2011))*100,2))
na_d2016 <- data.frame(number_na = colSums(is.na(d2016)),perc_na = round(colMeans(is.na(d2016))*100,2))

#----------------------------------------------------------------------------------------------------------
# Data preprocessing
#----------------------------------------------------------------------------------------------------------

# Remove absolute variables
tmp1 <- dhis2vars[dhis2vars$Rate_Absolute == "Absolute",]
d2011 <- d2011[,!names(d2011) %in% tmp1$Full_name] 
d2016 <- d2016[,!names(d2016) %in% tmp1$Full_name]

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

# Remove variables with 20% or more missings
missingthreshold <- 20
tmp2 <- na_d2011[na_d2011$perc_na >= missingthreshold,]
d2011 <- d2011[,!names(d2011) %in% rownames(tmp2)] 
d2016 <- d2016[,!names(d2016) %in% rownames(tmp2)] 

# Remove geo feature
d2011$DistrictGeo <- NULL
d2016$DistrictGeo <- NULL

# Set District to index
d2011 <- as.data.frame(d2011)
d2016 <- as.data.frame(d2016)
rownames(d2011) <- d2011$DistrictName
rownames(d2016) <- d2016$DistrictName
d2011$DistrictName <- NULL
d2016$DistrictName <- NULL

# Renaming variables
# names(d2011) <- var_names$Description[match(names(d2011), var_names$`Indicators 2011`)]
# names(d2016) <- var_names$Description[match(names(d2016), var_names$`Indicators 2011`)]

#----------------------------------------------------------------------------------------------------------
# Set outcome variables
#----------------------------------------------------------------------------------------------------------

sdg_mmr <- 70
sgd_ufmr <- 25
quantilelow <- 0.25 # Percentil 75th
quantilehigh <- 0.75 # Percentil 75th

# !!! Low threshold: MMR and under 5 based on SDGs and death rate and ANC based on percentil 25 for 2011 
# !!! High threshold: All based on percentil 75 for 2011 

# Maternal mortality rate categories
quantilehigh_mmr <- quantile(d2011$rate_maternal_mortality, quantilehigh, names=FALSE)
d2011$cat_mmr <- ifelse(d2011$rate_maternal_mortality<=sdg_mmr, "Low-MMR", ifelse(d2011$rate_maternal_mortality>=quantilehigh_mmr, "High-MMR", "Medium-MMR"))
d2016$cat_mmr <- ifelse(d2016$rate_maternal_mortality<=sdg_mmr, "Low-MMR", ifelse(d2016$rate_maternal_mortality>=quantilehigh_mmr, "High-MMR", "Medium-MMR"))

# Under 5 mortality rate categories
quantilehigh_ufmr <- quantile(d2011$rate_under5y_mortality, quantilehigh, names=FALSE)
d2011$cat_ufmr <- ifelse(d2011$rate_under5y_mortality<=sgd_ufmr, "Low-MMR", ifelse(d2011$rate_under5y_mortality>=quantilehigh_ufmr, "High-MMR", "Medium-MMR"))
d2016$cat_ufmr <- ifelse(d2016$rate_under5y_mortality<=sgd_ufmr, "Low-MMR", ifelse(d2016$rate_under5y_mortality>=quantilehigh_ufmr, "High-MMR", "Medium-MMR"))

# Death rate categories
quantilelow_death <- quantile(d2011$rate_death, quantilelow, names=FALSE)
quantilehigh_death <- quantile(d2011$rate_death, quantilehigh, names=FALSE)
d2011$cat_death <- ifelse(d2011$rate_death<=quantilelow_death, "Low-MMR", ifelse(d2011$rate_death>=quantilehigh_death, "High-MMR", "Medium-MMR"))
d2016$cat_death <- ifelse(d2016$rate_death<=quantilelow_death, "Low-MMR", ifelse(d2016$rate_death>=quantilehigh_death, "High-MMR", "Medium-MMR"))

# Antenatal care 4+ categories
quantilelow_anc <- quantile(d2011$prop_antenatal_care4., quantilelow, names=FALSE)
quantilehigh_anc <- quantile(d2011$prop_antenatal_care4., quantilehigh, names=FALSE)
d2011$cat_anc <- ifelse(d2011$prop_antenatal_care4.<=quantilelow_anc, "Low-MMR", ifelse(d2011$prop_antenatal_care4.>=quantilehigh_anc, "High-MMR", "Medium-MMR"))
d2016$cat_anc <- ifelse(d2016$prop_antenatal_care4.<=quantilelow_anc, "Low-MMR", ifelse(d2016$prop_antenatal_care4.>=quantilehigh_anc, "High-MMR", "Medium-MMR"))

# Plot histograms
graphics.off()
png(filename=paste0(outputpath,"Hist_MMR_2011.png"),res=300, width = 2000, height = 2000)
ggplot(d2011, aes(x=rate_maternal_mortality)) + 
  geom_histogram(aes(y=..density..), colour="darkgray", fill="darkgray") +
  geom_density(col=1, alpha=.2) +
  geom_vline(data=d2011, aes(xintercept=median(rate_maternal_mortality)), linetype="dashed") +
  labs(title="Histogram for MMR - 2011", x="Rate", y="Count")
dev.off()

graphics.off()
png(filename=paste0(outputpath,"Hist_UFMR_2011.png"),res=300, width = 2000, height = 2000)
ggplot(d2011, aes(x=rate_under5y_mortality)) + 
  geom_histogram(aes(y=..density..), colour="darkgray", fill="darkgray") +
  geom_density(col=1, alpha=.2) +
  geom_vline(data=d2011, aes(xintercept=median(rate_under5y_mortality)), linetype="dashed") +
  labs(title="Histogram for U5MR - 2011", x="Rate", y="Count")
dev.off()

graphics.off()
png(filename=paste0(outputpath,"Hist_DeathRate_2011.png"),res=300, width = 2000, height = 2000)
ggplot(d2011, aes(x=rate_death)) + 
  geom_histogram(aes(y=..density..), colour="darkgray", fill="darkgray") +
  geom_density(col=1, alpha=.2) +
  geom_vline(data=d2011, aes(xintercept=median(rate_death)), linetype="dashed") +
  labs(title="Histogram for Death Rate - 2011", x="Rate", y="Count")
dev.off()

graphics.off()
png(filename=paste0(outputpath,"Hist_Antenatal_2011.png"),res=300, width = 2000, height = 2000)
ggplot(d2011, aes(x=prop_antenatal_care4.)) + 
  geom_histogram(aes(y=..density..), colour="darkgray", fill="darkgray") +
  geom_density(col=1, alpha=.2) +
  geom_vline(data=d2011, aes(xintercept=median(prop_antenatal_care4.)), linetype="dashed") +
  labs(title="Histogram for Antenatal Care 4+ - 2011", x="Rate", y="Count")
dev.off()

#----------------------------------------------------------------------------------------------------------
# Create matrix for analysis
#----------------------------------------------------------------------------------------------------------

# Create dataset for percentage of change between years
df_d2011 <- Filter(is.numeric, d2011)
df_d2016 <- Filter(is.numeric, d2016)
df_change <- round((df_d2016[colnames(df_d2011)]-df_d2011)/df_d2011*100,2)

# Create scale and standardize datasets
s_d2011 <- as.data.frame(scale(df_d2011))
s_d2016 <- as.data.frame(scale(df_d2016))
s_change <- as.data.frame(scale(df_change))

#----------------------------------------------------------------------------------------------------------
# Save final datasets 
#----------------------------------------------------------------------------------------------------------

write.csv(d2011,paste0(outputpath,"all2011_final.csv"))
write.csv(d2016,paste0(outputpath,"all2016_final.csv"))
write.csv(df_change,paste0(outputpath,"change_final.csv"))

#----------------------------------------------------------------------------------------------------------
# Data Analysis
#----------------------------------------------------------------------------------------------------------

# 2011 ----------------------------------------------------------------------------------------------------
# Maternal mortality rate
X <- as.matrix(s_d2011[-c(grep("*maternal_mortality", colnames(s_d2011)))])
Y <- as.factor(d2011$cat_mmr)
ncomp = 5

# PLS-DA analysis
plsda.mmr <- plsda(X, Y, # Model with 2 components
      ncomp = ncomp,
      scale = FALSE,
      mode = "regression",
      tol = 1e-06,
      max.iter = 100,
      near.zero.var = FALSE,
      logratio="none",  
      multilevel=NULL,
      all.outputs = TRUE)

# Show individual component contribution
selectVar(plsda.mmr, comp=1)$value

#  Percentage of variance explained by each PLS components
plsda.mmr$explained_variance

Rd.YvsU <- cor(as.numeric(as.factor(Y)), plsda.mmr$variates$X[, 1:ncomp])
Rd.YvsU <- apply(Rd.YvsU^2, 2, sum)
Rd.Y <- cbind(Rd.YvsU, cumsum(Rd.YvsU))
colnames(Rd.Y) <- c("Proportion", "Cumulative")
Rd.Y

# Plot the samples
graphics.off()
png(filename=paste0(outputpath,"PLSDASample_MMR_2011.png"),res=300, width = 2000, height = 2000)
plotIndiv(plsda.mmr, 
          group = Y, 
          ind.names = FALSE, 
          legend = TRUE, 
          legend.position = "bottom",
          title = 'PLSDA samples on MMR - 2011',
          cex=1,
          ellipse = TRUE, 
          comp = 1:2,
          centroid = TRUE)
dev.off()

# Plot the variables
graphics.off()
png(filename=paste0(outputpath,"PLSDAVariables_MMR_2011.png"),res=300, width = 2000, height = 2000)
plotVar(plsda.mmr, 
        comp = 1:2, 
        rad.in = 0.3, 
        cex = 2, 
        title = 'PLSDA variables on MMR - 2011',
        var.names = FALSE) # Plot the variables
dev.off()

# Plot component contribution
graphics.off()
png(filename=paste0(outputpath,"PLSDALoadings_MMR_2011.png"),res=300, width = 4500, height = 3000)
plotLoadings(plsda.mmr, 
             comp = 1, 
             contrib = 'max', 
             method = 'median',
             ndisplay = 30,
             plot = TRUE, 
             size.name = 0.8, 
             size.legend = 0.8,
             name.var = NULL,
             name.var.complete = FALSE,
             title = 'PLSDA component contribution on MMR - 2011',
             layout = NULL, border = NA, xlim = NULL)
dev.off()

# Evaluate a PLS-DA performance
perf.plsda.mmr <- perf(plsda.mmr, # 5-fold cross-validation, repeated 10 times
                       validation = "Mfold", 
                       folds = 5, 
                       progressBar = FALSE, 
                       nrepeat = 10, 
                       auc = TRUE)

# Plot error performance
graphics.off()
png(filename=paste0(outputpath,"PLSDAPerformance_MMR_2011.png"),res=300, width = 2000, height = 2000)
plot(perf.plsda.mmr, 
     col = color.mixo(1:3), 
     sd = TRUE,
     main = "PLSDA performance on MMR - 2011")
dev.off()

# Plot ROC curve
graphics.off()
png(filename=paste0(outputpath,"PLSDAROC_MMR_2011.png"), res=300, width = 2000, height = 2000)
auroc(plsda.mmr) # ROC curve
dev.off()

# # Tuning model
# tune.splsda <- tune.splsda(X, Y, 
#                            ncomp = ncomp, 
#                            validation = 'Mfold', 
#                            folds = 10, 
#                            progressBar = FALSE, 
#                            dist = 'max.dist', 
#                            nrepeat = 10) 
# tune.splsda$choice.keepX
# tune.splsda$choice.ncomp$ncomp

