#----------------------------------------------------------------------------------------------------------
# dp_ihme.R
# Description: Data preprocessing of IHME datasets
# Organization: Health, Nutrition, and Population (HNP) | The World Bank
# Author: Hellen Matarazzo
# Date: Updated on 04-02-2019
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

if(!require(tidyverse)) installed.packages("tidyverse", dependencies=TRUE); library(tidyverse)
if(!require(WDI)) installed.packages("WDI", dependencies=TRUE); library(WDI)
if(!require(reshape2)) installed.packages("reshape2", dependencies=TRUE); library(reshape2)
if(!require(ggmap)) installed.packages("ggmap", dependencies=TRUE); library(ggmap)
if(!require(ggplot2)) installed.packages("ggplot2", dependencies=TRUE); library(ggplot2)
if(!require(corrplot)) installed.packages("corrplot", dependencies=TRUE); library(corrplot)
if(!require(Hmisc)) installed.packages("Hmisc", dependencies=TRUE); library(Hmisc)
if(!require(PerformanceAnalytics)) installed.packages("PerformanceAnalytics", dependencies=TRUE); library(PerformanceAnalytics)

#----------------------------------------------------------------------------------------------------------
# Load datasets 
#----------------------------------------------------------------------------------------------------------

# Data integration and cleaning: Load, combine and select features from IHME datasets
ihmefiles <- list.files("./data/ihme/Data/", pattern="*.csv", full.names=TRUE) # List files
ihme <- lapply(ihmefiles, read.csv)
ihme2 <- bind_rows(ihme) # Combine lists
df <- ihme2[c(4,13,6,8,10,2,14)] # Feature selection
sum(is.na(df)) # Check missing

# Data integration and cleaning: Load and merge GDP dataset
WDIsearch('GDP per capita') # List reports available
gdp <- WDI(indicator='NY.GDP.PCAP.PP.CD', country="all", start=1990, end=2017, extra=TRUE) # GDP data
gdp <- gdp[c(2,4,3,6,10,8,9)]
names(gdp)[3] <- "gdp"
df2 <- merge(df, gdp, all.x=TRUE, by.x=c("location_name", "year"), by.y=c("country","year"))
df2$longitude <- as.numeric(as.character(df2$longitude))
df2$latitude <- as.numeric(as.character(df2$latitude))

# Subset 
df3 <- df2 %>% # Subset female, by age, cause and measure
  filter(sex_name %in% c("Female"), 
         age_name %in% c("Under 5", "5-14 years", "15-49 years",  "50-69 years", "70+ years"),
         cause_name %in% c("Cardiovascular diseases", "Diabetes mellitus"), 
         measure_name %in% c("Incidence"))
df3wide <- dcast(df3, ... ~ sex_name + age_name + cause_name + measure_name, value.var="val") # Transform in wide
df3wide <- df3wide[c(1:7,17,11,9,13,15,16,10,8,12,14)] # Reorder features

df4wide <- df3wide %>% # Subset year==2017
  filter(year==2017)
  
#----------------------------------------------------------------------------------------------------------
# Exploratory data analysis 
#----------------------------------------------------------------------------------------------------------  

# Plot correlation matrix and p-value
corr <- rcorr(as.matrix(df3wide[-c(1:7)]))
m <- round(corr$r,1) # Creating correlation matrix 
m_mat <- round(corr$P,2) # Creating p-value matrix 

corrplot(m, method = "color", type = "upper", 
         # order="hclust",
         # diag = FALSE, 
         # insig="blank", 
         tl.col = "black", tl.srt = 60, tl.cex=0.6, 
         addCoef.col = "black",
         p.mat=m_mat, sig.level=0.01) # Plotting correlation matrix

# Plot distribution, correlaiton and p-value
chart.Correlation(df3wide[-c(1:7)], histogram = TRUE, pch = 19, pch="+")

#----------------------------------------------------------------------------------------------------------
# Data analysis 
#----------------------------------------------------------------------------------------------------------  

test1 <- na.omit(df4wide[-c(1:7)]) # Exclude na's
test1 <- scale(test1)

# Determine optimal number of clusters
wss <- (nrow(test1)-1)*sum(apply(test1, 2, var))
for (i in 2:15) wss[i] <- sum(kmeans(test1, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of clusters", ylab="Within groups sum of squares")

# K-Means clustering -------------------------------------------------------------------------------------
fit1 <- kmeans(test1, 5, nstart=20) # 5 clusters
fit1
fit1$size # Number of observations in each cluster
fit1$centers # Cluster centers (means) by variable
ggplot(df4wide, aes(gdp, income, color = df4wide$cluster)) + geom_point()
df4wide$km_cluster <- as.factor(fit1$cluster) # Cluster assignment for each observation

# Hierarchical clustering --------------------------------------------------------------------------------
d <- dist(test1, method = "euclidean") # distance matrix
fit2 <- hclust(d, method="ward.D") 
fit2
plot(fit2, hang=-1) # display dendogram
groups <- cutree(fit2, k=5) # cut tree into clusters
rect.hclust(fit2, k=5, border="red") # draw dendogram with red borders around the clusters 

# Hierarchical clustering with Bootstrapped --------------------------------------------------------------
library(pvclust)
fit3 <- pvclust(test1, method.hclust="ward.D", method.dist="euclidean")
fit3
plot(fit3) # dendogram with p values
pvrect(fit3, alpha=.95) # add rectangles around groups highly supported by the data

# Model Based Clustering ---------------------------------------------------------------------------------
library(mclust)
fit4 <- Mclust(test1)
fit4
summary(fit4) # display the best model
plot(fit4) # plot results 
df4wide$mb_cluster <- as.factor(fit4$classification) # Cluster assignment for each observation

# Plots ---------------------------------------------------------------------------------------------------
# vary parameters for most readable graph
library(cluster) 
clusplot(test1, fit1$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

#----------------------------------------------------------------------------------------------------------
# Save final dataset/metadata
#----------------------------------------------------------------------------------------------------------

write.csv(df2,"./output/ihme/data/data_ihme.csv", row.names=FALSE) # Save metadata
write.csv(df4wide,"./output/ihme/data/data_ihme_clusters.csv", row.names=FALSE) # Save metadata

meta_ihme <- data.frame("Source"="IHME", "File"= "IHME-GBD","Variable"=colnames(df))
write.csv(meta_ihme,"./output/ihme/data/metadata_ihme.csv", row.names=FALSE) # Save metadata
