#----------------------------------------------------------------------------------------------------------
# dp_ncd_merge.R
# Description: NCD data merging
# Organization: Health, Nutrition, and Population (HNP) | The World Bank
# Author: Hellen Matarazzo
# Date: Updated on 04-23-2019
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
if(!require(readxl)) installed.packages("readxl", dependencies=TRUE); library(readxl)
if(!require(plyr)) installed.packages("plyr", dependencies=TRUE); library(plyr)

#----------------------------------------------------------------------------------------------------------
# Load datasets
#----------------------------------------------------------------------------------------------------------

path <- list.files("./output/ncd/", recursive=TRUE, pattern="*.csv", full.names=TRUE) # List files
ncd <- lapply(path, read_csv)
names(ncd) <- c("d2012", "d2014", "d2015", "d2017", "h2012", "h2014", "h2015", "h2017")

#----------------------------------------------------------------------------------------------------------
# Data transformation
#----------------------------------------------------------------------------------------------------------

# Add year to variables names
for (i in 1:length(ncd)){ 
  names(ncd[[i]])[c(2:4)] <- paste(names(ncd[[i]])[c(2:4)],substr(names(ncd[i]), 1, 5), sep="_")
}

# Rename zilas
temp <- read_xlsx("./data/bbs/svrs/SVRS_17/MSVSB PSU 2015.xlsx")
temp <- data.frame(temp[4], temp[5])
temp <- unique(temp)

h2017 <- colnames(ncd$h2017) # h2017 dataset
ncd$h2017[[1]] <- as.numeric(ncd$h2017[[1]]) 
ncd$h2017 <- merge(temp, ncd$h2017, all.y=TRUE, by.x="zl", by.y="DIVN_ZILA")
ncd$h2017 <- data.frame(ncd$h2017[-c(1)])
names(ncd$h2017) <- h2017

d2017 <- colnames(ncd$d2017) # d2017 dataset
ncd$d2017[[1]] <- as.numeric(ncd$d2017[[1]]) 
ncd$d2017 <- merge(temp, ncd$d2017, all.y=TRUE, by.x="zl", by.y="DIVN_ZILA")
ncd$d2017 <- data.frame(ncd$d2017[-c(1)])
names(ncd$d2017) <- d2017

h2015 <- colnames(ncd$h2015) # h2015 dataset
ncd$h2015[[1]] <- as.numeric(ncd$h2015[[1]]) 
ncd$h2015 <- merge(temp, ncd$h2015, all.y=TRUE, by.x="zl", by.y="DIVN_ZILA")
ncd$h2015 <- data.frame(ncd$h2015[-c(1)])
names(ncd$h2015) <- h2015

d2015 <- colnames(ncd$d2015) # d2015 dataset
ncd$d2015[[1]] <- as.numeric(ncd$d2015[[1]]) 
ncd$d2015 <- merge(temp, ncd$d2015, all.y=TRUE, by.x="zl", by.y="DIVN_ZILA")
ncd$d2015 <- data.frame(ncd$d2015[-c(1)])
names(ncd$d2015) <- d2015

h2012 <- colnames(ncd$h2012) # h2012 dataset
ncd$h2012[[1]] <- substr(ncd$h2012[[1]], 3, 4)
ncd$h2012[[1]] <- as.numeric(ncd$h2012[[1]]) 
ncd$h2012 <- merge(temp, ncd$h2012, all.y=TRUE, by.x="zl", by.y="DIVN_ZILA")
ncd$h2012 <- data.frame(ncd$h2012[-c(1)])
names(ncd$h2012) <- h2012

d2012 <- colnames(ncd$d2012) # d2012 dataset
ncd$d2012[[1]] <- substr(ncd$d2012[[1]], 3, 4)
ncd$d2012[[1]] <- as.numeric(ncd$d2012[[1]]) 
ncd$d2012 <- merge(temp, ncd$d2012, all.y=TRUE, by.x="zl", by.y="DIVN_ZILA")
ncd$d2012 <- data.frame(ncd$d2012[-c(1)])
names(ncd$d2012) <- d2012

# Correct zila's names in 2014 datsets
from <- c("Kishorganj","Cox's Bazar")
to <- c("Kishorgonj","Cox'S Bazar")
ncd$h2014[[1]] <- mapvalues(ncd$h2014[[1]], from=from, to=to)
ncd$d2014[[1]] <- mapvalues(ncd$d2014[[1]], from=from, to=to)

# Merge all datasets
ncd_all <- Reduce(merge, ncd)

#----------------------------------------------------------------------------------------------------------
# Save final dataset
#----------------------------------------------------------------------------------------------------------

write.csv(ncd_all,"./output/ncd/prevalence_all.csv", row.names=FALSE) # Save metadata

