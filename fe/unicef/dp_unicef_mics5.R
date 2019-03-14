#----------------------------------------------------------------------------------------------------------
# dp_unicef_.R
# Description: Data preprocessing of Unicef MICS5 datasets
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

if(!require(tidyverse)) installed.packages("tidyverse", dependencies=TRUE); library(tidyverse)
if(!require(haven)) installed.packages("haven", dependencies=TRUE); library(haven)
if(!require(survey)) installed.packages("survey", dependencies=TRUE); library(survey)

#----------------------------------------------------------------------------------------------------------
# Load datasets
#----------------------------------------------------------------------------------------------------------

mics5files <- list.files("./data/unicef/MICS5", recursive=TRUE, pattern="*.sav", full.names=TRUE) # List files 
mics5 <- sapply(mics5files,read_sav)
names(mics5) <- c("ch5","hh5","hl5","wm5") # Rename lists

#----------------------------------------------------------------------------------------------------------
# Data transformation
#----------------------------------------------------------------------------------------------------------

ch5 <- mics5$ch5 # Load as data frame
ch5design <- svydesign(id=~HH7A, data=ch5, weight=~chweight) # Set survey design for dataset

# svyby(~CA1==1,~HH7A,design = ch5design,FUN = svymean,keep.names = FALSE) #does not include row.names for subgroup variable

# "Child had diarrhoea in last 2 weeks"
# attr(ch5[[24]],'labels'); table(ch5[[24]])
# table(ch5$CA1==1,ch5$HH7A)attributes(ch5$CA1)
diarrhoea <- ch5 %>%
  group_by(HH7A) %>%
  filter(CA1==1)%>%
  summarise(diarrhoea = n())

# "Drank fluid made from special packet (ORS)"
table(ch5$CA4A==1|ch5$CA4B==1,ch5$HH7A)
attributes(ch5$CA4A)
ors <- ch5 %>%
  group_by(HH7A) %>%
  filter(CA4A==1 | CA4B==1)%>%
  summarise(n())

# "Rice-based ORS packet for diarrhoea"
attributes(ch5$CA4B)
table(ch5$CA4B==1,ch5$HH7A)


#----------------------------------------------------------------------------------------------------------
# Create/save metadata
#----------------------------------------------------------------------------------------------------------

meta_mics5 <- data.frame()
for (i in seq_along(mics5)){
  meta <- data.frame("Source"="MICS5", "File"= names(mics5[i]),"Variable"=colnames(mics5[[i]]), "Description"= sapply(mics5[[i]], function(x) attributes(x)$label))
  meta_mics5 <- rbind(meta_mics5,meta)
}
write.csv(meta_mics5,"./output/unicef/data/metadata_unicef_mics5.csv", row.names=FALSE) # Save metadata
