#----------------------------------------------------------------------------------------------------------
# dp_unicef_.R
# Description: Data preprocessing of Unicef MICS3 datasets
# Organization: Health, Nutrition, and Population (HNP) | The World Bank
# Author: Hellen Matarazzo
# Date: Updated on 03-24-2019
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
if(!require(rdhs)) installed.packages("rdhs"); library(rdhs)
if(!require(survey)) installed.packages("survey", dependencies=TRUE); library(survey)
if(!require(plyr)) installed.packages("plyr", dependencies=TRUE); library(plyr)
if(!require(sjlabelled)) installed.packages("sjlabelled", dependencies=TRUE); library(sjlabelled)

#----------------------------------------------------------------------------------------------------------
# Load datasets 
#----------------------------------------------------------------------------------------------------------

mics3files <- list.files("./data/unicef/MICS3", recursive=TRUE, pattern="*.sav", full.names=TRUE) # List files 
mics3 <- sapply(mics3files,read_sav)
names(mics3) <- c("ch3","hh3","hl3","wm3") # Rename lists

#----------------------------------------------------------------------------------------------------------
# Data transformation
#----------------------------------------------------------------------------------------------------------

# Children dataset
ch3 <- mics3$ch3 # Load as data frame
ch3_var <- get_variable_labels(ch3) # Get the data dictionary

# Women dataset
wm3 <- mics3$wm3 # Load as data frame
wm3_var <- get_variable_labels(wm3) # Get the data dictionary

#----------------------------------------------------------------------------------------------------------
# Survey design
#----------------------------------------------------------------------------------------------------------

# ch3design <- svydesign(data=ch3, id=~HH1, weight=~chweight, strata=~HH7A) # Set survey design for dataset
# sum(duplicated(ch3)) # Uniqueness: Check for duplicates
# sum(is.na(ch3)) # Completeness: Check for NAs
# na_ch3 <- data.frame(lapply(ch3, function(y) sum(length(which(is.na(y)))))) # Save NAs by column

#----------------------------------------------------------------------------------------------------------
# List of variables of interest
#----------------------------------------------------------------------------------------------------------

### Unit
# HH7A      District
# HH7       Division
# chweight  Children's sample weight

#----------------------------------------------------------------------------------------------------------
# Data transformation
#----------------------------------------------------------------------------------------------------------

ch3 <- ch3 %>% filter(UF9 == 1) # Subset children actually interviewed
wm3 <- wm3 %>% filter(WM7 == 1) # Subset women actually interviewed

# Exclusive breastfeeding under 6 months
table(ch3$cage) # Age <6
attributes(ch3$BF1) # Child ever been breastfed ==1
attributes(ch3$BF2) # Child still been breastfed ==1
attributes(ch3$BF3B) # Child received plain water ==2
attributes(ch3$BF3C) # Child received sweetened water or juice ==2
attributes(ch3$BF3E) # Child received infant formula ==2
attributes(ch3$BF3F) # Child received milk ==2
attributes(ch3$BF3G) # Child received other liquids ==2
attributes(ch3$BF3H) # Child received solid or mushy food ==2
ch3$exclusive_breastfeeding <- ifelse(ch3$cage<6 & ch3$BF1==1 & ch3$BF2==1 & ch3$BF3B==2 & ch3$BF3C==2 & ch3$BF3E==2 &
                                        ch3$BF3F==2 & ch3$BF3G==2 & ch3$BF3H==2, 1,
                                      ifelse(ch3$cage<6, 0, NA))
 
# Diarrhorea treatment via ORT
attributes(ch3$CA1) # Child had diarrhoea in last 2 weeks ==1
attributes(ch3$CA2A) # Drank fluid made from special packet (ORS) ==1
attributes(ch3$CA2B) # Government-recommended homemade fluid ==1
attributes(ch3$CA2C) # Pre-packaged ORS fluid for diarrhoea ==1
attributes(ch3$CA3) # Child drank less or more during illness ==3
ch3$diarrhoea_ors <- ifelse(ch3$CA1==1 & (ch3$CA2A==4 | ch3$CA2B==1 | ch3$CA2C==1 | ch3$CA3==3), 1,
                            ifelse(ch3$CA1==1, 0, NA))

# Care-seeking for children with ARI symptoms
attributes(ch3$CA5) # Child ill with cough in last 2 weeks ==1
attributes(ch3$CA6) # Difficulty breathing during illness with cough & ==1
attributes(ch3$CA7) # Symptoms due to problem in chest or blocked nose & ==1 | 3
attributes(ch3$CA9A) # Place sought care: Govt Hospital/Health centre
attributes(ch3$CA9D) # Place sought care: Village health worker/HA
attributes(ch3$CA9E) # Place sought care: Mobile/outreach clinic
attributes(ch3$CA9H) # Place sought care: Other public source
attributes(ch3$CA9I) # Place sought care: Private hospital/clinic
attributes(ch3$CA9J) # Place sought care: Private physician
attributes(ch3$CA9O) # Place sought care: Other private medical
attributes(ch3$CA9S) # Place sought care: NGO hospital/clinic
ch3$ari_care <- ifelse((ch3$CA5==1 & ch3$CA6==1 & (ch3$CA7==1 | ch3$CA7==3)) &
                         (ch3$CA9A=="A" | ch3$CA9D=="D" | ch3$CA9E=="E" | ch3$CA9H=="H" |
                            ch3$CA9I=="I" | ch3$CA9J=="J" | ch3$CA9O=="O" | ch3$CA9S=="S"), 1,
                       ifelse(ch3$CA5==1 & ch3$CA6==1 & (ch3$CA7==1 | ch3$CA7==3), 0, NA))

# Antibiotics treatment for ARI symptoms
attributes(ch3$CA10) # Given medicine to treat this illness ==1
attributes(ch3$CA11A) # Medicine: Antibiotic pill / syrup =="A"
ch3$ari_antibiotics <- ifelse((ch3$CA5==1 & ch3$CA6==1 & (ch3$CA7==1 | ch3$CA7==3)) & ch3$CA11A=="A", 1,
                              ifelse(ch3$CA5==1 & ch3$CA6==1 & (ch3$CA7==1 | ch3$CA7==3), 0, NA))

# Antenatal care coverage
attributes(wm3$CM12) # Live birth in last 2 years =="Y"
attributes(wm3$MN2A) # Antenatal care: Doctor
attributes(wm3$MN2B) # Antenatal care: Nurse / Midwife
attributes(wm3$MN2F) # Antenatal care: Traditional birth attendant
attributes(wm3$MN2G) # Antenatal care: Community health worker (FWA/HA/MA)
attributes(wm3$MN2X) # Antenatal care: Other
wm3$antenatal <- ifelse(wm3$CM12=="Y" & (wm3$MN2A=="A"| wm3$MN2B=="B"| wm3$MN2F=="F"| wm3$MN2G=="G"| wm3$MN2X=="X"), 1,
                        ifelse(wm3$CM12=="Y", 0, NA))

# Skilled attendant at delivery
attributes(wm3$CM12) # Last birth in last two years =="Y"
attributes(wm3$MN7A) # Assistance at delivery: Doctor
attributes(wm3$MN7B) # Assistance at delivery: Nurse / Midwife
wm3$attendant_delivery <- ifelse(wm3$CM12=="Y" & (wm3$MN7A=="A" | wm3$MN7B=="B"), 1,
                                 ifelse(wm3$CM12=="Y", 0, NA))

# Institutional deliveries
attributes(wm3$CM12) # Last birth in last two years =="Y"
attributes(wm3$MN8) # Place of delivery ==21, 22, 23, 26, 31, 32, 33, 36
wm3$institutional_delivery <- ifelse(wm3$CM12=="Y" & wm3$MN8 %in% c(21, 22, 23, 26, 31, 32, 33, 36), 1, 
                                     ifelse(wm3$CM12=="Y", 0, NA))

#----------------------------------------------------------------------------------------------------------
# Create final dataset
#----------------------------------------------------------------------------------------------------------

final1 <- ddply(ch3, ~HH7A, summarise, # Summarize by district
                exclusive_breastfeeding=weighted.mean(exclusive_breastfeeding, chweight, na.rm=TRUE),
                diarrhoea_ors=weighted.mean(diarrhoea_ors, chweight, na.rm=TRUE),
                ari_care=weighted.mean(ari_care, chweight, na.rm=TRUE),
                ari_antibiotics=weighted.mean(ari_antibiotics, chweight, na.rm=TRUE))

final2 <- ddply(wm3, ~HH7A, summarise, # Summarize by district
                antenatal=weighted.mean(antenatal, wmweight, na.rm=TRUE),
                attendant_delivery=weighted.mean(attendant_delivery, wmweight, na.rm=TRUE),
                institutional_delivery=weighted.mean(institutional_delivery, wmweight, na.rm=TRUE))

final <- merge(final1, final2)
final$year <- 2006
names(final)[1] <- "District"
final$District <- get_labels(wm3$HH7A)
 
#----------------------------------------------------------------------------------------------------------
# Save final dataset/metadata
#----------------------------------------------------------------------------------------------------------

write.csv(final,"./output/unicef/data/data_unicef_mics3_2006.csv", row.names=FALSE) # Save metadata

meta_mics3 <- data.frame("Source"="MICS", "File"= "MICS3","Variable"=colnames(final))
write.csv(meta_mics3,"./output/unicef/data/metadata_unicef_mics3.csv", row.names=FALSE) # Save metadata

#----------------------------------------------------------------------------------------------------------
# Create/save metadata (raw data)
#----------------------------------------------------------------------------------------------------------

meta_mics3 <- data.frame()
for (i in seq_along(mics3)){
  meta <- data.frame("Source"="MICS3", "File"= names(mics3[i]),"Variable"=colnames(mics3[[i]]),
                     "Description"= sapply(mics3[[i]], function(x) attributes(x)$label))
  meta_mics3 <- rbind(meta_mics3,meta)
}
write.csv(meta_mics3,"./data/unicef/MICS3/metadata_unicef_mics3.csv", row.names=FALSE) # Save metadata
