#----------------------------------------------------------------------------------------------------------
# dp_unicef_.R
# Description: Data preprocessing of Unicef MICS5 datasets
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
if(!require(sjlabelled)) install.packages("sjlabelled", dependencies=TRUE); library(sjlabelled)

#----------------------------------------------------------------------------------------------------------
# Load datasets
#----------------------------------------------------------------------------------------------------------

mics5files <- list.files("./data/unicef/MICS5", recursive=TRUE, pattern="*.sav", full.names=TRUE) # List files 
mics5 <- sapply(mics5files,read_sav)
names(mics5) <- c("ch5","hh5","hl5","wm5") # Rename lists

#----------------------------------------------------------------------------------------------------------
# Data transformation
#----------------------------------------------------------------------------------------------------------

# Children dataset
ch5 <- mics5$ch5 # Load as data frame
ch5_var <- get_variable_labels(ch5) # Get the data dictionary

# Women dataset
wm5 <- mics5$wm5 # Load as data frame
wm5_var <- get_variable_labels(wm5) # Get the data dictionary

# # Household dataset
# hh5 <- mics5$hh5 # Load as data frame
# hh5_var <- get_variable_labels(hh5) # Get the data dictionary
# 
# # Household members dataset
# hl5 <- mics5$hl5 # Load as data frame
# hl5_var <- get_variable_labels(hl5) # Get the data dictionary

#----------------------------------------------------------------------------------------------------------
# Survey design
#----------------------------------------------------------------------------------------------------------

# ch5design <- svydesign(data=ch5, id=~HH1, weight=~chweight, strata=~HH7A) # Set survey design for dataset
# sum(duplicated(ch5)) # Uniqueness: Check for duplicates
# sum(is.na(ch5)) # Completeness: Check for NAs
# na_ch5 <- data.frame(lapply(ch5, function(y) sum(length(which(is.na(y)))))) # Save NAs by column

#----------------------------------------------------------------------------------------------------------
# List of variables of interest
#----------------------------------------------------------------------------------------------------------

### Unit
# HH7A    District
# HH7     Division

#----------------------------------------------------------------------------------------------------------
# Data transformation
#----------------------------------------------------------------------------------------------------------

ch5 <- ch5 %>% filter(UF9 == 1) # Subset children actually interviewed
wm5 <- wm5 %>% filter(WM7 == 1) # Subset women actually interviewed

# Underweight prevalence 
attributes(ch5$WAZ2) # Weight for age
ch5$underweight <- ifelse(ch5$WAZ2 < (-2), 1,
                          ifelse(!(ch5$WAZ2 %in% c(99.97, 99.98, 99.99)), 0, NA))

# Stuning prevalence 
attributes(ch5$HAZ2) # Height for age
ch5$stuning <- ifelse(ch5$HAZ2 < (-2), 1,
                      ifelse(!(ch5$HAZ2 %in% c(99.97, 99.98, 99.99)), 0, NA))

# Wasting prevalence
attributes(ch5$WHZ2) # Weight for height
ch5$wasting <- ifelse(ch5$WHZ2 < (-2), 1,
                      ifelse(!(ch5$WHZ2 %in% c(99.97, 99.98, 99.99)), 0, NA))

# Overweight prevalence
ch5$overweight <- ifelse(ch5$WHZ2 > 2 & !(ch5$WHZ2 %in% c(99.97, 99.98, 99.99)), 1,
                         ifelse(!(ch5$WHZ2 %in% c(99.97, 99.98, 99.99)), 0, NA))

# Exclusive breastfeeding under 6 months
attributes(ch5$CAGE) # Age
attributes(ch5$BF1) # Child ever been breastfed ==1
attributes(ch5$BF2) # Child still been breastfed ==1
attributes(ch5$BF3) # Child drank plain water yesterday ==2
attributes(ch5$BF4) # Child drank infant formula yesterday ==2
attributes(ch5$BF6) # Child drank infant formula yesterday ==2
attributes(ch5$BF8) # Child drank juice or juice drinks yesterday ==2
attributes(ch5$BF9) # Child drank broth/soup yesterday ==2
attributes(ch5$BF12) # Child drank any other liquid yesterday ==2
attributes(ch5$BF13) # Child drank or ate yogurt yesterday ==2
attributes(ch5$BF15) # Child ate thin porridge yesterday ==2
attributes(ch5$BF16) # Child ate solid or semi-solid food yesterday ==2
ch5$exclusive_breastfeeding <- ifelse(ch5$CAGE<6 & ch5$BF1==1 & ch5$BF2==1 & ch5$BF3==2 & ch5$BF4==2 & ch5$BF6==2 & ch5$BF8==2 & 
                                        ch5$BF9==2 & ch5$BF12==2 & ch5$BF13==2 & ch5$BF15==2 & ch5$BF16==2, 1,
                                      ifelse(ch5$CAGE<6, 0, NA))

# Diarrhorea treatment via ORT
attributes(ch5$CA1) # Child had diarrhoea in last 2 weeks ==1
attributes(ch5$CA2) # Child drank less or more during illness ==4
attributes(ch5$CA4A) # Drank fluid made from special packet (ORS) ==1
attributes(ch5$CA4B) # Rice-based ORS packet for diarrhoea ==1
attributes(ch5$CA4C) # Sugar and salt solution for diarrhoea ==1
attributes(ch5$CA4D) # Green coconut water for diarrhoea ==1
attributes(ch5$CA4E) # Rice water for diarrhoea ==1
attributes(ch5$CA4F) # Boiled rice water for diarrhoea ==1
ch5$diarrhoea_ors <- ifelse(ch5$CA1==1 & (ch5$CA2==4 | ch5$CA4A==1 | ch5$CA4B==1 | ch5$CA4C==1 | ch5$CA4D==1 | 
                                    ch5$CA4E==1 | ch5$CA4F==1), 1,
                           ifelse(ch5$CA1==1, 0, NA))

# Care-seeking for children with ARI symptoms
attributes(ch5$CA7) # Child ill with cough in last 2 weeks ==1
attributes(ch5$CA8) # Difficulty breathing during illness with cough & ==1
attributes(ch5$CA9) # Symptoms due to problem in chest or blocked nose & ==1 | 3
ch5$ari_care <- ifelse((ch5$CA7==1 & ch5$CA8==1 & (ch5$CA9==1 | ch5$CA9==3)) & 
                        (ch5$CA11A=="A" | ch5$CA11B=="B" | ch5$CA11C=="C" | ch5$CA11D=="D" | 
                         ch5$CA11E=="E" | ch5$CA11H=="H" | ch5$CA11I=="I" | ch5$CA11J=="J" | 
                         ch5$CA11L=="L" | ch5$CA11O=="O"), 1, 
                      ifelse(ch5$CA7==1 & ch5$CA8==1 & (ch5$CA9==1 | ch5$CA9==3), 0, NA))

# Antibiotics treatment for ARI symptoms
attributes(ch5$CA12) # Given medicine to treat this illness ==1
attributes(ch5$CA13A) # Medicine: Antibiotic pill / syrup =="A"
attributes(ch5$CA13B) # Medicine: Antibiotic injection ==B
ch5$ari_antibiotics <- ifelse((ch5$CA7==1 & ch5$CA8==1 & (ch5$CA9==1 | ch5$CA9==3)) & 
                               (ch5$CA13A=="A" | ch5$CA13B=="B"), 1, 
                             ifelse(ch5$CA7==1 & ch5$CA8==1 & (ch5$CA9==1 | ch5$CA9==3), 0, NA))

# Contraceptive prevalence
attributes(wm5$MA1) # Currently married ==1
attributes(wm5$CP2) # Currently using a method to avoid pregnancy ==1
wm5$contraception <- ifelse(wm5$MA1==1 & wm5$CP2==1, 1,
                            ifelse(wm5$MA1==1, 0, NA))

# Antenatal care coverage
attributes(wm5$CM13) # Last birth in last two years =="Y"
attributes(wm5$MN1) # Received antenatal care
attributes(wm5$MN3) # Times received antenatal care >=4 !=98 !=99
attributes(wm5$MN2A) # Antenatal care: Doctor
attributes(wm5$MN2B) # Antenatal care: Nurse / Midwife
attributes(wm5$MN2C) # Antenatal care: Auxiliary midwife
attributes(wm5$MN2F) # Antenatal care: Traditional birth attendant
attributes(wm5$MN2G) # Antenatal care: Community health worker
attributes(wm5$MN2X) # Antenatal care: Other
wm5$antenatal <- ifelse(wm5$CM13=="Y" & wm5$MN1==1 & (wm5$MN3>=4 & wm5$MN3!=98 & wm5$MN3!=99) &
                        (wm5$MN2A=="A"| wm5$MN2B=="B"| wm5$MN2C=="C"| wm5$MN2F=="F"| wm5$MN2G=="G"| wm5$MN2X=="X"), 1,
                        ifelse(wm5$CM13=="Y", 0, NA))

# Skilled attendant at delivery
attributes(wm5$CM13) # Last birth in last two years =="Y"
attributes(wm5$MN17A) # Assistance at delivery: Doctor
attributes(wm5$MN17B) # Assistance at delivery: Nurse / Midwife
attributes(wm5$MN17C) # Assistance at delivery: Auxiliary midwife
wm5$attendant_delivery <- ifelse(wm5$CM13=="Y" & (wm5$MN17A=="A" | wm5$MN17B=="B" | wm5$MN17C=="C"), 1, 
                                ifelse(wm5$CM13=="Y", 0, NA))

# Institutional deliveries
attributes(wm5$CM13) # Last birth in last two years =="Y"
attributes(wm5$MN18) # Place of delivery ==21, 22, 23, 26, 31, 32, 33, 36
wm5$institutional_delivery <- ifelse(wm5$CM13=="Y" & (wm5$MN18==21 | wm5$MN18==22 | wm5$MN18==23 | wm5$MN18==26 |
                                                       wm5$MN18==31 | wm5$MN18==32 | wm5$MN18==33 | wm5$MN18==36), 1, 
                                ifelse(wm5$CM13=="Y", 0, NA))

# C-section
attributes(wm5$CM13) # Last birth in last two years =="Y"
attributes(wm5$MN19) # Delivery by caesarean section ==1
wm5$caesarean <- ifelse(wm5$CM13=="Y" & wm5$MN19==1, 1,
                       ifelse(wm5$CM13=="Y", 0, NA))

#----------------------------------------------------------------------------------------------------------
# Create final dataset
#----------------------------------------------------------------------------------------------------------

final1 <- ddply(ch5, ~HH7A, summarise, # Summarize by district
                prop_underweight=weighted.mean(underweight, chweight, na.rm=TRUE),
                prop_stuning=weighted.mean(stuning, chweight, na.rm=TRUE),
                prop_wasting=weighted.mean(wasting, chweight, na.rm=TRUE),
                prop_overweight=weighted.mean(overweight, chweight, na.rm=TRUE),
                prop_exclusive_breastfeeding=weighted.mean(exclusive_breastfeeding, chweight, na.rm=TRUE),
                prop_diarrhoea_ors=weighted.mean(diarrhoea_ors, chweight, na.rm=TRUE),
                prop_ari_care=weighted.mean(ari_care, chweight, na.rm=TRUE),
                prop_ari_antibiotics=weighted.mean(ari_antibiotics, chweight, na.rm=TRUE))

final2 <- ddply(wm5, ~HH7A, summarise, # Summarize by district
                prop_contraception=weighted.mean(contraception, wmweight, na.rm=TRUE),
                prop_antenatal=weighted.mean(antenatal, wmweight, na.rm=TRUE),
                prop_attendant_delivery=weighted.mean(attendant_delivery, wmweight, na.rm=TRUE),
                prop_institutional_delivery=weighted.mean(institutional_delivery, wmweight, na.rm=TRUE),
                prop_caesarean=weighted.mean(caesarean, wmweight, na.rm=TRUE))

final <- merge(final1, final2)
final$year <- 2012
names(final)[1] <- "district"
final$district <- get_labels(wm5$HH7A)

#----------------------------------------------------------------------------------------------------------
# Save final dataset/metadata
#----------------------------------------------------------------------------------------------------------

write.csv(final,"./output/unicef/data/data_unicef_mics5_2012.csv", row.names=FALSE) # Save metadata

meta_mics5 <- data.frame("Source"="MICS", "File"= "MICS5","Variable"=colnames(final))
write.csv(meta_mics5,"./output/unicef/data/metadata_unicef_mics5.csv", row.names=FALSE) # Save metadata

# Save variables as proportions
final[c(2:14)] <- final[c(2:14)]*100
write.csv(final,"./output/unicef/data/data_unicef_mics5_2012_clean.csv", row.names=FALSE) # Save metadata

#----------------------------------------------------------------------------------------------------------
# Create/save metadata (raw data)
#----------------------------------------------------------------------------------------------------------

meta_mics5 <- data.frame()
for (i in seq_along(mics5)){
  meta <- data.frame("Source"="MICS5", "File"= names(mics5[i]),"Variable"=colnames(mics5[[i]]), 
                     "Description"= sapply(mics5[[i]], function(x) attributes(x)$label))
  meta_mics5 <- rbind(meta_mics5,meta)
}
write.csv(meta_mics5,"./data/unicef/MICS5/metadata_unicef_mics5.csv", row.names=FALSE) # Save metadata
