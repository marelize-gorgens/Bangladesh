#----------------------------------------------------------------------------------------------------------
# dp_unicef_.R
# Description: Data preprocessing of Unicef MICS5 datasets
# Organization: Health, Nutrition, and Population (HNP) | The World Bank
# Author: Hellen Matarazzo
# Date: Updated on 03-20-2019
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

# Household dataset
hh5 <- mics5$hh5 # Load as data frame
hh5_var <- get_variable_labels(hh5) # Get the data dictionary

# Household members dataset
hl5 <- mics5$hl5 # Load as data frame
hl5_var <- get_variable_labels(hl5) # Get the data dictionary

#----------------------------------------------------------------------------------------------------------
# Survey design
#----------------------------------------------------------------------------------------------------------

ch5design <- svydesign(data=ch5, id=~HH1, weight=~chweight, strata=~HH7A) # Set survey design for dataset
sum(duplicated(ch5)) # Uniqueness: Check for duplicates
sum(is.na(ch5)) # Completeness: Check for NAs
na_ch5 <- data.frame(lapply(ch5, function(y) sum(length(which(is.na(y)))))) # Save NAs by column
attach(ch5)

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
ch5$underweight <- ifelse(ch5$WAZ2==99.97 | ch5$WAZ2==99.98 | ch5$WAZ2==99.99, NA,
                          ifelse(ch5$WAZ2<(-2)*mad(ch5$WAZ2[ch5$WAZ2<=5]),1,0))

# Stuning prevalence 
attributes(ch5$HAZ2) # Height for age
ch5$stuning <- ifelse(ch5$HAZ2==99.97 | ch5$HAZ2==99.98 | ch5$HAZ2==99.99, NA,
                          ifelse(ch5$HAZ2<(-2)*mad(ch5$HAZ2[ch5$HAZ2<=5]),1,0))

# Wasting prevalence
attributes(ch5$WHZ2) # Weight for height
ch5$wasting <- ifelse(ch5$WHZ2==99.97 | ch5$WHZ2==99.98 | ch5$WHZ2==99.99, NA,
                         ifelse(ch5$WHZ2<(-2)*mad(ch5$WHZ2[ch5$WHZ2<=5]),1,0))

# Overweight prevalence
ch5$overweight <- ifelse(ch5$WHZ2==99.97 | ch5$WHZ2==99.98 | ch5$WHZ2==99.99, NA,
                          ifelse(ch5$WHZ2>2*mad(ch5$WHZ2[ch5$WHZ2<=5]),1,0))

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
attributes(ch5$BF18) # Child drank anything else from the bottle with a nipple yesterday ==2
ch5$exclusive_breastfeeding <- ifelse(ch5$CAGE>=6, NA,
                                      ifelse(ch5$BF1==1 & ch5$BF2==1 & ch5$BF3==2 & ch5$BF4==2 & 
                                               ch5$BF6==2 & ch5$BF8==2 & ch5$BF9==2 & ch5$BF12==2 & 
                                               ch5$BF13==2 & ch5$BF15==2 & ch5$BF16==2 & ch5$BF18==2, 1, 0))

# Diarrhorea treatment via ORT
attributes(ch5$CA1) # Child had diarrhoea in last 2 weeks ==1
attributes(ch5$CA2) # Child drank less or more during illness ==4
attributes(ch5$CA4A) # Drank fluid made from special packet (ORS) ==1
attributes(ch5$CA4B) # Rice-based ORS packet for diarrhoea ==1
attributes(ch5$CA4C) # Sugar and salt solution for diarrhoea ==1
attributes(ch5$CA4D) # Green coconut water for diarrhoea ==1
attributes(ch5$CA4E) # Rice water for diarrhoea ==1
attributes(ch5$CA4F) # Boiled rice water for diarrhoea ==1
ch5$diarrhoeaors <- ifelse(ch5$CA1!=1, NA,
                           ifelse(ch5$CA2==4 | ch5$CA4A==1 | ch5$CA4B==1 | ch5$CA4C==1 | ch5$CA4D==1 | 
                                                  ch5$CA4E==1 | ch5$CA4F==1,1,0))

# Care-seeking for children with ARI symptoms
attributes(ch5$CA7) # Child ill with cough in last 2 weeks ==1
attributes(ch5$CA8) # Difficulty breathing during illness with cough & ==1
attributes(ch5$CA9) # Symptoms due to problem in chest or blocked nose & ==1 | 3
ch5$aricare <- ifelse((ch5$CA7==1 & ch5$CA8==1 & (ch5$CA9==1 | ch5$CA9==3)) & 
                        (ch5$CA11A=="A" | ch5$CA11B=="B" | ch5$CA11C=="C" | ch5$CA11D=="D" | 
                         ch5$CA11E=="E" | ch5$CA11H=="H" | ch5$CA11I=="I" | ch5$CA11J=="J" | 
                         ch5$CA11L=="L" | ch5$CA11O=="O"), 1, 
                      ifelse(ch5$CA7==1 & ch5$CA8==1 & (ch5$CA9==1 | ch5$CA9==3), 0, NA))

# Antibiotics treatment for ARI symptoms
attributes(ch5$CA12) # Given medicine to treat this illness ==1
attributes(ch5$CA13A) # Medicine: Antibiotic pill / syrup =="A"
attributes(ch5$CA13B) # Medicine: Antibiotic injection ==B
ch5$ariantibiotics <- ifelse((ch5$CA7==1 & ch5$CA8==1 & (ch5$CA9==1 | ch5$CA9==3)) & 
                               (ch5$CA13A=="A" | ch5$CA13B=="B"), 1, 
                             ifelse(ch5$CA7==1 & ch5$CA8==1 & (ch5$CA9==1 | ch5$CA9==3), 0, NA))

# Adolescent birth rate

# Contraceptive prevalence
attributes(wm5$MA1) # Currently married ==1

# Antenatal care coverage
attributes(wm5$CM13) # Last birth in last two years =="Y"

# Skilled attendant at delivery

# Institutional deliveries

# C-section


#----------------------------------------------------------------------------------------------------------
# Create final dataset
#----------------------------------------------------------------------------------------------------------
final <- ddply(ch5, ~HH7A, summarise, # Summarize by district
                   underweight=weighted.mean(underweight, chweight, na.rm=TRUE),
                   stuning=weighted.mean(stuning, chweight, na.rm=TRUE),
                   wasting=weighted.mean(wasting, chweight, na.rm=TRUE),
                   overweight=weighted.mean(overweight, chweight, na.rm=TRUE),
                   exclusive_breastfeeding=weighted.mean(exclusive_breastfeeding, chweight, na.rm=TRUE),
                   diarrhoeaors=weighted.mean(diarrhoeaors, chweight, na.rm=TRUE),
                   aricare=weighted.mean(aricare, chweight, na.rm=TRUE),
                   ariantibiotics=weighted.mean(ariantibiotics, chweight, na.rm=TRUE))
# weighted.mean(ch5$ariantibiotics, ch5$chweight, na.rm=TRUE)
# mean(ch5$underweight, na.rm=TRUE)
# svymean(~underweight, design=ch5design, na.rm=T)
# nrow(ch5[ch5$underweight == 1,])
# length(ch5$underweight[ch5$underweight == 1])
# sum(which(ch5$underweight == 1))
# sum(is.na(ch5$ariantibiotics))
# table(ch5$ariantibiotics)

#----------------------------------------------------------------------------------------------------------
# Save final dataset
#----------------------------------------------------------------------------------------------------------

write.csv(final,"./output/unicef/data/data_unicef_mics5_2012.csv", row.names=FALSE) # Save metadata
# write.csv(ch5,"C:/Users/wb531612/Desktop/ch5.csv", row.names=FALSE) # Save dataset

#----------------------------------------------------------------------------------------------------------
# Create/save metadata
#----------------------------------------------------------------------------------------------------------

meta_mics5 <- data.frame()
for (i in seq_along(mics5)){
  meta <- data.frame("Source"="MICS5", "File"= names(mics5[i]),"Variable"=colnames(mics5[[i]]), 
                     "Description"= sapply(mics5[[i]], function(x) attributes(x)$label))
  meta_mics5 <- rbind(meta_mics5,meta)
}
write.csv(meta_mics5,"./output/unicef/data/metadata_unicef_mics5.csv", row.names=FALSE) # Save metadata
