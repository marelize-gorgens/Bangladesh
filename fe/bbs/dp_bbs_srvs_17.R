#----------------------------------------------------------------------------------------------------------
# dp_bbs_srvs_.R
# Description: Data preprocessing of SRVS datasets
# Organization: Health, Nutrition, and Population (HNP) | The World Bank
# Author: Hellen Matarazzo
# Date: Updated on 03-25-2019
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
if(!require(haven)) installed.packages("haven", dependencies=TRUE); library(haven)
if(!require(rdhs)) installed.packages("rdhs"); library(rdhs)
if(!require(plyr)) installed.packages("plyr", dependencies=TRUE); library(plyr)

#----------------------------------------------------------------------------------------------------------
# Load datasets
#----------------------------------------------------------------------------------------------------------

svrs17files <- list.files("C:/Users/wb531612/Documents/HNP/Bangladesh/data/bbs/svrs/SVRS_17/", recursive=TRUE, pattern="*.dta", full.names=TRUE) # List files
svrs17 <- sapply(svrs17files[c(2,3,8,10,11)], read_dta)
names(svrs17) <- c("tafsil-3", "tafsil-4","tafsil-9","tafsil-2h","tafsil-2p") # Rename lists

# Section 2: Household roster and demographic characteristics ---------------------------------------------
tafsil2h <- svrs17$'tafsil-2h' # Load as data frame
tafsil2h_var <- get_variable_labels(tafsil2h) # Get the data dictionary

tafsil2p <- svrs17$'tafsil-2p'
tafsil2p_var <- get_variable_labels(tafsil2p)

# Section 3: Birth ----------------------------------------------------------------------------------------
tafsil3 <- svrs17$'tafsil-3'
tafsil3_var <- get_variable_labels(tafsil3)

# Section 4: Deaths ---------------------------------------------------------------------------------------
tafsil4 <- svrs17$'tafsil-4'
tafsil4_var <- get_variable_labels(tafsil4)

# Section 9: Use of Contraceptives ------------------------------------------------------------------------
tafsil9 <- svrs17$'tafsil-9'
tafsil9_var <- get_variable_labels(tafsil9)

#----------------------------------------------------------------------------------------------------------
# List of variables of interest
#----------------------------------------------------------------------------------------------------------

# Section 2: Household roster and demographic characteristics
# Section 3: Birth 
# Section 4: Death 
# Section 9: Use of Contraceptives 

# Section 2: Household roster and demographic characteristics ---------------------------------------------
# "tafsil-2H" 
zila # Zila Code
upza # Upazila Code
rmo # Rural Urban Code
hh_no # Household Number
q2_1 # Sources of Drinking Water
q_4 # Sources of light
q_5 # Sources of Fuel
q_6 # Toilet Facilities

# "tafsil-2P" 
zila # Zila Code
upza # Upazila Code
rmo # Rural Urban Code
q_10 # Age
q_11 # Sex | Men==1
q_14 # Marital Status
q_16 # Level of Education
q_19 # Literacy
q_20 # Education
hhsize # RECODE of tot_pop
agecat1 # RECODE of q_10
agecat2 # RECODE of q_10
resi # RECODE of rmo
religion # RECODE of q_12
divn # RECODE of psu_no
sex # RECODE of q_11
agecatLT # RECODE of q_10

# Section 3: Birth ----------------------------------------------------------------------------------------
# "tafsil-3" 
zila # Zila Code
upza # Upazila Code
rmo # Rural Urban Code
q_4 # Birth Registration Yes/No (within 45 days of the birth) | Yes==1
q_7 # Birth Attendent
      # 1. Trained Doctor, 
      # 2. Nurse, Midwife, paramedic, Family welfare visitors (FVW), 
      # 3. Medical Assistant (MA)/ Sun assistant community medical officer (SACMO)
      # 4. Health Assistant (HA), Family welfare asssntant (FWA)
      # 5. Untrained treaditioanl birth attandent 
      # 6. Untrained village doctor/ QUACK
      # 7. Neighbour/Relatives
      # 9. Others
q_9 # Live Birth | Yes==1
q_12 # Mothers Age (in full years)

# Section 4: Deaths ---------------------------------------------------------------------------------------
q_3y # Age
q_5 # Causes of Death

# Section 9: Use of Contraceptives ------------------------------------------------------------------------
# "tafsil-9" 
zila # Zila Code
upza # Upazila Code
rmo # Rural Urban Code
i4 # Current age (in full years)
i5 # Education
      # 1.	Did not passed 1st grade 
      # 2.	Passed 1st grade 
      # 3.	Passed 2nd grade 
      # 4.	Passed 3rd grade 
      # 5.	Passed 4th grade 
      # 6.	Passed 5th grade 
      # 7.	Passed 6th grade 
      # 8.	Passed 7th grade 
      # 9.	Passed 8th grade 
      # 10.	 Passed 9th grade 
      # 11.	 Secondery or equal 
      # 12.	 Higher secondary or equal
      # 13.	 Undergraduate or equal 
      # 14.	 Graduate or equal 
      # 15.	 Doctor/Engineer/Agriculturist 
      # 16.	 Diploma
      # 17.	 Vocational 
      # 18.	 Others 
i14 # Currently use contrceptiev techniques | Yes==1

#----------------------------------------------------------------------------------------------------------
# Data transformation
#----------------------------------------------------------------------------------------------------------

# Births
births <- ddply(tafsil3, ~zila, summarise, # Summarize by district
                no_births=length(q_9),
                no_live_births=sum(q_9==1),
                no_registered_births=sum(q_4==1),
                prop_registered_births=sum(q_4==1)/length(q_9),
                prop_attendant_delivery=sum(q_7 %in% c(1,2,3))/length(q_9))

# Deaths
deaths <- ddply(tafsil4, ~zila, summarise, # Summarize by district
                no_deaths=length(q_2),
                no_deaths_rural=sum(resi==1),
                no_deaths_urban=sum(resi==2),
                prop_deaths_rural=sum(resi==1)/length(q_2),
                no_deaths_under5=sum(q_3y<5),
                no_deaths_under1=sum(q_3y<1),
                rate_maternal_mortality=sum(q_5 %in% c(37,38,39,40,41,42,43))/length(q_2))
                
# Demographic
demo <- ddply(tafsil2p, ~zila, summarise,
              total_pop=sum(tot_pop),
              prop_pop_rural=sum(resi==1)/sum(tot_pop),
              prop_pop_women=sum(q_11==2)/sum(tot_pop),
              women_15-19=,
              women_15-45=,
              women_15-49=,
              men_>15=,
              women_15-45_men_>15=
              pop_>15=,
              pop_15-19=,
              child_0-6m=,
              child_0-11m=,
              child_0-59m=,
              child_6-23m=,
              child_59m=,
              child_12-23m=,
              child_15-18m=,
              child_under5=,)

svrs17final <- merge(births, deaths, all=TRUE)
svrs17final1 <- merge(svrs17final, demo, all=TRUE)

#----------------------------------------------------------------------------------------------------------
# Save final dataset
#----------------------------------------------------------------------------------------------------------

# By District
write.csv(svrs17final,"./output/bbs/data/data_svrs_2017.csv", row.names=FALSE) # Save data

#----------------------------------------------------------------------------------------------------------
# Create/save metadata
#----------------------------------------------------------------------------------------------------------

meta_svrs17 <- data.frame()
for (i in seq_along(svrs17)){
  meta <- data.frame("Source"="SRVS_17", "File"= names(svrs17[i]),get_variable_labels(svrs17[[i]]))
  meta_svrs17 <- rbind(meta_svrs17,meta)
}

write.csv(meta_svrs17,"./data/bbs/svrs/SVRS_17/metadata_bbs_SRVS_17.csv", row.names=FALSE) # Save metadata

#----------------------------------------------------------------------------------------------------------
# Merge/save metadata from 2017, 2014 and 2013
#----------------------------------------------------------------------------------------------------------

svrs_17_14 <- merge(meta_svrs17, meta_svrs14, by="variable", all=TRUE)
svrs_17_14_13 <- merge(svrs_17_14, meta_svrs13, by="variable", all=TRUE)

write.csv(svrs_17_14_13,"./data/bbs/svrs/metadata_bbs_SRVS_merged_17_14_13.csv", row.names=FALSE) # Save metadata
