#----------------------------------------------------------------------------------------------------------
# dp_unicef_.R
# Description: Data preprocessing of Unicef CES datasets
# Organization: Health, Nutrition, and Population (HNP) | The World Bank
# Author: Hellen Matarazzo
# Date: Updated on 03-05-2019
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
options(java.parameters = "-Xmx2048m")
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre8') # for 64-bit version
if(!require(rJava)) install.packages("rJava"); library(rJava)
if(!require(tabulizerjars)) install.packages("tabulizerjars", dependencies = TRUE); library(tabulizerjars)
if(!require(tabulizer)) install.packages("tabulizer", dependencies = TRUE); library(tabulizer)
#if(!require(remotes)) install.packages("remotes", dependencies = TRUE); library(remotes)
#remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch") # on 64-bit Windows
if(!require(pdftools)) installed.packages("pdftools", dependencies=TRUE); library(pdftools)
if(!require(hablar)) install.packages("hablar", dependencies=TRUE); library(hablar)

#----------------------------------------------------------------------------------------------------------
# Load & transform CES datasets
#----------------------------------------------------------------------------------------------------------

list.files("./data/unicef/CES", recursive=T) # List all files on folder 'CEF' and its subfolders
temp <- list.files("./data/unicef/CES", recursive=T, pattern="*.pdf", full.names=TRUE) # List *.sav files

# 2006 ----------------------------------------------------------------------------------------------------

# area2006a <- locate_areas("./data/unicef/CES/EPI Coverage Evaluation Survey 2006.pdf", pages=191) # Find area
area2006a <- list(c(127.89474, 63.47367, 745.57895, 544.73685)) # Set area
ces2006a <- extract_tables(temp[2], pages=190:191, area = area2006a, output = "data.frame", guess=FALSE) # Extract data
ces2006a[[1]] <- ces2006a[[1]][-c(1),] # Drop messy row
ces2006a[[2]] <- ces2006a[[2]][-c(1),]
df1 <- bind_rows(ces2006a) # Combine lists
str(df1)
df1$District
df1$District <- gsub('Division', 'Division Total', df1$District) # Replace text to match others dataset
df1$Fully <- as.numeric(df1$Fully)
colnames(df1)[1] <- "District" # Rename variable

# area2006b <- locate_areas("./data/unicef/CES/EPI Coverage Evaluation Survey 2006.pdf", pages=204) # Find area
area2006b <- list(c(119.75604, 63.46886, 739.66963, 553.56291)) # Set area
ces2006b <- extract_tables(temp[2], pages=203:214, area = area2006b, output = "data.frame", guess=FALSE) # Extract data
df2 <- bind_rows(ces2006b[[1]],ces2006b[[2]]) # Combine list's elements
str(df2)
df2$District
df2$District[df2$District=="Habigonj"] <- "Habiganj" # Replace text to match others dataset
colnames(df2) <- c("District", # Rename variables
                   "TT1_Woman15-49Y",
                   "TT2_Woman15-49Y",
                   "TT3_Woman15-49Y",
                   "TT4_Woman15-49Y",
                   "TT5_Woman15-49Y")

df3 <- bind_rows(ces2006b[[5]],ces2006b[[6]]) # Combine list's elements
str(df3)
df3$District
grep("Division", df3$District)
df3$District[df3$District=="Chittagong Division"] <- "Chittagong Division Total" # Replace text to match others dataset
colnames(df3) <- c("District", # Rename variables
                   "TT1_Woman18-25Y",
                   "TT2_Woman18-25Y",
                   "TT3_Woman18-25Y",
                   "TT4_Woman18-25Y",
                   "TT5_Woman18-25Y")

df4 <- bind_rows(ces2006b[[9]],ces2006b[[10]]) # Combine list's elements
str(df4)
colnames(df4) <- c("District", # Rename variables
                   "MeaslesCoverage_Children9M-9Y")

df5 <- bind_rows(ces2006b[[11]],ces2006b[[12]]) # Combine list's elements
str(df5)
colnames(df5) <- c("District", # Rename variables
                   "OPVCoverage_1stRound_Children59M",
                   "OPVCoverage_2stRound_Children59M",
                   "OPVCoverage_3stRound_Children59M",
                   "OPVCoverage_All_Children59M")

# area2006c <- locate_areas("./data/unicef/CES/EPI Coverage Evaluation Survey 2006.pdf", pages=216) # Find area 
area2006c <- list(c(145, 66.48792, 744.70140, 556.58197)) # Set area
ces2006c <- extract_tables(temp[2], pages=215:216, area = area2006c, output = "data.frame", guess=FALSE) # Extract data
ces2006c[[1]] <- ces2006c[[1]][-c(1:2),] # Drop messy row
ces2006c[[1]] <- ces2006c[[1]] %>% # Separate values in two columns
  separate(Vitamin.A.Supplementation.Coverage, sep=" ", into=c("a","b"))
colnames(ces2006c[[1]]) <- colnames(ces2006c[[2]]) <- c("District", # Set up column names
                             "VitASupplementationCoverage_PostPartum", 
                             "VitASupplementationCoverage_Children12-59Months",
                             "VitASupplementationCoverage_Infants9-11Months",
                             "AnthelminticCoverage_Children24-59Months")
str(ces2006c)
ces2006c[[1]] <- ces2006c[[1]] %>% retype() # Change type of variable
df6 <- bind_rows(ces2006c) # Combine list's elements
str(df6)

# Final dataset
final <- merge(df1, df2, by="District", all=TRUE)
final1 <- merge(final, df3, by="District", all=TRUE)
final2 <- merge(final1, df4, by="District", all=TRUE)
final3 <- merge(final2, df5, by="District", all=TRUE)
ces2006 <- merge(final3, df6, by="District", all=TRUE)

# Data quality assessment
sum(is.na(ces2006)) # Completeness: Check for NAs
na_ces2006 <- data.frame(lapply(ces2006, function(y) sum(length(which(is.na(y)))))) # Show NAs by column

sum(duplicated(ces2006)) # Uniqueness: Check for duplicates

str(ces2006) # Validity: Check format and type

max(ces2006[,c(2:29)], na.rm=TRUE) # Validity: Check range (between 0 and 100)
max_ces2006 <- data.frame(lapply(ces2006, function(z) max((z), na.rm=TRUE))) # Show max by column

# Save final dataset
write.csv(ces2006,"./output/unicef/data/data_unicef_ces_2006.csv", row.names=FALSE) # Save data


# 2010 ----------------------------------------------------------------------------------------------------

# area2010a <- locate_areas("./data/unicef/CES/EPI Coverage Evaluation Survey 2010.pdf", pages=236) # Find area
area2010a <- list(c(108.68619, 82.47512, 743.69533, 582.63290)) # Set area
ces2010a <- extract_tables(temp[4], pages=236:268, area = area2010a, output = "data.frame", guess=FALSE) # Extract data
colnames(ces2010a[[1]]) <- colnames(ces2010a[[2]]) <- c("Survey.Units", # Set up column names
                                                        "BCG_Children12M",
                                                        "OPV1_Children12M",
                                                        "DPT1_Children12M",
                                                        "HB1_Children12M",
                                                        "OPV2_Children12M",
                                                        "DPT2_Children12M",
                                                        "HB2_Children12M",
                                                        "OPV3_Children12M",
                                                        "DPT3_Children12M",
                                                        "HB3_Children12M",
                                                        "Measles_Children12M",
                                                        "Fully_Children12M")
d1 <- bind_rows(ces2010a[[1]],ces2010a[[2]]) 
d1 <- d1[-c(37),] # Drop messy row
d1[1]
d1[14,1] <- "Moulavibazar"
d1[21,1] <- "Shariatpur"
d1[38,1] <- "Chapai Nawabganj"
d1[36,1] <- "Mymensingh"
d1[37,1] <- "Panchagarh"

colnames(ces2010a[[5]]) <- colnames(ces2010a[[6]]) <- c("Survey.Units", # Set up column names
                                                        "BCG_Children23M",
                                                        "OPV1_Children23M",
                                                        "DPT1_Children23M",
                                                        "HB1_Children23M",
                                                        "OPV2_Children23M",
                                                        "DPT2_Children23M",
                                                        "HB2_Children23M",
                                                        "OPV3_Children23M",
                                                        "DPT3_Children23M",
                                                        "HB3_Children23M",
                                                        "Measles_Children23M",
                                                        "Fully_Children23M")
ces2010a[[5]][c(2:13)] <- as.numeric(unlist(ces2010a[[5]][c(2:13)])) # Change type of variable
d2 <- bind_rows(ces2010a[[5]],ces2010a[[6]]) 
d2[38,1] <- "Joypurhat"
d2[52,1] <- "Rajshahi Division"
d2[56,1] <- "Sunamganj"
d2[74,1] <- "Chandpur"
d2 <- d2[-c(37,51,55,58,60,63,73,87),] # Drop messy row
d2[59,1] <- "Barguna"
d2[64,1] <- "Barisal Division"
d2[3,12] <- 93.4
d2[10,12] <- 86.3
d2[14,5] <- 100.0
d2[17,13] <- 83.5
d2[25,2] <- 99.5
d2[28,10] <- 90.3
d2[32,5] <- 99.3
d2[39,3] <- 100.0
d2[42,11] <- 90.3

ces2010a[[18]][13,1] <- "Mymensingh"
ces2010a[[18]][18,2] <- 96.2
ces2010a[[18]][2] <- as.numeric(unlist(ces2010a[[18]][2])) # Change type of variable
ces2010a[[19]][7,1] <- "Chittagong.Division 94.4"
ces2010a[[19]][24,1] <- "Ctg.Slum 99.5"
ces2010a[[19]][25,1] <- "Rangpur.Division 98"
ces2010a[[19]][27,1] <- "Gaibandha 92.9"
ces2010a[[19]][21,1] <- ces2010a[[19]][20,1]
ces2010a[[19]][25,4] <- 39.8
ces2010a[[19]][29,4] <- 64.8
ces2010a[[19]] <- ces2010a[[19]] %>% # Separate values in two columns
  separate(Survey.Units.TT1, sep=" ", into=c("Survey.Units","TT1"))
ces2010a[[19]][c(2,5)] <- as.numeric(unlist(ces2010a[[19]][c(2,5)])) # Change type of variable
d3 <- bind_rows(ces2010a[[18]],ces2010a[[19]])
d3 <- d3[-c(12,51,58,64,71,73,74,86, 92:137),] # Drop messy row
d3$Survey.Units
d3[22,1] <- "Khulna Division"
d3[24,1] <- "Chuadanga"
d3[34,1] <- "Rajshahi Division"
d3[36,1] <- "Chapai Nawabganj"
d3[37,1] <- "Joypurhat"
d3[44,1] <- "Sylhet Division"
d3[46,1] <- "Moulavibazar"
d3[47,1] <- "Sunamganj"
d3[58,1] <- "Chittagong Division"
d3[71,1] <- "Ctg Slum"
d3[72,1] <- "Rangpur Division"
colnames(d3) <- c("Survey.Units", # Rename variables
                  "TT1_Mother0-11MChildren",
                  "TT2_Mother0-11MChildren",
                  "TT3_Mother0-11MChildren",
                  "TT4_Mother0-11MChildren",
                  "TT5_Mother0-11MChildren")

ces2010a[[23]][c(3,6)] <- as.numeric(unlist(ces2010a[[23]][c(3,6)])) # Change type of variable
ces2010a[[23]][3,6] <- 23.8
ces2010a[[23]][11,3] <- 74.3
d4 <- bind_rows(ces2010a[[22]],ces2010a[[23]])
d4 <- d4[-c(30,34),] # Drop messy row
colnames(d4) <- c("Survey.Units", # Rename variables
                  "TT1_Woman18-49Y",
                  "TT2_Woman18-49Y",
                  "TT3_Woman18-49Y",
                  "TT4_Woman18-49Y",
                  "TT5_Woman18-49Y")
d4[33,1] <- "Rajshahi Division"
d4[57,1] <- "Chittagong Division"
d4[66,1] <- "Lakshmipur"
d4[70,1] <- "Chittagong Slum"

ces2010a[[26]][48,] <- c("National", 91.9)
ces2010a[[27]][33,] <- c("SCC", 87.6)
colnames(ces2010a[[26]]) <- colnames(ces2010a[[27]]) <- c("Survey.Units", # Set up column names
                                                        "PAB_Mother0-11MChildren")
d5 <- bind_rows(ces2010a[[26]],ces2010a[[27]])
d5[58,1] <- "Chittagong Division"
d5 <- d5[-c(50),] # Drop messy row
d5[2] <- as.numeric(unlist(d5[2]))
d5[33,1] <- "Rajshahi Division"
d5[41,1] <- "Serajganj"

ces2010a[[28]]
ces2010a[[29]]
ces2010a[[28]][c(3)] <- as.numeric(unlist(ces2010a[[28]][c(3)])) # Change type of variable
ces2010a[[29]][c(3)] <- as.numeric(unlist(ces2010a[[29]][c(3)])) # Change type of variable
colnames(ces2010a[[28]]) <- colnames(ces2010a[[29]]) <- c("Survey.Units", # Set up column names
                                                          "OPVCoverage_18NID_Children0-5M",
                                                          "OPVCoverage_MeaslesCampaign_Children0-5M",
                                                          "OPVCoverage_Both_Children0-5M")
d6 <- bind_rows(ces2010a[[28]],ces2010a[[29]])
d6[69,1] <- "Chittagong Division"
d6[66,1] <- "Patuakhali"
d6[35,1] <- "Rajshahi New Division"
d6 <- d6[-c(1,44),]# Drop messy row
d6[34,1] <- "Rajshahi Division"
d6[39,1] <- "Nartore"

ces2010a[[30]] <- ces2010a[[30]] %>% # Separate values in two columns
  separate(Vitamin.A.Supplementation.Coverage, sep=" ", into=c("a","b"))
ces2010a[[30]][c(2:5)] <- as.numeric(unlist(ces2010a[[30]][c(2:5)])) # Change type of variable
ces2010a[[30]][5,3] <- 97.1
ces2010a[[30]][32,3] <- 97.6
ces2010a[[30]][37,3] <- 95.7
ces2010a[[30]][44,3] <- 94.3
ces2010a[[31]] <- ces2010a[[31]] %>% # Separate values in two columns
  separate(X.1, sep="  ", into=c("a","b"))
ces2010a[[31]][c(2:5)] <- as.numeric(unlist(ces2010a[[31]][c(2:5)])) # Change type of variable
colnames(ces2010a[[30]]) <- colnames(ces2010a[[31]]) <- c("Survey.Units", # Set up column names
                                                          "VitASupplementationCoverage_PostPartum", 
                                                          "VitACoverage_Children12-59M",
                                                          "VitACoverage_Infants9-11M",
                                                          "AnthelminticCoverage_Children24-59M")
d7 <- bind_rows(ces2010a[[30]],ces2010a[[31]])
d7 <- d7[-c(1:3,28,31,38,50:52),] # Drop messy row
d7[33,1] <- "Rajshahi Division"
d7[41,1] <- "Serajganj"
d7[57,1] <- "Chittagong Division"

ces2010a[[32]][4,1] <- "Dhaka.Division 88.7"
ces2010a[[32]][23,1] <- "Dhaka.Slum 82.9"
ces2010a[[32]][24,1] <- "Khulna.Division 88.5"
ces2010a[[32]][36,1] <- "Rajshahi.New.Division 92.9"
ces2010a[[32]][38,1] <- "Chapai.Nawabganj 86.7"
ces2010a[[32]][46,1] <- "Rangpur.Division 90.4"
ces2010a[[32]] <- ces2010a[[32]] %>% # Separate values in two columns
  separate(Districts.Measles...., sep=" ", into=c("a","b"))
ces2010a[[32]][2] <- as.numeric(unlist(ces2010a[[32]][2])) # Change type of variable
colnames(ces2010a[[32]]) <- colnames(ces2010a[[33]]) <- c("Survey.Units", # Set up column names
                                                          "MeaslesCoverage_Children9-59M")
d8 <- bind_rows(ces2010a[[32]],ces2010a[[33]])
d8[3,2] <- 88
d8[21,2] <- 89
d8[22,2] <- 92.9
d8[33,2] <- 97.1
d8[35,2] <- 85.2
d8[4,1] <- "Dhaka Division"
d8[23,1] <- "Dhaka Slum"
d8[24,1] <- "Khulna Division"
d8[36,1] <- "Rajshahi Division"
d8[38,1] <- "Chapai Nawabganj"
d8[41,1] <- "Nartore"
d8[46,1] <- "Rangpur Division"
d8[78,1] <- "Lakshmipur"

# Final dataset
final <- merge(d1, d2, by="Survey.Units", all=TRUE)
final1 <- merge(final, d3, by="Survey.Units", all=TRUE)
final2 <- merge(final1, d4, by="Survey.Units", all=TRUE)
final3 <- merge(final2, d5, by="Survey.Units", all=TRUE)
final4 <- merge(final3, d6, by="Survey.Units", all=TRUE)
final5 <- merge(final4, d7, by="Survey.Units", all=TRUE)
ces2010 <- merge(final5, d8, by="Survey.Units", all=TRUE)

# Data quality assessment
sum(duplicated(ces2010)) # Uniqueness: Check for duplicates
ces2010 <- unique(ces2010)

sum(is.na(ces2010)) # Completeness: Check for NAs
na_ces2010 <- data.frame(lapply(ces2010, function(y) sum(length(which(is.na(y)))))) # Show NAs by column

str(ces2010) # Validity: Check format and type

max(ces2010[,c(2:44)], na.rm=TRUE) # Validity: Check range (between 0 and 100)

# Save final dataset
write.csv(ces2010,"./output/unicef/data/data_unicef_ces_2010.csv", row.names=FALSE) # Save data

# 2011 ----------------------------------------------------------------------------------------------------
#area2011a <- locate_areas("./data/unicef/CES/EPI Coverage Evaluation Survey 2011.pdf", pages=201) # Find area
#area2011b <- locate_areas("./data/unicef/CES/EPI Coverage Evaluation Survey 2011.pdf", pages=202) # Find area
area2011a <- list(c(79.38249, 53.25345, 716.26728, 561.48388)) # Set area
area2011b <- list(c(70, 70.58985, 662.43318, 555.09678 )) # Set area
pages2011a <- c(201, 207, 222, 226, 230, 232, 234)
pages2011b <- c(202, 208, 223, 227, 231, 233, 235)
ces2011a <- extract_tables(temp[5], pages=pages2011a, area = area2011a, output = "data.frame", guess=FALSE) # Extract data
ces2011b <- extract_tables(temp[5], pages=pages2011b , area = area2011b, output = "data.frame", guess=FALSE) # Extract data

ces2011a[[1]][10] <- NULL
ces2011b[[1]]
colnames(ces2011a[[1]]) <- colnames(ces2011b[[1]]) <- c("Survey.Units", # Set up column names
                                                        "BCG_Children12M",
                                                        "OPV1_Children12M",
                                                        "PENTA1_Children12M",
                                                        "OPV2_Children12M",
                                                        "PENTA2_Children12M",
                                                        "OPV3_Children12M",
                                                        "PENTA3_Children12M",
                                                        "Measles_Children12M",
                                                        "Fully_Children12M")
ces2011a[[1]][c(2:10)] <- as.numeric(unlist(ces2011a[[1]][c(2:10)])) # Change type of variable
ces2011b[[1]][c(2:10)] <- as.numeric(unlist(ces2011b[[1]][c(2:10)])) # Change type of variable
da1 <- bind_rows(ces2011a[[1]],ces2011b[[1]])
da1 <- da1[-c(1:2),] # Drop messy row

ces2011a[[2]] <- ces2011a[[2]] %>% # Separate values in two columns
  separate(X, sep=" ", into=c("BCG","OPV1","PENTA1","OPV2","PENTA2","OPV3","PENTA3","MSL"))
ces2011b[[2]]
colnames(ces2011a[[2]]) <- colnames(ces2011b[[2]]) <- c("Survey.Units", # Set up column names
                                                        "BCG_Children23M",
                                                        "OPV1_Children23M",
                                                        "PENTA1_Children23M",
                                                        "OPV2_Children23M",
                                                        "PENTA2_Children23M",
                                                        "OPV3_Children23M",
                                                        "PENTA3_Children23M",
                                                        "Measles_Children23M",
                                                        "Fully_Children23M")
ces2011a[[2]][c(2:10)] <- as.numeric(unlist(ces2011a[[2]][c(2:10)])) # Change type of variable
ces2011b[[2]][c(2:10)] <- as.numeric(unlist(ces2011b[[2]][c(2:10)])) # Change type of variable
da2 <- bind_rows(ces2011a[[2]],ces2011b[[2]])
da2 <- da2[-c(1:2),] # Drop messy row
da2[79,1] <- "Chittagong Division"

ces2011a[[3]] <- ces2011a[[3]] %>% # Separate values in two columns
  separate(X, sep=" ", into=c("TT1", "TT2", "TT3", "TT4", "TT5"))
ces2011b[[3]]
ces2011b[[3]][43,] <- c("Sylhet Division", "95.9", "94", "83.2", "64", "42.5")
colnames(ces2011a[[3]]) <- colnames(ces2011b[[3]]) <- c("Survey.Units", # Set up column names
                                                        "TT1_Mother0-11MChildren",
                                                        "TT2_Mother0-11MChildren",
                                                        "TT3_Mother0-11MChildren",
                                                        "TT4_Mother0-11MChildren",
                                                        "TT5_Mother0-11MChildren")
ces2011a[[3]][c(2:6)] <- as.numeric(unlist(ces2011a[[3]][c(2:6)])) # Change type of variable
ces2011b[[3]][c(2:6)] <- as.numeric(unlist(ces2011b[[3]][c(2:6)])) # Change type of variable
da3 <- bind_rows(ces2011a[[3]],ces2011b[[3]])
da3 <- da3[-c(1:2,45,82),] # Drop messy row
da3[33,1] <- "Rajshahi Division"

ces2011a[[4]] <- ces2011a[[4]] %>% # Separate values in two columns
  separate(X, sep=" ", into=c("TT1", "TT2", "TT3", "TT4", "TT5"))
ces2011b[[4]]
colnames(ces2011a[[4]]) <- colnames(ces2011b[[4]]) <- c("Survey.Units", # Set up column names
                                                        "TT1_Woman15-49Y",
                                                        "TT2_Woman15-49Y",
                                                        "TT3_Woman15-49Y",
                                                        "TT4_Woman15-49Y",
                                                        "TT5_Woman15-49Y")
ces2011a[[4]][c(2:6)] <- as.numeric(unlist(ces2011a[[4]][c(2:6)])) # Change type of variable
ces2011b[[4]][c(2:6)] <- as.numeric(unlist(ces2011b[[4]][c(2:6)])) # Change type of variable
da4 <- bind_rows(ces2011a[[4]],ces2011b[[4]])
da4 <- da4[-c(1:2,55),] # Drop messy row
da4[33,1] <- "Rajshahi Division"

ces2011a[[5]]
ces2011b[[5]]
colnames(ces2011a[[5]]) <- colnames(ces2011b[[5]]) <- c("Survey.Units", # Set up column names
                                                        "PAB_Mother0-11MChildren")
ces2011a[[5]][2] <- as.numeric(unlist(ces2011a[[5]][2])) # Change type of variable
ces2011b[[5]][2] <- as.numeric(unlist(ces2011b[[5]][2])) # Change type of variable
da5 <- bind_rows(ces2011a[[5]],ces2011b[[5]])
da5 <- da5[-c(1),] # Drop messy row
da5[33,1] <- "Rajshahi Division"

ces2011a[[6]] <- ces2011a[[6]] %>% # Separate values in two columns
  separate(X, sep=" ", into=c("OPV1", "OPV2", "OPVBoth"))
ces2011b[[6]]
colnames(ces2011a[[6]]) <- colnames(ces2011b[[6]]) <- c("Survey.Units", # Set up column names
                                                        "OPVCoverage_1stRound_Children59M",
                                                        "OPVCoverage_2stRound_Children59M",
                                                        "OPVCoverage_All_Children59M")
ces2011a[[6]][c(2:4)] <- as.numeric(unlist(ces2011a[[6]][c(2:4)])) # Change type of variable
ces2011b[[6]][c(2:4)] <- as.numeric(unlist(ces2011b[[6]][c(2:4)])) # Change type of variable
da6 <- bind_rows(ces2011a[[6]],ces2011b[[6]])
da6 <- da6[-c(1:2),] # Drop messy row
da6[33,1] <- "Rajshahi Division"

ces2011a[[7]] <- ces2011a[[7]] %>% # Separate values in two columns
  separate(X, sep=" ", into=c("Infant", "Children", "Postpartum Women", "Anthelmintic"))
ces2011b[[7]]
colnames(ces2011a[[7]]) <- colnames(ces2011b[[7]]) <- c("Survey.Units", # Set up column names
                                                        "VitACoverage_Infants9-11M",
                                                        "VitACoverage_Children12-59M",
                                                        "VitASupplementationCoverage_PostPartum", 
                                                        "AnthelminticCoverage_Children24-59M")
ces2011a[[7]][c(2:5)] <- as.numeric(unlist(ces2011a[[7]][c(2:5)])) # Change type of variable
ces2011b[[7]][c(2:5)] <- as.numeric(unlist(ces2011b[[7]][c(2:5)])) # Change type of variable
da7 <- bind_rows(ces2011a[[7]],ces2011b[[7]])
da7 <- da7[-c(1:3),] # Drop messy row
da7[33,1] <- "Rajshahi Division"

# Final dataset
final.da <- merge(da1, da2, by="Survey.Units", all=TRUE)
final.da1 <- merge(final.da, da3, by="Survey.Units", all=TRUE)
final.da2 <- merge(final.da1, da4, by="Survey.Units", all=TRUE)
final.da3 <- merge(final.da2, da5, by="Survey.Units", all=TRUE)
final.da4 <- merge(final.da3, da6, by="Survey.Units", all=TRUE)
ces2011 <- merge(final.da4, da7, by="Survey.Units", all=TRUE)
ces2011 <- ces2011[-c(46),] # Drop messy row

# Data quality assessment
sum(duplicated(ces2011)) # Uniqueness: Check for duplicates

sum(is.na(ces2011)) # Completeness: Check for NAs
na_ces2011 <- data.frame(lapply(ces2011, function(y) sum(length(which(is.na(y)))))) # Show NAs by column

str(ces2011) # Validity: Check format and type

max(ces2011[,c(2:37)], na.rm=TRUE) # Validity: Check range (between 0 and 100)

# Save final dataset
write.csv(ces2011,"./output/unicef/data/data_unicef_ces_2011.csv", row.names=FALSE) # Save data




# ---------------------------------------------------------------------------------------------------------
# ces2013 <- extract_tables(temp[6], pages=201:220, 226:239))
# ces2014 <- extract_tables(temp[8], pages=231:264)
# ces2015 <- extract_tables(temp[7], pages=215:242)
# ces2016 <- extract_tables(temp[1], pages=231:260)
