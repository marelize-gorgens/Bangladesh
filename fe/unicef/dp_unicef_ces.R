#----------------------------------------------------------------------------------------------------------
# dp_unicef_.R
# Description: Data preprocessing of Unicef CES datasets
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
df1$Fully <- as.numeric(df1$Fully)
colnames(df1) <- c("Survey.Units", # Set up column names
                   "BCG_Children12M",
                   "DPT1_Children12M",
                   "DPT2_Children12M",
                   "DPT3_Children12M",
                   "OPV1_Children12M",
                   "OPV2_Children12M",
                   "OPV3_Children12M",
                   "Measles_Children12M",
                   "Fully_Children12M")

# area2006b <- locate_areas("./data/unicef/CES/EPI Coverage Evaluation Survey 2006.pdf", pages=204) # Find area
area2006b <- list(c(119.75604, 63.46886, 739.66963, 553.56291)) # Set area
ces2006b <- extract_tables(temp[2], pages=203:214, area = area2006b, output = "data.frame", guess=FALSE) # Extract data
df2 <- bind_rows(ces2006b[[1]],ces2006b[[2]]) # Combine list's elements
df2$Districts <- gsub('Division Total', 'Division', df2$Districts) # Replace text to match others dataset
df2$Districts[df2$Districts=="Habigonj"] <- "Habiganj" # Replace text to match others dataset
colnames(df2) <- c("Survey.Units", # Rename variables
                   "TT1_Woman15-49Y",
                   "TT2_Woman15-49Y",
                   "TT3_Woman15-49Y",
                   "TT4_Woman15-49Y",
                   "TT5_Woman15-49Y")

df3 <- bind_rows(ces2006b[[5]],ces2006b[[6]]) # Combine list's elements
df3$Districts <- gsub('Division Total', 'Division', df3$Districts) # Replace text to match others dataset
df3$Districts[df3$Districts=="Chittagong Division"] <- "Chittagong Division" # Replace text to match others dataset
colnames(df3) <- c("Survey.Units", # Rename variables
                   "TT1_Woman18-25Y",
                   "TT2_Woman18-25Y",
                   "TT3_Woman18-25Y",
                   "TT4_Woman18-25Y",
                   "TT5_Woman18-25Y")

df4 <- bind_rows(ces2006b[[9]],ces2006b[[10]]) # Combine list's elements
df4$Districts <- gsub('Division Total', 'Division', df4$Districts) # Replace text to match others dataset
colnames(df4) <- c("Survey.Units", # Rename variables
                   "MeaslesCoverage_Children9M-9Y")

df5 <- bind_rows(ces2006b[[11]],ces2006b[[12]]) # Combine list's elements
df5$Districts <- gsub('Division Total', 'Division', df5$Districts) # Replace text to match others dataset
colnames(df5) <- c("Survey.Units", # Rename variables
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
colnames(ces2006c[[1]]) <- colnames(ces2006c[[2]]) <- c("Survey.Units", # Set up column names
                             "VitACoverage_PostPartum", 
                             "VitACoverage_Children12-59M",
                             "VitACoverage_Infants9-11M",
                             "AnthelminticCoverage_Children24-59M")
ces2006c[[1]] <- ces2006c[[1]] %>% retype() # Change type of variable
df6 <- bind_rows(ces2006c) # Combine list's elements
df6$Survey.Units <- gsub('Division Total', 'Division', df6$Survey.Units) # Replace text to match others dataset

# Final dataset
final <- merge(df1, df2, by="Survey.Units", all=TRUE)
final1 <- merge(final, df3, by="Survey.Units", all=TRUE)
final2 <- merge(final1, df4, by="Survey.Units", all=TRUE)
final3 <- merge(final2, df5, by="Survey.Units", all=TRUE)
ces2006 <- merge(final3, df6, by="Survey.Units", all=TRUE)
ces2006$Survey.Units <- gsub('City Corporation', 'CC', ces2006$Survey.Units) # Replace text to match others dataset
ces2006$Survey.Units[ces2006$Survey.Units=="Cox's Bazar"] <- "Cox’s Bazar"
ces2006$Survey.Units[ces2006$Survey.Units=="Moulvibazar"] <- "Moulavibazar"
ces2006$Year <- 2006

# Data quality assessment
sum(is.na(ces2006)) # Completeness: Check for NAs
na_ces2006 <- data.frame(lapply(ces2006, function(y) sum(length(which(is.na(y)))))) # Show NAs by column

sum(duplicated(ces2006)) # Uniqueness: Check for duplicates

str(ces2006) # Validity: Check format and type
ces2006$Fully <- as.numeric(ces2006$Fully)

max(ces2006[,c(2:29)], na.rm=TRUE) # Validity: Check range (between 0 and 100)

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
d1[14,1] <- "Moulavibazar" # Replace text to match others dataset
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
d2[38,1] <- "Joypurhat" # Replace text to match others dataset
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

ces2010a[[18]][13,1] <- "Mymensingh" # Replace text to match others dataset
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
d3[22,1] <- "Khulna Division" # Replace text to match others dataset
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
d4[33,1] <- "Rajshahi Division" # Replace text to match others dataset
d4[57,1] <- "Chittagong Division"
d4[66,1] <- "Lakshmipur"
d4[70,1] <- "Chittagong Slum"

ces2010a[[26]][48,] <- c("National", 91.9)
ces2010a[[27]][33,] <- c("SCC", 87.6)
colnames(ces2010a[[26]]) <- colnames(ces2010a[[27]]) <- c("Survey.Units", # Set up column names
                                                        "PAB_Mother0-11MChildren")
d5 <- bind_rows(ces2010a[[26]],ces2010a[[27]])
d5[58,1] <- "Chittagong Division" # Replace text to match others dataset
d5 <- d5[-c(50),] # Drop messy row
d5[2] <- as.numeric(unlist(d5[2]))
d5[33,1] <- "Rajshahi Division"
d5[41,1] <- "Serajganj"

ces2010a[[28]]
ces2010a[[29]]
ces2010a[[28]][c(3)] <- as.numeric(unlist(ces2010a[[28]][c(3)])) # Change type of variable
ces2010a[[29]][c(3)] <- as.numeric(unlist(ces2010a[[29]][c(3)]))
colnames(ces2010a[[28]]) <- colnames(ces2010a[[29]]) <- c("Survey.Units", # Set up column names
                                                          "OPVCoverage_18NID_Children0-5M",
                                                          "OPVCoverage_MeaslesCampaign_Children0-5M",
                                                          "OPVCoverage_Both_Children0-5M")
d6 <- bind_rows(ces2010a[[28]],ces2010a[[29]])
d6[69,1] <- "Chittagong Division" # Replace text to match others dataset
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
                                                          "VitACoverage_PostPartum", 
                                                          "VitACoverage_Children12-59M",
                                                          "VitACoverage_Infants9-11M",
                                                          "AnthelminticCoverage_Children24-59M")
d7 <- bind_rows(ces2010a[[30]],ces2010a[[31]])
d7 <- d7[-c(1:3,28,31,38,50:52),] # Drop messy row
d7[33,1] <- "Rajshahi Division" # Replace text to match others dataset
d7[41,1] <- "Serajganj"
d7[57,1] <- "Chittagong Division"

ces2010a[[32]][4,1] <- "Dhaka.Division 88.7" # Replace text to match others dataset
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
d8[4,1] <- "Dhaka Division" # Replace text to match others dataset
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
ces2010$Survey.Units[18] <- "Cox’s Bazar" # Replace text to match others dataset
ces2010$Survey.Units[ces2010$Survey.Units=="Nartore"] <- "Natore"
ces2010$Survey.Units[ces2010$Survey.Units=="Kustia"] <- "Kushtia"
ces2010$Year <- 2010

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
ces2011b[[1]][c(2:10)] <- as.numeric(unlist(ces2011b[[1]][c(2:10)]))
da1 <- bind_rows(ces2011a[[1]],ces2011b[[1]])
da1 <- da1[-c(1:2),] # Drop messy row

ces2011a[[2]] <- ces2011a[[2]] %>% # Separate values in columns
  separate(X, sep=" ", into=c("BCG","OPV1","PENTA1","OPV2","PENTA2","OPV3","PENTA3","MSL"))
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
ces2011b[[2]][c(2:10)] <- as.numeric(unlist(ces2011b[[2]][c(2:10)]))
da2 <- bind_rows(ces2011a[[2]],ces2011b[[2]])
da2 <- da2[-c(1:2),] # Drop messy row
da2[79,1] <- "Chittagong Division"

ces2011a[[3]] <- ces2011a[[3]] %>% # Separate values in columns
  separate(X, sep=" ", into=c("TT1", "TT2", "TT3", "TT4", "TT5"))
ces2011b[[3]][43,] <- c("Sylhet Division", "95.9", "94", "83.2", "64", "42.5")
colnames(ces2011a[[3]]) <- colnames(ces2011b[[3]]) <- c("Survey.Units", # Set up column names
                                                        "TT1_Mother0-11MChildren",
                                                        "TT2_Mother0-11MChildren",
                                                        "TT3_Mother0-11MChildren",
                                                        "TT4_Mother0-11MChildren",
                                                        "TT5_Mother0-11MChildren")
ces2011a[[3]][c(2:6)] <- as.numeric(unlist(ces2011a[[3]][c(2:6)])) # Change type of variable
ces2011b[[3]][c(2:6)] <- as.numeric(unlist(ces2011b[[3]][c(2:6)]))
da3 <- bind_rows(ces2011a[[3]],ces2011b[[3]])
da3 <- da3[-c(1:2,45,82),] # Drop messy row
da3[33,1] <- "Rajshahi Division"

ces2011a[[4]] <- ces2011a[[4]] %>% # Separate values in columns
  separate(X, sep=" ", into=c("TT1", "TT2", "TT3", "TT4", "TT5"))
colnames(ces2011a[[4]]) <- colnames(ces2011b[[4]]) <- c("Survey.Units", # Set up column names
                                                        "TT1_Woman15-49Y",
                                                        "TT2_Woman15-49Y",
                                                        "TT3_Woman15-49Y",
                                                        "TT4_Woman15-49Y",
                                                        "TT5_Woman15-49Y")
ces2011a[[4]][c(2:6)] <- as.numeric(unlist(ces2011a[[4]][c(2:6)])) # Change type of variable
ces2011b[[4]][c(2:6)] <- as.numeric(unlist(ces2011b[[4]][c(2:6)]))
da4 <- bind_rows(ces2011a[[4]],ces2011b[[4]])
da4 <- da4[-c(1:2,55),] # Drop messy row
da4[33,1] <- "Rajshahi Division"

colnames(ces2011a[[5]]) <- colnames(ces2011b[[5]]) <- c("Survey.Units", # Set up column names
                                                        "PAB_Mother0-11MChildren")
ces2011a[[5]][2] <- as.numeric(unlist(ces2011a[[5]][2])) # Change type of variable
ces2011b[[5]][2] <- as.numeric(unlist(ces2011b[[5]][2]))
da5 <- bind_rows(ces2011a[[5]],ces2011b[[5]])
da5 <- da5[-c(1),] # Drop messy row
da5[33,1] <- "Rajshahi Division"

ces2011a[[6]] <- ces2011a[[6]] %>% # Separate values in columns
  separate(X, sep=" ", into=c("OPV1", "OPV2", "OPVBoth"))
ces2011b[[6]]
colnames(ces2011a[[6]]) <- colnames(ces2011b[[6]]) <- c("Survey.Units", # Set up column names
                                                        "OPVCoverage_1stRound_Children59M",
                                                        "OPVCoverage_2stRound_Children59M",
                                                        "OPVCoverage_All_Children59M")
ces2011a[[6]][c(2:4)] <- as.numeric(unlist(ces2011a[[6]][c(2:4)])) # Change type of variable
ces2011b[[6]][c(2:4)] <- as.numeric(unlist(ces2011b[[6]][c(2:4)]))
da6 <- bind_rows(ces2011a[[6]],ces2011b[[6]])
da6 <- da6[-c(1:2),] # Drop messy row
da6[33,1] <- "Rajshahi Division"

ces2011a[[7]] <- ces2011a[[7]] %>% # Separate values in columns
  separate(X, sep=" ", into=c("Infant", "Children", "Postpartum Women", "Anthelmintic"))
colnames(ces2011a[[7]]) <- colnames(ces2011b[[7]]) <- c("Survey.Units", # Set up column names
                                                        "VitACoverage_Infants9-11M",
                                                        "VitACoverage_Children12-59M",
                                                        "VitACoverage_PostPartum", 
                                                        "AnthelminticCoverage_Children24-59M")
ces2011a[[7]][c(2:5)] <- as.numeric(unlist(ces2011a[[7]][c(2:5)])) # Change type of variable
ces2011b[[7]][c(2:5)] <- as.numeric(unlist(ces2011b[[7]][c(2:5)]))
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
ces2011$Survey.Units[ces2011$Survey.Units=="Khagrachhari"] <- "Khagrachari" # Replace text to match others dataset
ces2011$Survey.Units[ces2011$Survey.Units=="Cox's Bazar"] <- "Cox’s Bazar"
ces2011$Year <- 2011

# Data quality assessment
sum(duplicated(ces2011)) # Uniqueness: Check for duplicates

sum(is.na(ces2011)) # Completeness: Check for NAs
na_ces2011 <- data.frame(lapply(ces2011, function(y) sum(length(which(is.na(y)))))) # Show NAs by column

str(ces2011) # Validity: Check format and type

max(ces2011[,c(2:37)], na.rm=TRUE) # Validity: Check range (between 0 and 100)

# Save final dataset
write.csv(ces2011,"./output/unicef/data/data_unicef_ces_2011.csv", row.names=FALSE) # Save data


# 2013 ----------------------------------------------------------------------------------------------------
# area2013a <- locate_areas("./data/unicef/CES/EPI_Coverage_Evaluation_Survey_2013.pdf", pages=227) # Find area
# area2013b <- locate_areas("./data/unicef/CES/EPI_Coverage_Evaluation_Survey_2013.pdf", pages=253)
area2013a <- list(c(96.63158,71.05262,725.68421,545.68422)) # Set area
area2013b <- list(c(70.10526,66.31578,711.47368,546.63159))
pages2013a <- c(227,233,245,252,256,260,262,264) # Select pages
pages2013b <- c(228,234,246,253,257,261,263,265)
ces2013a <- extract_tables(temp[6], pages=pages2013a, area=area2013a, output="data.frame", guess=FALSE) # Extract data
ces2013b <- extract_tables(temp[6], pages=pages2013b, area=area2013b, output="data.frame", guess=FALSE)

ces2013a[[1]] <- ces2013a[[1]] %>% # Separate values in two columns
  separate(Corporation, sep=" ", into=c("a", "b"))
colnames(ces2013a[[1]]) <- colnames(ces2013b[[1]]) <- c("Survey.Units", # Set up column names
                                                        "BCG_Children12M",
                                                        "OPV1_Children12M",
                                                        "PENTA1_Children12M",
                                                        "OPV2_Children12M",
                                                        "PENTA2_Children12M",
                                                        "OPV3_Children12M",
                                                        "PENTA3_Children12M",
                                                        "Measles_Children12M",
                                                        "Fully_Children12M")
ces2013b[[1]][45,] <- c("Rural",99.3,95.4,91.1,94.5,93.1,92.2,92.5,86.4,81.2)
ces2013a[[1]][c(2:10)] <- as.numeric(unlist(ces2013a[[1]][c(2:10)])) # Change type of variable
ces2013b[[1]][c(2:10)] <- as.numeric(unlist(ces2013b[[1]][c(2:10)]))
db1 <- bind_rows(ces2013a[[1]],ces2013b[[1]])
db1 <- db1[-c(1),] # Drop messy row

ces2013a[[2]] <- ces2013a[[2]] %>% # Separate values in two columns
  separate(Corporation, sep=" ", into=c("a", "b"))
colnames(ces2013a[[2]]) <- colnames(ces2013b[[2]]) <- c("Survey.Units", # Set up column names
                                                        "BCG_Children23M",
                                                        "OPV1_Children23M",
                                                        "PENTA1_Children23M",
                                                        "OPV2_Children23M",
                                                        "PENTA2_Children23M",
                                                        "OPV3_Children23M",
                                                        "PENTA3_Children23M",
                                                        "Measles_Children23M",
                                                        "Fully_Children23M")
ces2013a[[2]][c(2:10)] <- as.numeric(unlist(ces2013a[[2]][c(2:10)])) # Change type of variable
db2 <- bind_rows(ces2013a[[2]],ces2013b[[2]])
db2 <- db2[-c(1),] # Drop messy row
db2[86,] <- c("Rural",99.3,95.4,91.1,94.6,93.1,92.7,92.7,89.2,83.8)

ces2013a[[3]][8] <- NULL
ces2013a[[3]][6] <- NULL
ces2013a[[3]] <- ces2013a[[3]] %>% # Separate values in two columns
  separate(and.Penta1.Measles, sep=" ", into=c("a", "b"))
ces2013a[[3]][c(2:7)] <- as.numeric(unlist(ces2013a[[3]][c(2:7)])) # Change type of variable
ces2013b[[3]][c(2:7)] <- as.numeric(unlist(ces2013b[[3]][c(2:7)]))
colnames(ces2013a[[3]]) <- colnames(ces2013b[[3]]) <- c("Survey.Units", # Set up column names
                                                        "InvalidPENTA1_Infant12M",
                                                        "InvalidPENTA2_Infant12M",
                                                        "InvalidPENTA3_Infant12M",
                                                        "InvalidMeasles_Infant12M",
                                                        "DropoutPENTA1-PENTA3_Children23M",
                                                        "DropoutPENTA1-Measles_Children23M")
db3 <- bind_rows(ces2013a[[3]],ces2013b[[3]])
db3 <- db3[-c(1:4,46:47),] # Drop messy row
db3$Survey.Units <- gsub('City Corp', 'CC', db3$Survey.Units) # Replace text to match others dataset
db3$Survey.Units <- gsub('Kustia', 'Kushtia', db3$Survey.Units)
db3$Survey.Units <- gsub('Nartore', 'Natore', db3$Survey.Units)

colnames(ces2013a[[4]]) <- colnames(ces2013b[[4]]) <- c("Survey.Units", # Set up column names
                                                        "TT1_Mother0-11MChildren",
                                                        "TT2_Mother0-11MChildren",
                                                        "TT3_Mother0-11MChildren",
                                                        "TT4_Mother0-11MChildren",
                                                        "TT5_Mother0-11MChildren")
db4 <- bind_rows(ces2013a[[4]],ces2013b[[4]])
db4$Survey.Units <- gsub('Kustia', 'Kushtia', db4$Survey.Units) # Replace text to match others dataset

colnames(ces2013a[[5]]) <- colnames(ces2013b[[5]]) <- c("Survey.Units", # Set up column names
                                                        "TT1_Woman18-49Y",
                                                        "TT2_Woman18-49Y",
                                                        "TT3_Woman18-49Y",
                                                        "TT4_Woman18-49Y",
                                                        "TT5_Woman18-49Y")
db4$Survey.Units <- gsub('Nartore', 'Natore', db4$Survey.Units) # Replace text to match others dataset

db5 <- bind_rows(ces2013a[[5]],ces2013b[[5]])
db5$Survey.Units <- gsub('Kustia', 'Kushtia', db5$Survey.Units) # Replace text to match others dataset
db5$Survey.Units <- gsub('Nartore', 'Natore', db5$Survey.Units)

colnames(ces2013a[[6]]) <- colnames(ces2013b[[6]]) <- c("Survey.Units", # Set up column names
                                                        "PAB_Mother0-11MChildren")
db6 <- bind_rows(ces2013a[[6]],ces2013b[[6]])
db6$Survey.Units <- gsub('City Corp', 'CC', db6$Survey.Units) # Replace text to match others dataset

colnames(ces2013a[[7]]) <- colnames(ces2013b[[7]]) <- c("Survey.Units", # Set up column names
                                                        "OPVCoverage_1stRound_Children59M",
                                                        "OPVCoverage_2stRound_Children59M",
                                                        "OPVCoverage_All_Children59M")
db6$Survey.Units <- gsub('Kustia', 'Kushtia', db6$Survey.Units) # Replace text to match others dataset
db6$Survey.Units <- gsub('Nartore', 'Natore', db6$Survey.Units)

db7 <- bind_rows(ces2013a[[7]],ces2013b[[7]])
db7$Survey.Units <- gsub('Kustia', 'Kushtia', db7$Survey.Units) # Replace text to match others dataset
db7$Survey.Units <- gsub('Nartore', 'Natore', db7$Survey.Units)

ces2013a[[8]] <- ces2013a[[8]] %>% # Separate values in four columns
  separate(Anthelmintic.Coverage.among.Children.aged.24.59.Months.by, sep=" ", into=c("a","b","c","d"))
ces2013b[[8]][46,] <- c("Rural",83.0,92.1,35.3,89.5)
ces2013a[[8]][c(2:5)] <- as.numeric(unlist(ces2013a[[8]][c(2:5)])) # Change type of variable
ces2013b[[8]][c(2:5)] <- as.numeric(unlist(ces2013b[[8]][c(2:5)]))
colnames(ces2013a[[8]]) <- colnames(ces2013b[[8]]) <- c("Survey.Units", # Set up column names
                                                        "VitACoverage_Infants6-11M",
                                                        "VitACoverage_Children12-59M",
                                                        "VitACoverage_PostPartum", 
                                                        "AnthelminticCoverage_Children24-59M")
db8 <- bind_rows(ces2013a[[8]],ces2013b[[8]])
db8 <- db8[-c(1:2),] # Drop messy row
db8$Survey.Units <- gsub('City Corp', 'CC', db8$Survey.Units) # Replace text to match others dataset
db8$Survey.Units <- gsub('Kustia', 'Kushtia', db8$Survey.Units)
db8$Survey.Units <- gsub('Nartore', 'Natore', db8$Survey.Units)

# Final dataset
final.db <- merge(db1, db2, by="Survey.Units", all=TRUE)
final.db1 <- merge(final.db, db3, by="Survey.Units", all=TRUE)
final.db2 <- merge(final.db1, db4, by="Survey.Units", all=TRUE)
final.db3 <- merge(final.db2, db5, by="Survey.Units", all=TRUE)
final.db4 <- merge(final.db3, db6, by="Survey.Units", all=TRUE)
final.db5 <- merge(final.db4, db7, by="Survey.Units", all=TRUE)
ces2013 <- merge(final.db5, db8, by="Survey.Units", all=TRUE)
ces2013$Survey.Units[22] <- "Cox’s Bazar" # Replace text to match others dataset
ces2013$Year <- 2013

# Data quality assessment
sum(duplicated(ces2013)) # Uniqueness: Check for duplicates

sum(is.na(ces2013)) # Completeness: Check for NAs
na_ces2013 <- data.frame(lapply(ces2013, function(y) sum(length(which(is.na(y)))))) # Show NAs by column

str(ces2013) # Validity: Check format and type

max(ces2013[,c(2:43)], na.rm=TRUE) # Validity: Check range (between 0 and 100)

# Save final dataset
write.csv(ces2013,"./output/unicef/data/data_unicef_ces_2013.csv", row.names=FALSE) # Save data

# 2014 ----------------------------------------------------------------------------------------------------
# area2014a <- locate_areas("./data/unicef/CES/EPIEvaluationSurvey2014.pdf", pages=261) # Find area
# area2014b <- locate_areas("./data/unicef/CES/EPIEvaluationSurvey2014.pdf", pages=286)
area2014a <- list(c(107.17992,86.30857,741.07256,559.94175)) # Set area
area2014b <- list(c(94.93078,61.81030,733.92723,535.44348))
pages2014a <- c(261,265,271,273,275,279,281,285,287,289,291) # Select pages
pages2014b <- c(262,266,272,274,276,280,282,286,288,290,292)
ces2014a <- extract_tables(temp[8], pages=pages2014a, area=area2014a, output="data.frame", guess=FALSE) # Extract data
ces2014b <- extract_tables(temp[8], pages=pages2014b, area=area2014b, output="data.frame", guess=FALSE)

colnames(ces2014a[[1]]) <- colnames(ces2014b[[1]]) <- c("Survey.Units", # Set up column names
                                                        "BCG_Children23M",
                                                        "OPV1_Children23M",
                                                        "PENTA1_Children23M",
                                                        "OPV2_Children23M",
                                                        "PENTA2_Children23M",
                                                        "OPV3_Children23M",
                                                        "PENTA3_Children23M",
                                                        "Measles_Children23M",
                                                        "Fully_Children23M")
dc1 <- bind_rows(ces2014a[[1]],ces2014b[[1]])

colnames(ces2014a[[2]]) <- colnames(ces2014b[[2]]) <- c("Survey.Units", # Set up column names
                                                        "BCG_Children12M",
                                                        "OPV1_Children12M",
                                                        "PENTA1_Children12M",
                                                        "OPV2_Children12M",
                                                        "PENTA2_Children12M",
                                                        "OPV3_Children12M",
                                                        "PENTA3_Children12M",
                                                        "Measles_Children12M",
                                                        "Fully_Children12M")
dc2 <- bind_rows(ces2014a[[2]],ces2014b[[2]])

ces2014a[[3]][2] <- NULL
ces2014b[[3]][2] <- NULL
colnames(ces2014a[[3]]) <- colnames(ces2014b[[3]]) <- c("Survey.Units", # Set up column names
                                                        "DropoutPENTA1-PENTA3_Children23M",
                                                        "DropoutPENTA1-Measles_Children23M")
dc3 <- bind_rows(ces2014a[[3]],ces2014b[[3]])

ces2014a[[4]][2] <- NULL
ces2014b[[4]][2] <- NULL
colnames(ces2014a[[4]]) <- colnames(ces2014b[[4]]) <- c("Survey.Units", # Set up column names
                                                        "InvalidPENTA1_Infant12M",
                                                        "InvalidPENTA2_Infant12M",
                                                        "InvalidPENTA3_Infant12M",
                                                        "InvalidMeasles_Infant12M")
dc4 <- bind_rows(ces2014a[[4]],ces2014b[[4]])

ces2014a[[5]][2] <- NULL
ces2014b[[5]][2] <- NULL
colnames(ces2014a[[5]]) <- colnames(ces2014b[[5]]) <- c("Survey.Units", # Set up column names
                                                        "Measles2ndDoseCrude_Children18-29M",
                                                        "Measles2ndDose_Children18-29M")
dc5 <- bind_rows(ces2014a[[5]],ces2014b[[5]])
dc5 <- dc5[-c(2)]

colnames(ces2014a[[6]]) <- colnames(ces2014b[[6]]) <- c("Survey.Units", # Set up column names
                                                        "TT1_Mother0-11MChildren",
                                                        "TT2_Mother0-11MChildren",
                                                        "TT3_Mother0-11MChildren",
                                                        "TT4_Mother0-11MChildren",
                                                        "TT5_Mother0-11MChildren")
dc6 <- bind_rows(ces2014a[[6]],ces2014b[[6]])

ces2014a[[7]][2] <- NULL
ces2014b[[7]][2] <- NULL
colnames(ces2014a[[7]]) <- colnames(ces2014b[[7]]) <- c("Survey.Units", # Set up column names
                                                        "TetanusAtBirth_Mother0-11MChildren")
dc7 <- bind_rows(ces2014a[[7]],ces2014b[[7]])

colnames(ces2014a[[8]]) <- colnames(ces2014b[[8]]) <- c("Survey.Units", # Set up column names
                                                        "TT1_Woman18-49Y",
                                                        "TT2_Woman18-49Y",
                                                        "TT3_Woman18-49Y",
                                                        "TT4_Woman18-49Y",
                                                        "TT5_Woman18-49Y")
dc8 <- bind_rows(ces2014a[[8]],ces2014b[[8]])

ces2014a[[9]][2] <- NULL
ces2014b[[9]][2] <- NULL
colnames(ces2014a[[9]]) <- colnames(ces2014b[[9]]) <- c("Survey.Units", # Set up column names
                                                        "MR_AdolescentGirls")
dc9 <- bind_rows(ces2014a[[9]],ces2014b[[9]])

ces2014a[[10]][2] <- NULL
colnames(ces2014a[[10]]) <- colnames(ces2014b[[10]]) <- c("Survey.Units", # Set up column names
                                                        "OPVCoverage_1stRound_Children59M",
                                                        "OPVCoverage_2stRound_Children59M",
                                                        "OPVCoverage_All_Children59M")
dc10 <- bind_rows(ces2014a[[10]],ces2014b[[10]])

ces2014a[[11]][2] <- NULL
ces2014b[[11]][2] <- NULL
colnames(ces2014a[[11]]) <- colnames(ces2014b[[11]]) <- c("Survey.Units", # Set up column names
                                                        "VitACoverage_Infants6-11M",
                                                        "VitACoverage_Children12-59M")
dc11 <- bind_rows(ces2014a[[11]],ces2014b[[11]])

# Final dataset
final.dc <- merge(dc1, dc2, by="Survey.Units", all=TRUE)
final.dc1 <- merge(final.dc, dc3, by="Survey.Units", all=TRUE)
final.dc2 <- merge(final.dc1, dc4, by="Survey.Units", all=TRUE)
final.dc3 <- merge(final.dc2, dc5, by="Survey.Units", all=TRUE)
final.dc4 <- merge(final.dc3, dc6, by="Survey.Units", all=TRUE)
final.dc5 <- merge(final.dc4, dc7, by="Survey.Units", all=TRUE)
final.dc6 <- merge(final.dc5, dc8, by="Survey.Units", all=TRUE)
final.dc7 <- merge(final.dc6, dc9, by="Survey.Units", all=TRUE)
final.dc8 <- merge(final.dc7, dc10, by="Survey.Units", all=TRUE)
ces2014 <- merge(final.dc8, dc11, by="Survey.Units", all=TRUE)
ces2014$Survey.Units <- gsub('City Corporation', 'CC', ces2014$Survey.Units) # Replace text to match others dataset
ces2014$Survey.Units[18] <- "Cox’s Bazar"
ces2014$Survey.Units[ces2014$Survey.Units=="Thakurgoan"] <- "Thakurgaon"
ces2014$Survey.Units[ces2014$Survey.Units=="Panchagharh"] <- "Panchagarh"
ces2014$Survey.Units[ces2014$Survey.Units=="Narsingdhi"] <- "Narsingdi"
ces2014$Survey.Units[ces2014$Survey.Units=="Moulvi Bazar"] <- "Moulavibazar"
ces2014$Survey.Units[ces2014$Survey.Units=="Jhalakati"] <- "Jhalokati"
ces2014$Year <- 2014

# Data quality assessment
sum(duplicated(ces2014)) # Uniqueness: Check for duplicates

sum(is.na(ces2014)) # Completeness: Check for NAs

str(ces2014) # Validity: Check format and type

max(ces2014[,c(2:43)], na.rm=TRUE) # Validity: Check range (between 0 and 100)

# Save final dataset
write.csv(ces2014,"./output/unicef/data/data_unicef_ces_2014.csv", row.names=FALSE) # Save data

# 2015 ----------------------------------------------------------------------------------------------------
# area2015a <- locate_areas("./data/unicef/CES/EPI_Coverage_Evaluation_Survey_2015.pdf", pages=229) # Find area
# area2015b <- locate_areas("./data/unicef/CES/EPI_Coverage_Evaluation_Survey_2015.pdf", pages=224)
area2015a <- list(c(98.82775,68.27511,725.70574,513.96891)) # Set area
area2015b <- list(c(78.48086,113.81339,682.10526,556.60049)) 
pages2015a <- c(221,223,229,231,233,237,239,243) # Select pages
pages2015b <- c(222,224,230,232,234,238,240,244)
ces2015a <- extract_tables(temp[7], pages=pages2015a, area=area2015a, output="data.frame", guess=FALSE) # Extract data
ces2015b <- extract_tables(temp[7], pages=pages2015b, area=area2015b, output="data.frame", guess=FALSE)

ces2015a[[1]][16,1] <- "Cox’s Bazar" # Replace text to match others dataset
ces2015a[[1]][25,1] <- "Dhaka North CC"
ces2015a[[1]][27,1] <- "Dhaka South CC"
ces2015a[[1]][40,1] <- "Narayanganj CC"
ces2015b[[1]][42,1] <- "Dhaka CC Slum"
ces2015b[[1]][44,1] <- "Chittagong CC Slum"
ces2015a[[1]][,1] <- gsub("City Corporation", "CC", ces2015a[[1]][,1], ignore.case=TRUE)
ces2015b[[1]][,1] <- gsub("City Corporation", "CC", ces2015b[[1]][,1], ignore.case=TRUE)
colnames(ces2015a[[1]]) <- colnames(ces2015b[[1]]) <- c("Survey.Units", # Set up column names
                                                        "BCG_Children23M",
                                                        "OPV1_Children23M",
                                                        "PENTA1_Children23M",
                                                        "OPV2_Children23M",
                                                        "PENTA2_Children23M",
                                                        "OPV3_Children23M",
                                                        "PENTA3_Children23M",
                                                        "Measles_Children23M",
                                                        "Fully_Children23M")
dd1 <- bind_rows(ces2015a[[1]],ces2015b[[1]])
dd1 <- dd1[-c(24,26,39,90,92),] # Drop messy row

ces2015a[[2]][13,7] <- 90.6
ces2015a[[2]][16,1] <- "Cox’s Bazar" # Replace text to match others dataset
ces2015a[[2]][25,1] <- "Dhaka North CC"
ces2015a[[2]][27,1] <- "Dhaka South CC"
ces2015a[[2]][40,1] <- "Narayanganj CC"
ces2015b[[2]][44,1] <- "Dhaka CC Slum"
ces2015b[[2]][46,1] <- "Chittagong CC Slum"
ces2015a[[2]][,1] <- gsub("City Corporation", "CC", ces2015a[[2]][,1], ignore.case=TRUE)
ces2015b[[2]][,1] <- gsub("City Corporation", "CC", ces2015b[[2]][,1], ignore.case=TRUE)
ces2015a[[2]][2:10] <- as.numeric(unlist(ces2015a[[2]][2:10])) # Change type of variable
ces2015b[[2]][2:10] <- as.numeric(unlist(ces2015b[[2]][2:10]))
colnames(ces2015a[[2]]) <- colnames(ces2015b[[2]]) <- c("Survey.Units", # Set up column names
                                                        "BCG_Children12M",
                                                        "OPV1_Children12M",
                                                        "PENTA1_Children12M",
                                                        "OPV2_Children12M",
                                                        "PENTA2_Children12M",
                                                        "OPV3_Children12M",
                                                        "PENTA3_Children12M",
                                                        "Measles_Children12M",
                                                        "Fully_Children12M")
dd2 <- bind_rows(ces2015a[[2]],ces2015b[[2]])
dd2 <- dd2[-c(24,26,39,53,90,92),] # Drop messy row

ces2015a[[3]] <- ces2015a[[3]] %>% # Separate values in three columns
  separate(Dropout.rate.Penta1.to.Penta3, sep=" ", into=c("a","b", "c"))
ces2015a[[3]] <- ces2015a[[3]] %>% 
  separate(Dropout.rate.Penta1.to.MR, sep=" ", into=c("d","e", "f"))
ces2015b[[3]] <- ces2015b[[3]] %>% 
  separate(Dropout.rate.Penta1.to.Penta3, sep=" ", into=c("a","b", "c"))
ces2015b[[3]] <- ces2015b[[3]] %>% 
  separate(Dropout.rate.Penta1.to.MR, sep=" ", into=c("d","e", "f"))
ces2015a[[3]][2:7] <- as.numeric(unlist(ces2015a[[3]][2:7])) # Change type of variable
ces2015b[[3]][2:7] <- as.numeric(unlist(ces2015b[[3]][2:7]))
colnames(ces2015a[[3]]) <- colnames(ces2015b[[3]]) <- c("Survey.Units", # Set up column names
                                                        "DropoutPENTA1-PENTA3_Male23M",
                                                        "DropoutPENTA1-PENTA3_Female23M",
                                                        "DropoutPENTA1-PENTA3_Children23M",
                                                        "DropoutPENTA1-Measles_Male23M",
                                                        "DropoutPENTA1-Measles_Female23M",
                                                        "DropoutPENTA1-Measles_Children23M")
dd3 <- bind_rows(ces2015a[[3]],ces2015b[[3]])
dd3 <- dd3[-c(1,46),] # Drop messy row
dd3[16,1] <- "Cox’s Bazar" # Replace text to match others dataset
dd3[,1] <- gsub("City Corporation", "CC", dd3[,1], ignore.case=TRUE)

dd4 <- bind_rows(ces2015a[[4]],ces2015b[[4]])
dd4[,1] <- gsub("City Corporation", "CC", dd4[,1], ignore.case=TRUE) # Replace text to match others dataset
dd4[16,1] <- "Cox’s Bazar"
colnames(dd4) <- c("Survey.Units", # Set up column names
                   "InvalidPENTA1_Infant12M",
                   "InvalidPENTA2_Infant12M",
                   "InvalidPENTA3_Infant12M",
                   "InvalidMeasles_Infant12M")

ces2015a[[5]][2:4] <- as.numeric(unlist(ces2015a[[5]][2:4])) # Change type of variable
ces2015b[[5]][2:4] <- as.numeric(unlist(ces2015b[[5]][2:4]))
colnames(ces2015a[[5]]) <- colnames(ces2015b[[5]]) <- c("Survey.Units", # Set up column names
                                                        "Measles2ndDoseCrude",
                                                        "Measles2ndDose_Children18M",
                                                        "Measles2ndDose_Children23M")
dd5 <- bind_rows(ces2015a[[5]],ces2015b[[5]])
dd5 <- dd5[-c(1,46),] # Drop messy row
dd5[16,1] <- "Cox’s Bazar" # Replace text to match others dataset
dd5[,1] <- gsub("City Corporation", "CC", dd5[,1], ignore.case=TRUE)

colnames(ces2015a[[6]]) <- colnames(ces2015b[[6]]) <- c("Survey.Units", # Set up column names
                                                        "TT1_Mother0-11MChildren",
                                                        "TT2_Mother0-11MChildren",
                                                        "TT3_Mother0-11MChildren",
                                                        "TT4_Mother0-11MChildren",
                                                        "TT5_Mother0-11MChildren")
dd6 <- bind_rows(ces2015a[[6]],ces2015b[[6]])
dd6[16,1] <- "Cox’s Bazar" # Replace text to match others dataset
dd6[,1] <- gsub("City Corporation", "CC", dd6[,1], ignore.case=TRUE)

colnames(ces2015a[[7]]) <- colnames(ces2015b[[7]]) <- c("Survey.Units", # Set up column names
                                                        "TetanusAtBirth_Mother0-11MChildren")
dd7 <- bind_rows(ces2015a[[7]],ces2015b[[7]])
dd7[16,1] <- "Cox’s Bazar" # Replace text to match others dataset
dd7[,1] <- gsub("City Corporation", "CC", dd7[,1], ignore.case=TRUE)

ces2015a[[8]][16,1] <- "Cox’s Bazar" # Replace text to match others dataset
ces2015a[[8]][26,1] <- "Dhaka South CC"
ces2015a[[8]][39,1] <- "Narayanganj CC"
ces2015b[[8]][44,1] <- "Chittagong CC Slum"
ces2015a[[8]][2:6] <- as.numeric(unlist(ces2015a[[8]][2:6])) # Change type of variable
ces2015b[[8]][2:6] <- as.numeric(unlist(ces2015b[[8]][2:6]))
colnames(ces2015a[[8]]) <- colnames(ces2015b[[8]]) <- c("Survey.Units", # Set up column names
                                                        "TT1_Woman15-49Y",
                                                        "TT2_Woman15-49Y",
                                                        "TT3_Woman15-49Y",
                                                        "TT4_Woman15-49Y",
                                                        "TT5_Woman15-49Y")
dd8 <- bind_rows(ces2015a[[8]],ces2015b[[8]])
dd8[,1] <- gsub("City Corporation", "CC", dd8[,1], ignore.case=TRUE) # Replace text to match others dataset
dd8 <- dd8[-c(25,38,89),] # Drop messy row

# Final dataset
final.dd <- merge(dd1, dd2, by="Survey.Units", all=TRUE)
final.dd1 <- merge(final.dd, dd3, by="Survey.Units", all=TRUE)
final.dd2 <- merge(final.dd1, dd4, by="Survey.Units", all=TRUE)
final.dd3 <- merge(final.dd2, dd5, by="Survey.Units", all=TRUE)
final.dd4 <- merge(final.dd3, dd6, by="Survey.Units", all=TRUE)
final.dd5 <- merge(final.dd4, dd7, by="Survey.Units", all=TRUE)
ces2015 <- merge(final.dd5, dd8, by="Survey.Units", all=TRUE)
ces2015$Survey.Units[ces2015$Survey.Units=="Thakurgoan"] <- "Thakurgaon"
ces2015$Survey.Units[ces2015$Survey.Units=="Panchagharh"] <- "Panchagarh"
ces2015$Survey.Units[ces2015$Survey.Units=="Narsingdhi"] <- "Narsingdi"
ces2015$Survey.Units[ces2015$Survey.Units=="Moulvi Bazar"] <- "Moulavibazar"
ces2015$Survey.Units[ces2015$Survey.Units=="Jhalakati"] <- "Jhalokati"

ces2015$Year <- 2015

# Data quality assessment
sum(duplicated(ces2015)) # Uniqueness: Check for duplicates

sum(is.na(ces2015)) # Completeness: Check for NAs
na_ces2015 <- data.frame(lapply(ces2015, function(y) sum(length(which(is.na(y)))))) # Show NAs by column

str(ces2015) # Validity: Check format and type

max(ces2015[,c(2:43)], na.rm=TRUE) # Validity: Check range (between 0 and 100)

# Save final dataset
write.csv(ces2015,"./output/unicef/data/data_unicef_ces_2015.csv", row.names=FALSE) # Save data

# 2016 ----------------------------------------------------------------------------------------------------
# area2016a <- locate_areas("./data/unicef/CES/Bangladesh_EPI_CES_2016_Final.pdf", pages=233) # Find area
# area2016b <- locate_areas("./data/unicef/CES/Bangladesh_EPI_CES_2016_Final.pdf", pages=236)
area2016a <- list(c(111.24624,52.46483,806.28007,557.66565)) # Set area
area2016b <- list(c(86.75165,57.56786,803.21825,557.66565))
pages2016a <- c(235,237,243,245,247,251,253,257,259) # Select pages
pages2016b <- c(236,238,244,246,248,252,254,258,260)
ces2016a <- extract_tables(temp[1], pages=pages2016a, area=area2016a, output="data.frame", guess=FALSE) # Extract data
ces2016b <- extract_tables(temp[1], pages=pages2016b, area=area2016b, output="data.frame", guess=FALSE)

colnames(ces2016a[[1]]) <- colnames(ces2016b[[1]]) <- c("Survey.Units", # Set up column names
                                                        "BCG_Children23M",
                                                        "OPV1_Children23M",
                                                        "PENTA1_Children23M",
                                                        "OPV2_Children23M",
                                                        "PENTA2_Children23M",
                                                        "OPV3_Children23M",
                                                        "PENTA3_Children23M",
                                                        "Measles_Children23M",
                                                        "Fully_Children23M")
de1 <- bind_rows(ces2016a[[1]],ces2016b[[1]])
de1[16,1] <- "Cox’s Bazar" # Replace text to match others dataset
de1[,1] <- gsub("City Corporation", "CC", de1[,1], ignore.case=TRUE)

colnames(ces2016a[[2]]) <- colnames(ces2016b[[2]]) <- c("Survey.Units", # Set up column names
                                                        "BCG_Children12M",
                                                        "OPV1_Children12M",
                                                        "PENTA1_Children12M",
                                                        "OPV2_Children12M",
                                                        "PENTA2_Children12M",
                                                        "OPV3_Children12M",
                                                        "PENTA3_Children12M",
                                                        "Measles_Children12M",
                                                        "Fully_Children12M")
de2 <- bind_rows(ces2016a[[2]],ces2016b[[2]])
de2[16,1] <- "Cox’s Bazar" # Replace text to match others dataset
de2[,1] <- gsub("City Corporation", "CC", de2[,1], ignore.case=TRUE)

ces2016a[[3]] <- ces2016a[[3]] %>% # Separate values in four columns
  separate(Dropout.rate.Penta1.to.Penta3, sep=" ", into=c("a","b", "c"))
ces2016a[[3]] <- ces2016a[[3]] %>% 
  separate(Dropout.rate.Penta1.to.MR1, sep=" ", into=c("d","e", "f"))
ces2016a[[3]] <- ces2016a[[3]][-c(2)]
ces2016b[[3]] <- ces2016b[[3]] %>%
  separate(Dropout.rate.Penta1.to.Penta3, sep=" ", into=c("a","b", "c"))
ces2016b[[3]] <- ces2016b[[3]] %>%
  separate(Dropout.rate.Penta1.to.MR1, sep=" ", into=c("d","e", "f"))
ces2016b[[3]] <- ces2016b[[3]][-c(2)]
ces2016a[[3]][2:7] <- as.numeric(unlist(ces2016a[[3]][2:7])) # Change type of variable
ces2016b[[3]][2:7] <- as.numeric(unlist(ces2016b[[3]][2:7]))
colnames(ces2016a[[3]]) <- colnames(ces2016b[[3]]) <- c("Survey.Units", # Set up column names
                                                        "DropoutPENTA1-PENTA3_Male23M",
                                                        "DropoutPENTA1-PENTA3_Female23M",
                                                        "DropoutPENTA1-PENTA3_Children23M",
                                                        "DropoutPENTA1-Measles_Male23M",
                                                        "DropoutPENTA1-Measles_Female23M",
                                                        "DropoutPENTA1-Measles_Children23M")
de3 <- bind_rows(ces2016a[[3]],ces2016b[[3]])
de3[18,1] <- "Cox’s Bazar" # Replace text to match others dataset
de3[,1] <- gsub("City Corporation", "CC", de3[,1], ignore.case=TRUE) 
de3 <- de3[-c(1:2,47:48),] # Drop messy row

de4 <- bind_rows(ces2016a[[4]],ces2016b[[4]])
de4[16,1] <- "Cox’s Bazar" # Replace text to match others dataset
de4[,1] <- gsub("City Corporation", "CC", de4[,1], ignore.case=TRUE) 
colnames(de4) <- c("Survey.Units", # Set up column names
                   "InvalidPENTA1_Infant12M",
                   "InvalidPENTA2_Infant12M",
                   "InvalidPENTA3_Infant12M",
                   "InvalidMeasles_Infant12M")

de5 <- bind_rows(ces2016a[[5]],ces2016b[[5]])
de5[16,1] <- "Cox’s Bazar" # Replace text to match others dataset
de5[,1] <- gsub("City Corporation", "CC", de5[,1], ignore.case=TRUE)
colnames(de5) <- c("Survey.Units", # Set up column names
                  "Measles2ndDoseCrude",
                  "Measles2ndDose_Children18M",
                  "Measles2ndDose_Children23M")

de6 <- bind_rows(ces2016a[[6]],ces2016b[[6]])
de6[16,1] <- "Cox’s Bazar" # Replace text to match others dataset
de6[,1] <- gsub("City Corporation", "CC", de6[,1], ignore.case=TRUE)
colnames(de6) <- c("Survey.Units", # Set up column names
                   "TT1_Mother0-11MChildren",
                   "TT2_Mother0-11MChildren",
                   "TT3_Mother0-11MChildren",
                   "TT4_Mother0-11MChildren",
                   "TT5_Mother0-11MChildren")

de7 <- bind_rows(ces2016a[[7]],ces2016b[[7]])
de7[16,1] <- "Cox’s Bazar" # Replace text to match others dataset
de7[,1] <- gsub("City Corporation", "CC", de7[,1], ignore.case=TRUE) 
colnames(de7) <- c("Survey.Units", # Set up column names
                   "TetanusAtBirth_Mother0-11MChildren")

de8 <- bind_rows(ces2016a[[8]],ces2016b[[8]])
de8[16,1] <- "Cox’s Bazar" # Replace text to match others dataset
de8[,1] <- gsub("City Corporation", "CC", de8[,1], ignore.case=TRUE)
colnames(de8) <- c("Survey.Units", # Set up column names
                   "TT1_Woman18-49Y",
                   "TT2_Woman18-49Y",
                   "TT3_Woman18-49Y",
                   "TT4_Woman18-49Y",
                   "TT5_Woman18-49Y")

ces2016a[[9]][2:3] <- as.numeric(unlist(ces2016a[[9]][2:3])) # Change type of variable
colnames(ces2016a[[9]]) <- colnames(ces2016b[[9]]) <- c("Survey.Units", # Set up column names
                                                        "VitACoverage_Infants6-11M",
                                                        "VitACoverage_Children12-59M")
de9 <- bind_rows(ces2016a[[9]],ces2016b[[9]])
de9[17,1] <- "Cox’s Bazar" # Replace text to match others dataset
de9[,1] <- gsub("City Corporation", "CC", de9[,1], ignore.case=TRUE)
de9 <- de9[-c(1),] # Drop messy row

# Final dataset
final.de <- merge(de1, de2, by="Survey.Units", all=TRUE)
final.de1 <- merge(final.de, de3, by="Survey.Units", all=TRUE)
final.de2 <- merge(final.de1, de4, by="Survey.Units", all=TRUE)
final.de3 <- merge(final.de2, de5, by="Survey.Units", all=TRUE)
final.de4 <- merge(final.de3, de6, by="Survey.Units", all=TRUE)
final.de5 <- merge(final.de4, de7, by="Survey.Units", all=TRUE)
final.de6 <- merge(final.de5, de8, by="Survey.Units", all=TRUE)
ces2016 <- merge(final.de6, de9, by="Survey.Units", all=TRUE)
ces2016$Survey.Units <- gsub("  ", " ", ces2016$Survey.Units) # Replace text to match others dataset
ces2016$Survey.Units[ces2016$Survey.Units=="Thakurgoan"] <- "Thakurgaon"
ces2016$Survey.Units[ces2016$Survey.Units=="Narsingdhi"] <- "Narsingdi"
ces2016$Survey.Units[ces2016$Survey.Units=="Moulvi Bazar"] <- "Moulavibazar"
ces2016$Year <- 2016 # Store year

# Data quality assessment
sum(duplicated(ces2016)) # Uniqueness: Check for duplicates

sum(is.na(ces2016)) # Completeness: Check for NAs
na_ces2016 <- data.frame(lapply(ces2016, function(y) sum(length(which(is.na(y)))))) # Show NAs by column

str(ces2016) # Validity: Check format and type

max(ces2016[,c(2:45)], na.rm=TRUE) # Validity: Check range (between 0 and 100)

# Save final dataset
write.csv(ces2016,"./output/unicef/data/data_unicef_ces_2016.csv", row.names=FALSE) # Save data

#----------------------------------------------------------------------------------------------------------
# Create/save metadata
#----------------------------------------------------------------------------------------------------------

ces <- list("ces2006"=ces2006, "ces2010"=ces2010, "ces2011"=ces2011, "ces2013"=ces2013, "ces2014"=ces2014, "ces2015"=ces2015, "ces2016"=ces2016)

meta_ces = data.frame()
for (i in seq_along(ces)){ # Select the variables of interest
  meta <- data.frame("Source"="CES", "File"= names(ces[i]),"Variable"=colnames(ces[[i]]), "Type"="Numeric, %")
  meta_ces <- rbind(meta_ces,meta)
}

write.csv(meta_ces,"./output/unicef/data/metadata_unicef_ces.csv", row.names=FALSE) # Save metadata
