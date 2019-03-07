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
#Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_x/")
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

# area2010a <- locate_areas("./data/unicef/CES/EPI Coverage Evaluation Survey 2010.pdf", pages=234) # Find area
# area2010a <- list(c(103.65442, 88.51324, 632.99643, 569.55030)) # Set area
# ces2010a <- extract_tables(temp[4], pages=234, area = area2010a, output = "data.frame", guess=FALSE) # Extract data
# ces2010a[[1]] <- ces2010a[[1]][-c(31),] # Drop messy row
# d1 <- ces2010a[[1]]
# str(d1)
# colnames(d1)[1] <- "District" # Rename variables
                   
#area2010a <- locate_areas("./data/unicef/CES/EPI Coverage Evaluation Survey 2010.pdf", pages=236) # Find area
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
d3[34,1] <- "Rajshahi New Division"
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

ces2010a[[26]][48,] <- c("National", 91.9)
ces2010a[[27]][33,] <- c("SCC", 87.6)
colnames(ces2010a[[26]]) <- colnames(ces2010a[[27]]) <- c("Survey.Units", # Set up column names
                                                        "PAB_Mother0-11MChildren")
d5 <- bind_rows(ces2010a[[26]],ces2010a[[27]])
d5[58,1] <- "Chittagong Division"
d5 <- d5[-c(50),] # Drop messy row
d5[2] <- as.numeric(unlist(d5[2]))

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

ces2010a[[30]]
ces2010a[[31]]


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

#-------------------------------------------------
# ces2011a <- extract_tables(temp[5], pages=199:216)
# ces2011b <- extract_tables(temp[5], pages=220:233)
# ces2013a <- extract_tables(temp[6], pages=201-220)
# ces2013b <- extract_tables(temp[6], pages=226:239)
# ces2014 <- extract_tables(temp[8], pages=231:264)
# ces2015 <- extract_tables(temp[7], pages=215:242)
# ces2016 <- extract_tables(temp[1], pages=231:260)