#----------------------------------------------------------------------------------------------------------
# dS_correlation.R
# Description: Correlation analysis of inputs variables and each outcome
# Organization: Health, Nutrition, and Population (HNP) | The World Bank
# Author: Hellen Matarazzo
# Date: Updated on 04-16-2019
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
if(!require(corrplot)) installed.packages("corrplot", dependencies=TRUE); library(corrplot)

#----------------------------------------------------------------------------------------------------------
# Load datasets
#----------------------------------------------------------------------------------------------------------

path <- list.files("./output/all/", pattern="*intersect_all.csv", full.names = TRUE) # List files
all <- lapply(path, read_csv)
names(all) <- c("anc", "mat", "under", "unmet")

path2 <- list.files("./output/all/", pattern="*union_all.csv", full.names = TRUE) # List files
all2 <- lapply(path2, read_csv)
names(all2) <- c("anc2", "mat2", "under2", "unmet2")

#----------------------------------------------------------------------------------------------------------
# Correlation coefficients
#----------------------------------------------------------------------------------------------------------

cor_anc <- cor(all[[1]][-c(1,2,14,20,21)],use = "complete.obs") # Create correlation matrix with numeric variables of intersec
cor_mat <- cor(all[[2]][-c(1,3,12,13,14)],use = "complete.obs")
cor_under <- cor(all[[3]][-c(1,8,14,15,16)],use = "complete.obs")
cor_unmet <- cor(all[[4]][-c(1,5,11,15,29,30)],use = "complete.obs")

cor_anc2 <- cor(all2[[1]][-c(1,88,90)],use = "complete.obs") # Create correlation matrix with numeric variables of union
cor_mat2 <- cor(all2[[2]][-c(1,62,63)],use = "complete.obs")
cor_under2 <- cor(all2[[3]][-c(1,70,71)],use = "complete.obs")
cor_unmet2 <- cor(all2[[4]][-c(1,117,119)],use = "complete.obs")

#----------------------------------------------------------------------------------------------------------
# Plotting heatmap from correlation coefficients matrix
#----------------------------------------------------------------------------------------------------------

jpeg(filename="./output/all/graphs/cor_anc.jpeg", units="in", width=10, height=10, res=300) # Save correlation heatmap
corrplot(cor_anc, method = "color", type = "lower", tl.col = "black", tl.cex=0.6, order = "alphabet", 
         addCoef.col = "black", mar=c(0,0,1,0), number.cex = 0.6, number.digits = 2)
dev.off()

jpeg(filename="./output/all/graphs/cor_mat.jpeg", units="in", width=10, height=10, res=300)
corrplot(cor_mat, method = "color", type = "lower", tl.col = "black", tl.cex=0.6, order = "alphabet", 
         addCoef.col = "black", mar=c(0,0,1,0), number.cex = 0.6, number.digits = 2)
dev.off()

jpeg(filename="./output/all/graphs/cor_under.jpeg", units="in", width=10, height=10, res=300)
corrplot(cor_under, method = "color", type = "lower", tl.col = "black", tl.cex=0.6, order = "alphabet", 
         addCoef.col = "black", mar=c(0,0,1,0), number.cex = 0.6, number.digits = 2)
dev.off()

jpeg(filename="./output/all/graphs/cor_unmet.jpeg", units="in", width=10, height=10, res=300)
corrplot(cor_unmet, method = "color", type = "lower", tl.col = "black", tl.cex=0.6, order = "alphabet", 
         addCoef.col = "black", mar=c(0,0,1,0), number.cex = 0.5, number.digits = 2)
dev.off()

#----------------------------------------------------------------------------------------------------------
# Save correlation coefficients matrix
#----------------------------------------------------------------------------------------------------------

write.csv(cor_anc,"./output/all/graphs/cor_anc.csv") # Save correlation matrix
write.csv(cor_mat,"./output/all/graphs/cor_mat.csv")
write.csv(cor_under,"./output/all/graphs/cor_under.csv")
write.csv(cor_unmet,"./output/all/graphs/cor_unmet.csv")

