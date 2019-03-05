#----------------------------------------------------------------------------------------------------------
# dp_unicef_.R
# Description: Data preprocessing of Unicef MICS 1 datasets
# Organization: Health, Nutrition, and Population (HNP) | The World Bank
# Author: Hellen Matarazzo
# Date: Updated on 03-04-2019
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

# if(!require(tabulizer)) install.packages("tabulizer", dependencies = TRUE); library(tabulizer)
if(!require(pdftools)) install.packages("pdftools", dependencies = TRUE); library(pdftools)
if(!require(tesseract)) install.packages("tesseract", dependencies = TRUE); library(tesseract)


# if(!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE); library(tidyverse)
# if(!require(haven)) install.packages("haven", dependencies = TRUE); library(haven)
# if(!require(survey)) install.packages("survey", dependencies = TRUE); library(survey)

#----------------------------------------------------------------------------------------------------------
# Load MICS datasets
#----------------------------------------------------------------------------------------------------------

list.files("./data/unicef/", recursive=T) # List all files on folder 'unicef' and its subfolders
path <- "data/unicef/MICS1/Bangladesh 1993 MICS_English.pdf"

img_file <- pdf_convert(path, format = 'png', pages = c(40:47), dpi = 400) # Render PDF to png image
text <- ocr(img_file) # Extract text from png image
cat(text)
write.csv(text, "test.csv")
