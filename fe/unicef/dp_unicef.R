
# ### The script outline --------------------------------------------------
PATH_MICS3 = "OneDrive_1_17-10-2018/MICS3/Bangladesh 2006 MICS_Datasets/Bangladesh MICS 2006 SPSS Datasets/"
PATH_MICS5 = "OneDrive_1_17-10-2018/MICS5/Bangladesh_MICS5_Datasets/Bangladesh MICS 2012-13 SPSS Datasets/"

# ### Libraries -----------------------------------------------------------
library(haven)





# ### Getting UNICEF MICS Data --------------------------------------------
mics3_data <- list()
mics5_data <- sapply


mics5_data <- list.files(PATH, full.names = T)
mics5_data
unicef <- sapply(mics5_data,read_sav)
names(unicef) <- list.files(PATH) 
View(unicef$ch.sav)

test_function <- function(x){summary(as.factor())}
sapply(unicef$hh.sav, test_function)
unicef$hh.sav <- as.data.frame(unicef$hh.sav)
for (i in seq_along(unicef$hh.sav[,1:20])) {
  print(i)
  print(i)
  print(i)
  print(summary(as.factor(unicef$hh.sav[,i])))
  }
  
