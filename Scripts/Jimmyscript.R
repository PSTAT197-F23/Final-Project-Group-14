library(tidyverse)
library(visdat)
library(corrplot)

library(readr)
# Data Preprocessing
bc_data_prepared <- read_csv("Data/bc_data_prepared.csv")
View(bc_data_prepared)

table(bc_data_prepared$diagnosis)

library(dplyr)
bc_data_prepared <- bc_data_prepared %>%
  mutate(diagnosis = recode(diagnosis, B = 'Benign', M = 'Malignant' ))

install.packages("dbscan", dependencies = FALSE)
library(dbscan)

actual_id_diagnosis <- bc_data_prepared[,c(1,2)]

#Make data set as a dataframe without character variable for scaling
scaled_bc_data <- as.data.frame(scale(bc_data_prepared[,-c(1,2)]))

#Use LOF function on the 569 observations to produce ratio of density around that specific observation vs average density of all point around it.
lof_result <- lof(scaled_bc_data)

# Adjust the threshold as needed
threshold <- 1.25

#Create an object that displayed logical values for whether or not the predicted LOF value is greater than or less than the threshold parameter
outliers <- lof_result > threshold