library(tidyverse)
library(visdat)
library(corrplot)

library(readr)
bc_data_prepared <- read_csv("Data/bc_data_prepared.csv")
View(bc_data_prepared)

table(bc_data_prepared$diagnosis)

library(dplyr)
bc_data_prepared <- bc_data_prepared %>%
  mutate(diagnosis = recode(diagnosis, B = 'Benign', M = 'Malignant' ))