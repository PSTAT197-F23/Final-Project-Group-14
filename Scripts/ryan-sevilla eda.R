library(ggplot2)
library(dplyr)
library(corrplot)

breast_cancer_data <- read.csv("Data/bc_data_prepared.csv")

## CODE IN VIGNETTE

# Count plot of each diagnosis class 
bc_count <- breast_cancer_data %>%
  mutate(diagnosis = factor(ifelse(diagnosis == "M", "Malignant", "Benign"))) %>%
  ggplot(aes(x = diagnosis)) +
  geom_bar() +
  labs(x = "Diagnosis",
       y = "Number of Patients")
bc_count

# Variable correlation matrix of numeric features
corrplot(corr = cor(breast_cancer_data[-c(1,2)]))



## MISCELLANEOUS SCRATCH WORK (not to be included in final script)
bc_correlations <- as.data.frame(cor(breast_cancer_data[-c(1,2)]))
bc_correlations_very_high <- bc_correlations
bc_correlations_high <- bc_correlations
bc_correlations_very_high[bc_correlations_very_high < 0.9] <- ""
bc_correlations_high[bc_correlations_high < 0.7 | bc_correlations_high > 0.9] <- ""