# Load in required libraries
library(ggplot2)
library(dplyr)
library(corrplot)
library(tidyverse)

# Set working directory
setwd("~/Documents/UCSB/2023-2024/Fall 2023/PSTAT 197/Projects/Final Project/vignette-anomaly-detection/Data")

# Read data in
breast_cancer_data <- read_csv("bc_data_prepared.csv")

# Count plot of each diagnosis class
bc_count <- breast_cancer_data %>%
  mutate(diagnosis = factor(ifelse(diagnosis == "M", "Malignant", "Benign"))) %>%
  ggplot(aes(x = diagnosis)) +
  geom_bar() +
  labs(x = "Diagnosis",
       y = "Number of Patients",
       title = "Breast Cancer Diagnosis Distribution")
bc_count

# Variable correlation matrix of numeric features
corrplot(corr = cor(breast_cancer_data[-c(1,2)]))


## Isolation Forests:

#install.packages("isotree")
library(isotree)
# Split the data set into benign and malignant
benign_data <- subset(breast_cancer_data, diagnosis == 'B')
malignant_data <- subset(breast_cancer_data, diagnosis == 'M')

# Remove the 'diagnosis' column for modeling
benign_data <- benign_data[,-which(names(benign_data) == "diagnosis")]
malignant_data <- malignant_data[,-which(names(malignant_data) == "diagnosis")]

# Train the model on benign data
set.seed(123) 
model <- isolation.forest(benign_data, ntrees=100)

# Apply the model to malignant data]
malignant_scores <- predict(model, malignant_data)

# Determine outliers
outliers <- malignant_scores > 0.5

# Calculate the accuracy
true_positive <- sum(outliers) 
false_negative <- length(outliers) - true_positive

# Accuracy
accuracy <- true_positive / length(outliers)

# Print the results
print(paste("Number of true positives (malignant identified as outliers):", true_positive))
print(paste("Number of false negatives (malignant not identified as outliers):", false_negative))
print(paste("Accuracy:", accuracy))

# Convert scores to a data frame for ggplot
scores_df <- data.frame(score = malignant_scores)

# Plot
ggplot(scores_df, aes(x = seq_along(score), y = malignant_scores)) +
  geom_point() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Anomaly Scores with Threshold",
       x = "Data Point",
       y = "Anomaly Score")


# Perform PCA on the data
pca_result <- prcomp(malignant_data[,-c(1, 2)], scale = TRUE)

# Get the first two principal components and add anomaly scores
pca_data <- data.frame(PC1 = pca_result$x[, 1], PC2 = pca_result$x[, 2], Score = malignant_scores)

# Creating the scatter plot
ggplot(pca_data, aes(x = PC1, y = PC2, color = malignant_scores)) +
  geom_point(alpha = 0.7) +
  scale_color_gradientn(colors = brewer.pal(9, "Reds")) +
  theme_minimal() +
  labs(title = "PCA Scatter Plot with Anomaly Scores",
       x = "Principal Component 1",
       y = "Principal Component 2")


## Local Outlier Factor

# Install required packages
library(dbscan)

# Check dataset dimensions
dim(breast_cancer_data)
library(dbscan)

# Convert dataset to a tibble
breast_cancer_data %>% tibble()

# Create dataframe so we can later determine performance of model
actual_id_diagnosis <- breast_cancer_data[,c(1,2)]

#Make data set as a dataframe without character variable for scaling
scaled_bc_data <- as.data.frame(scale(breast_cancer_data[,-c(1,2)]))
#Use LOF function on the 569 observations to produce ratio of density around that specific observation vs average density of all point around it.
lof_result <- lof(scaled_bc_data)
# Adjust the threshold as needed
threshold <- 1.25
#Create an object that displayed logical values for whether or not the predicted LOF value is greater than or less than the threshold parameter
outliers <- lof_result > threshold

#Visualize Outliers
plot(lof_result, pch = 19, col = ifelse(outliers, "red", "blue"), 
     main = "LOF Outlier Detection", xlab = "Data Point", ylab = "LOF Score")
legend("topright", legend = c("Outlier", "Inlier"), col = c("red", "blue"), 
       pch = 19)


#Append the outlier results to the actual diagnosis of the data set
actual_id_diagnosis$pred_diag <- outliers

#Prediction vs Actual Table
actual_id_diagnosis %>%
  select(diagnosis, pred_diag) %>%
  table()


## One Class SVM

# Load necessary packages
library(tinytex)
library(readr)
library(tidyverse)
library(e1071)
library(caret)
library(NLP)
library(tm)

# let's first relabe our interest class into 1s and 0s

breast_cancer_data <- breast_cancer_data %>%
  
  mutate(diagnosis = ifelse(diagnosis == 'B', 1, 0))

# Separate predictors and response variable

X <- breast_cancer_data[, -which(names(breast_cancer_data) == "diagnosis")]

y <- breast_cancer_data$diagnosis

# Perform PCA

pca_result <- prcomp(X, scale. = TRUE)

# Extract the first two principal components

pca_data <- as.data.frame(pca_result$x[, 1:2])

pca_data$diagnosis <- factor(y, levels = c(0, 1), labels = c("M", "B"))               

# Create a scatter plot using the first two principal components

ggplot(pca_data, aes(x = PC2, y = PC1, color = diagnosis)) +
  geom_point() +
  scale_color_manual(values = c("red", "blue")) +
  labs(title = "PCA Scatterplot of Breast Cancer Data", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()

# Create SVM Model that we can visualize
svm_model_PCA <- svm(diagnosis ~ ., data = pca_data,
                     kernel = 'radial')

plot(svm_model_PCA, pca_data)

# Splitting data into training and testing sets for one-class SVM
training_data <- breast_cancer_data %>% 
  filter(diagnosis == 1)

testing_data <- breast_cancer_data %>%
  filter(diagnosis == 0)


trainpredictors <- training_data[, -which(names(training_data) =="diagnosis")]
testpredictors <- testing_data[, -which(names(testing_data) == "diagnosis")]

# Train one-class SVM model
svm.model <- svm(trainpredictors, y = NULL, type = 'one-classification', 
                 nu = 0.10, scale = TRUE, kernel = "radial")

# Look at summary of one-class SVM model
summary(svm.model)

# Do predications based on trained model
svm.predtrain <- predict(svm.model, trainpredictors)
svm.predtest <- predict(svm.model, testpredictors)


# Create confusion matrix for one-class SVM model
confTrain <- table(Predicted = svm.predtrain, Reference =
                     training_data$diagnosis)
confTest <- table(Predicted = svm.predtest, Reference = 
                    testing_data$diagnosis)

print("Confusion Matrix for Training Set:")
print(confTrain)


print("Confusion Matrix for Testing Set:")
print(confTest)


accuracy_train <- sum(svm.predtrain == training_data$diagnosis) / length(training_data$diagnosis) * 100
accuracy_test <- sum(svm.predtest == testing_data$diagnosis) / length(testing_data$diagnosis) * 100
cat("Accuracy on Training Set: ", accuracy_train, "%\n")
cat("Accuracy on Testing Set: ", accuracy_test, "%\n")


