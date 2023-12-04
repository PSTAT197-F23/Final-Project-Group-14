library(tidyverse)
library(visdat)
library(corrplot)

bc_data_headers <- c('id', 'diagnosis', 'radius_mean', 'texture_mean', 'perimeter_mean',
                      'area_mean', 'smoothness_mean', 'compactness_mean', 'concavity_mean',
                      'concave points_mean', 'symmetry_mean', 'fractal_dimension_mean',
                      'radius_se', 'texture_se', 'perimeter_se', 'area_se', 'smoothness_se',
                      'compactness_se', 'concavity_se', 'concave points_se', 'symmetry_se',
                      'fractal_dimension_se', 'radius_worst', 'texture_worst',
                      'perimeter_worst', 'area_worst', 'smoothness_worst',
                      'compactness_worst', 'concavity_worst', 'concave points_worst',
                      'symmetry_worst', 'fractal_dimension_worst')

bc_data <- read_csv("Data/breast+cancer+wisconsin+diagnostic/wdbc.data", col_names = bc_data_headers)

write_csv(bc_data, "Data/bc_data_prepared.csv")


class_counts <- bc_data |>
  summarise(n(), .by = diagnosis)

class_dist_plot <- bc_data |>
  summarise(count = n(), .by = diagnosis) |>
  mutate(diagnosis = recode(diagnosis, "M" = "Malignant", "B" = "Benign")) |>
  ggplot(aes(x = diagnosis, y = count, fill = diagnosis)) +
  geom_bar(stat = "identity") +
  labs(title = "Breast Cancer Diagnosis Distribution", x = "Diagnosis", y = "Count", fill = "Diagnosis")

na_value_visual <- vis_miss(bc_data) # No missing data according to this

data_heatmap <- bc_data |>
  select(-diagnosis) |>
  cor() |>
  corrplot()
