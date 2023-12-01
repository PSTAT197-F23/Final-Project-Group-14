library(tidyverse)

network_data <- read_csv("Data/network_data_file_columns_named.csv")

attack_distribution_binomial <- network_data |>
  select(attack) |> 
  mutate(attack = as_factor(attack),
         attack = if_else(attack == "normal", "normal", "attack")) |>
  summarise(count = n(), .by = attack) |>
  ggplot() +
  geom_bar(aes(x = attack, y = count, fill = attack), stat = "identity")


duration_distribution <- summary(network_data$duration)

protocoltype_distribution <- network_data |>
  select(protocoltype) |>
  mutate(protocoltype = as_factor(protocoltype)) |>
  summarise(count = n(), .by = protocoltype) |>
  ggplot() +
  geom_bar(aes(x = protocoltype, y = count, fill = protocoltype), stat = "identity")



donor_data <- read_csv("Data/KDD2014_donors_10feat_nomissing_normalised.csv")

class_distribution <- donor_data |>
  mutate(class = as_factor(class)) |>
  summarise(count = n(), .by = class) |>
  ggplot() + 
  geom_bar(aes(x = class, y = count, fill = class), stat = "identity")

network_intrusion_data <- read_csv("Data/UNSW_NB15_training-set.csv") |>
  mutate(attack_cat = if_else(attack_cat == "Normal", "Normal", "Attack"))

nsw_attack_distribution
