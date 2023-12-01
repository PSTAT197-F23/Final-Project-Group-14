library(tidyverse)

network_data <- read_csv("Data/network_data_file_columns_named.csv")

duration_distribution <- summary(network_data$duration)

protocoltype_distribution <- network_data |>
  select(protocoltype) |>
  mutate(protocoltype = as_factor(protocoltype)) |>
  summarise(count = n(), .by = protocoltype) |>
  ggplot() +
  geom_bar(aes(x = protocoltype, y = count, fill = protocoltype), stat = "identity")
