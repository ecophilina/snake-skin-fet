
# data wrangle
library(tidyverse)

d <- read_csv(paste0("data/SS spatial analysis_23 July.csv")) |>
  mutate(species = ifelse(species == "bcti", "tuti", species)) |>
  filter(!is.na(latitude), !is.na(longitude), !is.na(ssy1.n0), !is.na(year)) |>
  mutate(present = ssy1.n0,
         common.name = case_when(
           `species` == "bewr" ~ "Bewick's Wren",
           `species` == "howr" ~ "House Wren",
           `species` == "gcfl" ~ "Great-crested Flycatcher",
           `species` == "tuti" ~ "Tufted Titmouse",
           `species` == "bcti" ~ "Black-crested Titmouse",
           TRUE ~ NA
         ))


write_csv(d, paste0("output/all-snake-skin-data-2026.csv"))

