# data wrangle
library(readxl)
library(tidyverse)

prep_snake_skin_data <- function(dat) {

spp <- gsub(" ", "-", gsub("\\'", "", tolower(dat$`Common Name`[1])))

names(dat) <- tolower(names(dat))

d <- dat |> select(
  `catalog #`,
  `snake skin?`,
  `common name`,
  species,
  family,
  month,
  day,
  year,
  collector,
  `decimal latitude`,
  `decimal longitude`
)

d <- d |>
  rename(
    cat.num = `catalog #`,
    common.name = `common name`,
    latitude = `decimal latitude`,
    longitude = `decimal longitude`
  ) |>
  mutate(
    latitude = as.numeric(latitude), # this converts all NAs to proper format
    longitude = as.numeric(longitude),
    collector = ifelse(is.na(collector), "Not recorded", collector),
    present = case_when(
    `snake skin?` == "yes" ~ 1,
    `snake skin?` == "no" ~ 0,
    TRUE ~ NA
  ))

d <- d |> filter(!is.na(latitude), !is.na(longitude), !is.na(present))

write_delim(d, paste0("output/", spp, ".txt"))
write_csv(d, paste0("output/", spp, ".csv"))

## test that it worked
# x <- read_delim(paste0("output/", spp, ".txt"))
# x <- read_csv(paste0("output/", spp, ".csv"))

}


d0 <- read_excel("data/Bewick's Wren SS frequency.xlsx", skip = 4)
# View(d1)

d1 <- prep_snake_skin_data(d0)

d2 <- read_excel("data/Great-crested Flycatcher SS frequency.18 Feb 2022.xlsx", skip = 4) |> prep_snake_skin_data()

d3 <- read_excel("data/Tufted titmouse SS frequency.xlsx", skip = 4) |> prep_snake_skin_data()

d <- bind_rows(d1,d2,d3)

d |> group_by(collector) |> summarise(n = n()) |> View()

write_delim(d, paste0("output/all-snake-skin-data.txt"))
write_csv(d, paste0("output/all-snake-skin-data.csv"))
