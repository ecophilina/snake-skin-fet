library(tidyverse)
library(readxl)
library(sdmTMB)
library(mapview)

d <- read_csv(paste0("data/SS spatial analysis_23 July.csv")) |>
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


mapview(d, xcol = "longitude", ycol = "latitude", zcol="present", crs = 4269, grid = FALSE)
mapview(d, xcol = "longitude", ycol = "latitude", zcol="common.name", crs = 4269, grid = FALSE)

ggplot(d) + geom_point(aes(longitude, latitude,
                           alpha = present,
                           colour = common.name))

d <- d |> filter(latitude > 25)


# projection for all of North America
Albers <- "+proj=aea +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0
+datum=NAD83 +units=m +no_defs"

d <- sdmTMB::add_utm_columns(d, utm_crs = Albers)

mesh <- make_mesh(d, xy_cols = c("X", "Y"), cutoff = 200)

plot(mesh)

d$museum <- as.factor(d$museum)

## don't think this configuration makes much sense
# m <- sdmTMB(present~common.name + card.photo + museum,
#        time = "year",
#        extra_time = sdmTMB:::find_missing_time(d$year),
#        spatial = "off",
#        spatiotemporal = "rw",
#        anisotropy = FALSE,
#        family = binomial(),
#        mesh = mesh,
#        data = d
#        )
#
# m
# sanity(m)
# plot_anisotropy(m)

mean(d$year)
d$year_centered <- d$year - 1950
d$year_scaled <- (d$year - 1950) / sd(d$year)
d$year_sd <- sd(d$year)
mesh2 <- make_mesh(d, xy_cols = c("X", "Y"), cutoff = 100)

plot(mesh2)

svc0 <- sdmTMB(present~common.name + year_scaled + card.photo + (1|museum),
              spatial = "on",
              spatial_varying = ~year_scaled,
              spatiotemporal = "off",
              anisotropy = TRUE,
              family = binomial(),
              mesh = mesh2,
              data = d
)

svc0
sanity(svc0)
plot_anisotropy(svc0)

# museum wasn't doing anything so drop it
# should potentially have a true observer variable?
svc <- sdmTMB(present~common.name + year_scaled + card.photo,
            spatial = "on",
            spatial_varying = ~year_scaled,
            spatiotemporal = "off",
            anisotropy = TRUE,
            family = binomial(),
            mesh = mesh2,
            data = d
)

svc
sanity(svc)
plot_anisotropy(svc)

tidy(svc, conf.int = TRUE)
tidy(svc, effects = "ran_pars", conf.int = TRUE)

saveRDS(svc, "output/model-svc.rds")

m <- svc

## add code for QQ plots

