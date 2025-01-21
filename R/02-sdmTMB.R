library(tidyverse)
library(readxl)
library(sdmTMB)
library(mapview)

d <- read_csv(paste0("output/all-snake-skin-data.csv"))

mapview(d, xcol = "longitude", ycol = "latitude", zcol="present", crs = 4269, grid = FALSE)
mapview(d, xcol = "longitude", ycol = "latitude", zcol="common.name", crs = 4269, grid = FALSE)

ggplot(d) + geom_point(aes(longitude, latitude,
                           alpha = present,
                           colour = common.name))

# projection for all of North America
Albers <- "+proj=aea +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0
+datum=NAD83 +units=m +no_defs"

d <- sdmTMB::add_utm_columns(d, utm_crs = Albers)

mesh <- make_mesh(d, xy_cols = c("X", "Y"), cutoff = 500)

plot(mesh)

d$collector <- as.factor(d$collector)

m <- sdmTMB(present~common.name + (1|collector),
       time = "year",
       extra_time = sdmTMB:::find_missing_time(d$year),
       spatial = "off",
       spatiotemporal = "rw",
       anisotropy = FALSE,
       family = binomial(),
       mesh = mesh,
       data = d
       )

m
sanity(m)
# plot_anisotropy(m)

mean(d$year)
d$year_centered <- d$year - 1920
d$year_scaled <- (d$year - 1920) / sd(d$year)
d$year_sd <- sd(d$year)
mesh2 <- make_mesh(d, xy_cols = c("X", "Y"), cutoff = 100)

plot(mesh2)

# no collector effect
svc0 <- sdmTMB(present~common.name + year_scaled,
              spatial = "on",
              spatial_varying = ~year_scaled,
              spatiotemporal = "off",
              anisotropy = TRUE,
              family = binomial(),
              mesh = mesh2,
              data = d
)

# accounting for collector
# very similar effects
svc <- sdmTMB(present~common.name + year_scaled + (1|collector),
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

