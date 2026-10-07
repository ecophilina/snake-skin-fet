library(tidyverse)
library(readr)
library(sdmTMB)
library(mapview)

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

mesh <- make_mesh(d, xy_cols = c("X", "Y"), cutoff = 100)

plot(mesh)

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

d$year_sd <- 50
d$year_centered <- d$year - 1925
d$year_scaled <- d$year_centered / d$year_sd

d$museum <- as.factor(d$museum)
d$common.name <- as.factor(d$common.name)

# try random effect for museum but doesn't ever converge so we'll ignore that
m_all <- sdmTMB(present ~ common.name*year_scaled + card.photo + (1|museum),
                time = "common.name",
                spatial = "on",
                spatiotemporal = "iid",
                share_range = FALSE,
                spatial_varying = ~year_scaled:common.name,
                anisotropy = TRUE,
                family = binomial(),
                mesh = mesh,
                data = d
)
sanity(m_all)
m_all
plot_anisotropy(m_all)
tidy(m_all, effects = "ran_pars", conf.int = TRUE)




# try dropping museums with very few samples

table(d$museum, d$common.name)

d2 <- filter(d, !is.na(museum)) |>
  filter(!(museum %in% c("UCM", "UWBM", "FMNH"))) |> # didn't help so drop
  mutate(museum = as.factor(as.character(museum)), # drops missing levels
        common.name = as.factor(as.character(common.name))
  )
table(d2$museum, d2$common.name)

mesh2 <- make_mesh(d2, xy_cols = c("X", "Y"), cutoff = 100)

plot(mesh2)

m <- sdmTMB(present ~ common.name*year_scaled + card.photo + (1|museum),
                time = "common.name",
                spatial = "on",
                spatiotemporal = "iid",
                share_range = FALSE,
                spatial_varying = ~year_scaled:common.name,
                anisotropy = TRUE,
                family = binomial(),
                mesh = mesh2,
                data = d2
)
sanity(m)
m

tidy(m, effects = "ran_pars", conf.int = TRUE)
# Still can't estimate sigma_G/random intercept SD
# give up using random effect for museum
# should potentially have a true observer variable?

# look for temporal change
# add back museums with few samples

d <- d

mesh3 <- make_mesh(d, xy_cols = c("X", "Y"), cutoff = 80)

plot(mesh3)

svc_full <- sdmTMB(present~common.name*year_scaled + card.photo,
                   time = "common.name",
                   spatial = "on",
                   spatiotemporal = "iid",
                   spatial_varying = ~year_scaled:common.name,
                   share_range = FALSE,
              anisotropy = TRUE,
              family = binomial(),
              mesh = mesh3,
              data = d
)

sanity(svc_full)
svc_full
plot_anisotropy(svc_full)

tidy(svc_full, conf.int = TRUE)
tidy(svc_full, effects = "ran_pars", conf.int = TRUE)

saveRDS(svc_full, "output/model-svc-full.rds")

svc_full1 <- sdmTMB(present~common.name*year_scaled + card.photo,
                   time = "common.name",
                   spatial = "on",
                   spatiotemporal = "iid",
                   spatial_varying = ~year_scaled:common.name,
                   # share_range = FALSE,
                   anisotropy = TRUE,
                   family = binomial(),
                   mesh = mesh3,
                   data = d
)

m_no_time <- sdmTMB(present~common.name + card.photo,
                    time = "common.name",
                    spatial = "on",
                    spatiotemporal = "iid",
                    share_range = FALSE,
                    anisotropy = TRUE,
                    family = binomial(),
                    mesh = mesh3,
                    data = d
)

saveRDS(m_no_time, "output/model-no-time.rds")

svc <- sdmTMB(present~common.name*year_scaled + card.photo,
              time = "common.name",
              spatial = "on",
              spatiotemporal = "iid",
              spatial_varying = ~year_scaled,
              # share_range = FALSE,
              anisotropy = TRUE,
              family = binomial(),
              mesh = mesh3,
              data = d
)

sanity(svc)
svc
plot_anisotropy(svc)

tidy(svc, conf.int = TRUE)
tidy(svc, effects = "ran_pars", conf.int = TRUE)

saveRDS(svc, "output/model-svc.rds")

svc <- sdmTMB(present~common.name*year_scaled + card.photo,
              time = "common.name",
              spatial = "on",
              spatiotemporal = "iid",
              spatial_varying = ~year_scaled,
              # share_range = FALSE,
              anisotropy = TRUE,
              family = binomial(),
              mesh = mesh3,
              data = d
)

sanity(svc)
svc
plot_anisotropy(svc)

tidy(svc, conf.int = TRUE)
tidy(svc, effects = "ran_pars", conf.int = TRUE)

saveRDS(svc, "output/model-svc.rds")


m_time <- sdmTMB(present ~ common.name*year_scaled + card.photo,
                time = "common.name",
                spatial = "on",
                spatiotemporal = "iid",
                # share_range = FALSE,
                anisotropy = TRUE,
                family = binomial(),
                mesh = mesh3,
                data = d
)


sanity(m_time)
m_time
plot_anisotropy(m_time)

tidy(m_time, conf.int = TRUE)
tidy(m_time, effects = "ran_pars", conf.int = TRUE)


saveRDS(m_time, "output/model-time.rds")


m_time2 <- sdmTMB(present ~ common.name + year_scaled + card.photo,
               time = "common.name",
               spatial = "on",
               spatiotemporal = "iid",
               # spatial_varying = ~year_scaled,
               # share_range = FALSE,
               anisotropy = TRUE,
               family = binomial(),
               mesh = mesh3,
               data = d
)


sanity(m_time2)

# AIC(svc_full, svc_full1, svc, m_time, m_time2, m_no_time)

AIC(svc_full, svc, m_time, m_time2, m_no_time)

# df      AIC
# svc_full  20 2822.611
# svc_full1 18 2837.105
# svc       15 2833.209
# m_time    14 2833.605
# m_time2   11 2852.982
# m_no_time 11 2854.959


## add code for QQ plots

rmarkdown::render("R/03-svc-plotting-2026.Rmd")

rmarkdown::render(
  "R/03-svc-plotting-2026.Rmd",
  output_format = "html_document"
)
