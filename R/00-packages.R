# install necessary packages

if(!require(tidyverse))install.packages("tidyverse")
if(!require(sdmTMB))install.packages("sdmTMB", dependencies = TRUE)
if(!require(patchwork))install.packages("patchwork")
if(!require(here))install.packages("here")
if(!require(readxl))install.packages("readxl")
if(!require(sf))install.packages("sf")

if(!require(remotes))install.packages("remotes")
if(!require(ggeffects))remotes::install_github("seananderson/ggeffects", ref = "sdmTMB")
if(!require(ggsidekick))remotes::install_github("seananderson/ggsidekick")
