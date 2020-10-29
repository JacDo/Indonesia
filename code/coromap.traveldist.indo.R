################################################################################
########## prepare rasters #####################################################
################################################################################

##### set-up ###################################################################
library(raster)
library(rgdal)
library(ggplot2)
library(gdistance)
library(abind)
library(rje)
library(malariaAtlas)
library(tidyverse)
library(sf)
library(furrr)
library(haven)
library(prioritizr)
library(rayshader)
library(viridis)

setwd("C:/Users/seufe/Dropbox/Unterlagen_Jacqueline/data")

##### set up shapefile #########################################################
analysis.shp <- readOGR("external/kap2015idm.corrected.shp")
ext <- extent(analysis.shp)
##### nightlight ###############################################################
nightlight <- raster("external/night.tif")
# Global nightlight has been cropped to Indonesia in ArcMap using Spatial Analyst
cmd_warp_nightlight <- paste0(
  c(
    paste("gdalwarp"),
    paste(c("-te", ext[c(1, 3, 2, 4)]), collapse = " "),
    paste(c("-tr", 0.0449964, 0.0449964), collapse = " "),
    paste("-srcnodata 128"),
    paste("-dstnodata NA"),
    paste("-r average"),
    paste("-overwrite"),
    paste("external/night.tif"),
    paste("tmp/coromap_nightlight.tif")
  ),
  collapse = " "
)

system(cmd_warp_nightlight)
nightlight <- raster("tmp/coromap_nightlight.tif")
##### population ###############################################################
pop_2011 <- read_stata("external/podes2014_population.dta")
pop_2014 <- readRDS("external/podes2014layer.Rds")
pop <- merge(pop_2014, pop_2011,
  by.x = "id.desa.podes.2014",
  by.y = "iddesapodes2014", all.x = T
)
pop@data <- pop@data %>%
  mutate(
    area = raster::area(pop) / 1000000, popu = population_2014,
    population = popu / area
  ) %>%
  dplyr::select(population)

writeOGR(
  obj = pop, dsn = "shape", layer = "population", driver = "ESRI Shapefile",
  overwrite_layer = T
)
population <- readOGR("shape/population.shp")

cmd_raster_pop <-
  paste0(
    c(
      paste("gdal_rasterize"),
      paste("-a population"),
      paste(c("-te", ext[c(1, 3, 2, 4)]), collapse = " "),
      paste(c("-tr", 0.0449964, 0.0449964), collapse = " "),
      paste("-a_nodata NA"),
      paste("shape/population.shp"),
      paste("tmp/coromap_population.tif")
    ),
    collapse = " "
  )

system(cmd_raster_pop)
##### traffic density ##########################################################
traffic_den <- raster("tmp/traffic.tif")
cmd_warp_traffic <- paste0(
  c(
    paste("gdalwarp"),
    paste(c("-te", ext[c(1, 3, 2, 4)]), collapse = " "),
    paste(c("-tr", 0.0449964, 0.0449964), collapse = " "),
    paste("-r average"),
    paste("-overwrite"),
    paste("tmp/traffic.tif"),
    paste("tmp/coromap_traffic_density.tif")
  ),
  collapse = " "
)
system(cmd_warp_traffic)
##### travel time ##############################################################
title <- paste(c("A global friction surface enumerating land-based travel"),
  c("speed for a nominal year 2015"),
  collapse = " "
)
friction <- malariaAtlas::getRaster(
  surface = title,
  shp = analysis.shp
)

malariaAtlas::autoplot_MAPraster(friction)
Tu <- gdistance::transition(friction, function(x) 1 / mean(x), 8)
Tu.GC <- gdistance::geoCorrection(Tu)
# points
point.locations <- read.csv(file = "tmp/domestic_air.csv")
point.locations <- point.locations %>%
  dplyr::select(longitude, latitude, iata)
names(point.locations) <- c("X_COORD", "Y_COORD", "name")
coordinates(point.locations) <- ~ X_COORD + Y_COORD
proj4string(point.locations) <- proj4string(analysis.shp)
points <- as.matrix(point.locations@coords)
# export raster
access.raster <- gdistance::accCost(Tu.GC, points)
writeRaster(access.raster, "tmp/travel_time_1x1.tif", overwrite = T)
# plot
p <- malariaAtlas::autoplot_MAPraster(access.raster,
  shp_df = analysis.shp, printed = F
)

full_plot <- p[[1]] + geom_point(
  data = data.frame(point.locations@coords),
  aes(x = X_COORD, y = Y_COORD)
) +
  ggtitle("Travel Time to Most Accessible Peak") +
  theme(
    axis.text = element_blank(),
    panel.border = element_rect(fill = NA, color = "white")
  ) +
  ggsave("tmp/travel_time.png")

cmd_warp_travel <- paste0(
  c(
    paste("gdalwarp"),
    paste(c("-te", ext[c(1, 3, 2, 4)]), collapse = " "),
    paste(c("-tr", 0.0449964, 0.0449964), collapse = " "),
    paste("-r average"),
    paste("-overwrite"),
    paste("tmp/travel_time_1x1.tif"),
    paste("tmp/coromap_travel_time.tif")
  ),
  collapse = " "
)
system(cmd_warp_travel)
travel_time <- raster("tmp/coromap_travel_time.tif")
##### fill in missing values ###################################################
source("C:/Users/seufe/Dropbox/Unterlagen_Jacqueline/code/impute_function.R")
ind <- !is.na(raster::values(nightlight)) & is.na(raster::values(travel_time))
raster::values(travel_time)[ind] <- -0.34
raster <- stack(travel_time, nightlight)
point <- data.frame(rasterToPoints(raster))
point <- point %>% filter(coromap_travel_time == -0.34)
raster$coromap_travel_time[ind] <- NA

plan(multisession)
input <- future_map2(point$x, point$y,
  ~ impute_raster(.x, .y, raster = raster, layer = "coromap_travel_time"),
  .progress = T
)
input <- map(input, mean) %>% flatten_dbl()
raster$coromap_travel_time[ind] <- input
writeRaster(raster$coromap_travel_time,
  "tmp/coromap_travel_time.tif",
  overwrite = T
)

##### contained in java and distance ###########################################
java <- st_read("tmp/java.shp")
java_raster <- travel_time
java <- intersecting_units(java_raster, java)
raster::values(java_raster)[which(!is.na(raster::values(java_raster)))] <- 0
raster::values(java_raster)[java] <- 1
writeRaster(java_raster, "tmp/coromap_java.tif", overwrite = T)
