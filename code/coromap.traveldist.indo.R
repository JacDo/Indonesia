######### setwd######
setwd("C:/Users/seufe/bla Dropbox/Jacqueline Seufert/Unterlagen_Jacqueline/data")

########## libraries#####

library(here)
library(raster)
library(rgdal)
library(ggplot2)
library(gdistance)
library(abind)
library(rje)
library(malariaAtlas)
library(tidyverse)

############# set up shapefile#########
analysis.shp <- readOGR("external/kap2015idm.corrected.shp")

plot(analysis.shp, main = "Shape for Clipping")

############### friction########

friction <- malariaAtlas::getRaster(
  surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015",
  shp = analysis.shp
)

malariaAtlas::autoplot_MAPraster(friction)
Tu <- gdistance::transition(friction, function(x) 1 / mean(x), 8)
Tu.GC <- gdistance::geoCorrection(Tu)

###### points and raster####

point.locations <- read.csv(file = "tmp/domestic_air.csv")
point.locations <- point.locations %>%
  dplyr::select(longitude, latitude, iata)
names(point.locations) <- c("X_COORD", "Y_COORD", "name")
coordinates(point.locations) <- ~ X_COORD + Y_COORD
proj4string(point.locations) <- proj4string(analysis.shp)
points <- as.matrix(point.locations@coords)
access.raster <- gdistance::accCost(Tu.GC, points)
writeRaster(access.raster, "travel_time.tif", overwrite = T)
############# plot#########

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
  ggsave("travel_time.png")

#######check raster#######
travel_time <- raster("travel_time.tif")
extent(travel_time)
###############
Sys.getenv("C:/OSGeo4W64/bin")
ext <- extent(travel_time)
crs <- crs(travel_time)

cmd_warp <- paste0(
  c(
    paste("gdalwarp"),
    paste(c("-te", ext[c(1, 3, 2, 4)]), collapse = " "),
    paste(c("-tr", 0.0449964, 0.0449964), collapse = " "),
    paste("-r average"),
    paste("-overwrite"),
    paste("travel_time.tif"),
    paste("travel_time_5x5.tif")
  ),
  collapse = " "
)
cmd_warp
system(cmd_warp)

############

data <- read.csv("tmp/flight_risk.csv")
geo <- read.csv("tmp/domestic_air.csv")

input <- geo %>%
  left_join(data, by = c("iata" = "departure.iata")) %>%
  dplyr::select(longitude, latitude, risk_index)

coordinates(input) <- ~ longitude + latitude
crs(input) <- crs(travel_time)
writeOGR(
  obj = input, dsn = "shape", layer = "risk_index", driver = "ESRI Shapefile",
  overwrite_layer = T
)
shape <- readOGR("shape/risk_index.shp")

cmd_raster <-
  paste0(
    c(
      paste("gdal_rasterize"),
      paste("-a risk_index"),
      paste(c("-te", ext[c(1, 3, 2, 4)]), collapse = " "),
      paste(c("-tr", 0.0449964, 0.0449964), collapse = " "),
      paste("shape/risk_index.shp"),
      paste("risk.tif")
    ),
    collapse = " "
  )
cmd_raster
system(cmd_raster)
risk <- raster("risk.tif")
plot(risk)
