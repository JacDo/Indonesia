#########setwd######
library(sf)
setwd("C:/Users/seufe/bla Dropbox/Jacqueline Seufert/Unterlagen_Jacqueline/data/external/idn_adm_bps_20200401_shp")

##########libraries#####

library(here)
library(raster)
library(rgdal)
library(ggplot2)
library(gdistance)
library(abind)
library(rje)
library(malariaAtlas)
library(tidyverse)

#############set up shapefile#########
analysis.shp <- readOGR("idn_admbnda_adm0_bps_20200401.shp")

plot(analysis.shp, main = "Shape for Clipping")

###############friction########

friction <- malariaAtlas::getRaster(
  surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015",
  shp = analysis.shp)

malariaAtlas::autoplot_MAPraster(friction)
Tu <- gdistance::transition(friction, function(x) 1 / mean(x), 8)
Tu.GC <- gdistance::geoCorrection(Tu)

######points and raster####
setwd("C:/Users/seufe/bla Dropbox/Jacqueline Seufert/Unterlagen_Jacqueline/data/tmp")
point.locations <- read.csv(file = "domestic_air.csv")
point.locations <- point.locations %>%
  dplyr::select(longitude, latitude, iata)
names(point.locations) <- c("X_COORD", "Y_COORD", "name") 
coordinates(point.locations) <- ~ X_COORD + Y_COORD
proj4string(point.locations) <- proj4string(analysis.shp)
points <- as.matrix(point.locations@coords)
access.raster <- gdistance::accCost(Tu.GC, points)
writeRaster(access.raster, "travel_time.tif", overwrite = T)
#############plot#########

p <- malariaAtlas::autoplot_MAPraster(access.raster,
  shp_df = analysis.shp, printed = F)

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


travel_time <- raster("travel_time.tif")
crs(travel_time)
crs(analysis.shp)



setwd("C:/Users/seufe/bla Dropbox/Jacqueline Seufert/Unterlagen_Jacqueline/data/external/idn_adm_bps_20200401_shp")

test <-raster("2015_friction_surface_v1.geotiff")
test <- crop(test, analysis.shp)

cmd<-paste0(
  c(
    "gdal_rasterize",
    "-a ID_2",
    paste(c("-te",extent(test)[c(1,3,2,4)]),collapse=" "),
    paste(c("-tr",res(test)),collapse=" "),
    file.path('idn_admbnda_adm0_bps_20200401.shp'),
    file.path("test.tif")
  ),
  collapse=" "
)
cmd

