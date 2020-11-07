################################################################################
######### prepare data for analysis ############################################
################################################################################

##### set-up ###################################################################
library(sp)
library(future.apply)
library(rgdal)
library(raster)
library(tidyverse)
library(INLA)
library(scales)
library(ggthemes)
library(inlabru)
library(furrr)
library(sf)
library(ggrepel)
library(rasterVis)
setwd("C:/Users/seufe/Dropbox/Unterlagen_Jacqueline/data")
##### data #####################################################################
traffic_den <- raster("tmp/coromap_traffic_density.tif")
risk <- raster("tmp/coromap_risk.tif")
travel_time <- raster("tmp/coromap_travel_time.tif")
java <- raster("tmp/coromap_java.tif")
java_dist <- raster("tmp/coromap_distancejava.tif")
raster <- stack(
  travel_time,
  traffic_den, java_dist, java
)
##### rescale covariates #######################################################
for (i in 1:3) {
  raster::values(raster[[i]]) <- rescale(raster::values(raster[[i]]),
    to = c(0.01, 0.99)
  )
}
##### mask rasters to minimize NA ##############################################
raster <- mask(raster, calc(raster, fun = sum))
##### risk #####################################################################
data <- read.csv("tmp/flight_risk.csv")
geo <- read.csv("tmp/domestic_air.csv")
point <- geo %>%
  left_join(data, by = c("iata" = "iata")) %>%
  dplyr::select(x = longitude, y = latitude, risk = risk_score)
point$risk <- rescale(point$risk, to = c(0.01, 0.99))

##### transform airports in spatial points #####################################
sps <- SpatialPoints(point[, c("x", "y")],
  proj4string = CRS(as.character(crs(risk)))
)
point[, c("long", "lat")] <- coordinates(sps)
# subset travel_time
point_input <- cbind(point, raster::extract(raster, point[, c("long", "lat")]))
##### fill NAs by imputing the closest non-NA value ############################
source("C:/Users/seufe/Dropbox/Unterlagen_Jacqueline/code/impute_function.R")
names(raster) <- sapply(names(raster), function(x) {
  str_replace(x, "coromap\\_", "")
})
for (i in c("distancejava", "traffic_density", "travel_time", "java")) {
  point_input[, i] <- future_sapply(fill_NA(
    r = raster[[i]],
    xy = point_input[, c("x", "y")]
  ), mean)
}
##### combine risk and covariates ##############################################
point_input <- point_input %>%
  dplyr::select(
    long, lat, risk, distancejava, traffic_density,
    travel_time, java
  )
# make coordinate dataframe
coo <- cbind(point$long, point$lat)
##### INLA######################################################################
##### mesh preparation #########################################################
shape <- readOGR("external/kap2015idm.corrected.shp")
bndry <- inla.sp2segment(
  shape,
  crs = crs(shape)
)
##### Build the mesh ###########################################################
max.edge <- 0.9
# set the length of the boundary extension
bound.outer <- 0.4
# build the mesh
mesh <- inla.mesh.2d(
  boundary = bndry,
  loc = coo,
  max.edge = c(0.2, 80),
  cutoff = 0.39,
  offset = c(max.edge, bound.outer),
  min.angle = 39
)
out <- inla.mesh.assessment(mesh,
  spatial.range = 5,
  alpha = 2,
  dims = c(200, 200)
)
print(names(out))
range(out$sd.dev, na.rm = T)
##### Plot the mesh ############################################################
names(point)[1:2] <- c("x", "y")
mesh_plot <- ggplot(point) +
  gg(mesh) +
  geom_point(aes(x = x, y = y, col = risk), size = 3) +
  scale_color_gradient2(
    midpoint = 0.5, low = "darkgreen", mid = "yellow",
    high = "red2", space = "Lab"
  ) +
  coord_fixed() +
  theme_map() +
  labs(color = "Risk") +
  ggtitle(paste("Vertices: ", mesh$n)) +
  ggsave("tmp/coromap_mesh.pdf",
    width = 1920 / 72 / 3, height = 1080 / 72 / 3,
    dpi = 72, limitsize = F
  )
##### INLA prep ################################################################
ratio <- 1 / 100
# % of maximal distance within the area that
# is unlikely to be below the range
range0 <- ratio * sqrt((max(point[, 1], na.rm = T) - min(point[, 1],
  na.rm = T
))^2 +
  (max(point[, 2], na.rm = T) - min(point[, 2],
    na.rm = T
  ))^2)
prange <- 0.01
spde <- inla.spde2.pcmatern(mesh,
  prior.range = c(range0, prange), prior.sigma = c(2, 0.01)
)
indexs <- inla.spde.make.index("s", spde$n.spde)
A <- inla.spde.make.A(mesh = mesh, loc = coo)
dp <- rasterToPoints(raster)
coop <- dp[, c("x", "y")]
Ap <- inla.spde.make.A(mesh = mesh, loc = coop)
save.image("tmp/input.Rds")
##### covariate plot ###########################################################
pdf(
  file = "tmp/coromap_covariates.pdf",
  width = 8,
  height = 4
)
levelplot(raster[[1:3]],
  xlab = "", ylab = "",
  names.attr = c(
    "(a)",
    "(b)", "(c)"
  ),
  par.strip.text = list(cex = 1.5, lines = 2, fontface = "bold")
)
dev.off()
##### airport plot #############################################################
geo <- read.csv("tmp/domestic_air.csv")
input_shape <- st_read("tmp/dissolve.shp")

ggplot() +
  geom_sf(data = input_shape, fill = "bisque") +
  geom_label_repel(
    data = geo, aes(x = longitude, y = latitude, label = iata),
    size = 15, label.size = 0.01, hjust = 0.3, nudge_x = 0.01,
    color = "darkgreen", segment.colour = "red"
  ) +
  theme(
    panel.background = element_rect(
      fill = "slategray1",
      colour = "slategray1",
      size = 2, linetype = "solid"
    )
  ) +
  xlab("") +
  ylab("") +
  ggsave("tmp/coromap_airports.pdf",
    width = 1920 / 72, height = 1080 / 72, dpi = 72, limitsize = F
  )
