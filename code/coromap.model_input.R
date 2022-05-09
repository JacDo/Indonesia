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
library(here)
##### data #####################################################################
pop <- raster(here("Spatial", "data", "coromap_population.tif"))
raster::values(pop)[raster::values(pop) == 0] <- NA

traffic_den <- raster(here("Spatial", "data", "coromap_traffic_density.tif"))
risk <- raster(here("Spatial", "data", "coromap_risk.tif"))
travel_time <- raster(here("Spatial", "data", "coromap_travel_time.tif"))
java <- raster(here("Spatial", "data", "coromap_java.tif"))
java_dist <- raster(here("Spatial", "data", "coromap_distancejava.tif"))
raster <- stack(
  pop,
  travel_time,
  traffic_den, java_dist, java
)

##### rescale covariates #######################################################
for (i in 1:4) {
  raster::values(raster[[i]]) <- rescale(raster::values(raster[[i]]),
    to = c(0.01, 0.99)
  )
}
##### mask rasters to minimize NA ##############################################
raster <- mask(raster, calc(raster, fun = sum))

##### risk #####################################################################
data <- read.csv(here("GAM", "data", "flight_risk.csv"))
geo <- read.csv(here("GAM", "data", "domestic_air.csv"))
point <- geo %>%
  left_join(data, by = c("iata" = "iata")) %>%
  dplyr::select(
    x = longitude, y = latitude, risk_score = risk_score,
    risk_score_lower = risk_score_lower,
    risk_score_upper = risk_score_upper
  )
point$risk <- point$risk_score
point$risk_lower <- point$risk_score_lower
point$risk_upper <- point$risk_score_upper
##### transform airports in spatial points #####################################
sps <- SpatialPoints(point[, c("x", "y")],
  proj4string = CRS(as.character(crs(risk)))
)
point[, c("long", "lat")] <- coordinates(sps)
# subset travel_time
point_input <- cbind(point, raster::extract(raster, point[, c("long", "lat")]))
##### fill NAs by imputing the closest non-NA value ############################
source(here("code", "impute_function.R"))
names(raster) <- sapply(names(raster), function(x) {
  str_replace(x, "coromap\\_", "")
})
for (i in c("distancejava", "population", "traffic_density", "travel_time", "java")) {
  point_input[, i] <- future_sapply(fill_NA(
    r = raster[[i]],
    xy = point_input[, c("x", "y")]
  ), mean)
}
##### combine risk and covariates ##############################################
point_input <- point_input %>%
  dplyr::select(
    long, lat, population, risk, risk_lower, risk_upper, distancejava, traffic_density,
    travel_time, java
  )
# make coordinate dataframe
coo <- cbind(point$long, point$lat)
##### INLA######################################################################
##### mesh preparation #########################################################
shape <- readOGR(here("Spatial", "data", "kap2015idm.corrected.shp"))
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
  ggtitle(paste("Vertices: ", mesh$n)) 
  ggsave(here("Spatial", "plots", "coromap_mesh.pdf"),
    width = 1920 / 72 / 3, height = 1080 / 72 / 3,
    dpi = 72, limitsize = F
  )
##### INLA prep ################################################################

# % of maximal distance within the area that
# is unlikely to be below the range
prange <- 0.01
ratio0 <- 1 / 100
ratio1 <- ratio0 + 1 / 200
ratio2 <- ratio0 - 1 / 200
sigma0 <- 2
sigma1 <- 1.75
sigma2 <- 2.25
sigma <- unlist(sapply(ls(pattern = "sigma\\d"), get))
ratio <- unlist(sapply(ls(pattern = "ratio\\d"), get))
range <- sapply(ratio, function(x) {
  x * sqrt((max(point[, 1], na.rm = T) - min(point[, 1],
    na.rm = T
  ))^2 +
    (max(point[, 2], na.rm = T) - min(point[, 2],
      na.rm = T
    ))^2)
})

sigma_ratio <- expand.grid(sigma = sigma, ratio = range)
sigma_ratio <- sigma_ratio[c(1, 5:6, 8:9), ]

spde <- map2(
  sigma_ratio$sigma, sigma_ratio$ratio,
  ~ inla.spde2.pcmatern(
    mesh = mesh,
    prior.range = c(.y, prange), prior.sigma = c(.x, 0.01)
  )
)


indexs <- lapply(spde, function(x) inla.spde.make.index("s", x$n.spde))
A <- inla.spde.make.A(mesh = mesh, loc = coo)
dp <- rasterToPoints(raster)
coop <- dp[, c("x", "y")]
Ap <- inla.spde.make.A(mesh = mesh, loc = coop)
save.image(here("Spatial", "data", "input.Rds"))

##### airport data #############################################################
data_flight <- read.csv(here("GAM", "data", "flight_risk.csv"))
geo <- read.csv(here("GAM", "data", "domestic_air.csv"))
flight <- geo %>%
  left_join(data_flight, by = c("iata" = "iata")) %>%
  dplyr::select(x = longitude, y = latitude, risk = risk_score)
flight <- SpatialPoints(flight)
##### covariate plot ###########################################################
input_shape <- st_read(here("Spatial", "data", "dissolve.shp"))
input_shp <- as(input_shape, "Spatial")

raster_log <- calc(raster, fun = log)
pdf(
  file = here("Spatial", "plots", "coromap_population.pdf"),
  width = 16,
  height = 8
)
x.scale <- list(cex = 3)
y.scale <- list(cex = 3)

levelplot(raster_log[[1]],
  xlab = list(label = ""), ylab = list(label = ""),
  scales = list(x = x.scale, y = y.scale),
  main = list(
    "(a)",
    cex = 2.5
  ), margin = F, colorkey = list(space="bottom"),
  par.strip.text = list(cex = 0.9, lines = 2, fontface = "bold"),
  layout = c(1, 1)
) +
  latticeExtra::layer(sp.polygons(input_shp,
    fill = "gray", alpha = 0.2
  ))
dev.off()
pdf(
  file = here("Spatial", "plots", "coromap_travel_time.pdf"),
  width = 16,
  height = 8
)
levelplot(raster_log[[2]],
  xlab = list(label = ""), ylab = list(label = ""),
  scales = list(x = x.scale, y = y.scale),
  main = list(
    "(b)",
    cex = 2.5
  ), margin = F, colorkey =  list(space="bottom", labels=list(cex=3)),
  par.strip.text = list(cex = 0.9, lines = 2, fontface = "bold"),
  layout = c(1, 1)
) +
  latticeExtra::layer(sp.polygons(input_shp,
    fill = "gray", alpha = 0.2
  ))+
  latticeExtra::layer(sp.points(flight,
                                pch = 16, col = "green",
                                cex = 2
  ))
dev.off()

pdf(
  file = here("Spatial", "plots", "coromap_traffic.pdf"),
  width = 16,
  height = 8
)

x.scale <- list(cex = 3.2)
y.scale <- list(cex = 3.2)
levelplot(raster_log[[3]],
  xlab = list(label = ""), ylab = list(label = ""),
  scales = list(x = x.scale, y = y.scale),
  main = list(
    "(c)",
    cex = 2.5
  ), margin = F, colorkey = F,
  par.strip.text = list(cex = 0.9, lines = 2, fontface = "bold"),
  layout = c(1, 1)
) +
  latticeExtra::layer(sp.polygons(input_shp,
    fill = "gray", alpha = 0.2
  ))
dev.off()
pdf(
  file = here("Spatial", "plots", "coromap_dist_java.pdf"),
  width = 16,
  height = 8
)

x.scale <- list(cex = 3.2)
y.scale <- list(cex = 3.2)
levelplot(raster_log[[4]],
  xlab = list(label = ""), ylab = list(label = ""),
  scales = list(x = x.scale, y = y.scale),
  main = list(
    "(d)",
    cex = 2.5
  ), margin = F, colorkey = F,
  par.strip.text = list(cex = 0.9, lines = 2, fontface = "bold"),
  layout = c(1, 1)
) +
  latticeExtra::layer(sp.polygons(input_shp,
    fill = "gray", alpha = 0.2
  ))
dev.off()

##### airport plot #############################################################
geo <- read.csv(here("Spatial", "GAM", "domestic_air.csv"))


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
  ggsave(here("Spatial", "plots", "coromap_airports.pdf"),
    width = 1920 / 72, height = 1080 / 72, dpi = 72, limitsize = F
  )
