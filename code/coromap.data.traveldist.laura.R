library(here)
library(raster)
library(rgdal)
library(ggplot2)
library(gdistance)
library(abind)
library(rje)
library(malariaAtlas)
library(tidyverse)

#######################
### analytical area ###
#######################
d<-extent(c(89.50, 93.0, 24.89, 26.38))
d.bb<-d

##################
### Meghalaya ####
##################
### crop to province extent
d <- readOGR(here("data/external/India_States_ADM1_GADM-shp/3e563fd0-8ea1-43eb-8db7-3cf3e23174512020330-1-layr7d.ivha.shp"), stringsAsFactors = F)
names(d) <- tolower(names(d))
names(d)
unique(d$name_1)
i <- which(d$name_1 %in%  c("Meghalaya"))
d <- d[i, ]
writeOGR(obj = d, dsn = "data/tmp/", layer = "meghalaya", driver = "ESRI Shapefile", overwrite_layer = T)

# #####################
# ### indian states ###
# #####################
# d <- readOGR(here("data/external/India_States_ADM1_GADM-shp/3e563fd0-8ea1-43eb-8db7-3cf3e23174512020330-1-layr7d.ivha.shp"), stringsAsFactors = F)
# names(d)<-tolower(names(d))
# names(d)
# d<-crop(d,d.bb)
# d.states<-d


############################
# ### rasterize meghalaya ###
# ##########################
# # extent
# d <- readOGR("data/tmp/meghalaya.shp")
# ext <- extent(d)
# # resolution
# if (Sys.info()["nodename"] == "Eliass-MBP") {
#   zip_path <- "/Users/eliascis/Dropbox/kairos/research_supplements/database/traveldistance/accessibility-weiss/2015_accessibility_to_cities_v1.0.zip"
# }
# if (Sys.info()["nodename"] == "LAPTOP-SC08IO0V") {
#   zip_path <- "C:/Users/seufe/bla Dropbox/Jacqueline Seufert/accessibility-weiss/2015_accessibility_to_cities_v1.0.zip"
# }
# unzip(zip_path, list = TRUE)
# t <- tempdir()
# unzip(zip_path, "2015_accessibility_to_cities_v1.0.tif", exdir = t)
# r <- raster(file.path(t, "2015_accessibility_to_cities_v1.0.tif"))
# res <- res(r)
# 
# cmd <- paste0(
#   c(
#     "gdal_rasterize",
#     "-a ID_1",
#     paste(c("-te", ext[c(1, 3, 2, 4)]), collapse = " "),
#     paste(c("-tr", res), collapse = " "),
#     here("data/external/India_States_ADM1_GADM-shp/3e563fd0-8ea1-43eb-8db7-3cf3e23174512020330-1-layr7d.ivha.shp"),
#     file.path("data/tmp/r.meghalaya.tif")
#   ),
#   collapse = " "
# )
# cmd
# system(cmd)
# r <- raster("data/tmp/r.meghalaya.tif")
# r[r != 23] <- 0
# writeRaster(r, "data/tmp/r.meghalaya.tif", overwrite = T)

######################
#### accessability ###
######################
## read
if (Sys.info()["nodename"] == "Eliass-MBP") {
  zip_path <- "/Users/eliascis/Dropbox/kairos/research_supplements/database/traveldistance/accessibility-weiss/2015_accessibility_to_cities_v1.0.zip"
}
if (Sys.info()["nodename"] == "LAPTOP-SC08IO0V") {
  zip_path <- "C:/Users/seufe/bla Dropbox/Jacqueline Seufert/accessibility-weiss/2015_accessibility_to_cities_v1.0.zip"
}
unzip(zip_path, list = TRUE)
t <- tempdir()
unzip(zip_path, "2015_accessibility_to_cities_v1.0.tif", exdir = t)
r <- raster(file.path(t, "2015_accessibility_to_cities_v1.0.tif"))
crs(r)

## crop extent
# d.meg <- readOGR("data/tmp/meghalaya.shp")
r <- raster::crop(r, extent(d.bb))

# ## set NA values
# r.meg <- raster("data/tmp/r.meghalaya.tif")
# dim(r.meg)
# dim(r)
# r[r.meg[] == 0] <- NA

## save
writeRaster(r, "data/tmp/r.traveldist.laura.general.tif", overwrite = T)


############################
### plot accessabiltiy #####
# ############################
# # theme
# theme.map.meghalaya <-
#   ggplot() +
#   theme_bw() +
#   theme(
#     # panel.background = b,
#     # panel.background = element_rect(fill = "lightblue", colour = "lightblue", size = 0.5, linetype = "solid"),
#     # panel.background = element_rect(fill = "grey30", colour = "grey30", size = 0.5, linetype = "solid"),
#     legend.title = element_blank(),
#     legend.position = "right",
#     axis.title = element_blank(),
#     axis.text = element_blank()
#     # axis.ticks=element_blank()
#   ) #+
# # labs(x="long",y="lat") +
# # labs(x="",y="") +
# # ggsn::north(x.min=90, x.max=93, y.min=25, y.max=26,scale=0.15,symbol=3) #+
# # ggsn::scalebar(x.min=95, x.max=142, y.min=-10.5, y.max=5.96, dist=500, dist_unit="km", model='WGS84',location="bottomleft",st.size=3,transform=T,box.color=NA)
# 
# 
# ## data
# r.acc <- raster("data/tmp/r.traveldist.laura.general.tif")
# # l.world<-readOGR("data/external/world001.shp")
# l.meg <- readOGR("data/tmp/meghalaya.shp")
# # x <-gSimplify(l.meg,tol=0.02, topologyPreserve=TRUE)
# # l.meg <- SpatialPolygonsDataFrame(x, data=l.meg@data)
# d <- read.csv("data/external/coordinates_refpoints.csv")
# # d<-d[d$name %in% c("Shillong","Cherrapunji"),]
# d.mis <- d
# 
# ## plotting pixel
# # plot(r.acc)
# # plot(l.meg,add=T)
# # rdd<-data.frame(rasterToPoints(r.acc))
# 
# p <-
#   theme.map.meghalaya +
#   geom_tile(data = data.frame(rasterToPoints(r.acc)), aes(x = x, y = y, fill = r.acc)) +
#   geom_polygon(data = fortify(l.meg), aes(x = long, y = lat, group = id), fill = NA, color = "black") +
#   geom_point(data = d.mis, aes(x = longitude, y = latitude)) +
#   geom_text(data = d.mis, aes(x = longitude, y = latitude, label = name), nudge_x = -0.18) +
#   coord_fixed(xlim = c(89.95, 92.7), ylim = c(25.07, 26.13), ratio = 1.3) +
#   scale_fill_gradientn(colours = rev(terrain.colors(10))) #+
# p
# ggsave(plot = p, "pub/figures/fig.traveldist.laura.pdf", device = "pdf", height = 15, width = 30.8, units = "cm")
# 
# 
# # @Jacqueline: please try to make the map more smooth
# b) add create a differnt color scheem that gives less weight to high values. You can do this by manipulating the
# scale_fill_gradientn() or by manipulating the values, I always found the latter one easier.


###############################
#### access points ############
###############################

## read destination points
d <- read.csv("data/external/coordinates_refpoints.csv")
# d<-d[d$name %in% c("Guwahati","Shillong"),]
# d <- d[d$name %in% c("Shillong", "Cherrapunji"), ]
d.mis <- d

####################
### friction map ###
####################
if (Sys.info()["nodename"] == "Eliass-MBP") {
  zip_path <- "/Users/eliascis/Dropbox/kairos/research_supplements/database/traveldistance/accessibility-weiss/2015_friction_surface_v1.zip"
}
if (Sys.info()["nodename"] == "LAPTOP-SC08IO0V") {
  zip_path <- "C:/Users/seufe/bla Dropbox/Jacqueline Seufert/accessibility-weiss/2015_friction_surface_v1.zip"
}
unzip(zip_path, list = TRUE)
t <- tempdir()
unzip(zip_path, "2015_friction_surface_v1.geotiff", exdir = t)
r <- raster(file.path(t, "2015_friction_surface_v1.geotiff"))
crs(r)
##crop
r<-crop(r,d.bb)
r.fric<-r
malariaAtlas::autoplot_MAPraster(r.fric)


#######################
### travel distance ###
#######################

Ti <- gdistance::transition(r.fric, function(x) 1 / mean(x), 8)
T.GC <- gdistance::geoCorrection(Ti)

a<-lapply(
  1:nrow(d.mis),
  function(i){
  # i<-1
  point.locations <- d.mis[i, ]
  names(point.locations) <- c("name", "Y_COORD", "X_COORD")
  coordinates(point.locations) <- ~ X_COORD + Y_COORD
  proj4string(point.locations) <- proj4string(analysis.shp)
  points <- as.matrix(point.locations@coords)
  access.raster <- gdistance::accCost(T.GC, points)
  writeRaster(access.raster,paste0("data/tmp/","r.traveldist.laura.",d.mis[i,"name"],".tif"))
  return(access.raster)
  }
)
a.tdist<-a

#####################
### plot maps #######
#####################
theme.map.meghalaya <-
  ggplot() +
  theme_bw() +
  theme(
    # panel.background = b,
    # panel.background = element_rect(fill = "lightblue", colour = "lightblue", size = 0.5, linetype = "solid"),
    # panel.background = element_rect(fill = "grey30", colour = "grey30", size = 0.5, linetype = "solid"),
    legend.title = element_blank(),
    legend.position = "right",
    axis.title = element_blank(),
    axis.text = element_blank()
    # axis.ticks=element_blank()
  ) #+
# labs(x="long",y="lat") +
# labs(x="",y="") +
# ggsn::north(x.min=90, x.max=93, y.min=25, y.max=26,scale=0.15,symbol=3) #+
# ggsn::scalebar(x.min=95, x.max=142, y.min=-10.5, y.max=5.96, dist=500, dist_unit="km", model='WGS84',location="bottomleft",st.size=3,transform=T,box.color=NA)

##state border
l.meg <- readOGR("data/tmp/meghalaya.shp")
##reference points
d <- read.csv("data/external/coordinates_refpoints.csv")
d.mis <- d
##read village coordinates
d <- read.csv("data/external/villagesid_coordinates.csv")
names(d)
d <- d[, c("village.id", "latitude", "longitude")]
d <- unique(d)
d.vill<-d

rlist<-c(
  "data/tmp/r.traveldist.laura.general.tif",
  "data/tmp/r.traveldist.laura.Cherrapunji.tif",
  "data/tmp/r.traveldist.laura.Guwahati.tif",
  "data/tmp/r.traveldist.laura.Shillong.tif"
)


a<-lapply(
  rlist,
  function(f){
    # f<-rlist[2]
    r<-raster(f) 
    r[]<-asinh(r[])
    rf<-data.frame(rasterToPoints(r))
    names(rf)[3]<-"val"
    p <-
      theme.map.meghalaya +
      geom_tile(data = rf, aes(x = x, y = y, fill = val)) +
      geom_polygon(data = fortify(l.meg), aes(x = long, y = lat, group = id), fill = NA, color = "black") +
      geom_point(data = d.vill, aes(x = longitude, y = latitude),color="black",size=0.8) +
      geom_point(data = d.mis, aes(x = longitude, y = latitude),color="green") +
      geom_text(data = d.mis, aes(x = longitude, y = latitude, label = name), nudge_x = -0.18) +
      coord_fixed(xlim = c(89.95, 92.7), ylim = c(25.07, 26.13), ratio = 1.3) +
      scale_fill_gradientn(
        colors = rev(rje::cubeHelix(
          gamma = 1.0,
          start = 1.5,
          r = -1.0,
          hue = 1.5,
          n = 16
        )),
        name = "Minutes \n of Travel"
      ) #+

      # scale_fill_gradientn(colours = rev(terrain.colors(10))) #+
    p
    n<-sub("r.traveldist.laura","fig.traveldist.laura",n)
    n<-sub("data/tmp","pub/figures",n)
    n<-sub("\\.tif",".pdf",n)
    ggsave(plot = p,filename=n, device = "pdf", height = 15, width = 30.8, units = "cm")
    return(p)
  }
)
a


# @Jacqueline: please try to make the map more smooth
# b) add create a differnt color scheem that gives less weight to high values. You can do this by manipulating the
# scale_fill_gradientn() or by manipulating the values, I always found the latter one easier.



################ friction ################
# analysis.shp <- l.meg
# plot(analysis.shp, main = "Shape for Clipping")
# friction <- malariaAtlas::getRaster(
#   surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015",
#   shp = analysis.shp
# )
# malariaAtlas::autoplot_MAPraster(friction)
# Ti <- gdistance::transition(friction, function(x) 1 / mean(x), 8)
# T.GC <- gdistance::geoCorrection(Ti)

# 
# travel_time <- function(i) {
#   ## Point locations
#   # i<-2
#   point.locations <- d.mis[i, ]
#   names(point.locations) <- c("name", "Y_COORD", "X_COORD")
#   coordinates(point.locations) <- ~ X_COORD + Y_COORD
#   proj4string(point.locations) <- proj4string(analysis.shp)
#   points <- as.matrix(point.locations@coords)
#   access.raster <- gdistance::accCost(T.GC, points)
# 
#   plot <- malariaAtlas::autoplot_MAPraster(access.raster,
#                                            shp_df = analysis.shp, printed = F
#   )
#   full_plot <- plot[[1]] + geom_point(
#     data = data.frame(point.locations@coords),
#     aes(x = X_COORD, y = Y_COORD)
#   ) +
#     geom_text(
#       data = data.frame(point.locations@coords),
#       aes(x = X_COORD, y = Y_COORD, label = d.mis$name[i]), nudge_x = -0.35
#     ) +
#     scale_fill_gradientn(
#       colors = rev(rje::cubeHelix(
#         gamma = 1.0,
#         start = 1.5,
#         r = -1.0,
#         hue = 1.5,
#         n = 16
#       )),
#       name = "Minutes \n of Travel"
#     ) +
#     ggtitle("Travel Time to Most Accessible Peak") +
#     theme(
#       axis.text = element_blank(),
#       panel.border = element_rect(fill = NA, color = "white")
#     )
#   print(full_plot)
# }
# 
# c(1:3) %>% map(~ travel_time(.))
### include Guwahati in the shapefile





#################################################
### retrieve distances by village coordinates ###
#################################################
## read village coordinates
d <- read.csv("data/external/villagesid_coordinates.csv")
names(d)
d <- d[, c("village.id", "latitude", "longitude")]
d <- unique(d)

rlist<-c(
  "data/tmp/r.traveldist.laura.general.tif",
  "data/tmp/r.traveldist.laura.Cherrapunji.tif",
  "data/tmp/r.traveldist.laura.Guwahati.tif",
  "data/tmp/r.traveldist.laura.Shillong.tif"
)
a<-lapply(rlist,raster)
a.tdist<-a

dd<-data.frame(cbind(
  # d,
  raster::extract(a.tdist[[1]],d[,c("longitude","latitude")]),
  raster::extract(a.tdist[[2]],d[,c("longitude","latitude")]),
  raster::extract(a.tdist[[3]],d[,c("longitude","latitude")]),
  raster::extract(a.tdist[[4]],d[,c("longitude","latitude")])
))
n<-sub("data/tmp/r.traveldist.laura.","traveldist.minutes.",rlist)
n<-sub(".tif","",n)
n<-tolower(n)
names(dd)<-n
dd
d<-cbind(d,dd)
write.csv(d,"data/tmp/traveldist.villages.csv")

