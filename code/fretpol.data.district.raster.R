library(here)
# library(devtools)
# install_github("eliascis/ecRutils")
library(rgdal)
library(dplyr)
library(raster)

###rasterize district layer 


########################################
### based on hansen 2016 raster file ###
########################################
##read data
l.districts<-readOGR(here("data/external/kap2015idm.corrected.shp"))
# cmd<-paste0("gdalinfo ",file.path(databasefolder,"deforestation","hansen","h.2018.indonesia.lossyear.tif"))
# writeClipboard(cmd)
# system(cmd)
40075*1000/360*0.0002499997
40075*1000/360*0.0002500008
res(r)

##choose resolution
# 1km resolution 
360/40075.15
1/110.574 
#0.25 km resolution
r.res<-c(0.009,0.009)/4


##choose extent
l.ext<-extent(l.districts)

##rasterize
cmd<-f.gdal.rasterize(
  a="id_m",
  te=paste(l.ext[1],l.ext[3],l.ext[2],l.ext[4],collapse=""),
  tr= paste(r.res,collapse=" "),
  ot="Int16",
  co="Compress=LZW",
  src_datasource=here("data/external/kap2015idm.corrected.shp"),
  dst_filename=here("data/store/fretpol.district.tif")
)
cmd
writeClipboard(paste0(cmd,collaplse="\n"))
system(cmd)

##test
r<-raster(here("data/store","fretpol.data.district.raster.tif"))
r
table(r[]==0)
table(is.na(r[]))
length(unique(r[]))-1


