# Accessibility Mapping in Google Earth Engine (GEE)
# 
# Dan Weiss, Malaria Atlas Project, University of Oxford
# 2017-11-06
#
# This script requires the gdistance package (van Etten, J. R Package gdistance: Distances and Routes on Geographical Grids. Journal of Statistical Software 76, 1-21)
#
# This script requires the two user supplied datasets:
# (a) The friction surface, which is available here:  http://www.map.ox.ac.uk/accessibility_to_cities/
# (b) A user-supplied .csv of points (i.e., known geographic coordinates) 
#
# Notes:
# (a) All file paths and names should be changed as needed.
# (b) Important runtime details can be found in the comments.
# (c) This script is suitable only for analyses of moderately sized areas for most (e.g., up to 10 million km^2 in lower latitude settings - GLOBAL RUNS WILL NOT WORK).
#     We recommend using Google Earth Engine for larger areas, with the exception of high-latitude areas where custom approaches are typically required.
#
# Citation: D.J. Weiss, A. Nelson, H.S. Gibson, W. Temperley, S. Peedell, A. Lieber, M. Hancher, E. Poyart, S. Belchior, N. Fullman, B. Mappin, U. Dalrymple, J. Rozier, 
# T.C.D. Lucas, R.E. Howes, L.S. Tusting, S.Y. Kang, E. Cameron, D. Bisanzio, K.E. Battle, S. Bhatt, and P.W. Gething. A global map of travel time to cities to assess 
# inequalities in accessibility in 2015. (2018). Nature. doi:10.1038/nature25181.
# 

## Required Packages
require(gdistance)

# User Defined Variables - used if clipping from the global layer, if no clipping is needed, see lines 54-55 (currently commented out).
# This could also be accomplished by importing a shapefile (for example) 
# Geographic Coordinates (WGS84)
left   <- -2.0
right  <- 0.0
bottom <- 50.0
top    <- 52.0
transition.matrix.exists.flag <- 0 # if the geo-corrected graph has already been made, this can save time.  Uses the same T.GC.filename as specified using the T.GC.filename variable.

# Input Files
friction.surface.filename <- 'E:\\accessibility\\friction_surface_2015_v1.0.tif'
point.filename <- 'E:\\accessibility\\points.csv' # Just 2 columns.  Structured as [X_COORD, Y_COORD] aka [LONG, LAT].  Use a header.

# Output Files
T.filename <- 'E:\\accessibility\\study.area.T.rds'
T.GC.filename <- 'E:\\accessibility\\study.area.T.GC.rds'
output.filename <- 'E:\\accessibility\\study.area.accessibility.tif'

# Read in the points table
points <- read.csv(file = point.filename)

# Fetch the number of points
temp <- dim(points)
n.points <- temp[1]

#  Define the spatial template
friction <- raster(friction.surface.filename)
fs1 <- crop(friction, extent(left, right, bottom, top))
# Use the following line instead of the preceding 2 if clipping is not needed (i.e., to run globally), but be warned that trying this will far exceed the computational capacity available to most users.
# fs1 <- raster(friction.surface.filename) 

# Make the graph and the geocorrected version of the graph (or read in the latter).
if (transition.matrix.exists.flag == 1) {
  # Read in the transition matrix object if it has been pre-computed
  T.GC <- readRDS(T.GC.filename)
} else {
  # Make and geocorrect the transition matrix (i.e., the graph)
  T <- transition(fs1, function(x) 1/mean(x), 8) # RAM intensive, can be very slow for large areas
  saveRDS(T, T.filename)
  T.GC <- geoCorrection(T)                    
  saveRDS(T.GC, T.GC.filename)
}

# Convert the points into a matrix
xy.data.frame <- data.frame()
xy.data.frame[1:n.points,1] <- points[,1]
xy.data.frame[1:n.points,2] <- points[,2]
xy.matrix <- as.matrix(xy.data.frame)

# Run the accumulated cost algorithm to make the final output map. This can be quite slow (potentially hours).
temp.raster <- accCost(T.GC, xy.matrix)

# Write the resulting raster
writeRaster(temp.raster, output.filename)