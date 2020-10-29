################################################################################
################# impute missing values for raster #############################
################################################################################

##### simple impute_raster #####################################################
# impute missing values
# impute_raster
# @ x,y locations
# @ layer raster layer
# @ raster raster
impute_raster <- function(x, y, layer, raster) {
    xy <- cbind(x, y)
    raster[[layer]] <- raster[[layer]][which.min(replace(
        distanceFromPoints(
            raster[[layer]],
            xy
        ),
        is.na(raster[[layer]]), NA
    ))]
}
##### impute large scale stacks, parallelization necessary #####################
# impute_raster
# @x,xy locations
# @ raster raster
# # or larger scale stacks

fill_NA <- function(raster, xy) {
    plan(multisession)
    future_apply(
        X = xy, MARGIN = 1,
        FUN = function(xy) {
            raster[which.min(replace(
                distanceFromPoints(raster, xy),
                is.na(raster), NA
            ))]
        }
    )
}