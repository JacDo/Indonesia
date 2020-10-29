################################################################################
####### diagnostics ############################################################
################################################################################

###### set-up ##################################################################
library(sp)
library(rgdal)
library(sf)
library(raster)
library(tidyverse)
library(INLA)
library(furrr)
library(leaflet)
library(raster)
library(xtable)
library(rlist)
library(wesanderson)
library(htmlwidgets)
library(readxl)
library(haven)
library(rayshader)
library(assertthat)
library(TraMineRextras)
library(cowplot)

setwd("C:/Users/seufe/Dropbox/Unterlagen_Jacqueline/data")
load("tmp/inla.Rdata")
source("C:/Users/seufe/Dropbox/Unterlagen_Jacqueline/code/inla_function.R")

###### model selection #########################################################
result <- data.frame(result) %>%
  mutate_at(vars(2), as.numeric) %>%
  setNames(c("Model", "CPO"))
print(xtable(result, type = "latex", digits = 3), file = "tmp/cpo_inla.tex")
selection <- data.frame(
  result[1:2, 1:2], komolgorov,
  kurtosis
) %>%
  mutate_at(vars(2:4), as.numeric) %>%
  setNames(c("Model", "CPO", "Komolgorov-Smirnov", "Kurtosis"))

print(xtable(selection, type = "latex", digits = 3),
  file = "tmp/selection_inla.tex"
)
###### qq-plot #################################################################
ggplot(data.frame(
  x = qunif(ppoints(length(mod.mode$cpo$pit))),
  y = sort(mod.mode$cpo$pit)
)) +
  geom_point(aes(x, y)) +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles")

##### transformation ###########################################################

# optional: to obtain exact estimation, execute this code
# mean and sd cannot be transformed with simple exp()
# plan(multiprocess)
#
# summary <- future_map_dfr(res$marginals.fitted.values[index],
#   function(marg) {
#     # Transform post. marginals
#     aux <- inla.tmarginal(exp, marg)
#     # Compute posterior mean
#     inla.zmarginal(aux, silent = T)
#   },
#   .progress = TRUE
# )

##### metrics ##################################################################
median <- (res$summary.fitted.values[index, "0.5quant"])
quant0.025 <- (res$summary.fitted.values[index, "0.025quant"])
quant.975 <- (res$summary.fitted.values[index, "0.975quant"])
mean <- (res$summary.fitted.values[index, "mean"])
sd <- (res$summary.fitted.values[index, "sd"])
##### diagnostics INLA #########################################################
summary(res)
inla.show.hyperspec(res)
model0.res <- inla.spde2.result(res, "s", spde)
list_of_dataframes <- list(
  res$summary.fixed, res$summary.hyperpar,
  exp(model0.res$summary.log.range.nominal),
  exp(model0.res$summary.log.variance.nominal)
) %>%
  map(~ rownames_to_column(.x))

output <- data.table::rbindlist(list_of_dataframes, fill = T)
output <- output %>% dplyr::select(-c("kld", "ID"))
print(xtable(output, type = "latex", digits = 3),
  file = "tmp/output_inla.tex"
)

##### rasterize ################################################################
data <- data.frame(coop, median, mean, quant.975, quant0.025, sd)
names(data)[1:2] <- c("longitude", "latitude")
coordinates(data) <- ~ longitude + latitude
crs(data) <- crs(travel_time)
writeOGR(
  obj = data, dsn = "shape", layer = "result", driver = "ESRI Shapefile",
  overwrite_layer = T
)

result_shape <- readOGR("shape/result.shp")
quantile <- names(result_shape@data)
ext <- extent(travel_time)
# rasterize: rasterize the metrics
# @ quantile metric of choice
rasterize <- function(input_metric, shape, destination) {
  cmd_raster <-
    paste0(
      c(
        paste("gdal_rasterize"),
        paste("-a ", input_metric, sep = ""),
        paste(c("-te", ext[c(1, 3, 2, 4)]), collapse = " "),
        paste(c("-tr", 0.0449964, 0.0449964), collapse = " "),
        paste("-a_nodata NA"),
        paste(shape),
        paste(destination, input_metric, ".tif", sep = "")
      ),
      collapse = " "
    )
  return(cmd_raster)
}
cmd_raster <- sapply(quantile, function(x) {
  rasterize(x,
    shape = "shape/result.shp",
    destination = "tmp/result/result_"
  )
})
sapply(cmd_raster, system)

result <-
  paste("tmp/result/", list.files(
    path = "tmp/result",
    pattern = "result_.*\\.tif"
  ), sep = "") %>%
  map(~ raster(.))
##### data.frame for plotting ##################################################
map.p <- lapply(result, rasterToPoints)
df <- data.frame(list.cbind(map.p)) %>%
  dplyr::select(-starts_with(c("x.", "y.")))

# plotting: plot the quantiles
# @name of the metric
plotting <- function(name) {
  name_input <- ensym(name)
  ggplot(data = df, aes(y = y, x = x)) +
    geom_raster(aes(fill = !!name_input)) +
    coord_equal() +
    scale_fill_viridis_c(option = "inferno") +
    theme(
      panel.background = element_rect(
        fill = "lightblue",
        colour = "lightblue",
        size = 2, linetype = "solid"
      )
    ) +
    xlab("") +
    ylab("") +
    labs(fill = "log(risk)") +
    ggsave(paste(
      "tmp/result/coromap_",
      (str_replace(name, "result_", "")), ".png"
    ))
}
name_input <- names(df)[3:length(names(df))]
plot_list <- lapply(name_input, plotting)

plot_grid(plot_list[[1]], plot_list[[2]],
  plot_list[[3]], plot_list[[4]],
  plot_list[[5]],
  labels = c(
    "Mean", "Median", "97.5% Quantile",
    "2.5% Quantile", "Standard deviation"
  ),
  label_size = 10, nrow = 3, vjust = 0.8, hjust = -0.2
)

##### exceedance probability ###################################################
# exceedance: calculate exceedance probability
# @exc the value to be exceeded
# @res INLA object
# @index index of values to be predicted in INLA
exceedance <- function(exc, res, index) {
  exc <- sapply(res$marginals.fitted.values[index],
    FUN = function(marg) {
      1 - inla.pmarginal(q = exc, marginal = marg)
    }
  )
}

plan(multiprocess)
exc <- future_map_dfc(seq(-2.2, -3.4, by = -0.2), ~ exceedance(.x, res, index),
  .progress = T
)
# subceeding probability
opposite_exc <- 1 - exc
data_exc <- data.frame(coop, exc, opposite_exc)
names(data_exc)[1:2] <- c("longitude", "latitude")
names(data_exc)[3:9] <- paste("X", as.character(seq(-2.2, -3.4,
  by = -0.2
) * -1), sep = "_")
names(data_exc)[10:16] <- paste("X_opp", as.character(seq(-2.2, -3.4,
  by = -0.2
) * -1), sep = "_")

coordinates(data_exc) <- ~ longitude + latitude
crs(data_exc) <- crs(travel_time)
writeOGR(
  obj = data_exc, dsn = "shape", layer = "result_exc", driver = "ESRI Shapefile",
  overwrite_layer = T
)

result_exc <- readOGR("shape/result_exc.shp")
exceedance <- names(result_exc@data)
ext <- extent(travel_time)

cmd_raster <- sapply(exceedance, function(x) {
  rasterize(x,
    shape = "shape/result_exc.shp", destination = "tmp/result/exc"
  )
})
sapply(cmd_raster, system)

exc_input <-
  paste("tmp/result/", list.files(
    path = "tmp/result",
    pattern = "excX_\\d.*\\.tif"
  ), sep = "") %>%
  map(~ raster(.))

opp_exc_input <-
  paste("tmp/result/", list.files(
    path = "tmp/result",
    pattern = "excX_opp.*\\.tif"
  ), sep = "") %>%
  map(~ raster(.))
##### airport data #############################################################
data_flight <- read.csv("tmp/flight_risk.csv")
geo <- read.csv("tmp/domestic_air.csv")
flight <- geo %>%
  left_join(data_flight, by = c("iata" = "iata")) %>%
  dplyr::select(x = longitude, y = latitude, risk = risk_score)
##### visualize exceedance #####################################################
# P(log(risk)>|< exceedance)>larger_than
# plotting_exc: plot exceedance probabilty
# @ exc_filter keep only values larger than
# @ exceedance value to be subceeded/exceeding
# @ larger_than probability threshold to exceed/subceed a value
# @ opp indicating whether subceeding or exceeding probability
plotting_exc <- function(exc_filter, exceedance, larger_than, opp = T) {
  option <- ifelse(opp == T, "viridis", "inferno")
  direction <- ifelse(opp == T, -1, 1)
  ggplot(data = exc_filter, aes(y = y, x = x)) +
    geom_raster(aes(fill = cond)) +
    geom_point(
      data = flight, aes(y = y, x = x), color = "darkgreen", fill = "green",
      size = 0.4, shape = 21, stroke = 0.3
    ) +
    coord_equal() +
    scale_fill_viridis_c(
      option = option, na.value = "gray57",
      direction = direction
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
    labs(fill = "Prob") +
    ggsave(paste("tmp/result/coromap_exc_", ifelse(opp == T, "opp_", ""),
      exceedance, "_", larger_than, ".png",
      sep = ""
    ))
}
# plot_exceedance
# @ exceedance value to subceeded or exceeded
# @ larger_than probability threshold to subceed/exceed a value
# @ opp indicating whether subceeding or exceeding probability
plot_exceedance <- function(exceedance, larger_than, opp = T) {
  if (is.double(exceedance)) {
    exceedance_inp <- str_replace(as.character(exceedance), "\\.", "_")
  } else {
    exceedance_inp <- exceedance
  }
  exc <- raster(paste("tmp/result/excX_", ifelse(opp == T, "opp_", ""),
    exceedance_inp,
    ".tif",
    sep = ""
  ))
  exc <- rasterToPoints(exc)
  exc <- data.frame(exc)
  var <- paste("excX_", ifelse(opp == T, "opp_", ""), exceedance_inp, sep = "")
  var_sym <- ensym(var)
  exc_filter <- exc %>%
    mutate(cond = (ifelse(!!var_sym > larger_than,
      !!var_sym, NA
    )))
  plotting_exc(
    exc_filter = exc_filter, exceedance = exceedance,
    larger_than = larger_than, opp = opp
  )
}

input <- tidyr::crossing(
  exceedance = seq(2.2, 3.4, by = 0.2),
  larger_than = seq(0.6, 0.75, by = 0.05)
)

exc_plot <- map2(
  input$exceedance, input$larger_than,
  ~ plot_exceedance(.x, .y, opp = F)
)

opp_plot <- map2(
  input$exceedance, input$larger_than,
  ~ plot_exceedance(.x, .y, opp = T)
)

vis_input <- tidyr::crossing(exc = c(2.2, 3.4), prob = 0.75, opp = c(T, F))
visualization <- pmap(
  list(vis_input$exc, vis_input$prob, vis_input$opp),
  ~ plot_exceedance(..1, ..2, ..3)
)
plot_grid(
  plotlist = visualization, nrow = 2,
  labels = c(
    "P(log(risk)>-2.2)", paste(
      "P(log(risk)",
      as.character(expression("\u2264")),
      "-2.2)"
    ),
    "P(log(risk)>-3.4)",
    paste(
      "P(log(risk)",
      as.character(expression("\u2264")),
      "-3.4)"
    )
  ),
  label_size = 10, vjust = 1.2, hjust = -0.1
)

##### zoom plot ################################################################
raster_input <- raster("tmp/result/excX_3_4.tif")
raster::values(raster_input)[raster::values(raster_input) < 0.75] <- NA
pal <- colorNumeric("inferno", c(0.75, 1), na.color = "transparent")

zoom_plot <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addRasterImage(raster_input, colors = pal, opacity = 0.5) %>%
  addLegend("bottomright",
    pal = pal,
    values = raster::values(raster_input), title = "Exceedance prob."
  ) %>%
  addCircleMarkers(
    radius = 0.2, data = flight, lng = ~x, lat = ~y,
    color = "green"
  ) %>%
  addScaleBar(position = c("bottomleft"))

setwd(paste(getwd(), "/tmp/result", sep = ""))

saveWidget(zoom_plot, file = "zoom_plot.html")

##### validation ###############################################################
setwd("C:/Users/seufe/Dropbox/Unterlagen_Jacqueline/data")
##### external validation ODP/PDP ##############################################
val <- read_excel("tmp/result/lovita_excel.xlsx", sheet = 2)
val <- val %>%
  filter(!is.na(id_m))
##### population ###############################################################
pop_2011 <- read_stata("external/podes2014_population.dta")
pop_2014 <- readRDS("external/podes2014layer.Rds")
pop <- merge(pop_2014, pop_2011,
  by.x = "id.desa.podes.2014",
  by.y = "iddesapodes2014", all.x = T
)

pop_input <- pop@data %>%
  group_by(id_m) %>%
  summarise(population = sum(population_2014, na.rm = T))
validation <- val %>%
  left_join(pop_input, by = "id_m")
validation <- validation %>%
  filter(!is.na(population))
##### run on server ############################################################
# poly <- readOGR("/home/cloud/data/result/dissolve.shp")
#
# #create a raster stack
# s <- stack("/home/cloud/data/result/result_mean.tif")
#
# #extract raster cell count (sum) within each polygon area (poly)
# beginCluster()
# ex <- raster::extract(s, poly, fun=mean, na.rm=TRUE, df=TRUE)
# endCluster()
# df <- data.frame(ex)
# df$ID <- as.numeric(poly$id_m)
# write.csv(df, file = "/home/cloud/data/validation_cases.csv")
##### compare actual cases with results ########################################
cases <- read.csv("tmp/result/validation_cases.csv")
names(cases) <- c("index", "id_m", "mean_risk")
validation <- validation %>%
  left_join(cases, by = "id_m") %>%
  mutate(
    ODP_per_pop = as.numeric(ODP) / population,
    PDP_per_pop = as.numeric(PDP) / population
  )
# dissolved the vilalge shapefile to provinces in ArcMap
shape <- st_read("tmp/result/dissolve.shp")

validation <- validation %>%
  left_join(shape, by = "id_m")
validation <- st_as_sf(validation)
validation <- validation %>%
  dplyr::select(-c(
    "...1", "...11", "...12", "index", "Additional Information",
    "OBJECTID", "Shape_Leng", "Shape_Area"
  )) %>%
  mutate(ODP = as.numeric(ODP), PDP = as.numeric(PDP)) %>%
  arrange(desc(ODP), desc(PDP))

##### plot metrics #############################################################
mean <- ggplot(validation) +
  geom_sf(aes(fill = mean_risk), color = NA) +
  scale_fill_viridis_c(option = "inferno") +
  labs(fill = "Mean of \n log risk") +
  ggsave("tmp/result/coromap_agg_risk.png")

odp_per_pop <- ggplot(validation) +
  geom_sf(aes(fill = ODP_per_pop), color = NA) +
  scale_fill_viridis_c(option = "inferno") +
  labs(labs = "ODP per \n population")

plot_gg(odp_per_pop,
  multicore = TRUE, width = 5, height = 5, scale = 250, windowsize = c(1400, 866),
  zoom = 0.55, phi = 30
)
render_snapshot()

pdp_per_pop <- ggplot(validation) +
  geom_sf(aes(fill = PDP_per_pop), color = NA) +
  scale_fill_viridis_c(option = "inferno") +
  labs(fill = "PDP per \n population")

plot_gg(pdp_per_pop,
  multicore = TRUE, width = 5, height = 5, scale = 250, windowsize = c(1400, 866),
  zoom = 0.55, phi = 30
)
render_snapshot()

odp <- ggplot(validation) +
  geom_sf(aes(fill = as.numeric(ODP)), color = NA) +
  scale_fill_viridis_c(option = "inferno") +
  labs(fill = "ODP")

plot_gg(odp,
  multicore = TRUE, width = 5, height = 5, scale = 250, windowsize = c(1400, 866),
  zoom = 0.55, phi = 30
)
render_snapshot()

pdp <- ggplot(validation) +
  geom_sf(aes(fill = as.numeric(PDP)), color = NA) +
  scale_fill_viridis_c(option = "inferno") +
  labs(fill = "PDP")

plot_gg(pdp,
  multicore = TRUE, width = 5, height = 5, scale = 250, windowsize = c(1400, 866),
  zoom = 0.55, phi = 30
)
render_snapshot()
testi <- validation %>%
  st_drop_geometry()
##### correlation ##############################################################
input <- validation %>%
  st_drop_geometry() %>%
  dplyr::select(starts_with(c("ODP", "PDP")))
wilcox <- input %>%
  map(~ wilcox.test(.x, validation$mean_risk,
    paired = TRUE,
    alternative = "two.sided"
  ))

correlation <- input %>%
  map(~ cor(.x, validation$mean_risk,
    method = "spearman",
    use = "complete.obs"
  ))

wil <- map_dbl(wilcox, ~.$p.value)
correlation <- unlist(correlation)
val_concept <- data.frame(wilcox = wil, corr = correlation)
names(val_concept) <- c( "Wilcoxon test p-value",
                        "Spearman rank correlation")
print(xtable(val_concept, type = "latex", digits = 3),
      file = "tmp/validation.tex")

##### eps for publication ######################################################
convert.g(
  path = "tmp", fileroot = "*", from = "png",
  to = "eps", create.path = TRUE, options = NULL
)
convert.g(
  path = "tmp/result", fileroot = "*", from = "png",
  to = "eps", create.path = TRUE, options = NULL
)
