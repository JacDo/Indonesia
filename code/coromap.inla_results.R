################################################################################
####### diagnostics ############################################################
################################################################################

###### set-up ##################################################################
library(sp)
library(rasterVis)
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
library(data.table)
library(RColorBrewer)
library(htmlwidgets)
library(readxl)
library(haven)
library(rayshader)
library(assertthat)
library(TraMineRextras)
library(cowplot)
library(moments)
library(here)
library(MapPalettes)

load(here("Spatial", "data", "inla.RData"))
source(here("code", "inla_function.R"))
analysis.shp <- readOGR(here("Spatial", "data", "dissolve.shp"))
 # setwd("/home/cloud")
 # load("inla.RData")
###### spatial field ###########################################################
A1.grid <- inla.mesh.projector(mesh, dims = c(100, 1000))
eta.spde <- inla.mesh.project(A1.grid, res[[1]]$summary.random$s) %>%
  as.matrix() %>%
  as.data.frame() %>%
  bind_cols(
    expand.grid(x = A1.grid$x, y = A1.grid$y)
  ) %>%
  filter(!is.na(ID))
shape <- st_read(here("spatial", "data", "kap2015idm.corrected.shp"))
ggplot() +
  geom_tile(data = eta.spde, aes(x = x, y = y, fill = mean)) +
  geom_sf(data = shape, fill = NA) +
  scale_fill_gradient2()

###### model selection #########################################################
result <- data.frame(result) %>%
  mutate_at(vars(2), as.numeric) %>%
  setNames(c("Model", "CPO", "WAIC"))
komolgorov <- map_dbl(model[3:4], ~ ks.test(
  .$cpo$pit, "punif", 0,
  1
)$p.value)
kurtosis <- map_dbl(model[3:4], ~ kurtosis(.$cpo$pit))
selection <- data.frame(
  result[3:4, 1:3], komolgorov,
  kurtosis
) %>%
  setNames(c("Model", "CPO", "WAIC", "Komolgorov-Smirnov", "Kurtosis")) %>%
  bind_rows(result[c(1:2, 5:16), ]) %>%
  mutate_at(vars(2:4), as.numeric) %>%
  mutate(
    Model = str_replace(Model, c("0 \\+ b0 \\+ f\\(s, model = spde\\) \\+ "), ""),
    Model = str_replace_all(Model, c(
      "log\\(travel\\)" = "ACCESS",
      "log\\(traffic\\)" = "TRAFFIC",
      "log\\(java_dist\\)" = "DIST.JAVA",
      "log\\(population\\)" = "POP"
    ))
  ) %>%
  head(15)

print(xtable(selection, type = "latex", digits = 3),
  file = here("Spatial", "tables", "selection_inla.tex")
)
###### qq-plot #################################################################
ggplot(data.frame(
  x = qunif(ppoints(length(mod.mode$cpo$pit))),
  y = sort(mod.mode$cpo$pit)
)) +
  geom_point(aes(x, y)) +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles")+theme_bw()+theme(axis.text = element_text(size = 14),
                                            axis.title = element_text(size = 14)) 
ggsave(here("Spatial", "plots", "coromap_qqplot.pdf"),
  width = 1920 / 72 / 3, height = 1080 / 72 / 3,
  dpi = 600, limitsize = F
)
##### diagnostics ##############################################################
pdf(
  file = here("Spatial", "plots", "coromap_diagnostics_inla.pdf"),
  width = 12,
  height = 7
)
autoplot(res[[1]])
dev.off()

pdf(
  file = here("Spatial", "plots", "coromap_fixed_sensitivity.pdf"),
  width = 12,
  height = 7
)
plot_fixed_marginals(res[c(1, 4:7)], sigma_ratio = sigma_ratio, combo = T)
dev.off()
pdf(
  file = here("Spatial", "plots", "coromap_hyper_marginals.pdf"),
  width = 12,
  height = 7
)
plot_hyper_marginals(res[c(1, 4:7)], sigma_ratio = sigma_ratio, combo = T)
dev.off()
pdf(
  file = here("Spatial", "plots", "coromap_random_sensitivity.pdf"),
  width = 12,
  height = 7
)
plot_random_effects(res[c(1, 4:7)], sigma_ratio = sigma_ratio, combo = T)
dev.off()

index_new <- list()
for (i in 1:3) {
  index_new[[i]] <- index[[i]]
}
for (i in 4:7) {
  index_new[[i]] <- index[[1]]
}
index <- index_new

##### metrics ##################################################################
median <- (res$summary.fitted.values[index, "0.5quant"])
quant0.025 <- (res$summary.fitted.values[index, "0.025quant"])
quant.975 <- (res$summary.fitted.values[index, "0.975quant"])
mean <- (res$summary.fitted.values[index, "mean"])
sd <- (res$summary.fitted.values[index, "sd"])
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

model0.res <- map2(res[c(1, 4:7)], spde, ~ inla.spde2.result(.x, "s", .y))
model1.res<- map(res[c(1:3)],~ inla.spde2.result(.x, "s", spde[[1]]))
clean_summary <- function(model0.res, res) {
  kappa <- data.frame(inla.zmarginal(model0.res$marginals.kappa[[1]],
    silent = T
  )[-c(4, 6)],
  mode =
    inla.mmarginal(model0.res$marginals.kappa[[1]])
  )
  names(kappa) <- names(res$summary.fixed)[-c(7)]
  tau <- data.frame(inla.zmarginal(model0.res$marginals.tau[[1]],
    silent = T
  )[-c(4, 6)],
  mode =
    inla.mmarginal(model0.res$marginals.tau[[1]])
  )
  names(tau) <- names(res$summary.fixed)[-c(7)]
  list_of_dataframes <- list(
    res$summary.fixed[, 1:6], res$summary.hyperpar,
    kappa = kappa, tau = tau
  ) %>%
    map(~ rownames_to_column(.x))
  output <- data.table::rbindlist(list_of_dataframes, fill = T)
  return(output)
}

output <- map2_dfr(model0.res, res[c(1, 4:7)], ~ clean_summary(.x, .y))
output <- data.frame(output, sigma_ratio %>% slice(rep(1:n(), each = 9)))
output <- output %>%
  filter(!rowname %in% c("Range for s", "Stdev for s")) %>%
  dplyr::select(-mode)
output$rowname[seq(6, nrow(output), 7)] <- "range"
output$rowname[seq(7, nrow(output), 7)] <- "variance"
output <- output %>%
  arrange(rowname, sigma, ratio) %>%
  dplyr::select(
    rowname, sigma, ratio, mean, X0.5quant, X0.025quant,
    X0.975quant, sd
  ) %>%
  arrange(factor(rowname, levels = c(
    "log(travel)", "log(traffic)", "log(population)",
    "b0",  "range", "variance",
  "Precision for the Gaussian observations")))

output1 <- map2_dfr(model1.res, res[c(1:3)], ~ clean_summary(.x, .y))
output1<- data.frame(output1, id= rep(c("baseline", "lower", "upper"), each=9))
output1 <- output1 %>%
  filter(!rowname %in% c("Range for s", "Stdev for s")) %>%
  dplyr::select(-mode)
output1$rowname[seq(6, nrow(output1), 7)] <- "range"
output1$rowname[seq(7, nrow(output1), 7)] <- "variance"
output1 <- output1 %>%
  dplyr::select(
    rowname,  mean, X0.5quant, X0.025quant,
    X0.975quant, sd, id
  ) %>%
  arrange(id,factor(rowname, levels = c(
    "log(travel)", "log(traffic)", "log(population)",
    "b0", "Precision for the Gaussian observations", "range", "variance"
  )))%>%select(-rowname, - id)

print(xtable(output, type = "latex", digits = 2),
  file = here("Spatial", "tables", "output_inla.tex")
)

##### rasterize ################################################################
identifier <- c("risk", "risk_lower", "risk_upper")
for (i in 1:3) {
  data <- data.frame(
    coop, median[[i]], mean[[i]], quant.975[[i]],
    quant0.025[[i]], sd[[i]]
  )
  names(data)[1:2] <- c("longitude", "latitude")
  names(data)[3:7] <- c("median", "mean", "q_0975", "q0_025", "sd")
  coordinates(data) <- ~ longitude + latitude
  crs(data) <- crs(travel_time)
  writeOGR(
    obj = data, dsn = paste(here("Spatial", "data", "shape")),
    layer = paste("result", identifier[i], sep = ""),
    driver = "ESRI Shapefile",
    overwrite_layer = T
  )
}


result_shape <- lapply(identifier, function(x) {
  readOGR(paste(here("Spatial", "data", "shape"), "/result", x, ".shp", sep = ""))
})


quantile <- c("median", "mean", "q_0975", "q0_025", "sd")

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
        paste(c("-a_nodata -999999")),
        paste(shape),
        paste(destination, input_metric, ".tif", sep = "")
      ),
      collapse = " "
    )
  system(cmd_raster)
}
cmd_raster <- map(
  c(
    "resultrisk", "resultrisk_lower",
    "resultrisk_upper"
  ),
  ~ sapply(quantile, function(x) {
    rasterize(x,
      shape = paste(here("Spatial", "data", "shape"), "/", .x, ".shp", sep = ""),
      destination = paste(here("Spatial", "data"),
        "/", .x, "_",
        sep = ""
      )
    )
  })
)
sapply(unlist(cmd_raster), system)

result <-
  paste(here("Spatial", "data"), "/", list.files(
    path = here("Spatial", "data"),
    pattern = "resultrisk.*\\.tif"
  ), sep = "") %>%
  map(~ raster(.))


##### data.frame for plotting ##################################################


map.p <- lapply(result, rasterToPoints)
df <- data.frame(list.cbind(map.p)) %>%
  dplyr::select(-starts_with(c("x.", "y.")))


analysis.shp <- st_as_sf(analysis.shp)
# plotting: plot the quantiles
# @name of the metric
# plotting <- function(name) {
#   name_input <- ensym(name)
#   ggplot()+
#     geom_sf(data=analysis.shp, fill="grey")+
#     geom_raster(data=df, aes(fill= !!name_input,x=x, y=y))+
#     scale_fill_viridis_c(option = "inferno") +
#     theme(
#       panel.background = element_rect(
#         fill = "lightblue",
#         colour = "lightblue",
#         size = 2, linetype = "solid"
#       )
#     ) +
#     xlab("") +
#     ylab("") +
#     labs(fill = "log(risk)")
#     ggsave(paste(
#       here("Spatial", "plots"),"/",
#       str_replace(name, "result", ""), ".png", sep=""
#     ))
# }
# name_input <- names(df)[3:length(names(df))]
# plot_list <- lapply(name_input, plotting)
#


input_shape <- st_read(here("Spatial", "data", "dissolve.shp"))
input_shp <- as(input_shape, "Spatial")

plot_raster <- function(name, index, letter, title, input.shp = input.shp) {
  pdf(
    file = here("Spatial", "plots", paste("coromap_", name, ".pdf", sep = "")),
    width = 16,
    height = 8
  )
  x.scale <- list(cex = 3)
  y.scale <- list(cex = 3)

  print(levelplot(result[[index]],
    xlab = list(label = ""), ylab = list(label = ""),
    scales = list(x = x.scale, y = y.scale),
    main = list(
      paste("(", letter, ")", " ", title, sep = ""),
      cex = 2.5
    ), margin = F, colorkey = list(space = "bottom", labels=list(cex=3.2)),
    par.strip.text = list(cex = 0.9, lines = 2, fontface = "bold"),
    layout = c(1, 1)
  ) +
    latticeExtra::layer(sp.polygons(input_shp,
      fill = "gray", alpha = 0.2
    )))


  dev.off()
}

names <- sapply(result, function(x) names(x))
names <- names[c(2:5, 7:10, 12:15)]
index_raster <- c(2:5, 7:10, 12:15)

letter <- rep(letters[c(1, 4, 3, 2)], times = 3)
title <- rep(c("Median", "97.5 % quantile", "2.5 % quantile", "Standard deviation"), times = 3)
pmap(list(names, index_raster, letter, title), ~ plot_raster(..1, ..2, ..3, ..4))


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
exc <- list()
for (i in 1:3) {
  exc[[i]] <- future_map_dfc(seq(-1.0, -2.2, by = -0.2), ~ exceedance(
    .x, res[[i]],
    index[[i]]
  ),
  .progress = T
  )
}
# subceeding probability
# opposite_exc <- 1 - exc
ext <- extent(travel_time)
exceedance_f <- function(coop = coop, exc, name, ext = ext) {
  data_exc <- data.frame(
    coop, exc # , opposite_exc)
  )
  names(data_exc)[1:2] <- c("longitude", "latitude")
  names(data_exc)[3:9] <- paste("X", as.character(seq(-1.0, -2.2,
    by = -0.2
  ) * -1), sep = "_")
  # names(data_exc)[10:16] <- paste("X_opp", as.character(seq(-2.2, -3.4,
  #   by = -0.2
  # ) * -1), sep = "_")

  coordinates(data_exc) <- ~ longitude + latitude
  crs(data_exc) <- crs(travel_time)
  writeOGR(
    obj = data_exc, dsn = "shape", layer = name, driver = "ESRI Shapefile",
    overwrite_layer = T
  )
  result_exc <- readOGR(paste("shape/", name, ".shp", sep = ""))
  exceedance <- names(result_exc@data)

  cmd_raster <- sapply(exceedance, function(x) {
    rasterize(x,
      shape = paste("shape/", name, ".shp", sep = ""), destination = name
    )
  })
  sapply(cmd_raster, system)
}

map2(
  exc, c("exc", "exc_lower", "exc_upper"),
  ~ exceedance_f(coop = coop, ext = ext, exc = .x, name = .y)
)

exc_input <-
  paste("Spatial/data/", list.files(
    path = "Spatial/data",
    pattern = "excX_\\d.*\\.tif"
  ), sep = "") %>%
  purrr::map(~ raster(.))


##### zoom plot ################################################################
raster_plot <- function(raster, letter, input.shp = input.shp) {
  raster_input <- raster(paste(here("Spatial", "data"), "/",
                               raster, ".tif",
    sep = ""
  ))
  
  input <- raster::values(raster_input)
  raster::values(raster_input) <- cut(raster::values(raster_input), 
                                             seq(0,1, 0.25))
  myPal <- map_palette("bruiser", n=4)
  myTheme <- rasterTheme(region = myPal)
  x.scale <- list(cex = 2.5)
  y.scale <- list(cex = 2.5)

  p <-   levelplot(raster_input, 
                   panel=function(...) {
                     sp.polygons(input_shp, fill = "gray79", col = NA)
                     panel.levelplot(...)
                   },
    scales = list(x = x.scale, y = y.scale),
    xlab = list(label = ""), ylab = list(label = ""),
    par.settings = myTheme,
    main = list(
      paste("(", letter, ")", paste = ""),
      cex = 2.5
    ), margin = F, colorkey = list(space="bottom"),
    par.strip.text = list(cex = 0.9, lines = 2, fontface = "bold"),
    layout = c(1, 1)
  )
  
  pdf(
    file = paste(here("Spatial", "plots"), "/", raster, ".pdf", sep = ""),
    width = 12,
    height = 7
  )
  print(p)
  dev.off()
}
exceedance <- c(
   "exc_lowerX_2_2","excX_2_2",  "exc_upperX_2_2","exc_lowerX_1", 
 "excX_1", "exc_upperX_1"
)

raster_input<- exceedance[1]
raster_input <- raster(paste(here("Spatial", "data"), "/",
                             raster, ".tif",
                             sep = ""
))
input <- raster::values(raster_input)
raster::values(raster_input) <- cut(raster::values(raster_input), 
                                    seq(0,1, 0.1))
myPal <- map_palette("bruiser", n=10)
myTheme <- rasterTheme(region = myPal)

p <-   levelplot(raster_input, 
                 panel=function(...) {
                   sp.polygons(input_shp, fill = "gray79", col = NA)
                   panel.levelplot(...)
                 },
                 scales = list(x =NULL, y = NULL,draw=F),
                 xlab = list(label = ""), ylab = list(label = ""),
                 par.settings = myTheme,
                 margin = F, colorkey = list(space="bottom"),
                 layout = c(1, 1),
                 main= ("Exceedance Probability, threshold: -2.2 (log scale)")
)

pdf(
  file = paste(here("twitter"), "/", raster, "twitter.pdf", sep = ""),
  width = 12,
  height = 7
)
print(p)
dev.off()

for (i in 1:6){
raster_plot(exceedance[i], letters[i])
}
##### validation ###############################################################

##### external validation ODP/PDP ##############################################
validation <- fread(here("Extvalidation", "data", "coromap.data.covid-wisnu.csv"))
validation <- validation %>%
  filter(!is.na(id_m))
##### population ###############################################################
# pop_2011 <- read_stata("external/podes2014_population.dta")
# pop_2014 <- readRDS("external/podes2014layer.Rds")
# pop <- merge(pop_2014, pop_2011,
#   by.x = "id.desa.podes.2014",
#   by.y = "iddesapodes2014", all.x = T
# )
# 
# pop_input <- pop@data %>%
#   group_by(id_m) %>%
#   summarise(population = sum(population_2014, na.rm = T))
# validation <- val %>%
#   left_join(pop_input, by = "id_m")
# validation <- validation %>%
#   filter(!is.na(population))

analysis.shp <- st_read(here("Spatial", "data", "kap2015idm.corrected.shp"))
sf_use_s2(F)  
dis_idm <-analysis.shp%>% group_by(id_m)%>%mutate(geometry=st_union(geometry))%>%
  select(geometry, id_m)
st_write(dis_idm, here("extvalidation", "data", "dis_idm.shp"))

##### run on server ############################################################
#poly <- readOGR("/home/cloud/dis_idm.shp")
# 
# # #create a raster stack
# s <- stack("/home/cloud/resultrisk_mean.tif")
# #
# # #extract raster cell count (sum) within each polygon area (poly)
# 
# ex <- raster::extract(s, poly, fun=mean, na.rm=TRUE, df=TRUE)
# 
# df <- data.frame(ex)
# df$ID <- as.numeric(poly$id_m)
# write.csv(df, file = "/home/cloud/validation_cases.csv")
##### compare actual cases with results ########################################
cases <- read.csv(here("Extvalidation", "data", "validation_cases.csv"))
names(cases) <- c("index", "id_m", "mean_risk")
validation <- validation %>%
  left_join(cases, by = "id_m")%>%filter(month<6)

mean<-cases%>%left_join(dis_idm)%>%st_as_sf()
# dissolved the village shapefile to provinces in ArcMap
shape <- st_read(here("Extvalidation", "data","dis_idm.shp"))

validation <- validation %>%
  left_join(shape, by = "id_m")
validation <- st_as_sf(validation)
validation <- validation %>%
  mutate(log_conf = log(confirmed_case))%>%
  select(mean_risk,log_conf, month)

##### plot metrics #############################################################
# ggplot(mean) +
#   geom_sf(aes(fill = mean_risk), color = NA) +
#   scale_fill_viridis_c(option = "inferno") +
#   labs(fill = "Mean of \n log risk") +
#   ggsave(here("Extvalidation","plots","coromap_aggrisk.pdf"),
#     width = 1920 / 72 / 3, height = 1080 / 72 / 3,
#     dpi = 72, limitsize = F
#   )
st_write(mean, here("Extvalidation", "data", "coromap_aggrisk.shp"))

system(rasterize("mean_risk", here("Extvalidation", "data", "coromap_aggrisk.shp"), 
                 here("Extvalidation", "data", "coromap_aggrisk")))

agg_risk <- raster( here("Extvalidation", "data","coromap_aggriskmean_risk.tif"))
x.scale <- list(cex = 2)
y.scale <- list(cex = 2)

pdf(
  file = here("Extvalidation", "plots","coromap_aggrisk.pdf"),
  width = 16,
  height = 8
)
levelplot(agg_risk,
  xlab = list(label = ""), ylab = list(label = ""),
  scales = list(x = x.scale, y = y.scale),
  main = list(
    "",
    cex = 1
  ), margin = F, colorkey = list(space = "bottom", labels = list(cex = 2)),
  par.strip.text = list(cex = 0.9, lines = 2, fontface = "bold"),
  layout = c(1, 1)
)
dev.off()


##### correlation ##############################################################
input <- validation %>%
  st_drop_geometry() %>%
  dplyr::select(month, mean_risk, log_conf)
wilcox <- input %>%
  group_by(month)%>%
  group_map(~ wilcox.test(.x$log_conf, .x$mean_risk,
    paired = TRUE,
    alternative = "two.sided"
  ))

correlation <- input %>%
  group_by(month)%>%
  group_map(~ cor(.x$log_conf, .x$mean_risk,
    method = "spearman",
    use = "complete.obs"
  ))

wil <- map_dbl(wilcox, ~ .$p.value)
correlation <- unlist(correlation)
val_concept <- data.frame(wilcox = wil, corr = correlation, month = c("March",
                                                                      "April",
                                                                      "May"))
names(val_concept)[1:2] <- c(
  "Wilcoxon test p-value",
  "Spearman rank correlation"
)
print(xtable(val_concept, type = "latex", digits = 3),
  file = here("Extvalidation", "tables", "validation.tex")
)

# ##### eps for publication ######################################################
# convert.g(
#   path = "tmp", fileroot = "*", from = "png",
#   to = "eps", create.path = TRUE, options = NULL
# )
# convert.g(
#   path = "tmp/result", fileroot = "*", from = "png",
#   to = "eps", create.path = TRUE, options = NULL
# )
