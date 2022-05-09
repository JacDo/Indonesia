################################################################################
####### predict flight risk ####################################################
################################################################################

###### set-up ##################################################################
library(xtable)
library(MASS)
library(caret)
library(furrr)
library(tidyverse)
library(scales)
library(igraph)
library(circlize)
library(MASS)
library(mgcv)
library(texreg)
library(mgcViz)
library(gridExtra)
library(data.table)
library(here)

source(here("code", "loocv_function.R"))
###### data ####################################################################
data <-
  paste(here("GAM", "data"), "/",
    list.files(
      pattern = "coromap.flights.*\\.csv",
      path = here("GAM", "data")
    ),
    sep = ""
  ) %>%
  map_df(~ read_csv(., col_types = cols(.default = "c")))

airports <- read.csv(here("GAM", "data", "airport_list.csv")) # geolocation of airports

##### cleaning #################################################################

# for every departure airport in Indonesia, count the number of connections
# between the airports

indonesia <- data %>%
  filter(departure.iata %in% airports$iata_code) %>%
  group_by(arrival.iata, departure.iata) %>%
  dplyr::summarise(n_flight = n())

# merge location information about lon/lat

airport_input <- indonesia %>%
  ungroup() %>%
  dplyr::select(arrival.iata, departure.iata)

airport <- data.frame(iata = unlist(airport_input), use.names = FALSE) %>%
  distinct(iata) %>%
  left_join(airports, by = c("iata" = "iata_code"))

write.csv(airport, here("GAM", "data", "domestic_air.csv"))

airport_subset <- airport %>%
  dplyr::select(iata, longitude, latitude)

# merge geolocation to arrival and departure airports

indonesia <- indonesia %>%
  left_join(airport_subset, by = c("departure.iata" = "iata"))

indonesia <- indonesia %>%
  left_join(airport_subset, by = c("arrival.iata" = "iata"))

names(indonesia)[4:7] <- c(
  "longitude_dep", "latitude_dep",
  "longitude_arr", "latitude_arr"
)
##### calculate risk ###########################################################
# assign importation risk to departure airports
# see http://rocs.hu-berlin.de/corona/docs/model/importrisk/
# for more information

# idea: take the importation risk at departure airports,
# spread risk through the network

indonesia <- indonesia %>%
  mutate(
    weight_dep = case_when(
      departure.iata == "DPS" ~ 0.00053,
      departure.iata == "CGK" ~ 0.00038,
      departure.iata == "SUB" ~ 0.00016,
      departure.iata == "MDC" ~ 0.00006,
      departure.iata == "UPG" ~ 0.00004,
      departure.iata == "KNO" ~ 0.00004,
      departure.iata == "JOG" ~ 0.00004
    ),
    weight_arr = case_when(
      arrival.iata == "DPS" ~ 0.00053,
      arrival.iata == "CGK" ~ 0.00038,
      arrival.iata == "SUB" ~ 0.00016,
      arrival.iata == "MDC" ~ 0.00006,
      arrival.iata == "UPG" ~ 0.00004,
      arrival.iata == "KNO" ~ 0.00004,
      arrival.iata == "JOG" ~ 0.00004
    )
  )

# calculate for each departure airport:

indonesia <- indonesia %>%
  dplyr::group_by(departure.iata) %>%
  dplyr::mutate(
    sum_departure = sum(n_flight), # number of total flight connections
    proportion_departure = n_flight / sum_departure, # proportion of flight going
    # to each airport
    risk = proportion_departure * weight_dep + ifelse(is.na(weight_arr),
      0, weight_arr
    ), # spread risk proportionately
    distance_geo = spatialrisk::haversine(
      latitude_dep, longitude_dep,
      latitude_arr, longitude_arr
    )
  ) # geographic distance between airports

##### visualize flight connection ##############################################
indonesia_ig <- indonesia %>%
  dplyr::select(departure.iata, arrival.iata, n_flight)
names(indonesia_ig) <- c("from", "to", "weight")

indonesia_dir <- indonesia_ig %>%
  group_by(ID1 = pmin(from, to), ID2 = pmax(from, to)) %>%
  summarise(Count = sum(weight))
names(indonesia_dir) <- c("from", "to", "weight")


airport_list <- indonesia %>%
  dplyr::select(arrival.iata, departure.iata) %>%
  pivot_longer(c("arrival.iata", "departure.iata"),
    names_to = "key", values_to = "value"
  ) %>%
  dplyr::select(value) %>%
  distinct(value) %>%
  left_join(., airports %>%
    dplyr::select(airport_name, iata_code),
  by = c("value" = "iata_code")
  )

arr_v <- indonesia %>%
  group_by(arrival.iata) %>%
  summarise(arr = sum(n_flight))

dep_v <- indonesia %>%
  group_by(departure.iata) %>%
  summarise(dep = sum(n_flight))

ind_table <- full_join(dep_v, arr_v, by = c("departure.iata" = "arrival.iata")) %>%
  left_join(airport_list, by = c("departure.iata" = "value")) %>%
  rowwise() %>%
  mutate(volume = sum(c_across(arr:dep), na.rm = T)) %>%
  dplyr::select(-dep, -arr) %>%
  relocate(airport_name, .after = departure.iata) %>%
  ungroup() %>%
  mutate(rel_vol = volume / sum(volume) * 100)

# print(xtable(ind_table[1:39, ], type = "latex", digits = 2), file = "airport1.tex")
# print(xtable(ind_table[40:78, ], type = "latex", digits = 2), file = "airport2.tex")


pdf(
  file = here("GAM", "plots", "coromap_network.pdf"),
  width = 4,
  height = 4
)
circos.clear()
freqpairs <- indonesia_dir

set.seed(7854)

col <- colorRamp2(
  seq(min(indonesia_dir$weight),
    max(indonesia_dir$weight),
    length = nrow(indonesia_dir)
  ),
  rainbow(241)
)

circos.par(
  cell.padding = c(0, 0, 0, 0),
  gap.degree = 2.6
)

chordDiagram(freqpairs,
  annotationTrack = "grid",
  preAllocateTracks = list(
    track.height = 0.2, col = col,
    link.lwd = 1, link.lty = 1, transparency = 0
  )
)

circos.trackPlotRegion(
  track.index = 1, panel.fun = function(x, y) {
    xlim <- get.cell.meta.data("xlim")
    xplot <- get.cell.meta.data("xplot")
    ylim <- get.cell.meta.data("ylim")
    sector.name <- get.cell.meta.data("sector.index")
    circos.text(mean(xlim), ylim[1], sector.name,
      facing = "clockwise",
      niceFacing = TRUE, adj = c(0, 0.1), cex = 0.5
    )
  },
  bg.border = NA
)
dev.off()

##### prediction ###############################################################
##### igraph input #############################################################

gD <- simplify(graph.data.frame(indonesia_ig, directed = T))
layout <- layout_with_fr(gD)
E(gD)$width <- E(gD)$weight / 200
layout <- layout_with_lgl(gD)
plot(gD,
  layout = layout,
  edge.arrow.size = 0.1,
  vertex.label.cex = 0.5,
  vertex.label.font = 2,
  vertex.shape = "circle",
  vertex.size = 1,
  vertex.label.color = "black",
  edge.width = E(gD)$width
)

##### vertex measures ##########################################################
V(gD)$transitivity <- transitivity(gD, type = "local", isolates = "zero")
V(gD)$in_strength <- igraph::strength(gD, mode = "in")
V(gD)$out_strength <- igraph::strength(gD, mode = "out")

##### prediction prep ##########################################################
input <- data.frame(
  bet = betweenness(gD),
  eig = evcent(gD, directed = T)$vector,
  trans = V(gD)$transitivity,
  in_strength = V(gD)$in_strength,
  out_strength = V(gD)$out_strength
)

input$name <- rownames(input)
ind_join <- indonesia %>%
  left_join(input, by = c("departure.iata" = "name"))

##### additional edge-specific measures ########################################
dist <- ind_join %>%
  mutate(dis = map2_dbl(
    arrival.iata, departure.iata,
    ~ as.numeric(distances(gD,
      v = V(gD)[name == .x],
      to = V(gD)[name == .y]
    ))
  )) %>%
  ungroup() %>%
  dplyr::select(dis) %>%
  unlist()

sim <- ind_join %>%
  ungroup() %>%
  mutate(
    arrival.iata = as.character(arrival.iata),
    departure.iata = as.character(departure.iata),
    sim = map2(
      arrival.iata, departure.iata,
      ~ as.numeric(
        similarity.dice(gD, c(.x, .y))[2, 1]
      )
    )
  ) %>%
  ungroup() %>%
  dplyr::select(sim) %>%
  unlist()

connectivity <- ind_join %>%
  ungroup() %>%
  mutate(
    arrival.iata = as.character(arrival.iata),
    departure.iata = as.character(departure.iata),
    con = map2(
      arrival.iata, departure.iata,
      ~ as.numeric(
        edge_connectivity(gD, .x, .y, checks = T)
      )
    )
  ) %>%
  ungroup() %>%
  dplyr::select(con) %>%
  unlist()

##### build model ##############################################################
ind_join <- data.frame(ind_join,
  dis = unlist(dist), sim = unlist(sim),
  con = unlist(connectivity)
)
# scale vertex parameters
ind_join <- ind_join %>%
  mutate_at(vars(c(
    "bet", "eig", "trans", "in_strength",
    "out_strength"
  )), rescale, to = c(0, 1))

train <- ind_join %>% filter(!is.na(weight_dep))
test <- ind_join %>% filter(is.na(weight_dep))

## Best model based on AIC
options(scipen = 3)


#####model selection############################################################
formula_comp <- c(
  "log(risk) ~ bet + trans + in_strength + out_strength+
               s(con)+
              s(proportion_departure)+s(n_flight)",
  "log(risk) ~ bet + trans + in_strength + out_strength+
               con+
              proportion_departure+n_flight",
  "log(risk) ~ bet + trans + in_strength + out_strength+ eig +
              s(con)+s(sim)+ s(dis)+s(distance_geo)+
              s(proportion_departure)+s(n_flight)",
  "log(risk) ~ bet + trans + in_strength + out_strength+
                s(con, bs='cs')+
              s(proportion_departure, bs='cs')+s(n_flight, bs='cs')",
  "log(risk) ~ bet + trans + in_strength + out_strength+
               con+
              proportion_departure+n_flight",
  "log(risk) ~ bet + trans + in_strength + out_strength+ eig +
              s(con, bs='cs')+s(sim,bs='cs')+ s(dis,bs='cs')+s(distance_geo,bs='cs')+
              s(proportion_departure, bs='cs')+s(n_flight, bs='cs')",
  "log(risk) ~ bet + trans + in_strength + out_strength+
                s(con, bs='cs')+ in_strength*out_strength+
              s(proportion_departure, bs='cs')+s(n_flight, bs='cs')",
  "log(risk) ~ bet + trans + in_strength + out_strength+
               con+ in_strength*out_strength+
              proportion_departure+n_flight",
  "log(risk) ~ bet + trans + in_strength + out_strength+ eig +
               s(con)+ in_strength*out_strength+s(sim,bs='cs')+ s(dis,bs='cs')+
               s(distance_geo,bs='cs')+
              s(proportion_departure,bs='cs')+s(n_flight, bs='cs')"
)

train_input <- train %>% dplyr::select(
  risk, bet, trans, in_strength, out_strength, eig,
  con, sim, dis, distance_geo,
  proportion_departure, n_flight
)
models_list <- list()
models_list[4:9] <- lapply(formula_comp[4:9], function(x) {
  gam(as.formula(x), data = train_input, method = "REML", select = T)
})
models_list[1:3] <- lapply(formula_comp[1:3], function(x) {
  gam(as.formula(x), data = train_input)
})

lapply(models_list, function(x) summary(x))


plan(multisession)
list_cv <- list()
list_cv[1:3] <- future_map(
  1:3,
  ~ loogam(models_list[[.x]], train_input, n = 132, LOO = F, select = F, method = "GCV.Cp")
)
list_cv[4:9] <- future_map(
  4:9,
  ~ loogam(models_list[[.x]], train_input,
    n = 132, LOO = F, select = T, method = "REML"
  )
)

summary_model <- data.frame(
  model = formula_comp,
  rmse = sapply(list_cv, function(x) round(x$RMSE, 6)),
  mae = sapply(list_cv, function(x) round(x$MAE, 6)),
  aic = map_dbl(models_list, ~ AIC(.x)),
  r2 = map_dbl(models_list, ~ summary(.x)$r.sq)
)

summary_model <- summary_model[c(2, 3, 1, 5, 6, 4, 8, 9, 7), ]
prep_models <- function(data) {
  regressors <- paste("s(", names(data)[c(3, 11, 13, 19, 20, 21)], ", bs='cs')",
                      sep = "")
  regressors <- c(regressors, paste(names(data)[c(14:18)], sep = ""))
  regressors <- c(regressors, paste(names(data)[c(3, 11, 13, 19, 20, 21)], sep = ""))
  regMat <- expand.grid(replicate(
    n = length(regressors),
    c(T, F), simplify = FALSE
  ))
  regMat <- regMat[-nrow(regMat), ]
  allModelsList <- apply(regMat, 1, function(x) {
    as.formula(
      paste("log(risk) ~ ", paste(regressors[x],
        collapse = " + "
      ), sep = "")
    )
  })
  return(allModelsList)
}
allModelsList <- prep_models(train)
combinations <- c(
  "in_strength.*eig|eig.*in_strength",
  "(s\\()?\\bdis\\b(\\))?.*in_strength|in_strength.*(s\\()?\\bdis\\b(\\))?",
  "out_strength.*eig|eig.*out_strength",
  "out_strength.*(s\\()?\\bdis\\b(\\))?|(s\\()?\\bdis\\b(\\))?.*out_strength",
  "(s\\()?\\bsim\\b(\\))?.*(s\\()?\\bcon\\b(\\))?|(s\\()?\\bcon\\b(\\))?.*(s\\()?\\bsim\\b(\\))?",
  "s\\(sim, bs.*\\).*sim|sim.*s\\(sim, bs.*\\)",
  "s\\(con, bs.*\\).*con|con.*s\\(con, bs.*\\)",
  "s\\(distance_geo, bs.*\\).*distance_geo|distance_geo.*s\\(distance_geo, bs.*\\)",
  "s\\(dis, bs.*\\).*dis|dis.*s\\(dis, bs.*\\)",
  "s\\(proportion_departure, bs.*\\).*proportion_departure|proportion_departure.*s\\(proportion_departure, bs.*\\)",
  "s\\(n_flight, bs.*\\).*n_flight|n_flight.*s\\(n_flight, bs.*\\)"
)
comb_excluded<-c("s\\(sim, bs.*\\).*sim|sim.*s\\(sim, bs.*\\)",
"s\\(con, bs.*\\).*con|con.*s\\(con, bs.*\\)",
"s\\(distance_geo, bs.*\\).*distance_geo|distance_geo.*s\\(distance_geo, bs.*\\)",
"s\\(dis, bs.*\\).*dis|dis.*s\\(dis, bs.*\\)",
"s\\(proportion_departure, bs.*\\).*proportion_departure|proportion_departure.*s\\(proportion_departure, bs.*\\)",
"s\\(n_flight, bs.*\\).*n_flight|n_flight.*s\\(n_flight, bs.*\\)")

index <- map_lgl(allModelsList, ~
any(str_detect(as.character(.x[3]), combinations)))
models <- allModelsList[index == F]


index_exc <- map_lgl(allModelsList, ~
                   any(str_detect(as.character(.x[3]), comb_excluded)))
models_exc <- allModelsList[index_exc == F]
models_selected <- data.frame(
  model = as.character(models),
  aic = map_dbl(models, ~ AIC(gam(.x, data = train)))
)


models_selected <- models_selected %>%
  arrange(aic) %>%
  head(20)

formula <- models_selected %>%
  pull(model)

#####################cross-validation###########################################
cross_validation <- function(train, formula, family = "gaussian") {
  ## cross_validation: cross-validation of GAM
  # @train: training data
  # @gam_diag: formula
  # @family: family to be specified
  family <- ifelse(family == "gaussian", "gaussian", "Gamma(link='log')")
  set.seed(4019)
  training.samples <- train$risk %>%
    createDataPartition(p = 0.7, list = FALSE)
  train.data <- train[training.samples, ]
  test.data <- train[-training.samples, ]
  # Build the model
  model_cv <- gam(as.formula(formula), data = train.data, family = family)
  predictions <- predict(model_cv, test.data)
  cv <- data.frame(
    RMSE = RMSE(predictions, log(test.data$risk)),
    MAE = MAE(predictions, log(test.data$risk))
  )

  return(cv)
}
result_cv <- data.frame(
  formula,
  map_dfr(formula, ~ cross_validation(train, .x)$cv)
)
result_cv <- result_cv %>% arrange(RMSE, MAE)
result_cv$formula <- result_cv$formula %>%
  str_replace_all(c(
    "trans" = "TRA", "bet" = "BET",
    "n\\_flight" = "NB.FLY",
    "con" = "CONNECT", "sim" = "SIM",
    "proportion\\_departure" = "DEP.FLY",
    "in\\_strength" = "IN.STR", "out\\_strength" = "OUT.STR",
    "log\\(risk\\)" = "", "~" = "", "s\\(" = "f(", " " = "",
    "distance.geo" = "E.DIST.AIRPORT"
  ))
sorted_list <- sapply(
  result_cv$formula,
  function(x) str_split(x, pattern = "\\+")
)

sorted_list <- sapply(sorted_list, function(x) sort(x))
result_cv$formula <- sapply(sorted_list, function(x) paste(x, collapse = " + "))

print(xtable(result_cv, type = "latex", digits = 2), file = here("GAM", "tables", "T3.tex"))



formula_new <- c("log(risk) ~ bet + trans + in_strength + out_strength + s(con,
                                  bs = 'cs') + s(proportion_departure, bs = 'cs') +
                               s(n_flight, bs = 'cs')",
                               "risk ~ bet + trans + in_strength + out_strength + s(con,
                                  bs = 'cs') + s(proportion_departure, bs = 'cs') +
                               s(n_flight, bs = 'cs')"
                 )
                 
family <- c("gaussian", "gamma")
result_cv_new <- data.frame(
  family,
  map2_dfr(formula_new, family, ~ cross_validation(train,
    formula = .x,
    family = .y
  ))
)

model <- models_list[[4]]

model_gamma <- gam(as.formula("risk ~ bet + trans + in_strength + out_strength + s(con,
                                  bs = 'cs') + s(proportion_departure, bs = 'cs') +
                               s(n_flight, bs = 'cs')"),
  data = train, family = "Gamma(link =
                                              'log' )"
)

# fit distributions

lognormal <- fitdistr(train$risk, "lognormal")
gamma <- fitdistr(train$risk, "gamma")

AIC(lognormal)
AIC(gamma)


y_lik <- dlnorm(train$risk,
  mean = predict(model, type = "response", data = train),
  sd = sqrt(model$sig2), log = TRUE
)
AIC_gaussian <- -2 * sum(y_lik) + 2 * 8

aic_comparison <- data.frame(
  distribution = c("log-normal", "gamma"),
  AIC_MLE = c(
    AIC(lognormal),
    AIC(gamma)
  ),
  AIC_models = c(AIC_gaussian, AIC(model_gamma))
)%>%
 bind_cols(result_cv_new[,2:3])

print(xtable(aic_comparison, type = "latex", digits = 2),
  file = here("GAM", "tables", "lognormal_gamma_gam.tex")
)



result_cv$formula <- result_cv$formula %>%
  str_replace_all(c(
    "trans" = "TRA", "bet" = "BET",
    "n\\_flight" = "NB.FLY",
    "con" = "CONNECT", "sim" = "SIM",
    "proportion\\_departure" = "DEP.FLY",
    "in\\_strength" = "IN.STR", "out\\_strength" = "OUT.STR",
    "log\\(risk\\)" = "", "~" = "", "s\\(" = "f(", " " = "",
    "distance.geo" = "E.DIST.AIRPORT"
  ))
sorted_list <- sapply(
  result_cv$formula,
  function(x) str_split(x, pattern = "\\+")
)

sorted_list <- sapply(sorted_list, function(x) sort(x))
result_cv$formula <- sapply(sorted_list, function(x) paste(x, collapse = " + "))
result_cv <- result_cv %>% arrange(RMSE)
print(xtable(result_cv, type = "latex", digits = 2),
  file = here("GAM", "tables", "rmse_mae_gam.tex")
)


gam_diag <- getViz(model)

## gam_diagnostics: function to perform GAM smooths
# @x index
# @gam_diag getViz object for the model

gam_diagnostics <- function(x, name, gam_diag) {
  plot <- plot(sm(gam_diag, x))
  plot <- plot + l_fitLine(colour = "red") + l_rug(
    mapping = aes(x = x, y = y),
    alpha = 0.8
  ) +
    xlab(name) +
    ylab(paste("f(", name, ")", sep = "")) +
    l_ciLine(mul = 5, colour = "blue", linetype = 2) +
    l_points(shape = 19, size = 1, alpha = 0.1)
  return(plot)
}
##### plotting #################################################################
plot_list <- map2(
  c(1:3), c("NB.FLY", "CONNECT", "DEP.FLY"),
  ~ gam_diagnostics(.x, gam_diag, name = .y)
)
tmp <- lapply(plot_list, function(x) x$ggObj)


pdf(
  file = here("GAM", "plots", "coromap_smooths.pdf"),
  width = 6,
  height = 4
)
grid.arrange(grobs = tmp, ncol = 2, nrow = 2)
dev.off()

gam_plotting <- function(gam_diag) {
  obj <- gam_diag

  fv <- if (inherits(obj$family, "extended.family")) {
    predict(obj, type = "response")
  } else {
    fitted(obj)
  }
  resid <- residuals(obj, type = "tnormal")
  resp <- napredict(obj$na.action, obj$y)
  df <- data.frame(
    resid = resid,
    response = resp, fv = fv
  )

  linpred <- if (is.matrix(obj$linear.predictors) && !is.matrix(resid)) {
    napredict(obj$na.action, obj$linear.predictors[, 1])
  } else {
    napredict(obj$na.action, obj$linear.predictors)
  }

  qq_resid <- ggplot(df, aes(sample = resid))
  qq_resid <- qq_resid + stat_qq(size = 0.05) + stat_qq_line(col = "red") +
    xlab("Theoretical Quantiles") +
    ylab("Deviance Residuals") +
    ggtitle("a) QQ-Plot of residuals") + theme_bw()


  hist <- ggplot(data = df, mapping = aes(x = resid)) +
    geom_histogram() +
    xlab("Residuals") +
    ylab("Frequency") +
    ggtitle("b) Histogram of residuals") +
    theme_bw()


  qq_resp <- ggplot(df, aes(sample = response))
  qq_resp <- qq_resp + stat_qq(size = 0.05) + stat_qq_line(col = "red") +
    xlab("Theoretical Quantiles") +
    ylab("Sample Quantiles") +
    ggtitle("c) QQ-Plot of response") + theme_bw()
  return(list(qq_resid, hist, qq_resp))
}

qq_resid <- gam_plotting(gam_diag)[[1]]
hist <- gam_plotting(gam_diag)[[2]]
qq_resp <- gam_plotting(gam_diag)[[3]]
pdf(
  file = here("GAM", "plots", "coromap_gam_diagnostics.pdf"),
  width = 6,
  height = 4
)
grid.arrange(qq_resid, hist, qq_resp, ncol = 2, nrow = 2)
dev.off()

gam_diag_gamma <- getViz(model_gamma)

qq_resid_gamma <- gam_plotting(gam_diag_gamma)[[1]]
pdf(
  file = here("GAM", "plots", "coromap_gam_diagnostics_gamma.pdf"),
  width = 6,
  height = 4
)
qq_resid_gamma
dev.off()




##### prediction ###############################################################

test_data <- test %>%
  ungroup() %>%
  dplyr::select(
    bet, eig, trans, in_strength, out_strength, n_flight, dis, sim, con,
    proportion_departure, distance_geo, weight_arr
  )
prediction <- cbind(test %>% dplyr::select(-risk),
  risk = pred$fit,
  risk_lower = pred$fit - pred$se.fit,
  risk_upper = pred$fit + pred$se.fit
)


train <- train %>% mutate(
  risk_lower = log(risk),
  risk_upper = log(risk),
  risk = log(risk)
)
result <- rbind(train, prediction)

result <- result %>% mutate(
  risk = risk -
    ifelse(is.na(weight_arr), 0, log(weight_arr)),
  risk_lower = risk_lower -
    ifelse(is.na(weight_arr), 0, log(weight_arr)),
  risk_upper = risk_upper -
    ifelse(is.na(weight_arr), 0, log(weight_arr))
)
result <- result %>%
  mutate(
    min = min(risk_lower), max = max(risk_upper),
    risk_score_lower = rescale(risk_lower,
      from = c(
        unique(min),
        unique(max)
      ),
      to = c(0.01, 0.99)
    ),
    risk_score_upper = rescale(risk_upper,
      from = c(
        unique(min),
        unique(max)
      ),
      to = c(0.01, 0.99)
    ),
    risk_score = rescale(risk,
      from = c(
        unique(min),
        unique(max)
      ),
      to = c(0.01, 0.99)
    )
  )

##### predict risk #############################################################
options(scipen = 500)
risk_input <- result %>%
  dplyr::select(departure.iata, arrival.iata, risk_score)
names(risk_input) <- c("from", "to", "weight")
risk_input_lower <- result %>%
  dplyr::select(departure.iata, arrival.iata, risk_score_lower)
names(risk_input_lower) <- c("from", "to", "weight")
risk_input_upper <- result %>%
  dplyr::select(departure.iata, arrival.iata, risk_score_upper)
names(risk_input_upper) <- c("from", "to", "weight")

risk_graph <- simplify(graph.data.frame(risk_input, directed = T))
risk_graph_lower <- simplify(graph.data.frame(risk_input_lower, directed = T))
risk_graph_upper <- simplify(graph.data.frame(risk_input_upper, directed = T))

E(risk_graph)$width <- E(risk_graph)$weight
E(risk_graph_lower)$width <- E(risk_graph_lower)$weight
E(risk_graph_upper)$width <- E(risk_graph_upper)$weight
##### final dataset ############################################################
final <- data.frame(
  risk = strength(risk_graph),
  risk_lower = strength(risk_graph_lower),
  risk_upper = strength(risk_graph_upper),
  iata = names(strength(risk_graph))
)

final <- final %>%
  mutate(
    min = min(risk_lower), max = max(risk_upper),
    risk_score_lower = rescale(risk_lower,
      from = c(
        unique(min),
        unique(max)
      ),
      to = c(0.01, 0.99)
    ),
    risk_score_upper = rescale(risk_upper,
      from = c(
        unique(min),
        unique(max)
      ),
      to = c(0.01, 0.99)
    ),
    risk_score = rescale(risk,
      from = c(
        unique(min),
        unique(max)
      ),
      to = c(0.01, 0.99)
    )
  )

write.csv(final, here("GAM", "data", "flight_risk.csv"))

