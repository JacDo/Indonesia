################################################################################
######### INLA #################################################################
################################################################################

##### setup ####################################################################
library(sp)
library(rgdal)
library(raster)
library(tidyverse)
library(INLA)
library(ggthemes)
library(furrr)
library(scales)
library(spdep)
library(moments)
setwd("/home/cloud")
load("input.Rds")

##### estimation ###############################################################
stk.e <- list()
for (i in 1:3) {
  name <- names(point_input)[i + 3]
  stk.e[[i]] <- inla.stack(
    tag = "est",
    data = list(y = point_input[[name]]),
    A = list(1, A),
    effects = list(data.frame(
      b0 = 1, travel = point_input$travel_time,
      traffic = point_input$traffic_density,
      population = point_input$population,
      java = point_input$java,
      java_dist = point_input$distancejava
    ), s = indexs[[1]])
  )
}

#### model selection ##########################################################
regressors <- c(
  "log(travel)", "log(java_dist)",
  "log(traffic)",
  "log(population)"
)
regMat <- expand.grid(
  c(TRUE, FALSE), c(TRUE, FALSE), c(TRUE, FALSE),
  c(TRUE, FALSE)
)
allModelsList <- apply(regMat, 1, function(x) {
  as.formula(
    paste(c("log(y) ~ 0 + b0 + f(s, model = spde)", regressors[x]),
      collapse = " + "
    )
  )
})


plan(multisession)
model <- allModelsList %>%
  future_map(~ inla(.x,
    data = inla.stack.data(stk.e[[1]], spde = spde[[1]]),
    control.predictor = list(A = inla.stack.A(stk.e[[1]]), compute = FALSE),
    control.compute = list(cpo = T, dic = T, waic = T),
    control.fixed = list(
      mean = list(
        default = -7,
        b0 = -4.2
      ),
      prec = 0.85
    ),
    control.family = list(hyper = list(prec = list(param = c(1, 0.8)))),
    verbose = T
  ))


result <- cbind(
  map_chr(allModelsList, ~ paste(.x)[3]),
  map_dbl(model, ~ sum(log(.$cpo$cpo))),
  map_dbl(model, ~ .$waic$waic)
)
komolgorov <- map_dbl(model[1:2], ~ ks.test(
  .$cpo$pit, "punif", 0,
  1
)$p.value)
kurtosis <- map_dbl(model[1:2], ~ kurtosis(.$cpo$pit))

# java and sole travel time/no covariates lead to bad results for
# validation
##### INLA estimation ##########################################################
stk.p <- list()
for (i in 1:5) {
  stk.p[[i]] <- inla.stack(
    tag = "pred",
    data = list(y = NA),
    A = list(1, Ap),
    effects = list(data.frame(
      b0 = 1,
      population = dp[, 3],
      travel = dp[, 4],
      traffic = dp[, 5],
      java_dist = dp[, 6],
      java = dp[, 7]
    ),
    s = indexs[[i]]
    )
  )
}

stk.full <- map(stk.e[1:3], ~ inla.stack(.x, stk.p[[1]]))
formula <- as.formula(paste("log(y) ~ 0", "b0", "f(s, model = spde)",
  "log(traffic)",
  # "log(java_dist)",
  "log(travel)",
  "log(population)",
  sep = "+"
))

inla_run <- function(formula, stk.e, stk.full, spde) {
  mod.mode <- inla(formula,
    data = inla.stack.data(stk.e, spde = spde),
    control.predictor = list(
      A = inla.stack.A(stk.e),
      compute = FALSE
    ),
    control.compute = list(cpo = T, dic = T, waic = T),
    control.fixed = list(
      mean = list(
        default = -7,
        b0 = -4.2
      ),
      prec = 0.85
    ),
    control.family = list(hyper = list(prec = list(param = c(1, 0.8)))),
    verbose = T
  )

  res <- inla(formula,
    data = inla.stack.data(stk.full, spde = spde),
    control.predictor = list(A = inla.stack.A(stk.full), compute = TRUE, link = 1),
    control.mode = list(theta = mod.mode$mode$theta, restart = FALSE),
    control.compute = list(cpo = T, dic = T, waic = T),
    control.fixed = list(
      mean = list(
        default = -7,
        b0 = -4.2
      ),
      prec = 0.85
    ),
    control.family = list(hyper = list(prec = list(param = c(1, 0.8)))),
    verbose = T
  )
  return(list(res = res, mod.mode = mod.mode))
}
res <- list()
for (i in 1:3) {
  res[[i]] <- inla_run(formula = formula, stk.e[[i]], stk.full[[i]], spde[[1]])$res
}
for (i in 1:4) {
  res[[i + 3]] <- inla_run(formula = formula, stk.e[[1]], stk.full[[1]], spde[[i + 1]])$res
}
mod.mode <- inla_run(formula = formula, stk.e[[1]], stk.full[[1]], spde[[1]])$mod.mode
# use theta from mod.mode to speed up calculation
##### summary ##################################################################
index <- lapply(stk.full, function(x) {
  inla.stack.index(
    stack = x,
    tag = "pred"
  )$data
})
save.image(file = "inla.RData")
