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
setwd("/home/cloud/data")
load("input.Rds")
##### rescale data #############################################################
point_input$risk <- rescale(point_input$risk, c(0.01, 0.99))
##### estimation ###############################################################
stk.e <- inla.stack(
  tag = "est",
  data = list(y = point_input$risk),
  A = list(1, A),
  effects = list(data.frame(
    b0 = 1, travel = point_input$travel_time,
    traffic = point_input$traffic_density,
    java = point_input$java,
    java_dist = point_input$distancejava
  ), s = indexs)
)
#### model selection ##########################################################
regressors <- c(
  "log(travel)", "log(java_dist)",
  "log(traffic)"
)
regMat <- expand.grid(
  c(TRUE, FALSE), c(TRUE, FALSE), c(TRUE, FALSE)
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
    data = inla.stack.data(stk.e, spde = spde),
    control.predictor = list(A = inla.stack.A(stk.e), compute = FALSE),
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
  map_dbl(model, ~sum(log(.$cpo$cpo))),
  map_dbl(model, ~.$waic$waic)
)
komolgorov <- map_dbl(model[1:2], ~ ks.test(.$cpo$pit, "punif", 0,
                         1)$p.value)
kurtosis <- map_dbl(model[1:2], ~kurtosis(.$cpo$pit))

# java and sole travel time/no covariates lead to bad results for
# validation
##### INLA estimation ##########################################################
stk.p <- inla.stack(
  tag = "pred",
  data = list(y = NA),
  A = list(1, Ap),
  effects = list(data.frame(
    b0 = 1,
    travel = dp[, 3],
    traffic = dp[, 4],
    java_dist = dp[, 5],
    java = dp[, 6]
  ),
  s = indexs
  )
)

stk.full <- inla.stack(stk.e, stk.p)
formula <- as.formula(paste("log(y) ~ 0", "b0", "f(s, model = spde)",
  "log(travel)", "log(traffic)", "log(java_dist)",
  sep = "+"
))
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
summary(mod.mode)
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
# use theta from mod.mode to speed up calculation
##### summary ##################################################################
summary(res)
index <- inla.stack.index(stack = stk.full, tag = "pred")$data
save.image(file = "inla.RData")
