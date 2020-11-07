################################################################################
######### INLA validation ######################################################
################################################################################

##### set-up ###################################################################
library(sp)
library(rgdal)
library(raster)
library(tidyverse)
library(INLA)
library(ggthemes)
library(furrr)
library(scales)
library(spdep)
library(furrr)

setwd("/home/cloud/data")
load("input.Rds")
##### scale point ##############################################################
point_input$risk <- rescale(point_input$risk, c(0.01, 0.99))

##### validation ###############################################################
# validation method
# @spde SPDE
# @indexs SPDE index
# @mesh Mesh
# @point_input observations
# @coo prediction covariates
# @i index of airport
#
validation_method <- function(spde, indexs, mesh, point_input, coo, i) {
  train <- point_input[-i, ]
  test <- point_input[i, ]
  test$risk <- NA
  train_coords <- coo[-i, ]
  test_coords <- coo[i, ]
  Ae <- inla.spde.make.A(mesh = mesh, loc = train_coords)
  test_coords <- as.matrix(test_coords)
  dim(test_coords) <- c(1, 2)
  Ap <- inla.spde.make.A(mesh = mesh, loc = test_coords)
  stk.e <- inla.stack(
    data = list(y = train$risk),
    A = list(Ae, 1),
    effects = list(
      c(
        list(b0 = 1),
        indexs
      ),
      list(
        travel = train$travel_time,
        traffic = train$traffic_density,
        java = train$java,
        java_dist = train$distancejava
      )
    ),
    tag = "est"
  )

  stk.p <- inla.stack(
    data = list(y = test$risk),
    A = list(Ap, 1),
    effects = list(
      c(
        list(b0 = 1),
        indexs
      ),
      list(
        travel = test$travel_time,
        traffic = test$traffic_density,
        java = test$java,
        java_dist = test$distancejava
      )
    ),
    tag = "pred"
  )
  # put them together
  stk.full <- inla.stack(stk.e, stk.p)
  formula <- as.formula(paste("log(y) ~ 0", "b0", "f(s, model = spde)",
    "log(travel)", "log(traffic)", "java", "java_dist",
    sep = "+"
  ))
  mod.mode <- inla(formula,
    data = inla.stack.data(stk.full, spde = spde),
    control.predictor = list(link = 1, A = inla.stack.A(stk.full), compute = TRUE),
    control.compute = list(cpo = T, dic = T, waic = T),
    control.fixed = list(
      mean = list(
        default = -7,
        b0 = -4.2, java = 2
      ),
      prec = 0.85
    ),
    control.family = list(hyper = list(prec = list(param = c(1, 0.8)))),
    verbose = T
  )
  # getting the predictions
  index_pred <- inla.stack.index(stk.full, "pred")$data
  mean <- mod.mode$summary.linear.predictor[index_pred, "mean"]
  quant0.025 <- mod.mode$summary.linear.predictor[index_pred, "0.025quant"]
  quant0.975 <- mod.mode$summary.linear.predictor[index_pred, "0.975quant"]
  result <- data.frame(
    mean = mean, quant0.025 = quant0.025,
    quant0.975 = quant0.975
  )
  return(result)
}
##### apply validation #########################################################
plan(multisession)
validation_summary <- future_map_dfr(c(1:78), ~ validation_method(
  spde = spde,
  indexs = indexs,
  mesh = mesh,
  point_input = point_input,
  coo = coo, i = .x
))

validation_new <- cbind(validation_summary, risk = log(point_input$risk))
save.image("validation.Rds")
##### visualization ############################################################
setwd("C:/Users/seufe/Dropbox/Unterlagen_Jacqueline/data")
load("tmp/validation.Rds")
validation_new <- cbind(validation_new, index = 1:78)

ggplot(validation_new, aes(x = index, y = mean, color = "Prediction")) +
  geom_point() +
  geom_point(aes(x = index, y = risk, color = "True value")) +
  geom_errorbar(aes(
    ymin = quant0.025, ymax = quant0.975,
    color = "0.025/\n0.975 CI"
  ),
  width = .5, # Width of the error bars
  position = position_dodge(.9)
  ) +
  ylab("Mean") +
  xlab("Index") +
  labs(color = "Colour") +
  ggsave("tmp/coromap_validation.pdf",
    width = 1920 / 72 / 3, height = 1080 / 72 / 3,
    dpi = 72, limitsize = F
  )
write.csv(validation_new, "tmp/validation.csv")
