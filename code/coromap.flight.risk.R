################################################################################
####### predict flight risk ####################################################
################################################################################

###### set-up ##################################################################

library(tidyverse)
library(scales)
library(igraph)
library(circlize)
library(MASS)
library(mgcv)
library(texreg)
library(mgcViz)
library(gridExtra)
setwd("C:/Users/seufe/Dropbox/Unterlagen_Jacqueline/data/tmp")

###### data ####################################################################
data <-
  list.files(pattern = "coromap.flights.*\\.csv") %>%
  map_df(~ read_csv(., col_types = cols(.default = "c")))

airports <- read.csv("airport_list.csv") # geolocation of airports

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

write.csv(airport, "domestic_air.csv")

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

pdf(
  file = "coromap_network.pdf",
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
  rainbow(438)
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
title("Domestic Flight Connections")
##### prediction ###############################################################
##### igraph input #############################################################

gD <- simplify(graph.data.frame(indonesia_ig, directed = T))
l <- layout_with_fr(gD)
E(gD)$width <- E(gD)$weight / 200
l <- layout_with_lgl(gD)
plot(gD,
  layout = l,
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
  con = unlist(connectivity), bw = edge_betweenness(gD, E(gD))
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
model <- gam(log(risk) ~ bet + trans + in_strength + out_strength
  + s(n_flight) + s(con) +
  s(proportion_departure), data = train)

##### diagnostics ##############################################################
summary(model)
texreg(model,
  caption = "General Additive Model",
  digits = 3, file = "gam_output.tex"
)

gam_diag <- getViz(model)

## gam_diagnostics: function to perform GAM smooths
# @x index
# @gam_diag getViz object for the model

gam_diagnostics <- function(x, gam_diag) {
  plot <- plot(sm(gam_diag, x))
  plot <- plot + l_fitLine(colour = "red") + l_rug(
    mapping = aes(x = x, y = y),
    alpha = 0.8
  ) +
    l_ciLine(mul = 5, colour = "blue", linetype = 2) +
    l_points(shape = 19, size = 1, alpha = 0.1)
  return(plot)
}
##### plotting #################################################################
plot_list <- lapply(c(1:3), function(x) gam_diagnostics(x, gam_diag))
tmp <- lapply(plot_list, function(x) x$ggObj)

grid.arrange(grobs = tmp, ncol = 2, nrow = 2)

gam_plot <- check(gam_diag,
  a.qq = list(
    method = "tnorm",
    a.cipoly = list(fill = "light blue")
  ),
  a.respoi = list(size = 0.5),
  a.hist = list(bins = 30)
)

##### prediction ###############################################################
test_data <- test %>%
  ungroup() %>%
  dplyr::select(
    bet, eig, trans, in_strength, out_strength, n_flight, dis, sim, con, bw,
    proportion_departure, distance_geo, weight_arr
  )

prediction <- cbind(test %>% dplyr::select(-risk),
  risk = exp(predict(model, newdata = test_data)) * model$sig2 / 2
)

result <- rbind(train, prediction)
result <- result %>% mutate(risk = risk +
  ifelse(is.na(weight_arr), 0, weight_arr))

##### predict risk #############################################################
options(scipen = 500)
risk_input <- result %>%
  dplyr::select(departure.iata, arrival.iata, risk)
names(risk_input) <- c("from", "to", "weight")

risk_graph <- simplify(graph.data.frame(risk_input, directed = T))
l <- layout_in_circle(risk_graph)
E(risk_graph)$width <- E(risk_graph)$weight

plot(risk_graph,
  layout = l,
  edge.arrow.size = 0.1,
  vertex.label.cex = 0.5,
  vertex.label.font = 2,
  vertex.shape = "circle",
  vertex.size = 1,
  vertex.label.color = "black",
  edge.width = E(risk_graph)$width
)
title("Risk-adjusted flight network")
##### final dataset ############################################################
final <- data.frame(
  risk = strength(risk_graph),
  iata = names(strength(risk_graph))
)

final <- final %>%
  mutate(min = min(risk), max = max(risk), risk_score = (risk - min) /
    (max - min))

write.csv(final, "flight_risk.csv")

ggplot(final, aes(reorder(iata, -risk_score), risk_score)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
  xlab("Airports") +
  ylab("Risk") +
  ggsave("tmp/coromap_flight_ranking.pdf",
    width = 1920 / 72 / 3, height = 1080 / 72 / 3,
    dpi = 72, limitsize = F
  )

ggplot(train) +
  geom_histogram(aes(x = log(risk)), bins = 15) +
  ylab("Count") +
  ggsave("tmp/coromap_airport_weight.pdf")

p <- ggplot(train, aes(sample = log(risk)))
p <- p +
  stat_qq() +
  stat_qq_line() +
  ggsave("tmp/qq_risk.pdf")
