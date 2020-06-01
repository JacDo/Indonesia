########### set-up######################

setwd("C:/Users/seufe/bla Dropbox/Jacqueline Seufert/Unterlagen_Jacqueline/data/tmp")

####### libraries###########

library(tidyverse)
library(igraph)
library(circlize)
library(MASS)
library(mgcv)

########### data #####################

data <- read.csv("coromap.flights.csv") # all flight arrivals to Indonesia
airports <- read.csv("airport_list.csv") # geolocation of airports

########### cleaning #######################

# for every departure airport in Indonesia, count the number of connections
# between the airports

indonesia <- data %>%
  filter(departure.iata %in% airports$iata_code) %>%
  group_by(arrival.iata, departure.iata) %>%
  summarise(n = n())

############# merge location information about lon/lat###########
airport_input <- indonesia %>%
  ungroup() %>%
  dplyr::select(arrival.iata, departure.iata)

airport <- data.frame(iata = unlist(airport_input), use.names = FALSE) %>%
  distinct(iata) %>%
  left_join(airports, by = c("iata" = "iata_code"))

# make list of geocoded airports used in domestic air traffic

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
################ calculate risk################
# assign importation risk to departure airports

#idea: take the importation risk at departure airports, 
#spread risk through the network 
#sum up by ceparture airports


indonesia <- indonesia %>%
  mutate(weight_dep = case_when(
    departure.iata == "DPS" ~ 0.00053,
    departure.iata == "CGK" ~ 0.00038,
    departure.iata == "SUB" ~ 0.00016,
    departure.iata == "MDC" ~ 0.00006,
    departure.iata == "UPG" ~ 0.00004,
    departure.iata == "KNO" ~ 0.00004,
    departure.iata == "JOG" ~ 0.00004),
    
    weight_arr = case_when(
      arrival.iata == "DPS" ~ 0.00053,
      arrival.iata == "CGK" ~ 0.00038,
      arrival.iata == "SUB" ~ 0.00016,
      arrival.iata == "MDC" ~ 0.00006,
      arrival.iata == "UPG" ~ 0.00004,
    arrival.iata == "KNO" ~ 0.00004,
    arrival.iata == "JOG" ~ 0.00004
  ))


# calculate for each departure airport:

indonesia <- indonesia %>%
  group_by(departure.iata) %>%
  mutate(
    sum_departure = sum(n), # number of total flight connections
    proportion_departure = n / sum_departure, # proportion of flight going
    # to each airport
    risk = proportion_departure * weight_dep+ifelse(is.na(weight_arr),
                                                    0, weight_arr), # spread risk proportionately
    distance_geo = spatialrisk::haversine(
      latitude_dep, longitude_dep,
      latitude_arr, longitude_arr
    )
  ) # geographic distance between airports

########## plot input############
indonesia_ig <- indonesia %>%
  dplyr::select(departure.iata, arrival.iata, n)
names(indonesia_ig) <- c("from", "to", "weight")

circos.clear()
freqpairs <- indonesia_ig

set.seed(234534256)

col <- colorRamp2(
  seq(min(indonesia_ig$weight),
    max(indonesia_ig$weight),
    length = nrow(indonesia_ig)
  ),
  rainbow(363)
)
circos.par(
  cell.padding = c(0, 0, 0, 0),
  gap.degree = 2
)
chordDiagram(freqpairs,
  annotationTrack = "grid",
  preAllocateTracks = list(
    track.height = 0.1, col = col,
    link.lwd = 2, link.lty = 2
  )
)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim <- get.cell.meta.data("xlim")
  xplot <- get.cell.meta.data("xplot")
  ylim <- get.cell.meta.data("ylim")
  sector.name <- get.cell.meta.data("sector.index")
  if (abs(xplot[2] - xplot[1]) < 5) {
    circos.text(mean(xlim), ylim[1], sector.name,
      facing = "clockwise",
      niceFacing = TRUE, adj = c(0, 0.5), cex = 0.5
    )
  } else {
    circos.text(mean(xlim), ylim[1], sector.name,
      facing = "inside",
      niceFacing = TRUE, adj = c(0.5, 0), cex = 0.6
    )
  }
}, bg.border = NA)

############# prediction##########
########## igraph input##################

gD <- simplify(graph.data.frame(indonesia_ig, directed = T))

l <- layout_in_circle(gD)
E(gD)$width <- E(gD)$weight / 20

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

########## vertex measures#######
V(gD)$transitivity <- transitivity(gD, type = "local", isolates = "zero")
page_rank <- page.rank(gD)$vector
V(gD)$degree <- igraph::degree(gD, normalized = TRUE)

########### prediction prep########
input <- data.frame(
  bet = betweenness(gD),
  eig = evcent(gD, directed = T)$vector,
  trans = V(gD)$transitivity,
  page = page_rank,
  degree = V(gD)$degree
)

input$name <- rownames(input)
ind_join <- indonesia %>% left_join(input, by = c("departure.iata" = "name"))

###### addtional edge-specific measures#####
dist <- ind_join %>%
  mutate(dis = map2_dbl(
    arrival.iata, departure.iata,
    ~ as.numeric(distances(gD,
      v = V(gD)[name == .x],
      to = V(gD)[name == .y]
    ))
  )) %>%
  unnest(dis) %>%
  as_tibble() %>%
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
  unnest(sim) %>%
  as_tibble() %>%
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
  unnest(con) %>%
  as_tibble() %>%
  dplyr::select(con) %>%
  unlist()

########### build model##########

ind_join <- data.frame(ind_join,
  dis = unlist(dist), sim = unlist(sim),
  con = unlist(connectivity), bw = edge_betweenness(gD, E(gD))
)

train <- ind_join %>% filter(!is.na(weight_dep))
test <- ind_join %>% filter(is.na(weight_dep))

model <- gam(log(risk) ~ bet + eig + trans + page + degree +
  n + dis + sim + con + bw + distance_geo +
  proportion_departure, data = train)

summary(model)
gam.check(model)

################ prediction#######

test_data <- test %>%
  ungroup() %>%
  dplyr::select(
    bet, eig, trans, page, degree, n, dis, sim, con, bw,
    proportion_departure, distance_geo
  )

prediction <- cbind(test %>% dplyr::select(-risk),
  risk = predict(model, newdata = test_data)
)

result <- rbind(train, prediction)
result <- result %>% mutate(exp_risk = exp(risk) + 0.5 * model$sig2+ 
                              ifelse(is.na(weight_arr), 0, weight_arr))

final <- result %>%
  group_by(departure.iata) %>%
  summarise(sum_risk = sum(exp_risk)) # sum the proportional risks for every
# arrival airport

############### output##################

options(scipen = 500)
final <- final %>%
  mutate(
    max = max(sum_risk),
    min = min(sum_risk),
    risk_index = (sum_risk - min) / (max - min)
  )

write.csv(final, "flight_risk.csv")
ggplot(final, aes(reorder(departure.iata, -risk_index), risk_index)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5)) +
  xlab("Airports") +
  ylab("risk")
ggsave("flight_Ranking.png")
