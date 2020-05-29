library(httr)
library(jsonlite)
library(tidyverse)
library(data.table)
library(plyr)
library(furrr)

#################################################
####### setup############
key <- "8d0cfd86b0575aab13f2e68d339d7191"
plan(multiprocess)
setwd("C:/Users/seufe/Dropbox/Unterlagen_Jacqueline/data/tmp")
######### functions#############
##### function to clean the JSON code after request
clean <- function(i, params, link) {
  rest <- fromJSON(rawToChar(
    GET(link, query = c(params, offset = i))$content
  ))$data
  rest <- data.frame(lapply(rapply(rest, enquote, how = "unlist"), eval))
}

scrape <- function(airport, link, date, key ) {
  params <- list(
    access_key = key, flight_date = date, arr_iata = airport,
    flight_status = "landed"
  )
  start <- GET(link,
    query = c(params, offset = 0)
  )
  total <- fromJSON(rawToChar(start$content))$pagination$total
  upper <- round_any(total, 100, f = floor)
  data <- seq(0, upper, by = 100) %>%
    future_map_dfr(~ clean(., link = link, params = params))
}
########### links###################
airport_link <- "https://api.aviationstack.com/v1/airports"
link <- "https://api.aviationstack.com/v1/flights"

######### get list of airports##########
params <- list(access_key = key, country_name = "Indonesia")
start <- GET(airport_link,
             query = c(params, offset = 0)
)
total <- fromJSON(rawToChar(start$content))$pagination$total
upper <- round_any(total, 100, f = floor)

airports <- seq(0, upper, by = 100) %>%
  future_map_dfr(~ clean(.,
    params = params,
    link = airport_link
  ))
write.csv(airports,"airport_list.csv")
########## input parameters#############
date_input <- seq(as.Date("2019-12-01"), as.Date("2019-12-07"), by = "days")
arrival_input <- airports$iata_code
input <- expand.grid(date_input, arrival_input)
colnames(input) <- c("date", "airport")
input <- input %>%
  mutate_all(as.character)

######## scrape################
air_total <- future_map2_dfr(input$airport, input$date, ~ {
  scrape(
    airport = .x,
    date = .y,
    link = link,
     key = key
  )
},.progress = TRUE)

write.csv(file = "coromap.flights.csv", air_total)
