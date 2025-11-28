library(tidyverse)

#Step 1: Estimate rates of arrival 

#'Estimating Arrival Rates
#'@description Creating a function to estimate the arrival rates between station i to j during hour h
#' @param xhat average number of trips between each origin-destin pair (ex. average trips between station 10 to station 11 during hour 2)
#' @param trips_long pivot long table that assigns 1 to an arrival at a station and a -1 for a departure 
#' @param dates dates in day format
#' @param hour establishing hours in one day
#' @param station station start and end ID 
#' @param hr_pts hour ticks for each day and each station
#' @param alpha_hat average availability of bikes per station for each hr
#' @param avg_avail average of each hour that at least one bike was avail.
#' @param mu_hat trips per hour per available bike
#' @param mu_hat_max maximum arrival rate across stations and hours
#' @param arrival_rates a data frame 
#' @param simulate_arrival_time simulate one arrival time
#' @param simulate_arrival_times simulate all arrival times 
#' @param prob_i probability that a trip is kept in simulation
#' @param coin_flip a threshold to keep or discard trips, if coinflip is less than or equal to prob_i, keep the event
#' @param thinning discarding trips that don't match true arrival rates
#' @param after_thinning data frame of true trips 

estimate_arrival_rates <- function(data) {
  
  # compute the average number of trips per hour between each pair
  x_hat <- data %>%
    mutate(hour = hour(start_time)) %>%
    filter(start_station != "R", end_station != "R") %>%
    group_by(start_station, end_station, hour) %>%
    summarise(avg_trips = n() / n_distinct(as_date(start_time)), 
              .groups = "drop") 
  
  # pivot longer to get change in count 
  data$end_station <- as.character(data$end_station)
  trips_long <- data %>%
    pivot_longer(cols = c("start_station", "start_time", 
                          "end_station", "end_time"),
                 names_to = c("type", ".value"),   
                 names_pattern = "(start|end)_(.*)") %>%
    mutate(change = ifelse(type == "start", -1, 1),
           hour = hour(time)) %>%
    dplyr::select(station, time, hour, change)
  
  # add hour markers so we can get cumulative time
  dates <- unique(as_date(trips_long$time))
  hours <- c(seq(0,23,1),seq(0,23,1)+0.9999999)
  stations <- unique(trips_long$station)
  hr_pts <- expand.grid(time = dates, hour = hours, 
                        station = stations) %>%
    mutate(time = as.POSIXct(time) + hour*60*60,
           hour = hour(time))
  hr_pts$change <- 0
  trips_long <- rbind(trips_long, hr_pts)
  
  # find average availability 
  alpha_hat <- trips_long %>%
    group_by(station) %>%
    filter(station != "R") %>%
    arrange(time) %>% 
    mutate(count = cumsum(change),
           date = as_date(time)) %>%
    group_by(station, hour, date) %>%
    summarize(time_avail = 
                sum(difftime(time, lag(time), units="hours")*(count > 0), 
                    na.rm = TRUE)) %>%
    summarize(avg_avail = mean(time_avail)) %>%
    mutate(avg_avail = round(as.numeric(avg_avail), digits = 4)) %>%
    ungroup()
  
  # join the data and compute arrival rates
  mu_hat <- x_hat %>%
    left_join(alpha_hat, by = c("start_station" = "station", "hour")) %>%
    mutate(mu_hat = ifelse(avg_avail > 0, avg_trips / avg_avail, NA))
  
  return(mu_hat)
}

# Load the sample dataset
bike_data <- read_csv("/Users/kellynevaudreuil/Documents/Public Health/R/Lab/Lab 11/sample_bike.csv")

# Estimate arrival rates
arrival_rates <- estimate_arrival_rates(bike_data)

# View the results
print(arrival_rates, n = 10)

#Estimate the maximum number of trips per station per hour per available bike (8 trips per hour per available bike)
max_mu_hat<-max(arrival_rates$mu_hat, na.rm = TRUE)

#Step 2: Simulate

#' @description Simulation
simulate_arrival_time<-rexp(1, max_mu_hat)
simulate_arrival_times <- rexp(2362, max_mu_hat)


prob_i <- arrival_rates$mu_hat/max_mu_hat
coin_flip <-runif(2362)

thinning<- coin_flip <= prob_i

after_thinning<- arrival_rates[thinning,]

#WILL THIS CHANGE APPEAR???
#PLEASE WORK


