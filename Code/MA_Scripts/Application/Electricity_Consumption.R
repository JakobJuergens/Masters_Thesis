# Load necessary packages
library(fds)
library(tidyverse)
library(fda)
library(stringr)
library(lubridate)

# load data into list
# These data sets consist of half-hourly electricity demands from Sunday to
# Saturday in Adelaide between 6/7/1997 and 31/3/2007.
demands <- list(
  sun_demand = fds::sundaydemand,
  mon_demand = fds::mondaydemand,
  tue_demand = fds::tuesdaydemand,
  wed_demand = fds::wednesdaydemand,
  thu_demand = fds::thursdaydemand,
  fri_demand = fds::fridaydemand,
  sat_demand = fds::saturdaydemand
)

# first combine all observations into a single tibble
# for further processing

demand_tibble <- as_tibble(
  matrix(data = NA, nrow = 7*48*508, ncol = 4)
)

names(demand_tibble) <- c('week', 'weekday', 'time', 'demand')

for(i in 1:508){
  demand_tibble$week[(7*48*(i-1) + 1):(7*48*i)] <- i
  demand_tibble$weekday[(7*48*(i-1) + 1):(7*48*i)] <- rep(x = 0:6, each = 48)
  demand_tibble$demand[(7*48*(i-1) + 1):(7*48*i)] <- unlist(
    purrr::map(
      .x = demands,
      .f = function(wd_demand){wd_demand$y[,i]}
    ))
  demand_tibble$time[(7*48*(i-1) + 1):(7*48*i)] <- unlist(
    purrr::map(
      .x = demands,
      .f = function(wd_demand){wd_demand$x}
    ))
}

# add correct date column
demand_tibble <- demand_tibble %>%
  mutate(day = 7*(week - 1) + weekday,
         DATE = as_date(x = day, origin = as_date('1997-07-06')),
         weekday = weekdays(DATE),
         month = month(DATE),
         year = year(DATE),
         time = time / 2) %>%
  select(!c(day, week))

