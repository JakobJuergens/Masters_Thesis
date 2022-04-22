library(fds)

# load data for weekdays
weekday_demand <- list(
  # mon_demand = fds::mondaydemand,
  tue_demand = fds::tuesdaydemand,
  wed_demand = fds::wednesdaydemand,
  thu_demand = fds::thursdaydemand#,
  # fri_demand = fds::fridaydemand
)

# load data for weekends
weekend_demand <- list(
  sat_demand = fds::saturdaydemand,
  sun_demand = fds::sundaydemand
)

