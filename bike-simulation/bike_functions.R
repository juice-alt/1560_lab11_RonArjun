---
  title: "STAT 1560: Week 11 Lab"
author: [Your Name]
format: pdf
editor: visual
---
  
  # Writing Your Own Functions
  
  ## Overview
  
  This week, you will turn repeated operations into reusable tools, document functions, and test them early. You will also begin tracking changes with Git. Your Week 10 lab solution will be needed for this lab.

```{r}
#| warning: false
#| message: false
library(tidyverse)
library(testthat)
set.seed(1560)
```


  ### Problem 5: Function 1 --- Estimate NHPP arrival rates
  
  We want to estimate $\mu_{s,t,h}$, the rate of user requests to travel from origin station $s$ to destination station $t$ during hour $h$, in requests per hour. Observed trips alone may understate demand because a user cannot take a bike when the origin station is empty. Instead, we estimate the request rate by adjusting observed departures for the time bikes were available:x

$$\widehat{\mu}_{s,t,h} = \frac{\widehat{x}_{s,t,h}}{\widehat{\alpha}_{s,h}},$$
  
  where $\widehat{x}_{s,t,h}$ is the number of observed customer departures from $s$ to $t$ during hour $h$, divided by the number of complete observation days, and $\widehat{\alpha}_{s,h}$ is the fraction of that hour's total observation time with at least one bike at station $s$. This adjustment assumes the request rate is the same during available and unavailable periods within each station-pair/hour group.

Use your code from Week 10 to reconstruct station inventories and measure bike availability. Generalize that code into a function such as `estimate_arrival_rates(data, window_start, window_end)` that takes trip data and an observation window and returns the estimated hourly rates.

Use `../Data/sample_bike.csv` and February 16--21, 2022 as six complete observation days: the window starts at midnight on February 16 and ends at midnight on February 22, with the endpoint excluded.

Your function should return a table with columns `origin`, `destination`, `hour` (0--23), `observed_rate`, `availability`, and `rate`. Calculate `rate` using the formula above. When availability is zero, return `NA` for `rate`: demand cannot be estimated from that group, even if its observed count is zero. Document arguments and return values with roxygen-style comments. State how you handle any input data checks.

Check the function on a few tiny dataset you can calculate by hand. For example, one departure during a two-day window gives an observed rate of 0.5 for that station-pair/hour. If the origin was available for half of that hour's total observation time, the adjusted rate is 1 request per hour.

```{r}
# Solution (for tests):
library(dplyr)
library(lubridate)
bike <- read.csv("/Users/arjunray24/Desktop/STAT1560/Data/sample_bike.csv")
estimate_arrival_rates <- function(data, window_start, window_end) {
  required <- c("start_station", "end_station",
                "start_time", "end_time")
  if (!all(required %in% names(data))) {
    stop("Missing columns")
  }
  if (any(is.na(data[, required]))) {
    stop("Missing info")
  }
  window_start <- ymd_hms(paste0(window_start, " 00:00:00"),
                          tz = "America/New_York")
  window_end <- ymd_hms(paste0(window_end, " 00:00:00"),
                        tz = "America/New_York")
  if (is.na(window_start) || is.na(window_end) ||
      window_end <= window_start) {
    stop("Invalid window")
  }
  n_days <- as.numeric(difftime(window_end, window_start,
                                units = "days"))
  if (n_days %% 1 != 0) {
    stop("must contain complete days")
  }
  bike <- data %>%
    mutate(
      start_station = as.character(start_station),
      end_station = as.character(end_station),
      start_time = ymd_hms(start_time, tz = "America/New_York"),
      end_time = ymd_hms(end_time, tz = "America/New_York"),
      row_id = row_number()
    )
  if (any(is.na(bike$start_time)) ||
      any(is.na(bike$end_time)) ||
      any(bike$end_time < bike$start_time)) {
    stop("Invalid times")
  }
  if (any(bike$start_time < window_start &
          bike$end_time >= window_start &
          bike$start_station != "R" &
          bike$end_station != "R")) {
    stop("need initial inventory")
  }
  #  departures
  departures <- bike %>%
    filter(start_station != "R", end_station != "R") %>%
    transmute(
      station = start_station,
      time = start_time,
      inventory_change = -1,
      event_type = "C",
      priority = 2,
      row_id = row_id
    )
  #  arrivals
  arrivals <- bike %>%
    filter(start_station != "R", end_station != "R") %>%
    transmute(
      station = end_station,
      time = end_time,
      inventory_change = 1,
      event_type = "C",
      priority = 3,
      row_id = row_id
    )
  # Rebalancing additions
  rebalancing_add <- bike %>%
    filter(start_station == "R", end_station != "R") %>%
    transmute(
      station = end_station,
      time = end_time,
      inventory_change = 1,
      event_type = "R",
      priority = 1,
      row_id = row_id
    )
  # Rebalancing removals
  rebalancing_remove <- bike %>%
    filter(end_station == "R", start_station != "R") %>%
    transmute(
      station = start_station,
      time = start_time,
      inventory_change = -1,
      event_type = "R",
      priority = 4,
      row_id = row_id
    )
  #sort events
  events <- bind_rows(
    rebalancing_add,
    departures,
    arrivals,
    rebalancing_remove
  ) %>%
    arrange(time, priority, row_id) %>%
    select(station, time, inventory_change, event_type)
  stations <- unique(events$station)
  inventory <- setNames(rep(0, length(stations)), stations)
  events$bike_count <- NA_integer_
  
  for (i in seq_len(nrow(events))) {
    station <- events$station[i]
    change <- events$inventory_change[i]
    inventory[station] <- inventory[station] + change
    events$bike_count[i] <- inventory[station]
  }
  if (any(events$bike_count < 0)) {
    stop("Negative station inventory")
  }
  #availability
  hours <- seq(window_start, window_end, by = "hour")
  availability_results <- data.frame()
  for (s in stations) {
    station_events <- events %>%
      filter(station == s) %>%
      arrange(time)
    for (j in seq_len(length(hours) - 1)) {
      hour_start <- hours[j]
      hour_end <- hours[j + 1]
      # Inventory at beginning of hour
      previous <- station_events %>%
        filter(time < hour_start)
      if (nrow(previous) == 0) {
        current_inventory <- 0
      } else {
        current_inventory <- tail(previous$bike_count, 1)
      }
      hour_events <- station_events %>%
        filter(time >= hour_start, time < hour_end)
      last_time <- hour_start
      available_seconds <- 0
      for (k in seq_len(nrow(hour_events))) {
        event_time <- hour_events$time[k]
        if (current_inventory > 0) {
          available_seconds <- available_seconds +
            as.numeric(difftime(event_time, last_time,
                                units = "secs"))
        }
        current_inventory <- hour_events$bike_count[k]
        last_time <- event_time
      }
      if (current_inventory > 0) {
        available_seconds <- available_seconds +
          as.numeric(difftime(hour_end, last_time,
                              units = "secs"))
      }
      availability_results <- bind_rows(
        availability_results,
        data.frame(
          origin = s,
          hour = hour(hour_start),
          available_seconds = available_seconds
        )
      )
    }
  }
  # Average availability
  availability_table <- availability_results %>%
    group_by(origin, hour) %>%
    summarise(
      availability = sum(available_seconds) / (n_days * 3600),
      .groups = "drop"
    )
  #departures during window
  trips <- bike %>%
    filter(start_station != "R",
           end_station != "R",
           start_time >= window_start,
           start_time < window_end) %>%
    mutate(hour = hour(start_time))
  counts <- trips %>%
    group_by(start_station, end_station, hour) %>%
    summarise(departures = n(), .groups = "drop") %>%
    rename(origin = start_station,
           destination = end_station)
  #24 hours for observed station pairs
  pairs <- trips %>%
    distinct(start_station, end_station) %>%
    rename(origin = start_station,
           destination = end_station)
  results <- merge(pairs, data.frame(hour = 0:23),
                   by = NULL)
  results <- results %>%
    left_join(counts, by = c("origin", "destination", "hour")) %>%
    mutate(
      departures = ifelse(is.na(departures), 0, departures),
      observed_rate = departures / n_days
    ) %>%
    left_join(availability_table, by = c("origin", "hour")) %>%
    mutate(
      rate = ifelse(availability == 0,
                    NA_real_,
                    observed_rate / availability)
    ) %>%
    select(origin, destination, hour,
           observed_rate, availability, rate) %>%
    arrange(origin, destination, hour)
  return(results)
}
# Estimate rates
rates <- estimate_arrival_rates(
  bike,
  window_start = "2022-02-16",
  window_end = "2022-02-22"
)
head(rates)
summary(rates$rate)
# Test
tiny <- data.frame(
  start_station = c("R", "A"),
  end_station = c("A", "B"),
  start_time = c("2022-02-15 12:00:00",
                 "2022-02-17 09:00:00"),
  end_time = c("2022-02-15 12:00:00",
               "2022-02-17 09:00:00")
)
test <- estimate_arrival_rates(
  tiny, "2022-02-16", "2022-02-18"
)
test_row <- test %>%
  filter(origin == "A", destination == "B", hour == 9)
stopifnot(
  test_row$observed_rate == 0.5,
  test_row$availability == 0.5,
  test_row$rate == 1
)
print("Test passed")
```

### Problem 6: Function 2 --- Simulate NHPP arrivals

Write a function such as `simulate_arrivals(rates, start_time, end_time)` that uses the first function's rate table to simulate customer requests during the half-open interval `[start_time, end_time)`. Simulate each origin--destination pair independently using **thinning**.

For each station pair $(s,t)$, find its maximum hourly rate $M_{s,t}=\max_h\widehat{\mu}_{s,t,h}$. First generate all candidate arrivals over the entire simulation window from a homogeneous Poisson process with this constant rate. Its successive waiting times are independent exponential random variables with rate $M_{s,t}$ (mean $1/M_{s,t}$ hours). Then, for each candidate time $u$, look up its hour $h(u)$ and accept the candidate independently with probability

$$p_{s,t}(u)=\frac{\widehat{\mu}_{s,t,h(u)}}{M_{s,t}}.$$

For example, if the maximum rate is 8 requests per hour and a candidate falls in an hour with rate 2, accept that candidate with probability $2/8=0.25$. Keep only accepted candidates. This process is summarized below.

``` text
Create an empty arrival table.
For each origin-destination pair:
    Set M to the maximum of its 24 hourly rates.
    If M is zero, skip this pair (it has no arrivals).
    Create an empty list of candidate times.
    Set candidate_time to start_time.
    Repeat:
        Draw a waiting time from an exponential distribution with rate M.
        Advance candidate_time by this waiting time, converted to seconds.
        If candidate_time is at or after end_time, stop this loop.
        Store candidate_time in the candidate list.
    For each stored candidate time u:
        Find its hour of the day and look up this pair's hourly rate.
Set acceptance_probability to hourly_rate / M.
Draw U independently from Uniform(0, 1).
If U < acceptance_probability:
  Append origin, destination, and u to the arrival table.
Sort the arrival table by arrival_time and return it.
```

Your function should return columns `origin`, `destination`, and `arrival_time`, including a correctly structured empty table if no arrivals occur. Document the function and test a few boundary cases.

```{r}
# Solution (for tests):

simulate_arrivals <- function(rates, start_time, end_time) {
  required <- c("origin", "destination", "hour", "rate")
  if (!all(required %in% names(rates))) {
    stop("missing columns")
  }
  start_time <- ymd_hms(start_time, tz = "America/New_York")
  end_time <- ymd_hms(end_time, tz = "America/New_York")
  if (length(start_time) != 1 || length(end_time) != 1 ||
      is.na(start_time) || is.na(end_time) ||
      end_time <= start_time) {
    stop("Invalid window")
  }
  if (any(is.na(rates$origin)) ||
      any(is.na(rates$destination)) ||
      any(is.na(rates$hour)) ||
      any(!is.finite(rates$hour)) ||
      any(rates$hour < 0) || any(rates$hour > 23) ||
      any(rates$hour %% 1 != 0)) {
    stop("invalid station/hour")
  }
  if (!is.numeric(rates$rate) ||
      any(!is.finite(rates$rate[!is.na(rates$rate)])) ||
      any(rates$rate < 0, na.rm = TRUE)) {
    stop("invalid arrival rates")
  }
  if (any(duplicated(rates[, c("origin", "destination", "hour")]))) {
    stop("duplicate combinations")
  }
  # Create empty arrival table
  arrivals <- data.frame(
    origin = character(),
    destination = character(),
    arrival_time = as.POSIXct(character(),
                              tz = "America/New_York")
  )
  # Identify pairs
  pairs <- rates %>%
    distinct(origin, destination)
  for (i in seq_len(nrow(pairs))) {
    s <- pairs$origin[i]
    t <- pairs$destination[i]
    pair_rates <- rates %>%
      filter(origin == s, destination == t)
    # NA rates represent unidentified demand, not measured zero
    # For this simulation, treat those hours as zero
    hourly_rates <- rep(0, 24)
    hourly_rates[pair_rates$hour + 1] <-
      ifelse(is.na(pair_rates$rate), 0, pair_rates$rate)
    # Find maximum hourly rate
    M <- max(hourly_rates)
    if (M == 0) {
      next
    }
    # Generate candidate arrivals
    candidate_time <- start_time
    candidates <- as.POSIXct(character(),
                             tz = "America/New_York")
    repeat {
      #  waiting timeconverted to seconds
      waiting_time <- rexp(1, rate = M) * 3600
      candidate_time <- candidate_time + waiting_time
      if (candidate_time >= end_time) {
        break
      }
      candidates <- c(candidates, candidate_time)
    }
    # Apply thinning
    for (j in seq_along(candidates)) {
      u <- candidates[j]
      h <- hour(u)
      hourly_rate <- hourly_rates[h + 1]
      acceptance_probability <- hourly_rate / M
      if (runif(1) < acceptance_probability) {
        arrivals <- bind_rows(
          arrivals,
          data.frame(
            origin = s,
            destination = t,
            arrival_time = u
          )
        )
      }
    }
  }
  # Sort by arrival time
  arrivals <- arrivals %>%
    arrange(arrival_time)
  return(arrivals)
}
# Simulate arrivals
set.seed(5)
simulated_arrivals <- simulate_arrivals(
  rates,
  start_time = "2022-02-22 00:00:00",
  end_time = "2022-02-23 00:00:00"
)
head(simulated_arrivals)
nrow(simulated_arrivals)
# Check: 0 arrival rates
test_zero <- data.frame(
  origin = "A",
  destination = "B",
  hour = 0:23,
  rate = rep(0, 24)
)
zero_arrivals <- simulate_arrivals(
  test_zero,
  "2022-02-22 00:00:00",
  "2022-02-23 00:00:00"
)
stopifnot(
  nrow(zero_arrivals) == 0,
  all(names(zero_arrivals) ==
        c("origin", "destination", "arrival_time"))
)
# Check: Arrivals must be in the window
test_rates <- data.frame(
  origin = "A",
  destination = "B",
  hour = 0:23,
  rate = rep(10, 24)
)
set.seed(5)
test_arrivals <- simulate_arrivals(
  test_rates,
  "2022-02-22 09:00:00",
  "2022-02-22 10:00:00"
)
stopifnot(
  nrow(test_arrivals) > 0,
  all(test_arrivals$arrival_time >=
        ymd_hms("2022-02-22 09:00:00", tz = "America/New_York")),
  all(test_arrivals$arrival_time <
        ymd_hms("2022-02-22 10:00:00", tz = "America/New_York"))
)
print("All checks passed")
```

## Submission

Submit your completed rendered PDF and a link to your github repository for part 2. You and your partner will have the same github repository but will submit separate quarto pdfs.
