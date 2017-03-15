# To Do:
# see if I can generalize plots 1 and 5

# Plot 1
plot_sleep_over_time <- function(Person) {
    ggplot2::ggplot(data = tidy_sleep(Person)) +
      ggplot2::geom_line(mapping = ggplot2::aes(x = date, 
                                                y = mins / 60, 
                                                color = sleep_type)) +
      ggplot2::labs(x = "Date", y = "Sleep Duration (hours)",
                    title = "Quantity of Sleep") +
      ggplot2::guides(color = ggplot2::guide_legend(title = "Sleep Type", 
                                                    reverse = TRUE)) +
      ggplot2::scale_color_discrete(labels = c("Time Asleep", "Sleep Duration"))
}

tidy_sleep <- function(Person) {
  return(tidyr::gather(data = Person$fitbit$sleep, 
                       key = "sleep_type", 
                       value = "mins", 
                       sleepDuration, minAsleep))
    
}

# need lines to denote weekend?


# Plot 2: Percent of Restless Sleep
plot_sleep_restless_prop <- function(Person) {
  ggplot2::ggplot(data = Person$fitbit$sleep) +
    ggplot2::geom_line(mapping = ggplot2::aes(x = date, 
      y = (((sleepDuration - minAsleep) / sleepDuration) / 60) * 100)) +
    ggplot2::labs(x = "Date", y = "Percent of Restless Sleep",
                  title = "Quality of Sleep: Restlessness")
}


# Plot 3: Length of Restless Sleep
plot_sleep_restless_min <- function(Person) {
  p <- plot_daily(Person, "sleep", "restlessDuration")
  p <- p + ggplot2::labs(x = "Date", y = "Length of Restless Sleep (minutes)",
                    title = "Quality of Sleep: Restlessness")
  return(p)
}


# Plot 4: Subjective Quality of Sleep
plot_sleep_quality <- function(Person) {
  p <- plot_daily(Person, "sleep", "sleepQualityScoreA")
  p <- p + ggplot2::labs(y = "Sleep Quality Score", 
                         title = "Quality of Sleep: Quality Score")
  return(p)
}


# Plot 5: by day of week
plot_sleep_weekday <- function(Person) {
  ggplot2::ggplot(data = tidy_sleep_weekday(Person),
                  mapping = ggplot2::aes(x = weekday, 
                                         y = hours,
                                         fill = measure)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(x = "Day of the Week", y = "Hours") +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Sleep Type", 
                                                  reverse = TRUE)) +
    ggplot2::scale_fill_discrete(labels = c("Time Asleep", "Sleep Duration"))
}

tidy_sleep_weekday <- function(Person) {
  tmp <- dplyr::mutate(Person$fitbit$sleep, 
                       weekday = lubridate::wday(date, label = TRUE))
  tmp <- dplyr::group_by(tmp, weekday)
  tmp <- dplyr::summarize(tmp, sleepDurationHrs = mean(sleepDuration / 60),
                          minAsleepHrs = mean(minAsleep / 60))
  tmp <- tidyr::gather(tmp, key = "measure", value = "hours", -weekday)
  return(tmp)
}


# Plot 6: start and end
# want y axis to be month
plot_sleep_start_end <- function(Person, color_var = "day_type") {
  # Pull relevant data
  data <- create_dataset(person = Person,
                         all_variables = c("sleep", "day_type", "day_of_week", "month"),
                         all_sources = c("fitbit", rep("util", 3)))
  data <- dplyr::select(data, date, startTime, startDateTime, endTime,
                        endDateTime, day_type, day_of_week)

  # If went to sleep before midnight adjust the start time
  data$startTime <-
    ifelse(as.Date(as.POSIXct(data$startTime, format = "%H:%M")) !=
             as.Date(as.POSIXct(data$endTime, format = "%H:%M")),
           as.POSIXct(data$startTime, format = "%H:%M") - lubridate::days(1),
           as.POSIXct(data$startTime, format = "%H:%M"))

  data$startTime <- as.POSIXct(data$startTime, origin = "1970-01-01")
  data$endTime <- as.POSIXct(data$endTime, format = "%H:%M")
  print(str(data))

  p <- ggplot2::ggplot(data = data) +
    ggplot2::geom_segment(mapping =
                            ggplot2::aes_string(x = data$date,
                                         xend = data$date,
                                         y = data$startTime,
                                         yend = data$endTime,
                                         color = color_var)) +
    ggplot2::labs(x = "Date", y = "Hours Asleep") +
    ggplot2::coord_cartesian(xlim = c(max(data$date), min(data$date))) +
    ggplot2::scale_y_datetime(date_labels = "%H:%M %p",
                              date_breaks = "3 hours",
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(NULL))  +
    ggplot2::scale_color_discrete(labels = stringr::str_to_title) +
    ggplot2::coord_flip()
  return(p)
}

# toggle weekday/day of week
# add avg

# Plot 7: Rohisha's idea: raw time vs time after sleep: restless periods
create_restless_date_time <- function(Person) {
  n <- nrow(Person$fitbit$sleep$breaks[[1]])

  # not sure why this isn't working
  sapply(1:n, function(x) { create_restless_date_time_break(Person, x) })
}

create_restless_date_time_break <- function(Person, n_break) {
  breaks <- Person$fitbit$sleep$breaks[[1]]$startDateTime[[n_break]]
  date <- lubridate::make_datetime(year = breaks[1],
                           month = breaks[2],
                           day = breaks[3],
                           hour = breaks[4],
                           min = breaks[5],
                           sec = breaks[6],
                           tz = Sys.timezone())
  return(as.POSIXct(date, origin = "1970-01-01"))
}
create_restless_date_time_break(RA)

create_restless_date_time(RA)

# time zone problems
# lubridate::make_datetime(RA$fitbit$sleep$breaks[[1]]$startDateTime[[1]])
