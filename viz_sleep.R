# To Do:
# see if I can generalize plots 1 and 5
# Plot 6 doesn't work yet
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

# Example:
plot_sleep_over_time(RA)


# Plot 2: Percent of Restless Sleep
plot_sleep_restless_prop <- function(Person) {
  ggplot2::ggplot(data = Person$fitbit$sleep) +
    ggplot2::geom_line(mapping = ggplot2::aes(x = date, 
      y = (((sleepDuration - minAsleep) / sleepDuration) / 60) * 100)) +
    ggplot2::labs(x = "Date", y = "Percent of Restless Sleep",
                  title = "Quality of Sleep: Restlessness")
}
plot_sleep_restless_prop(RA)

# Plot 3: Length of Restless Sleep
plot_sleep_restless_min <- function(Person) {
  p <- plot_daily(Person, "sleep", "restlessDuration")
  p <- p + ggplot2::labs(x = "Date", y = "Length of Restless Sleep (minutes)",
                    title = "Quality of Sleep: Restlessness")
  return(p)
}

plot_sleep_restless_min(RA)

# Plot 4: Subjective Quality of Sleep
plot_sleep_quality <- function(Person) {
  p <- plot_daily(Person, "sleep", "sleepQualityScoreA")
  p <- p + ggplot2::labs(y = "Sleep Quality Score", 
                         title = "Quality of Sleep: Quality Score")
  return(p)
}
plot_sleep_quality(RA)


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
plot_sleep_weekday(RA)

# # Add one for start and end times - one for start time, one for end time? geom_segment?
# # startTime, endTime chr objects - don't sort correctly - need to convert to date-times
# # careful: 24 hour system - maybe 
# plot_sleep_start_end <- function(Person) {
#   ggplot2::ggplot(data = Person$fitbit$sleep) +
#     ggplot2::geom_segment(mapping = ggplot2::aes(x = date,
#                                                  xend = date,
#                                                  y = as.POSIXct(startDateTime, format = "%H:%M"),
#                                                  yend = as.POSIXct(endTime, format = "%H:%M")))
#     # ggplot2::labs(x = "Day of the Week", y = "Hours") +
#     # ggplot2::guides(fill = ggplot2::guide_legend(title = "Sleep Type",
#     #                                              reverse = TRUE)) +
#     # ggplot2::scale_fill_discrete(labels = c("Time Asleep", "Sleep Duration"))
# }
# plot_sleep_start_end(RA)
# as.POSIXct(RA$fitbit$sleep$endDateTime, format = "%H:%M") - 
#   as.POSIXct(RA$fitbit$sleep$startDateTime, format = "%H:%M")
# use lubridate::am