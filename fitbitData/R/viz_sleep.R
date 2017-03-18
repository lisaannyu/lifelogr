#' @include viz_daily.R, experiments.R

#' A function to plot a series of six sleep graphs
#' 
#' @description Returns six plots: two are related to quantity of sleep, and 
#' four are related to quality of sleep
#' 1.  Sleep by day of week (bar graph)
#' 2.  Start and end of sleep period for each day in the range
#' 3.  Duration of sleep and time asleep over time
#' 4.  Proportion of time spent restless out of total sleep duration over time
#' 5.  Time spent restless over time (in minutes)
#' 6.  Sleep quality over time (subjective score, out of 100)
#' 
#' @param Person The user's data
#' @return A ggplot2 object
#' @importFrom ggplot2 ggplot aes geom_col guides guide_legend scale_fill_discrete
#' @export
#' @example
#' load("../data/EX.rda")
#' plot_sleep(EX)
#'
plot_sleep <- function(Person) {
  dev.hold()
  plot_sleep_weekday(Person)
  readline(prompt = "Press [enter] to continue")
  dev.flush()
  
  dev.hold()
  plot_sleep_start_end(Person)
  readline(prompt = "Press [enter] to continue")
  dev.flush()
  
  dev.hold()
  plot_sleep_over_time(Person)
  readline(prompt = "Press [enter] to continue")
  dev.flush()
  
  dev.hold()
  plot_sleep_restless_prop(Person)
  readline(prompt = "Press [enter] to continue")
  dev.flush()
  
  dev.hold()
  plot_sleep_restless_min(Person)
  readline(prompt = "Press [enter] to continue")
  dev.flush()
  
  dev.hold()
  plot_sleep_quality(Person)
  readline(prompt = "Press [enter] to continue")
  invisible()
}


# Quantity of Sleep

#' A function to preprocess sleep data for the Person object.
#' 
#' @description Preprocesses data to be used by the plot_sleep_weekday() 
#' function.  Specifically, it calculates the sleep duration and time asleep for 
#' each day of the week (in hours).
#' 
#' @param Person The user's data
#' @return A tidy data frame with the columns weekday, measure, and hours
#' @importFrom dplyr mutate group_by summarize
#' @importFrom tidyr gather
#' @importFrom lubridate wday
#' @export
#' @example
#' load("../data/EX.rda")
#' tidy_sleep_weekday(EX)
#'
tidy_sleep_weekday <- function(Person) {
  data <- create_dataset(person = Person,
                         all_variables = 
                           list("fitbit_daily" = c("sleepDurationHrs", 
                                                   "minAsleepHrs"),
                                "util" = c("day_of_week", "day_type")), 
                         time_var = c("date"))
  data <- dplyr::group_by(data, day_of_week)
  data <- dplyr::summarize(data, 
                           sleepDuration = mean(sleepDurationHrs, na.rm = TRUE),
                           minAsleep = mean(minAsleepHrs, na.rm = TRUE))
  data <- tidyr::gather(data, key = "measure", value = "hours", -day_of_week)
  return(data)
}

#' A function to plot sleep by day of week.
#' 
#' @description Returns a bar graph plotting sleep by day of week (Sunday, 
#' Monday, ...).
#' 
#' @param Person The user's data
#' @return A ggplot2 object, prints to screen
#' @importFrom ggplot2 ggplot aes geom_col labs guides guide_legend scale_fill_discrete
#' @export
#' @examples
#' load("../data/EX.rda")
#' plot_sleep_weekday(EX)
#'
# Plot 1: by day of week
plot_sleep_weekday <- function(Person) {
  p <- ggplot2::ggplot(data = tidy_sleep_weekday(Person),
                  mapping = ggplot2::aes(x = day_of_week, 
                                         y = hours,
                                         fill = measure)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(x = "Day of the Week", y = "Hours",
                  title = "Hours of Sleep by Day of the Week") +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Sleep Type", 
                                                 reverse = TRUE)) +
    ggplot2::scale_fill_discrete(labels = c("Time Asleep", "Sleep Duration"))
  print(p)
}

#' A function to plot sleep each night by start time and end time.
#' 
#' @description Returns a plot with start time of sleep and end time of sleep
#' each night, colored by weekday vs. weekend.
#' 
#' @param Person The user's data
#' @param color_var "day_type" by default for weekend/weekday, or "day_of_week"
#' for day of week.  Determines color of the lines.
#' @return A ggplot2 object, prints to screen
#' @importFrom ggplot2 ggplot aes geom_col labs guides guide_legend scale_fill_discrete
#' @export
#' @examples
#' load("../data/EX.rda")
#' plot_sleep_start_end(EX)
#' plot_sleep_start_end(EX, "day_of_week")
#'
# Plot 2: start and end
plot_sleep_start_end <- function(Person, color_var = "day_type") {
  
  if (!(color_var %in% c("day_type", "day_of_week"))) {
    stop("'color_var' must be 'day_type' for weekend/weekday or 'day_of_week' 
         for day of the week")
  }
  
  # Pull relevant data
  data <- create_dataset(person = Person,
                         all_variables = 
                           list("fitbit_daily" = c("startTime", 
                                                   "startDateTime",
                                                   "endTime",
                                                   "endDateTime"),
                                "util" = c("day_of_week", "day_type")), 
                         time_var = c("date"))
  
  # If went to sleep before midnight adjust the start time
  data$startTime <- 
    ifelse(as.Date(as.POSIXct(data$startTime, format = "%H:%M")) !=
             as.Date(as.POSIXct(data$endTime, format = "%H:%M")),
           as.POSIXct(data$startTime, format = "%H:%M") - lubridate::days(1),
           as.POSIXct(data$startTime, format = "%H:%M"))
  data$startTime <- as.POSIXct(data$startTime, origin = "1970-01-01", tz = Sys.timezone())
  data$endTime <- as.POSIXct(data$endTime, format = "%H:%M")

  p <- ggplot2::ggplot(data = data) +
    ggplot2::geom_segment(mapping =
                            ggplot2::aes_string(x = data$date,
                                                xend = data$date,
                                                y = data$startTime,
                                                yend = data$endTime,
                                                color = color_var)) +
    ggplot2::labs(x = "Date", y = "Hours Asleep",
                  title = "Sleep Start and End Times")  +
    ggplot2::coord_cartesian(xlim = c(max(data$date), min(data$date))) +
    ggplot2::scale_y_datetime(date_labels = "%H:%M %p",
                              date_breaks = "3 hours",
    )  +
    ggplot2::guides(color = ggplot2::guide_legend(NULL)) +
    ggplot2::scale_color_discrete(labels = stringr::str_to_title) +
    ggplot2::coord_flip()
  print(p)
}

# Quality of Sleep

# Plot 3
#' A function to plot sleep over time.  
#' 
#' @description Returns a line plot plotting sleep over time.  Includes sleep
#' duration and time asleep (in hours).
#' 
#' @param Person The user's data
#' @return A ggplot2 object, prints to screen
#' @export
#' @importFrom ggplot2 ggplot geom_line aes labs guides guide_legend scale_color_discrete
#' @examples
#' load("../data/EX.rda")
#' plot_sleep_over_time(EX)
#'
plot_sleep_over_time <- function(Person) {
  p <- plot_daily(Person, measures = c("sleepDurationHrs", "minAsleepHrs")) +
    ggplot2::labs(y = "Sleep Duration (hours)",
                  title = "Sleep Over Time") +
    ggplot2::guides(color = ggplot2::guide_legend(title = "Sleep Type",
                                                  reverse = TRUE)) +
    ggplot2::scale_color_discrete(labels = c("Time Asleep", "Sleep Duration"))
  print(p)
}


# Plot 4: Percent of Restless Sleep
#' A function to plot the proportion of restless sleep over time.  
#' 
#' @description Returns a line plot plotting the proportion of restless sleep 
#' over time.  The proportion is calculated as the difference between sleep
#' duration and time spent asleep over sleep duration.
#' 
#' @param Person The user's data
#' @return A ggplot2 object, prints to screen
#' @export
#' @importFrom ggplot2 ggplot geom_line aes labs
#' @examples
#' load("../data/EX.rda")
#' plot_sleep_restless_prop(EX)
#'
plot_sleep_restless_prop <- function(Person) {
  p <- plot_daily(Person, "restlessProp")
  p <- p + 
      ggplot2::labs(x = "Date", y = "Percent of Restless Sleep",
                    title = "Quality of Sleep: Restlessness (%)")
  print(p)
}


# Plot 5: Length of Restless Sleep
#' A function to plot the minutes of restless sleep over time.  
#' 
#' @description Returns a line plot plotting the length of restless sleep 
#' over time (in minutes).
#' 
#' @param Person The user's data
#' @return A ggplot2 object, prints to screen
#' @export
#' @importFrom ggplot2 labs
#' @examples
#' load("../data/EX.rda")
#' plot_sleep_restless_min(EX)
#'
plot_sleep_restless_min <- function(Person) {
  p <- plot_daily(Person, "restlessDuration")
  p <- p + ggplot2::labs(y = "Length of Restless Sleep (minutes)",
                    title = "Quality of Sleep: Restlessness (mins)")
  print(p)
}


# Plot 6: Subjective Quality of Sleep
#' A function to plot sleep quality over time.  
#' 
#' @description Returns a line plot plotting sleep quality over time.  Sleep
#' quality is a subjective score given by Fitbit
#' 
#' @param Person The user's data
#' @return A ggplot2 object, prints to screen
#' @export
#' @importFrom ggplot2 labs
#' @examples
#' load("../data/EX.rda")
#' plot_sleep_quality(EX)
#'
plot_sleep_quality <- function(Person) {
  p <- plot_daily(Person, "sleepQualityScoreA")
  p <- p + ggplot2::labs(y = "Sleep Quality Score", 
                         title = "Quality of Sleep: Quality Score")
  print(p)
}