#' @include global_var.R, experiment.R
#' 
#' Tidy daily data.
#' 
#' @description Tidy daily data with multiple measures.
#' 
#' @param data Data with columns date and other columns of interest.  If 
#' multiple measures are passed in, they must be of the same type
#' @return Tidy data with the columns date, measures, and value
#' 
#' @export
#' @importFrom tidyr gather
#' 
#' @example
#' a <- tibble::tibble(date = c("1970-01-01", "1970-01-02", "1970-01-03"), 
#' sleepDurationHrs = c(7.5, 8.0, 7.9), 
#' minAsleepHrs = c(7.4, 7.0, 7.7)
#' )
#' tidy_multi_meas_data(a)
tidy_multi_meas_data <- function(data) {
  return(tidyr::gather(data, key = "measures", value = "value", -date))
}

#' A function to create a line graph for a single continuous variable.
#' 
#' @description Returns a generic line graph with axis labels based on the
#' names of the variables passed in
#' 
#' @param Person The user's data
#' @param measure_data_name
#' @param measure_var_name
#' @return 
#' @export
#' @importFrom ggplot2 ggplot aes geom_line labs
#' @importFrom stringr str_to_title
#' @examples
#' load("../data/EX.rda")
#' plot_daily(EX, "steps")
#' plot_daily(EX, c("steps", "distance"))
plot_daily <- function(Person, measures) {
  data <- create_dataset(person = Person,
                         all_variables = 
                           list("fitbit_daily" = measures), 
                         time_var = c("date"))
  if (length(measures) == 1) {
    p <- 
      ggplot2::ggplot(data = data,
                      mapping = ggplot2::aes(x = date, y = data[[measures]]))  +
      ggplot2::geom_line(color = CARDINAL)  +
      ggplot2::labs(y = stringr::str_to_title(measures),
                    title = stringr::str_to_title(measures))
  } else if (length(measures) > 1) {
    data <- tidy_multi_meas_data(data)
    p <- 
      ggplot2::ggplot(data = data,
                      mapping = ggplot2::aes(x = date, y = value, color = measures)) +
      ggplot2::geom_line()
  } else {
    stop("Enter a measure you wish to plot")
  }
  
  p <- p +
    ggplot2::labs(x = "Date")
  
  return(p)
}

# Plot 1: Steps
#' Plot steps per day over time.  
#' 
#' @description Returns a line plot plotting steps per day over time.  The
#' reference line refers to the user's target number of steps.
#' 
#' @param Person The user's data
#' @return A ggplot2 object, prints to screen
#' 
#' @export
#' @importFrom modelr geom_ref_line
#' @importFrom ggplot2 labs
#' @example
#' load("../data/EX.rda")
#' plot_steps(EX)
#'
plot_steps <- function(Person) {
  p <- plot_daily(Person, "steps") + 
    modelr::geom_ref_line(h = Person$target_steps, colour = "orange", 
                          size = 0.7) +
    ggplot2::labs(title = "Number of Steps Per Day")
  return(p)
}

#' Plot steps per day over time.  
#' 
#' @description Returns a line plot plotting steps per day over time.  The
#' reference line refers to the user's target number of steps.
#' 
#' @param Person The user's data
#' @return A ggplot2 object, prints to screen
#' 
#' @export
#' @importFrom ggplot2 labs
#' @example
#' load("../data/EX.rda")
#' plot_floors(EX)
# Plot 2: Floors
plot_floors <- function(Person) {
  p <- plot_daily(Person, "floors") +
    ggplot2::labs(title = "Number of Floors Per Day")
}

# Plot 3: Distance
#' Plot distance per day over time.  
#' 
#' @description Returns a line plot plotting distance in miles or kilometers per 
#' day over time
#' 
#' @param Person The user's data
#' @param unit a unit of distance, 'mi' or 'km'.  The default value is 'mi'
#' @return A ggplot2 object, prints to screen
#' 
#' @export
#' @importFrom ggplot2 labs
#' @examples
#' load("../data/EX.rda")
#' plot_distance(EX)
#' plot_distance(EX, "mi")
#' plot_distance(EX, "km")
plot_distance <- function(Person, unit = "mi") {
  
  if (unit == "mi") {
    p <- plot_daily(Person, "distance")
  } else if (unit == "km") {
    p <- plot_daily(Person, "distanceKm")
  } else {
    stop("'unit' must be 'mi' or 'km'")
  }
  
  p <- p + ggplot2::labs(y = paste0("Distance (", unit, ")"), title = "Distance")
  return(p)
}

# Plot 4: Calories burned vs intake
# I expect the intake to be highly inaccurate: who actually records all they intake?
# interesting to put with weight
# plot_cal <- function(Person) {
#   if (sum(Person$fitbit$cal_ratio$caloriesIntake) == 0) {
#     p <- plot_daily(Person, "cal_ratio", "caloriesBurned")
#     p <- p + ggplot2::labs(y = "Calories", title = "Calories Burned")
#   } else if (sum(Person$fitbit$cal_ratio$caloriesIntake) != 0) {
#     data <- Person$fitbit$cal_ratio
#     data <- tidyr::gather(data, key = "measure", value = "cal", -time)
# 
#     # should I create a generic 2 lines function?
#     ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = time, y = cal)) +
#       ggplot2::geom_line(mapping = ggplot2::aes(color = measure)) +
#       ggplot2::labs(x = "Date", y = "Calories", 
#                     title = "Calories Burned and Consumed") +
#       ggplot2::guides(color = ggplot2::guide_legend(NULL)) +
#       ggplot2::scale_color_discrete(labels = c("Burned", "Consumed"))
#   }
#   return(p)
# }
# plot_cal(RA)

# Plot 5: Minutes Very
#' Plot minutes 'very active' over time.  
#' 
#' @description Returns a line plot plotting minutes 'very active' per day over 
#' time.  'Very active' is a subjective term defined by fitbit
#' 
#' @param Person The user's data
#' 
#' @export
#' @importFrom ggplot2 labs
#' @examples
#' load("../data/EX.rda")
#' plot_mins_very(EX)
plot_mins_very <- function(Person) {
  p <- plot_daily(Person, "minutesVery")
  p <- p + ggplot2::labs(y = "Time 'Very Active' (mins)", 
                    title = "Time Spent 'Very Active' by Day")
  return(p)
}

# Plot 6: Resting Heart Rate
plot_rest_hr <- function(Person) {
  plot_daily(Person, "restingHeartRate") +
    ggplot2::labs(y = "Resting Heart Rate (bpm)", title = "Resting Heart Rate")
}
plot_rest_hr(EX)
# Should be between 60 and 100, but can be lower, especially for physically active people
# maybe add this info to the Shiny app


# Plot 8: Weight
plot_daily_weight <- function(Person) {
  p <- plot_daily(Person, "weight", "weight") 
  p + ggplot2::labs(y = "Weight (lbs)")
}
plot_daily_weight(RA)

# plot <- function(Person, type = c("weight", "sleep", etc.)){
#   switch(type)
#   case "weight"
#   plot_weight(obj)
#   case "sleep"
#   plot_sleep(obj)
#   ...
# }
# 
# plot_weight <- function(Person){
#   plot_weight
# }