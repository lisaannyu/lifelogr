#' @include global_var.R
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
#' @examples
#' plot_daily(EX, "steps")
#' plot_daily(EX, "steps", "orange")
#'
# plot_daily <- function(Person, measure, color = CARDINAL) {
#   data <- create_dataset(person = Person,
#                          all_variables = 
#                            list("fitbit_daily" = measure), 
#                          time_var = c("date"))
#   p <- ggplot2::ggplot(data = data,
#                 mapping = ggplot2::aes(x = date, y = data[[measure]]))  +
#     ggplot2::geom_line(color = color)  +
#     ggplot2::labs(x = "Date", y = stringr::str_to_title(measure),
#                   title = stringr::str_to_title(measure))
#   return(p)
# }
plot_daily(EX, "steps")
plot_daily(EX, c("steps", "distance"))
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

tidy_multi_meas_data <- function(data) {
  return(tidyr::gather(data, key = "measures", value = "value", -date))
}

# Plot 1: Steps
#' A function to plot steps per day over time.  
#' 
#' @description Returns a line plot plotting steps per day over time.  The
#' reference line refers to the user's target number of steps.
#' 
#' @param Person The user's data
#' @return A ggplot2 object, prints to screen
#' @export
#' @examples
#' plot_steps(EX)
#'
plot_steps <- function(Person) {
  p <- plot_daily(Person, "steps", "steps") + 
    modelr::geom_ref_line(h = Person$target_steps, colour = "orange", 
                          size = 0.7) +
    labs(title = "Number of Steps Per Day")
  return(p)
}

# Plot 2: Floors
plot_floors <- function(Person) {
  p <- plot_daily(RA, "floors", "floors") +
    labs(title = "Number of Floors Per Day")
}
plot_floors(RA)

# Plot 3: Distance
plot_distance <- function(Person, unit = "mi") {
  if (unit == "mi") {
    ydist <- Person$fitbit$dist$distance
  } else if (unit == "km") {
    ydist <- Person$fitbit$dist$distance * MI_TO_KM
  } else {
    stop("'unit' must be 'mi' or 'km'")
  }
  p <- plot_daily(RA, "dist", "distance")
  p + ggplot2::labs(y = paste0("Distance (", unit, ")"), title = "Distance")
}
plot_distance(RA)
plot_distance(RA, "mi")
plot_distance(RA, "km")

# Plot 4: Calories burned vs intake
# I expect the intake to be highly inaccurate: who actually records all they intake?
# interesting to put with weight
plot_cal <- function(Person) {
  if (sum(Person$fitbit$cal_ratio$caloriesIntake) == 0) {
    p <- plot_daily(Person, "cal_ratio", "caloriesBurned")
    p <- p + ggplot2::labs(y = "Calories", title = "Calories Burned")
  } else if (sum(Person$fitbit$cal_ratio$caloriesIntake) != 0) {
    data <- Person$fitbit$cal_ratio
    data <- tidyr::gather(data, key = "measure", value = "cal", -time)

    # should I create a generic 2 lines function?
    ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = time, y = cal)) +
      ggplot2::geom_line(mapping = ggplot2::aes(color = measure)) +
      ggplot2::labs(x = "Date", y = "Calories", 
                    title = "Calories Burned and Consumed") +
      ggplot2::guides(color = ggplot2::guide_legend(NULL)) +
      ggplot2::scale_color_discrete(labels = c("Burned", "Consumed"))
  }
  return(p)
}
plot_cal(RA)

# Plot 5: Minutes Very
plot_mins_very <- function(Person) {
  p <- plot_daily(Person, "minsVery", "minutesVery")
  p <- p + ggplot2::labs(y = "Time Very Active (mins)", 
                    title = "Time Spent Very Active by Day")
  return(p)
}
plot_mins_very(RA)

# Plot 6: Resting Heart Rate
plot_rest_hr <- function(Person) {
  plot_daily(Person, "rest_hr", "restingHeartRate") +
    ggplot2::labs(y = "Resting Heart Rate (bpm)", title = "Resting Heart Rate")
}
plot_rest_hr(RA)
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