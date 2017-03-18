#' @include global_var.r
#' #' A generic function to create a line graph for a single continuous variable
#' 
#' @param Person
#' @param measure_data_name
#' @param measure_var_name
#' @return 
#' @export
#' @examples
#' add(1, 2)
#' add(rnorm(10), rnorm(10))
#'
# interest: steps over one day, steps in general at those times

# ask for unit: over 5 minute interval or by range
# plot_i <- function(Person, measure_data_name, measure_var_name) {
#   data <- Person$fitbit[[measure_data_name]]
#   p <- ggplot2::ggplot(data = data,
#                        mapping = ggplot2::aes(x = time, y = data[[measure_var_name]]))  +
#     ggplot2::geom_line(color = CARDINAL)  +
#     ggplot2::labs(x = "Date", y = stringr::str_to_title(
#       stringr::str_extract(measure_data_name, "[^i].*")),
#                   title = stringr::str_to_title(
#                     stringr::str_extract(measure_data_name, "[^i].*")))
#   return(p)
# }
# 
# plot_i(RA, "ihr", "bpm")

# Averaged over the course of one day
# plot_i <- function(Person, measure_data_name, measure_var_name) {
#   data <- Person$fitbit[[measure_data_name]]
#   data$time <- lubridate::make_datetime(year = "1970", month = "01", day = "01",
#                                         hour = lubridate::hour(data$time),
#                                         min = lubridate::minute(data$time))
#   
#   data <- dplyr::group_by(data, time)
#   data <- dplyr::summarize_(data, avg = 
#                               stringr::str_c("mean(", measure_var_name, ")"))
#   p <- ggplot2::ggplot(data = data,
#                        mapping = ggplot2::aes(x = time, y = avg))  +
#     ggplot2::geom_line(color = CARDINAL)  +
#     ggplot2::scale_x_datetime(date_labels = "%H:%M %p",
#                               date_breaks = "3 hours") +
#     ggplot2::labs(x = "Time", 
#                   y = stringr::str_to_title(
#                     stringr::str_extract(measure_data_name, "[^i].*")),
#                   title = stringr::str_c(stringr::str_to_title(
#                     stringr::str_extract(measure_data_name, "[^i].*")),
#                   " by Time of Day"))
#   return(p)
# }


# Generalized
plot_i <- function(Person, measure_var_name, unit = "daily") {
  if (unit == "daily") {
    # Put data in right format
    data <- Person$fitbit_intraday[ , c("time", measure_var_name)]
    data <- dplyr::group_by(data, time)
    data <- dplyr::summarize_(data, avg = 
                                stringr::str_c("mean(", measure_var_name, ", na.rm = TRUE)"))
    data <- data[complete.cases(data), ]

    p <- ggplot2::ggplot(data = data,
                         mapping = ggplot2::aes(x = time, y = avg)) +
      ggplot2::scale_x_datetime(date_labels = "%H:%M %p", 
                                date_breaks = "3 hours") +
      ggplot2::labs(x = "Time")
  } else if (unit == "intraday") {
    data <- Person$fitbit_intraday[ , c("datetime", measure_var_name)]
    data <- data[complete.cases(data), ]
    p <- ggplot2::ggplot(data = data,
                         mapping = ggplot2::aes(x = datetime, y = data[[measure_var_name]])) +
      ggplot2::labs(x = "Date")
  } else {
    stop("'unit' must be 'daily' or 'intraday'")
  }
    p <- p +
      ggplot2::geom_line(color = CARDINAL, alpha = 0.75)  +
      ggplot2::labs(y = stringr::str_to_title(
                      stringr::str_extract(measure_var_name, "[^i].*")),
                    title = stringr::str_c(stringr::str_to_title(
                      stringr::str_extract(measure_var_name, "[^i].*")),
                      " by Time of Day"))
    return(p)
}

plot_i(RA, "steps")
plot_i(RA, "steps", "intraday")
plot_i(RA, "floors")
plot_i(RA, "floors", "intraday")
plot_i(RA, "distance")
plot_i(RA, "distance", "intraday")
plot_i(RA, "caloriesBurned")
plot_i(RA, "caloriesBurned", "intraday")
plot_i(RA, "bpm")
plot_i(RA, "bpm", "intraday")
# plot_i(RA, "`active-minutes`")

plot_i_hr_intraday <- function(Person) {
  hr_zones <- get_hr_zones(Person)

  p <- plot_i(RA, "ihr", "bpm", "intraday") + 
    ggplot2::geom_rect(mapping = ggplot2::aes(
      xmin = min(data$time, na.rm = TRUE),
      xmax = max(data$time, na.rm = TRUE),
      ymin = hr_zones$fat_burn[1],
      ymax = hr_zones$fat_burn[2]),
      fill = ORANGE_1, alpha = 0.005) +
    ggplot2::geom_rect(mapping = ggplot2::aes(
      xmin = min(data$time, na.rm = TRUE),
      xmax = max(data$time, na.rm = TRUE),
      ymin = hr_zones$fat_burn[2],
      ymax = hr_zones$cardio[2]),
      fill = "green", alpha = 0.005) +
    ggplot2::geom_rect(mapping = ggplot2::aes(
      xmin = min(data$time, na.rm = TRUE),
      xmax = max(data$time, na.rm = TRUE),
      ymin = hr_zones$cardio[2],
      ymax = hr_zones$peak[2]),
      fill = "blue", alpha = 0.005) +
    ggplot2::labs(x = "Date", y = "Heart Rate (bpm)", 
                 title = "Heart Rate over 15 minute intervals over time") +
    ggplot2::scale_fill_manual(values = c("blue", "green", ORANGE_1),
                               labels = stringr::str_c(
                                 c("Peak", "Cardio", "Fat Burn"), ": ", 
                                 hr_zones))
  # not sure why legend not showing up
  return(p)
}

plot_i_hr_intraday(RA)

# Person$data$ical_burn$activityLevel -> Very Active
# Overlay sleep with Heart Rate

# Heart Rate Data
# Heart Rate Zones
get_hr_zones <- function(Person) {
  age <- Person$user_info$age
  max_hr <- HR_MAX_BEFORE_AGE - age
  hr_zones <- list(peak = NULL, cardio = NULL, fat_burn = NULL)
  hr_zones$peak <- round(max_hr * HR_PEAK_PROP)
  hr_zones$cardio <- round(max_hr * HR_CARDIO_PROP)
  hr_zones$fat_burn <- round(max_hr * HR_FAT_BURN_PROP)
  return(hr_zones)
}
get_hr_zones(RA)

# also want to plot weights over course of the day

# Multiple Measures

# Plot 8: Weight
plot_daily_weight <- function(Person) {
  p <- plot_i(Person, "weight") 
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