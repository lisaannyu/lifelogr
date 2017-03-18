#' @include global_var.r
#' #' A generic function to create a line graph for a single continuous variable
#' 
#' @param Person
#' @param measure_var_name
#' @param unit
#' @return 
#' @export
#' @examples
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
      ggplot2::geom_line(color = CARDINAL)  +
      ggplot2::scale_x_datetime(date_labels = "%H:%M %p", 
                                date_breaks = "3 hours") +
      ggplot2::labs(x = "Time")
  } else if (unit == "intraday") {
    data <- Person$fitbit_intraday[ , c("datetime", measure_var_name)]
    data <- data[complete.cases(data), ]
    p <- ggplot2::ggplot(data = data,
                         mapping = ggplot2::aes(x = datetime, y = data[[measure_var_name]])) +
      ggplot2::geom_line(color = CARDINAL, alpha = 0.5) +
      ggplot2::labs(x = "Date")
  } else {
    stop("'unit' must be 'daily' or 'intraday'")
  }
    p <- p +
      ggplot2::labs(y = stringr::str_to_title(
                      stringr::str_extract(measure_var_name, "[^i].*")),
                    title = stringr::str_c(stringr::str_to_title(
                      stringr::str_extract(measure_var_name, "[^i].*")),
                      " by Time of Day"))
    return(p)
}

# create a switch table
plot_i(RA, "steps")
plot_i(RA, "steps", "intraday")
plot_i(RA, "floors")
plot_i(RA, "floors", "intraday")
plot_i(RA, "distance")
plot_i(RA, "distance", "intraday")
plot_i(RA, "caloriesBurned") # calls plot_i_cal
plot_i(RA, "caloriesBurned", "intraday") # calls plot_i_cal
plot_i(RA, "bpm")
plot_i(RA, "bpm", "intraday")
# plot_i(RA, "`active-minutes`") - deal with this later

plot_i_cal <- function(Person, unit = "daily") {
  p <- plot_i(Person, "caloriesBurned", unit)
  p <- p + 
    ggplot2::labs(y = "Calories Burned",
                  title = "Calories Burned by Time of Day")
  return(p)
}
plot_i_cal(RA)
plot_i_cal(RA, "intraday")

# Heart Rate Data
# Heart Rate Zones
get_hr_zones <- function(Person) {
  age <- Person$user_info$age
  max_hr <- HR_MAX_BEFORE_AGE - age
  hr_zones <- list(peak = NULL, cardio = NULL, fat_burn = NULL)
  hr_zones$peak <- as.integer(max_hr * HR_PEAK_PROP)
  hr_zones$cardio <- as.integer(max_hr * HR_CARDIO_PROP)
  hr_zones$fat_burn <- as.integer(max_hr * HR_FAT_BURN_PROP)
  return(hr_zones)
}

plot_i_hr <- function(Person, unit = "daily") {
  if (unit == "daily") {
    p <- plot_i(Person, "bpm", unit) + 
      ggplot2::labs(y = "Heart Rate (bpm)",
                    title = "Heart Rate by Time of Day")
  } else if (unit == "intraday") {
    p <- plot_i_hr_intraday(Person)
  } else {
    stop("unit must be 'daily' or 'intraday'")
  }
  return(p)
}

plot_i_hr(RA)
plot_i_hr(RA, "intraday")

plot_i_hr_intraday <- function(Person) {
  hr_zones <- get_hr_zones(Person)

  p <- plot_i(Person, "bpm", "intraday")  + 
    ggplot2::annotate("rect",
                      xmin = rep(as.POSIXct(Person$start_date), 3),
                      xmax = rep(as.POSIXct(Person$end_date), 3),
                      ymin = c(hr_zones$fat_burn[1], hr_zones$fat_burn[2], hr_zones$cardio[2]),
                      ymax = c(hr_zones$fat_burn[2], hr_zones$cardio[2], hr_zones$peak[2]),
                      fill = c("orange", "green", "blue"),
                      alpha = 0.25) +
    ggplot2::annotate("text",
                      x = rep(as.POSIXct(Person$start_date), 3),
                      y = c(hr_zones$fat_burn[1], hr_zones$fat_burn[2], hr_zones$cardio[2]),
                      label = c("Fat burn", "Cardio", "Peak"),
                      hjust = -0.1,
                      vjust = -0.1) +
    ggplot2::labs(x = "Date", y = "Heart Rate (bpm)",
                 title = "Heart Rate over 5 minute intervals over time")
  return(p)
}

plot_i_hr_intraday(RA)

# Person$data$ical_burn$activityLevel -> Very Active
# Overlay sleep with Heart Rate


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