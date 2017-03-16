#' @include global_var.R
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
plot_daily <- function(Person, measure_data_name, measure_var_name) {
  data <- Person$fitbit[[measure_data_name]]
  p <- ggplot2::ggplot(data = data,
                mapping = ggplot2::aes(x = date, y = data[[measure_var_name]]))  +
    ggplot2::geom_line(color = CARDINAL)  +
    ggplot2::labs(x = "Date", y = stringr::str_to_title(measure_data_name),
                  title = stringr::str_to_title(measure_data_name))
  return(p)
}

# Plot 1: Steps
plot_steps <- function(Person) {
  p <- plot_daily(Person, "steps", "steps")
  p + modelr::geom_ref_line(h = Person$target_steps, colour = "red", size = 1)
}
plot_steps(RA)

# Plot 2: Floors
plot_floors <- function(Person) {
  plot_daily(RA, "floors", "floors")
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

# Plot 7: Heart Rate Zones
calc_hr_zones <- function(Person) {
  age <- Person$user_info$age
  max_hr <- HR_MAX_BEFORE_AGE - age
  hr_zones <- list(peak = NULL, cardio = NULL, fat_burn = NULL)
  hr_zones$peak <- round(max_hr * HR_PEAK_PROP)
  hr_zones$cardio <- round(max_hr * HR_CARDIO_PROP)
  hr_zones$fat_burn <- round(max_hr * HR_FAT_BURN_PROP)
  return(hr_zones)
}
calc_hr_zones(RA)

# Inconsistent mapping - can't use this
# plot_hr_zones <- function(Person) {
#   data <- Person$fitbit$hr_zones
#   print(data)
#   data <- tidyr::gather(data, key = "zone", value = "mins", -time, -date)
#   ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = date, y = mins, fill =
#                                                         zone)) +
#     ggplot2::geom_area(position = "stack") +
#     ggplot2::labs(x = "Date", y = "Minutes", title = "Heart Rate Zones") +
#     ggplot2::guides(fill = ggplot2::guide_legend(labels = stringr::str_to_title))
#   # want to list the heart rate zones on there too
#     
# }
# plot_hr_zones(RA)

# Plot 8: Weight
plot_daily_weight <- function(Person) {
  p <- plot_daily(Person, "weight", "weight") 
  p + ggplot2::labs(y = "Weight (lbs)")
}
plot_daily_weight(RA)
# also want to plot weights over course of the day

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