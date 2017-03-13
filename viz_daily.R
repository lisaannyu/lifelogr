# steps
# distance
# floors
# minutesVery
# caloriesBurnedVsIntake
# getTimeInHeartRateZonesPerDay
# getRestingHeartRateData

# Generic Function - because plotting a line graph by day seems to be trivial
plot_daily <- function(Person, measure_data_name, measure_var_name) {
  data <- Person$fitbit[[measure_data_name]]
  p <- ggplot2::ggplot(data = data,
                mapping = ggplot2::aes(x = time, y = data[[measure_var_name]]))  +
    ggplot2::geom_line()  +
    ggplot2::labs(x = "Date", y = stringr::str_to_title(measure_data_name),
                  title = stringr::str_to_title(measure_data_name))
  return(p)
}

# Plot 1: Steps
plot_steps <- function(Person) {
  p <- plot_daily(Person, "steps", "steps")
  p + modelr::geom_ref_line(h = 10000, colour = "red", size = 1)
}
plot_steps(RA)

# Plot 2: Floors
plot_daily(RA, "floors", "floors")

# Plot 3: Distance
MI_TO_KM <- 1.60934 # Figure out where to put this
plot_distance <- function(Person, unit = "mi") {
  if (unit == "mi") {
    ydist <- Person$fitbit$dist$distance
  } else if (unit == "km") {
    ydist <- Person$fitbit$dist$distance * MI_TO_KM
  } else {
    stop("'unit' must be 'mi' or 'km'")
  }
  p <- plot_daily(RA, "dist", "distance")
  p + ggplot2::labs(y = paste0("Distance (", unit, ")"))
}
plot_distance(RA)
plot_distance(RA, "mi")
plot_distance(RA, "km")
plot_distance(RA, "hello")

# Plot 4: Calories burned vs intake
plot_cal_burned <- function(Person) {
  if (sum(Person$fitbit$cal_ratio$caloriesIntake) == 0) {
    p <- plot_daily(Person, "cal_ratio", "caloriesBurned")
    p <- p + labs(y = "Calories Burned")
  } else if (sum(Person$fitbit$cal_ratio$caloriesIntake) != 0) {
    data <- Person$fitbit$cal_ratio
    # need to make this tidy to plot
    # should I create a generic 2 lines function?
  }
}