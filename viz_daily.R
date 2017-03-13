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
                mapping = ggplot2::aes(x = date, y = data[[measure_var_name]]))  +
    ggplot2::geom_line(color = CARDINAL)  +
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
plot_distance(RA, "hello")

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
