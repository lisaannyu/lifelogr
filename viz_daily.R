# need to fix minsVery in Person

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

# Plot 5: Minutes Very - not really sure what this is
plot_mins_very <- function(Person) {
  plot_daily(Person, "minsVery", "minutesVery")
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

# Calculate maximum heart rate
max_hr <- 220 - RA$user_info$age
max_hr * c(0.5, 0.85)

RA$fitbit$hr_zones

# not sure what this is
# https://help.fitbit.com/articles/en_US/Help_article/1565#zones
# peak zone: > 85% maximum
max_hr * 0.85
# cardio: 70 - 84% max
max_hr * c(0.7, 0.84)
# fat burn: 50 - 69% max
max_hr * c(0.5, 0.69)
# out of zone: below 50%
max_hr * 0.5

# after looking at fitbit website, looks like 
  # peak (IN_DEFAULT_ZONE_2): 0 mins
  # cardio (zone2): 1 min
  # fat burn (zone1): 63 mins