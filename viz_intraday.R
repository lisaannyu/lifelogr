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

# ask for unit: over 15 minute interval or by range
plot_i <- function(Person, measure_data_name, measure_var_name) {
  data <- Person$fitbit[[measure_data_name]]
  p <- ggplot2::ggplot(data = data,
                       mapping = ggplot2::aes(x = time, y = data[[measure_var_name]]))  +
    ggplot2::geom_line(color = CARDINAL)  +
    ggplot2::labs(x = "Date", y = stringr::str_to_title(
      stringr::str_extract(measure_data_name, "[^i].*")),
                  title = stringr::str_to_title(
                    stringr::str_extract(measure_data_name, "[^i].*")))
  return(p)
}

plot_i(RA, "isteps", "steps")

plot_daily(RA, "steps", "steps")

# If aggregate over range

# Want to generalize
plot_i <- function(Person, measure_data_name, measure_var_name) {
  data <- Person$fitbit[[measure_data_name]]
  data$time <- lubridate::make_datetime(year = "1970", month = "01", day = "01",
                                        hour = lubridate::hour(data$time),
                                        min = lubridate::minute(data$time))
  
  data <- dplyr::group_by(data, time)
  data <- dplyr::summarize_(data, avg = 
                              stringr::str_c("mean(", measure_var_name, ")"))
  p <- ggplot2::ggplot(data = data,
                       mapping = ggplot2::aes(x = time, y = avg))  +
    ggplot2::geom_line(color = CARDINAL)  +
    ggplot2::scale_x_datetime(date_labels = "%H:%M %p",
                              date_breaks = "3 hours",
    ) +
    ggplot2::labs(x = "Time", 
                  y = stringr::str_to_title(
                    stringr::str_extract(measure_data_name, "[^i].*")),
                  title = stringr::str_c(stringr::str_to_title(
                    stringr::str_extract(measure_data_name, "[^i].*")),
                  " by Time of Day"))
  return(p)
}

plot_i(RA, "isteps", "steps")
plot_i(RA, "ifloors", "floors")
plot_i(RA, "idist", "distance")
plot_i(RA, "ical_burn", "`calories-burned`")
plot_i(RA, "ihr", "bpm")
plot_i(RA, "iactive_min", "`active-minutes`")

# Person$data$ical_burn$activityLevel -> Very Active
# Overlay sleep with Heart Rate

# Multiple Measures
# not sure how to get isteps, i..., etc. - create_dataset produces an error
# plot_multiple_measures <- function(Person, measures) {
#   data <- create_dataset(person = Person,
#                          all_variables = c("isteps", "idist", "ifloors", 
#                                            "iactive_min", "ical_ratio", "ihr"),
#                          all_sources = rep("fitbit", 6))
#   data <- dplyr::select(data, date, startTime, startDateTime, endTime,
#                         endDateTime, day_type, day_of_week)  
#     data <- tidyr::gather(data, key = "measure", value = "cal", -time)
#     
#     # should I create a generic 2 lines function?
#     ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = time, y = cal)) +
#       ggplot2::geom_line(mapping = ggplot2::aes(color = measure)) +
#       ggplot2::labs(x = "Date", y = "Calories", 
#                     title = "Calories Burned and Consumed") +
#       ggplot2::guides(color = ggplot2::guide_legend(NULL)) +
#       ggplot2::scale_color_discrete(labels = c("Burned", "Consumed"))
#   return(p)
# }