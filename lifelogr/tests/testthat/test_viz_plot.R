##
## Entire file layu code
##
library(lifelogr)
context("Plot Functions Output")

test_that("plot_sleep produces the right error messages", {
  expect_error(plot_sleep(EX, "weekday"), 
               '"plot_type" must be one of "all", "by_weekday", "by_start_end_time", "by_datetime", "by_restless_prop", "by_restless_min", or "by_quality"')
})

test_that("plot_daily produces the right error messages", {
  expect_error(plot_daily(EX, "step"), 
               '"measure_var" must be one of "all", "steps", "floors", "distance", "calories", "mins_very", "rest_hr"')
})

test_that("plot_intraday produces the right error messages", {
  expect_error(plot_intraday(EX, "step"), 
               '"measure_var" must be one of "all", "steps", "floors", "distance", "caloriesBurned", "activeMin", "bpm", "weight"')
})

test_that("plot_sleep_start_end produces the right error messages", {
  expect_error(plot_sleep_start_end(EX, "week"),
               "'color_var' must be 'day_type' for weekend/weekday or 'day_of_week' for day of the week")
})

test_that("plot_distance produces the right error messages", {
  expect_error(plot_distance(EX, "KM"),
               "'unit' must be 'mi' or 'km'")
})

test_that("plot_i_distance produces the right error messages", {
  expect_error(plot_i_distance(EX, unit = "pound"),
               "unit must be 'lb' or 'kg'")
})
