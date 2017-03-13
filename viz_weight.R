# careful about order - or just move this to viz_daily
plot_daily_weight <- function(Person) {
  p <- plot_daily(Person, "weight", "weight") 
  p + ggplot2::labs(y = "Weight (lbs)")
}
plot_daily_weight(RA)

# Also want to actually know about time: do people weigh less in the morning?
# plot_time_weight <- function(Person) {
#   
# }