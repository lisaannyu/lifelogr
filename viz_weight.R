plot_weight <- function(Person) {
  p <- plot_daily(Person, "weight", "weight") 
  p + ggplot2::labs(y = "Weight (lbs)")
}
plot_weight(RA)
