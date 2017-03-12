plot_weight <- function(Person) {
  ggplot2::ggplot(data = Person$fitbit$weight, mapping = ggplot2::aes(x = time, y = weight)) +
     ggplot2::geom_line()  +
     ggplot2::labs(x = "Date", y = "Weight (lbs)")
}

# Usage
plot_weight(RA)
