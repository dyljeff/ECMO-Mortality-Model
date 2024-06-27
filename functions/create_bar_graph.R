create_bar_graph <- function(data, var1, var2) {
  ggplot(data, aes(x = data[[var1]])) +
    geom_bar(fill = "navy", color = "black") +
    labs(title = paste0("Bar Graph of Outcomes by ", var2),
         x = var1,
         y = var2) +
    theme_minimal() +
    facet_wrap(~ data[[var2]])
}
