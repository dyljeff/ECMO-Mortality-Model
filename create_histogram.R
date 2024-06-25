create_histogram <- function(data, var1, system) {
  ggplot(data, aes(x = !!sym(var1), fill = Outcome)) +
    geom_histogram(color = "black", position = "identity", bins = 30, alpha = 0.7) +
    scale_fill_manual(values = c("Survived" = "#00AFBB", "Died" = "#E7B800")) +
    labs(
      title = paste("Histogram of", var1, "for", system, "by Outcome"),
      x = var1,
      y = "Frequency"
    ) +
    theme_minimal()
}


