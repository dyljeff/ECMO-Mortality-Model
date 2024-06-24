create_scatter <- function(data, var1, var2, system) {
  ggplot(data, aes_string(x = var1, y = var2)) +
    geom_point(size = 1, shape = 23, fill = "blue", color = "black") +
    labs(title = paste0(var1, " vs ", var2, " in ", system),
         x = var1,
         y = var2) +
    theme_minimal()
}
