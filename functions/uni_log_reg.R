uni_log_reg <- function(data, var) {
  data$died <- ifelse(data$Outcome == "Died", 1, 0)
  formula <- as.formula(paste("died ~ ", var))
  model <- glm(formula, family = binomial, data = data)
}