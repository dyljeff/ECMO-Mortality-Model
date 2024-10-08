comparison_tests <- function(data,var) {
  survived <- data[[var]][data$Outcome == "Survived"]
  died  <- data[[var]][data$Outcome == "Died"]
  
  t_test_result <- t.test(survived, died)
  wilcox_test_result <- wilcox.test(survived, died)
  
  return(list(t_test_result, wilcox_test_result))
}
