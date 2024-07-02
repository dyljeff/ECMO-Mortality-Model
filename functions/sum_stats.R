sum_stats <- function(data, var) {
  survived_summary <- summary(data[[var]][data$Outcome == "Survived"])
  died_summary <- summary(data[[var]][data$Outcome == "Died"])
  overall_summary <- summary(data[[var]])
  
  return(list(Survived = survived_summary, Died = died_summary, Overall = overall_summary))
}

