make_numeric <- function(data, var) {
  data[[var]][data[[var]] == "NULL"] <- NA
  data[[var]] <- as.numeric(data[[var]])
  return(data)
}

