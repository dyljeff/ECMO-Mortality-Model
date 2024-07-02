create_summary <- function(data, var1, var2) {
  
  summary_table <- data %>%
    group_by_at(vars(!!sym(var2))) %>%
    summarize(var1 = mean(!!sym(var1), na.rm = TRUE)) %>%
    as.data.frame()
  
  # Rename the summarized variable column
  colnames(summary_table)[2] <- paste("average", var1, sep = " ")
  
  return(summary_table)
}