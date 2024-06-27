create_table <- function(data, var1, var2) {
  table_result <- table(data[[var1]], data[[var2]])
  df_table <- as.data.frame.matrix(table_result)
  df_table["Total", ] <- colSums(df_table)
  df_table[,1] <- as.integer(round(df_table[,1], 0))
  df_table[,2] <- as.integer(round(df_table[,2], 0))
  df_table$`Mortality Rate` <- (df_table[,1] / (df_table[,1] + df_table[,2])) * 100
  df_table$`Mortality Rate`[is.nan(df_table$`Mortality Rate`)] <- 0
  table_with_total <- as.table(as.matrix(df_table))
  return(df_table)
}