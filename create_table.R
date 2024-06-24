create_table <- function(data, var1, var2) {
  table_result <- table(data[[var1]], data[[var2]])
  df_table <- as.data.frame.matrix(table_result)
  df_table["Total", ] <- colSums(df_table)
  table_with_total <- as.table(as.matrix(df_table))
  return(table_with_total)
}
