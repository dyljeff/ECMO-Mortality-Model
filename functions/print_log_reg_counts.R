print_log_reg_counts <- function(train, test) {
  
  tbl <- as.data.frame(matrix(nrow = 3, ncol = 5))
  colnames(tbl) <- c("group", "n_train", "perc_train", "n_test", "perc_test")

  tbl[, 1] <- c("Survived", "Died", "Total")
  tbl[, 2] <- c(table(train$Died), nrow(train))
  tbl[, 3] <- 100 * c(table(train$Died), nrow(train)) / nrow(train)
  tbl[, 4] <- c(table(test$Died), nrow(test))
  tbl[, 5] <- 100 * c(table(test$Died), nrow(test)) / nrow(test)

  tbl[, c(3, 5)] <- apply(tbl[, c(3, 5)], 2, round, 3)
  tbl[, c(2, 4)] <- apply(tbl[, c(2, 4)], 2, prettyNum, big.mark = ",")

  cat("\\begin{table}[!ht]")
  cat("\\centering")
  cat("\\begin{tabular}{|l|rr|rr|}")
  cat("\\hline")
  cat("\\multicolumn{1}{|c|}{} & \\multicolumn{2}{c|}{Training} & \\multicolumn{2}{c|}{Testing} \\\\ \n")
  cat("& N & \\% & N & \\% \\\\ \n")
  print(xtable(tbl),
    append = TRUE,
    table.placement = "!ht",
    include.rownames = FALSE,
    include.colnames = FALSE,
    hline.after = c(-1, 0, nrow(tbl) - 1, nrow(tbl)),
    only.contents = TRUE
  )
  cat("\\end{tabular}")
  cat("\\end{table}")

  return(tbl)
}
