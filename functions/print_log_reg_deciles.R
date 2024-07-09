print_log_reg_deciles <- function(HOSLEM) {
  
  tbl <- as.data.frame(matrix(nrow = nrow(HOSLEM$observed), ncol = 7))
  colnames(tbl) <- c(
    "group", "probability", "n",
    "observed_survived", "expected_survived", "observed_died", "expected_died"
  )

  tbl[, 1] <- 1:nrow(HOSLEM$observed)
  tbl[, 2] <- rownames(HOSLEM$observed)
  tbl[, 3] <- rowSums(HOSLEM$observed)
  tbl[, 4] <- HOSLEM$observed[, "y0"]
  tbl[, 5] <- HOSLEM$expected[, "yhat0"]
  tbl[, 6] <- HOSLEM$observed[, "y1"]
  tbl[, 7] <- HOSLEM$expected[, "yhat1"]

  tbl[, c(5, 7)] <- apply(tbl[, c(5, 7)], 2, round, 2)
  tbl[is.na(tbl)] <- 0
  tbl <- rbind(tbl, c("Total", "", colSums(tbl[, 3:7])))
  tbl[, 3:7] <- apply(tbl[, 3:7], 2, prettyNum, big.mark = ",")

  cat("\\begin{table}[!ht]")
  cat("\\centering")
  cat("\\begin{tabular}{|c|l|r|rr|rr|}")
  cat("\\hline\n ")
  cat("\\multicolumn{1}{|c|}{Group} & \\multicolumn{1}{c|}{Probability} & \\multicolumn{1}{c|}{N} &
      \\multicolumn{1}{c}{Observed} & \\multicolumn{1}{c|}{Expected} & \\multicolumn{1}{c}{Observed} &
      \\multicolumn{1}{c|}{Expected} \\\\ \n")
  cat("&  &  & \\multicolumn{1}{c}{Survived} & \\multicolumn{1}{c|}{Survived} &
      \\multicolumn{1}{c}{Died} & \\multicolumn{1}{c|}{Died} \\\\ \n")
  print(xtable(tbl),
    append = TRUE,
    table.placement = "!ht",
    include.rownames = FALSE,
    include.colnames = FALSE,
    hline.after = c(-1, 0, nrow(tbl) - 1, nrow(tbl)),
    only.contents = TRUE
  )
  cat("\\end{tabular} ")
  cat(paste0("\\caption{Validation Set: Quantiles of Mortality Risk} "))
  cat("\\end{table}")

  return(tbl)
}
