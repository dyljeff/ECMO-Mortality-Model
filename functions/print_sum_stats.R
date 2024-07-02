print_sum_stats <- function(stats, caption = ""){
  
  tbl <- rbind(
    stats[["Survived"]][1:6],
    stats[["Died"]][1:6],
    stats[["Overall"]][1:6]
  )
  rownames(tbl) = c("Survived", "Died", "Overall")
  
  tbl = tbl %>%
    as.data.frame() %>%
    mutate(across(where(is.numeric), round, 2))
  
  add_hline = which(rownames(tbl) == "Overall") - 1
  
  cat("\\begin{table}[!ht]\n")
  cat("\\centering\n")
  cat("\\begin{adjustbox}{max width=\\textwidth}\n")
  cat("\\begin{tabular}{|l|",paste0(rep("r", ncol(tbl)), collapse=""),"|}\n")
  cat("\\hline\n")
  print(xtable(tbl),
        append = TRUE,
        include.rownames = TRUE,
        include.colnames = TRUE,
        sanitize.text.function = pretty_latex,
        hline.after = c(0, add_hline, nrow(tbl)),
        only.contents = TRUE)
  cat("\\end{tabular}\n")
  cat("\\end{adjustbox}\n")
  cat("\\caption{Summary Statistics:",caption,"}\n")
  cat("\\end{table}")
  
}
