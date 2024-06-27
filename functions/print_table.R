print_table <- function(tbl, caption = ""){
  
  tbl = tbl %>%
    mutate(across(where(is.integer), pretty_integer)) %>%
    mutate(across(where(is.numeric), round, 2))
  
  add_hline = which(rownames(tbl) == "Total") - 1
  
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
  cat("\\caption{Mortality Rate:",caption,"}\n")
  cat("\\end{table}")

}
