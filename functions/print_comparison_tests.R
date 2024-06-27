print_comparison_tests <- function(tests, caption = ""){
  
  tbl = data.frame(method = character(), p_val = numeric())
  for(i in 1:length(tests)){
    test = tests[[i]]
    
    sub_tbl = data.frame(
      method = test$method,
      p_val = pretty_pvalue(test$p.value)
    )
    tbl = rbind(tbl, sub_tbl)
  }
  colnames(tbl) = c("Test", "P-Value")
  
  cat("\\begin{table}[!ht]\n")
  cat("\\centering\n")
  cat("\\begin{adjustbox}{max width=\\textwidth}\n")
  cat("\\begin{tabular}{|l|r|}\n")
  cat("\\hline\n")
  print(xtable(tbl),
        append = TRUE,
        include.rownames = FALSE,
        include.colnames = TRUE,
        sanitize.text.function = pretty_latex,
        hline.after = c(0, nrow(tbl)),
        only.contents = TRUE)
  cat("\\end{tabular}\n")
  cat("\\end{adjustbox}\n")
  cat("\\caption{Comparison Tests:",caption,"}\n")
  cat("\\end{table}")
  
}
