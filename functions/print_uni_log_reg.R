print_uni_log_reg <- function(model, df = admit, caption = ""){
  
  options("scipen"=100, "digits"=2)

  model_df = model$data
  
  model_died = nrow(model_df[model_df$died==1,])
  model_survived = nrow(model_df[model_df$died==0,])
  total_died = nrow(df[df$Outcome=="Died",])
  total_survived = nrow(df[df$Outcome=="Survived",])
  
  var = names(coefficients(model))[2]
  coef = coefficients(model)[var]
  conf_int = confint.default(model)[var,]
  p_val = coefficients(summary(model))[var,4]
  
  tbl = data.frame(
    variable = var,
    complete_died = model_died,
    complete_survived = model_survived,
    missing_died = (total_died - model_died),
    missing_survived = (total_survived - model_survived),
    odds = exp(coef),
    odds_lowerCI = suppressMessages(exp(conf_int[1])),
    odds_upperCI = suppressMessages(exp(conf_int[2])),
    p_value = p_val
  )
  
  tbl = tbl %>%
    mutate(across(contains("complete"), prettyNum, big.mark=",")) %>%
    mutate(across(contains("missing"), prettyNum, big.mark=",")) %>%
    mutate(across(where(is.numeric), round, 3)) %>%
    mutate(p_value = ifelse(p_value<0.001, "<0.001", p_value),
           odds = ifelse(odds>100, ">100", odds),
           odds_lowerCI = ifelse(odds_lowerCI>100, ">100", odds_lowerCI),
           odds_upperCI = ifelse(odds_upperCI>100, ">100", odds_upperCI)) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(across(everything(), ~coalesce(na_if(.,"-"), "-")))
  
  cat("\\begin{table}[!ht]\n")
  cat("\\centering\n")
  cat("\\begin{adjustbox}{max width=\\textwidth}\n")
  cat("\\begin{tabular}{|l|rr|rr|rrrr|}\n")
  cat("\\hline\n")
  cat("Risk Factor & \\multicolumn{2}{c|}{Complete Data} & \\multicolumn{2}{c|}{Missing Data} & \\multicolumn{4}{c|}{Unadjusted}\\\\ \n")
  cat(" & Died & Survived & Died & Survived & OR & 95\\% LCL & 95\\% UCL & P-Value\\\\ \n")
  print(
    xtable(tbl),
    append = TRUE,
    include.rownames = FALSE,
    include.colnames = FALSE,
    sanitize.text.function = pretty_latex,
    hline.after = c(0, nrow(tbl)),
    only.contents = TRUE
  )
  cat("\\end{tabular}\n")
  cat("\\end{adjustbox}\n")
  cat("\\caption{Unadjusted Univariate Analysis:",caption,"}\n")
  cat("\\end{table}")
  
  options("scipen"=0, "digits"=7)
}
