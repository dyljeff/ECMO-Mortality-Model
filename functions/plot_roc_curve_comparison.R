plot_roc_curve_comparison <- function(roc_pim, roc_prism, roc_a, roc_b, model_a_name = "", model_b_name = "") {

  plot_roc_curve_for_comparison <- function(ROC, model_name = "", color = "black", add_plot = TRUE, x_text = 2){
    text_n <- prettyNum(length(ROC$original.predictor), big.mark = ",")
    text_auc <- round(ROC$auc, 2)
    text_lcl <- round(ROC$ci[1], 2)
    text_ucl <- round(ROC$ci[3], 2)
    
    plt_roc <- plot(
      ROC, col = color, add = add_plot,
      percent = TRUE, auc.polygon = FALSE, max.auc.polygon = FALSE,
      grid = TRUE, print.auc = FALSE, show.thres = TRUE, ci = FALSE,
      main = paste0("Area ROC (Validation Set n=", prettyNum(text_n, big.mark = ","), ")")
    )    
    text(
      x = x_text, col = color, pos = 2, cex = 0.9,
      labels = paste0(
        model_name, " AUC: ", text_auc, "% (95% CI: ", text_lcl, "% - ", text_ucl, "%)"
      )
    )
  }

  plot_roc_curve_for_comparison(roc_pim, model_name = "PIM3", color = "black", add_plot = FALSE, x_text = 11)
  plot_roc_curve_for_comparison(roc_prism, model_name = "PRISM III", color = "blue", add_plot = TRUE, x_text = 8)
  plot_roc_curve_for_comparison(roc_a, model_name = model_a_name, color = "red", add_plot = TRUE, x_text = 5)
  plot_roc_curve_for_comparison(roc_b, model_name = model_b_name, color = "orange", add_plot = TRUE, x_text = 2)
  
}
