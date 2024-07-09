plot_roc_curve <- function(ROC) {
  
  youden <- coords(
    ROC,
    x = "best", best.method = "youden",
    ret = c("threshold", "sensitivity", "specificity", "youden")
  )

  text_n <- prettyNum(length(ROC$original.predictor), big.mark = ",")
  text_auc <- round(ROC$auc, 2)
  text_lcl <- round(ROC$ci[1], 2)
  text_ucl <- round(ROC$ci[3], 2)

  plt_roc <- plot(
    ROC,
    percent = TRUE, auc.polygon = FALSE, max.auc.polygon = FALSE,
    grid = TRUE, print.auc = FALSE, show.thres = TRUE, ci = FALSE,
    main = paste0("Area ROC (Validation Set n=", prettyNum(text_n, big.mark = ","), ")")
  )
  plt_ci <- ci.se(plt_roc, specificities = seq(0, 100, 5))
  plot(plt_ci, type = "shape", col = alpha("#6F9E90", 0.25), border = alpha("#6F9E90", 0.25))

  text(
    x = 5, pos = 2, cex = 0.9,
    labels = paste0(
      "AUC: ", text_auc, "%\n",
      "(95% CI: ", text_lcl, "% - ", text_ucl, "%)"
    )
  )

  points(youden["specificity"], youden["sensitivity"], col = "#6F9E90", pch = 19)
  text(
    youden["specificity"], youden["sensitivity"],
    labels = paste0(
      "Youden's Index\n",
      round(youden$threshold, 3),
      " (", round(youden$specificity, 2), "%, ", round(youden$sensitivity, 2), "%)"
    ),
    pos = 4,
    cex = 0.8
  )
  
}
