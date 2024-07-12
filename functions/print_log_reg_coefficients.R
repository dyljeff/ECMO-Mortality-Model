print_log_reg_coefficients <- function(model) {
  
  options("scipen" = 100, "digits" = 2)

  tbl <- as.data.frame(matrix(nrow = length(coefficients(model)), ncol = 9))
  colnames(tbl) <- c("variable", "coef", "coef_lowerCI", "coef_upperCI", "odds", "odds_lowerCI", "odds_upperCI", "p_value", "vif")

  tbl[, 1] <- names(coefficients(model))
  tbl[, 2] <- coefficients(model)
  tbl[, 3:4] <- suppressMessages(confint.default(model))
  tbl[, 5] <- exp(tbl[, 2])
  tbl[, 6:7] <- exp(tbl[, 3:4])
  tbl[, 8] <- coefficients(summary(model))[, 4]

  if (length(model$coefficients) > 2) {
    vifs <- car::vif(model)
    if (!is.null(dim(vifs))) {
      vifs <- vifs[, "GVIF^(1/(2*Df))"]
    }
  } else {
    vifs <- rep("-", nrow(tbl))
    names(vifs) <- tbl[, 1]
  }
  for (i in 1:length(vifs)) {
    rws <- which(grepl(names(vifs)[i], tbl$variable))
    tbl[rws, "vif"] <- vifs[i]
  }

  tbl <- tbl %>%
    mutate(across(where(is.numeric), round, 3)) %>%
    mutate(
      p_value = ifelse(p_value < 0.001, "<0.001", p_value),
      odds = ifelse(odds > 100, ">100", odds),
      odds_lowerCI = ifelse(odds_lowerCI > 100, "-", odds_lowerCI),
      odds_upperCI = ifelse(odds_upperCI > 100, "-", odds_upperCI)
    ) %>%
    mutate(
      `Variable` = variable,
      `Coefficients (95% CI)` = paste0(coef, " (", coef_lowerCI, " to ", coef_upperCI, ")"),
      `Odds Ratio (95% CI)` = paste0(odds, " (", odds_lowerCI, " to ", odds_upperCI, ")"),
      `p-value` = p_value,
      `VIF` = vif
    ) %>%
    mutate(`Odds Ratio (95% CI)` = ifelse(Variable == "(Intercept)", "-", `Odds Ratio (95% CI)`)) %>%
    dplyr::select(c("Variable", "Coefficients (95% CI)", "Odds Ratio (95% CI)", "p-value", "VIF")) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(across(everything(), ~ coalesce(na_if(., "-"), "-"))) %>%
    mutate(Variable = dplyr::recode(
      Variable,
      "prism3score_sq" = "PRISM 3 Score Squared",
      "is_neonate" = "Neonate",
      "pupils_fixed_to_light" = "Pupils Fixed to Light",
      "base_excess_abs" = "Abs. Base Excess",
      "systolic_bp_sq" = "Systolic Blood Pressure Squared/1000",
      "fio_100_pao2" = "100 x Fio2/Pao2",
      "isrecoverybypasscardiac" = "Recovery from Bypass Cardiac",
      "isrecoverynonbypasscardiac" = "Recovery from Non-Bypass Cardiac",
      "isrecoverynoncardiac" = "Recovery from Non-Cardiac",
      "isveryhighriskdx" = "Very High-Risk Diagnosis",
      "ishighriskdx" = "High-Risk Diagnosis",
      "islowriskdx" = "Low-Risk Diagnosis",
      "has_ecmo_va" = "VA ECMO",
      "has_ecmo_vv" = "VV ECMO",
      "has_cpr_within_60min" = "eCPR",
      "has_renal" = "Renal Procedure",
      "is_over18" = "18 Years or Older",
      "time_before_first_ecmo" = "Days to First ECMO"
    ))

  print(
    xtable(tbl, align = "|l|l|llrr|"),
    scalebox = "0.9",
    append = TRUE,
    table.placement = "!ht",
    include.rownames = FALSE,
    include.colnames = TRUE,
    sanitize.text.function = pretty_latex,
    hline.after = c(-1, 0, nrow(tbl))
  )

  options("scipen" = 0, "digits" = 7)

  return(tbl)
}
