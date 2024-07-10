require(ggplot2)
require(rlang)
require(xtable)
require(dplyr)
require(tidyr)
require(pROC)
require(ResourceSelection)
require(givitiR)
require(car)

source("functions/create_table.r")
source("functions/create_bar_graph.r")
source("functions/create_scatter.r")
source("functions/create_histogram.r")
source("functions/comparison_tests.r")
source("functions/uni_log_reg.r")
source("functions/make_numeric.r")
source("functions/create_summary.r")
source("functions/sum_stats.r")
source("functions/plot_roc_curve.r")
source("functions/pretty_decimal.r")
source("functions/pretty_integer.r")
source("functions/pretty_latex.r")
source("functions/pretty_pvalue.r")
source("functions/print_comparison_tests.r")
source("functions/print_log_reg_coefficients.r")
source("functions/print_log_reg_counts.r")
source("functions/print_log_reg_deciles.r")
source("functions/print_sum_stats.r")
source("functions/print_table.r")
source("functions/print_uni_log_reg.r")


#### Load data ####
admit <- read.csv("Data/Files/Admission_and_Discharge.csv", check.names = FALSE)
PIM <- read.csv("Data/Files/PIM.csv", check.names = FALSE)
PRISM3 <- read.csv("Data/Files/PRISM3.csv", check.names = FALSE)
proc <- read.csv(file = "Data/Files/Procedure.csv", check.names = FALSE)
proc_vars <- read.csv(file = "Data/Files/Procedure_Variables.csv", check.names = FALSE)


#### Merge necessary fields #### 
prism_columns_to_merge <- c(
  "Case Index Id", 
  "PRISM 3 Probability of Death", 
  "High Systolic Blood Pressure (mmHg)", "High Systolic Blood Pressure Unknown", 
  "Low Systolic Blood Pressure (mmHg)", "Low Systolic Blood Pressure Unknown", 
  "High Heart Rate (bpm)", "High Heart Rate Unknown", 
  "Low Heart Rate (bpm)", "Low Heart Rate Unknown", 
  "Low PaO2 (mmHg)", "Low PaO2 Unknown", 
  "High Platelet Count (10(9)/L)", "High Platelet Count Unknown", 
  "Low Platelet Count (10(9)/L)", "Low Platelet Count Unknown", 
  "High Creatinine (mg/dL)", "High Creatinine Unknown", 
  "High Blood Urea Nitrogen (mg/dL)", "High Blood Urea Nitrogen Unknown",
  "PRISM 3 Length of Stay (Days)", 
  "PRISM 3 Score", 
  "Post-Operative",
  "Admission from Inpatient Unit", 
  "Acute Diabetes", 
  "Cardiac Massage Prior to ICU Admission"
)
admit <- merge(admit, PRISM3[, prism_columns_to_merge], all.x = TRUE, all.y = FALSE)

pim_columns_to_merge <- c(
  "Case Index Id",
  "PIM3 Probability of Death",
  "PIM3 Pupillary Reaction",
  "Elective Admission to ICU",
  "Mechanical Ventilation (First Hour)",
  "Base Excess",
  "Systolic Blood Pressure",
  "PaO2 (mmHg)",
  "FiO2",
  "PIM3 Recovery from Surgery",
  "PIM3 - No Very High Risk Dx",       
  "PIM3 - No High Risk Dx",    
  "PIM3 - No Low Risk Dx", 
  "PIM3 Probability of Death"
)
admit <- merge(admit, PIM[, pim_columns_to_merge], all.x = TRUE, all.y = FALSE)


#### Transform data ####
admit <- make_numeric(admit, "Medical Length of Stay (Days)")
admit <- make_numeric(admit, "High Heart Rate (bpm)")
admit <- make_numeric(admit, "Low Heart Rate (bpm)")
admit <- make_numeric(admit, "Low Systolic Blood Pressure (mmHg)")
admit <- make_numeric(admit, "High Systolic Blood Pressure (mmHg)")
admit <- make_numeric(admit, "Low PaO2 (mmHg)")
admit <- make_numeric(admit, "High Platelet Count (10(9)/L)")
admit <- make_numeric(admit, "Low Platelet Count (10(9)/L)")
admit <- make_numeric(admit, "High Creatinine (mg/dL)")
admit <- make_numeric(admit, "High Blood Urea Nitrogen (mg/dL)")
admit <- make_numeric(admit, "PRISM 3 Score")
admit <- make_numeric(admit, "Base Excess")
admit <- make_numeric(admit, "Systolic Blood Pressure")
admit <- make_numeric(admit, "PIM3 - No Very High Risk Dx")
admit <- make_numeric(admit, "FiO2")
admit <- make_numeric(admit, "PaO2 (mmHg)")
admit <- make_numeric(admit, "Admission from Inpatient Unit")
admit <- make_numeric(admit, "Post-Operative")
admit <- make_numeric(admit, "Acute Diabetes")
admit <- make_numeric(admit, "Cardiac Massage Prior to ICU Admission")
proc <- make_numeric(proc, "Procedure Duration (Constrained to ICU Stay Days)")
admit <- make_numeric(admit, "PIM3 Probability of Death")


#### Create new fields ####
ecmo <- proc[proc$`Procedure Code` == 135, ]
ecmo <- merge(ecmo, proc_vars, by.x = "Procedure Id", by.y = "Procedure Code", all.x = TRUE, all.y = FALSE)
admit$has_ecmo_va <- (admit$`Case Index Id` %in%
                        ecmo[ecmo$`Variable Name` == "VA ECMO" & !is.na(ecmo$`Variable Name`), ]$`Case Index Id.x`)
admit$has_ecmo_vv <- (admit$`Case Index Id` %in%
                        ecmo[ecmo$`Variable Name` == "VV ECMO" & !is.na(ecmo$`Variable Name`), ]$`Case Index Id.x`)

cpr <- proc[proc$`Procedure Code` == 133, ]
admit$has_cpr_within_60min <- FALSE
for (i in 1:nrow(cpr)) {
  id <- as.character(cpr[i, "Case Index Id"])
  case_ecmo <- ecmo[ecmo$`Case Index Id.x` == id, ]
  
  case_ecmo$minutes_between_ecmo_start_and_cpr_end <- 24 * 60 * (
    as.numeric(case_ecmo$`Procedure Start - Time Since ICU Admission (Days)`) -
      as.numeric(cpr[i, ]$`Procedure End - Time Since ICU Admission (Days)`)
  )
  case_ecmo$has_cpr_within_60min <- (
    case_ecmo$minutes_between_ecmo_start_and_cpr_end < 60 &
      case_ecmo$minutes_between_ecmo_start_and_cpr_end >= 0 &
      !is.na(case_ecmo$minutes_between_ecmo_start_and_cpr_end)
  )
  
  if (admit[admit$`Case Index Id` == id, "has_cpr_within_60min"] == FALSE) {
    admit[admit$`Case Index Id` == id, "has_cpr_within_60min"] <-
      (sum(case_ecmo$has_cpr_within_60min) > 0)
  }
}
remove(cpr, ecmo, case_ecmo)

admit$Died <- ifelse(admit$Outcome == "Died", 1, 0)

# PRISM III variables
admit$prism3score_sq <- admit[["PRISM 3 Score"]]^2

admit$is_neonate <- 0
admit$is_neonate[admit[["Age at ICU Admission"]] == "Neonate Birth to 29 days"] <- 1

admit$is_over18 <- 0
admit$is_over18[admit[["Age at ICU Admission"]] %in% c("Adolescent (late) 18 years to < 21 years", "Adult 21 years and up")] <- 1

admit[["Admission from Inpatient Unit"]][is.na(admit[["Admission from Inpatient Unit"]])] <- 0
admit[["Post-Operative"]][is.na(admit[["Post-Operative"]])] <- 0
admit[["Acute Diabetes"]][is.na(admit[["Acute Diabetes"]])] <- 0
admit[["Cardiac Massage Prior to ICU Admission"]][is.na(admit[["Cardiac Massage Prior to ICU Admission"]])] <- 0
#admit[["High Platelet Count (10(9)/L)"]][is.na(admit[["High Platelet Count (10(9)/L)"]])] <- 0
#admit[["High Creatinine (mg/dL)"]][is.na(admit[["High Creatinine (mg/dL)"]])] <- 0
#admit[["High Blood Urea Nitrogen (mg/dL)"]][is.na(admit[["High Blood Urea Nitrogen (mg/dL)"]])] <- 0


# PIM 3 variables
admit$pupils_fixed_to_light <- ifelse(admit$`PIM3 Pupillary Reaction` == ">3mm and both fixed", 1, 0)

admit$base_excess_abs <- abs(admit[["Base Excess"]])
admit[["base_excess_abs"]][is.na(admit[["base_excess_abs"]])] <- 0

admit[["Systolic Blood Pressure"]][is.na(admit[["Systolic Blood Pressure"]])] <- 120
admit$systolic_bp_sq <- admit[["Systolic Blood Pressure"]]^2/1000

admit$fio_100_pao2 <- 100 * admit[["FiO2"]] / admit[["PaO2 (mmHg)"]]
admit$fio_100_pao2[is.na(admit$fio_100_pao2)] <- 0.23

admit$isrecoverybypasscardiac <- ifelse(admit$`PIM3 Recovery from Surgery` == "Yes, Recovery from bypass cardiac procedure", 1, 0)
admit$isrecoverynonbypasscardiac <- ifelse(admit$`PIM3 Recovery from Surgery` == "Yes, Recovery from non-bypass cardiac procedure", 1, 0)
admit$isrecoverynoncardiac <- ifelse(admit$`PIM3 Recovery from Surgery` == "Yes, Recovery from non-cardiac procedure", 1, 0)

admit$isveryhighriskdx <- ifelse(
  admit$`PIM3 - No Very High Risk Dx` == 0 & !is.na(admit$`PIM3 - No Very High Risk Dx`), 
  1, 0
)
admit$ishighriskdx <- ifelse(
  (admit$`PIM3 - No High Risk Dx` == 0 & !is.na(admit$`PIM3 - No High Risk Dx`)) & 
    (admit$`PIM3 - No Very High Risk Dx` == 1 | is.na(admit$`PIM3 - No Very High Risk Dx`)),
  1, 0
)
admit$islowriskdx <- ifelse(
  admit$`PIM3 - No Low Risk Dx` == 0 & !is.na(admit$`PIM3 - No Low Risk Dx`), 
  1, 0
)


#### Filter data ####
proc <- merge(proc, admit[c("Case Index Id", "Outcome")], all.x = TRUE, all.y = FALSE)
proc_ecmo <- proc[proc$`Procedure Name` == "ECMO", ]

admit_PRISM3 <- admit[admit$`Collects PRISM 3` != 0, ]
admit_PRISM3_excluding_neonates <- admit_PRISM3[admit_PRISM3$`Age at ICU Admission` != "Neonate Birth to 29 days", ]
admit_PRISM3_only_neonates <- admit_PRISM3[admit_PRISM3$`Age at ICU Admission` == "Neonate Birth to 29 days", ]

admit$has_ecmo_va <- as.integer(admit$has_ecmo_va)
admit$has_ecmo_vv <- as.integer(admit$has_ecmo_vv)
admit$has_cpr_within_60min <- as.integer(admit$has_cpr_within_60min)

admit$has_renal <- ifelse(admit$`Case Index Id` %in% proc[proc$Category=="Renal and Hepatic Support", ]$`Case Index Id`, 1, 0)  

admit_excluding_neonates <- admit[admit$`Age at ICU Admission` != "Neonate Birth to 29 days", ]
admit_only_neonates <- admit[admit$`Age at ICU Admission` == "Neonate Birth to 29 days", ]

admit_filter <- admit[admit$`Collects PRISM 3` != 0, ]

col_model_1 <- c(
  "Died",
  "PRISM 3 Score",
  "prism3score_sq",
  "Admission from Inpatient Unit",
  "Post-Operative",
  "Acute Diabetes",
  "Cardiac Massage Prior to ICU Admission",
  "is_neonate",
  "pupils_fixed_to_light",
  "Elective Admission to ICU",
  "Mechanical Ventilation (First Hour)",
  "base_excess_abs",
  "Systolic Blood Pressure",
  "systolic_bp_sq",
  "fio_100_pao2",
  "isrecoverybypasscardiac",
  "isrecoverynonbypasscardiac",
  "isrecoverynoncardiac",
  "isveryhighriskdx",
  "ishighriskdx",
  "islowriskdx",
  "has_ecmo_va",
  "has_ecmo_vv",
  "has_cpr_within_60min",
  "Weight (kg)",
  "has_renal",
  "is_over18"
)
model_1 <- admit_filter[, col_model_1]

null_model <- admit_filter$Died


col_model_3 <- c(
  "Died",
  "is_neonate",
  "has_ecmo_va",
  "has_ecmo_vv",
  "has_cpr_within_60min",
  "Weight (kg)",
  "is_over18",
  "High Platelet Count (10(9)/L)", 
  "High Creatinine (mg/dL)", 
  "High Blood Urea Nitrogen (mg/dL)",
  "High Systolic Blood Pressure (mmHg)",
  "Low Systolic Blood Pressure (mmHg)",
  "Low Heart Rate (bpm)",
  "High Heart Rate (bpm)",
  "Low PaO2 (mmHg)",
  "Low Platelet Count (10(9)/L)"
)
model_3 <- admit_filter[, col_model_3]



