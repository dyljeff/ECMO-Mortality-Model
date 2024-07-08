admit <- read.csv("Data/Files/Admission_and_Discharge.csv", check.names = FALSE)
PIM <- read.csv("Data/Files/PIM.csv", check.names = FALSE)
PRISM3 <- read.csv("Data/Files/PRISM3.csv", check.names = FALSE)
proc <- read.csv(file = "Data/Files/Procedure.csv", check.names = FALSE)
proc_vars <- read.csv(file = "Data/Files/Procedure_Variables.csv", check.names = FALSE)

source("functions/make_numeric.R")

#### Get necessary fields #### 
prism_columns_to_merge <- c(
  "Case Index Id", 
  "High Systolic Blood Pressure (mmHg)", "High Systolic Blood Pressure Unknown", 
  "Low Systolic Blood Pressure (mmHg)", "Low Systolic Blood Pressure Unknown", 
  "High Heart Rate (bpm)", "High Heart Rate Unknown", 
  "Low Heart Rate (bpm)", "Low Heart Rate Unknown", 
  "Low PaO2 (mmHg)", "Low PaO2 Unknown", 
  "High Platelet Count (10(9)/L)", "High Platelet Count Unknown", 
  "Low Platelet Count (10(9)/L)", "Low Platelet Count Unknown", 
  "High Creatinine (mg/dL)", "High Creatinine Unknown", 
  "High Blood Urea Nitrogen (mg/dL)", "High Blood Urea Nitrogen Unknown",
  "PRISM 3 Score",
  "Post-Operative",
  "Admission from Inpatient Unit",
  "Acute Diabetes",
  "Cardiac Massage Prior to ICU Admission"
)
admit <- merge(admit, PRISM3[, prism_columns_to_merge], all.x = TRUE, all.y = FALSE)

pim_columns_to_merge <- c(
  "Case Index Id",
  "Elective Admission to ICU",
  "Mechanical Ventilation (First Hour)",
  "Base Excess",
  "PIM3 Pupillary Reaction",
  "Systolic Blood Pressure",
  "PaO2 (mmHg)",
  "FiO2",
  "PIM2 Recovery from Surgery",
  "Admitted Following Cardiac Bypass - PIM2/PIM3", 
  "PIM3 - No Very High Risk Dx",       
  "PIM3 - No High Risk Dx",    
  "PIM3 - No Low Risk Dx"    
)

admit <- merge(admit, PIM[, pim_columns_to_merge], all.x = TRUE, all.y = FALSE)



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

admit$prism3score_sq <- admit[["PRISM 3 Score"]]^2

admit$is_neonate <- 0
admit$is_neonate[admit[["Age at ICU Admission"]] == "Neonate Birth to 29 days"] <- 1

admit <- make_numeric(admit, "Base Excess")
admit$base_excess_abs <- abs(admit[["Base Excess"]])

admit <- make_numeric(admit, "Systolic Blood Pressure")
admit[["Systolic Blood Pressure"]][is.na(admit[["Systolic Blood Pressure"]])] <- 120
admit$systolic_bp_sq <- admit[["Systolic Blood Pressure"]]^2

admit <- make_numeric(admit, "PIM3 - No Very High Risk Dx")

admit <- make_numeric(admit, "FiO2")
admit <- make_numeric(admit, "PaO2 (mmHg)")
admit$fio_100_pao2 <- 100 * admit[["FiO2"]] / admit[["PaO2 (mmHg)"]]
admit$fio_100_pao2[is.na(admit$fio_100_pao2)] <- 0.23

admit$Died <- admit$Outcome
admit$Died <- ifelse(admit$Died == "Died", 1, 0)

admit <- make_numeric(admit, "Admission from Inpatient Unit")
admit <- make_numeric(admit, "Post-Operative")
admit <- make_numeric(admit, "Acute Diabetes")
admit <- make_numeric(admit, "Cardiac Massage Prior to ICU Admission")
admit <- make_numeric(admit, "Admitted Following Cardiac Bypass - PIM2/PIM3")

admit[["Admission from Inpatient Unit"]][is.na(admit[["Admission from Inpatient Unit"]])] <- 0
admit[["Acute Diabetes"]][is.na(admit[["Acute Diabetes"]])] <- 0
admit[["Cardiac Massage Prior to ICU Admission"]][is.na(admit[["Cardiac Massage Prior to ICU Admission"]])] <- 0
admit[["Post-Operative"]][is.na(admit[["Post-Operative"]])] <- 0
admit[["base_excess_abs"]][is.na(admit[["base_excess_abs"]])] <- 0
admit[["PIM2 Recovery from Surgery"]][is.na(admit[["PIM2 Recovery from Surgery"]])] <- 0
admit[["PIM3 - No Very High Risk Dx"]][is.na(admit[["PIM3 - No Very High Risk Dx"]])] <- 0
admit[["Admitted Following Cardiac Bypass - PIM2/PIM3"]][is.na(admit[["Admitted Following Cardiac Bypass - PIM2/PIM3"]])] <- 0

#### Transform data ####
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
  "Elective Admission to ICU",
  "Mechanical Ventilation (First Hour)",
  "base_excess_abs",
  "Systolic Blood Pressure",
  "systolic_bp_sq",
  "fio_100_pao2",
  "PIM2 Recovery from Surgery",
  "Admitted Following Cardiac Bypass - PIM2/PIM3", 
  "PIM3 - No Very High Risk Dx",       
  "PIM3 - No High Risk Dx",    
  "PIM3 - No Low Risk Dx"  
)
model_1 <- admit_filter[, col_model_1]


