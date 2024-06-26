admit<-read.csv("Data/Files/Admission_and_Discharge.csv", check.names = FALSE)
PIM<-read.csv("Data/Files/PIM.csv", check.names = FALSE)
diagnosis<-read.csv("Data/Files/Diagnosis.csv", check.names = FALSE)
PRISM3<-read.csv("Data/Files/PRISM3.csv", check.names = FALSE)
proc = read.csv(file = "Data/Files/Procedure.csv", check.names = FALSE)
proc_vars = read.csv(file = "Data/Files/Procedure_Variables.csv", check.names = FALSE)

require(ggplot2)
require(rlang)

columns_to_merge = c("Case Index Id", "PRISM 3 Probability of Death", "High Systolic Blood Pressure (mmHg)", "High Systolic Blood Pressure Unknown", "Low Systolic Blood Pressure (mmHg)", "Low Systolic Blood Pressure Unknown", "High Heart Rate (bpm)", "High Heart Rate Unknown", "Low Heart Rate (bpm)", "Low Heart Rate Unknown", "Low PaO2 (mmHg)", "Low PaO2 Unknown", "High Platelet Count (10(9)/L)", "High Platelet Count Unknown","Low Platelet Count (10(9)/L)", "Low Platelet Count Unknown", "High Creatinine (mg/dL)", "High Creatinine Unknown", "High Blood Urea Nitrogen (mg/dL)", "High Blood Urea Nitrogen Unknown")
admit = merge(admit, PRISM3[, columns_to_merge], all.x = TRUE, all.y = FALSE)

ecmo = proc[proc$`Procedure Code` == 135,]
ecmo = merge(ecmo, proc_vars, by.x = "Procedure Id", by.y = "Procedure Code", all.x = TRUE, all.y = FALSE)
admit$has_ecmo_va = (admit$`Case Index Id` %in% 
                       ecmo[ecmo$`Variable Name` == "VA ECMO" & !is.na(ecmo$`Variable Name`),]$`Case Index Id.x`)
admit$has_ecmo_vv = (admit$`Case Index Id` %in% 
                       ecmo[ecmo$`Variable Name` == "VV ECMO" & !is.na(ecmo$`Variable Name`),]$`Case Index Id.x`)

cpr = proc[proc$`Procedure Code` == 133,]
admit$has_cpr_within_60min = FALSE
for(i in 1:nrow(cpr)){
  id = as.character(cpr[i,"Case Index Id"])
  case_ecmo = ecmo[ecmo$`Case Index Id.x` == id,]
  
  case_ecmo$minutes_between_ecmo_start_and_cpr_end = 24*60*(
    as.numeric(case_ecmo$`Procedure Start - Time Since ICU Admission (Days)`) - 
      as.numeric(cpr[i,]$`Procedure End - Time Since ICU Admission (Days)`)
  )
  case_ecmo$has_cpr_within_60min = (
    case_ecmo$minutes_between_ecmo_start_and_cpr_end < 60 &
      case_ecmo$minutes_between_ecmo_start_and_cpr_end >= 0 &
      !is.na(case_ecmo$minutes_between_ecmo_start_and_cpr_end)
  )
  
  if(admit[admit$`Case Index Id` == id,"has_cpr_within_60min"] == FALSE){
    admit[admit$`Case Index Id` == id,"has_cpr_within_60min"] = 
      (sum(case_ecmo$has_cpr_within_60min) > 0)
  }
}

admit_excluding_neonates <- admit[admit$`Age at ICU Admission` != "Neonate Birth to 29 days", ]
admit_only_neonates <- admit[admit$`Age at ICU Admission` == "Neonate Birth to 29 days", ]

admit_PRISM3 <- admit[admit$`Collects PRISM 3` != 0, ]

source("make_numeric.r")
admit_PRISM3 <- make_numeric(admit_PRISM3, "High Heart Rate (bpm)")
admit_PRISM3 <- make_numeric(admit_PRISM3,"Low Heart Rate (bpm)")
admit_PRISM3 <- make_numeric(admit_PRISM3, "Low Systolic Blood Pressure (mmHg)")
admit_PRISM3 <- make_numeric(admit_PRISM3, "High Systolic Blood Pressure (mmHg)")
admit_PRISM3 <- make_numeric(admit_PRISM3, "Low PaO2 (mmHg)")
admit_PRISM3 <- make_numeric(admit_PRISM3, "Low PaO2 (kPa)")
admit_PRISM3 <- make_numeric(admit_PRISM3, "High Platelet Count (10(9)/L)")
admit_PRISM3 <- make_numeric(admit_PRISM3, "Low Platelet Count (10(9)/L)")
admit_PRISM3 <- make_numeric(admit_PRISM3, "High Creatinine (mg/dL)")
admit_PRISM3 <- make_numeric(admit_PRISM3, "High Blood Urea Nitrogen (mg/dL)")

admit_PRISM3_excluding_neonates <- admit_PRISM3[admit_PRISM3$`Age at ICU Admission` != "Neonate Birth to 29 days", ]
admit_PRISM3_only_neonates <- admit_PRISM3[admit_PRISM3$`Age at ICU Admission` == "Neonate Birth to 29 days", ]

