#### Demographics ####
plt_race_all <- create_bar_graph(admit, "Outcome", "Race")
tbl_race_all <- create_table(admit, "Race", "Outcome")

plt_sex_all <- create_bar_graph(admit, "Outcome", "Sex Assigned at Birth")
tbl_sex_all <- create_table(admit, "Sex Assigned at Birth", "Outcome")

plt_age_all <- create_bar_graph(admit, "Outcome", "Age at ICU Admission")
tbl_age_all <- create_table(admit, "Age at ICU Admission", "Outcome")

plt_ethnicity_all <- create_bar_graph(admit, "Outcome", "Ethnicity")
tbl_ethnicity_all <- create_table(admit, "Ethnicity", "Outcome")

#### Procedure Variables ####
plt_ecmo_va_all <- create_bar_graph(admit, "Outcome", "has_ecmo_va")
tbl_ecmo_va_all <- create_table(admit, "has_ecmo_va", "Outcome")
plt_ecmo_va_neonates <- create_bar_graph(admit_only_neonates, "Outcome", "has_ecmo_va")
tbl_ecmo_va_neonates <- create_table(admit_only_neonates, "has_ecmo_va", "Outcome")

plt_ecmo_vv_all <- create_bar_graph(admit, "Outcome", "has_ecmo_vv")
tbl_ecmo_vv_all <- create_table(admit, "has_ecmo_vv", "Outcome")
plt_ecmo_vv_neonates <- create_bar_graph(admit_only_neonates, "Outcome", "has_ecmo_vv")
tbl_ecmo_vv_neonates <- create_table(admit_only_neonates, "has_ecmo_vv", "Outcome")

plt_cpr60_all <- create_bar_graph(admit, "Outcome", "has_cpr_within_60min")
tbl_cpr60_all <- create_table(admit, "has_cpr_within_60min", "Outcome")
plt_cpr60_neonates <- create_bar_graph(admit_only_neonates, "Outcome", "has_cpr_within_60min")
tbl_cpr60_neonates <- create_table(admit_only_neonates, "has_cpr_within_60min", "Outcome")

#### Weight: Neonates, Non-Neonates, All  ####
plt_weight_non_neonates1 <- create_scatter(admit_excluding_neonates, "Weight (kg)", "Outcome", "Age > 29 Days")
plt_weight_non_neonates2 <- create_histogram(admit_excluding_neonates, "Weight (kg)", "Non-Neonates")
test_weight_non_neonates <- comparison_tests(admit_excluding_neonates, "Weight (kg)")
univ_weight_non_neonates <- uni_log_reg(admit_excluding_neonates,"`Weight (kg)`")

plt_weight_neonates1 <- create_scatter(admit_only_neonates, "Weight (kg)", "Outcome", "Age < 29 Days")
plt_weight_neonates2 <- create_histogram(admit_only_neonates, "Weight (kg)", "Neonates")
test_weight_neonates <- comparison_tests(admit_only_neonates, "Weight (kg)")
univ_weight_neonates <- uni_log_reg(admit_only_neonates,"`Weight (kg)`")

plt_weight_all1 <- create_scatter(admit, "Weight (kg)", "Outcome", "All Pediatric Patients")
plt_weight_all2 <- create_histogram(admit, "Weight (kg)", "Patients")
test_weight_all <- comparison_tests(admit, "Weight (kg)")
univ_weight_all <- uni_log_reg(admit,"`Weight (kg)`")

#### PRISM III Labs ####
plt_low_sbp_all1 <- create_scatter(admit_PRISM3,"Low Systolic Blood Pressure (mmHg)","Outcome", "PRISM3 Patients")
plt_low_sbp_all2 <- create_histogram(admit_PRISM3, "Low Systolic Blood Pressure (mmHg)", "PRISM 3 Patients")
test_low_sbp_all <- comparison_tests(admit_PRISM3,"Low Systolic Blood Pressure (mmHg)")
univ_low_sbp_all <- uni_log_reg(admit_PRISM3,"`Low Systolic Blood Pressure (mmHg)`")

plt_high_bpm_all1 <- create_scatter(admit_PRISM3,"High Heart Rate (bpm)","Outcome", "PRISM3 Patients")
plt_high_bpm_all2 <- create_histogram(admit_PRISM3, "High Heart Rate (bpm)", "PRISM3 Patients")
test_high_bpm_all <- comparison_tests(admit_PRISM3, "High Heart Rate (bpm)")
univ_high_bpm_all <- uni_log_reg(admit_PRISM3,"`High Heart Rate (bpm)`")

plt_high_sbp_all1 <- create_scatter(admit_PRISM3,"High Systolic Blood Pressure (mmHg)","Outcome", "PRISM3 Patients")
plt_high_sbp_all2 <- create_histogram(admit_PRISM3, "High Systolic Blood Pressure (mmHg)", "PRISM 3 Patients")
test_high_sbp_all <- comparison_tests(admit_PRISM3,"High Systolic Blood Pressure (mmHg)")
univ_high_sbp_all <- uni_log_reg(admit_PRISM3,"`High Systolic Blood Pressure (mmHg)`")

plt_low_bpm_all1 <- create_scatter(admit_PRISM3,"Low Heart Rate (bpm)","Outcome", "PRISM3 Patients")
plt_low_bpm_all2 <- create_histogram(admit_PRISM3, "Low Heart Rate (bpm)", "PRISM 3 Patients")
test_low_bpm_all <- comparison_tests(admit_PRISM3,"Low Heart Rate (bpm)")
univ_low_bpm_all <- uni_log_reg(admit_PRISM3,"`Low Heart Rate (bpm)`")

plt_low_pao2_all1 <- create_scatter(admit_PRISM3,"Low PaO2 (mmHg)","Outcome", "PRISM3 Patients")
plt_low_pao2_all2 <- create_histogram(admit_PRISM3, "Low PaO2 (mmHg)", "PRISM 3 Patients")
test_low_pao2_all <- comparison_tests(admit_PRISM3,"Low PaO2 (mmHg)")
univ_low_pao2_all <- uni_log_reg(admit_PRISM3,"`Low PaO2 (mmHg)`")

plt_high_platelet_all1 <- create_scatter(admit_PRISM3,"High Platelet Count (10(9)/L)","Outcome", "PRISM3 Patients")
plt_high_platelet_all2 <- create_histogram(admit_PRISM3, "High Platelet Count (10(9)/L)", "PRISM 3 Patients")
test_high_platelet_all <- comparison_tests(admit_PRISM3,"High Platelet Count (10(9)/L)")
univ_high_platelet_all <- uni_log_reg(admit_PRISM3,"`High Platelet Count (10(9)/L)`")

plt_high_creat_all1 <- create_scatter(admit_PRISM3,"High Creatinine (mg/dL)","Outcome", "PRISM3 Patients")
plt_high_creat_all2 <- create_histogram(admit_PRISM3, "High Creatinine (mg/dL)", "PRISM 3 Patients")
test_high_creat_all <- comparison_tests(admit_PRISM3,"High Creatinine (mg/dL)")
univ_high_creat_all <- uni_log_reg(admit_PRISM3,"`High Creatinine (mg/dL)`")

plt_high_bun_all1 <- create_scatter(admit_PRISM3,"High Blood Urea Nitrogen (mg/dL)","Outcome", "PRISM3 Patients")
plt_high_bun_all2 <- create_histogram(admit_PRISM3, "High Blood Urea Nitrogen (mg/dL)", "PRISM 3 Patients")
test_high_bun_all <- comparison_tests(admit_PRISM3,"High Blood Urea Nitrogen (mg/dL)")
univ_high_bun_all <- uni_log_reg(admit_PRISM3,"`High Blood Urea Nitrogen (mg/dL)`")

#### Summary Statistics ####
plt_outcome_by_year <- create_bar_graph(admit, "Outcome", "Discharge Year")
tbl_outcome_by_year <- create_table(admit, "Discharge Year", "Outcome")

sum_plos1 <- create_summary(admit,"Physical Length of Stay (Days)", "Outcome")
sum_mlos1 <- create_summary(admit,"Medical Length of Stay (Days)", "Outcome")
sum_prism3los1 <- create_summary(admit_PRISM3,"PRISM 3 Length of Stay (Days)", "Outcome")
sum_prism3pod1 <- create_summary(admit_PRISM3,"PRISM 3 Probability of Death", "Outcome")
sum_ecmo_duration1 <- create_summary(proc_ecmo, "Procedure Duration (Constrained to ICU Stay Days)", "Outcome")

sum_plos2 <- sum_stats(admit,"Physical Length of Stay (Days)")
sum_mlos2 <- sum_stats(admit,"Medical Length of Stay (Days)")
sum_prism3los2 <- sum_stats(admit_PRISM3,"PRISM 3 Length of Stay (Days)")
sum_prism3pod2 <- sum_stats(admit_PRISM3,"PRISM 3 Probability of Death")
sum_ecmo_duration2 <- sum_stats(proc_ecmo, "Procedure Duration (Constrained to ICU Stay Days)")
