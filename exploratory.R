source("load_data.r")

source("create_table.r")
source("create_bar_graph.r")

create_bar_graph(admit, "Outcome", "Race")
create_table(admit, "Race", "Outcome")

create_bar_graph(admit, "Outcome", "Sex Assigned at Birth")
create_table(admit, "Sex Assigned at Birth", "Outcome")

create_bar_graph(admit, "Outcome", "Age at ICU Admission")
create_table(admit, "Age at ICU Admission", "Outcome")

create_bar_graph(admit, "Outcome", "Ethnicity")
create_table(admit, "Ethnicity", "Outcome")

create_bar_graph(admit, "Outcome", "has_ecmo_va")
create_table(admit, "has_ecmo_va", "Outcome")
create_bar_graph(admit_only_neonates, "Outcome", "has_ecmo_va")
create_table(admit_only_neonates, "has_ecmo_va", "Outcome")

create_bar_graph(admit, "Outcome", "has_ecmo_vv")
create_table(admit, "has_ecmo_vv", "Outcome")
create_bar_graph(admit_only_neonates, "Outcome", "has_ecmo_vv")
create_table(admit_only_neonates, "has_ecmo_vv", "Outcome")

create_bar_graph(admit, "Outcome", "has_cpr_within_60min")
create_table(admit, "has_cpr_within_60min", "Outcome")
create_bar_graph(admit_only_neonates, "Outcome", "has_cpr_within_60min")
create_table(admit_only_neonates, "has_cpr_within_60min", "Outcome")

source("create_scatter.r")
source("create_histogram.r")
source("comparison_tests.r")
source("uni_log_reg.r")

create_scatter(admit_excluding_neonates, "Weight (kg)", "Outcome", "Age > 29 Days")
create_histogram(admit_excluding_neonates, "Weight (kg)", "Non-Neonates")
comparison_tests(admit_excluding_neonates, "Weight (kg)")
uni_log_reg(admit_excluding_neonates,"`Weight (kg)`")

create_scatter(admit_only_neonates, "Weight (kg)", "Outcome", "Age < 29 Days")
create_histogram(admit_only_neonates, "Weight (kg)", "Neonates")
comparison_tests(admit_only_neonates, "Weight (kg)")
uni_log_reg(admit_only_neonates,"`Weight (kg)`")

create_scatter(admit, "Weight (kg)", "Outcome", "All Pediatric Patients")
create_histogram(admit, "Weight (kg)", "Patients")
comparison_tests(admit, "Weight (kg)")
uni_log_reg(admit,"`Weight (kg)`")

create_scatter(admit_PRISM3,"Low Systolic Blood Pressure (mmHg)","Outcome", "PRISM3 Patients")
create_histogram(admit_PRISM3, "Low Systolic Blood Pressure (mmHg)", "PRISM 3 Patients")
comparison_tests(admit_PRISM3,"Low Systolic Blood Pressure (mmHg)")
uni_log_reg(admit_PRISM3,"`Low Systolic Blood Pressure (mmHg)`")

create_scatter(admit_PRISM3,"High Heart Rate (bpm)","Outcome", "PRISM3 Patients")
create_histogram(admit_PRISM3, "High Heart Rate (bpm)", "PRISM3 Patients")
comparison_tests(admit_PRISM3, "High Heart Rate (bpm)")
uni_log_reg(admit_PRISM3,"`High Heart Rate (bpm)`")

create_scatter(admit_PRISM3,"High Systolic Blood Pressure (mmHg)","Outcome", "PRISM3 Patients")
create_histogram(admit_PRISM3, "High Systolic Blood Pressure (mmHg)", "PRISM 3 Patients")
comparison_tests(admit_PRISM3,"High Systolic Blood Pressure (mmHg)")
uni_log_reg(admit_PRISM3,"`High Systolic Blood Pressure (mmHg)`")

create_scatter(admit_PRISM3,"Low Heart Rate (bpm)","Outcome", "PRISM3 Patients")
create_histogram(admit_PRISM3, "Low Heart Rate (bpm)", "PRISM 3 Patients")
comparison_tests(admit_PRISM3,"Low Heart Rate (bpm)")
uni_log_reg(admit_PRISM3,"`Low Heart Rate (bpm)`")

create_scatter(admit_PRISM3,"Low PaO2 (mmHg)","Outcome", "PRISM3 Patients")
create_histogram(admit_PRISM3, "Low PaO2 (mmHg)", "PRISM 3 Patients")
comparison_tests(admit_PRISM3,"Low PaO2 (mmHg)")
uni_log_reg(admit_PRISM3,"`Low PaO2 (mmHg)`")

create_scatter(admit_PRISM3,"High Platelet Count (10(9)/L)","Outcome", "PRISM3 Patients")
create_histogram(admit_PRISM3, "High Platelet Count (10(9)/L)", "PRISM 3 Patients")
comparison_tests(admit_PRISM3,"High Platelet Count (10(9)/L)")
uni_log_reg(admit_PRISM3,"`High Platelet Count (10(9)/L)`")

create_scatter(admit_PRISM3,"High Creatinine (mg/dL)","Outcome", "PRISM3 Patients")
create_histogram(admit_PRISM3, "High Creatinine (mg/dL)", "PRISM 3 Patients")
comparison_tests(admit_PRISM3,"High Creatinine (mg/dL)")
uni_log_reg(admit_PRISM3,"`High Creatinine (mg/dL)`")

create_scatter(admit_PRISM3,"High Blood Urea Nitrogen (mg/dL)","Outcome", "PRISM3 Patients")
create_histogram(admit_PRISM3, "High Blood Urea Nitrogen (mg/dL)", "PRISM 3 Patients")
comparison_tests(admit_PRISM3,"High Blood Urea Nitrogen (mg/dL)")
uni_log_reg(admit_PRISM3,"`High Blood Urea Nitrogen (mg/dL)`")
