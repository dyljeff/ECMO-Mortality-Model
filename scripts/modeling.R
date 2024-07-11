################################################################################
#### Model 1a: All PIM3/PRISM III Variables ####
################################################################################

# Split data into training and testing sets with 75/25 split
set.seed(42)
sample_size <- floor(0.75 * nrow(model_1))
train <- sample(seq_len(nrow(model_1)), size = sample_size)

training_1 <- model_1[train, ]
testing_1 <- model_1[-train, ]

# Fit logistic regression using the training set
log_reg_model_1a <- glm(Died ~ ., data = training_1[, col_model_1], family = binomial)
summary(log_reg_model_1a)

# Predict using the testing set
testing_1$pred_model_1a <- predict(log_reg_model_1a, newdata = testing_1, type = "response")

# Evaluate model
roc_obj_1a <- roc(
  testing_1$Died, testing_1$pred_model_1a, 
  percent = TRUE, auc.polygon = TRUE, max.auc.polygon = FALSE, grid = TRUE, 
  print.auc = FALSE, show.thres = TRUE, ci = TRUE
)
auc_value_1a <- auc(roc_obj_1a)
# print(paste("AUC:", auc_value_1a))
# plot(roc_obj_1a, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value_1a, 2), ")"))

hl_test_1a <- hoslem.test(testing_1$Died, testing_1$pred_model_1a, g = 10)
# print(hl_test_1a)

giviti_belt_1a <- givitiCalibrationBelt(
  testing_1$Died,
  ifelse(testing_1$pred_model_1a == 1, 0.99999, testing_1$pred_model_1a),
  devel = "external"
)


################################################################################
#### Model 1b: Forward Selection by AIC - All PIM3/PRISM III Variables ####
################################################################################

null_model_1 <- glm(Died ~ 1, data = training_1, family = binomial)
summary(null_model_1)

log_reg_model_1b <- stepAIC(null_model_1, scope = list(lower = null_model_1, upper = log_reg_model_1a), direction = "forward")
summary(log_reg_model_1b)

testing_1$pred_model_1b <- predict(log_reg_model_1b, newdata = testing_1, type = "response")

roc_obj_1b <- roc(
  testing_1$Died, testing_1$pred_model_1b, 
  percent = TRUE, auc.polygon = TRUE, max.auc.polygon = FALSE, grid = TRUE, 
  print.auc = FALSE, show.thres = TRUE, ci = TRUE
)
auc_value_1b <- auc(roc_obj_1b)
# print(paste("AUC:", auc_value_1b))
# plot(roc_obj_1b, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value_1b, 3), ")"))

hl_test_1b <- hoslem.test(testing_1$Died, testing_1$pred_model_1b, g = 10)
# print(hl_test_1b)

giviti_belt_1b <- givitiCalibrationBelt(
  testing_1$Died,
  ifelse(testing_1$pred_model_1b == 1, 0.99999, testing_1$pred_model_1b),
  devel = "external"
)


################################################################################
#### Model 2a: Exploratory Analysis Variables ####
################################################################################

set.seed(42)
sample_size <- floor(0.75 * nrow(model_2))
train <- sample(seq_len(nrow(model_2)), size = sample_size)

training_2 <- model_2[train, ]
testing_2 <- model_2[-train, ]

log_reg_model_2a <- glm(Died ~ ., data = training_2[, col_model_2], family = binomial)
summary(log_reg_model_2a)

testing_2$pred_model_2a <- predict(log_reg_model_2a, newdata = testing_2, type = "response")

roc_obj_2a <- roc(
  testing_2$Died, testing_2$pred_model_2a, 
  percent = TRUE, auc.polygon = TRUE, max.auc.polygon = FALSE, grid = TRUE, 
  print.auc = FALSE, show.thres = TRUE, ci = TRUE
)
auc_value_2a <- auc(roc_obj_2a)
# print(paste("AUC:", auc_value_2a))
# plot(roc_obj_2a, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value_2a, 3), ")"))

hl_test_2a <- hoslem.test(testing_2$Died, testing_2$pred_model_2a, g = 10)
# print(hl_test_2a)

giviti_belt_2a <- givitiCalibrationBelt(
  testing_2$Died,
  ifelse(testing_2$pred_model_2a == 1, 0.99999, testing_2$pred_model_2a),
  devel = "external"
)


################################################################################
#### Model 2b: Forward Selection by AIC - Exploratory Analysis Variables ####
################################################################################

null_model_2 <- glm(Died ~ 1, data = training_2, family = binomial)
summary(null_model_2)

log_reg_model_2b <-  stepAIC(null_model_2, scope = list(lower = null_model_2, upper = log_reg_model_2a), direction = "forward")
summary(log_reg_model_2b)

testing_2$pred_model_2b <- predict(log_reg_model_2b, newdata = testing_2, type = "response")

roc_obj_2b <- roc(
  testing_2$Died, testing_2$pred_model_2b, 
  percent = TRUE, auc.polygon = TRUE, max.auc.polygon = FALSE, grid = TRUE, 
  print.auc = FALSE, show.thres = TRUE, ci = TRUE
)
auc_value_2b <- auc(roc_obj_2b)
# print(paste("AUC:", auc_value_2b))
# plot(roc_obj_2b, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value_2b, 3), ")"))

hl_test_2b <- hoslem.test(testing_2$Died, testing_2$pred_model_2b, g = 10)
# print(hl_test_2b)

giviti_belt_2b <- givitiCalibrationBelt(
  testing_2$Died,
  ifelse(testing_2$pred_model_2b == 1, 0.99999, testing_2$pred_model_2b),
  devel = "external"
)


################################################################################
#### ROC Curve Comparison ####
################################################################################

df_1 <- merge(
  testing_1[, c("Case Index Id", "Died")], 
  admit_PRISM3[, c("Case Index Id", "PIM3 Probability of Death", "PRISM 3 Probability of Death")],
  all.x = TRUE, all.y = FALSE
)

roc_obj_pim3_1 <- roc(
  df_1$Died, df_1$`PIM3 Probability of Death`, 
  percent = TRUE, auc.polygon = TRUE, max.auc.polygon = FALSE, grid = TRUE, 
  print.auc = FALSE, show.thres = TRUE, ci = TRUE
)
auc_value_pim3_1 <- auc(roc_obj_pim3_1)

roc_obj_prism3_1 <- roc(
  df_1$Died, df_1$`PRISM 3 Probability of Death`/100, 
  percent = TRUE, auc.polygon = TRUE, max.auc.polygon = FALSE, grid = TRUE, 
  print.auc = FALSE, show.thres = TRUE, ci = TRUE
)
auc_value_prism3_1 <- auc(roc_obj_prism3_1)

# plot(roc_obj_pim3_1, col = "black")
# plot(roc_obj_prism3_1, col = "blue", add = TRUE)
# plot(roc_obj_1a, col = "red", add = TRUE)
# plot(roc_obj_1b, col = "orange", add = TRUE)

df_2 <- merge(
  testing_2[, c("Case Index Id", "Died")], 
  admit_PRISM3[, c("Case Index Id", "PIM3 Probability of Death", "PRISM 3 Probability of Death")],
  all.x = TRUE, all.y = FALSE
)

roc_obj_pim3_2 <- roc(
  df_2$Died, df_2$`PIM3 Probability of Death`,
  percent = TRUE, auc.polygon = TRUE, max.auc.polygon = FALSE, grid = TRUE, 
  print.auc = FALSE, show.thres = TRUE, ci = TRUE
)
auc_value_pim3_2 <- auc(roc_obj_pim3_2)

roc_obj_prism3_2 <- roc(
  df_2$Died, df_2$`PRISM 3 Probability of Death`/100,
  percent = TRUE, auc.polygon = TRUE, max.auc.polygon = FALSE, grid = TRUE, 
  print.auc = FALSE, show.thres = TRUE, ci = TRUE
)
auc_value_prism3_2 <- auc(roc_obj_prism3_2)

# plot(roc_obj_pim3_2, col = "black")
# plot(roc_obj_prism3_2, col = "blue", add = TRUE)
# plot(roc_obj_2a, col = "red", add = TRUE)
# plot(roc_obj_2b, col = "orange", add = TRUE)
