#### Model 1: All PIM3/PRISM III Variables ####

# Split data into training and testing sets with 75/25 split
set.seed(42)
sample_size <- floor(0.75 * nrow(model_1))
train <- sample(seq_len(nrow(model_1)), size = sample_size)

training1 <- model_1[train, ]
testing1 <- model_1[-train, ]

# Fit logistic regression using the training set
log_reg_model1 <- glm(Died ~ ., data = training1, family = binomial)

summary(log_reg_model1)

# Predict using the testing set
testing1$pred <- predict(log_reg_model1, newdata = testing1, type = "response")

# Evaluate model
roc_obj1 <- roc(testing1$Died, testing1$pred) 
                # percent = TRUE, plot = FALSE, auc.polygon = TRUE, max.auc.polygon = FALSE,
                # grid = TRUE, print.auc = FALSE, show.thres = TRUE, ci = TRUE)
auc_value1 <- auc(roc_obj1)

# print(paste("AUC:", auc_value1))
# plot(roc_obj1, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value1, 2), ")"))

hl_test1 <- hoslem.test(testing1$Died, testing1$pred, g = 10)
# print(hl_test1)

giviti_belt1 <- givitiCalibrationBelt(
  testing1$Died, ifelse(testing1$pred==1,0.99999,testing1$pred), devel="external"
)

#---------------------------------------

null_model <- glm(Died ~ 1, data = training1, family = binomial)
summary(null_model)

library(MASS)
log_reg_model2 <-  stepAIC(null_model, scope = list(lower = null_model, upper = log_reg_model1), direction = "forward")

summary(log_reg_model2)

test_probabilities_select <- predict(log_reg_model2, newdata = testing1, type = "response")

roc_obj_select <- roc(testing1$Died, test_probabilities_select)
auc_value <- auc(roc_obj_select)
print(paste("AUC:", auc_value))
plot(roc_obj_select, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value, 3), ")"))

testing1PRISMPOD <- testing1
testing1PRISMPOD <- admit_filter[, c("Died", "PRISM 3 Probability of Death")]

testing1PIMPOD <- testing1
testing1PIMPOD <- admit_filter[, c("Died", "PIM3 Probability of Death")]

PIM3_obj_select <- roc(testing1PIMPOD$Died, testing1PIMPOD$`PIM3 Probability of Death`)
auc_PIM3 <- auc(PIM3_obj_select)
print(paste("AUC:", auc_PIM3))

PRISM3_obj_select <- roc(testing1PRISMPOD$Died, testing1PRISMPOD$`PRISM 3 Probability of Death`)
auc_PRISM3 <- auc(PRISM3_obj_select)
print(paste("AUC:", auc_PRISM3))
plot(PRISM3_obj_select, col = "blue")
plot(roc_obj1, col = "red", add = TRUE)
plot(PIM3_obj_select, col = "black", add = TRUE)
plot(roc_obj_3, col = "orange", add = TRUE)
plot(roc_obj_select, col = "pink", add = TRUE)

model_1 <- model_1[complete.cases(model_1),]
nrow(model_1)

model_3 <- model_3[complete.cases(model_3),]
nrow(model_3)

set.seed(42)
sample_size <- floor(0.75 * nrow(model_3))
train <- sample(seq_len(nrow(model_3)), size = sample_size)

training3 <- model_3[train, ]
testing3 <- model_3[-train, ]

complete_cases <- complete.cases(training3)
training3complete <- training3[complete_cases, ]

log_reg_model3 <- glm(Died ~ ., data = training3, family = binomial)

summary(log_reg_model3)

test_probabilities_3 <- predict(log_reg_model3, newdata = testing3, type = "response")

roc_obj_3 <- roc(testing3$Died, test_probabilities_3)
auc_value <- auc(roc_obj_3)
print(paste("AUC:", auc_value))
plot(roc_obj_3, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value, 3), ")"))

null_model_3 <- training3complete$Died
null_model_3 <- glm(Died ~ 1, data = training3complete, family = binomial)
summary(null_model_3)



library(MASS)
log_reg_model4 <-  stepAIC(null_model_3, scope = list(lower = null_model_3, upper = log_reg_model3), direction = "forward")

summary(log_reg_model4)

test_probabilities_4 <- predict(log_reg_model3, newdata = testing3, type = "response")

roc_obj_4 <- roc(testingcomplete3$Died, test_probabilities_4)
auc_value <- auc(roc_obj_4)
print(paste("AUC:", auc_value))
plot(roc_obj_select, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value, 3), ")"))
