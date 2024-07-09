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
roc_obj1 <- roc(testing1$Died, testing1$pred, 
                percent = TRUE, plot = FALSE, auc.polygon = TRUE, max.auc.polygon = FALSE,
                grid = TRUE, print.auc = FALSE, show.thres = TRUE, ci = TRUE)
auc_value1 <- auc(roc_obj1)

# print(paste("AUC:", auc_value1))
# plot(roc_obj1, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value1, 2), ")"))

hl_test1 <- hoslem.test(testing1$Died, testing1$pred, g = 10)
# print(hl_test1)

giviti_belt1 <- givitiCalibrationBelt(
  testing1$Died, ifelse(testing1$pred==1,0.99999,testing1$pred), devel="external"
)
