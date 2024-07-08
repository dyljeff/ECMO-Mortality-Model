source("filtered_df.R")

set.seed(42)
sample_size <- floor(0.75 * nrow(model_1))
train <- sample(seq_len(nrow(model_1)), size = sample_size)
training <- model_1[train, ]
testing <- model_1[-train, ]

log_reg_model <- glm(Died ~ ., data = training, family = binomial)

summary(log_reg_model)

library("pROC")
test_probabilities <- predict(log_reg_model, newdata = testing, type = "response")

roc_obj <- roc(testing$Died, test_probabilities)
auc_value <- auc(roc_obj)

print(paste("AUC:", auc_value))
plot(roc_obj, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"))

# ---------------------
library(ResourceSelection)

hl_test <- hoslem.test(testing$Died, test_probabilities, g = 10)

# Print the test results
print(hl_test)
