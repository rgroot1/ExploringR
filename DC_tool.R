# __________Caret______________
# Fit lm model on train: model
model <- lm(price ~ ., train)

# Predict on test: p
p <- predict(model, test)

# 10-fold cross-validation
# Fit lm model using 10-fold CV: model
model <- train(
  y~., diamonds,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)

# Print model to console
print(model)

# Fit a logistic regression model
# Fit glm model: model
model <- glm(Class ~ ., family = "binomial", train)

# Predict on test: p
p <- predict(model, test, type = "response")

# Calculate a confusion matrix
# Calculate class probabilities: p_class
p_class <- ifelse(p > 0.50, "M", "R")

# Create confusion matrix
confusionMatrix(p_class, test[["Class"]])

# _____________Probabilities and classes____________
# Predicted classes are based off of predicted probabilities plus a classification threshold.
# classification threshold does not always align with the goals for a given modeling problem
# Predict on test: p
p <- predict(model, test, type = "response")

# Make ROC curve
colAUC(p, test[["Class"]], plotROC = TRUE)

# Create trainControl object: myControl
myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

# Train glm with custom trainControl: model
model <- train(Class ~ ., Sonar, method = "glm",
               trControl = myControl)



