#===============================================================================
#PART-2
#===============================================================================

#==========================
# Load necessary libraries
#===========================
library(mice)  
library(dplyr) 
library(ggplot2)
library(corrplot)
library(gridExtra)
library(caret)
library(rattle)
library(randomForest)
library(rpart)
library(ggplot2)
library(patchwork)
#=======================
#loading datasets
#=======================

train<-read.csv("~/Desktop/R/AML Report Part-2/concrete_strength_train.csv")
test<-read.csv("~/Desktop/R/AML Report Part-2/concrete_strength_test.csv")


# Extracting non-numeric columns
dataset <- final_imputed_data_without_outliers[, !sapply(final_imputed_data_without_outliers, is.numeric)]
str(final_imputed_data_without_outliers)

# Check the structure of num_data and dataset
str(num_data)
str(dataset)

# Combining num_data and dataset by column
final_data <- cbind(num_data, isTrain = dataset)

# Check the structure of final_data
str(final_data)

#Splitting the dataset
strength_train<-final_data[final_data$isTrain=="train",]
strength_test<-final_data[final_data$isTrain=="test", ]

#removing variable isTrain from both train and test
strength_train$isTrain<-NULL
strength_test$isTrain<-NULL

View(strength_train)
View(strength_test)
str(strength_train)
str(strength_test)

# Assuming Strength as the dependent variable and other as the independent variables
formula <- Strength ~ Cement + Blast.Furnace.Slag + Fly.Ash + Water + Superplasticizer + Coarse.Aggregate + Fine.Aggregate + Age



#=======================
#Linear Regression Model
#=======================

# Training the model
model <- train(formula, data = strength_train, method = "lm")
summary(model)

# Predicting on the test set
predictions <- predict(model, newdata = strength_test)
predictions

# Calculating R2
r2 <- R2(predictions, strength_test$Strength)

# Calculating Adjusted R2
n <- nrow(strength_test)
p <- length(model$finalModel$coefficients) - 1
adj_r2 <- 1 - (1-r2)*(n-1)/(n-p-1)

# Calculating MSE
mse <- mean((predictions - strength_test$Strength)^2)

# Calculating RMSE
rmse <- sqrt(mse)

# Calculating MAE
mae <- mean(abs(predictions - strength_test$Strength))

# Printing the metrics
print(paste("R2: ", r2))
print(paste("Adjusted R2: ", adj_r2))
print(paste("MSE: ", mse))
print(paste("RMSE: ", rmse))
print(paste("MAE: ", mae))


# Creating a data frame with the actual and predicted values
comparison_lr <- data.frame(Actual = strength_test$Strength, Predicted = predictions)

# Creating a scatter plot
ggplot(comparison_lr, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, color = "blue") +
  labs(title = "Actual vs Predicted", x = "Actual", y = "Predicted") +
  theme_minimal()



#=======================
#Random Forest Model
#=======================

# Training the model
model <- randomForest(formula, data = strength_train)
plot(model)
# Printing the model summary
print(summary(model))

# Predicting on the test set
predictions <- predict(model, newdata = strength_test)

# Calculating R2
r2 <- R2(predictions, strength_test$Strength)

# Calculating Adjusted R2
n <- nrow(strength_test)
p <- length(model$finalModel$coefficients) - 1
adj_r2 <- 1 - (1-r2)*(n-1)/(n-p-1)

# Calculating MSE
mse <- mean((predictions - strength_test$Strength)^2)

# Calculating RMSE
rmse <- sqrt(mse)

# Calculating MAE
mae <- mean(abs(predictions - strength_test$Strength))

# Printing the metrics
print(paste("R2: ", r2))
print(paste("Adjusted R2: ", adj_r2))
print(paste("MSE: ", mse))
print(paste("RMSE: ", rmse))
print(paste("MAE: ", mae))



# Creating a data frame with the actual and predicted values
comparison_rf <- data.frame(Actual = strength_test$Strength, Predicted = predictions)

# Creating a scatter plot
ggplot(comparison_rf, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, color = "blue") +
  labs(title = "Actual vs Predicted", x = "Actual", y = "Predicted") +
  theme_minimal()


#=======================
#Decision Tree Model
#=======================

# Training the model
model <- rpart(formula, data = strength_train, method = "anova")

# Printing the model summary
print(summary(model))

# Plot the decision tree
fancyRpartPlot(model, sub = '')


# Predicting on the test set
predictions <- predict(model, newdata = strength_test)
predictions
# Calculating R2
r2 <- R2(predictions, strength_test$Strength)


# Calculating Adjusted R2
n <- nrow(strength_test)
p <- length(model$finalModel$coefficients) - 1
adj_r2 <- 1 - (1-r2)*(n-1)/(n-p-1)

# Calculating MSE
mse <- mean((predictions - strength_test$Strength)^2)

# Calculating RMSE
rmse <- sqrt(mse)

# Calculating MAE
mae <- mean(abs(predictions - strength_test$Strength))

# Printing the metrics
print(paste("R2: ", r2))
print(paste("Adjusted R2: ", adj_r2))
print(paste("MSE: ", mse))
print(paste("RMSE: ", rmse))
print(paste("MAE: ", mae))


# Creating a data frame with the actual and predicted values
comparison_dt <- data.frame(Actual = strength_test$Strength, Predicted = predictions)

# Creating a scatter plot 
ggplot(comparison_dt, aes(x = Actual, y = Predicted)) +  # Initialize the plot with data
  geom_point() +  # Add a layer of points
  geom_smooth(method = lm, se = FALSE, color = "blue") +  # Add a layer of a linear regression line
  labs(title = "Actual vs Predicted", x = "Actual", y = "Predicted") +  # Add labels
  theme_minimal()


#=======================
#KNN Model
#=======================

# Defining training control
train_control <- trainControl(method = "cv", number = 10)
model <- train(formula, data = strength_train, method = "knn", trControl = train_control)

# Printing the model summary
print(summary(model))

# Predicting on the test set
predictions <- predict(model, newdata = strength_test)
predictions

# Calculating R2
r2 <- R2(predictions, strength_test$Strength)

# Calculating Adjusted R2
n <- nrow(strength_test)
p <- length(model$finalModel$coefficients) - 1
adj_r2 <- 1 - (1-r2)*(n-1)/(n-p-1)

# Calculating MSE
mse <- mean((predictions - strength_test$Strength)^2)

# Calculating RMSE
rmse <- sqrt(mse)

# Calculating MAE
mae <- mean(abs(predictions - strength_test$Strength))

# Printing the metrics
print(paste("R2: ", r2))
print(paste("Adjusted R2: ", adj_r2))
print(paste("MSE: ", mse))
print(paste("RMSE: ", rmse))
print(paste("MAE: ", mae))

# Creating a data frame with the actual and predicted values
comparison_knn <- data.frame(Actual = strength_test$Strength, Predicted = predictions)

# Creating a scatter plot
ggplot(comparison_knn, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, color = "green") +
  labs(title = "Actual vs Predicted", x = "Actual Strength", y = "Predicted Strength") +
  theme_minimal()





#======================================
#KNN Model with optimal value of k = 4
#======================================

# Defining training control
train_control <- trainControl(method = "cv", number = 10)

# Training the model with a different value of k
tuneGrid <- expand.grid(.k = 4)
model <- train(formula, data = strength_train, method = "knn", trControl = train_control, tuneGrid = tuneGrid)

# Printing the model summary
print(summary(model))

# Predicting on the test set
predictions <- predict(model, newdata = strength_test)

# Calculating R2
r2 <- R2(predictions, strength_test$Strength)

# Calculating Adjusted R2
n <- nrow(strength_test)
p <- length(model$finalModel$coefficients) - 1
adj_r2 <- 1 - (1-r2)*(n-1)/(n-p-1)

# Calculating MSE
mse <- mean((predictions - strength_test$Strength)^2)

# Calculating RMSE
rmse <- sqrt(mse)

# Calculating MAE
mae <- mean(abs(predictions - strength_test$Strength))

# Printing the metrics
print(paste("R2: ", r2))
print(paste("Adjusted R2: ", adj_r2))
print(paste("MSE: ", mse))
print(paste("RMSE: ", rmse))
print(paste("MAE: ", mae))

# Creating a data frame with the actual and predicted values
comparison <- data.frame(Actual = strength_test$Strength, Predicted = predictions)

# Creating a scatter plot
ggplot(comparison, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, color = "red") +
  labs(title = "Actual vs Predicted", x = "Actual Strength", y = "Predicted Strength") +
  theme_minimal()

#--------------------
#Plotting all module
#-----------------------
par(mfrow = c(2,6))

plot1<-ggplot(comparison_lr, aes(x = Actual, y = Predicted))+labs(title = "Linear Regression") + geom_point()

plot2<-ggplot(comparison_rf, aes(x = Actual, y = Predicted))+labs(title = "Random Forest") + geom_point()

plot3<-ggplot(comparison_dt, aes(x = Actual, y = Predicted))+labs(title = "Decision Tree") + geom_point()

plot4<-ggplot(comparison_knn, aes(x = Actual, y = Predicted))+labs(title = "KNN") + geom_point()

plot5<-ggplot(comparison, aes(x = Actual, y = Predicted)) +labs(title = "KNN with different values") + geom_point()


# Combining the plots 
combined_plot <- wrap_plots(
  plot1, plot2, plot3, plot4, plot5,
  nrow = 3,  # Set the number of rows
  ncol = 2,  # Set the number of columns
  guides = "collect"  # Optional: Control how legends are collected
)

# Displaying the combined plot
combined_plot



# Defining a function to calculate evaluation metrics
calculate_metrics <- function(actual, predicted) {
  r_squared <- R2(predicted, actual)
  n <- length(actual)
  p <- 1  # Assuming one predictor in all models
  adj_r_squared <- 1 - (1 - r_squared) * (n - 1) / (n - p - 1)
  mse <- mean((predicted - actual)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(predicted - actual))
  return(list(R2 = r_squared, Adj_R2 = adj_r_squared, MSE = mse, RMSE = rmse, MAE = mae))
}

# Calculating evaluation metrics for each model
metrics_lr <- calculate_metrics(comparison_lr$Actual, comparison_lr$Predicted)
metrics_rf <- calculate_metrics(comparison_rf$Actual, comparison_rf$Predicted)
metrics_dt <- calculate_metrics(comparison_dt$Actual, comparison_dt$Predicted)
metrics_knn <- calculate_metrics(comparison_knn$Actual, comparison_knn$Predicted)
metrics_knn_4 <- calculate_metrics(comparison$Actual, comparison$Predicted)

# Combinuing metrics into a data frame for comparison
metrics <- data.frame(Model = c("Linear Regression", "Random Forest", "Decision Tree", "KNN", "KNN (k=4)"),
                      R2 = c(metrics_lr$R2, metrics_rf$R2, metrics_dt$R2, metrics_knn$R2, metrics_knn_4$R2),
                      Adj_R2 = c(metrics_lr$Adj_R2, metrics_rf$Adj_R2, metrics_dt$Adj_R2, metrics_knn$Adj_R2, metrics_knn_4$Adj_R2),
                      MSE = c(metrics_lr$MSE, metrics_rf$MSE, metrics_dt$MSE, metrics_knn$MSE, metrics_knn_4$MSE),
                      RMSE = c(metrics_lr$RMSE, metrics_rf$RMSE, metrics_dt$RMSE, metrics_knn$RMSE, metrics_knn_4$RMSE),
                      MAE = c(metrics_lr$MAE, metrics_rf$MAE, metrics_dt$MAE, metrics_knn$MAE, metrics_knn_4$MAE))

# Printing the metrics
print(metrics)

# Selecting the best model based on the evaluation metrics
best_model <- metrics[which.min(metrics$RMSE), ]
print("Best Predicting Model based on RMSE:")
print(best_model)


# Training the Random Forest model
rf_model <- randomForest(formula, data = strength_train)

# Extracting feature importance
importance <- importance(rf_model)
importance

# Ploting feature importance
varImpPlot(rf_model)
