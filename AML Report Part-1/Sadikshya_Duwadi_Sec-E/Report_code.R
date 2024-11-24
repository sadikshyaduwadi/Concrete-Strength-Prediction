#==========================
# Load necessary libraries
#===========================
library(mice)  # for imputation using mice()
library(dplyr) # for data manipulation
library(ggplot2)
library(corrplot)
library(gridExtra)

#=======================
#loading datasets
#=======================

train<-read.csv("~/Desktop/R/AML Report Part-1/concrete_strength_train.csv")
test<-read.csv("~/Desktop/R/AML Report Part-1/concrete_strength_test.csv")

#===============================
# Viewing Structure of data set
#================================
str(train)
str(test)

#===================================================================
#To combine data sets and preserve information from which datasets 
#records are we need to create the artificial variable
#the variable will be called isTrain and will have value "train" 
#for training records and "test" for testing records
#===================================================================

train$isTrain<-"train"
test$isTrain<-"test"

#combine data sets for analysis 
comb<- rbind(train, test)
str(comb)


#=======================
#EDA
#=======================

# Dimensions of combined data set
print(paste("Combined dataset has", nrow(comb), "number of rows and", ncol(comb), "number of columns."))

#Summary of combined data set
summary(comb)


# Target Variable Identification
target_variable <- "Strength"
cat("Target Variable: ", target_variable, "\n\n")

#Identifying features with high standard deviation
summary_stats <- summary(comb[, -ncol(comb)])[, c(1,4,5)] 
high_std_dev<- names(summary_stats[summary_stats[,3] > 50, 1])
cat("Features with standard deviation > 50: ", paste(high_std_dev, collapse = ", "), "\n")

#Check for outliers in the target variable
boxplot(comb[, target_variable])


#PCA
#preparing the data
num_data <- comb[sapply(comb, is.numeric)]
comb_scaled <- scale(num_data)
comb_scaled <- na.omit(comb_scaled)

# Performing PCA
pca <- prcomp(comb_scaled, center = TRUE, scale. = TRUE)
# Viewing a summary of the PCA results
summary(pca)

#Extracting the first two principal component scores
pc1_scores <- pca$x[, 1]
pc2_scores <- pca$x[, 2]

#Creating data frame for plotting
pca_plot <- data.frame(PC1 = pc1_scores, PC2 = pc2_scores)

#Creating scatterplot
ggplot(pca_plot, aes(x = PC1, y = PC2)) +
  geom_point() +
  labs(x = "Principal Component 1", y = "Principal Component 2", 
       title = "PCA Scatterplot of First Two Components")


#=======================
#Data Visualization
#=======================

#Distribution of Cement Content
ggplot(comb, aes(x = Cement)) +
  geom_histogram(bins = 30, fill = "green", alpha = 0.5) +  
  facet_wrap(~isTrain) + 
  labs(title = "Distribution of Cement Content", x = "Cement content", y = "Frequency") +
  theme_bw() 

#Graph to view the density and strength
ggplot(comb, aes(x = Strength)) +
  geom_density(alpha = 0.5) +
  geom_histogram(aes(y = ..density..),fill = "red", color = "black", bins = 30, alpha = 0.5) +
  labs(title = "Density and Histogram Plot of Strength",
       x = "Strength",
       y = "Density/Frequency")

#Cement vs. Strength (Scatter Plot)
ggplot(comb, aes(x = Cement, y = Strength, color = Age)) +
  geom_point(alpha = 0.5,color="blue") +
  scale_color_gradient(name = "Age (days)", low = min(comb$Age), high = max(comb$Age)) +
  labs(title = "Cement vs. Strength", x = "Cement content", y = "Compressive Strength") +
  theme_bw() 



#=======================
#Data pre-processing
#=======================

# Calculate the percentage of missing values in each column and overall
col_missing_percentage <- sapply(comb, function(x) mean(is.na(x))) * 100
overall_missingnes <- mean(col_missing_percentage)
print(col_missing_percentage)
print(paste("Overall missingness:", overall_missingnes, "%"))


#=======================
#Data imputation
#=======================

# Impute missing values using mice()
if (overall_missingnes > 1) {
  imputed_data <- mice(comb)
  final_imputed_data <- complete(imputed_data)
  print(summary(final_imputed_data))
} else {
  cleaned_data <- na.omit(comb)
  print(summary(cleaned_data))
  
}

imputed_data %>%
  is.na() %>%
  colSums()
md.pattern(final_imputed_data, rotate.names = TRUE)

#===============
#Outliers
#===============

# Creating a boxplot for each numeric variable
boxplot_list <- lapply(final_imputed_data[, sapply(final_imputed_data, is.numeric)], boxplot.stats)

# Identify outliers based on boxplot statistics
outliers <- lapply(boxplot_list, function(x) x$out)

# Print the outliers for each variable
for (i in seq_along(outliers)) {
  if (length(outliers[[i]]) > 0) {
    cat("Outliers in", names(outliers)[i], ":\n")
    print(outliers[[i]])
  }
}
boxplot(outliers)

# Removing the outliers
final_imputed_data_without_outliers <- final_imputed_data
for (col in names(final_imputed_data)) {
  if (is.numeric(final_imputed_data[[col]])) {
    q1 <- quantile(final_imputed_data[[col]], 0.25, na.rm = TRUE)
    q3 <- quantile(final_imputed_data[[col]], 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr
    
    final_imputed_data_without_outliers[[col]][final_imputed_data[[col]] < lower_bound | final_imputed_data[[col]] > upper_bound] <- NA
  }
}

#Visualizations to compare before and after outlier handling
for (col in names(final_imputed_data)) {
  if (is.numeric(final_imputed_data[[col]])) {
    # Plot before outlier handling
    p1 <- ggplot(final_imputed_data, aes_string(x = col)) +
      geom_boxplot() + 
      labs(title = paste0("Boxplot of ", col, " (Before)"))
    
    #Plot after outlier handling
    p2 <- ggplot(final_imputed_data_without_outliers, aes_string(x = col)) +
      geom_boxplot() + 
      labs(title = paste0("Boxplot of ", col, " (After)"))
    
    #Arrange plots side-by-side
    grid.arrange(p1, p2, ncol = 2)
  }
}


#=======================
#Multicollinearity 
#=======================

# Calculate correlation matrix
cor_matrix <- cor((final_imputed_data_without_outliers[, sapply(final_imputed_data_without_outliers, is.numeric)]), use = "complete.obs")
print(cor_matrix)

# Visualizing correlation matrix
corrplot(cor_matrix, method = "circle")

ggplot(final_imputed_data_without_outliers, aes(Strength, Cement)) +
  geom_jitter(aes(col = Superplasticizer, size = Age)) + 
  labs(title = "Highest Correlated Variables on Strength", x= "Strength", y= 'Cement')+
  theme(plot.title = element_text(hjust = 0.5))

#Investigating Variable
# Calculating variance for each numeric variable
variances <- sapply(final_imputed_data_without_outliers, var, na.rm = TRUE)
low_variance_vars <- names(variances[variances < 0.01])
# Removing low variance variables
final_imputed_data_without_outliers <- final_imputed_data_without_outliers[, !names
                                      (final_imputed_data_without_outliers) %in% 
                                      low_variance_vars]
str(final_imputed_data_without_outliers)
summary(final_imputed_data_without_outliers)


#===============
#Scaling
#===============
# Identifying numeric columns
num_data <- final_imputed_data_without_outliers[sapply(final_imputed_data_without_outliers, is.numeric)]

# Scaling numeric columns
scaled_data <- scale(num_data)
final_scaled_data <- as.data.frame(scaled_data)

variable_name <- "Strength"

# Original data
p1 <- ggplot(data = num_data, aes_string(x = variable_name)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  ggtitle(paste("Original", variable_name))


# Scaled data
p2 <- ggplot(data = final_scaled_data, aes_string(x = variable_name)) +
  geom_histogram(binwidth = 0.1, fill = "red", color = "black") +
  ggtitle(paste("Scaled", variable_name))
grid.arrange(p1, p2, ncol = 2)

#===============================================================================
PART-2
#===============================================================================

library(caret)
# Extract non-numeric columns
dataset <- final_imputed_data_without_outliers[, !sapply(final_imputed_data_without_outliers, is.numeric)]

# Combine scaled numeric data with non-numeric data
final_data <- cbind(num_data, dataset)


#Splitting teh dataset

final_train<-final_data[final_data$dataset=="train",]
final_test<-final_data[final_data$dataset=="test",]


#remove variable dataset both train and test
final_train$dataset<-NULL
final_test$dataset<-NULL

View(final_train)
View(final_test)


str(final_train)
str(final_test)

#checking for data missing in both train and test
if (anyNA(strength_train)) {
  print("There are NA values in the dataset.")
} else {
  print("There are no NA values in the dataset.")
}


# Assuming that 'Strength' is the dependent variable and all other variables are the independent variables
formula <- Strength ~ Cement + Blast.Furnace.Slag + Fly.Ash + Water + Superplasticizer + Coarse.Aggregate + Fine.Aggregate + Age




#************************************#
#*Linear Regression Model
#************************************#


# Train the model
model <- train(formula, data = final_train, method = "lm")

# Predict on the test set
predictions <- predict(model, newdata = final_test)

# Calculate R2
r2 <- R2(predictions, final_test$Strength)

# Calculate Adjusted R2
n <- nrow(final_test)
p <- length(model$finalModel$coefficients) - 1
adj_r2 <- 1 - (1-r2)*(n-1)/(n-p-1)

# Calculate MSE
mse <- mean((predictions - final_test$Strength)^2)

# Calculate RMSE
rmse <- sqrt(mse)

# Calculate MAE
mae <- mean(abs(predictions - final_test$Strength))

# Print the metrics
print(paste("R2: ", r2))
print(paste("Adjusted R2: ", adj_r2))
print(paste("MSE: ", mse))
print(paste("RMSE: ", rmse))
print(paste("MAE: ", mae))


# Create a data frame with the actual and predicted values
comparison <- data.frame(Actual = final_test$Strength, Predicted = predictions)

# Create a scatter plot
ggplot(comparison, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, color = "red") +
  labs(title = "Actual vs Predicted", x = "Actual", y = "Predicted") +
  theme_minimal()




#************************************#
#*Random Forest Model
#************************************#

# Train the model
model <- randomForest(formula, data = final_train)

# Print the model summary
print(summary(model))

# Predict on the test set
predictions <- predict(model, newdata = final_test)

# Calculate R2
r2 <- R2(predictions, final_test$Strength)

# Calculate Adjusted R2
n <- nrow(final_test)
p <- length(model$finalModel$coefficients) - 1
adj_r2 <- 1 - (1-r2)*(n-1)/(n-p-1)

# Calculate MSE
mse <- mean((predictions - final_test$Strength)^2)

# Calculate RMSE
rmse <- sqrt(mse)

# Calculate MAE
mae <- mean(abs(predictions - final_test$Strength))

# Print the metrics
print(paste("R2: ", r2))
print(paste("Adjusted R2: ", adj_r2))
print(paste("MSE: ", mse))
print(paste("RMSE: ", rmse))
print(paste("MAE: ", mae))



# Create a data frame with the actual and predicted values
comparison <- data.frame(Actual = final_test$Strength, Predicted = predictions)

# Create a scatter plot
ggplot(comparison, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, color = "red") +
  labs(title = "Actual vs Predicted", x = "Actual", y = "Predicted") +
  theme_minimal()



#************************************#
#*Decision Tree Model
#************************************#


# Train the model
model <- rpart(formula, data = final_train, method = "anova")

# Print the model summary
print(summary(model))

# Predict on the test set
predictions <- predict(model, newdata = final_test)

# Calculate R2
r2 <- R2(predictions, final_test$Strength)

# Calculate Adjusted R2
n <- nrow(final_test)
p <- length(model$finalModel$coefficients) - 1
adj_r2 <- 1 - (1-r2)*(n-1)/(n-p-1)

# Calculate MSE
mse <- mean((predictions - final_test$Strength)^2)

# Calculate RMSE
rmse <- sqrt(mse)

# Calculate MAE
mae <- mean(abs(predictions - final_test$Strength))

# Print the metrics
print(paste("R2: ", r2))
print(paste("Adjusted R2: ", adj_r2))
print(paste("MSE: ", mse))
print(paste("RMSE: ", rmse))
print(paste("MAE: ", mae))


# Create a data frame with the actual and predicted values
comparison <- data.frame(Actual = final_test$Strength, Predicted = predictions)

# Create a scatter plot with different colors for actual and predicted values
ggplot(comparison, aes(x = Actual, y = Predicted)) +  # Initialize the plot with data
  geom_point() +  # Add a layer of points
  geom_smooth(method = lm, se = FALSE, color = "red") +  # Add a layer of a linear regression line
  labs(title = "Actual vs Predicted", x = "Actual", y = "Predicted") +  # Add labels
  theme_minimal()



#************************************#
#*KNN Model
#************************************#


# Define training control
train_control <- trainControl(method = "cv", number = 10)


model <- train(formula, data = final_train, method = "knn", trControl = train_control)

# Print the model summary
print(summary(model))

# Predict on the test set
predictions <- predict(model, newdata = final_test)

# Calculate R2
r2 <- R2(predictions, final_test$Strength)

# Calculate Adjusted R2
n <- nrow(final_test)
p <- length(model$finalModel$coefficients) - 1
adj_r2 <- 1 - (1-r2)*(n-1)/(n-p-1)

# Calculate MSE
mse <- mean((predictions - final_test$Strength)^2)

# Calculate RMSE
rmse <- sqrt(mse)

# Calculate MAE
mae <- mean(abs(predictions - final_test$Strength))

# Print the metrics
print(paste("R2: ", r2))
print(paste("Adjusted R2: ", adj_r2))
print(paste("MSE: ", mse))
print(paste("RMSE: ", rmse))
print(paste("MAE: ", mae))

# Create a data frame with the actual and predicted values
comparison <- data.frame(Actual = final_test$Strength, Predicted = predictions)

# Create a scatter plot
ggplot(comparison, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, color = "red") +
  labs(title = "Actual vs Predicted", x = "Actual Strength", y = "Predicted Strength") +
  theme_minimal()






#************************************#
#*KNN Model with optimal value of k = 4
#************************************#



# Define training control
train_control <- trainControl(method = "cv", number = 10)

# Train the model with a different value of k
tuneGrid <- expand.grid(.k = 4)
model <- train(formula, data = final_train, method = "knn", trControl = train_control, tuneGrid = tuneGrid)

# Print the model summary
print(summary(model))

# Predict on the test set
predictions <- predict(model, newdata = final_test)

# Calculate R2
r2 <- R2(predictions, final_test$Strength)

# Calculate Adjusted R2
n <- nrow(final_test)
p <- length(model$finalModel$coefficients) - 1
adj_r2 <- 1 - (1-r2)*(n-1)/(n-p-1)

# Calculate MSE
mse <- mean((predictions - final_test$Strength)^2)

# Calculate RMSE
rmse <- sqrt(mse)

# Calculate MAE
mae <- mean(abs(predictions - final_test$Strength))

# Print the metrics
print(paste("R2: ", r2))
print(paste("Adjusted R2: ", adj_r2))
print(paste("MSE: ", mse))
print(paste("RMSE: ", rmse))
print(paste("MAE: ", mae))

# Create a data frame with the actual and predicted values
comparison <- data.frame(Actual = final_test$Strength, Predicted = predictions)

# Create a scatter plot
ggplot(comparison, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, color = "red") +
  labs(title = "Actual vs Predicted", x = "Actual Strength", y = "Predicted Strength") +
  theme_minimal()


