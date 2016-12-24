# CREDIT RISK MODELLING IN R 
# By: Mendelsohn Chan 
# --------------------------------------------------------------------------------
# View the structure of loan_data 
str(loan_data)

# Load the gmodels package 
install.packages("gmodels") 
library(gmodels)

# Call CrossTable() on loan_status
CrossTable(loan_data$loan_status)

# Call CrossTable() on grade and loan_status
CrossTable(loan_data$grade, loan_data$loan_status, prop.r = TRUE, 
           prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

# Interpreting a CrossTable() 
# The probability of default increases when the credit
# rating moves from A to G
# --------------------------------------------------------------------------------
# SPLITTING THE DATA SET 
# Set seed of 567
set.seed(567)

# Store row numbers for training set: index_train
index_train <- sample(1:nrow(loan_data), 2 / 3 * nrow(loan_data))

# Create training set: training_set
training_set <- loan_data[index_train, ]

# Create test set: test_set
test_set <- loan_data[-index_train, ]
# --------------------------------------------------------------------------------
# Create the Confusion Matrix 
conf_matrix <- table(test_set$loan_status, model_pred)

# Compute classification accuracy
(6092 + 349) / nrow(test_set)

# Compute sensitivity
349 / 1037
# ------------------------------------------------------------------------------
# SIMPLE LOGISTIC REGRESSION 

# Build a glm model with variable ir_cat as a predictor
log_model_cat <- glm(formula = loan_status ~ ir_cat, family = "binomial",
                     data = training_set)

# Print the parameter estimates 
log_model_cat

# Look at the different categories in ir_cat using table()
table(loan_data$ir_cat)
# --------------------------------------------------------------------------------
# MULTIPLE VARIABLES IN A LOGISTIC REGRESSION MODEL 

# Build the logistic regression model
log_model_multi <- glm(loan_status ~ age + ir_cat + grade + loan_amnt +
                         annual_inc , family = "binomial", data = training_set)

# Obtain significance levels using summary()
summary(log_model_multi)
# --------------------------------------------------------------------------------
# INTERPRETING SIGNIFICANCE LEVELS 
# Loan amount and annual income are of the same order, but 
# annual_inc is statistically significant where loan_amount is not 

# PREDICTING THE PROBABILITY OF DEFAULT 
# Build the logistic regression model
log_model_full <- glm(loan_status ~ ., family = "binomial", data = training_set)

# Make PD-predictions for all test set elements using the the full logistic regression model
predictions_all_full <- predict(log_model_full, newdata = test_set, type = "response")

# Look at the predictions range
range(predictions_all_full)

# Make a binary predictions-vector using a cut-off of 15%
pred_cutoff_15 <- ifelse(predictions_all_full > 0.15, 1, 0)

# Construct a confusion matrix
table(test_set$loan_status, pred_cutoff_15)

  
  