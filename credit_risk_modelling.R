###------------------------------------------- CKME 136 ------------------------------------------------------------ ### 
###------------------------------------  MENDELSOHN NEIL CHAN ------------------------------------------------------ ### 

# Load the relevant R packages required for this project 
library(tidyverse)
library(dplyr)
library(gmodels) 
library(ggthemes)
library(ggplot2) 
library(readxl)
library(rpart.plot)
library(pROC)

# Read the Excel raw data from LendingClub into RStudio 
data <- read_excel("loan_data.xlsx", col_names = TRUE) 

###------------------------------------ DATA PREPARATION STAGE ---------------------------------------------------- ### 

# Take a look at the structure of the data 
str(data) 
# There are 11948 rows of 12 variables in this credit risk data set 

# Check the data set for any missing values 
any(is.na(data)) # There is no missing data for this data set 

# Convert the term variable from a character to a categorical factor variable 
summary(data$term) 
data$term <- as.factor(data$term) 

# Convert the grade variable from a character to a factor variable representing 7 levels, 
# pertaining to a loan applicant's credit grade (A to G) representing best to worst credit grade 
summary(data$grade)
data$grade <- as.factor(data$grade) 
table(data$grade) 

# Convert the home ownership variable from a character to a categorical factor variable 
summary(data$home_ownership) 
data$home_ownership <- as.factor(data$home_ownership)
table(data$home_ownership)

# Convert the loan_status variable from a character to a factor variable 
summary(data$loan_status)
data$loan_status <- as.factor(data$loan_status)
table(data$loan_status)

# To prepare the binary response variable for the logistic regression model, 
# create a new variable, charge_off, derived from the loan_status, wherein 1 denotes 
# a loan charge off (loan cannot be recovered by the bank) and 0 denotes FALSE (loan still outstanding) 
data <- data %>% mutate(charge_off = ifelse(loan_status == "Charged Off", 1, 0))

# Examine the output of the newly created variable using a Cross Table
CrossTable(data$charge_off, prop.r = T, prop.c = FALSE, prop.chisq = F, prop.t = F)
# The table shows that 12% of loans issued by the credit company defaulted and have been charged-off 
# There are 1428 cases of charge-off loans out of 11948 data points in the data set 


###------------------------------------ DATA SPLITTING --------------------------------------------------------------- ### 


# To make the training and test sets, set a seed using set.seed(). 
# set seed of 567 
set.seed(567) 

# Store row numbers for the TRAINING SET: index_train using 2/3 of the original data 
index_train <- sample(1:nrow(data), 2/3 * nrow(data))

# Create the TRAINING SET as training_set containing 
training_set <- data[index_train, ]

# Create the TEST SET as test_set 
test_set <- data[-index_train, ]


###--------------------------------- LOGISTIC REGRESSION -------------------------------------------------------------- ### 


# Fit the Logistic Regression Model with the charge_off column as our binary response variable 
# Use the training_set to build the model 
logistic.model <- glm(charge_off ~ home_ownership + annual_inc + loan_amnt + term + int_rate +
                        grade + fico_score + inq_last_6mths, family = "binomial", data = training_set)

# Obtain the significance levels of the variables using the summary() function 
# The statistically significant variables based on the p-value are annual_inc, term, grade, inq_last_6_mths 
# The variables home_ownership, loan_amnt, int_rate and fico_score are not statistically significant 
summary(logistic.model) 


# Fit another Logistic Regression Model using only statistically significant predictor variables 
logistic.model.sig <- glm(charge_off ~ annual_inc + term + grade + inq_last_6mths, 
                                  family = "binomial", data = training_set) 


###---------------------------- PREDICTING PROBABILITY OF A LOAN CHARGE-OFF ------------------------------------------ ### 


# Make predictions for the test set elements using the created logistic regression model
predictions.logistic <- predict(logistic.model, newdata = test_set, type = "response") 

# Make predictions for the test set elements using the logistic regression model only with significant variables 
predictions.logistic.sig <- predict(logistic.model.sig, newdata = test_set, type = "response") 

# Take a look at the range of the probability predictions 
range(predictions.logistic)
range(predictions.logistic.sig)

# The range of predictions for both models is wide which is a good indicator; a small range means that the test set 
# cases do not lie far apart, therefore the model might not be good in discriminating good and bad loans 


###------------------------ EVALUATING THE RESULT OF LOGISTIC REGRESSION MODEL --------------------------------------- ### 

# To compare our predictions with the binary test_set$charge_off column, we must 
# transform the prediction vector to binary values of 1 and 0 indicating the status of the loan. 
# A cut-off or threshold must be set in this case. 
# If the predicted probability lies above the cutoff value then the prediction is set to 1, indicating
# a loan that charged off, otherwise it is set to 0, indicating the loan is still active;  
# A confusion matrix can be created afterwards to calculate Accuracy and compare cut-offs 
# The cut-off is basically a measure of risk tolerance of the financial institution 
# If we set a lower cutoff value, it means that we will classify a loan as a charge-off if it exceeds 
# a certain level of probabilistic risk. 

# USING A CUT-OFF VALUE OF 25% 
# Make a binary predictions-vector using a cut-off of 25%
pred_cutoff_25 <- ifelse(predictions > 0.25, 1, 0)
# Construct a confusion matrix using a cut-off of 25% 
conf_matrix_25 <- table(test_set$charge_off, pred_cutoff_25)
# Calculate for Accuracy 
accuracy.25 <- sum(diag(conf_matrix_25)) / nrow(test_set)
# The accuracy for the model is 86.32% 

# USING A CUT-OFF VALUE OF 50% 
# Make a binary predictions vector using a cut-off of 50% 
pred_cutoff_50 <- ifelse(predictions > 0.50, 1, 0) 
# Construct a confusion matrix using a cut-off of 50% 
conf_matrix_50 <- table(test_set$charge_off, pred_cutoff_50) 
# Calculate for Accuracy 
accuracy.50 <- sum(diag(conf_matrix_50)) / nrow(test_set) 
# The accuracy for the model is 89.02% 

# COMPARING THE TWO CUT-OFFS 
# Moving from a cut-off of 25% to 50% increases overall Accuracy of the model 


###----------------------------------------- DECISION TREE------------------------------------------------------------ ### 


# Load the rpart package for the construction of the decision tree 
library(rpart)

# Construct a Decision Tree 
tree.model <- rpart(charge_off ~ home_ownership + annual_inc + loan_amnt + term + int_rate +
                      grade + fico_score + inq_last_6mths, method = "class",
                    data = training_set, control = rpart.control(cp = 0.001))

# Construct a Decision Tree with changed prior probabilities 
# The original data set is imbalanced in the sense that the cases of non charge-off loans outnumber
# loans that are charged-off. To fix this, we can change the prior probabilities in the rpart function 
# By default, the prior probabilities of charge-off and non charge-off are set equal to their proportions 
# in the training set. By making the prior probabilities of charge-off larger, we place a greater importance 
# on charge-offs, leading to a better decision tree 

tree.model.modified <- rpart(charge_off ~ home_ownership + annual_inc + loan_amnt + term + int_rate +
                      grade + fico_score + inq_last_6mths, method = "class",
                    data = training_set, parms = list(prior = c(0.60, 0.40)),
                    control = rpart.control(cp = 0.001))

# Plot the decision trees 
plot(tree.model, uniform = TRUE)
plot(tree.model.modified, uniform = TRUE) 

# Add labels to the decision trees 
text(tree.model) 
text(tree.model.modified)


###--------------------------------------- PRUNING THE DECISION TREE--- ---------------------------------------------- ### 


# Pruning a large tree is necessary to prevent overfitting, which can lead to inaccurate predictions 

# Use plotcp() to visualize cross-vaidated error (X-val Relative Error) in relation 
# to the complexity parameter for the tree.model 
plotcp(tree.model.modified)

# Use printcp() to print a table of information about CP, splits, and errors. The goal is to identify 
# which split has the minimum cross-validated error in tree.model 
printcp(tree.model.modified) 

# Create an index for the row with the minimum xerror
index <- which.min(tree.model.modified$cptable[, "xerror"])

# Create tree_min
tree_min <- tree.model.modified$cptable[index, "CP"]

#  Prune the tree using tree_min
pruned.tree <- prune(tree.model.modified, cp = tree_min)

# Use prp() to plot the pruned tree
prp(pruned.tree)


###--------------------------------------- EVALUATING THE DECISION TREE ---------------------------------------------- ### 

# Make predictions for the probability of default using the 2 decision trees created 
predictions.tree <- predict(tree.model, newdata = test_set)[ ,2]
predictions.ptree <- predict(pruned.tree, newdata = test_set)[, 2]

# Make binary predictions for the pruned decision tree and original tree using the test set 
predictions.binary.tree <- predict(tree.model, newdata = test_set, type = "class")
predictions.binary.ptree <- predict(pruned.tree, newdata = test_set, type = "class")

# Construct confusion matrices using the predictions.
confmatrix_tree <- table(test_set$charge_off, predictions.tree)

# Calculate for the model accuracy 
accuracy.tree <- sum(diag(confmatrix_tree)) / nrow(test_set)
# The accuracy of the decision tree model is 72% 

###--------------------------------------- THE ROC CURVE ------------------------------------------------------------ ### 

# ROC Curves for the comparison of the 2 Logistic Regression Models 
ROC_logistic <- roc(test_set$charge_off, predictions.logistic)
ROC_logistic.significant <- roc(test_set$charge_off, predictions.logistic.sig)

# Use the previously created objects to construct ROC-curves all in one plot 
plot(ROC_logistic, col = "green") 
lines(ROC_logistic.significant, col = "red")

# Compute for the Area Under the Curve (AUC) 
# The logistic regression model with only significant variables has a higher AUC and should be preferred 
auc(ROC_logistic) # 0.6690
auc(ROC_logistic.significant) # 0.6709 

# ROC Curves for the comparison of the 2 Decision Tree Models 
ROC_tree <- roc(test_set$charge_off, predictions.tree)
ROC_tree.pruned <- roc(test_set$charge_off, predictions.ptree)

# Use the previously created objects to construct ROC-curves all in one plot 
plot(ROC_tree, col = "black") 
lines(ROC_tree.pruned, col = "blue")

# Compute for the Area Under the Curve (AUC) 
# The pruned decision tree model has a higher AUC and should be preferred 
auc(ROC_tree) # 0.6222 
auc(ROC_tree.pruned) # 0.6539 

# Compare the ROC of the logistic regression and decision tree models 
plot(ROC_logistic.significant, col = "red")
lines(ROC_tree.pruned, col = "blue")

# COMPARE THE best logistic vs. the best decision tree 
auc(ROC_logistic.significant) # 0.6709 
auc(ROC_tree.pruned) # 0.6539 





