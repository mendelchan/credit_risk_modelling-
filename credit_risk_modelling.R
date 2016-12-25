# CREDIT RISK MODELLING IN R 
# R Script By: Mendelsohn Chan 
## -------------------------------------------------------------------------------- ##
# View the structure of loan_data 
str(loan_data)

# View the summary of loan_data
summary(loan_data) 

# Convert credit_policy as a categorical variable 
summary(loan_data) 

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

## -------------------------------------------------------------------------------- ##

# DATA PREPARATION  
library(caTools)
set.seed(100) 
spl <- sample.split(loan_data$not_fully_paid, 0.7) 
train <- subset(loan_data, spl == TRUE) 
test <- subset(loan_data, spl == FALSE)

## -------------------------------------------------------------------------------- ##

# BUILDING THE LOGISTIC MODEL 
modLog = glm(not_fully_paid ~. -annualincome, data=train, family="binomial")
summary(modLog)

# SIGNIFICANCE LEVEL OF INDEPENDENT VARIABLES 
# Significant variables:CreditCard_Purpose, int_rate, inq.last.6mths and pub_records 

# REVISED LOGISTIC REGRESSION MODEL 
# Include only the significant variables 
modLog2 = glm(not_fully_paid ~ purpose + int_rate + installment + log.annual.inc + inq.last.6mths 
              + pub_records, data = train, family = "binomial")
summary(modLog2)

# MAKING PREDICTIONS ON THE TEST DATA 
test$predicted_risk <- predict(modLog2, newdata = test, type = "response")

# MEASURE THE ACCURACY OF THE MODEL
table(test$not_fully_paid, as.numeric(test$predicted_risk >= 0.5))
# Accuracy <- (970 + 154) / nrow(test) 
# Sensitivity <- 154/460 
# Specificity <- 970 / 1040 

# COMPARE TO BASELINE ACCURACY 
table(test$not_fully_paid) 

# The baseline accuracy is 0.693 so the Revised Model with a score of 0.749
# is a substantial improvement 

## -------------------------------------------------------------------------------- ##

# TEST SET AREA UNDER THE CURVE (AUC) 
library (gplots)
library(ROCR) 
pred <- prediction(test$predicted_risk, test$not_fully_paid)
as.numeric(performance(pred, "auc")@y.values)

# Make predictions on the training data 
predictTrain <- predict(modLog2, type = "response") 
ROCRpred <- prediction(predictTrain, train$not_fully_paid) 
ROCRperf <- performance(ROCRpred, "tpr", "fpr") 

# Plot the ROC curve 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
# The cut-off value of 0.35 is the best choice to balance sensitivity and specificity

# TESTING THE NEW THRESHOLD VALUE 
t1 <- table(test$not_fully_paid, as.numeric(test$predicted_risk >= 0.35))
# Accuracy = (828 + 241) / nrow(test) 
# Sensitivity = 241 / 460
# Specificity = 828/ 1040 

## -------------------------------------------------------------------------------- ##




