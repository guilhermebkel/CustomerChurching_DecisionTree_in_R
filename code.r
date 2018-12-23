# -> Import our required libraries:
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

# -> Normalizing Data:
custchurn = read.csv("https://community.watsonanalytics.com/wp-content/uploads/2015/03/WA_Fn-UseC_-Telco-Customer-Churn.csv")
# Changing column names
colnames(custchurn) = c("CustomerID","Gender","SeniorCitizen","Partner","Dependents","Tenure","PhoneService",
                        "MultipleLines", "Internet","OnlineSecurity","OnlineBackup","DeviceProtection",
                        "TechSupport", "StreamingTV","StreamingMovies","Contract","PaperlessBilling",
                        "Payment", "Monthly Charges","Total Charges","Churn")
# Define the factor names for "Contract"
levels(custchurn$Contract) = c("Month", "One Year", " Two Years")
# Define the factor names for "Churn"
levels(custchurn$Churn) = c("Stays", "Leaves")

# -> Taking only the columns we want to our model
custchurn.test = custchurn[c("Tenure", "Contract", "Churn", "Total Charges")]

# -> Create a decision tree using "Churn" as the variable target and everything else as the predictors:
myDecisionTree = rpart(Churn ~ ., data = custchurn.test, method = "class")
# Vizualising the tree:
rpart.plot(myDecisionTree, type = 1, extra = 2, under = TRUE, faclen=20, cex = 0.65, shadow.col = 'gray')

# -> Model Accuracy:
train_ind = sample(c(1:n), size = 10)
# 75% of the sample size
n = nrow(custchurn.test)
smp_size = floor(0.75 * n)
# set the seed to make your partition reproductible
set.seed(123)
train_ind = base::sample(c(1:n), size = smp_size)
custchurn.test_train = custchurn.test[train_ind, ]
custchurn.test_test = custchurn.test[-train_ind, ]
newDT = rpart(Churn ~ ., data = custchurn.test_train, method = "class")
result = predict(newDT, custchurn.test_test, type = "class")
# Prints out the confusion matrix and accuracy of model
confMat = table(custchurn.test_test$Churn, result)
accuracy = sum(diag(base))/sum(base)
cat("\014")
cat(" -> The confusion matrix for customer churning is below with [", formatC(accuracy*100, 4, digits = 2, format = "f"), "% ] accuracy: ")
confMat
