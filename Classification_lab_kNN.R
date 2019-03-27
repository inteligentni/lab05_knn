#######################
# Examine the data set
#######################

# We will use the same data set (Carseats) as in the previous Lab 
# (classification w/ Decision Trees)
library(ISLR)
str(Carseats)

# As we did before, we will introduce a categorical (factor) variable - HighSales - to 
# be used as the outcome variable (variable defining the class for each observation).
# if a sale is greater than the 3rd quartile (9.32), it qualified as high sale:
Carseats$HighSales <- ifelse(test = Carseats$Sales > 9.32, yes = 'Yes', no = 'No')
Carseats$HighSales <- as.factor(Carseats$HighSales)

# Remove the Sales variable, as we do not need it any more
Carseats <- Carseats[,-1]

# Recall that kNN algorithm primarily works with numerical data. 
# So, if we want to use categorical and/or binary variables, we have to transform them 
# into numerical variables. 
# In addition, kNN is very sensitive to differences in the value range of predictor variables. 
# Predictors with wider range of values (e.g. Price) tend to diminish the influence 
# of variables with a significantly narrower range (e.g. Education). 

# Examine the variables and their value ranges
summary(Carseats)

# Numerical variables should be rescalled. This can be generally done in two ways:
# - normalization - reducing variable values to a common value range, typically [0,1]; 
#   this is often done using the formula: (X - min(X))/(max(X) - min(X))
# - standardization - rescaling variables so that their mean=0 and SD=1; 
#   for variable X that is normally distributed, this is done by computing: 
#   (X - mean(X))/SD(X); if variable X is not normally distributed, then, standardization 
#   is typically done using median and inter-quartile range (IQR): (X - median(X))/IQR(X) 

# Normalization should be avoided if (numerical) variables have outliers; 
# standardization should be used instead. In the absence of outliers, 
# either of the two can be used.

# Check the presence of outliers:
numeric.vars <- c(1:5,7,8) # indices of numeric columns (variables)
apply(X = Carseats[,numeric.vars], # select numeric columns
      MARGIN = 2, # apply the function to columns
      FUN = function(x) length(boxplot.stats(x)$out)) # the function to be applied to each column

# only 2 columns have just a few outliers; hence, either of the scaling methods can be used.

##################################
# Standardize numerical variables
##################################

# The numerical variables will be rescalled by standardizing them (typical approach). 
# To determine how to standardize the variables, we need to check their distribution 
# (if they follow Normal distribution or not). 

# We will use Shapiro-Wilk test to check for normality 
# The null hypothesis of this test is that a sample comes from a normally distributed 
# population; if the test is not significant (p>0.05), we can assume that the null 
# hypothesis holds.
apply(X = Carseats[,numeric.vars], MARGIN = 2, FUN = shapiro.test)

# Only CompPrice and Price are normally distributed.
# So, we will standardize Price and CompPrice using mean and SD, and for other variables, 
# we'll use median and IQR. 

# To do the scaling, use the scale() f. (from the base package):
?scale

# Rescaling variable that are not normally distributed:
carseats.st <- Carseats[, c(2,3,4,7,8)]
carseats.st <- as.data.frame(apply(X = carseats.st, 
                                   MARGIN = 2, 
                                   FUN = function(x) scale(x, center = median(x), scale = IQR(x))))

# Next, standardize and add (to carseats.st) the two normally distributed variables
carseats.st$Price <- as.vector(scale(x = Carseats$Price, center = TRUE, scale = TRUE))
carseats.st$CompPrice <- as.vector(scale(x = Carseats$CompPrice, center = TRUE, scale = TRUE))

#######################################################
# Transform factor (binary and categorical) variables
#######################################################

# Transform binary variables into numerical:
carseats.st$Urban <- as.integer(Carseats$Urban)
carseats.st$US <- as.integer(Carseats$US)

# It is often recommended to first encode categorical variables as binary dummy variables, 
# and then transform the resulting binary variables into numerical ones. 
# However, for simplicity reasons, and since our categorical variable - ShelveLoc - is ordered, 
# we can directly transform it into a numerical variable. 

# Examine the order of ShelveLoc levels:
levels(Carseats$ShelveLoc)

# As the order is not a 'natural' one, it should be changed:
Carseats$ShelveLoc <- factor(Carseats$ShelveLoc, levels = c("Bad", "Medium", "Good"))
levels(Carseats$ShelveLoc)
# Next, transform ShelveLoc into numerical variable:
carseats.st$ShelveLoc <- as.numeric(Carseats$ShelveLoc)

# TASK: create dummy variables for ShelveLoc and build a model with these new variables; 
# to learn how to create dummy variables using the caret package, read: 
# http://topepo.github.io/caret/pre-processing.html#dummy

# Finaly, add the outcome (class) variable to the transformed data set
carseats.st$HighSales <- Carseats$HighSales

# Examine the transformed data set
str(carseats.st)
summary(carseats.st)

##################################
# Create train and test data sets
##################################

# This is to be done as in the previous Labs
library(caret)
# take 80% of observations for the training set and the rest for the test set
set.seed(1010)
train.indices <- createDataPartition(carseats.st$HighSales, p = 0.8, list = FALSE)
train.data <- carseats.st[train.indices,]
test.data <- carseats.st[-train.indices,]

##################
# Model building
##################

# To build a kNN classification model, we will use the knn() f. from the class R package
library(class)

# The knn() f. should be supplied with:
# - training data without the class variable
# - test data without the class variable 
# - class values for the the training set
# - number of neighbours to consider

knn.pred <- knn(train = train.data[,-11], # the class variable (HighSales) is the 11th column
                test = test.data[,-11],
                cl = train.data$HighSales,
                k = 5) # 5 is chosen here just as a random guess

# The result of the knn f. are, in fact, predictions on the test set
head(knn.pred)

# To evaluate the results, first create the confusion matrix:
knn.cm <- table(true = test.data$HighSales, predicted = knn.pred)
knn.cm

# To compute the evaluation metrics, use the function from the previous Lab,
# now moved to a separate R script
source("EvaluationMeasures.R")

# Compute the evaluation metrics based on the confusion matrix
knn.eval <- compute.eval.metrics(knn.cm)
knn.eval

# We made a guess about the number of neighbours, and might not have made the best one. 
# Intead of guessing, we should cross-validate models with several different values for k; 
# then, use the test set to evaluate the model that proves to be the best on cross-validation.

# As was done with decision trees, the caret package will be used to find the optimal 
# parameter value through cross validation.
# First, define cross-validation (cv) parameters; we'll do 10-fold cross-validation:
numFolds = trainControl( method = "cv", number = 10 )
# Then, define the range of k values to examine in the cross-validation; 
# we'll take odd numbers between 3 and 25 - recall that in case of binary classification, 
# it is recommended to choose an odd number for k 
cpGrid = expand.grid(.k = seq(from=3, to = 25, by = 2)) 

# Train the model through cross-validation
set.seed(1010)
knn.cv <- train(HighSales ~ ., 
                data = train.data, 
                method = "knn", 
                trControl = numFolds, 
                tuneGrid = cpGrid)

# Examine the obtained results:
knn.cv
plot(knn.cv)

# k=9 proved to be the best value. Build a model with that value for k:
knn.pred2 <- knn(train = train.data[,-11],
                 test = test.data[,-11],
                 cl = train.data$HighSales,
                 k = 9)

# Create the confusion matrix
knn.cm2 <- table(true = test.data$HighSales, predicted = knn.pred2)
knn.cm2

# Compute evaluation measures
knn.eval2 <- compute.eval.metrics(knn.cm2)
knn.eval2

# Compare the evaluation metrics of the two models to check how the new model fares
data.frame(rbind(knn.eval, knn.eval2), row.names = c("knn 1", "knn 2"))


# TASK: Create a new model by taking only a subset of variables, for example, 
# those that proved relevant in the DT model, and compare the performance with 
# the previously built models.

# Potentially useful articles:
# - https://rpubs.com/njvijay/16444
# - http://dataaspirant.com/2017/01/09/knn-implementation-r-using-caret-package/
  