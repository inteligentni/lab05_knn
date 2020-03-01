##########################
# K Nearest Neighbours (KNN)
##########################
  
# load ISLR package
library(ISLR)

# print dataset structure
str(Carseats)

# calculate 3rd quartile
sales.3Q <- quantile(Carseats$Sales, 0.75)

# create a new variable HighSales based on the value of the 3rd quartile
Carseats$HighSales <- ifelse(test = Carseats$Sales > sales.3Q,
                             yes = 'Yes',
                             no = 'No')

# convert HighSales from character to factor
Carseats$HighSales <- as.factor(Carseats$HighSales)

# remove the Sales variable
Carseats <- Carseats[,-1]

####################################
# Standardize numerical attributes
####################################

# print the summary of the dataset
summary(Carseats)

# plot the boxplot for the CompPrice variable
boxplot(Carseats$CompPrice)

# print the number of outliers in the CompPrice variable
length(boxplot.stats(Carseats$CompPrice)$out)

# select numeric variables
numeric.vars <- c(1:5,7,8) # indices of numeric columns

# apply the function that returns the number of outliers to each numeric column
apply(X = Carseats[,numeric.vars], # select numeric columns
      MARGIN = 2, # apply the function to columns
      FUN = function(x) length(boxplot.stats(x)$out)) # the function to be applied to each column

# apply the test to each numerical column (variable)
apply(X = Carseats[,numeric.vars], 
      MARGIN = 2, 
      FUN = shapiro.test)

# get the documentation for the scale function
?scale

# select not-normally distributed numerical columns (variables)
carseats.st <- Carseats[, c(2,3,4,7,8)]

# apply the scalling function to each column
carseats.st <- apply(X = carseats.st, 
                     MARGIN = 2,
                     FUN = function(x) scale(x, center = median(x), scale = IQR(x)))

# since apply() f. returns a list, convert it to a data frame
carseats.st <- as.data.frame(carseats.st)

# standardize the Price variable (and convert to vector)
carseats.st$Price <- as.vector(scale(x = Carseats$Price, center = TRUE, scale = TRUE))

# standardize the CompPrice variable (and convert to vector)
carseats.st$CompPrice <- as.vector(scale(x = Carseats$CompPrice, center = TRUE, scale = TRUE))

#######################################################
# Transform factor (binary and categorical) variables
#######################################################

# transform the Urban variable to integer
carseats.st$Urban <- as.integer(Carseats$Urban)

# transform the US variable to integer
carseats.st$US <- as.integer(Carseats$US)

# examine the levels of the ShelveLoc variable
levels(Carseats$ShelveLoc)

# update the order of levels for the ShelveLoc variable to: "Bad", "Medium", "Good"
Carseats$ShelveLoc <- factor(Carseats$ShelveLoc, levels = c("Bad", "Medium", "Good"))
levels(Carseats$ShelveLoc)

# convert ShelveLoc into a numeric variable
carseats.st$ShelveLoc <- as.integer(Carseats$ShelveLoc)

# add the outcome variable HighSales to the transformed dataset
carseats.st$HighSales <- Carseats$HighSales

# examine the structure of the transformed dataset.
str(carseats.st)

# exaine the summary of the data frame
summary(carseats.st)

##################################
# Create train and test data sets
##################################

# load the caret package
library(caret)

# set seed
set.seed(10320)

# create train and test sets
train.indices <- createDataPartition(carseats.st$HighSales, p = 0.8, list = FALSE)
train.data <- carseats.st[train.indices,]
test.data <- carseats.st[-train.indices,]

###################
# Model building
###################

# load the class package
library(class)

?knn

# createa a knn model with k=5
knn.pred <- knn(train = train.data[,-11], 
                test = test.data[,-11],
                cl = train.data$HighSales,
                k = 5) # 5 is chosen here just as a random guess

# print several predictions
head(knn.pred)

# create the confusion matrix
knn.cm <- table(true = test.data$HighSales, predicted = knn.pred)
knn.cm

# function for computing evaluation metrics
compute.eval.metrics <- function(cmatrix) {
  TP <- cmatrix[1,1] # true positive
  TN <- cmatrix[2,2] # true negative
  FP <- cmatrix[2,1] # false positive
  FN <- cmatrix[1,2] # false negative
  acc = sum(diag(cmatrix)) / sum(cmatrix)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- 2*precision*recall / (precision + recall)
  c(accuracy = acc, precision = precision, recall = recall, F1 = F1)
}

# compute the evaluation metrics
knn.eval <- compute.eval.metrics(knn.cm)
knn.eval


# Find the optimal value for *k* through 10-fold cross-validation

# load e1071 library
library(e1071)

# define cross-validation (cv) parameters; we'll perform 10-fold cross-validation
numFolds = trainControl( method = "cv", number = 10)

# define the range for the k values to examine in the cross-validation
cpGrid = expand.grid(.k = seq(from=3, to = 25, by = 2))

# since cross-validation is a probabilistic process, it is advisable to set the seed so that we can replicate the results
set.seed(10320)

# run the cross-validation
knn.cv <- train(x = train.data[,-11],
                y = train.data$HighSales,
                method = "knn", 
                trControl = numFolds,
                tuneGrid = cpGrid)
knn.cv

# plot the cross-validation results
plot(knn.cv)

# build a new model with the best value for k
best_k <- knn.cv$bestTune$k
knn.pred2 <- knn(train = train.data[,-11],
                 test = test.data[,-11],
                 cl = train.data$HighSales,
                 k = best_k)

# create the confusion matrix
knn.cm2 <- table(true = test.data$HighSales, predicted = knn.pred2)
knn.cm2

# compute the evaluation metrics
knn.eval2 <- compute.eval.metrics(knn.cm2)
knn.eval2

# compare the evaluation metrics for knn1 and knn2 models
data.frame(rbind(knn.eval, knn.eval2), row.names = c("knn_1", "knn_2"))

