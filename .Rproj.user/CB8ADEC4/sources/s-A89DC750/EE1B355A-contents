##########################
# Decision trees
##########################

# load ISLR package
# install.packages('ISLR')
library(ISLR)

# get the Carseats dataset docs
?Carseats

# print dataset structure
str(Carseats)

# print Sales variable distribution
summary(Carseats$Sales)

# get the 3rd quartile of the Sales variable
sales.3Q <- quantile(Carseats$Sales, 0.75)

# create a new variable HighSales based on the value of the Sales variable
Carseats$HighSales <- ifelse(test = Carseats$Sales > sales.3Q, yes = 'Yes', no = 'No')
head(Carseats)

# class of the HighSales variable
class(Carseats$HighSales)

# convert HighSales into a factor variable
Carseats$HighSales <- as.factor(Carseats$HighSales)
head(Carseats$HighSales)

# get the distribution of the HighSales variable
table(Carseats$HighSales)

# get the proportions of the HighSales variable
prop.table(table(Carseats$HighSales))

##################################
# Create train and test datasets
##################################

# remove Sales variable
Carseats$Sales <- NULL

# load caret package
library(caret)

# create train and test datasets
set.seed(10)
train.indices <- createDataPartition(Carseats$HighSales, # the variable defining the class
                                     p = .80,            # the proportion of observations in the training set
                                     list = FALSE)       # do not return the result as a list (which is the default)
train.data <- Carseats[train.indices,]
test.data <- Carseats[-train.indices,]

# print distributions of train and test datasets
prop.table(table(train.data$HighSales))
prop.table(table(test.data$HighSales))

##################################################
# Create a prediction model using Decision Trees
##################################################

# load rpart library
library(rpart)

# build the model
tree1 <- rpart(HighSales ~ ., data = train.data, method = "class")

# print the model
print(tree1)

# load rpart.plot library
# install.packages("rpart.plot")
library(rpart.plot)

# plot the tree
prp(tree1, type = 3, extra = 1)

# make the predictions with tree1 over the test dataset
tree1.pred <- predict(object = tree1, newdata = test.data, type = "class")

# print several predictions
head(tree1.pred)

# create the confusion matrix
tree1.cm <- table(true=test.data$HighSales, predicted=tree1.pred)
tree1.cm

# function for computing evaluation metrix
compute.eval.metrics <- function(cmatrix) {
  TP <- cmatrix[1,1] # true positive
  TN <- cmatrix[2,2] # true negative
  FP <- cmatrix[2,1] # false positive
  FN <- cmatrix[1,2] # false negative
  acc <- sum(diag(cmatrix)) / sum(cmatrix)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- 2*precision*recall / (precision + recall)
  c(accuracy = acc, precision = precision, recall = recall, F1 = F1)
}

# compute the evaluation metrics
tree1.eval <- compute.eval.metrics(tree1.cm)
tree1.eval

# get the docs for the rpart.control function
?rpart.control

# build the second model with minsplit = 10 and cp = 0.001
tree2 <- rpart(HighSales ~ ., data = train.data, method = "class",
control = rpart.control(minsplit = 10, cp = 0.001))

# print the model
print(tree2)

# plot the tree2
prp(tree2, type = 3, extra = 1)

# make the predictions with tree2 over the test dataset
tree2.pred <- predict(tree2, newdata = test.data, type = "class")

# create the confusion matrix for tree2 predictions
tree2.cm <- table(true=test.data$HighSales, predicted=tree2.pred)
tree2.cm

# compute the evaluation metrics
tree2.eval <- compute.eval.metrics(tree2.cm)
tree2.eval

# compare the evaluation metrics for tree1 and tree2
data.frame(rbind(tree1.eval, tree2.eval), 
row.names = c("tree 1", "tree 2"))

# load e1071 library
# install.packages('e1071')
library(e1071)

# define cross-validation (cv) parameters; we'll do 10-fold cross-validation
numFolds = trainControl( method = "cv", number = 10 )

# then, define the range of the cp values to examine in the cross-validation
cpGrid = expand.grid( .cp = seq(0.001, to = 0.05, by = 0.001)) 

# since cross-validation is a probabilistic process, it is advisable to set the seed so that we can replicate the results
set.seed(10)

# run the cross-validation
dt.cv <- train(HighSales ~ ., 
               data = train.data, 
               method = "rpart", 
               control = rpart.control(minsplit = 10), 
               trControl = numFolds, 
               tuneGrid = cpGrid)
dt.cv

# plot the cross-validation results
plot(dt.cv)

# prune the tree2 using the cp = 0.041
tree3 <- prune(tree2, cp = 0.041)

# print the new tree
print(tree3)

# make the predictions with tree3 over the test dataset
tree3.pred <- predict(tree3, newdata = test.data, type = "class")

# create the confusion matrix for tree2 predictions
tree3.cm <- table(true = test.data$HighSales, predicted = tree3.pred)
tree3.cm

# compute the evaluation metrics
tree3.eval <- compute.eval.metrics(tree3.cm)
tree3.eval

# compare the evaluation metrics for tree1, tree2 and tree3
data.frame(rbind(tree1.eval, tree2.eval, tree3.eval),
           row.names = c(paste("tree", 1:3)))