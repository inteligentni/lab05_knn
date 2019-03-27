##########################
# K Nearest Neighbours (KNN)
##########################
  
# load ISLR package


# print dataset structure


# calculate 3rd quartile


# create a new variable HighSales based on the value of the 3rd quartile


# convert HighSales from character to factor


# remove the Sales variable


####################################
# Standardize numerical attributes
####################################

# print the summary of the dataset


# plot the boxplot for the CompPrice variable


# print the number of outliers in the CompPrice variable


# filter all numerica variables


# apply the function for returning the number of outliers for all numeric variables


# apply the test to each numerical column (variable)


# get the documentation for the scale function


# select not-normally distributed numerical columns (variables)


# apply the scalling function to each column


# since apply() f. returns a list, convert it to a data frame


# standardize the Price variable (and convert to vector)


# standardize the CompPrice variable (and convert to vector)


#######################################################
# Transform factor (binary and categorical) variables
#######################################################

# transform the Urban variable to integer


# transform the US variable to integer


# print the levels of the ShelveLoc variable


# update the order of levels for the ShelveLoc variable to: "Bad", "Medium", "Good"


# convert ShelveLoc into a numeric variable


# add the outcome variable HighSales


# print the structure of the data frame


# print the summary of the data frame


##################################
# Create train and test data sets
##################################

# load the caret package


# set seed


# create train and test sets


###################
# Model building
###################

# load the class package


# createa a knn model with k=5


# print several predictions


# create the confusion matrix


# function for computing evaluation metrix
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


# load e1071 library


# define cross-validation (cv) parameters; we'll perform 10-fold cross-validation


# define the range for the k values to examine in the cross-validation


# since cross-validation is a probabilistic process, it is advisable to set the seed so that we can replicate the results


# run the cross-validation


# plot the cross-validation results


# build a new model with k=9


# create the confusion matrix


# compute the evaluation metrics


# compare the evaluation metrics for knn1 and knn2 models

