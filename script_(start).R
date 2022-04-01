#############################
# K Nearest Neighbours (KNN)
#############################

# load ISLR package


# recall the Carseats dataset structure


# calculate 3rd quartile for the Sales variable


# create a new variable HighSales


# convert HighSales to factor


# remove the Sales variable


####################################
# Standardize numerical attributes
####################################

# print the summary of the dataset


# check for outliers 
# - plot the boxplot for the CompPrice variable
# - get the number of outliers in the CompPrice variable


# select numeric variables


# apply the function that returns the number of outliers to each numeric column


# check for each numerical variable if it is normally distributed (using the Shapiro-Wilk test)


# use the scale function to standardise numerical variables


# select not-normally distributed numerical columns (variables)


# apply the scale function to each column


# since apply() f. returns a list, convert it to a data frame


# standardize the Price variable (and convert to vector)


# standardize the CompPrice variable (and convert to vector)


#######################################################
# Transform factor (binary and categorical) variables
#######################################################

# transform the Urban variable to integer


# transform the US variable to integer


# examine the levels of the ShelveLoc variable


# update the order of levels for the ShelveLoc variable to: "Bad", "Medium", "Good"


# convert ShelveLoc into a numeric variable


# add the outcome variable HighSales to the transformed data set


# examine the structure of the transformed data set


# examine the summary of the transformed data set


##################################
# Create train and test data sets
##################################

# load the caret package


# set seed and create train and test sets


###################
# Model building
###################

# load the class package


# create a knn model with a randomly chosen k


# print several predictions


# create the confusion matrix


# function for computing evaluation metrics


# compute the evaluation metrics


#
# Find the optimal value for *k* through 10-fold cross-validation
#

# load e1071 library


# define cross-validation (cv) parameters; we'll perform 10-fold cross-validation


# define the range for the k values to examine in the cross-validation


# since cross-validation is a probabilistic process, 
# it is advisable to set the seed so that we can replicate the results


# run the cross-validation


# plot the cross-validation results


# build a new model with the best value for k


# create the confusion matrix


# compute the evaluation metrics


# compare the evaluation metrics for knn1 and knn2 models


