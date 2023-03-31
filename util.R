
get_adapted_carseats_dataset <- function() {
  
  # load ISLR package
  require(ISLR)
  
  # calculate the 3rd quartile
  sales.3Q <- quantile(Carseats$Sales, 0.75)
  
  # create a new variable HighSales based on the value of the 3rd quartile
  Carseats$HighSales <- ifelse(test = Carseats$Sales > sales.3Q,
                               yes = 'Yes',
                               no = 'No')
  
  # convert HighSales from character to factor
  Carseats$HighSales <- as.factor(Carseats$HighSales)
  
  # remove the Sales variable
  Carseats$Sales <- NULL
  
  return(Carseats)
  
}

compute_eval_metrics <- function(cmatrix) {
  
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