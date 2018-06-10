createConfMatrixAndParams <- function(predicted, actual, nrOfClasses) {
  confMatrix<- matrix(data=0, nrow=nrOfClasses, ncol = nrOfClasses)
  # Fill confusion matrix
  for( i in 1:length(actual)){
    confMatrix[actual[i],predicted[i]] <- confMatrix[actual[i],predicted[i]] + 1
  }
  
  m_diag <- diag(confMatrix)
  m_n <- sum(confMatrix)
  m_rowsum <- rowSums(confMatrix)
  m_colsum <- colSums(confMatrix)
  
  accuracy <- sum(m_diag)/m_n
  precision <- m_diag / m_colsum
  recall <- m_diag / m_rowsum
  f1 <- 2*precision*recall/(precision + recall)
  f1[is.na(f1)] <- 0
  
  result <- vector("list")
  result[["confMatrix"]] <- confMatrix
  result[["accuracy"]] <- accuracy
  result[["precision"]] <- precision
  result[["recall"]] <- recall
  result[["f1"]] <- f1
  result[["macroPrecision"]] <- mean(precision)
  result[["macroRecall"]] <- mean(recall)
  result[["macroF1"]] <- mean(f1)
  
  return(result)
}