# Best Model for cloud/no cloud prediction
library(MASS)

# Import data
# Train Set consists of image1,2,3
image1 <- read.table('image1.txt', header=F, stringsAsFactors=FALSE)
image2 <- read.table('image2.txt', header=F, stringsAsFactors=FALSE)
image3 <- read.table('image3.txt', header=F, stringsAsFactors=FALSE)

# Test Set is a new image, use the correct location and name for the test set
# ***Make sure this image is the test image***
image4 <- read.table('image3.txt', header=F, stringsAsFactors=FALSE)

# Add labels to the images
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image1) <- collabs
names(image2) <- collabs
names(image3) <- collabs
names(image4) <- collabs

# discard all unsure rows (label == 0) for the purposes of classification
image1.sure <- image1[which(image1$label != 0), ]
image1.sure$label <- factor(image1.sure$label)
image2.sure <- image2[which(image2$label != 0), ]
image2.sure$label <- factor(image2.sure$label)
image3.sure <- image3[which(image3$label != 0), ]
image3.sure$label <- factor(image3.sure$label)
image4.sure <- image4[which(image4$label != 0), ]
image4.sure$label <- factor(image4.sure$label)


imageall.sure <- rbind(image1.sure, image2.sure, image3.sure)
train <- imageall.sure
test <- image4.sure

bestModel <- function(train, test){
  # Prints the probability  vector for the test set.
  # Args:
  #  train: The data of the training set.
  #  test: The data of the test set.
  
  # Use logit model with NDAI, CORR and SD to find the predicted probability vector
  logitall <- glm(label ~ NDAI+CORR+SD, data=train, family="binomial")
  logit.pred <- predict(logitall, newdata=test, type="response")

  # Use QDA with NDAI, CORR and SD to find the predicted probability vector
  qda <- qda(label ~ NDAI+SD+CORR, data = train)
  qda.pred <- predict(qda, newdata=test)$posterior[, "1"]
  return(0.5*logit.pred + 0.5*qda.pred)
}

bestModel.pred <- bestModel(train, test)

# Evaluate the model
### Helper function for ROC curve and AUC computation
CalculateTPR <- function(thresh, preds, truth) {
  # Print the true positive rate for a fitted model.
  # Args:
  #  thresh: the threshold to decide classification
  #  preds: The numeric predictions on the held out data
  #  truth: The true classifications of the held out data (same length as preds)
  as.numeric(sum(preds[truth] > thresh) / sum(truth))
}

CalculateFPR <- function(thresh, preds, truth) {
  # Print the false positive rate for a fitted model.
  # Args:
  #  thresh: the threshold to decide classification
  #  preds: The numeric predictions on the held out data
  #  truth: The true classifications of the held out data (same length as preds)
  as.numeric(sum(preds[!truth] > thresh) / sum(!truth))
}

EvaluateModel <- function(preds, truth) {
  # Print the threshold that gives the optimal TPR and FPR, and the optimal TPR, FPR values.
  # Print an ROC curve and return the AUC for a fitted model.
  # Args:
  #  preds: The numeric predictions on the held out data
  #  truth: The true classifications of the held out data (same length as preds)
  
  # Plot an ROC curve
  res <- 1000  
  # Get the TPRs and FPRs for all possible thresholds.  (Note that this can
  # be done more efficiently for large data sets.  See "An introduction to ROC analysis"
  # by Fawcett.)
  tprs <- sapply(seq(0, 1, length.out = res + 1), FUN=CalculateTPR, preds, truth)
  fprs <- sapply(seq(0, 1, length.out = res + 1), FUN=CalculateFPR, preds, truth)
  Distance <- ((1-tprs)+fprs)
  ind <- which(Distance == min(Distance))
  best <- c(seq(0, 1, length.out = res + 1)[ind],tprs[ind], fprs[ind])
  # Prints the best threshold, the associated TPR and FPR
  print(best)
  
  print(
    ggplot(data=data.frame(fprs, tprs)) +
      geom_line(aes(x=fprs, y=tprs), color="blue") +
      geom_abline(aes(slope=1, intercept=0))
  )
  negative.classifications <-
    sapply(preds[truth],
           FUN = function(threshold) { CalculateFPR(threshold, preds, truth) })
  positive.auc <- sum(1 - negative.classifications) / sum(truth)  
  return(positive.auc)
}

# True Classifications:
bestModel.true <- image4.sure$label==1

# Evaluate our model
EvaluateModel(bestModel.pred, bestModel.true)
