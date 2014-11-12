library('e1071')
library('dplyr')
library('parallel')
library('foreach')
library('doParallel')
library('rlecuyer')


# Change the variable directory to the path in your system
directory = "/accounts/class/s215a/s215a-4/215A/Lab4/RScript/"
#directory = "/Users/gorripaty/Desktop/UCB_Fall14/STATS215A/Lab/Lab4/RScript"

setwd(file.path(Sys.getenv("GIT_REPO_LOC"),
                directory))
# Get the data for three images

image1 <- read.table('image1.txt', header=F)
image2 <- read.table('image2.txt', header=F)
image3 <- read.table('image3.txt', header=F)

# Add informative column names.
collabs <- c('y','x','label','NDAI','SD',
             'CORR','DF','CF','BF','AF','AN')
names(image1) <- collabs
names(image2) <- collabs
names(image3) <- collabs

# Stacking image 1 and 2 together to form the training data
image.train <- rbind(image1, image2)
image.test <- image3


# Removing the not-sure data
image.train <- filter(image.train, label != 0) %>% select(-c(x, y))
image.test <- filter(image.test, label != 0) %>% select(-c(x, y))

# Test Labels
test.label <- image.test$label

# Number of cores alloted for running the code and regstering these cores
ncores <- 10
registerDoParallel(ncores)

# To ensure that random number generator creates random numbers in parallel processes
RNGkind("L'Ecuyer-CMRG")

parallel.process <- foreach(i = 1:10) %dopar% {  # Running the code parallely
  
  # Noting the start-time of the processesing
  start.time <- Sys.time()
  
  # Sampling 5000 random rows from the training and testing set
  # For reproducable research
  set.seed(i)
  train.sample <- image.train[sample(1:nrow(image.train),
                                     (5000 + i*1000), replace=FALSE),]
  # Tuning the SVM
  formula <- as.formula(factor(label) ~ . )
  svm.tune <- tune.svm(formula, data = train.sample,
                       gamma = 10^(-5:4), cost = 10^(-2:5))
  # Best SVM model
  svm.model <- svm.tune$best.model
  
  # Predicting on test set
  svm.predict <- predict(svm.model, image.test[,-1])
  
  # Duration of the SVM convergence
  duration <- Sys.time() - start.time
  
  # Confusion matrix for the SVM prediction
  confusion.matrix <- table(pred = svm.predict, true = test.label)
  
  # Outputting the confusion table and time for different sample sizes
  file.out <- paste("confusion_time", i, ".csv", sep = "")   
  con <- file(file.out, "wt")
  write.table(file.out, file = con, row.names=FALSE, 
              col.names=FALSE, append = TRUE)
  write.table(duration, file=con, sep=";", dec=",", row.names=FALSE, 
              col.names=FALSE, append=TRUE)
  write.table(confusion.matrix, file=con, sep=";", dec=",", row.names=FALSE, 
              col.names=TRUE, append = TRUE)
  close(con)
  return(list(confusion.matrix, duration))
}