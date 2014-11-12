# The 0-1 classification methods and assessments
library(Rmixmod)
library(MASS)
library(ggplot2)
library(dplyr)

# Get the data for three images
image1 <- read.table('image1.txt', header=F)
image2 <- read.table('image2.txt', header=F)
image3 <- read.table('image3.txt', header=F)

# Add informative column names.
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image1) <- collabs
names(image2) <- collabs
names(image3) <- collabs

# discard all unsure rows (label == 0) for the purposes of classification
image1.sure <- image1[which(image1$label != 0), ]
image1.sure$label <- factor(image1.sure$label)
image2.sure <- image2[which(image2$label != 0), ]
image2.sure$label <- factor(image2.sure$label)
image3.sure <- image3[which(image3$label != 0), ]
image3.sure$label <- factor(image3.sure$label)

# partition the dataset into folds for training and testing
# train on 2 images and test on the last one
fold1 <- rbind(image1.sure, image2.sure)
fold2 <- rbind(image1.sure, image3.sure)
fold3 <- rbind(image2.sure, image3.sure)

# Try different CV folds
image1.part.list <- SplitImage4(image1.sure)
image2.part.list <- SplitImage4(image2.sure)
image3.part.list <- SplitImage4(image3.sure)

# Accessing the four parts of the image
image1.part1 <- image1.part.list[[1]]
image1.part2 <- image1.part.list[[2]]
image1.part3 <- image1.part.list[[3]]
image1.part4 <- image1.part.list[[4]]

image2.part1 <- image2.part.list[[1]]
image2.part2 <- image2.part.list[[2]]
image2.part3 <- image2.part.list[[3]]
image2.part4 <- image2.part.list[[4]]

image3.part1 <- image3.part.list[[1]]
image3.part2 <- image3.part.list[[2]]
image3.part3 <- image3.part.list[[3]]
image3.part4 <- image3.part.list[[4]]

CV <- rbind( image1.part1, 
            # image1.part2, 
             image1.part3, 
             image1.part4,
             #image2.part1, 
             image2.part2, 
             image2.part3,
             #image2.part4, 
             image3.part1,
             image3.part2, 
             #image3.part3, 
             image3.part4)

logit11 <- glm(label ~ NDAI+CORR+SD, data=CV, family="binomial")
logit.pred11 <- predict(logit11, newdata=rbind(image1.part2, image2.part1, image2.part4, image3.part3), type="response")
logit.true11 <- rbind(image1.part2, image2.part1, image2.part4, image3.part3)$label==1
EvaluateModel(logit.pred11, logit.true11)
# The result for more batches are not stable, thus not a good idea to split an image into several batches.

##### let's fit some models
# 1. Logistic Regression

# Subset of predictors:
logit11 <- glm(label ~ NDAI+CORR+SD, data=fold1, family="binomial")
logit12 <- glm(label ~ NDAI+CORR+SD, data=fold2, family="binomial")
logit13 <- glm(label ~ NDAI+CORR+SD, data=fold3, family="binomial")

# probabilities:
logit.pred11 <- predict(logit11, newdata=image3.sure, type="response")
logit.pred12 <- predict(logit12, newdata=image2.sure, type="response")
logit.pred13 <- predict(logit13, newdata=image1.sure, type="response")

# True Classifications:
logit.true11 <- image3.sure$label==1
logit.true12 <- image2.sure$label==1
logit.true13 <- image1.sure$label==1

# Evaluate
# AIC
AIC(logit11)
# 72739.29
AIC(logit12)
# 81264.35
AIC(logit13)
# 72984.58
AIC(logitall.1)
# 116338

# ROC
EvaluateModel(logit.pred11, logit.true11)
### [1] 0.1620000 0.9499623 0.2445485
### [1] 0.8970348
EvaluateModel(logit.pred12, logit.true12)
### [1] 0.3980000 0.9803647 0.1038897
### [1] 0.955428
EvaluateModel(logit.pred13, logit.true13)
### [1] 0.2090000 0.9131454 0.1551164
### [1] 0.9356495
# Average AUC: 0.929371

# Joint Evaluation
EvaluateModel(c(logit.pred11, logit.pred12, logit.pred13), c(logit.true11, logit.true12, logit.true13))
### Best threshold[1] 0.2520000 0.9402576 0.1505115
### AUC :0.9396199

# Another subset of predictors:
logit21 <- glm(label ~ NDAI, data=fold1, family="binomial")
logit22 <- glm(label ~ NDAI, data=fold2, family="binomial")
logit23 <- glm(label ~ NDAI, data=fold3, family="binomial")
logitall.2 <- glm(label ~ NDAI, data=imageall.sure, family="binomial")

# probabilities:
logit.pred21 <- predict(logit21, newdata=image3.sure, type="response")
logit.pred22 <- predict(logit22, newdata=image2.sure, type="response")
logit.pred23 <- predict(logit23, newdata=image1.sure, type="response")

# True Classifications:
logit.true11 <- image3.sure$label==1
logit.true12 <- image2.sure$label==1
logit.true13 <- image1.sure$label==1

# Evaluation
# AIC
AIC(logit21)
# 79758.82
AIC(logit22)
# 85215.58
AIC(logit23)
# 92612.87
AIC(logitall.2)
# 130234.2

# Individual evaluations:
EvaluateModel(logit.pred21, logit.true11)
### [1] 0.1800000 0.9553286 0.2616734
### [1] 0.8812808
EvaluateModel(logit.pred22, logit.true12)
### [1] 0.3190000 0.9870117 0.1124481
### [1] 0.9421621
EvaluateModel(logit.pred23, logit.true13)
### [1] 0.45200000 0.96448635 0.09481426
### [1] 0.9571441
# Average AUC: 0.92686233

# Joint evaluation
EvaluateModel(c(logit.pred21, logit.pred22, logit.pred23), c(logit.true11, logit.true12, logit.true13))
### [1] 0.3230000 0.9573480 0.1475291
### [1] 0.930507


# Another set of predictors
logit31 <- glm(label ~ NDAI + CORR + SD + AN, data=fold1, family="binomial")
logit32 <- glm(label ~ NDAI + CORR + SD + AN, data=fold2, family="binomial")
logit33 <- glm(label ~ NDAI + CORR + SD + AN, data=fold3, family="binomial")
logitall.3 <- glm(label ~ NDAI + CORR + SD + AN, data=imageall.sure, family="binomial")

# probabilities:
logit.pred31 <- predict(logit31, newdata=image3.sure, type="response")
logit.pred32 <- predict(logit32, newdata=image2.sure, type="response")
logit.pred33 <- predict(logit33, newdata=image1.sure, type="response")

# True Classifications:
logit.true11 <- image3.sure$label == 1
logit.true12 <- image2.sure$label == 1
logit.true13 <- image1.sure$label == 1

# Individual evaluations:
EvaluateModel(logit.pred31, logit.true11)
### 0.0820000 0.9449727 0.2404006
### 0.8988472
EvaluateModel(logit.pred32, logit.true12)
### 0.3590000 0.9834462 0.1093466
### 0.9521472
EvaluateModel(logit.pred33, logit.true13)
### 0.2640000 0.9013238 0.1576537
### 0.9319251
# Average AUC: 0.9276398

AIC(logit31)
# 68693.97
AIC(logit32)
# 81164.8
AIC(logit33)
# 71237.18
AIC(logitall.3)
# 114806.1

# Joint evaluation
EvaluateModel(c(logit.pred31, logit.pred32, logit.pred33), c(logit.true11, logit.true12, logit.true13))
### [1] 0.2480000 0.9208580 0.1556657
### [1] 0.9329753

##############
# Pick the first set of predictors NDAI+SD+CORR based on 
# both pooled and average AUC (higher is better)
# Pick the third set of predictors NDAI+SD+CORR+AN based on lowest AIC, but notice AIC of the third set is not a lot smaller than AIC of the first set.
##############

# 2. LDA
lda11 <- lda(label ~ NDAI+SD+CORR, data = fold1)
lda12 <- lda(label ~ NDAI+SD+CORR, data = fold2)
lda13 <- lda(label ~ NDAI+SD+CORR, data = fold3)

lda.pred11 <- predict(lda11, newdata=image3.sure)$posterior[, "1"]
lda.pred12 <- predict(lda12, newdata=image2.sure)$posterior[, "1"]
lda.pred13 <- predict(lda13, newdata=image1.sure)$posterior[, "1"]

# Evaluate
lda.true11 <- image3.sure$label==1
lda.true12 <- image2.sure$label==1
lda.true13 <- image1.sure$label==1

# individual models:
EvaluateModel(lda.pred11, lda.true11)
### [1] 0.1010000 0.9509509 0.2431560
### 0.8953017
EvaluateModel(lda.pred12, lda.true12)
### [1] 0.3890000 0.9878521 0.1072245
### 0.9543476
EvaluateModel(lda.pred13, lda.true13)
### [1] 0.2100000 0.9476821 0.1304365
### 0.9511885
# Average AUC: 0.9336126

# Joint Evaluation
EvaluateModel(c(lda.pred11, lda.pred12, lda.pred13), c(lda.true11, lda.true12, lda.true13))
### [1] 0.2510000 0.9475926 0.1414149
## AUC 0.9397012

# try different subset of predictors on LDA:
lda21 <- lda(label ~ NDAI, data = fold1)
lda22 <- lda(label ~ NDAI, data = fold2)
lda23 <- lda(label ~ NDAI, data = fold3)

lda.pred21 <- predict(lda21, newdata=image3.sure)$posterior[, "1"]
lda.pred22 <- predict(lda22, newdata=image2.sure)$posterior[, "1"]
lda.pred23 <- predict(lda23, newdata=image1.sure)$posterior[, "1"]

# Evaluate
lda.true11 <- image3.sure$label==1
lda.true12 <- image2.sure$label==1
lda.true13 <- image1.sure$label==1


EvaluateModel(lda.pred21, lda.true11)
### [1] 0.1410000 0.9556110 0.2619104
### 0.8812808
EvaluateModel(lda.pred22, lda.true12)
### [1] 0.3120000 0.9871899 0.1126114
### 0.9421621
EvaluateModel(lda.pred23, lda.true13)
### [1] 0.49600000 0.96429095 0.09465567
### 0.9571441
# Average AUC: 0.92686

# Joint Evaluation
EvaluateModel(c(lda.pred21, lda.pred22, lda.pred23), c(lda.true11, lda.true12, lda.true13))
### [1] 0.3170000 0.9603117 0.1490321
### AUC: 0.9294839

# try different subset of predictors on LDA:
lda31 <- lda(label ~ NDAI+SD+CORR+AN, data = fold1)
lda32 <- lda(label ~ NDAI+SD+CORR+AN, data = fold2)
lda33 <- lda(label ~ NDAI+SD+CORR+AN, data = fold3)

lda.pred31 <- predict(lda31, newdata=image3.sure)$posterior[, "1"]
lda.pred32 <- predict(lda32, newdata=image2.sure)$posterior[, "1"]
lda.pred33 <- predict(lda33, newdata=image1.sure)$posterior[, "1"]

# Evaluate
lda.true11 <- image3.sure$label==1
lda.true12 <- image2.sure$label==1
lda.true13 <- image1.sure$label==1


EvaluateModel(lda.pred31, lda.true11)
# [1] 0.0290000 0.9475146 0.2398080
# [1] 0.8987907
EvaluateModel(lda.pred32, lda.true12)
# [1] 0.3490000 0.9861458 0.1106991
# [1] 0.9484622
EvaluateModel(lda.pred33, lda.true13)
# [1] 0.2540000 0.9255532 0.1460175
# [1] 0.9422423
# Average AUC: 0.92983

# Joint Evaluation
EvaluateModel(c(lda.pred31, lda.pred32, lda.pred33), c(lda.true11, lda.true12, lda.true13))
# [1] 0.2370000 0.9182648 0.1458058
# [1] 0.9307121

####################
# Pick NDAI+SD+CORR based on both average and pooled AUC
# AIC cannot be easily calculated for LDA models (is this true?)
####################

# 3. QDA
# Pick a set of predictors for qda
qda11 <- qda(label ~ NDAI+SD+CORR, data = fold1)
qda12 <- qda(label ~ NDAI+SD+CORR, data = fold2)
qda13 <- qda(label ~ NDAI+SD+CORR, data = fold3)

qda.pred11 <- predict(qda11, newdata=image3.sure)$posterior[, "1"]
qda.pred12 <- predict(qda12, newdata=image2.sure)$posterior[, "1"]
qda.pred13 <- predict(qda13, newdata=image1.sure)$posterior[, "1"]

# Evaluate
qda.true11 <- image3.sure$label==1
qda.true12 <- image2.sure$label==1
qda.true13 <- image1.sure$label==1

EvaluateModel(qda.pred11, qda.true11)
### [1] 0.0150000 0.9448786 0.2601624
### [1] 0.8877764
EvaluateModel(qda.pred12, qda.true12)
### [1] 0.41300000 0.99401518 0.08866191
### [1] 0.9707214
EvaluateModel(qda.pred13, qda.true13)
### [1] 0.2200000 0.9628743 0.0938231
### [1] 0.9593713
# Average AUC: 0.93929

# Joint Evaluation
EvaluateModel(c(qda.pred11, qda.pred12,qda.pred13), c(qda.true11,qda.true12, qda.true13))
### [1] 0.1400000 0.9465060 0.1338055
### AUC: 0.9473045

# Another subset of predictors for QDA
qda21 <- qda(label ~ NDAI, data = fold1)
qda22 <- qda(label ~ NDAI, data = fold2)
qda23 <- qda(label ~ NDAI, data = fold3)

qda.pred21 <- predict(qda21, newdata=image3.sure)$posterior[, "1"]
qda.pred22 <- predict(qda22, newdata=image2.sure)$posterior[, "1"]
qda.pred23 <- predict(qda23, newdata=image1.sure)$posterior[, "1"]

# Evaluate
qda.true11 <- image3.sure$label==1
qda.true12 <- image2.sure$label==1
qda.true13 <- image1.sure$label==1

EvaluateModel(qda.pred21, qda.true11)
# [1] 0.0390000 0.9559405 0.2623252
# [1] 0.8818084
EvaluateModel(qda.pred22, qda.true12)
# [1] 0.2790000 0.9871899 0.1126114
# [1] 0.9421621
EvaluateModel(qda.pred23, qda.true13)
# [1] 0.43300000 0.96438865 0.09469532
# [1] 0.9596373
# Average AUC: 0.927868

# Joint Evaluation
EvaluateModel(c(qda.pred21, qda.pred22, qda.pred23), c(qda.true11, qda.true12, qda.true13))
# [1] 0.2570000 0.9520628 0.1411788
# [1] 0.9301948

# Another set of predictors
qda31 <- qda(label ~ NDAI+SD+CORR+AN, data = fold1)
qda32 <- qda(label ~ NDAI+SD+CORR+AN, data = fold2)
qda33 <- qda(label ~ NDAI+SD+CORR+AN, data = fold3)

qda.pred31 <- predict(qda31, newdata=image3.sure)$posterior[, "1"]
qda.pred32 <- predict(qda32, newdata=image2.sure)$posterior[, "1"]
qda.pred33 <- predict(qda33, newdata=image1.sure)$posterior[, "1"]

# Evaluate
qda.true11 <- image3.sure$label==1
qda.true12 <- image2.sure$label==1
qda.true13 <- image1.sure$label==1

EvaluateModel(qda.pred31, qda.true11)
# [1] 0.0130000 0.9480795 0.3229735
# [1] 0.8764701
EvaluateModel(qda.pred32, qda.true12)
# [1] 0.19900000 0.98935466 0.09915582
# [1] 0.9607259
EvaluateModel(qda.pred33, qda.true13)
# [1] 0.1320000 0.9693225 0.1386235
# [1] 0.9531734
# Average AUC: 0.930123

# Joint Evaluation
EvaluateModel(c(qda.pred31, qda.pred32, qda.pred33), c(qda.true11, qda.true12, qda.true13))
# [1] 0.1210000 0.9443079 0.1559175
# [1] 0.940264

###################
# Pick label~NDAI+SD+CORR based on both average and pooled AUC
###################

# 4. Semi-supervised EM
library(Rmixmod)
image1.label <- as.numeric(as.character(image1.sure$label))
image2.label <- as.numeric(as.character(image2.sure$label))
image3.label <- as.numeric(as.character(image3.sure$label))

image1.label[image1.label==-1] <- 2
image2.label[image2.label==-1] <- 2
image3.label[image3.label==-1] <- 2

# Fold1
lab = c(rep(NA,54996), image1.label, image2.label)
x=rbind(image3.sure[,4:6], image1.sure[,4:6], image2.sure[,4:6])

res <- mixmodCluster(data=x , nbCluster=2, knownLabels = lab)
mixmodPred.1 <- res@results[[1]]@proba[1:(416122/2)]
EM.true11 <- image3.sure$label==1
EvaluateModel(mixmodPred.1[1:54996], EM.true11)
### 0.0660000 0.9644135 0.2661768
### AUC: 0.8885992
class.1 <- rep(2, 54996)
class.1[which(mixmodPred.1[1:54996] > 0.066)] <- 1

# Fold2
lab = c(rep(NA,82148), image1.label, image3.label)
x=rbind(image2.sure[,4:6], image3.sure[,4:6], image1.sure[,4:6])

res <- mixmodCluster(data=x , nbCluster=2, knownLabels = lab)
mixmodPred.2 <- res@results[[1]]@proba[1:(416122/2)]
EM.true12 <- image2.sure$label==1
EvaluateModel(mixmodPred.2[1:82148], EM.true12)
###  0.2060000 0.9912647 0.1047293
###  AUC: 0.9600006
class.2 <- rep(2, 82148)
class.2[which(mixmodPred.2[1:82148] > 0.2060000)] <- 1

# Fold3
lab = c(rep(NA,70917), image2.label, image3.label)
x=rbind(image1.sure[,4:6], image2.sure[,4:6], image3.sure[,4:6])

res <- mixmodCluster(data=x , nbCluster=2, knownLabels = lab)
mixmodPred.3 <- res@results[[1]]@proba[1:(416122/2)]
EM.true13 <- image1.sure$label==1
EvaluateModel(mixmodPred.3[1:70917], EM.true13)
### [1] 0.1420000 0.9627766 0.1120208
### AUC:  0.9541129
class.3 <- rep(2, 70917)
class.3[which(mixmodPred.3[1:70917] > 0.1420000)] <- 1

# Joint Evaluation
EvaluateModel(c(mixmodPred.1[1:82148],mixmodPred.2[1:54996],mixmodPred.3[1:70917]), c(EM.true11, EM.true12, EM.true13))
### 0.1980000 0.9368370 0.1240085
### AUC: 0.9259815

# Ensemble based on best logistic and QDA models

# get probabilities for already-fitted LDA and QDA models
lda11.predfold1 <- predict(lda11, newdata=fold1)$posterior[, "1"]
qda11.predfold1 <- predict(qda11, newdata=fold1)$posterior[, "1"]
lda12.predfold2 <- predict(lda12, newdata=fold2)$posterior[, "1"]
qda12.predfold2 <- predict(qda12, newdata=fold2)$posterior[, "1"]
lda13.predfold3 <- predict(lda13, newdata=fold3)$posterior[, "1"]
qda13.predfold3 <- predict(qda13, newdata=fold3)$posterior[, "1"]

# create data frames of label(response) and probabilities(features for the ensemble)
fold1prob <- data.frame(fold1$label, logit11$fitted.values, 
                        lda11.predfold1, qda11.predfold1)
fold2prob <- data.frame(fold2$label, logit12$fitted.values, 
                        lda12.predfold2, qda12.predfold2)
fold3prob <- data.frame(fold3$label, logit13$fitted.values, 
                        lda13.predfold3, qda13.predfold3)

# create column names to pass into glm()
ensemblefeatures <- c("label", "logit_prob", "LDA_prob", "QDA_prob")
names(fold1prob) <- ensemblefeatures
names(fold2prob) <- ensemblefeatures
names(fold3prob) <- ensemblefeatures


# predict probabilities using ensemble model
ensemble1.pred <- 1/2*logit.pred11+1/2*qda.pred11
ensemble2.pred <- 1/2*logit.pred12+1/2*qda.pred12
ensemble3.pred <- 1/2*logit.pred13+1/2*qda.pred13


# evaluate performance of ensemble models
EvaluateModel(ensemble1.pred, logit.true11)
# [1] 0.0890000 0.9558464 0.2517777
# [1] 0.8949305
EvaluateModel(ensemble2.pred, logit.true12)
# [1] 0.43800000 0.99151938 0.09197332
# [1] 0.9639261
EvaluateModel(ensemble3.pred, logit.true13)
# [1] 0.2460000 0.9584778 0.1037545
# [1] 0.9573461

EvaluateModel(c(ensemble1.pred, ensemble2.pred, ensemble3.pred),
              c(logit.true11, logit.true12, logit.true13))
# [1] 0.2460000 0.9444314 0.1286355
# [1] 0.9470624

#############

# Misclassification Analysis
# Let's use the classification result from semi-supervised EM
# Train set: Image3
misClass <- which(class.1 != image3.label)
misClassification.1 <- image3.sure[misClass,]
ggplot(misClassification.1) + geom_point(aes(x=x, y=y))
ggplot(image3) + geom_point(aes(x=x, y=y, color=factor(label)))
ggplot(image3) + geom_point(aes(x=x, y=y, color=AN))

# Plot the density of misclassification
ggplot(misClassification.1) + geom_density(aes(x=NDAI, group=factor(label), fill=factor(label)), alpha=0.5)
ggplot(image3.sure) + geom_density(aes(x=NDAI, group=factor(label), fill=factor(label)), alpha=0.5)

ggplot(misClassification.1) + geom_density(aes(x=CORR, group=factor(label), fill=factor(label)), alpha=0.5)
ggplot(image3.sure) + geom_density(aes(x=CORR, group=factor(label), fill=factor(label)), alpha=0.5)

ggplot(misClassification.1) + geom_density(aes(x=SD, group=factor(label), fill=factor(label)), alpha=0.5)
ggplot(image3.sure) + geom_density(aes(x=SD, group=factor(label), fill=factor(label)), alpha=0.5)


# Train set: Image2
misClass <- which(class.2 != image2.label)
misClassification.2 <- image2.sure[misClass,]
ggplot(misClassification.2) + geom_point(aes(x=x, y=y))
ggplot(image2) + geom_point(aes(x=x, y=y, color=factor(label)))
ggplot(image2) + geom_point(aes(x=x, y=y, color=AN))

# Plot the density of misclassification
ggplot(misClassification.2) + geom_density(aes(x=CORR, group=factor(label), fill=factor(label)), alpha=0.5)
ggplot(image2.sure) + geom_density(aes(x=CORR, group=factor(label), fill=factor(label)), alpha=0.5)

# Train set: Image1
misClass <- which(class.3 != image1.label)
misClassification.3 <- image1.sure[misClass,]
ggplot(misClassification.3) + geom_point(aes(x=x, y=y))
ggplot(image1) + geom_point(aes(x=x, y=y, color=factor(label)))
ggplot(image1) + geom_point(aes(x=x, y=y, color=AN))

# Plot the density of misclassification
ggplot(misClassification.3) + geom_density(aes(x=NDAI, group=factor(label), fill=factor(label)), alpha=0.5)
ggplot(image1.sure) + geom_density(aes(x=NDAI, group=factor(label), fill=factor(label)), alpha=0.5)
