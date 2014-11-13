# Stat215A
# Lab4
library(dplyr)
library(ggplot2)

# Helper functions
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Evaluation functions
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

# Writing a function to split the image into 4 parts for CV
SplitImage4 <- function(image){
  # Divides an image into 4 parts.
  #
  #Args:
  # image: A dataframe containing the coordinates and properties of the image
  #
  # Returns:
  #   Four Dataframes which are the four parts of the image.
  
  # Finding the mid-point values of the coordinates
  y.mid <- (min(image$y) + max(image$y))/2
  x.mid <- (min(image$x) + max(image$x))/2
  
  image.part1 <- filter(image, y > y.mid, x < x.mid)
  image.part2 <- filter(image, y < y.mid, x < x.mid)
  image.part3 <- filter(image, y > y.mid, x > x.mid)
  image.part4 <- filter(image, y < y.mid, x > x.mid)
  
  return(list(image.part1, image.part2, image.part3, image.part4))
}


# Import three images
image1 <- read.table('image1.txt', header=F, stringsAsFactors=FALSE)
image2 <- read.table('image2.txt', header=F, stringsAsFactors=FALSE)
image3 <- read.table('image3.txt', header=F, stringsAsFactors=FALSE)

collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image1) <- collabs
names(image2) <- collabs
names(image3) <- collabs

# The expert label plots.
png("part1.png")
ggplot(image1) + geom_point(aes(x=x, y=y, color=factor(label))) + labs(title = "Image1 labels")
dev.off()
png("part2.png")
ggplot(image2) + geom_point(aes(x=x, y=y, color=factor(label))) + labs(title = "Image2 labels")
dev.off()
png("part3.png")
ggplot(image3) + geom_point(aes(x=x, y=y, color=factor(label))) + labs(title = "Image3 labels")
dev.off()

# Different angle plots
# Visually
p1 <- ggplot(image1) + geom_point(aes(x=x, y=y, color=DF))
p2 <- ggplot(image1) + geom_point(aes(x=x, y=y, color=CF))
p3 <- ggplot(image1) + geom_point(aes(x=x, y=y, color=BF))
p4 <- ggplot(image1) + geom_point(aes(x=x, y=y, color=AF))
p5 <- ggplot(image1) + geom_point(aes(x=x, y=y, color=AN))
p6 <- ggplot(image1) + geom_point(aes(x=x, y=y, color=SD))
png("multiangleimage1.png")
multiplot(p1, p2, p3, p4, p5, p6, cols=2)
dev.off()

# Quantitatively
p1 <- ggplot(image1) + geom_density(aes(x=AN, group=factor(label), fill=factor(label)), alpha=0.5)
p2 <- ggplot(image1) + geom_density(aes(x=AF, group=factor(label), fill=factor(label)), alpha=0.5)
p3 <- ggplot(image1) + geom_density(aes(x=BF, group=factor(label), fill=factor(label)), alpha=0.5)
p4 <- ggplot(image1) + geom_density(aes(x=CF, group=factor(label), fill=factor(label)), alpha=0.5)
p5 <- ggplot(image1) + geom_density(aes(x=DF, group=factor(label), fill=factor(label)), alpha=0.5)
png("multidensity.png")
multiplot(p1, p2, p3, p4, p5, cols=2)
dev.off()

# Class conditional densities.
p1 <- ggplot(image1) + geom_density(aes(x=NDAI, group=factor(label), fill=factor(label)), alpha=0.5)
p2 <- ggplot(image1) + geom_density(aes(x=CORR, group=factor(label), fill=factor(label)), alpha=0.5)
p3 <- ggplot(image1) + geom_density(aes(x=SD, group=factor(label), fill=factor(label)), alpha=0.5)
png("densityfeature.png")
multiplot(p1, p2, p3, cols=3)
dev.off()

# discard all unsure rows (label == 0) for the purposes of classification
image1.sure <- image1[which(image1$label != 0), ]
image1.sure$label <- factor(image1.sure$label)
image2.sure <- image2[which(image2$label != 0), ]
image2.sure$label <- factor(image2.sure$label)
image3.sure <- image3[which(image3$label != 0), ]
image3.sure$label <- factor(image3.sure$label)


# Unsupervised K-means Clustering on the plots
# Image 1
res <- kmeans(image1.sure$NDAI, centers=2)
ggplot(image1.sure) + geom_density(aes(x=NDAI, fill=factor(res$cluster)), alpha=0.5)

# Change labels:
res$cluster[which(res$cluster==1)]=-1
res$cluster[which(res$cluster==2)]=1

kmeans.pred <- res$cluster
kmeans.truth <- image1.sure$label==1

# Compute TPR, FPR
CalculateTPR(0.1, kmeans.pred, kmeans.truth)
CalculateFPR(0.1, kmeans.pred, kmeans.truth)

# Image 2
res <- kmeans(image2$NDAI, centers=2)
ggplot(image2) + geom_density(aes(x=NDAI, fill=factor(res$cluster)), alpha=0.5)

# Change labels:
res$cluster[which(res$cluster==1)]=-1
res$cluster[which(res$cluster==2)]=1

kmeans.pred <- res$cluster
kmeans.truth <- image2.sure$label==1

# Compute TPR, FPR
CalculateTPR(0.1, kmeans.pred, kmeans.truth)
CalculateFPR(0.1, kmeans.pred, kmeans.truth)

# Image 3
res <- kmeans(image3$NDAI, centers=2)
ggplot(image2) + geom_density(aes(x=NDAI, fill=factor(res$cluster)), alpha=0.5)
# Change labels:
res$cluster[which(res$cluster==1)]=-1
res$cluster[which(res$cluster==2)]=1

kmeans.pred <- res$cluster
kmeans.truth <- image3.sure$label==1

# Compute TPR, FPR
CalculateTPR(0.1, kmeans.pred, kmeans.truth)
CalculateFPR(0.1, kmeans.pred, kmeans.truth)

# Random forest variable selection
library(randomForest)
imageall.sure <- rbind(image1.sure, image2.sure, image3.sure)
imageall.sure$label <- factor(imageall.sure$label)
imageall.rf <- randomForest(label~NDAI+SD+CORR+DF+CF+BF+AF+AN,
                            data=imageall.sure, mtry=3, ntree=300,
                            importance=TRUE)
importance(imageall.rf)
jpeg("varImp.jpeg")
varImpPlot(imageall.rf)
dev.off()
