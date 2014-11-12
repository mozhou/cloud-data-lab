setwd(file.path(Sys.getenv("GIT_REPO_LOC"),
                "/Users/gorripaty/Desktop/UCB_Fall14/STATS215A/Lab/Lab4/RScript/"))
library(ggplot2)
library(dplyr)

# Writing a function for merging dataframes 
# (idea borrowed from http://novicemetrics.blogspot.com/2011/04/merging-multiple-data-files-into-one.html)
multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=F)})

}

# Merging all the files in the folder SVM (which consists of the SVM output csv files for multiple sample sizes)
# Ensure to keep all the individual output files from SVM seperately in one folder
all <- multmerge("/Users/gorripaty/Desktop/UCB_Fall14/STATS215A/Lab/Lab4/RScript/SVM/")

# Creating an empty dataframe
SVM.summary <- data.frame(sample.size=character(),
                 TP = character(), 
                 FP = character(), 
                 FN = character(), 
                 TN = character(),
                 time = character(), stringsAsFactors=F) 

# Filling up the dataframe with the summary information from different sample sizes
for (i in 1:10){
  newRow <- c((5+i)*1000, strsplit(as.character(all[[i]][[1]][5]), ";")[[1]][2], 
              strsplit(as.character(all[[i]][[1]][5]), ";")[[1]][1], 
              strsplit(as.character(all[[i]][[1]][4]), ";")[[1]][2], 
              strsplit(as.character(all[[i]][[1]][4]), ";")[[1]][1], 
              as.character(all[[i]][[1]][2]))
  SVM.summary[i,] <- newRow
}

# Changing the type of the columns
SVM.summary <- mutate(SVM.summary, sample.size = as.numeric(sample.size), 
                      TP = as.numeric(TP), FP = as.numeric(FP), FN = as.numeric(FN),
                      TN = as.numeric(TN), time = as.numeric(time))

# Plotting the different curves for SVM
ggplot(SVM.summary, aes(x = sample.size, y = time)) + 
  geom_point() + xlab("Sample Size") + ylab("Convergence Time")


SVM.summary <- mutate(SVM.summary, TPR = TP/(TP+FN), FPR = FP/(FP+TN), accuracy = (TP + TN)/(TP + TN + FP + FN), FDR = FP/( FP + TP))

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
# Taken from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
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

accuracy <- ggplot(SVM.summary, aes(sample.size, accuracy)) + geom_point()
TPR <- ggplot(SVM.summary, aes(sample.size, TPR)) + geom_point()
FPR <- ggplot(SVM.summary, aes(sample.size, FPR)) + geom_point()
FDR <- ggplot(SVM.summary, aes(sample.size, FDR)) + geom_point()
  
multiplot(accuracy, TPR, FPR, FDR, cols = 2)  
