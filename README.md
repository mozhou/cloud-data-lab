cloud-data-lab
==============

This repository contains the necessary files to reproduce the cloud data lab.

The lab consists of the .r code used, the raw .tex file used to generate the report and the final pdf report.

The .r code used has four parts:
1. EDA.r contains the code used for exploratory data analysis and the helper functions:
  a) Multiplot to plot multi graphs on one plot.
  b) EvaluateModel, CalculateFPR, CalculateTPR for assessing the classification models.
  c) SplitImage4 that splits a given image into 4 parts by cutting in half horizontally and vertically.

2. classification.r contains the code for each classification model, including logistic regression, LDA, QDA, Semi-supervised EM, SVM and the ensemble model . It also contains the assessment of the models using CV, AIC, ROC and AUC. The file also includes the miss classification error analysis.

3. SVM_parallel.R and postProcessSVM.R contains code to use SVM for prediction.

3. bestModel.r contains the code to use our bestModel to predict cloud/no cloud for a new set of image data. It also provides assessment of fit using ROC and AUC.


