# FeatAUC
The FeatAUC function is designed for feature selection based on the area under the receiver operating characteristic (ROC) curve (AUC) in logistic regression models. It iterates through different combinations of independent variables to find the model with the highest AUC value.

Parameters:
  dependent: The name of the dependent variable (response variable).
  data: The data frame containing the dependent variable and independent variables.
  ind_num: The maximum number of independent variables to consider in the feature selection process.

Output:
The function returns a list containing the following elements:
  A vector of AUC values for each model.
  The index of the best model (with the highest AUC value).
  A string describing the best model and its AUC value.
  The combination of predictors for the best model.
