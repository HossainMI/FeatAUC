# Forward step-wise selection based on AUC value.

FeatAUC = function(dependent, data, ind_num) {
  
  auc_values = numeric()
  best_model = 0
  best_combination = NULL
  
  for (x_count in 1:ind_num) {
    x_combinations = combn(names(data)[-1], x_count)
    
    for (i in 1:ncol(x_combinations)) {
      predictors = c(dependent,
                      as.character(x_combinations[, i]))
      
      formula = as.formula(paste(dependent, "~", paste(predictors[-1],
                                                        collapse = " + ")))
      model = glm(formula,
                   data = data,
                   family = "binomial")
      
      pred = predict(model,
                      type = "response")
      
      auc = pROC::roc(data[[dependent]], pred)$auc
      
      auc_values = c(auc_values, auc)
      
      cat("AUC for", paste(predictors[-1],
                           collapse = " + "), ":", auc, "\n")
      
      cat("Selected X variables:", paste(as.character(x_combinations[, i]),
                                         collapse = ", "), "\n\n")
      if (auc == max(auc_values)) {
        best_model = length(auc_values)
        best_combination = as.character(x_combinations[,i])
      }
    }
  }
  
  return(list(auc_values,
              best_model,
              best_combination))
}

# Demo analysis

library(Hmisc)

library(pROC)

data = spss.get("FeatAUC_data.sav",
                use.value.labels = TRUE)

data_s = data[, c(1,3,4,5,6,7,8,9)]

auc_values = FeatAUC(dependent = "EANC", data = data_s, ind_num = 7)

cat("Best model:", auc_values[[2]],
    "with AUC value:", max(auc_values[[1]]),
    "with combination:", paste(auc_values[[3]],
          collapse = " + "), "\n")

