# Feature selection for count dependent variable

FeatCount = function(dependent, data, ind_num) {
  
  auc_results = data.frame(Dependent = numeric(),
                            Independent = character(),
                            AUC = numeric())
  
  for (value in sort(unique(data[[dependent]]))) {
    
    Y_bin = ifelse(data[[dependent]] == value, 1, 0)
    
    for (i in 1:ind_num) {
      
      independent = names(data)[i+1]
      
      formula = as.formula(paste("Y_bin ~", independent))
      
      model = glm(formula,
                  data = data,
                  family = "binomial")
      
      pred = predict(model, type = "response")
      
      auc_value = pROC::roc(Y_bin, pred)$auc
      
      auc_results = rbind(auc_results,
                           data.frame(Dependent = value,
                                      Independent = independent,
                                      AUC = auc_value))
    }
    
    for (comb in combn(paste0(names(data[-1])), 2, simplify = FALSE)) {
      
      independent = paste(comb, collapse = " + ")
      
      formula = as.formula(paste("Y_bin ~", independent))
      
      model = glm(formula,
                  data = data,
                  family = "binomial")
      
      pred = predict(model, type = "response")
      
      auc_value = pROC::roc(Y_bin, pred)$auc
      
      auc_results = rbind(auc_results,
                           data.frame(Dependent = value,
                                      Independent = independent,
                                      AUC = auc_value))
    }
  }
  
  avg_auc = aggregate(AUC ~ Independent,
                      data = auc_results,
                      FUN = mean)
  
  best_combination = avg_auc[which.max(avg_auc$AUC), "Independent"]
  
  return(list(auc_results,
              avg_auc,
              best_combination))
}


# Demo analysis

library(Hmisc)

library(pROC)

data = spss.get("FeatAUC_data.sav",
                use.value.labels = TRUE)

count = sample(0:5, 4604, replace = TRUE)

data_s = cbind(count, data)

data_m = data_s[,c(1,4,5,6)]

head(data_m)

auc_values = FeatCount("Y",
                       data = data_m,
                       ind_num = 3)
auc_values

cat("Best model:", which.max(auc_values[[2]]$AUC),
    "with AUC value:", max(auc_values[[2]]$AUC),
    "with combination:", paste(auc_values[[3]],
                               collapse = " + "), "\n")
