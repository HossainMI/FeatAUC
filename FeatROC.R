# Forward step-wise selection based on AUC value.

FeatAUC = function(dependent, data, ind_num) {
  
  auc_values = numeric()
  model_combinations = list()
  best_model = 0
  best_combination = NULL
  roc_list = list()
  
  for (x_count in 1:ind_num) {
    x_combinations = combn(names(data)[-1],
                           x_count)
    
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
      
      roc = pROC::roc(data[[dependent]], pred)
      
      roc_list[[length(roc_list) + 1]] = roc
      
      model_combinations[[length(model_combinations) + 1]] = predictors[-1]
      
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
  
  MC_df = data.frame(AUC = auc_values,
                     Model_Combinations = sapply(model_combinations,
                                                 paste, collapse = " + "))
  
  return(list(auc_values,
              MC_df,
              best_model,
              best_combination,
              roc_list))
}

# Demo analysis

setwd('G:\\R studio Project\\Feature Selection (AUC)')

library(Hmisc)

library(pROC)

data = spss.get("FeatAUC_data.sav",
                use.value.labels = TRUE)

data_s = data[, c(1,3,4,5,6,7)]

auc_values = FeatAUC(dependent = "EANC",
                     data = data_s,
                     ind_num = 5)

cat("Best model:", auc_values[[3]],
    "with AUC value:", max(auc_values[[1]]),
    "with combination:", paste(auc_values[[4]],
                               collapse = " + "), "\n")


# ROC curve for all combinations.

FeatROC = function(roc_list, ncol) {
  
  library(ggplot2)
  library(gridExtra)
  
  ggroc_plots = list()
  
  for (i in seq_along(roc_list)) {
    ggroc_plots[[i]] = ggroc(roc_list[[i]],
                             legacy.axes = TRUE) +
      coord_fixed() +
      geom_abline(color = "red") +
      ggtitle(paste("ROC Curve for Combination", i)) +
      theme(title = element_text(face = "bold"),
            plot.title = element_text(size = 12,
                                      face = "bold",
                                      hjust = 0.5),
            axis.title = element_text(size = 8,
                                      face = "bold.italic"),
            panel.background = element_blank(),
            panel.grid = element_line(color = "#8ccde3",
                                      size = 0.5,
                                      linetype = 2),
            panel.border = element_rect(fill = NA,
                                        size = 0.5,
                                        colour = "black"),
            axis.ticks = element_line(colour = "black"),
            axis.text = element_text(colour = "black"))
  }
  
  grid.arrange(grobs = ggroc_plots,
               ncol = ncol)
}


roc_list = auc_values[[4]]

FeatROC(roc_list = roc_list,
    ncol = 3)

