# Evaluation matrix list extraction.

MatDF = function(model_combinations, roc_list, auc_values) {
  
  df_list = list()
  
  for (i in seq_along(model_combinations)) {
    model = model_combinations[[i]]
    roc = roc_list[[i]]
    auc = auc_values[[i]]
    
    df = data.frame(Model_Combination = model,
                     Sensitivity = roc$sensitivities,
                     Specificity = 1 - roc$specificities,
                     AUC = auc)
    
    cat("Model Combination:", paste(model, collapse = " + "), "\n")
    print(df)
    cat("\n")
    
    df_list[[i]] = df
  }
  
  combined_df = do.call(rbind, df_list)
  
  df_list$combined = combined_df
  
  return(df_list)
}


df_list = MatDF(auc_values[[2]]$Model_Combinations,
                auc_values[[5]],
                auc_values[[1]])
