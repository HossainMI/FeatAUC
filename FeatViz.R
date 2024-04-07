# Graphical Presentation of feature selection

FeatViz = function(auc_values) {
  auc_data = data.frame(Combination = seq(1, length(auc_values[[1]])),
                        AUC = auc_values[[1]])
  
  colnames(auc_data) = c("Combination", "AUC")
  
  library(ggplot2)
  
  plot = ggplot(data = auc_data,
                aes(x = Combination,
                    y = AUC)) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = auc_values[[2]],
               linetype = 'dotted',
               col = 'red',
               size = 0.4) +
    labs(title = "",
         x = "Combination",
         y = "AUC value") +
    theme(title = element_text(face = "bold"),
          plot.title = element_text(size = 15,
                                    face = "bold",
                                    hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5),
          axis.title = element_text(size = 10,
                                    face = "bold.italic"),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "#E5E4E2"),
          panel.border = element_rect(fill = NA,
                                      "black",
                                      size = 0.7),
          axis.text.x = element_text(angle = 90, vjust = 0.5,
                                     hjust=1,
                                     color = "black"),
          axis.text.y = element_text(color = "black")) +
    scale_x_continuous(limits = c(1, length(auc_values[[1]])),
                       breaks = seq(0, length(auc_values[[1]]), 10))
  
  return(plot)
}

FeatViz(auc_values = auc_values)
