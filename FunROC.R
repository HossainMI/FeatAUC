# ROC curve of all model combinations in one frame.

FunROC = function(roc_list) {
  
  library(gridExtra)
  library(ggplot2)
  
  plots = lapply(roc_list,
                 function(roc) {
    ggroc(roc,
          legacy.axes = TRUE) +
                     coord_fixed()+
                     geom_abline(color="red")+
                     theme(title = element_text(face = "bold"),
                           plot.title = element_text(size = 12,
                                                     face = "bold",
                                                     hjust = 0.5),
                           axis.title = element_text(size = 8,
                                                     face = "bold.italic"),
                           legend.position = "none",
                           panel.background = element_blank(),
                           panel.grid = element_line(color = "#8ccde3",
                                                     size = 0.5,
                                                     linetype = 2),
                           panel.border = element_rect(fill = NA,
                                                       size = 0.5,
                                                       colour = "black"),
                           axis.ticks = element_line(colour = "black"),
                           axis.text = element_text(colour = "black"))
                   })
  grid.arrange(grobs = plots, ncol = length(roc_list))
}

roc_list = auc_values[[5]]

FunROC(list(roc_list))
