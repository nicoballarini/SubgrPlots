## Generate main bar plot
Make_main_bar_legend_t <- function(Main_bar_data, Q, show_num, ratios, customQ, number_angles,
                          ebar, ylabel, ymax, scale_intersections, text_scale, attribute_plots,
                          treatment_var, fill.trt){

  bottom_margin <- (-1)*0.65

  if(is.null(attribute_plots) == FALSE){
    bottom_margin <- (-1)*0.45
  }

  if(length(text_scale) > 1 && length(text_scale) <= 6){
    y_axis_title_scale <- text_scale[1]
    y_axis_tick_label_scale <- text_scale[2]
    intersection_size_number_scale <- text_scale[6]
  }
  else{
    y_axis_title_scale <- text_scale
    y_axis_tick_label_scale <- text_scale
    intersection_size_number_scale <- text_scale
  }

  if(is.null(Q) == F){
    inter_data <- Q
    if(nrow(inter_data) != 0){
      inter_data <- inter_data[order(inter_data$x), ]
    }
    else{inter_data <- NULL}
  }
  else{inter_data <- NULL}

  if(is.null(ebar) == F){
    elem_data <- ebar
    if(nrow(elem_data) != 0){
      elem_data <- elem_data[order(elem_data$x), ]
    }
    else{elem_data <- NULL}
  }
  else{elem_data <- NULL}
  Main_bar_data_all = data.frame(x= sort(unique(Main_bar_data$x)),
                                 freq = tapply(Main_bar_data$freq, Main_bar_data$x, FUN=sum))

  #ten_perc creates appropriate space above highest bar so number doesnt get cut off
  if(is.null(ymax) == T){
  ten_perc <- ((max(Main_bar_data$freq)) * 0.1)
  ymax <- max(Main_bar_data_all$freq) + ten_perc
  }

  if(ylabel == "Intersection Size" && scale_intersections != "identity"){
    ylabel <- paste("Intersection Size", paste0("( ", scale_intersections, " )"))
  }
  if(scale_intersections == "log2"){
    Main_bar_data$freq <- round(log2(Main_bar_data$freq), 2)
    ymax <- log2(ymax)
  }
  if(scale_intersections == "log10"){
    Main_bar_data$freq <- round(log10(Main_bar_data$freq), 2)
    ymax <- log10(ymax)
  }

  Main_bar_data$x =  max(Main_bar_data$x) - Main_bar_data$x +1
  Main_bar_data_all$x = max(Main_bar_data_all$x) - Main_bar_data_all$x + 1

  if (fill.trt){
    Main_bar_data$trt = as.factor(Main_bar_data$trt)
    Main_bar_plot <- (ggplot(data = Main_bar_data, aes_string(x = "x", y = "freq"))
                      + geom_bar(aes_string(fill = "trt"), stat = "identity", width = 0.6)
                      + scale_fill_manual(values = c("#a6cee3","#1f78b4")))
  } else{
    Main_bar_plot <- (ggplot(data = Main_bar_data, aes_string(x = "x", y = "freq"))
                      + geom_bar(stat = "identity", width = 0.6))
  }
  Main_bar_plot <- (Main_bar_plot
                    + scale_y_continuous(trans = scale_intersections,
                                         position = "right",
                                         expand = c(0,0))
                    + coord_flip(ylim = c(0,ymax*1.15))
                    + scale_x_continuous(limits = c(0,(max(Main_bar_data$x)+1)),
                                         expand = c(0,0),
                                         breaks = NULL)
                    + xlab(NULL) + ylab(ylabel)
                    + labs(title = NULL, fill = "Treatment")
                    + guides(fill = guide_legend(title.position="top", title.hjust =0.5))
                    + theme(axis.line.x = element_line(),
                            panel.background = element_rect(fill = "white"),
                            legend.text = element_text(vjust =  0.5, size = 7*y_axis_title_scale),
                            legend.title = element_text(vjust =  0.5, size = 8.3*y_axis_title_scale),
                            legend.direction = "horizontal",
                            legend.key.size = unit(1, "line"),
                            axis.title.x.top = element_text(vjust =  0, size = 8.3*y_axis_title_scale),
                            axis.text.x.top  = element_text(vjust =  0.3, size = 7*y_axis_tick_label_scale, color = "gray0"),
                            plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"),
                            panel.border = element_blank()))
  legend <- g_legend(Main_bar_plot)
  return(legend)
}
