subgroupset_transposed <- function(data, nsets = 5, nintersects = 40, sets = NULL, keep.order = F, set.metadata = NULL, intersections = NULL,
                  matrix.color = "gray23", main.bar.color = "gray23", mainbar.y.label = "Intersection Size", mainbar.y.max = NULL,
                  sets.bar.color = "gray23", sets.x.label = "Set Size", point.size = 2.2, line.size = 0.7,
                  mb.ratio = c(0.70,0.30), expression = NULL, att.pos = NULL, att.color = main.bar.color, order.by = c("freq", "degree"),
                  decreasing = c(T, F), show.numbers = "yes", number.angles = 0, group.by = "degree",cutoff = NULL,
                  queries = NULL, query.legend = "none", shade.color = "gray88", shade.alpha = 0.25, matrix.dot.alpha =0.5,
                  empty.intersections = NULL, color.pal = 1, boxplot.summary = NULL,
                  effects.summary = NULL, outcome.type = c("continuous", "binary",  "survival"),
                  attribute.plots = NULL, scale.intersections = "identity",
                  scale.sets = "identity", text.scale = 1, set_size.angles = 0,
                  treatment.var = NULL, min.n = 20, icon = c("dots", "pm", "pm.circle", "value"), fill.trt = TRUE){
  if(is.null(treatment.var)) stop("Please provide a treatment variable in treatment.var")
  outcome.type = match.arg(outcome.type)
  icon = match.arg(icon)
  startend <- FindStartEnd(data)
  first.col <- startend[1]
  last.col <- startend[2]
  total_size = nrow(data)
  if(color.pal == 1){
    palette <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2",
                 "#7F7F7F", "#BCBD22", "#17BECF")
  } else {
    palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
                 "#CC79A7")
  }

  if(is.null(intersections) == F){
    Set_names <- unique((unlist(intersections)))
    Sets_to_remove <- Remove(data, first.col, last.col, Set_names)
    New_data <- Wanted(data, Sets_to_remove)
    Num_of_set <- Number_of_sets(Set_names)
    if(keep.order == F){
      Set_names <- order_sets(New_data, Set_names)
    }
    All_Freqs <- specific_intersections(data, first.col, last.col, intersections, order.by, group.by, decreasing,
                                        cutoff, main.bar.color, Set_names)
  } else if(is.null(intersections) == T){
    Set_names <- sets
    if(is.null(Set_names) == T || length(Set_names) == 0 ){
      Set_names <- FindMostFreq(data, first.col, last.col, nsets)
    }
    Sets_to_remove <- Remove(data, first.col, last.col, Set_names)
    New_data <- Wanted(data, Sets_to_remove)
    New_data = cbind(New_data, data[effects.summary])
    Num_of_set <- Number_of_sets(Set_names)
    if(keep.order == F){
    Set_names <- order_sets(New_data, Set_names)
    }
    All_Freqs <- Counter_all(New_data, Num_of_set, first.col, Set_names, nintersects, main.bar.color,
                         order.by, group.by, cutoff, empty.intersections, decreasing)

    Trt_data = New_data[which(New_data[[treatment.var]]==unique(New_data[[treatment.var]])[2]), ]
    Trt_Freqs <- Counter_all(Trt_data, Num_of_set, first.col, Set_names, nintersects, main.bar.color,
                                            order.by, group.by, cutoff, empty.intersections, decreasing)
    Ctl_data = New_data[which(New_data[[treatment.var]]==unique(New_data[[treatment.var]])[1]), ]
    Ctl_Freqs <- Counter_all(Ctl_data, Num_of_set, first.col, Set_names, nintersects, main.bar.color,
                                            order.by, group.by, cutoff, empty.intersections, decreasing)
  }
  Matrix_setup <- Create_matrix(All_Freqs)
  labels <- Make_labels(Matrix_setup)
  #Chose NA to represent NULL case as result of NA being inserted when at least one contained both x and y
  #i.e. if one custom plot had both x and y, and others had only x, the y's for the other plots were NA
  #if I decided to make the NULL case (all x and no y, or vice versa), there would have been alot more if/else statements
  #NA can be indexed so that we still get the non NA y aesthetics on correct plot. NULL cant be indexed.
  att.x <- c(); att.y <- c();
  if(is.null(attribute.plots) == F){
    for(i in seq_along(attribute.plots$plots)){
      if(length(attribute.plots$plots[[i]]$x) != 0){
        att.x[i] <- attribute.plots$plots[[i]]$x
      }
      else if(length(attribute.plots$plots[[i]]$x) == 0){
        att.x[i] <- NA
      }
      if(length(attribute.plots$plots[[i]]$y) != 0){
        att.y[i] <- attribute.plots$plots[[i]]$y
      }
      else if(length(attribute.plots$plots[[i]]$y) == 0){
        att.y[i] <- NA
      }
    }
  }

  BoxPlots <- NULL
  if(is.null(boxplot.summary) == F){
    warning("boxplot.summary is not available for subgroUpSet")
  }

  EffectPlots <- list()
  if(is.null(effects.summary) == F){
    EffectData <- IntersectionEffectPlot2(All_Freqs, New_data, first.col, Set_names, effects.summary, outcome.type)
    EffectPlots      <- list()
    EffectPlots[[1]] <- EffectPlotsPlot_t(EffectData, effects.summary, att.color, outcome.type, text.scale)
  }

  customAttDat <- NULL
  customQBar <- NULL
  Intersection <- NULL
  Element <- NULL
  legend <- NULL
  EBar_data <- NULL
  if(is.null(queries) == F){
    warning("queries is not available for subgroUpSet")
    Matrix_col <- NULL
  } else{
    Matrix_col <- NULL
  }

  Matrix_layout <- Create_layout(Matrix_setup, matrix.color, Matrix_col, matrix.dot.alpha)
  Set_sizes <- FindSetFreqs(New_data, first.col, Num_of_set, Set_names, keep.order)
  Bar_Q <- NULL
  QInter_att_data <- NULL
  QElem_att_data <- NULL
  AllQueryData <- NULL

  ShadingData <- NULL
  set.metadata.plots <- NULL
  if(is.null(set.metadata) == F){
    warning("set.metadata is not available for subgroUpSet")
  }
  if(is.null(ShadingData) == TRUE){
    ShadingData <- MakeShading(Matrix_layout, shade.color)
  }

  All_Freqs_Trt = rbind(data.frame(Trt_Freqs, trt = unique(New_data[[treatment.var]])[2]),
                        data.frame(Ctl_Freqs, trt = unique(New_data[[treatment.var]])[1]))
  Main_bar <- suppressMessages(Make_main_bar_t(All_Freqs_Trt, Bar_Q, show.numbers, mb.ratio, customQBar, number.angles, EBar_data, mainbar.y.label,
                            mainbar.y.max, scale.intersections, text.scale, attribute.plots, treatment.var, fill.trt))
  Matrix <- Make_matrix_plot_t(Matrix_layout, Set_sizes, All_Freqs, point.size, line.size,
                             text.scale, labels, ShadingData, shade.alpha, icon)
  Sizes <- Make_size_plot_t(Set_sizes, sets.bar.color, mb.ratio, sets.x.label, scale.sets, text.scale, set_size.angles,
                           total_size)
  Main_bar_legend <- suppressMessages(Make_main_bar_legend_t(All_Freqs_Trt, Bar_Q, show.numbers, mb.ratio, customQBar, number.angles, EBar_data, mainbar.y.label,
                                               mainbar.y.max, scale.intersections, text.scale, attribute.plots, treatment.var, fill.trt))

  Make_base_plot_t(Main_bar, Matrix, Sizes, labels, mb.ratio, att.x, att.y, New_data,
                 expression, att.pos, first.col, att.color, AllQueryData, attribute.plots,
                 legend, query.legend, EffectPlots, Set_names, set.metadata, set.metadata.plots, Main_bar_legend)

}
