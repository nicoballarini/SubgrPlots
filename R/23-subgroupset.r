#' UpSetR Plot with treatment effects
#'
#' @description Visualization of set intersections using novel UpSet matrix design, modified to show treatment effects and confidence intervals
#' @param data Data set
#' @param nsets Number of sets to look at
#' @param nintersects Number of intersections to plot. If set to NA, all intersections will be plotted.
#' @param sets Specific sets to look at (Include as combinations. Ex: c("Name1", "Name2"))
#' @param keep.order Keep sets in the order entered using the sets parameter. The default is FALSE, which orders the sets by their sizes.
#' @param set.metadata Metadata that offers insight to an attribute of the sets. Input should be a data frame where the first column is set names, and the
#'        remaining columns are attributes of those sets. To learn how to use this parameter it is highly suggested to view the set metadata vignette. The link
#'        can be found on the package's GitHub page.
#' @param intersections Specific intersections to include in plot entered as a list of lists.
#'        Ex: list(list("Set name1", "Set name2"), list("Set name1", "Set name3")). If data is entered into this parameter the only data shown on the UpSet plot
#'        will be the specific intersections listed.
#' @param matrix.color Color of the intersection points
#' @param main.bar.color Color of the main bar plot
#' @param mainbar.y.label The y-axis label of the intersection size bar plot
#' @param mainbar.y.max The maximum y value of the intersection size bar plot scale. May be useful when aligning multiple UpSet plots horizontally.
#' @param sets.bar.color Color of set size bar plot
#' @param sets.x.label The x-axis label of the set size bar plot
#' @param point.size Size of points in matrix plot
#' @param line.size Width of lines in matrix plot
#' @param mb.ratio Ratio between matrix plot and main bar plot (Keep in terms of hundreths)
#' @param expression Expression to subset attributes of intersection or element query data. Enter as string (Ex: "ColName > 3")
#' @param att.pos Position of attribute plot. If NULL or "bottom" the plot will be at below UpSet plot. If "top" it will be above UpSert plot
#' @param att.color Color of attribute histogram bins or scatterplot points for unqueried data represented by main bars. Default set to color of main bars.
#' @param order.by How the intersections in the matrix should be ordered by. Options include frequency (entered as "freq"), degree, or both in any order.
#' @param decreasing How the variables in order.by should be ordered. "freq" is decreasing (greatest to least) and "degree" is increasing (least to greatest)
#' @param show.numbers Show numbers of intersection sizes above bars
#' @param number.angles The angle of the numbers atop the intersection size bars
#' @param group.by How the data should be grouped ("degree" or "sets")
#' @param cutoff The number of intersections from each set (to cut off at) when aggregating by sets
#' @param queries Unified querie of intersections, elements, and custom row functions. Entered as a list that contains a list of
#'        queries. query is the type of query being conducted. params are the parameters of the query (if any). color is the color of the points on the
#'        plot that will represent the query. If no color is selected one will be provided automatically. active takes TRUE or FALSE, and if
#'        TRUE, it will overlay the bars present  with the results from the query. If FALSE a tick mark will indicate the intersection size.
#'        See examples section on how to do this.
#' @param query.legend Position query legend on top or bottom of UpSet plot
#' @param shade.color Color of row shading in matrix
#' @param shade.alpha Transparency of shading in matrix
#' @param matrix.dot.alpha Transparency of the empty intersections points in the matrix
#' @param empty.intersections Additionally display empty sets up to nintersects
#' @param color.pal Color palette for attribute plots
#' @param boxplot.summary Boxplots representing the distribution of a selected attribute for each intersection. Select attributes by entering a character vector of attribute names (e.g. c("Name1", "Name2")).
#'        The maximum number of attributes that can be entered is 2.
#' @param effects.summary Forest plot. Select the response variable by entering a character attribute name (e.g. "y" or c("survtime", "cens")
#' @param attribute.plots Create custom ggplot using intersection data represented in the main bar plot. Prior to adding custom plots, the UpSet plot is set up in a 100 by 100 grid.
#'        The attribute.plots parameter takes a list that contains the number of rows that should be allocated for the custom plot, and a list of plots with specified positions.
#'        nrows is the number of rows the custom plots should take up. There is already 100 allocated for the custom plot. plots takes a list that contains a function that returns
#'        a custom ggplots and the x and y aesthetics for the function. ncols is the number of columns that your ggplots should take up. See examples for how to add custom ggplots.
#' @param scale.intersections The scale to be used for the intersection sizes. Options: "identity", "log10", "log2"
#' @param scale.sets The scale to be used for the set sizes. Options: "identity", "log10", "log2"
#' @param text.scale Numeric, value to scale the text sizes, applies to all axis labels, tick labels, and numbers above bar plot. Can be a universal scale, or a vector containing individual scales
#'        in the following format: c(intersection size title, intersection size tick labels, set size title, set size tick labels, set names, numbers above bars)
#' @param set_size.angles Numeric, angle to rotate the set size plot x-axis text
#' @param outcome.type One of "continuous", "binary", or "survival" to determine the model to implement
#' @param treatment.var A character indicating the name of the treatment variable in the dataset
#' @param min.n The minimum number of subjects in a subgroup to be included in the plot
#' @param icon One of "dots", "pm", "pm.circle", or "value" which determines the icon to use in the matrix plot
#' @param fill.trt A logical indicating whether the bar plot is coloured by treatment.
#' @param transpose A logical indicating whether to draw the plot vertically.
#' @details Visualization of set data in the layout described by Lex and Gehlenborg in \url{http://www.nature.com/nmeth/journal/v11/n8/abs/nmeth.3033.html}.
#' The plot is modified here to select a treatment variable, compute the treatment effects and display them along with their confidence
#' intervals in a forest plot-like panel.
#' @note Data set must be formatted as described on the orginal UpSet github page: \url{http://github.com/VCG/upset/wiki}.
#' @references Lex et al. (2014). UpSet: Visualization of Intersecting Sets
#' IEEE Transactions on Visualization and Computer Graphics (Proceedings of InfoVis 2014), vol 20, pp. 1983-1992, (2014). \url{http://people.seas.harvard.edu/~alex/papers/2014_infovis_upset.pdf}
#' @references Lex and Gehlenborg (2014). Points of view: Sets and intersections. Nature Methods 11, 779 (2014). \url{http://www.nature.com/nmeth/journal/v11/n8/abs/nmeth.3033.html}
#' @seealso Original UpSet Website: \url{http://vcg.github.io/upset/about/}
#' @seealso UpSetR github for additional examples: \url{http://github.com/hms-dbmi/UpSetR}
#'
#'
#' @examples
#' \donttest{
#' data(prca)
#' dat <- prca
#' vars = data.frame(variable = names(dat), index = 1:length(names(dat)))
#'
#' ## 12. SubgroUpSet -----------------------------------------------------------
#' prca.upset = data.frame(trt = factor(ifelse(prca$rx == 1, "Experimental", "Control")),
#'                         bm = 1*(prca$bm == 1),
#'                         pf = 1*(prca$pf == 1),
#'                         hx = 1*(prca$hx == 1),
#'                         stage = 1*(prca$stage == 4),
#'                         age = 1*(prca$age > 75),
#'                         wt = 1*(prca$weight > 100),
#'                         survtime = prca$survtime,
#'                         cens = prca$cens==1)
#' subgroupset(prca.upset,
#'             order.by = "freq",
#'             empty.intersections = "on",
#'             sets = c("bm", 'pf', "hx"),
#'             text.scale = 1.,
#'             mb.ratio = c(0.25, 0.50,0.20),
#'             treatment.var = "trt",
#'             outcome.type = "survival",
#'             effects.summary = c("survtime", "cens"),
#'             query.legend = "top", icon = "pm")
#'
#' subgroupset(prca.upset,
#'             order.by = "freq",
#'             empty.intersections = "on",
#'             sets = c("bm", 'pf', "hx"),
#'             text.scale = 1.,
#'             mb.ratio = c(0.25, 0.50,0.20),
#'             treatment.var = "trt",
#'             outcome.type = "survival",
#'             effects.summary = c("survtime", "cens"),
#'             query.legend = "top", icon = "pm", transpose = TRUE)
#' }
#'
#' @import gridExtra
#' @import ggplot2
#' @import utils
#' @import stats
#' @import methods
#' @import grDevices
#' @import scales
#' @export
subgroupset <- function(data, nsets = 5, nintersects = 40, sets = NULL, keep.order = F, set.metadata = NULL, intersections = NULL,
                  matrix.color = "gray23", main.bar.color = "gray23", mainbar.y.label = "Intersection Size", mainbar.y.max = NULL,
                  sets.bar.color = "gray23", sets.x.label = "Set Size", point.size = 2.2, line.size = 0.7,
                  mb.ratio = c(0.70,0.30), expression = NULL, att.pos = NULL, att.color = main.bar.color, order.by = c("freq", "degree"),
                  decreasing = c(T, F), show.numbers = "yes", number.angles = 0, group.by = "degree",cutoff = NULL,
                  queries = NULL, query.legend = "none", shade.color = "gray88", shade.alpha = 0.25, matrix.dot.alpha =0.5,
                  empty.intersections = NULL, color.pal = 1, boxplot.summary = NULL,
                  effects.summary = NULL, outcome.type = c("continuous", "binary",  "survival"),
                  attribute.plots = NULL, scale.intersections = "identity",
                  scale.sets = "identity", text.scale = 1, set_size.angles = 0,
                  treatment.var = NULL, min.n = 20, icon = c("dots", "pm", "pm.circle", "value"),
                  fill.trt = TRUE, transpose = FALSE){

  if (transpose){
    subgroupset_transposed(data, nsets, nintersects, sets, keep.order, set.metadata, intersections,
                           matrix.color, main.bar.color, mainbar.y.label, mainbar.y.max,
                           sets.bar.color, sets.x.label, point.size, line.size,
                           mb.ratio, expression, att.pos, att.color, order.by,
                           decreasing, show.numbers, number.angles, group.by,cutoff,
                           queries, query.legend, shade.color, shade.alpha, matrix.dot.alpha,
                           empty.intersections, color.pal, boxplot.summary,
                           effects.summary, outcome.type,
                           attribute.plots, scale.intersections,
                           scale.sets, text.scale, set_size.angles,
                           treatment.var, min.n, icon, fill.trt)
  } else {
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
    EffectPlots[[1]] <- EffectPlotsPlot(EffectData, effects.summary, att.color, outcome.type)
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
  Main_bar <- suppressMessages(Make_main_bar(All_Freqs_Trt, Bar_Q, show.numbers, mb.ratio, customQBar, number.angles, EBar_data, mainbar.y.label,
                            mainbar.y.max, scale.intersections, text.scale, attribute.plots, treatment.var, fill.trt))
  Matrix <- Make_matrix_plot2(Matrix_layout, Set_sizes, All_Freqs, point.size, line.size,
                             text.scale, labels, ShadingData, shade.alpha, icon)
  Sizes <- Make_size_plot2(Set_sizes, sets.bar.color, mb.ratio, sets.x.label, scale.sets, text.scale, set_size.angles,
                           total_size)

  Make_base_plot(Main_bar, Matrix, Sizes, labels, mb.ratio, att.x, att.y, New_data,
                 expression, att.pos, first.col, att.color, AllQueryData, attribute.plots,
                 legend, query.legend, EffectPlots, Set_names, set.metadata, set.metadata.plots)
  }
}
