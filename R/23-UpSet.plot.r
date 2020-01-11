#' @importFrom grid grid.newpage
#' @importFrom grid pushViewport
#' @importFrom grid viewport
#' @importFrom grid grid.layout
#' @importFrom grid grid.draw
#' @importFrom grid popViewport
#' @importFrom grid legendGrob
#' @importFrom grid gpar

## Assemble plots to make UpSet plot
Make_base_plot <- function(Main_bar_plot, Matrix_plot, Size_plot, labels, hratios, att_x, att_y,
                           Set_data, exp, position, start_col, att_color, QueryData,
                           attribute_plots, legend, query_legend, boxplot, names, set_metadata,
                           set_metadata_plots){

  end_col <- ((start_col + as.integer(length(labels))) - 1)
  Set_data <- Set_data[which(rowSums(Set_data[ ,start_col:end_col]) != 0), ]
  Main_bar_plot$widths <- Matrix_plot$widths
  Matrix_plot$heights <- Size_plot$heights
  if(is.null(set_metadata) ==F){
    ncols <- 0
    for(i in 1:length(set_metadata_plots)){
      ncols <- ncols + set_metadata$plots[[i]]$assign
      set_metadata_plots[[i]]$heights <- Size_plot$heights
    }
    set_metadata$ncols <- ncols
  }
  if(is.null(legend)==F){
    legend$widths <- Matrix_plot$widths
  }
  if(is.null(boxplot) == F){
    for(i in seq_along(boxplot)){
      boxplot[[i]]$widths <- Matrix_plot$widths
    }
  }

  size_plot_height <- (((hratios[1])+0.01)*100)
  if((hratios[1] > 0.8 || hratios[1] < 0.2) ||
     (hratios[2] > 0.8 || hratios[2] < 0.2) ||
     (hratios[3] > 0.8 || hratios[3] < 0.2)) warning("Plot might be out of range if ratio > 0.8 or < 0.2")
  if(is.null(boxplot)==F && is.null(attribute_plots) == T){
    BaseBoxPlot(boxplot, position, size_plot_height, Main_bar_plot, Matrix_plot, Size_plot,
                hratios, set_metadata, set_metadata_plots)
  }
}

## Viewport function
vplayout <- function(x,y){
  viewport(layout.pos.row = x, layout.pos.col = y)
}

## Generates UpSet plot with boxplots representing distributions of attributes
BaseBoxPlot <- function(box_plot, position, size_plot_height, Main_bar_plot, Matrix_plot,
                        Size_plot, hratios, set_metadata, set_metadata_plots){
  if(length(box_plot) > 2){
    return(warning("UpSet can only show 2 box plots at a time"))
  }
  if(is.null(position) == T || position == tolower("bottom")){
    bar_top <- 1
    matrix_bottom <- (sum(hratios[1:2])*100)
    att_top <- (sum(hratios[1:2])*100)+1
    att_bottom <- 100-5
    if(length(box_plot) == 2){
      att_top <- 105
      att_bottom <- 120
      gridrow <- 145
    }
  }
  if((is.null(position) == F) && (position != tolower("bottom"))){
    if(length(box_plot)==1){
      size_plot_height <- (size_plot_height + hratios[3]*100)
      bar_top <- hratios[3]*100+1
      matrix_bottom <- (sum(hratios[1:2])*100)
      att_top <- 10
      att_bottom <- hratios[3]*100
    }
    else if(length(box_plot) == 2){
      size_plot_height <- (size_plot_height + 50)
      bar_top <- 51
      matrix_bottom <- 150
      att_top <- 15
      att_bottom <- 30
      gridrow <- 150
    }
  }
  if(is.null(set_metadata) == T){
    matrix_and_mainbar_right <- 100
    matrix_and_mainbar_left <- 21
    size_bar_right <- 20
    size_bar_left <- 1
  }
  else if(is.null(set_metadata) == F){
    matrix_and_mainbar_right <- set_metadata$ncols + 100
    matrix_and_mainbar_left <- set_metadata$ncols + 21
    size_bar_right <- set_metadata$ncols + 20
    size_bar_left <- set_metadata$ncols + 1
    metadata_right <- set_metadata$ncols
    metadata_left <- 1
  }
  grid.newpage()
  if(length(box_plot) == 1){
    pushViewport(viewport(layout = grid.layout(100,matrix_and_mainbar_right)))
  }
  else if(length(box_plot) == 2){
    pushViewport(viewport(layout = grid.layout(gridrow,matrix_and_mainbar_right)))
  }
  vp = vplayout(bar_top:(size_plot_height-2), matrix_and_mainbar_left:matrix_and_mainbar_right)
  pushViewport(vp)
  grid.draw(Main_bar_plot)
  popViewport()
  vp = vplayout(size_plot_height:matrix_bottom, matrix_and_mainbar_left:matrix_and_mainbar_right)
  pushViewport(vp)
  grid.draw(Matrix_plot)
  popViewport()

  vp = vplayout(size_plot_height:matrix_bottom, size_bar_left:size_bar_right)
  pushViewport(vp)
  grid.draw(arrangeGrob(Size_plot))
  popViewport()
  if(is.null(set_metadata) == F){
    for(i in 1:length(set_metadata_plots)){
      if(i != 1){
        metadata_left <- 1+metadata_right
        metadata_right <- metadata_right + set_metadata$plots[[i]]$assign
      }
      else{
        metadata_left <- 1
        metadata_right <- set_metadata$plots[[i]]$assign
      }

      vp = vplayout(size_plot_height:matrix_bottom, metadata_left:metadata_right)
      pushViewport(vp)
      grid.draw(arrangeGrob(set_metadata_plots[[i]]))
      popViewport()
    }
  }
  vp = vplayout(att_top:att_bottom, matrix_and_mainbar_left:matrix_and_mainbar_right)
  pushViewport(vp)
  grid.draw(arrangeGrob(box_plot[[1]]))
  popViewport()
  if(length(box_plot) == 2){
    vp = vplayout((att_bottom + 10):(att_bottom + 25), matrix_and_mainbar_left:matrix_and_mainbar_right)
    pushViewport(vp)
    grid.draw(arrangeGrob(box_plot[[2]]))
    popViewport()
  }
}
