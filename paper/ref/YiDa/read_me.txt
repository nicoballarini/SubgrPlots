1. need to re-write the following code' paths 


setwd("C:/Users/chiuy1/graphical display for subgrp analysis in clinical trials")
load("C:/Users/chiuy1/graphical display for subgrp analysis in clinical trials/data/dat.Rda")
load("C:/Users/chiuy1/graphical display for subgrp analysis in clinical trials/data/dat2.Rda")
load("C:/Users/chiuy1/graphical display for subgrp analysis in clinical trials/data/dat3.Rda")
source('C:/Users/chiuy1/graphical display for subgrp analysis in clinical trials/circleSP.r')
source('C:/Users/chiuy1/graphical display for subgrp analysis in clinical trials/ellipseSP.r')
source('C:/Users/chiuy1/graphical display for subgrp analysis in clinical trials/image.scale.r')
source('C:/Users/chiuy1/graphical display for subgrp analysis in clinical trials/text.pos.r')
source('C:/Users/chiuy1/graphical display for subgrp analysis in clinical trials/rectangleSP_test.r')


2.produce plots:
 
  -level plot-
  source('C:/Users/chiuy1/graphical display for subgrp analysis in clinical trials/level_plot_function.r')

  -Venn digram (3, 4, 5 sets)-
  source('C:/Users/chiuy1/graphical display for subgrp analysis in clinical trials/venn_diag_function.r')

  -bar chart(3 levels)-
  source('C:/Users/chiuy1/graphical display for subgrp analysis in clinical trials/barchart_function.r')

  -tree plots (4 covariates)-
  source('C:/Users/chiuy1/graphical display for subgrp analysis in clinical trials/tree_plot_function.r')

  -contour plot-
  source('C:/Users/chiuy1/graphical display for subgrp analysis in clinical trials/contour_plot_function.r')

  -forest plot-
  source('C:/Users/chiuy1/graphical display for subgrp analysis in clinical trials/forest_plot_function.r')

  -LAbbe plot-
  source('C:/Users/chiuy1/graphical display for subgrp analysis in clinical trials/labbe_plot_function.r')

  -Galbraith plot-
  source('C:/Users/chiuy1/graphical display for subgrp analysis in clinical trials/radial_plot_function.r')

  -STEPP-
  source('C:/Users/chiuy1/graphical display for subgrp analysis in clinical trials/stepp_plot_function.r')

  -plot 1 for subgroup overlap-
  source('C:/Users/chiuy1/graphical display for subgrp analysis in clinical trials/unidir_aw_ovlp.R')

  -plot 2 for subgroup overlap-
  source('C:/Users/chiuy1/graphical display for subgrp analysis in clinical trials/alt_unidir_aw_ovlp.R')

  -plot 3 for subgroup overlap-
   source('C:/Users/chiuy1/graphical display for subgrp analysis in clinical trials/mat_ovlp.r')

  -plot 4 for dissimilarity distance-
  source('C:/Users/chiuy1/graphical display for subgrp analysis in clinical trials/plt_dis_msr.r')

  -plot 5 for dissimilarity distance-
  source('C:/Users/chiuy1/graphical display for subgrp analysis in clinical trials/alt_plt_dis_msr.r')

  an example of combining forest plot and matrix plot
  "combined_plots_example.r"





















