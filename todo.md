# Bugs/changes since v0.1.2

* Removed dependency with geoR

# Bugs/changes since v0.1.0

* Gailbraith plot:

  There is a new function ggplot_radial that implements the plot using ggplot.

  This is better for handling subgroup levels with ggrepel

* Forest plot 

  Third panel colors for treatment and control in legend.

  Fix xaxis in forest plots.

  added eff.scale for showing HR instead of logHR

* Contour plot:
  
  The default value for subtitle argument is "default", which prints the sample sizes.

* Venn diagram:
  
  Fixed problem with font sizes.

* Barchart:
  
  Added additional check for tau. If RMST is specified and time = NULL then it throws an error.
  
  The example is modified so that time = 50.

* plot_dissimilarity
  
  Now it does not print the overlap matrix.
  
* plot_stepp
  
  Fixed problem when having identical subgroups. Added ggplot_stepp to use the proper scaling in the x-axis

# Additional features

* Forest Plot: 

  Allow to have only two panels 

  Warning messages:
  1: In doTryCatch(return(expr), name, parentenv, handler) :
    calling par(new=TRUE) with no plot
  #416 -> col.line duplicated
 
 Allow for results in HR scale

 Check time = NA option. Give error if not specified. Dont care about it if the KM TRUE

 Check for tibble and trnasform to data.frame

 Document labels in forest plot function
