This project contains the article and code for: 
# Graphical displays for subgroup analysis in clinical trials

* `/data` Contains the dataset used in the examples.

* `/man` Documentation files for functions

* `/paper` Has contains R code, figures, tables for the manuscript

  * `/figures` a folder containing the plots for the manuscript
  
  * `/Rcode` a folder containing R code to produce the plots for the manuscript
  
* `/R`  R Code for functions in the package


Dependencies:
  diagram  polyclip  shape   spatstat  sp  UpSetR VennDiagram
  colorspace  survRM2   dplyr   geoR  gridBase  pacman  alluvial  circlize
Optional:
rgeos for the proportional area venn diagram (This package requires additional dev libraries which may not be trivial to install)
