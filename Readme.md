This project contains the article and code for: 
# Graphical displays for subgroup analysis in clinical trials

* `/data` Contains the dataset used in the examples.

* `/man` Documentation files for functions

* `/paper` Contains R code and figures for the manuscript

  * `/figures` a folder containing the plots for the manuscript
  
  * `/Rcode` a folder containing R code to produce the plots for the manuscript
  
* `/R`  R Code for functions in the package

Dependencies:
    alluvial,
    circlize,
    colorspace,
    diagram,
    dplyr,
    geoR,
    grid,
    gridBase,
    pacman,
    polyclip,
    shape,
    sp,
    spatstat,
    survRM2,
    survival,
    UpSetR,
    VennDiagram
Optional:
    rgeos for the proportional area venn diagram (This package requires additional dev libraries which may not be trivial to install)
