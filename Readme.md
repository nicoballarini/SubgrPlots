This project contains the article and code for: 
# A Critical Review of Graphics for Subgroup Analyses in Clinical Trials and Some Improvements

* `/data` Contains the dataset used in the examples.

* `/man` Documentation files for functions

* `/paper` Contains R code and figures for the manuscript

  * `/figures` a folder containing the plots for the manuscript
  
  * `/Rcode` a folder containing R code to produce the plots for the manuscript
  
* `/R`  R Code for functions in the package

Imports:
    alluvial,
    circlize,
    colorspace,
    diagram,
    dplyr,
    ggplot2 (>= 2.1.1),
    ggrepel,
    geoR,
    grid,
    gridBase,
    gridExtra,
    methods,
    plyr,
    polyclip,
    scales,
    shape,
    sp,
    survRM2,
    survival,
    UpSetR,
    VennDiagram
    
Optional:
  rgeos for the proportional area venn diagram (This package requires additional dev libraries which may not be trivial to install)
    
  knitr and rmarkdown for building the vignettes
