This project contains the article and code for: 
# Graphical displays for subgroup analysis in clinical trials

* `/data` Contains the dataset used in the examples.

* `/man` Documentation files for functions

* `/paper` Has contains R code, figures, tables for the manuscript

* `/R`  R Code for functions in the package

* `/ref`  Other files, to be removed.


Packages used:
    diagram, 
    polyclip,
    shape,
    spatstat,
    sp,
    UpSetR,
    VennDiagram
To do:
- Venn diagrams with 2 sets need some work when filling colors.
- YiDa may have new functions for the ovelapping plots (figure 13) since the arrangement of the subgroups is different in the paper than the output I get.





Done:
- Created an R package using YiDa's R code.
- Added dataset prca to illustrate functions.
- Created new functions by slightly modifying the provided ones:
    - Changed dev.new(width = 10, height = 10, noRStudioGD = TRUE) to grid.new() so that new plots are shown in the Rstudio plot window. If one wants to output the plot to a pdf, simply have to wrap the function with pdf() and dev.off(). This allows also to output the file with while specifying the name and the size of the plot.
    - Changed the name of the new functions so that they are concise and meaningful. Proposal: Use plot_XXXXX (http://adv-r.had.co.nz/Style.html https://journal.r-project.org/archive/2012-2/RJournal_2012-2_Baaaath.pdf)
    - Venn diagrams are now also plotted with the VennDiagram package to avoid dependeance on the GEOS library (except for the case of two sets, which still needs some work because VennDiagram arranges them by size.). The proportional area is not possible with this implementation.
- Added Alluvial plots and UpSet plots curretly using other packages. Should we wrap this functions into new functions so that they have a similar sintax as the other ones?
- Implemented L'Abbe for survival data. Now the function has an extra parameter 'time'. The x- and y-coordinates correspond to the survival probability at 'time' for treatment and control, respectively.
- Changed some parameters for bar charts. Added just = "center" in grid.yaxis. Added vjust = 0 for y-axis' label. The 0 text is not plotted now as it overlapped
- Implemented second third panel for forest plots when outcome is survival. Now the survival probability at time 'time' is plotted where 'time is a new parameter in the function. Also changed the alignment in the first panel of the plot (table).
- added grid.rect(gp=gpar(fill=NA)) in the tree plot
- In L'abbe plot, changed the alignment of effect size in table at top-left.
- added argument 'new' to forestplot and matrix plot. If TRUE, it will produce the plot by itself. If FALSE, the layout should be given. This is to compute the combined plots as in Figure 14



February 2018:
- Changed colors in all plots 13. When there is no overlap, there is no arrow. Also the order and labels are modified.
- Level plot: Changed colors for treatment effects to have a divergent scale from 0.
- Filled venn diagram: Changed colors for treatment effects to have a divergent scale from 0.
- Contour plot: Changed colors for treatment effects to have a divergent scale from 0.
