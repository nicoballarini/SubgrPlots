==> devtools::check(document = FALSE)

Setting env vars --------------------------------------------------------------
CFLAGS  : -Wall -pedantic
CXXFLAGS: -Wall -pedantic
Building SubgrPlots -----------------------------------------------------------
'/usr/lib/R/bin/R' --no-site-file --no-environ --no-save --no-restore --quiet  \
  CMD build '/home/nico/Rpackages/SubgrPlots' --no-resave-data --no-manual 

* checking for file ‘/home/nico/Rpackages/SubgrPlots/DESCRIPTION’ ... OK
* preparing ‘SubgrPlots’:
* checking DESCRIPTION meta-information ... OK
* checking for LF line-endings in source and make files and shell scripts
* checking for empty or unneeded directories
* looking to see if a ‘data/datalist’ file should be added
* building ‘SubgrPlots_0.1.0.tar.gz’

Setting env vars --------------------------------------------------------------
_R_CHECK_CRAN_INCOMING_ : FALSE
_R_CHECK_FORCE_SUGGESTS_: FALSE
Checking SubgrPlots -----------------------------------------------------------
'/usr/lib/R/bin/R' --no-site-file --no-environ --no-save --no-restore --quiet  \
  CMD check '/tmp/Rtmppg3iI7/SubgrPlots_0.1.0.tar.gz' --as-cran --timings  \
  --no-manual 

* using log directory ‘/home/nico/Rpackages/SubgrPlots.Rcheck’
* using R version 3.4.2 (2017-09-28)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --as-cran’
* checking for file ‘SubgrPlots/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘SubgrPlots’ version ‘0.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘SubgrPlots’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... WARNING
'::' or ':::' imports not declared from:
  ‘SubgroUpSetR’ ‘rgeos’
'loadNamespace' or 'requireNamespace' call not declared from: ‘rgeos’
Missing or unexported object: ‘grid::gpar.text’
Unavailable namespace imported from by a ':::' call: ‘SubgroUpSetR’
  See the note in ?`:::` about the use of this operator.
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... NOTE
EffectPlotsPlot_t: no visible global function definition for ‘%>%’
EffectPlotsPlot_t: no visible global function definition for ‘mutate’
EffectPlotsPlot_t: no visible binding for global variable ‘x’
IntersectionEffectPlot: no visible global function definition for
  ‘coxph’
IntersectionEffectPlot : <anonymous>: no visible global function
  definition for ‘coxph’
IntersectionEffectPlot2: no visible global function definition for
  ‘coxph’
IntersectionEffectPlot2 : <anonymous>: no visible global function
  definition for ‘coxph’
Make_base_plot: no visible global function definition for
  ‘GenerateCustomPlots’
Make_base_plot_t: no visible global function definition for
  ‘GenerateCustomPlots’
Make_main_bar: no visible binding for global variable ‘trt’
Make_main_bar_t: no visible global function definition for ‘%>%’
Make_main_bar_t: no visible global function definition for ‘mutate’
Make_main_bar_t: no visible binding for global variable ‘x’
Make_main_bar_t: no visible binding for global variable ‘trt’
Make_matrix_plot_t: no visible global function definition for ‘%>%’
Make_matrix_plot_t: no visible global function definition for ‘mutate’
Make_matrix_plot_t: no visible binding for global variable ‘x’
subgroupset: no visible global function definition for
  ‘specific_intersections’
subgroupset: no visible global function definition for
  ‘IntersectionBoxPlot’
subgroupset: no visible global function definition for ‘BoxPlotsPlot’
subgroupset: no visible global function definition for
  ‘SeperateQueries’
subgroupset: no visible global function definition for ‘customQueries’
subgroupset: no visible global function definition for ‘GuideGenerator’
subgroupset: no visible global function definition for ‘Make_legend’
subgroupset: no visible global function definition for ‘CustomAttData’
subgroupset: no visible global function definition for
  ‘customQueriesBar’
subgroupset: no visible global function definition for ‘intersects’
subgroupset: no visible binding for global variable ‘QuerieInterData’
subgroupset: no visible global function definition for ‘ElemBarDat’
subgroupset: no visible binding for global variable ‘QuerieInterBar’
subgroupset: no visible binding for global variable ‘QuerieInterAtt’
subgroupset: no visible global function definition for ‘elements’
subgroupset: no visible binding for global variable ‘QuerieElemAtt’
subgroupset: no visible global function definition for
  ‘get_shade_groups’
subgroupset: no visible global function definition for
  ‘Make_set_metadata_plot’
subgroupset_transposed: no visible global function definition for
  ‘specific_intersections’
subgroupset_transposed: no visible global function definition for
  ‘IntersectionBoxPlot’
subgroupset_transposed: no visible global function definition for
  ‘BoxPlotsPlot’
subgroupset_transposed: no visible global function definition for
  ‘SeperateQueries’
subgroupset_transposed: no visible global function definition for
  ‘customQueries’
subgroupset_transposed: no visible global function definition for
  ‘GuideGenerator’
subgroupset_transposed: no visible global function definition for
  ‘Make_legend’
subgroupset_transposed: no visible global function definition for
  ‘CustomAttData’
subgroupset_transposed: no visible global function definition for
  ‘customQueriesBar’
subgroupset_transposed: no visible global function definition for
  ‘intersects’
subgroupset_transposed: no visible binding for global variable
  ‘QuerieInterData’
subgroupset_transposed: no visible global function definition for
  ‘ElemBarDat’
subgroupset_transposed: no visible binding for global variable
  ‘QuerieInterBar’
subgroupset_transposed: no visible binding for global variable
  ‘QuerieInterAtt’
subgroupset_transposed: no visible global function definition for
  ‘elements’
subgroupset_transposed: no visible binding for global variable
  ‘QuerieElemAtt’
subgroupset_transposed: no visible global function definition for
  ‘get_shade_groups’
subgroupset_transposed: no visible global function definition for
  ‘Make_set_metadata_plot’
Undefined global functions or variables:
  %>% BoxPlotsPlot CustomAttData ElemBarDat GenerateCustomPlots
  GuideGenerator IntersectionBoxPlot Make_legend Make_set_metadata_plot
  QuerieElemAtt QuerieInterAtt QuerieInterBar QuerieInterData
  SeperateQueries coxph customQueries customQueriesBar elements
  get_shade_groups intersects mutate specific_intersections trt x
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd line widths ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... WARNING
Undocumented code objects:
  ‘plot_circle_std’ ‘plot_circle_std_by’ ‘plot_mosaic_2’
  ‘plot_mosaic_2_marginal’ ‘plot_mosaic_3’ ‘plot_mosaic_3_noeffect’
All user-level objects in a package should have documentation entries.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... WARNING
Undocumented arguments in documentation object 'alluvial_new'
  ‘...’ ‘freq’ ‘col’ ‘border’ ‘layer’ ‘hide’ ‘alpha’ ‘gap.width’ ‘xw’
  ‘cw’ ‘blocks’ ‘ordering’ ‘axis_labels’ ‘cex’ ‘cex.axis’ ‘rotate’

Undocumented arguments in documentation object 'alluvial_transposed'
  ‘...’ ‘freq’ ‘col’ ‘border’ ‘layer’ ‘hide’ ‘alpha’ ‘gap.width’ ‘xw’
  ‘cw’ ‘blocks’ ‘ordering’ ‘axis_labels’ ‘cex’ ‘cex.axis’ ‘rotate’
  ‘bottom.mar’

Undocumented arguments in documentation object 'contourplt_new'
  ‘dat’ ‘covari.sel’ ‘trt.sel’ ‘resp.sel’ ‘outcome.type’ ‘setup.ss’
  ‘n.grid’ ‘brk.es’ ‘n.brk.axis’ ‘para.plot’ ‘font.size’ ‘title’
  ‘subtitle’ ‘effect’ ‘point.size’ ‘filled’ ‘spiral’ ‘strip’
  ‘show.overall’ ‘verbose’ ‘palette’ ‘col.power’
Documented arguments not in \usage in documentation object 'contourplt_new':
  ‘dat:’ ‘covari.sel:’ ‘trt.sel:’ ‘resp.sel:’ ‘outcome.type:’
  ‘setup.ss:’ ‘n.grid:’ ‘brk.es:’ ‘para.plot:’ ‘font.size:’ ‘title:’
  ‘subtitle:’

Undocumented arguments in documentation object 'contourplt_new2'
  ‘dat’ ‘covari.sel’ ‘trt.sel’ ‘resp.sel’ ‘outcome.type’ ‘setup.ss’
  ‘n.grid’ ‘brk.es’ ‘n.brk.axis’ ‘para.plot’ ‘font.size’ ‘title’
  ‘subtitle’ ‘effect’ ‘point.size’ ‘filled’ ‘strip’ ‘show.overall’
  ‘verbose’ ‘palette’ ‘col.power’ ‘show.points’
Documented arguments not in \usage in documentation object 'contourplt_new2':
  ‘dat:’ ‘covari.sel:’ ‘trt.sel:’ ‘resp.sel:’ ‘outcome.type:’
  ‘setup.ss:’ ‘n.grid:’ ‘brk.es:’ ‘para.plot:’ ‘font.size:’ ‘title:’
  ‘subtitle:’

Undocumented arguments in documentation object 'plot_barchart'
  ‘dat’ ‘covari.sel’ ‘trt.sel’ ‘resp.sel’ ‘outcome.type’ ‘font.size’
  ‘title’ ‘lab.y’ ‘effect’ ‘time’ ‘sig.dig’
Documented arguments not in \usage in documentation object 'plot_barchart':
  ‘dat:’ ‘covari.sel:’ ‘trt.sel:’ ‘resp.sel:’ ‘outcome.type:’
  ‘font.size:’ ‘title:’ ‘lab.y:’

Undocumented arguments in documentation object 'plot_circle'
  ‘dat’ ‘covari.sel’ ‘trt.sel’ ‘resp.sel’ ‘outcome.type’ ‘range.v’
  ‘adj.ann.subgrp’ ‘range.strip’ ‘n.brk’ ‘n.brk.axis’ ‘font.size’
  ‘title’ ‘lab.xy’ ‘strip’ ‘effect’ ‘equal.width’ ‘show.KM’
  ‘show.effect’ ‘conf.int’ ‘show.overall’ ‘palette’ ‘col.power’
Documented arguments not in \usage in documentation object 'plot_circle':
  ‘dat:’ ‘covari.sel:’ ‘trt.sel:’ ‘resp.sel:’ ‘outcome.type:’
  ‘range.v:’ ‘adj.ann.subgrp:’ ‘font.size:’ ‘title:’ ‘lab.xy:’

Undocumented arguments in documentation object 'plot_contour'
  ‘dat’ ‘covari.sel’ ‘trt.sel’ ‘resp.sel’ ‘outcome.type’ ‘setup.ss’
  ‘n.grid’ ‘brk.es’ ‘para.plot’ ‘font.size’ ‘title’ ‘subtitle’
Documented arguments not in \usage in documentation object 'plot_contour':
  ‘dat:’ ‘covari.sel:’ ‘trt.sel:’ ‘resp.sel:’ ‘outcome.type:’
  ‘setup.ss:’ ‘n.grid:’ ‘brk.es:’ ‘para.plot:’ ‘font.size:’ ‘title:’
  ‘subtitle:’

Undocumented arguments in documentation object 'plot_contour_localreg'
  ‘dat’ ‘covari.sel’ ‘trt.sel’ ‘resp.sel’ ‘outcome.type’ ‘setup.ss’
  ‘n.grid’ ‘brk.es’ ‘n.brk.axis’ ‘para.plot’ ‘font.size’ ‘unit.x’
  ‘unit.y’ ‘title’ ‘subtitle’ ‘effect’ ‘show.overall’ ‘strip’
Documented arguments not in \usage in documentation object 'plot_contour_localreg':
  ‘dat:’ ‘covari.sel:’ ‘trt.sel:’ ‘resp.sel:’ ‘outcome.type:’
  ‘setup.ss:’ ‘n.grid:’ ‘brk.es:’ ‘para.plot:’ ‘font.size:’ ‘title:’
  ‘subtitle:’

Undocumented arguments in documentation object 'plot_dissimilarity'
  ‘dat’ ‘covari.sel’ ‘mode’ ‘range.ds’ ‘font.size’ ‘title’ ‘lab.x’
Documented arguments not in \usage in documentation object 'plot_dissimilarity':
  ‘dat:’ ‘covari.sel:’ ‘mode:’ ‘range.ds:’ ‘font.size:’ ‘title:’
  ‘lab.x:’

Undocumented arguments in documentation object 'plot_dissimilarity_alternative'
  ‘dat’ ‘covari.sel’ ‘mode’ ‘range.ds’ ‘font.size’ ‘title’ ‘lab.y’
Documented arguments not in \usage in documentation object 'plot_dissimilarity_alternative':
  ‘dat:’ ‘covari.sel:’ ‘mode:’ ‘range.ds:’ ‘font.size:’ ‘title:’
  ‘lab.y:’

Undocumented arguments in documentation object 'plot_dissimilarity_alternative_new'
  ‘dat’ ‘covari.sel’ ‘mode’ ‘range.ds’ ‘font.size’ ‘title’ ‘lab.y’
Documented arguments not in \usage in documentation object 'plot_dissimilarity_alternative_new':
  ‘dat:’ ‘covari.sel:’ ‘mode:’ ‘range.ds:’ ‘font.size:’ ‘title:’
  ‘lab.y:’

Undocumented arguments in documentation object 'plot_forest'
  ‘dat’ ‘covari.sel’ ‘trt.sel’ ‘resp.sel’ ‘outcome.type’ ‘size.shape’
  ‘font.size’ ‘title’ ‘lab.x’ ‘time’ ‘new’ ‘KM’
Documented arguments not in \usage in documentation object 'plot_forest':
  ‘dat:’ ‘covari.sel:’ ‘trt.sel:’ ‘resp.sel:’ ‘outcome.type:’
  ‘size.shape:’ ‘font.size:’ ‘title:’ ‘lab.x:’

Undocumented arguments in documentation object 'plot_forest2'
  ‘dat’ ‘covari.sel’ ‘trt.sel’ ‘resp.sel’ ‘outcome.type’ ‘size.shape’
  ‘font.size’ ‘title’ ‘lab.x’ ‘time’ ‘pdf’ ‘KM’ ‘show.km.axis’ ‘widths’
  ‘max.time’ ‘n.brk’
Documented arguments not in \usage in documentation object 'plot_forest2':
  ‘dat:’ ‘covari.sel:’ ‘trt.sel:’ ‘resp.sel:’ ‘outcome.type:’
  ‘size.shape:’ ‘font.size:’ ‘title:’ ‘lab.x:’

Undocumented arguments in documentation object 'plot_labbe'
  ‘dat’ ‘covari.sel’ ‘trt.sel’ ‘resp.sel’ ‘outcome.type’ ‘effect’
  ‘size.shape’ ‘adj.ann.subgrp’ ‘font.size’ ‘title’ ‘lab.xy’ ‘show.ci’
Documented arguments not in \usage in documentation object 'plot_labbe':
  ‘dat:’ ‘covari.sel:’ ‘trt.sel:’ ‘resp.sel:’ ‘outcome.type:’
  ‘adj.ann.subgrp:’ ‘size.shape:’ ‘font.size:’ ‘title:’ ‘lab.xy:’

Undocumented arguments in documentation object 'plot_level'
  ‘dat’ ‘covari.sel’ ‘trt.sel’ ‘resp.sel’ ‘outcome.type’ ‘ss.rect’
  ‘range.strip’ ‘n.brk’ ‘n.brk.axis’ ‘font.size’ ‘title’ ‘strip’
  ‘effect’ ‘time’ ‘blow.up’ ‘show.overall’ ‘palette’ ‘col.power’
Documented arguments not in \usage in documentation object 'plot_level':
  ‘dat:’ ‘covari.sel:’ ‘trt.sel:’ ‘resp.sel:’ ‘outcome.type:’
  ‘ss.rect:’ ‘range.strip:’ ‘n.brk:’ ‘font.size:’ ‘title:’ ‘strip:’

Undocumented arguments in documentation object 'plot_matrix_overlap'
  ‘dat’ ‘covari.sel’ ‘mode’ ‘font.size’ ‘title’ ‘new’
Documented arguments not in \usage in documentation object 'plot_matrix_overlap':
  ‘dat:’ ‘covari.sel:’ ‘mode:’ ‘font.size:’ ‘title:’

Undocumented arguments in documentation object 'plot_mosaic'
  ‘dat’ ‘covari.sel’ ‘trt.sel’ ‘resp.sel’ ‘outcome.type’ ‘range.v’
  ‘adj.ann.subgrp’ ‘range.strip’ ‘n.brk’ ‘font.size’ ‘title’ ‘lab.xy’
  ‘strip’ ‘effect’ ‘lwd.’ ‘sep.’ ‘palette’ ‘col.power’
Documented arguments not in \usage in documentation object 'plot_mosaic':
  ‘dat:’ ‘covari.sel:’ ‘trt.sel:’ ‘resp.sel:’ ‘outcome.type:’
  ‘range.v:’ ‘adj.ann.subgrp:’ ‘font.size:’ ‘title:’ ‘lab.xy:’

Undocumented arguments in documentation object 'plot_network'
  ‘dat’ ‘covari.sel’ ‘para’ ‘font.size’ ‘title’
Documented arguments not in \usage in documentation object 'plot_network':
  ‘dat:’ ‘covari.sel:’ ‘para:’ ‘font.size:’ ‘title:’

Undocumented arguments in documentation object 'plot_overlap'
  ‘dat’ ‘covari.sel’ ‘para’ ‘font.size’ ‘title’
Documented arguments not in \usage in documentation object 'plot_overlap':
  ‘dat:’ ‘covari.sel:’ ‘para:’ ‘font.size:’ ‘title:’

Undocumented arguments in documentation object 'plot_overlap2'
  ‘dat’ ‘covari.sel’ ‘para’ ‘font.size’ ‘title’
Documented arguments not in \usage in documentation object 'plot_overlap2':
  ‘dat:’ ‘covari.sel:’ ‘para:’ ‘font.size:’ ‘title:’

Undocumented arguments in documentation object 'plot_overlap_alternative'
  ‘dat’ ‘covari.sel’ ‘para’ ‘mode’ ‘font.size’ ‘title’
Documented arguments not in \usage in documentation object 'plot_overlap_alternative':
  ‘dat:’ ‘covari.sel:’ ‘para:’ ‘mode:’ ‘font.size:’ ‘title:’

Undocumented arguments in documentation object 'plot_overlap_alternative2'
  ‘dat’ ‘covari.sel’ ‘para’ ‘mode’ ‘font.size’ ‘title’
Documented arguments not in \usage in documentation object 'plot_overlap_alternative2':
  ‘dat:’ ‘covari.sel:’ ‘para:’ ‘mode:’ ‘font.size:’ ‘title:’

Undocumented arguments in documentation object 'plot_radial'
  ‘dat’ ‘covari.sel’ ‘trt.sel’ ‘resp.sel’ ‘outcome.type’ ‘range.v’
  ‘adj.ann.subgrp’ ‘font.size’ ‘title’ ‘lab.xy’
Documented arguments not in \usage in documentation object 'plot_radial':
  ‘dat:’ ‘covari.sel:’ ‘trt.sel:’ ‘resp.sel:’ ‘outcome.type:’
  ‘range.v:’ ‘adj.ann.subgrp:’ ‘font.size:’ ‘title:’ ‘lab.xy:’

Undocumented arguments in documentation object 'plot_radial2'
  ‘dat’ ‘covari.sel’ ‘trt.sel’ ‘resp.sel’ ‘outcome.type’ ‘range.v’
  ‘adj.ann.subgrp’ ‘font.size’ ‘title’ ‘lab.xy’ ‘plot.full’
Documented arguments not in \usage in documentation object 'plot_radial2':
  ‘dat:’ ‘covari.sel:’ ‘trt.sel:’ ‘resp.sel:’ ‘outcome.type:’
  ‘range.v:’ ‘adj.ann.subgrp:’ ‘font.size:’ ‘title:’ ‘lab.xy:’

Undocumented arguments in documentation object 'plot_radial3'
  ‘dat’ ‘covari.sel’ ‘trt.sel’ ‘resp.sel’ ‘outcome.type’ ‘range.v’
  ‘adj.ann.subgrp’ ‘font.size’ ‘title’ ‘lab.xy’
Documented arguments not in \usage in documentation object 'plot_radial3':
  ‘dat:’ ‘covari.sel:’ ‘trt.sel:’ ‘resp.sel:’ ‘outcome.type:’
  ‘range.v:’ ‘adj.ann.subgrp:’ ‘font.size:’ ‘title:’ ‘lab.xy:’

Undocumented arguments in documentation object 'plot_stepp'
  ‘dat’ ‘covari.sel’ ‘trt.sel’ ‘resp.sel’ ‘outcome.type’ ‘setup.ss’
  ‘alpha’ ‘font.size’ ‘title’ ‘lab.y’ ‘subtitle’
Documented arguments not in \usage in documentation object 'plot_stepp':
  ‘dat:’ ‘covari.sel:’ ‘trt.sel:’ ‘resp.sel:’ ‘outcome.type:’
  ‘setup.ss:’ ‘alpha:’ ‘font.size:’ ‘title:’ ‘lab.y:’ ‘subtitle:’

Undocumented arguments in documentation object 'plot_tree'
  ‘dat’ ‘covari.sel’ ‘trt.sel’ ‘resp.sel’ ‘outcome.type’ ‘add.aux.line’
  ‘font.size’ ‘title’ ‘lab.y’ ‘text.shift’ ‘keep.y.axis’
Documented arguments not in \usage in documentation object 'plot_tree':
  ‘dat:’ ‘covari.sel:’ ‘trt.sel:’ ‘resp.sel:’ ‘outcome.type:’
  ‘add.aux.line:’ ‘font.size:’ ‘title:’ ‘lab.y:’ ‘is’

Undocumented arguments in documentation object 'plot_venn'
  ‘dat’ ‘covari.sel’ ‘cat.sel’ ‘trt.sel’ ‘resp.sel’ ‘outcome.type’
  ‘outside.area’ ‘range.strip’ ‘n.brk’ ‘font.size’ ‘title’ ‘strip’
  ‘cat.dist’
Documented arguments not in \usage in documentation object 'plot_venn':
  ‘dat:’ ‘covari.sel:’ ‘cat.sel:’ ‘trt.sel:’ ‘resp.sel:’
  ‘outcome.type:’ ‘range.strip:’ ‘n.brk:’ ‘font.size:’ ‘title:’
  ‘strip:’

Undocumented arguments in documentation object 'plot_venn_fill'
  ‘dat’ ‘covari.sel’ ‘cat.sel’ ‘trt.sel’ ‘resp.sel’ ‘outcome.type’
  ‘outside.area’ ‘range.strip’ ‘n.brk’ ‘n.brk.axis’ ‘font.size’ ‘title’
  ‘strip’ ‘effect’ ‘show.overall’ ‘palette’ ‘col.power’
Documented arguments not in \usage in documentation object 'plot_venn_fill':
  ‘dat:’ ‘covari.sel:’ ‘cat.sel:’ ‘trt.sel:’ ‘resp.sel:’
  ‘outcome.type:’ ‘range.strip:’ ‘n.brk:’ ‘font.size:’ ‘title:’
  ‘strip:’

Undocumented arguments in documentation object 'plot_venn_proportional'
  ‘dat’ ‘covari.sel’ ‘cat.sel’ ‘trt.sel’ ‘resp.sel’ ‘outcome.type’
  ‘outside.area’ ‘range.strip’ ‘n.brk’ ‘n.brk.axis’ ‘font.size’ ‘title’
  ‘strip’ ‘fill’ ‘fill.background’ ‘effect’ ‘show.overall’ ‘palette’
  ‘col.power’
Documented arguments not in \usage in documentation object 'plot_venn_proportional':
  ‘dat:’ ‘covari.sel:’ ‘cat.sel:’ ‘trt.sel:’ ‘resp.sel:’
  ‘outcome.type:’ ‘range.strip:’ ‘n.brk:’ ‘font.size:’ ‘title:’
  ‘strip:’

Undocumented arguments in documentation object 'subgroupset'
  ‘outcome.type’ ‘treatment.var’ ‘min.n’ ‘icon’ ‘fill.trt’

Undocumented arguments in documentation object 'subgroupset_transposed'
  ‘outcome.type’ ‘treatment.var’ ‘min.n’ ‘icon’ ‘fill.trt’

Functions with \usage entries need to have the appropriate \alias
entries, and all their arguments documented.
The \usage entries must correspond to syntactically valid R code.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... WARNING
  Warning: package needs dependence on R (>= 2.10)
* checking examples ... ERROR
Running examples in ‘SubgrPlots-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: plot_venn_fill
> ### Title: Venn diagram for subgroup effect size
> ### Aliases: plot_venn_fill
> 
> ### ** Examples
> 
> library(SubgrPlots)
> data(prca)
> plot_venn_fill(prca,
+         covari.sel = c(5, 7, 4),#vars,
+         cat.sel = c(2,2,2),
+         trt.sel = 3,
+         resp.sel = c(1,2),
+         outcome.type = "survival",
+         outside.area = FALSE,
+         range.strip = c(-3, 3),
+         n.brk = 31,
+         n.brk.axis = 7,
+         font.size = c(0.5, 0.5, 0.7, 0.5, 0.6, 0.6),
+         strip = paste("Treatment effect size (log hazard ratio)"), palette = "hcl")
Error in Surv(time, status) : could not find function "Surv"
Calls: plot_venn_fill ... eval -> <Anonymous> -> model.frame.default -> eval -> eval
Execution halted
* DONE
Status: 1 ERROR, 4 WARNINGs, 1 NOTE

See
  ‘/home/nico/Rpackages/SubgrPlots.Rcheck/00check.log’
for details.

checking examples ... ERROR
Running examples in ‘SubgrPlots-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: plot_venn_fill
> ### Title: Venn diagram for subgroup effect size
> ### Aliases: plot_venn_fill
> 
> ### ** Examples
... 8 lines ...
+         resp.sel = c(1,2),
+         outcome.type = "survival",
+         outside.area = FALSE,
+         range.strip = c(-3, 3),
+         n.brk = 31,
+         n.brk.axis = 7,
+         font.size = c(0.5, 0.5, 0.7, 0.5, 0.6, 0.6),
+         strip = paste("Treatment effect size (log hazard ratio)"), palette = "hcl")
Error in Surv(time, status) : could not find function "Surv"
Calls: plot_venn_fill ... eval -> <Anonymous> -> model.frame.default -> eval -> eval
Execution halted

checking dependencies in R code ... WARNING
'::' or ':::' imports not declared from:
  ‘SubgroUpSetR’ ‘rgeos’
'loadNamespace' or 'requireNamespace' call not declared from: ‘rgeos’
Missing or unexported object: ‘grid::gpar.text’
Unavailable namespace imported from by a ':::' call: ‘SubgroUpSetR’
  See the note in ?`:::` about the use of this operator.

checking for missing documentation entries ... WARNING
Undocumented code objects:
  ‘plot_circle_std’ ‘plot_circle_std_by’ ‘plot_mosaic_2’
  ‘plot_mosaic_2_marginal’ ‘plot_mosaic_3’ ‘plot_mosaic_3_noeffect’
All user-level objects in a package should have documentation entries.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.

checking Rd \usage sections ... WARNING
Undocumented arguments in documentation object 'alluvial_new'
  ‘...’ ‘freq’ ‘col’ ‘border’ ‘layer’ ‘hide’ ‘alpha’ ‘gap.width’ ‘xw’
  ‘cw’ ‘blocks’ ‘ordering’ ‘axis_labels’ ‘cex’ ‘cex.axis’ ‘rotate’

Undocumented arguments in documentation object 'alluvial_transposed'
  ‘...’ ‘freq’ ‘col’ ‘border’ ‘layer’ ‘hide’ ‘alpha’ ‘gap.width’ ‘xw’
  ‘cw’ ‘blocks’ ‘ordering’ ‘axis_labels’ ‘cex’ ‘cex.axis’ ‘rotate’
  ‘bottom.mar’

... 203 lines ...
Undocumented arguments in documentation object 'subgroupset'
  ‘outcome.type’ ‘treatment.var’ ‘min.n’ ‘icon’ ‘fill.trt’

Undocumented arguments in documentation object 'subgroupset_transposed'
  ‘outcome.type’ ‘treatment.var’ ‘min.n’ ‘icon’ ‘fill.trt’

Functions with \usage entries need to have the appropriate \alias
entries, and all their arguments documented.
The \usage entries must correspond to syntactically valid R code.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.

checking data for ASCII and uncompressed saves ... WARNING
  Warning: package needs dependence on R (>= 2.10)

checking R code for possible problems ... NOTE
EffectPlotsPlot_t: no visible global function definition for ‘%>%’
EffectPlotsPlot_t: no visible global function definition for ‘mutate’
EffectPlotsPlot_t: no visible binding for global variable ‘x’
IntersectionEffectPlot: no visible global function definition for
  ‘coxph’
IntersectionEffectPlot : <anonymous>: no visible global function
  definition for ‘coxph’
IntersectionEffectPlot2: no visible global function definition for
  ‘coxph’
... 70 lines ...
  ‘QuerieElemAtt’
subgroupset_transposed: no visible global function definition for
  ‘get_shade_groups’
subgroupset_transposed: no visible global function definition for
  ‘Make_set_metadata_plot’
Undefined global functions or variables:
  %>% BoxPlotsPlot CustomAttData ElemBarDat GenerateCustomPlots
  GuideGenerator IntersectionBoxPlot Make_legend Make_set_metadata_plot
  QuerieElemAtt QuerieInterAtt QuerieInterBar QuerieInterData
  SeperateQueries coxph customQueries customQueriesBar elements
  get_shade_groups intersects mutate specific_intersections trt x
R CMD check results
1 error  | 4 warnings | 1 note 

R CMD check succeeded
