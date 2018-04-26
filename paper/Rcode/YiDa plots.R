###############################################################################-
##
## This program creates the figures for the manuscript using the
## prca data that is included in this package
##
##

## Instead of using rm(list = ls()), make sure you start with a fresh R
## by restarting R -> Control+Shift+F10
cat("\014") # Cleans the console
## Load needed libraries
## If SubgrPlots package is not installed, then open project and use following lines or
## in the build window, click Install and Restart
# devtools::build()
# devtools::install()

library(SubgrPlots) # Loads this package. Install it first
library(survival)
library(ggplot2)
library(dplyr)


# Removing Rplot files in top folder
all.files  = list.files()
plot.files = all.files[grep(pattern = "Rplot",x = all.files)]
file.remove(plot.files)

# # Load the data to be used
data(prca)
dat <- prca
vars = data.frame(variable = names(dat), index = 1:length(names(dat)))

dat
covari.sel = c(14,15)
trt.sel = 3
resp.sel = c(1, 2)
outcome.type = "survival"

###############################################################################-
## 1. Level plot --------------------------------------------------------
main.title = paste("Treatment effect sizes across subgroups (N = 475)", sep = "");
strip.title = paste("Treatment effect size (log hazard ratio)");
lvplt(dat,
      covari.sel = c(14,15),
      trt.sel = 3,
      resp.sel = c(1, 2),
      outcome.type = "survival",
      ss.rect = FALSE,
      range.strip=c(-3, 3),
      n.brk = 7,
      font.size = c(22, 20, 1.5, 20, 1.5),
      title = main.title,
      strip = strip.title)
dev.off()


## General Comment: The functions open a new device via dev.new() with the option noRStudioGD = TRUE.
## The plot is then not shown, at least in my environment, until I call dev.off() and a new file Rplots##.pdf is generated in the working directory.
## It may be more convenient to show the plot in the Rstudio's Plots tab.
## General Comment 2: the name of the functions are not consistent. Some have .plt, others no separation.
## lvplt: The font.size is not consistent in the values it take. In the default values c(15, 12, 0.8, 15, 0.6), why for positions 3 and 5 we have such a small numbers? are they the relative size compared to the others?


###############################################################################-
# 2. Contour plot -----------------------------------------------------------
covari.sel = c(9,8)
main.title = paste("Effect sizes (ES) on the plane of ", names(dat)[covari.sel[1]], "and", names(dat)[covari.sel[2]]) ;
setup.ss = c(35,40,15,20);
sub.title = paste("(N1 approx.", setup.ss[1], "; N2 approx.", setup.ss[2], "; N3 approx.", setup.ss[3], "; N4 approx.", setup.ss[4], ")" )
contourplt(dat,
           covari.sel = c(9,8),
           trt.sel = 3,
           resp.sel = c(1,2),
           outcome.type = "survival",
           setup.ss =  setup.ss,
           n.grid = c(10, 10),
           brk.es = c(-1,-0.5, 0, 0.5, 1),
           para.plot = c(0.35, 2, 20),
           font.size = c(2, 1.5, 1.2, 1.3, 1),
           title = main.title,
           subtitle = sub.title)
dev.off()

## contour.plt: It may be very difficult to come up with sensitive values for the subgroups: n1 n2 n3 and n4.
## Check for best practices when using dependencies. Should we call packages inside the functions? or load the dependencies when the package is loaded.
## Legend inside plot may overlap with points
## Font.size not consistent. No option for tickmarks

###############################################################################-
## 3. Venn Diagram -----------------------------------------------------------
# source("YiDa_R/venn_diag_function.r")
main.title = paste("Venm diagram for subgroup overlap");
vd_new(dat,
   covari.sel = c(4,5,7),#vars,
   cat.sel = c(2,2,2),
   trt.sel = 3,
   resp.sel = c(1,2),
   outcome.type = "survival",
   outside.area = FALSE,
   range.strip = c(-6, 6),
   n.brk = 13,
   font.size = c(1, 1.5, 1, 0.9, 1, 1),
   title = main.title,
   strip = NULL)
dev.off()
## vd: The package rgeos cannot be installed in my environment so the function does not work. It requires GEOS library.
## May be better to use another library to do the venn diagram like the VennDiagram package


###############################################################################-
## 4. Bar chart -----------------------------------------------------------
# source("YiDa_R/barchart_function.r")
main.title = paste("Treatment effect sizes across subgroups", sep = "");
lab.y.title = paste("Treatment effect size (log hazard ratio)");
barcht(dat,
       covari.sel = c(14,15),
       trt.sel = 3,
       resp.sel = c(1, 2),
       outcome.type = "survival",
       font.size = c(22, 20, 16, 1),
       title = main.title,
       lab.y = lab.y.title)
dev.off()

## barcht: When using survival data, the y scale is in terms of the log hazard ratio. It may be useful to provide an option to show hazard ratio?


###############################################################################-
## 5. Forest Plot -----------------------------------------------------------
# source("YiDa_R/forest_plot_function.r")
main.title = list("", "Forestplot of subgroups", "Forestplot of subgroups (T | C)")
label.x = list("", "Log hazard ratio", "")
forestplt(dat,
          covari.sel = c(4,5,6,7),#vars
          trt.sel = 3,
          resp.sel = c(1, 2),
          outcome.type = "survival",
          size.shape = c(0.25, 0.12),
          font.size = c(2, 2, 1, 1.5),
          title = main.title,
          lab.x = label.x)
dev.off()

## forestplt: It does not display the subgroup effects panel

###############################################################################-
# ## 5. Forest Plot with combined plot -------------------------------------------------------
# source("YiDa_R/mat_ovlp.r")
# source("YiDa_R/image.scale.r")
# # produce a new plot window
# dev.new(width=10,height=10,noRStudioGD = TRUE)
# # make layout setup
# layout(matrix(c(1,2,3, 1,2,3, 1,2,3, 4, 4, 4, 4, 4, 4,
#                 4, 4, 4, 0, 5, 0), byrow = TRUE, nrow= 7, ncol=3), widths=c(1,1, 1), heights=c(2.5, 2.5, 2.5, 2, 2, 2, 1))
#
# main.title = list("", "Forestplot of subgroups", "Forestplot of subgroups (T | C)")
# label.x = list("", "Log odds ratio", "Log odds")
# forestplt(dat,
#           covari.sel = c(14,15),
#           trt.sel = 3,
#           resp.sel = c(1, 2),
#           outcome.type = "survival")
# main.title = "Relative overlapping proportions of pairwise subgroups";
# mat.subgrp.op(dat, c(14,15), mode = 1, font.size = c(1.2, 1), title = main.title)
# dev.off()
##image.scale has to be sourced! if using R package this is not necessary

###############################################################################-
## 6. Tree Plot -----------------------------------------------------------
# source("YiDa_R/tree_plot_function.r")
main.title = paste("Tree plot for treatment effect difference across subgroups", sep = "");
lab.y.title = paste("Effect size (log hazard ratio)");
treeplt(dat,
        covari.sel = c(4, 5, 7),
        trt.sel = 3,
        resp.sel = c(1, 2),
        outcome.type = "survival",
        add.aux.line = TRUE,
        font.size = c(22, 16, 0.8),
        title = main.title,
        lab.y = lab.y.title)
dev.off()


###############################################################################-
## 7. Radial Plot -----------------------------------------------------------
# source("YiDa_R/radial_plot_function.r")
main.title = "Radial plot"
label.xy = list("1/SE", "Standardized effect size (log hazard ratio) difference   ")
radplt(dat,
       covari.sel = c(4, 5, 6),
       trt.sel = 3,
       resp.sel = c(1, 2),
       outcome.type = "survival",
       range.v = c(-14, 4),
       adj.ann.subgrp = 4,
       font.size = c(2, 1.8, 1.4, 1, 1),
       title = main.title, lab.xy = label.xy)
dev.off()


###############################################################################-
## 8. Labbe Plot -----------------------------------------------------------
# source("YiDa_R/labbe_plot_function.r")
### Do not support SURVIVAL!!!
# labbeplt(dat,
#           covari.sel = c(14,15),
#           trt.sel = 3,
#           resp.sel = c(1, 2),
#           outcome.type = "survival")
# dev.off()

## The Labbe plot currently does not support survival outcome

###############################################################################-
## 9. stepp Plot -----------------------------------------------------------
# source("YiDa_R/stepp_plot_function.r")
var.sel = 8
main.title = paste("STEPP for treatment effect size of overlapping subgroups defined by", names(dat)[var.sel]);
lab.y.title = paste("Treatment effect diffence");
sub.title = paste("(Subgroup sample sizes are set to", setup.ss[2], "; overlap sizes are set to", setup.ss[1], ")" )
setup.ss = c(35, 40);
stepp.plt(dat,
          covari.sel = 8,
          trt.sel = 3,
          resp.sel = c(1, 2),
          outcome.type = "survival",
          setup.ss = c(35,40),
          alpha = 0.05,
          font.size = c(1.7, 1.5, 1.4, 1),
          title = main.title,
          lab.y = lab.y.title,
          subtitle = sub.title)
dev.off()







###############################################################################-
## ***Improvement and alternatives*** -----

## 10 Modified Level plot --------------------------------------------------------
main.title = paste("Treatment effect sizes across subgroups (N = 475)", sep = "");
strip.title = paste("Treatment effect size (log hazard ratio)");
lvplt(dat,
      covari.sel = c(14,15),
      trt.sel = 3,
      resp.sel = c(1, 2),
      outcome.type = "survival",
      ss.rect = TRUE,
      range.strip=c(-3, 3),
      n.brk = 7,
      font.size = c(22, 20, 1.5, 20, 1.5),
      title = main.title,
      strip = strip.title)
dev.off()



###############################################################################-
# TEST ---------
# Alluvial diagrams ----
# Put labels to the variables so that they appear in the plot
dat %>%
  dplyr::select(rx, bm, hx) %>%
  dplyr::group_by(rx, bm, hx) %>%
  dplyr::summarise(freq = n()) -> alldat

alluvial::alluvial(alldat[,1:3], freq = alldat$freq,
                   alpha=1, xw=0.2,
                   col=ifelse( alldat$rx == 1, "red", "gray"),
                   layer = alldat$rx != 1)


pdf("Rplots20.pdf", width = 10, height = 10)
dat %>%
  dplyr::select(rx, bm, hx, pf, stage) %>%
  dplyr::group_by(rx, bm, hx, pf, stage) %>%
  dplyr::summarise(Freq = n()) -> alldat
alluvial::alluvial(alldat[,1:5], freq = alldat$Freq,
                   xw=0.2,
                   alpha  = 0.8,
                   col=ifelse( alldat$rx == 1, "#fc8d59", "#91bfdb"),
                   layer = alldat$rx == 1)
dev.off()

pdf("Rplots20.pdf", width = 10, height = 10)
pal <- RColorBrewer::brewer.pal(5, 'RdYlBu')
alluvial::alluvial(alldat[,1:5],
                   freq   = alldat$Freq,
                   col    = ifelse(alldat$rx == 1, pal[1], pal[5]),
                   border = ifelse(alldat$rx == 1, pal[1], pal[5]),
                   # layer,
                   hide = FALSE,
                   alpha = 0.35,
                   gap.width = 0.05,
                   xw = 0.15,
                   cw = 0.05,
                   blocks = TRUE,
                   ordering = NULL,
                   axis_labels = NULL,
                   cex = par("cex"),
                   cex.axis = par("cex.axis"))
dev.off()


# library(ggalluvial)
# ggplot(as.data.frame(alldat),
#          aes(weight = Freq,
#              axis1 = hx, axis2 = bm, axis3 = pf, axis4 = stage, axis5 = trt)) +
#   # geom_flow() +
#   # geom_flow(aes(fill = trt)) +
#   geom_alluvium(aes(fill = trt),  width = 0, reverse = TRUE) +
#   geom_stratum(width = 1/8, reverse = TRUE) +
#   guides(fill = FALSE) +
#   geom_text(stat = "stratum", label.strata = TRUE, reverse = TRUE) +
#   # scale_x_continuous(breaks = 1:3, labels = c("Survived", "Sex", "Class")) +
#   coord_flip() +
#   ggtitle("Titanic survival by class and sex")
#
#
# ggplot(as.data.frame(alldat),
#        aes(weight = Freq,
#            axis1 = hx, axis2 = bm, axis3 = trt)) +
#   # geom_alluvium(aes(fill = trt), width = 1/12) +
#   geom_flow(aes(fill = trt), width = 1/12) +
#   geom_stratum(width = 1/12, fill = "black", color = "grey") +
#   geom_label(stat = "stratum", label.strata = TRUE) +
#   scale_x_continuous(breaks = 1:2, labels = c("Gender", "Dept")) +
#   coord_flip() +
#   theme_void()
#   ggtitle("UC Berkeley admissions and rejections, by sex and department")
#
#   ggplot(as.data.frame(Titanic),
#          aes(weight = Freq,
#              axis1 = Class, axis2 = Sex)) +
#     geom_flow(aes(fill = Age)) +
#     geom_stratum() + geom_text(stat = "stratum", label.strata = TRUE) +
#     scale_x_continuous(breaks = 1:2, labels = c("Class", "Sex")) +
#     facet_wrap(~ Survived, scales = "fixed")



###############################################################################-
# ## Riverplot package ----
# riverplot::riverplot.example()
# x <- riverplot::riverplot.example()
# plot(x)
# plot(x, srt=90, lty=1)



###############################################################################-
# END OF PLOTS ----

# Removing Rplot files in top folder
all.files = list.files()
plot.files = all.files[grep(pattern = "Rplot",x = all.files)]
file.remove(paste0("paper/figures/",plot.files))
file.copy(from = plot.files, to = paste0("paper/figures/",plot.files))
file.remove(plot.files)
