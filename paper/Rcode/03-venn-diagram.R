###############################################################################-
##
## This program creates the figures for the manuscript using the
## prca data that is included in this package
##
##
## Instead of using rm(list = ls()), make sure you start with a fresh R
## by restarting R -> Control+Shift+F10
# cat("\014") # Cleans the console
## Load needed libraries
## If SubgrPlots package is not installed, then open project and use following lines or
## in the build window, click Install and Restart
# devtools::build()
# devtools::install()
library(SubgrPlots) # Loads this package. Install it first
library(survival)
library(ggplot2)
library(dplyr)

# # Load the data to be used
data(prca)
dat <- prca
vars = data.frame(variable = names(dat), index = 1:length(names(dat)))

dat
covari.sel = c(5,7, 4)#vars,
cat.sel = c(2,2,2)
trt.sel = 3
resp.sel = c(1, 2)
outcome.type = "survival"
outside.area = FALSE
range.strip = c(-6, 6)
n.brk = 13
font.size = c(0.5, 0.5, 0.7, 0.5, 0.6, 0.6)
main.title = paste("Venm diagram for subgroup overlap");
title = main.title
strip = NULL
library(dplyr)
dat %>%
  rename(Performance = pf,
         `Bone\nMetastasis` = bm,
         `History of\ncardiovascular\nevents` = hx) -> dat
vars
###############################################################################-
## 3. Venn Diagram -----------------------------------------------------------
pdf("paper/figures/03-venn-diagram.pdf", width = 5, height = 5)
# 3 variables
main.title = paste("Venm diagram for subgroup overlap");
plot_venn(dat,
       covari.sel = c(5, 7, 4),#vars,
       cat.sel = c(2,2,2),
       trt.sel = 3,
       resp.sel = c(1,2),
       outcome.type = "survival",
       outside.area = FALSE,
       range.strip = c(-6, 6),
       n.brk = 13, cat.dist = c(0.04,0.04,0.07),
       font.size = c(0.5, 0.5, 0.7, 0.5, 0.6, 0.6),
       title = main.title,
       strip = NULL)
dev.off()
## vd: The package rgeos cannot be installed in my environment so the function does not work. It requires GEOS library.
## May be better to use another library to do the venn diagram like the VennDiagram package

# # 4 variables
# main.title = paste("Venm diagram for subgroup overlap");
# plot_venn(dat,
#        covari.sel = c(4,5,6,7),#vars,
#        cat.sel = c(2,2,2,2),
#        trt.sel = 3,
#        resp.sel = c(1,2),
#        outcome.type = "survival",
#        outside.area = FALSE,
#        range.strip = c(-6, 6),
#        n.brk = 13,
#        font.size = c(0.5, 0.5, 0.7, 0.5, 0.6, 0.6),
#        title = main.title,
#        strip = NULL)
# # 5 variables
# dat$age1 = factor(dat$age1)
# plot_venn(dat,
#           covari.sel = c(4,5,6,7,10),#vars,
#           cat.sel = c(2,2,2,2,1),
#           trt.sel = 3,
#           resp.sel = c(1,2),
#           outcome.type = "survival",
#           outside.area = FALSE,
#           range.strip = c(-6, 6),
#           n.brk = 13,
#           font.size = c(0.5, 0.5, 0.7, 0.5, 0.6, 0.6),
#           title = main.title,
#           strip = NULL)
