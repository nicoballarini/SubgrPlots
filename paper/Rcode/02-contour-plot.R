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
covari.sel = c(9,8)
trt.sel = 3
resp.sel = c(1, 2)
outcome.type = "survival"
n.grid = c(10, 10)
brk.es = c(-1,-0.5, 0, 0.5, 1)
para.plot = c(0.35, 2, 20)
font.size = c(0.6, 0.4, 0.3, 0.5, 0.3)
covari.sel = c(9,8)
main.title = paste0("Effect sizes (ES) on the plane of ", names(dat)[covari.sel[1]], " and ", names(dat)[covari.sel[2]]) ;
setup.ss =  c(10,60,15,30)
sub.title = bquote(N[11] %~~% .(setup.ss[2]) ~", "~
                     N[12] %~~% .(setup.ss[1]) ~", "~
                        N[21] %~~% .(setup.ss[4]) ~", "~
                          N[22] %~~% .(setup.ss[3]))
main.title = NULL
###############################################################################-
# 2. Contour plot -----------------------------------------------------------
pdf("paper/figures/02-contour-plot.pdf", width = 6, height = 6)
contourplt_new(dat,
               covari.sel = c(8,9),
               trt.sel = 3,
               resp.sel = c(1,2),
               outcome.type = "survival",
               setup.ss =  setup.ss,
               n.grid = c(100,100),
               brk.es = c(-2, -1, 0, 1, 2),
               para.plot = c(0.5, 2, 5),
               font.size = c(1, 1, 0.7, 0.7, 0.75),
               title = main.title,
               subtitle = sub.title)
dev.off()

pdf("paper/figures/02-contour-plot-filled.pdf", width = 6, height = 5)
contourplt_new(dat,
               covari.sel = c(8,9),
               trt.sel = 3,
               resp.sel = c(1,2),
               outcome.type = "survival",
               setup.ss =  setup.ss,
               n.grid = c(100,100),
               # brk.es = c(-2, -1, 0, 1, 2),
               brk.es = seq(-3,3,length.out = 31),
               n.brk.axis =  7,
               para.plot = c(0.5, 2, 6),
               font.size = c(1, 1, 0.7, 0.7, 0.75),
               title = main.title,
               subtitle = sub.title,
               strip = paste("Treatment effect size (log hazard ratio)"),
               filled = T)
dev.off()

pdf("paper/figures/02-contour-plot-filled-overall.pdf", width = 6, height = 5)
contourplt_new(dat,
               covari.sel = c(8,9),
               trt.sel = 3,
               resp.sel = c(1,2),
               outcome.type = "survival",
               setup.ss =  setup.ss,
               n.grid = c(100,100),
               # brk.es = c(-2, -1, 0, 1, 2),
               brk.es = seq(-3,3,length.out = 101),
               n.brk.axis =  7,
               para.plot = c(0.5, 2, 6),
               font.size = c(1, 1, 0.7, 0.7, 0.75),
               title = main.title,
               subtitle = sub.title,
               strip = paste("Treatment effect size (log hazard ratio)"),show.overall = T,
               filled = T, palette = "hcl",col.power = 0.75)
dev.off()

pdf("paper/figures/02-contour-plot-filled-overall-continuous.pdf", width = 6, height = 5)
contourplt_new(dat,
               covari.sel = c(8,9),
               trt.sel = 3,
               resp.sel = c(1,2),
               outcome.type = "survival",
               setup.ss =  setup.ss,
               n.grid = c(100,100),
               # brk.es = c(-2, -1, 0, 1, 2),
               brk.es = seq(-3,3,length.out = 31),
               n.brk.axis =  7,
               para.plot = c(0.5, 2, 6),
               font.size = c(1, 1, 0.7, 0.7, 0.75),
               title = main.title,
               subtitle = sub.title,
               strip = paste("Treatment effect size (log hazard ratio)"),
               filled = T, show.overall = TRUE, palette = "continuous")
dev.off()
