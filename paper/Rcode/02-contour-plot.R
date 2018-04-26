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
###############################################################################-
# 2. Contour plot -----------------------------------------------------------
# pdf("paper/figures/02-contour-plot-gg.pdf", width = 5, height = 5)
# covari.sel = c(9,8)
# main.title = paste0("Effect sizes (ES) on the plane of ", names(dat)[covari.sel[1]], " and ", names(dat)[covari.sel[2]]) ;
# setup.ss = c(35,40,15,20);
# sub.title = paste0("(N1 approx. ", setup.ss[1], "; N2 approx. ", setup.ss[2], "; N3 approx. ", setup.ss[3], "; N4 approx. ", setup.ss[4], ")" )
# plot_contour(dat,
#              covari.sel = c(8,9),
#              trt.sel = 3,
#              resp.sel = c(1,2),
#              outcome.type = "survival",
#              setup.ss =  setup.ss,
#              n.grid = c(10, 10),
#              brk.es = c(-1, 0, 1),
#              para.plot = c(0.35, 2, 4),
#              font.size = c(1, 0.8, 0.5, 0.5, 0.5),
#              title = main.title,
#              subtitle = sub.title)
# dev.off()

## contour.plt: It may be very difficult to come up with sensitive values for the subgroups: n1 n2 n3 and n4.
## Check for best practices when using dependencies. Should we call packages inside the functions? or load the dependencies when the package is loaded.
## Legend inside plot may overlap with points
## Font.size not consistent. No option for tickmarks
# contourplt_new(dat,
#                covari.sel = c(8,9),
#                trt.sel = 3,
#                resp.sel = c(1),
#                outcome.type = "continuous",
#                setup.ss =  setup.ss,
#                n.grid = c(5, 5),
#                brk.es = c(-2, -1, 0, 1, 2),
#                para.plot = c(0.35, 2, 6),
#                font.size = c(1, 1, 0.7, 0.7, 0.75),
#                title = main.title,
#                subtitle = sub.title)


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
# pdf("paper/figures/02-contour-plot-filled-overall.pdf", width = 6, height = 5)
# contourplt_new(dat,
#                covari.sel = c(8,9),
#                trt.sel = 3,
#                resp.sel = c(1,2),
#                outcome.type = "survival",
#                setup.ss =  setup.ss,
#                n.grid = c(100,100),
#                # brk.es = c(-2, -1, 0, 1, 2),
#                brk.es = seq(-3,3,length.out = 31),
#                n.brk.axis =  7,
#                para.plot = c(0.5, 2, 6),
#                font.size = c(1, 1, 0.7, 0.7, 0.75),
#                title = main.title,
#                subtitle = sub.title,
#                strip = paste("Treatment effect size (log hazard ratio)"),
#                filled = T)
# dev.off()
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
# contourplt_new(dat,
#                covari.sel = c(8,9),
#                trt.sel = 3,
#                resp.sel = c(1,2),
#                outcome.type = "survival",
#                setup.ss =  c(10,100,15,20),
#                n.grid = c(100,100),
#                brk.es = c(-2, -1, 0, 1, 2),
#                para.plot = c(0.5, 2, 6),
#                font.size = c(1, 1, 0.7, 0.7, 0.75),
#                title = main.title,
#                subtitle = sub.title, filled = T, spiral = TRUE) -> pp
# pp +
#   theme_void() +
#   theme(panel.background = element_rect(fill = "gray95"))
#
# ggsave("plot1.png", width = unit(7.3,"cm"),
#        height =  unit(20,"cm"))
