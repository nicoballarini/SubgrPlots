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
library(dplyr)
library(grid)
library(gridGraphics)
# Load the data to be used
data(prca)
dat <- prca
setup.ss =  c(10,60,15,30)
dat %>%
  rename(Weight = weight,
         Age = age) -> dat

setEPS()
postscript(file = "paper/figures_eps/03-contour-plot.eps", width = 7.1, height = 3)

graphics::layout(matrix(c(6,6,7,8,8,
                          1,2,3,4,5), nrow=2, ncol=5, byrow = TRUE),
                 heights = c(.075, .925),
                 widths = c(4, .9, .25, 4, .9))



# graphics::layout(matrix(c(1, 2), nrow=1, ncol=2), widths=c(4,1))
plot_contour(dat,
             covari.sel = c(8,9),
             trt.sel = 3,
             resp.sel = c(1,2),
             outcome.type = "survival",
             setup.ss =  setup.ss,
             n.grid = c(100,100),
             brk.es = seq(-4.5,4.5,length.out = 101),
             n.brk.axis =  7,
             para.plot = c(0.5, 2, 6),
             font.size = c(1, 1, .75, 1, .9),
             title = NULL,
             strip = paste("Treatment effect size (log hazard ratio)"),
             show.overall = T,show.points = T,
             filled = T, palette = "hcl", col.power = 0.75,
             new.layout = FALSE)


graphics::par(mar=c(0,0,0,0))
plot(0, type = "n", axes = F, xlab = "", ylab = "")

plot_contour_localreg(dat,
                      covari.sel = c(8,9),
                      trt.sel = 3,
                      resp.sel = c(1,2),
                      n.grid = c(100,100),
                      font.size = c(1, 1, .75, 1, .9),
                      brk.es = seq(-4.5,4.5,length.out = 101),
                      n.brk.axis =  7,
                      strip = "Treatment effect size (log hazard ratio)",
                      outcome.type = "survival",
                      new.layout = FALSE)

gridGraphics::grid.echo()

p = grid.grab()


graphics::par(mar=c(0,0,0,0), xpd=TRUE)
plot(0, 0, type = "n", axes = F, xlab = "", ylab = "")
text(-1, -.5, "(a)", cex = 1.5)
plot(0, 0, type = "n", axes = F, xlab = "", ylab = "")
plot(0, 0, type = "n", axes = F, xlab = "", ylab = "")
text(-1, -.5, "(b)", cex = 1.5)

dev.off()
