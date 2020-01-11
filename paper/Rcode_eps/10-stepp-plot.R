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
###############################################################################-
## 9. stepp Plot -----------------------------------------------------------
# pdf("paper/figures/10-stepp-plot.pdf", width = 5, height = 5)
setEPS()
postscript(file = "paper/figures_eps/10-stepp-plot.eps", width = 5, height = 5)

lab.y.title = paste("Treatment effect size (log-hazard ratio)");
setup.ss = c(30,40)
sub.title = paste0("(Subgroup sample sizes are set to ", setup.ss[2], "; overlap of ", setup.ss[1], ")" )
plot_stepp(dat,
          covari.sel = 8,
          trt.sel = 3,
          resp.sel = c(1, 2),
          outcome.type = "survival",
          setup.ss = c(30,40),
          alpha = 0.05,
          font.size = c(1, 1, 1, 1),
          title = NULL,
          lab.y = lab.y.title,
          subtitle = sub.title)
dev.off()


# pdf("paper/figures/10-ggstepp-plot.pdf", width = 7, height = 5)
setEPS()
postscript(file = "paper/figures_eps/10-ggstepp-plot.eps", width = 7, height = 5)

lab.y.title = paste("Treatment effect size (log-hazard ratio)");
setup.ss = c(30,40)
sub.title = paste0("(Subgroup sample sizes are set to ", setup.ss[2], "; overlap of ", setup.ss[1], ")" )
ggplot_stepp(dat,
             covari.sel = 8,
             trt.sel = 3,
             resp.sel = c(1, 2),
             outcome.type = "survival",
             setup.ss = c(30,40),
             alpha = 0.05,
             title = NULL,
             lab.y = lab.y.title,
             subtitle = sub.title) +
  ggplot2::theme(text = ggplot2::element_text(size = 14))
dev.off()
