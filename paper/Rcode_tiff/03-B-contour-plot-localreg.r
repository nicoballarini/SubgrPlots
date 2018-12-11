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
pkgroot = rprojroot::find_package_root_file()


library(SubgrPlots)
data(prca)
dat = prca
dat %>%
  rename(Weight = weight,
         Age = age) -> dat


filename = "03-B-contour-plot-localreg"
width    = 6
height   = 5
res = 600
tiff(paste0(rprojroot::find_package_root_file(),"/paper/figures_tiff/", filename, ".tiff"),
     width = width, height = height, units = "in", res = res)
# postscript("paper/figures_eps/03-B-contour-plot-localreg.eps", width = 6, height = 5)
# pdf("paper/figures/03-B-contour-plot-localreg.pdf", width = 6, height = 5)
plot_contour_localreg(dat,
               covari.sel = c(8,9),
               trt.sel = 3,
               resp.sel = c(1,2),
               n.grid = c(100,100),
               font.size = c(1, 1, 1, 1, 1),
               brk.es = seq(-4.5,4.5,length.out = 101),
               n.brk.axis =  7,
               strip = "Treatment effect size (log hazard ratio)",
               outcome.type = "survival")
dev.off()
