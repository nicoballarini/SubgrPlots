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


library(SubgrPlots) # Loads this package. Install it first
library(dplyr)

# Load the data to be used
data(prca)
dat <- prca
setup.ss =  c(10,60,15,30)
sub.title = bquote(N[11] %~~% .(setup.ss[2]) ~", "~
                     N[12] %~~% .(setup.ss[1]) ~", "~
                     N[21] %~~% .(setup.ss[4]) ~", "~
                     N[22] %~~% .(setup.ss[3]))
dat %>%
  rename(Weight = weight,
         Age = age) -> dat

filename = "03-A-contour-plot"
width    = 6
height   = 5
res = 600
tiff(paste0(rprojroot::find_package_root_file(),"/paper/figures_tiff/", filename, ".tiff"),
     width = width, height = height, units = "in", res = res)
# postscript("paper/figures_eps/03-A-contour-plot.eps", width = 6, height = 5)
# pdf("paper/figures/03-A-contour-plot.pdf", width = 6, height = 5)
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
                font.size = c(1, 1, 1, 1, 1),
                title = NULL,
                subtitle = sub.title,
                strip = paste("Treatment effect size (log hazard ratio)"),
                show.overall = T,show.points = T,
                filled = T, palette = "hcl",col.power = 0.75)
dev.off()
