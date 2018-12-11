###############################################################################-
## This program creates the figures for the manuscript using the
## prca data that is included in this package
##
##
## Instead of using rm(list = ls()), make sure you start with a fresh R
## by restarting R -> Control+Shift+F10

## Load needed libraries
## If SubgrPlots package is not installed, then open project and use following
## lines or in the build window, click Install and Restart
# devtools::build()
# devtools::install()

pkgroot = rprojroot::find_package_root_file()


library(SubgrPlots) # Loads this package. Install it first
# Load the data to be used
data(prca)
dat <- prca
vars <- data.frame(variable = names(dat), index = 1:length(names(dat)))
levels(dat$age_group) <- c("Young", "Middle-aged", "Old")
levels(dat$weight_group) <- c("Low", "Mid", "High")
names(dat)[c(14,15)] <- c("age", "weight")

###############################################################################-
## 1. Level plot ---------------------------------------------------------------
filename = "01-A-level-plot"
width    = 5
height   = 5
res = 600
tiff(paste0(rprojroot::find_package_root_file(),"/paper/figures_tiff/", filename, ".tiff"),
     width = width, height = height, units = "in", res = res)
# postscript("paper/figures_eps/01-A-level-plot.eps", width = 5, height = 5)
# pdf("paper/figures/01-A-level-plot.pdf", width = 5, height = 5)
main.title = "Treatment effect sizes across subgroups (N = 475)"
strip.title = "Treatment effect size (log hazard ratio)"
plot_level(dat,
           covari.sel = c(14,15),
           trt.sel = 3,
           resp.sel = c(1, 2),
           outcome.type = "survival",
           ss.rect = FALSE,
           range.strip=c(-3, 3),
           n.brk = 31,
           n.brk.axis =  7,
           font.size = c(14, 12, .8, 14, 0.7),
           title = "Total sample size = 475",
           strip = strip.title, effect = "HR",
           show.overall = TRUE, palette = "hcl")
dev.off()
