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

library(SubgrPlots) # Loads this package. Install it first
# Load the data to be used
data(prca)
dat <- prca
vars <- data.frame(variable = names(dat), index = 1:length(names(dat)))
levels(dat$age_group) <- c("Young", "Middle-aged", "Old")
levels(dat$weight_group) <- c("Low", "Mid", "High")
names(dat)[c(14,15)] <- c("Age", "Weight")

main.title = "Treatment effect sizes across subgroups (N = 475)"
strip.title = "Treatment effect size (log hazard ratio)"
###############################################################################-
## 1. Level plot ---------------------------------------------------------------
setEPS()
postscript(file = "paper/figures_eps/01-level-plot.eps", width = 11, height = 5)

grid::grid.newpage()
vp <- grid::viewport(x = 0, y = 0, width = .48, height = 1,
                     just = c("left", "bottom"))
grid::pushViewport(vp)
grid::grid.text(x=0.05, y=.95,
                gp = grid::gpar(cex = 1.5),
                label = "(a)")
plot_level(dat,
           covari.sel = c(14,15),
           trt.sel = 3,
           resp.sel = c(1, 2),
           outcome.type = "survival",
           ss.rect = FALSE,
           range.strip=c(-3, 3),
           n.brk = 31,
           n.brk.axis =  7,
           font.size = c(14, 12, 1, 14, 1),
           title = "Total sample size = 475",
           strip = strip.title, effect = "HR",
           show.overall = TRUE, palette = "hcl",
           grid.newpage = FALSE)
grid::upViewport()
vp <- grid::viewport(x = 1, y = 1, width = .48, height = 1, just = c("right", "top"))
grid::pushViewport(vp)
grid::grid.text(x=0.05, y=.95,
                gp = grid::gpar(cex = 1.5),
                label = "(b)")
plot_level(dat,
           covari.sel = c(14,15),
           trt.sel = 3,
           resp.sel = c(1, 2),
           outcome.type = "survival",
           ss.rect = TRUE,
           range.strip=c(-3, 3),
           n.brk = 31,
           n.brk.axis =  7,
           font.size = c(14, 12, 1, 14, 1),
           title = paste0("Total sample size = ", nrow(dat)),
           strip = strip.title, show.overall = TRUE, palette = "hcl",
           grid.newpage = FALSE)
grid::upViewport()
dev.off()
