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

# # Load the data to be used
data(prca)
dat <- prca
dat %>%
  mutate(bm = factor(ifelse(bm == 0 , "No", "Yes")),
         hx = factor(ifelse(hx == 0 , "No", "Yes"))) -> dat

# pdf("paper/figures/07-A-tree-plot.pdf", width = 5, height = 4.5)
setEPS()
postscript(file = "paper/figures_eps/07-tree-plot.eps", width = 11, height = 4.5)

grid::grid.newpage()
vp <- grid::viewport(x = 0, y = 0, width = .48, height = 1,
                     just = c("left", "bottom"))
grid::pushViewport(vp)
grid::grid.text(x=0.05, y=.975,
                gp = grid::gpar(cex = 1.5),
                label = "(a)")
plot_tree(dat,
          covari.sel = c(4, 5, 7),
          trt.sel = 3,
          resp.sel = c(1, 2),
          outcome.type = "survival",
          add.aux.line = TRUE,
          font.size = c(12, 12, 0.8),
          text.shift = 0.01,
          title = NULL,
          lab.y = "Effect size (log hazard ratio)",
          keep.y.axis = TRUE,
          grid.newpage = FALSE)


grid::upViewport()
vp <- grid::viewport(x = 1, y = 1, width = .48, height = 1, just = c("right", "top"))
grid::pushViewport(vp)
grid::grid.text(x=0.05, y=.975,
                gp = grid::gpar(cex = 1.5),
                label = "(b)")

plot_tree(dat,
          covari.sel = c(4, 5, 7),
          trt.sel = 3,
          resp.sel = c(1, 2),
          outcome.type = "survival",
          add.aux.line = TRUE,
          font.size = c(12, 12, 0.8),
          title = NULL, text.shift = 0.01,
          lab.y = "Effect size (log hazard ratio)",
          keep.y.axis = FALSE,
          grid.newpage = FALSE)


grid::upViewport()
dev.off()
