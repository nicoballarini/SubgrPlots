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
library(gridGraphics)
# Load the data to be used
data(prca)
dat <- prca
dat %>%
  rename(Stage = stage,
         Performance = pf,
         `Bone\nmetastasis` = bm,
         `History of\ncardiovascular\nevents` = hx) -> dat
###############################################################################-
## 3. Venn Diagram -----------------------------------------------------------


layout(matrix(c(5, 1, 2, 3, 4), nrow = 1, ncol = 5),
       widths=c(4, 4, 1, 4, 1))
plot_venn(dat,
          covari.sel = c(4,6,7,5),#vars,
          cat.sel = c(2,2,2,2),
          trt.sel = 3,
          resp.sel = c(1,2),
          outcome.type = "survival",
          fill = TRUE,
          range.strip = c(-3, 3),
          n.brk = 31, n.brk.axis = 7,
          font.size = c(1, 1.7, 1.75, 1.75, 1.75, 1.75),
          strip = paste("Treatment effect size (log hazard ratio)"),
          palette = "hcl",
          cat.dist = c(0.22, 0.22, 0.11, 0.16),
          grid.newpage = FALSE)


dat <- prca
dat %>%
  rename(Stage = stage,
         Performance = pf,
         `Bone\nmetastasis` = bm,
         `History of\ncardiovascular events` = hx) -> dat
plot_venn(dat,
          covari.sel = c(5,7,4),
          cat.sel = c(2,2,2),
          trt.sel = 3,
          resp.sel = c(1,2),
          outcome.type = "survival",
          fill = TRUE,
          range.strip = c(-3, 3),
          n.brk = 31, n.brk.axis = 7,
          font.size = c(1, 1.7, 1.75, 1.75, 1.75, 1.75),
          strip = paste("Treatment effect size (log hazard ratio)"),
          palette = "hcl", prop_area = TRUE,
          grid.newpage = FALSE)
gridGraphics::grid.echo()
p = grid.grab()


# pdf("paper/figures_eps/04-A-venn-diagram.pdf", width = 15, height = 5)
# setEPS()
# postscript(file = "paper/figures_eps/04-venn-diagram.eps", width = 15, height = 4.5,
#            fonts = "sans")
cairo_ps(file = "paper/figures_eps/04-venn-diagram.eps",
         width = 15, height = 4.5, onefile = FALSE, fallback_resolution = 600)

grid::grid.newpage()
vp <- grid::viewport(x = 0, y = 0, width = 1, height = .925, just = c("left", "bottom"))
grid::pushViewport(vp)
# grid::grid.text(x=0.05, y=.95,
#                 gp = grid::gpar(cex = 1.5),
#                 label = "b)")
grid.draw(p)
# grid::upViewport()
vp <- grid::viewport(x = 0, y = 0, width = .28, height = 1,
                     just = c("left", "bottom"))
grid::pushViewport(vp)
dat <- prca
dat %>%
  rename(Performance = pf,
         `Bone\nmetastasis` = bm,
         `History of\ncardiovascular\nevents` = hx) -> dat

plot_venn(dat,
          covari.sel = c(5, 7, 4),
          cat.sel  = c(2,2,2),
          trt.sel  = 3,
          resp.sel = c(1,2),
          outcome.type = "survival",
          fill      = FALSE,
          cat.dist  = c(.03,.04,0.08),
          font.size = c(1, 1.1, 1.3, 1, 1, 1),
          grid.newpage = FALSE)

grid::upViewport()
grid::upViewport()

grid::grid.text(x=0.015, y=.9725,
                gp = grid::gpar(cex = 2),
                label = "(a)")
grid::grid.text(x=0.300, y=.9725,
                gp = grid::gpar(cex = 2),
                label = "(b)")
grid::grid.text(x=0.655, y=.9725,
                gp = grid::gpar(cex = 2),
                label = "(c)")

dev.off()
