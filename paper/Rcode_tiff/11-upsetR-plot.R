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
library(UpSetR)

# Load the data to be used
data(prca)
dat <- prca
###############################################################################-
## 11. UpSet -----------------------------------------------------------
prca.upset = data.frame(trt = factor(ifelse(prca$rx == 1, "Experimental", "Control")),
                        bm = 1*(prca$bm == 1),
                        pf = 1*(prca$pf == 1),
                        hx = 1*(prca$hx == 1),
                        stage = 1*(prca$stage == 4),
                        age = 1*(prca$age > 75),
                        wt = 1*(prca$weight > 100),
                        survtime = prca$survtime,
                        cens = prca$cens==1)

# Creating a custom query to operate on the rows of the data.
Myfunc <- function(row, param1, param2) {
  data <- (row["trt"] %in% c(param1, param2))
}
pal <- c("#1f78b4", "#a6cee3")


filename = "11-upSetR-plot"
width    = 6
height   = 5
res = 600
tiff(paste0(rprojroot::find_package_root_file(),"/paper/figures_tiff/", filename, ".tiff"),
     width = width, height = height, units = "in", res = res)
# postscript("paper/figures_eps/11-upSetR-plot.eps", width = 6, height = 5)
# pdf("paper/figures/11-upSetR-plot.pdf", width = 6, height = 5, onefile=FALSE)
upset(prca.upset,
      order.by = "freq",
      # empty.intersections = "on",
      sets = c("bm",'pf',"hx",'stage',"age","wt"),
      nintersects = 14,
      text.scale = 1.4,
      queries = list(list(query = Myfunc,
                          params = c("Control", "Experimental"),
                          color = pal[2],
                          active = T,
                          query.name = "Control"),
                     list(query = Myfunc,
                          params = c("Experimental", "Experimental"),
                          color = pal[1],
                          active = T,
                          query.name = "Experimental")),
      query.legend = "top")
dev.off()
