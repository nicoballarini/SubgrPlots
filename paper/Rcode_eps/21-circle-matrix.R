pkgroot = rprojroot::find_package_root_file()

library(SubgrPlots) # Loads this package. Install it first
library(survival)
library(ggplot2)
library(dplyr)


# # Load the data to be used
data(prca)
dat <- prca
filename = "21-circle-plot-matrix"
width    = 10
height   = 5
res = 600
setEPS()
postscript(paste0(rprojroot::find_package_root_file(),
                  "/paper/figures_eps/", filename, ".eps"),
           width = width, height = height)
set.seed(12)
plot_circle2(dat,
                                covari.sel = c(4, 5, 6, 7),
                                trt.sel = 3,
                                resp.sel = c(1,2),
                                outcome.type = "survival",
                                range.v = NULL,
                                adj.ann.subgrp = 4,
                                range.strip=c(-3, 3),
                                n.brk = 31,
                                n.brk.axis = 7,
                                font.size = c(1, 1, 0.85, 0.85, 1),
                                title = NULL, lab.xy = NULL,
                                strip = "Treatment effect size (log hazard ratio)",
                                effect = "HR",
                                equal.width = FALSE,
                                show.KM = FALSE,
                                show.effect = TRUE,
                                conf.int = FALSE, palette = "hcl")
dev.off()



filename = "21-circle-plot-matrix-equal"
width    = 10
height   = 5
tiff(paste0(rprojroot::find_package_root_file(),"/paper/figures_tiff/", filename, ".tiff"),
     width = width, height = height, units = "in", res = res)
# pdf("paper/figures/21-circle-plot-matrix-equal.pdf", height = 5, width = 10)
set.seed(12)
plot_circle2(dat,
                                covari.sel = c(4, 5, 6, 7),
                                trt.sel = 3,
                                resp.sel = c(1,2),
                                outcome.type = "survival",
                                range.v = NULL, adj.ann.subgrp = 4,
                                range.strip=c(-3, 3),
                                n.brk = 31,
                                n.brk.axis = 7,
                                font.size = c(1, 1, 0.85, 0.85, 1),
                                title = NULL, lab.xy = NULL,
                                strip = "Treatment effect size (log hazard ratio)",
                                effect = "HR",
                                equal.width = TRUE,
                                show.KM = FALSE,
                                show.effect = TRUE,
                                conf.int = FALSE, palette = "hcl")
dev.off()
