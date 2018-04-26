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
vars = data.frame(variable = names(dat), index = 1:length(names(dat)))
levels(dat$age_group) = c("Young","Middle-aged","Old")
levels(dat$weight_group)  = c("Low","Mid","High")

covari.sel = c(14,15)
trt.sel = 3
resp.sel = c(1, 2)
outcome.type = "survival"

names(dat)[covari.sel] = c("age", "weight")

###############################################################################-
## 1. Level plot ---------------------------------------------------------------
pdf("paper/figures/01-level-plot-hr.pdf", width = 5, height = 5)
main.title = paste("Treatment effect sizes across subgroups (N = 475)", sep = "");
strip.title = paste("Treatment effect size (log hazard ratio)");
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
      strip = strip.title, effect = "HR")
dev.off()
# pdf("paper/figures/01-level-plot-hr-overall.pdf", width = 5, height = 5)
# main.title = paste("Treatment effect sizes across subgroups (N = 475)", sep = "");
# strip.title = paste("Treatment effect size (log hazard ratio)");
# plot_level(dat,
#            covari.sel = c(14,15),
#            trt.sel = 3,
#            resp.sel = c(1, 2),
#            outcome.type = "survival",
#            ss.rect = FALSE,
#            range.strip=c(-3, 3),
#            n.brk = 31,
#            n.brk.axis =  7,
#            font.size = c(14, 12, .8, 14, 0.7),
#            title = "Total sample size = 475",
#            strip = strip.title, effect = "HR", show.overall = TRUE)
# dev.off()

pdf("paper/figures/01-level-plot-hr-overall.pdf", width = 5, height = 5)
main.title = paste("Treatment effect sizes across subgroups (N = 475)", sep = "");
strip.title = paste("Treatment effect size (log hazard ratio)");
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

pdf("paper/figures/01-level-plot-rmst.pdf", width = 5, height = 5)
strip.title = paste("Treatment effect size (RMST difference)");
plot_level(dat,
           covari.sel = c(14,15),
           trt.sel = 3,
           resp.sel = c(1, 2),
           outcome.type = "survival",
           ss.rect = FALSE,
           range.strip=c(-21, 21),
           n.brk = 31,
           n.brk.axis =  7,
           font.size = c(14, 12, .8, 14, 0.7),
           title = "Total sample size = 475",
           strip = strip.title,
           effect = "RMST", time = 50)
dev.off()
## General Comment: The functions open a new device via dev.new() with the option noRStudioGD = TRUE.
## The plot is then not shown, at least in my environment, until I call dev.off() and a new file Rplots##.pdf is generated in the working directory.
## It may be more convenient to show the plot in the Rstudio's Plots tab.
## General Comment 2: the name of the functions are not consistent. Some have .plt, others no separation.
## lvplt: The font.size is not consistent in the values it take. In the default values c(15, 12, 0.8, 15, 0.6), why for positions 3 and 5 we have such a small numbers? are they the relative size compared to the others?
