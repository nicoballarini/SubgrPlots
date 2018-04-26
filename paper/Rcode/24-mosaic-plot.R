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
## 1. Mosaic plot ---------------------------------------------------------------
library(SubgrPlots) # Loads this package. Install it first
library(survival)
library(ggplot2)
library(dplyr)
library(grid)

# # Load the data to be used
data(prca)

trt.sel = 3
resp.sel = c(1, 2)
outcome.type = "survival"
effect = "HR"
strip = strip.title = paste("Treatment effect size (log hazard ratio)");
font.size = c(10, 10, 10, 10, 0.7)

dat <- prca
dat %>%
  mutate(bm = factor(ifelse(bm == 0 , "No", "Yes")),
         hx = factor(ifelse(hx == 0 , "No", "Yes")),
         # pf = factor(ifelse(pf == 0 , "No", "Yes")),
         Treatment = factor(ifelse(rx == 0 , "Control", "Treatment")),
         Survival = factor(ifelse(survtime > 24 , "Yes", "No"), levels = c("Yes", "No")))-> dat
levels(dat$age_group) = c("Young","Middle-aged","Old")
levels(dat$weight_group)  = c("Low","Mid","High")
vars = data.frame(variable = names(dat), index = 1:length(names(dat)))

covari.sel = c(14,15)
names(dat)[covari.sel] = c("age.", "weight.")
head(dat)
dat %>%
  rename(`Bone Metastasis` = bm,
         `Performance rating` = pf,
         `History of cardiovascular events` = hx,
         `2-year survival` = Survival,
         Weight = weight.,
         Age = age.) -> dat


# Survival -------------------
pdf("paper/figures/24-mosaic-plot-surv.pdf", width = 5, height = 4)
SubgrPlots:::plot_mosaic_3_noeffect(dat,
                       covari.sel = c(14, 16, 17),
                       trt.sel, resp.sel, outcome.type,
                       range.v = NULL, adj.ann.subgrp = 4,
                       range.strip=c(-3, 3),
                       n.brk = 7,
                       font.size = c(12, 12, 8, 10, 0.7),
                       title = NULL, lab.xy = NULL,sep. = 0.03,
                       strip = "Treatment effect size",
                       effect = "HR")
dev.off()

vars


col =c("#80b1d3", "#fccde5")
lwd. = 2
sep. = 0.05
n.sep.h = 2
vars
n.brk = 7
range.strip=c(-3, 3)
# covari.sel = c(15, 4, 7)
covari.sel = c(15, 7, 4)
# covari.sel = c(5, 7, 4)
unlist(table(dat[, covari.sel]))
table(dat[, covari.sel])


pdf("paper/figures/25-mosaic-plot-hr.pdf", width = 5, height = 4)
SubgrPlots:::plot_mosaic_3(dat,
                           covari.sel, trt.sel, resp.sel, outcome.type,
                           range.v = NULL, adj.ann.subgrp = 4,
                           range.strip=c(-3, 3),
                           n.brk = 31, n.brk.axis = 7,
                           font.size = c(10, 10, 10, 10, 0.7),
                           title = NULL, lab.xy = NULL,
                           strip = "Treatment effect size (log-hazard ratio)",
                           effect = "HR", palette = "hcl")
dev.off()

plot_mosaic(dat,
            covari.sel = c(4, 5, 7),
            trt.sel, resp.sel, outcome.type,
            range.v = NULL, adj.ann.subgrp = 4,
            range.strip=c(-3, 3),
            n.brk = 7,
            font.size = c(10, 10, 10, 10, 0.7),
            title = NULL, lab.xy = NULL,
            strip = "Treatment effect size",
            effect = "HR")

SubgrPlots:::plot_mosaic_3(dat,
            covari.sel = c(14, 15, 7),
            trt.sel, resp.sel, outcome.type,
            range.v = NULL, adj.ann.subgrp = 4,
            range.strip=c(-3, 3),
            n.brk = 31,
            n.brk.axis = 7,
            font.size = c(10, 10, 10, 10, 0.7),
            title = NULL, lab.xy = NULL,
            strip = "Treatment effect size",
            effect = "HR")

SubgrPlots:::plot_mosaic_2(dat,
                           covari.sel = c(14, 5),
                           trt.sel, resp.sel, outcome.type,
                           range.v = NULL, adj.ann.subgrp = 4,
                           range.strip=c(-3, 3),
                           n.brk = 7,
                           font.size = c(10, 10, 10, 10, 0.7),
                           title = NULL, lab.xy = NULL,
                           strip = "Treatment effect size",
                           effect = "HR")

SubgrPlots:::plot_mosaic_2(dat,
                           covari.sel = c(4, 5),
                           trt.sel, resp.sel, outcome.type,
                           range.v = NULL, adj.ann.subgrp = 4,
                           range.strip=c(-3, 3),
                           n.brk = 7,
                           font.size = c(10, 10, 10, 10, 0.7),
                           title = NULL, lab.xy = NULL,
                           strip = "Treatment effect size",
                           effect = "HR")
SubgrPlots:::plot_mosaic_2(dat,
                           covari.sel = c(14, 15),
                           trt.sel, resp.sel, outcome.type,
                           range.v = NULL, adj.ann.subgrp = 4,
                           range.strip=c(-3, 3),
                           n.brk = 31,
                           n.brk.axis = 7,
                           font.size = c(10, 10, 10, 10, 0.7),
                           title = NULL, lab.xy = NULL,
                           strip = "Treatment effect size",
                           effect = "HR", print.ss = TRUE)

pdf("paper/figures/25-mosaic-plot-hr-2er.pdf", width = 5, height = 4)
SubgrPlots:::plot_mosaic_2_marginal(dat,
                                    covari.sel = c(14, 15),
                                    trt.sel, resp.sel, outcome.type,
                                    range.v = NULL, adj.ann.subgrp = 4,
                                    range.strip=c(-3, 3),
                                    n.brk = 31,
                                    n.brk.axis = 7, sep. = 0.034,
                                    font.size = c(10, 10, 10, 10, 0.7),
                                    title = NULL, lab.xy = NULL,
                                    strip = "Treatment effect size (log-hazard ratio)",
                                    col.line = "white", lwd. = 2,
                                    effect = "HR", print.ss = FALSE, palette = "hcl")
dev.off()

