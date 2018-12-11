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
## 1. Mosaic plot ---------------------------------------------------------------
pkgroot = rprojroot::find_package_root_file()
library(SubgrPlots) # Loads this package. Install it first
library(dplyr)
# # Load the data to be used
data(prca)
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
# Change variable names
head(dat)
dat %>%
  rename(`Bone Metastasis` = bm,
         `Performance rating` = pf,
         `History of cardiovascular events` = hx,
         `2-year survival` = Survival,
         Weight = weight_group,
         Age = age_group) -> dat


filename = "14-mosaic-plot-surv"
width    = 5
height   = 4
res = 600
setEPS()
postscript(paste0(rprojroot::find_package_root_file(),
                  "/paper/figures_eps/", filename, ".eps"),
           width = width, height = height)
# pdf("paper/figures/14-mosaic-plot-surv.pdf", width = 5, height = 4)
plot_mosaic(dat,
            covari.sel = c(14, 16, 17),
            trt.sel = 3,
            resp.sel = c(1, 2),
            outcome.type =  "survival",
            range.v = NULL, adj.ann.subgrp = 4,
            range.strip=c(-3, 3),
            n.brk = 7,
            font.size = c(12, 12, 8, 10, 0.7),
            title = NULL, lab.xy = NULL, sep. = 0.03,
            strip = "Treatment effect size",
            effect = "HR", show.effect = FALSE)
dev.off()


