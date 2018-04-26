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
dat
covari.sel = c(4,5,6,7)
trt.sel = 3
resp.sel = c(1, 2)
outcome.type = "survival"
main.title = list("", "Forestplot of subgroups", "Forestplot of subgroups (T | C)")
label.x = list("", "Log hazard ratio", "Survival probability at t=50")
effect = "HR"
size.shape = c(0.25, 0.12)
font.size = c(1, 1, 1, 0.7)
title = main.title
lab.x = label.x
time = 50
new=TRUE
strip = strip.title = paste("Treatment effect size (log hazard ratio)");
range.strip=c(-3, 3)
n.brk = 7


col.line = c("#a6cee3", "#1f78b4")
max.time =ceiling(max(dat[, resp.sel[1]]))
equal.width = FALSE
show.KM = FALSE
show.effect = TRUE
conf.int = FALSE

dat %>%
  rename(Age= age_group,
         Weight = weight_group)-> dat



pdf("paper/figures/26-circle-plot-2er.pdf", height = 5, width = 6)
set.seed(55643)
plot_circle(dat,
            covari.sel = c(14, 15),
            trt.sel, resp.sel, outcome.type,
            range.v = NULL, adj.ann.subgrp = 4,
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

set.seed(12)
mat = matrix(nrow = 8,ncol = 8)
col_mat = rand_color(length(mat),
                     luminosity = "bright",
                     transparency = 0.45)
dim(col_mat) = dim(mat)


vars
plot_circle(dat,
            covari.sel = c(4, 5, 6, 7, 14, 15),
            trt.sel, resp.sel, outcome.type,
            range.v = NULL, adj.ann.subgrp = 4,
            range.strip=c(-3, 3),
            n.brk = 31,
            n.brk.axis = 7,
            font.size = c(1, 1, 0.85, 0.85, 1),
            title = NULL, lab.xy = NULL,
            strip = "Treatment effect size( log hazard ratio)",
            effect = "HR",
            equal.width = FALSE,
            show.KM = FALSE,
            show.effect = TRUE,
            conf.int = FALSE)
pdf("paper/figures/26-circle-plot-matrix.pdf", height = 5, width = 10)
set.seed(12)
SubgrPlots:::plot_circle_std_by(dat,
                                covari.sel = c(4, 5, 6, 7),
                                trt.sel, resp.sel, outcome.type,
                                range.v = NULL, adj.ann.subgrp = 4,
                                range.strip=c(-3, 3),
                                n.brk = 31,
                                n.brk.axis = 7,
                                font.size = c(1, 1, 0.85, 0.85, 1),
                                title = NULL, lab.xy = NULL,
                                strip = "Treatment effect size( log hazard ratio)",
                                effect = "HR",
                                equal.width = FALSE,
                                show.KM = FALSE,
                                show.effect = TRUE,
                                conf.int = FALSE, palette = "hcl")
dev.off()
pdf("paper/figures/26-circle-plot-matrix-equal.pdf", height = 5, width = 10)
set.seed(12)
SubgrPlots:::plot_circle_std_by(dat,
                                covari.sel = c(4, 5, 6, 7),
                                trt.sel, resp.sel, outcome.type,
                                range.v = NULL, adj.ann.subgrp = 4,
                                range.strip=c(-3, 3),
                                n.brk = 31,
                                n.brk.axis = 7,
                                font.size = c(1, 1, 0.85, 0.85, 1),
                                title = NULL, lab.xy = NULL,
                                strip = "Treatment effect size( log hazard ratio)",
                                effect = "HR",
                                equal.width = TRUE,
                                show.KM = FALSE,
                                show.effect = TRUE,
                                conf.int = FALSE, palette = "hcl")
dev.off()

# pdf("paper/figures/26-circle-plot-KM.pdf", height = 5, width = 6)
# plot_circle(dat, covari.sel, trt.sel, resp.sel, outcome.type,
#             range.v = NULL, adj.ann.subgrp = 4,
#             range.strip=c(-3, 3),
#             n.brk = 7,
#             font.size = c(1, 1, 0.85, 0.85, 1),
#             title = NULL, lab.xy = NULL,
#             strip = "Treatment effect size( log hazard ratio)",
#             effect = "HR",
#             equal.width = FALSE,
#             show.KM = TRUE,
#             show.effect = TRUE,
#             conf.int = FALSE)
# dev.off()
#
#
# pdf("paper/figures/26-circle-plot-KM-equal.pdf", height = 5, width = 6)
# plot_circle(dat, covari.sel, trt.sel, resp.sel, outcome.type,
#             range.v = NULL, adj.ann.subgrp = 4,
#             range.strip=c(-3, 3),
#             n.brk = 7,
#             font.size = c(1, 1, 0.85, 0.85, 1),
#             title = NULL, lab.xy = NULL,
#             strip = "Treatment effect size( log hazard ratio)",
#             effect = "HR",
#             equal.width = TRUE,
#             show.KM = TRUE,
#             show.effect = TRUE,
#             conf.int = FALSE)
# dev.off()
#
#
#
# pdf("paper/figures/26-circle-plot-cis.pdf", height = 5, width = 6)
# plot_circle(dat, covari.sel, trt.sel, resp.sel, outcome.type,
#             range.v = NULL, adj.ann.subgrp = 4,
#             range.strip=c(-3, 3),
#             n.brk = 7,
#             font.size = c(1, 1, 0.85, 0.85, 1),
#             title = NULL, lab.xy = NULL,
#             strip = "Treatment effect size( log hazard ratio)",
#             effect = "HR",
#             equal.width = TRUE,
#             show.KM = FALSE,
#             show.effect = TRUE,
#             conf.int = TRUE)
# dev.off()
