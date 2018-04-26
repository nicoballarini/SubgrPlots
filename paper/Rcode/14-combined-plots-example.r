################################################################################
# This script just shows an example of combining two graphical displays.
# To do this, the function of each display should be changed so that they will
# not generate a new plot window and produce a display under the original layout
# setup! After this procedure, one can run the following code to combine the two
# displays.
#
# Note that a few graphical displays which are created under the grid system
# are not compatible with others which are created
# under the graphics system. So, they may not be able to combine.
#
#
################################################################################

library(SubgrPlots) # Loads this package. Install it first
library(survival)
library(ggplot2)
library(dplyr)

# # Load the data to be used
data(prca)
dat <- prca
vars = data.frame(variable = names(dat), index = 1:length(names(dat)))

dat
covari.sel = c(4,5,6,7)
trt.sel = 3
resp.sel = c(1, 2)
outcome.type = "survival"
main.title = list("", "Forestplot of subgroups", "Forestplot of subgroups (T | C)")
label.x = list("", "Log hazard ratio", "Survival probability at t=50")

size.shape = c(0.25, 0.12)
font.size = c(1, 1, 1, 0.75)
title = main.title
lab.x = label.x
time = 50
names(dat)[c(14,15)] = c("age", "weight")

###############################################################################-
## 5. Forest Plot -----------------------------------------------------------
pdf("paper/figures/14-combined-plot.pdf", width = 7, height = 8)
layout(matrix(c(1, 2, 3,
                1, 2, 3,
                1, 2, 3,
                4, 4, 4,
                4, 4, 4,
                4, 4, 4,
                5, 5, 5),
              byrow = TRUE, nrow= 7, ncol=3),
       widths=c(1.4, 1, 1), heights=c(2.5, 2.5, 2.5, 2, 2, 2, 1))

main.title = list("", "Forestplot of subgroups", "Forestplot of subgroups (T | C)")
label.x = list("", "Log hazard ratio", "Survival probability at t=50")
plot_forest(dat,
            covari.sel = c(4,5,6,7, 14, 15),#vars
            trt.sel = 3,
            resp.sel = c(1, 2),
            outcome.type = "survival",
            size.shape = c(0.25, 0.12),
            font.size = c(1, 1, 0.7, 0.6),
            title = main.title,
            lab.x = label.x, time = 50, new = FALSE)

main.title = "Proportions of overlapped pairwise subgroups"
plot_matrix_overlap(dat,
                    c(4,5,6,7,14,15),
                    mode = 1,
                    font.size = c(1.5, 1.25),
                    title = main.title, new = FALSE)
dev.off()
