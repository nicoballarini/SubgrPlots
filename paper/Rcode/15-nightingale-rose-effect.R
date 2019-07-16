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

pdf("paper/figures/15-nightingale-rose-effect.pdf", width = 6, height = 5, onefile = FALSE)
strip.title = "Treatment effect size (log hazard ratio)"
plot_nightingale_effect(dat,
                       covari.sel = c(14,15),
                       trt.sel = 3,
                       resp.sel = c(1, 2),
                       outcome.type = "survival",
                       seq_by = 50,
                       range.strip=c(-3, 3),
                       n.brk = 31,
                       n.brk.axis =  7,
                       title = "Total sample size = 475",
                       strip = strip.title, effect = "HR",
                       show.overall = TRUE, palette = "hcl")
dev.off()
