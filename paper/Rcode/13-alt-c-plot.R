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

covari.sel = c(14,15)
trt.sel = 3
resp.sel = c(1, 2)
outcome.type = "survival"

vars
names(dat)[covari.sel] = c("age", "weight")
which(dat$bm == levels(dat$bm)[1])
dat$hx
dat$bm
###############################################################################-
## 1. Level plot ---------------------------------------------------------------
pdf("paper/figures/13-alt-c-plot.pdf", width = 5, height = 5)
plot_network(dat = dat,
             covari.sel = c(6,5,4,7),
             para = c(0.1, 0.5, 1),
             font.size = c(1.2, 1.2),
             title = NULL)


dev.off()



# data.frame(x = 0, y = 0, label="Code not provided") %>%
#   ggplot() +
#   geom_text(aes(x=x,y=y,label=label), size= 13) +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         axis.title = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank())
