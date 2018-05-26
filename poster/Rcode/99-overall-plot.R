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

names(dat)[covari.sel] = c("age", "weight")
vars
head(dat)

dat$trt = factor(ifelse(dat$rx==1, "Treatment", "Control"))
dat$trt = dat$rx
model.int = coxph(Surv(survtime, cens) ~ trt, data = dat)
model.sum = summary(model.int)
max.time = ceiling(max(dat$survtime))
time = 60
rmst = survRM2::rmst2(time = dat$survtime, status = dat$cens,
                      arm = dat$trt, tau = 70)
rmst
plot(rmst, yaxs="i")
###############################################################################-
surv.fit = survfit(Surv(survtime, cens) ~ trt, data = dat)
difference <- summary(surv.fit, time=time)
col.line = c("#a6cee3", "#1f78b4")

survfit(Surv(survtime, cens) ~ 1, data = dat)
33/4
median(dat$survtime)
pdf("paper/figures/00-overall-km.pdf", width = 7, height = 5)
plot(surv.fit, col = col.line, yaxs="i",  yaxt = "n", bty="n", xaxt = "n", xmax = max.time, lwd = 3)
axis(side = 1, at = seq(0,max.time+1, by = 6), label = seq(0,max.time+1, by = 6))
axis(side = 2, at = seq(0,1, len = 6), label = seq(0,1, len = 6))
mtext("Time (months)", side = 1, line = 2)
mtext("Survival Probability", side = 2, line = 2)
legend("topright",legend = c("Control", "Treatment"), col = col.line, lty = 1, lwd =3, horiz = TRUE)
dev.off()

pdf("paper/figures/00-overall-rmst.pdf", width = 7, height = 4)
plot(rmst)
dev.off()
