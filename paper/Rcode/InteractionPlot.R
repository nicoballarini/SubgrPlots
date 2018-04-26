###############################################################################-
##
## This program creates the figures for the manuscript using the
## prca data that is included in this package
##
##

## Instead of using rm(list = ls()), make sure you start with a fresh R
## by restarting R -> Control+Shift+F10
cat("\014") # Cleans the console
## Load needed libraries
## If SubgrPlots package is not installed, then open project and use following lines or
## in the build window, click Install and Restart
# devtools::build()
# devtools::install()

library(SubgrPlots) # Loads this package. Install it first
library(survival)
library(ggplot2)
library(dplyr)
library(tidyverse)
# install.packages("emmeans")
# library(lsmeans)
library(emmeans)
# Removing Rplot files in top folder
# all.files  = list.files()
# plot.files = all.files[grep(pattern = "Rplot",x = all.files)]
# file.remove(plot.files)

# Load the data to be used
data(prca)
prca0<-prca
head(prca0)

# Select the variables that we use for the analysis
prca <- prca0[,c("SURVTIME","CENS","RX","BM","HX","STAGE","PF", "AGE", "WT")]
head(prca) # Preview dataset
names(prca)<- c("survtime","cens","rx","bm",
                "hx","stage","pf","age", "wt")  # Change names of variables to lower case

# Create subgroups for Age and Weight and Stage
prca$age1 <- 1 * (prca$age > 65 & prca$age <= 75)
prca$age2 <- 1 * (prca$age > 75)
prca$wt1  <- 1 * (prca$wt > 90 & prca$wt <= 110)
prca$wt2  <- 1 * (prca$wt > 110)
# Create subgroups for Age and Weight and Stage with (-1,1) coding
prca$agegroup <- 1 + (1 * (prca$age > 65) + 1 * (prca$age > 75))
prca$wtgroup  <- 1 + (1 * (prca$wt > 90) + 1 * (prca$wt > 110))
dat = prca
dat$agegroup = factor(dat$agegroup)
dat$wtgroup = factor(dat$wtgroup)
range(dat$age)
range(dat$wt)
levels(dat$agegroup) = c("[48,65]","(65,75]","(75,89]")
levels(dat$wtgroup)  = c("[69,90]","(90,110]","(110,152]")
## We need variables as factors
dat$bm    = factor(dat$bm)
dat$hx    = factor(dat$hx)
dat$stage = factor(dat$stage)
dat$pf    = factor(dat$pf)
dat$rx    = factor(dat$rx) # Treatment

# Put labels to the variables so that they appear in the plot
names(dat)<- c("survtime",
               "cens",
               "rx",
               "bm",
               "hx",
               "stage",
               "pf",
               "age",
               "weight",
               "age1",
               "age2",
               "wt1",
               "wt2",
               "Age",
               "Weight")
vars = data.frame(variable = names(dat), index = 1:length(names(dat)))

head(dat)
Surv(dat$survtime, dat$cens)
lrt <- coxph(formula = Surv(survtime, cens) ~rx, data = dat)
broom::tidy(lrt)

model = coxph(formula = Surv(survtime, cens) ~ rx * (bm * age) , data = dat)
broom::tidy(model)
# Figure 1

emmeans(model, ~ bm | rx,
              at = list(rx  = factor(c(0, 1)),
                        bm  = factor(c(0, 1))))
emmeans(model, ~ rx | bm,
              at = list(rx  = factor(c(0, 1)),
                        bm  = factor(c(0, 1))))

ls1 = emmeans(model, ~ age | bm + rx,
              at = list(rx  = factor(c(0, 1)),
                        age = c(75:90),
                        bm  = factor(c(0, 1))))

ls1_df = data.frame(summary(ls1))

ls1_df %>%
  ggplot(aes(x = age, y = emmean, colour = factor(rx))) +
  geom_line() +
  facet_grid(bm ~ ., labeller = labeller(bm = label_both)) +
  # coord_cartesian(ylim = c(0, 1)) +
  theme_bw() +
  labs(x      = "Age",
       y      = "Predicted response",
       colour = "Treatment",
       title  = "Predicted response")

ls1_df %>%
  ggplot(aes(x = age, y = exp(emmean), colour = factor(rx))) +
  geom_line() +
  geom_point() +
  facet_grid(bm ~ ., labeller = labeller(bm = label_both)) +
  # coord_cartesian(ylim = c(0, 1)) +
  theme_bw() +
  labs(x      = "Age",
       y      = "Predicted response",
       colour = "Treatment",
       title  = "Predicted response")



# Figure 2
or1 = lsmeans(model, ~ age | rx,
              type = "response",
              at = list(rx  = factor(c(0, 1)),
                        age = c(75,90)))
CI1 = data.frame(confint(pairs(or1)))
CI1 %>% ggplot(aes(x = factor(rx), y = ratio)) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  coord_flip() +
  theme_bw() +
  labs(title = "Odds ratios with 95% confidence limits",
       y     = "Hazard ratio",
       x     = "Treatment") +
  geom_hline(yintercept = 1)



# Figure 3
or2 = lsmeans(model, ~rx | age + bm,
              at = list(rx  = factor(c(0, 1)),
                        age = c(75, 90),
                        bm  = factor(c(0, 1))))
CI2 = data.frame(confint(pairs(or2)))
CI2 %>%
  ggplot(aes(x = factor(age), y = estimate, colour = factor(bm))) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = asymp.LCL,  ymax = asymp.UCL),
                position = position_dodge(0.5), width = 0.2) +
  coord_flip() +
  theme_bw() +
  labs(title = "Odds ratios with 95% confidence limits",
       y = "Hazard ratio",
       x = "Age",
       colour = "bm") +
  geom_hline(yintercept = 1)
# Figure 4

# install.packages("visreg")
# library(visreg)
# visreg2d(model,
#          xvar = "age",
#          yvar = "weight",
#          cond = list(bm = 0),
#          zlim = c(0, 1),
#          main = " ",
#          levels = seq(0, 1, 0.1))
# visreg2d(model,
#          xvar = "age",
#          yvar = "weight",
#          # cond = list(bm = 1),
#          zlim = c(0, 1),
#          main = "Underweight prob (Boy=1)",
#          levels = seq(0, 1, 0.1))
