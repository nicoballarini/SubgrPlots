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
library(UpSetR)

# Load the data to be used
data(prca)
dat <- prca
vars = data.frame(variable = names(dat), index = 1:length(names(dat)))


###############################################################################-
## 22. UpSet -----------------------------------------------------------
pdf("paper/figures/22-upSetR-plot.pdf", width = 6, height = 5, onefile=FALSE)
prca.upset = data.frame(
  trt = factor(ifelse(prca$rx == 1, "Treatment", "Control")),
  bm = 1*(prca$bm == 1),
  pf = 1*(prca$pf == 1),
  hx = 1*(prca$hx == 1),
  stage = 1*(prca$stage == 4),
  age = 1*(prca$age > 75),
  wt = 1*(prca$weight > 100),
  survtime = prca$survtime,
  cens = prca$cens==1)
# Creating a custom query to operate on the rows of the data.
Myfunc <- function(row, param1, param2) {
  data <- (row["trt"] %in% c(param1, param2))
}
pal <- c("#1f78b4", "#a6cee3")
upset(prca.upset,
      order.by = "freq",
      # empty.intersections = "on",
      sets = c("bm",'pf',"hx",'stage',"age","wt"),
      nintersects = 14,
      text.scale = 1.4,
      queries = list(list(query = Myfunc,
                          params = c("Control", "Treatment"),
                          color = pal[2],
                          active = T,
                          query.name = "Control"),
                     list(query = Myfunc,
                          params = c("Treatment", "Treatment"),
                          color = pal[1],
                          active = T,
                          query.name = "Treatment")),
      query.legend = "top")
dev.off()




# Other Plots ------

# Using list
# We dichotomized age (≤ 65,> 65) and weight (≤ 100,> 100)
prca.list = list(`pf=1` = which(prca$pf == 1),
                 `bm=1` = which(prca$bm == 1),
                 `hx=1` = which(prca$hx == 1),
                 `stage=4` = which(prca$stage == 4),
                 `age>65` = which(prca$age > 65),
                 `wt>100` = which(prca$weight > 100))
prca.up = UpSetR::fromList(prca.list)
upset(prca.up, order.by = "freq", text.scale = 1.5)
upset(prca.up, order.by = "degree", text.scale = 1.5)
upset(prca.up, order.by = "freq", empty.intersections = "on")


# Using dataframe
str(prca)
prca.upset = data.frame(
  trt = factor(ifelse(prca$rx == 1, "Treatment", "Control")),
  `pf=1` = 1*(prca$pf == 1),
  `bm=1` = 1*(prca$bm == 1),
  `hx=1` = 1*(prca$hx == 1),
  `stage=4` = 1*(prca$stage == 4),
  `age>65` = 1*(prca$age > 65),
  `wt>100` = 1*(prca$weight > 100),
  survtime = prca$survtime,
  cens = prca$cens==1, check.names = F)
head(prca.upset)
upset(prca.upset, order.by = "freq", text.scale = 1.5)
upset(prca.upset, order.by = "degree", text.scale = 1.5)
upset(prca.upset, order.by = "freq", empty.intersections = "on")

# Creating a custom query to operate on the rows of the data.
Myfunc <- function(row, param) {
  data <- (row["trt"] == "Treatment")
}
# Applying the created query to the queries parameter.
upset(prca.upset,
      order.by = "freq",
      queries = list(list(query = Myfunc,
                          params = 1,
                          color = "blue",
                          active = T)))

upset(prca.upset,
      order.by = "freq",
      queries = list(list(query = Myfunc,
                          params = 1,
                          color = "blue",
                          active = T,
                          query.name = "Treatment")),
      query.legend = "top")
# prca.upset["trt"] == "Treatment"


mydata<- prca.upset
myplot <- function(mydata, x, y) {
  fit<-survfit(Surv(survtime, cens) ~ trt, data=mydata)
  reg.1<-survival::coxph(Surv(survtime, cens) ~ trt,
                         data=mydata,ties="breslow", x=TRUE)
  dataplot = broom::tidy(reg.1)
  plot = (ggplot(data = dataplot, aes(x=term, y=estimate, ymax=conf.high, ymin=conf.low)) +
    geom_errorbar() +
    geom_point())
}
pp = myplot(mydata)
str(pp)
pp
upset(prca.upset,
      order.by = "freq",
      queries = list(list(query = Myfunc,
                          params = 1,
                          color = "blue",
                          active = T,
                          query.name = "Treatment"),
                     list(query = intersects,
                          params = list("hx.1", "age.65"),
                          active = T)),
      query.legend = "top",
      attribute.plots = list(gridrows = 45,
                             plots = list(list(plot = myplot,
                                               x = 1,
                                               y = 1,
                                               queries = T)),
                             ncols = 2))

upset(prca.upset,
      order.by = "freq", nintersects = 12,
      queries = list(list(query = Myfunc,
                          params = 1,
                          color = "blue",
                          active = T,
                          query.name = "Treatment")),
                     # list(query = intersects,
                     #      params = list("hx.1", "age.65"),
                     #      active = T)),
      query.legend = "top",
      boxplot.summary = c("survtime"))


# Using dataframe
str(prca)
prca.upset = data.frame(
  trt = factor(ifelse(prca$rx == 1, "Treatment", "Control")),
  pf = 1*(prca$pf == 1),
  bm = 1*(prca$bm == 1),
  hx = 1*(prca$hx == 1),
  stage = 1*(prca$stage == 4),
  age = 1*(prca$age > 65),
  wt = 1*(prca$weight > 100),
  survtime = prca$survtime,
  cens = prca$cens==1)

var.labels = c(trt="trt",
               pf="pf=1",
               bm="bm=1",
               hx="hx=1",
               stage="stage=4",
               age="age>65",
               wt="wt>100",
               survtime="survtime",
               cens="cens")

Hmisc::label(prca.upset) = lapply(names(var.labels),
                     function(x) Hmisc::label(prca.upset[,x]) = var.labels[x])

Hmisc::label(prca.upset)



# Creating a custom query to operate on the rows of the data.
Myfunc <- function(row, param1, param2) {
  data <- (row["trt"] %in% c(param1, param2))
}

c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728",
  "#9467BD", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22",
  "#17BECF")

mydata
fit <- survfit(Surv(survtime, cens) ~ trt, data = mydata)
reg.1 <- survival::coxph(Surv(survtime, cens) ~ trt,
                         data=mydata,ties="breslow", x=TRUE)
dataplot = broom::tidy(reg.1)

upset(prca.upset,
      order.by = "freq",
      nintersects = 12,
      queries = list(list(query = Myfunc,
                          params = c("Control", "Treatment"),
                          color = "gray23",
                          active = T,
                          query.name = "Control"),
                     list(query = Myfunc,
                          params = c("Treatment", "Treatment"),
                          color = "#17BECF",
                          active = T,
                          query.name = "Treatment")),
      query.legend = "top")

prca.upset
formula=Surv(survtime, cens) ~ trt
prca.upset %>%
  do(data.frame(subgroup = 2,
                hr = exp(survival::coxph(formula, data = ., ties="breslow")$coefficients),
                broom::tidy(survival::coxph(formula, data = ., ties="breslow")),
                n  = nrow(.))) %>%
  mutate(variable = "Overall",
         hr.ll = exp(conf.low),
         hr.ul = exp(conf.high)) %>%
  group_by(subgroup) -> overall
overall

hrSubgr <- function(data, var){
  data %>%
    group_by_(var) %>%
    do(data.frame(hr = exp(survival::coxph(formula, data = ., ties="breslow")$coefficients),
                  broom::tidy(survival::coxph(formula, data = ., ties="breslow")),
                  n  = nrow(.))) %>%
    mutate(variable = var,
           hr.ll = exp(conf.low),
           hr.ul = exp(conf.high)) %>%
    rename_(subgroup = var)%>%
    ungroup()
}
bind_rows(overall,
          hrSubgr(prca.upset, "age"),
          hrSubgr(prca.upset, "wt"),
          hrSubgr(prca.upset, "pf"),
          hrSubgr(prca.upset, "bm"),
          hrSubgr(prca.upset, "hx"),
          hrSubgr(prca.upset, "stage")) -> fp.dat
fp.dat %>%
  mutate(variable = factor(variable,
                           levels = c("Overall", "agegroup", "wtgroup", "pf", "bm", "hx", "stage")),
         label = sprintf("%s, %s, %s, %s, %s", variable, subgroup, hr, hr.ll, hr.ul)) -> fp.dat


upset(prca.upset,
      order.by = "freq",
      nintersects = 12,
      queries = list(list(query = Myfunc,
                          params = c("Control", "Treatment"),
                          color = "gray23",
                          active = T,
                          query.name = "Control"),
                     list(query = Myfunc,
                          params = c("Treatment", "Treatment"),
                          color = "#17BECF",
                          active = T,
                          query.name = "Treatment")),
      query.legend = "top")




