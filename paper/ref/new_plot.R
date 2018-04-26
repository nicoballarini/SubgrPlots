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

# dat
covari.sel = c(4, 5, 6, 7)
trt.sel = 3
resp.sel = c(1, 2)
outcome.type = "survival"
range.v = c(-6,6)
lab.xy = list(x= "Treatment effect estimate",
              y= "Interaction test z-score")
################################################ 0. argument validity check  #################################################################

if (missing(dat)) stop("Data have not been inputed!")
if (!(is.data.frame(dat))) stop("The data set is not with a data frame!")

if (missing(covari.sel)) stop("The variables for defining subgroups have not been specified!")
if (!(is.numeric(covari.sel))) stop("The variables for defining subgroups are not numeric!")
for (i in 1 : length(covari.sel)) if (!(is.factor(dat[,covari.sel[i]]))) stop("The variables for defining subgroups are not categorical!")

if (missing(trt.sel)) stop("The variable specifying the treatment code (for treatment / control groups) has not been specified!")
if (!(length(trt.sel) == 1)) stop("The variable specifying the treatment code can not have more than one component!")
if (!(is.factor(dat[, trt.sel]))) stop("The variable specifying the treatment code is not categorical!")
if (length(names(table(dat[, trt.sel]))) > 2) stop("The variable specifying the treatment code is not binary!")
if (sum(is.element(names(table(dat[, trt.sel])), c("0","1"))) != 2) stop("The treatment code is not 1 and 0 (for treatment / control groups)!")

type.all = c("continuous", "binary",  "survival")
if (is.null(outcome.type)) stop("The type of the response variable has not been specified!")
if (!(is.element(outcome.type, type.all)) == TRUE) stop("A unrecognized type has been inputed!")
if (outcome.type == "continuous"){
  if (missing(resp.sel)) stop("The response variable has not been specified!")
  if (!(length(resp.sel) == 1)) stop("The response variable has more than one component!")
  if (!(is.numeric(dat[, resp.sel]))) stop("The response variable is not numeric!")
}else if (outcome.type == "binary"){
  if (missing(resp.sel)) stop("The response variable has not been specified!")
  if (!(length(resp.sel) == 1)) stop("The response variable has more than one component!")
  if (!(is.factor(dat[, resp.sel]) || is.numeric(dat[, resp.sel])  )) stop("The response variable is not categorical or numerical!")
  if (length(names(table(dat[, resp.sel]))) > 2) stop("The response variable is not binary!")
  if (sum(is.element(names(table(dat[, resp.sel])), c("0","1"))) != 2) stop(" The response variable is not coded as 0 and 1!")
}else if (outcome.type == "survival"){
  if (missing(resp.sel)) stop("The response variablehas not been specified!")
  if (!(length(resp.sel) == 2)) stop("The response variable for analysing survival data should have two components!")
  if (!(is.numeric(dat[, resp.sel[1]]))) stop("The response variable specifying survival time is not numeric!")
  if (!(is.numeric(dat[, resp.sel[2]]) || is.logical(dat[, resp.sel[2]]) ) ) stop("The response variable specifying indicators of right censoring should be numerical or logical!")
  if (length(names(table(dat[, resp.sel[2]]))) > 2) stop("The response variable specifying indicators of right censoring is not binary!")
  if (sum(is.element(names(table(dat[, resp.sel[2]])), c("0","1"))) != 2) stop("The response variable specifying indicators of right censoring is not coded as 0 and 1!")
}

if (!(length(range.v) == 2)) stop("The vertical range of graphical display should have two components (specifying the minimum and maximum!)")

if (!(is.numeric(adj.ann.subgrp))) stop("The argument adjusting the distance between a point and its corresponding subgroup label is not numeric!")
if (adj.ann.subgrp < 0) stop("The value adjusting the distance between a point and its corresponding subgroup label is not positive!")

if (!(is.numeric(font.size))) stop("The argument about the font sizes of the label and text is not numeric!")
if (!(length(font.size) == 5)) stop("The font size setups for labels or text should have five components only!")

################################################ 1. create subgroup data  #################################################################

library(survival)

n.covari = length(covari.sel)
lab.vars = names(dat)[covari.sel]                                                # set the names of the covariates which relates to the defined subgroup; if a covariate
# are considered for multiple times, we make their name identical. (otherwise, the resulsting
# names are like var, var.1, var.2 and so on.)

names(dat)[trt.sel] = "trt"                            # rename the variable for treatment code
if (outcome.type == "continuous"){
  names(dat)[resp.sel] = "resp"                        # rename the response variable
}else if (outcome.type == "binary"){
  names(dat)[resp.sel] = "resp"                        # rename the response variable
}else if (outcome.type == "survival"){
  names(dat)[resp.sel[1]] = "time"                     # rename the response variable for survival time
  names(dat)[resp.sel[2]] = "status"                   # rename the response variable for survival right censoring status
}


n_t = sum(dat$trt == 1)
n_c = sum(dat$trt == 0)

for (i in 1: length(covari.sel)){
  cond = covari.sel == covari.sel[[i]]
  lab.vars[cond] = rep(lab.vars[i], length(which(cond == TRUE)))
}
cats.var = list()
n.subgrp.tol = 0
for (i in 1 : length(covari.sel)){
  cats.var[[i]] = names(table(dat[,covari.sel[i]]))
  n.subgrp.tol = n.subgrp.tol + length(cats.var[[i]])
}

cond = list()
data.subgrp = list()
lambda_t = lambda_c = lambda = deltaS = list()
ss.subgrp = matrix(rep(0, n.subgrp.tol * n.subgrp.tol), nrow = n.subgrp.tol)
k = 0
for (i in 1 : length(covari.sel)) {
  for (j in 1 : length(cats.var[[i]])){
    k = k + 1
    cond[[k]] = which((dat[, covari.sel[i]] == cats.var[[i]][j])  == T )
    ss.subgrp[k, k] = length(cond[[k]])
    data.subgrp[[k]] = dat[cond[[k]], ]
    lambda_t[[k]] = sum(dat[, covari.sel[i]] == cats.var[[i]][j] & dat$trt == 1)/sum(dat$trt == 1)
    lambda_c[[k]] = sum(dat[, covari.sel[i]] == cats.var[[i]][j] & dat$trt == 0)/sum(dat$trt == 0)
    lambda[[k]]   = sum(dat[, covari.sel[i]] == cats.var[[i]][j])/nrow(dat)

    if (outcome.type == "survival"){
      which. = dat[, covari.sel[i]] == cats.var[[i]][j]
      # dat[which.,]
      lambda[[k]] = sum(dat[which., "status"] == 1)/sum(dat$status == 1)
    }
    if (outcome.type == "continuous"){
      deltaS[[k]]   = mean(dat$resp[which(dat[, covari.sel[i]] == cats.var[[i]][j] & dat$trt == 1)]) -
        mean(dat$resp[which(dat[, covari.sel[i]] == cats.var[[i]][j] & dat$trt == 0)])
    }
  }
}

k = n.subgrp.tol
for (i in 1 : (n.subgrp.tol - 1) ){
  for (j in (i + 1) : (n.subgrp.tol) ){
    k = k + 1
    cond[[k]] = intersect(cond[[i]], cond[[j]])
    ss.subgrp[i, j] = length(cond[[k]])
    ss.subgrp[j, i] = length(cond[[k]])
  }
}

# create matrices for treatment size and standard error of MLE for subgroups and the full
# population

if (sum((dat$trt == "1")) == 0 | sum((dat$trt == "0")) == 0){
  treatment.mean.full = NA
  treatment.std.full = NA
  zscore.full = NA

}else{

  if (outcome.type == "continuous"){

    model.int = lm(resp ~ trt,  data = dat)
    model.sum = summary(model.int)
    treatment.mean.full = model.sum$coefficients[2, 1]
    treatment.std.full  = model.sum$coefficients[2, 2]
    zscore.full = treatment.mean.full / treatment.std.full
    zscore.full = 0

    sigma2      = sum((model.int$residuals^2))/model.int$df.residual
    varDeltaF   = (sigma2/sum(dat$trt == 1) + sigma2/sum(dat$trt == 0))
    deltaF = mean(dat$resp[which(dat$trt == 1)]) -
      mean(dat$resp[which(dat$trt == 0)])

  }else if (outcome.type == "binary"){

    model.int = glm(resp ~ trt, family = "binomial", data = dat)
    model.sum = summary(model.int)
    treatment.mean.full = model.sum$coefficients[2, 1]
    treatment.std.full = model.sum$coefficients[2, 2]
    zscore.full = treatment.mean.full / treatment.std.full

  }else if (outcome.type == "survival"){

    model.int = coxph(Surv(time, status) ~ trt, data = dat)
    model.sum = summary(model.int)
    treatment.mean.full = model.sum$coef[1, 1]
    treatment.std.full = model.sum$coef[1, 3]
    zscore.full = treatment.mean.full / treatment.std.full
    zscore.full = 0

    varDeltaF   = treatment.std.full^2
    deltaF = treatment.mean.full

  }
}






treatment.mean = matrix(0, nrow = n.subgrp.tol, ncol = 1)
treatment.std  = matrix(0, nrow = n.subgrp.tol, ncol = 1)
zscore = VarDsDf = sdDsDf = matrix(0, nrow = n.subgrp.tol, ncol = 1)
i=1
for (i in 1 : (n.subgrp.tol)){
  if (sum((data.subgrp[[i]]$trt == "1")) == 0 | sum((data.subgrp[[i]]$trt == "0")) == 0){
    treatment.mean[i] = NA
    treatment.std[i] = NA
    zscore[i] = NA

  }else{

    if (outcome.type == "continuous"){
      model.int =  lm(resp ~ trt, data = data.subgrp[[i]])
      model.sum = summary(model.int)
      treatment.mean[i] = model.sum$coefficients[2, 1]                                # record subgroup effect size
      treatment.std[i] = model.sum$coefficients[2, 2]
      # zscore[i] = treatment.mean[i] / treatment.std[i]                             # calculate the z-score of the subgroup effect size relative
      # to the standardized error of the full population effect size
      # estimator
      VarDsDf[i] = ((1-lambda_t[[i]])/lambda_t[[i]]/n_t +
                      (1-lambda_c[[i]])/lambda_c[[i]]/n_c) * sigma2
      zscore[i] = (deltaS[[i]] - deltaF) / sqrt(VarDsDf[i])
      sdDsDf[i] = sqrt(VarDsDf[i])
    }else if (outcome.type == "binary"){

      model.int = glm(resp ~ trt, family = "binomial", data = data.subgrp[[i]])
      model.sum = summary(model.int)
      treatment.mean[i] = model.sum$coefficients[2, 1]                                # record subgroup effect size
      treatment.std[i] = model.sum$coefficients[2, 2]
      zscore[i] = treatment.mean[i] / treatment.std[i]                        # calculate the z-score of the subgroup effect size relative
      # to the standardized error of the full population effect size
      # estimator
    }else if (outcome.type == "survival"){

      model.int = coxph(Surv(time, status) ~ trt, data = data.subgrp[[i]])
      model.sum = summary(model.int)
      treatment.mean[i] = model.sum$coef[1, 1]
      treatment.std[i] = model.sum$coef[1, 3]
      zscore[i] = treatment.mean[i] / treatment.std[i]

      VarDsDf[i] = treatment.std[i]^2 + varDeltaF -
        2 * sqrt(lambda[[i]])*treatment.std[i]*sqrt(varDeltaF)
      zscore[i] = (treatment.mean[i] - deltaF) / sqrt(VarDsDf[i])
      sdDsDf[i] = sqrt(VarDsDf[i])

    }
  }
}

i=1
z.score.int = numeric(length(covari.sel))
for (i in 1 : length(covari.sel)){
  if (sum((data.subgrp[[i]]$trt == "1")) == 0 | sum((data.subgrp[[i]]$trt == "0")) == 0){
    treatment.mean[i] = NA
    treatment.std[i] = NA
    zscore[i] = NA

  }else{

    if (outcome.type == "continuous"){
      model.int =  lm(resp ~ trt, data = data.subgrp[[i]])
      model.sum = summary(model.int)
      treatment.mean[i] = model.sum$coefficients[2, 1]                                # record subgroup effect size
      treatment.std[i] = model.sum$coefficients[2, 2]
      # zscore[i] = treatment.mean[i] / treatment.std[i]                             # calculate the z-score of the subgroup effect size relative
      # to the standardized error of the full population effect size
      # estimator
      VarDsDf[i] = ((1-lambda_t[[i]])/lambda_t[[i]]/n_t +
                      (1-lambda_c[[i]])/lambda_c[[i]]/n_c) * sigma2
      zscore[i] = (deltaS[[i]] - deltaF) / sqrt(VarDsDf[i])
      sdDsDf[i] = sqrt(VarDsDf[i])
    }else if (outcome.type == "binary"){

      model.int = glm(resp ~ trt, family = "binomial", data = data.subgrp[[i]])
      model.sum = summary(model.int)
      treatment.mean[i] = model.sum$coefficients[2, 1]                                # record subgroup effect size
      treatment.std[i] = model.sum$coefficients[2, 2]
      zscore[i] = treatment.mean[i] / treatment.std[i]                        # calculate the z-score of the subgroup effect size relative
      # to the standardized error of the full population effect size
      # estimator
    }else if (outcome.type == "survival"){
      var = names(dat[covari.sel[i]])
      formula = as.formula(paste0("Surv(time, status) ~ trt +", var, "+ trt*", var))
      model.int = coxph(formula, data = dat)
      model.sum = summary(model.int)
      z.score.int[i] = model.int$coefficients[3]/sqrt(model.int$var[3,3])
    }
  }
}


z.score.int







lab.subgrp = vector()
lab.subgrp2 = vector()
k = 0
for (i in 1: length(covari.sel)){
  for (j in 1 : length(cats.var[[i]])){
    k = k + 1
    lab.subgrp[k] = paste("(", LETTERS[i], j, ") ", lab.vars[i], " = ", cats.var[[i]][j], sep = "")
    lab.subgrp2[k]  = paste(LETTERS[i], j, sep = "")
  }
}
dimnames(treatment.mean) = list(c(lab.subgrp), c("mean") )
dimnames(treatment.std) = list(c(lab.subgrp), c("std") )
dimnames(zscore) = list(c(lab.subgrp), c("zscore") )
dimnames(sdDsDf) = list(c(lab.subgrp), c("sdDsDf") )
print(treatment.mean.full)
print(data.frame(treatment.mean, zscore, sdDsDf, treatment.std,
                 lambda_t=unlist(lambda_t),
                 lambda_c=unlist(lambda_c),
                 lambda = unlist(lambda),
      interaction = rep(z.score.int, each = 2)))
data.plot = data.frame(treatment.mean, zscore, sdDsDf, treatment.std,
                       lambda_t=unlist(lambda_t),
                       lambda_c=unlist(lambda_c),
                       lambda = unlist(lambda),
                       interaction = rep(z.score.int, each = 2),
                       color = rep(c("blue", "red", "green", "purple"), each = 2), stringsAsFactors = FALSE)
print(data.plot)
names(z.score.int) = lab.vars
################################################ 2. produce a graph  #################################################################


par(mar = c(4,4,2,2))

if (is.null(range.v)){
  y.lim.max = ceiling(max(z.score.int, na.rm = TRUE))
  y.lim.min = floor(min(z.score.int, na.rm = TRUE))
  if (y.lim.max <= 2)  {y.lim.max = y.lim.max + 3}
  if (y.lim.min >= -2) {y.lim.min = y.lim.min - 3}
}else{
  y.lim.max = range.v[2]
  y.lim.min = range.v[1]
}

plot(0, 0, type='n',
     xlab = lab.xy[[1]], ylab = lab.xy[[2]],
     ylim = c(y.lim.min, y.lim.max),
     xlim = c(min(treatment.mean)-2, max(treatment.mean)+2),
     # xaxt = 'n', yaxt='n',
     xaxs = "i",
     main = title,
     cex.lab = font.size[2], cex.main = font.size[1])

i=1
polygon(c(min(treatment.mean)-2, min(treatment.mean)-2, max(treatment.mean)+2, max(treatment.mean)+2),
        c(-2,2,2,-2),
        col = "gray90", border = NA)                                         # the confidence band (-2 to 2)

points(treatment.mean, rep(z.score.int, each = 2), col = data.plot$color, pch = 19)
text(treatment.mean + rep(c(0.1,-0.1), 4),
     rep(z.score.int, each = 2),
     # labels = rownames(data.plot),
     labels = lab.subgrp2)
abline(v = treatment.mean.full, lwd = 2, lty = 2)
abline(v = 0)


abline(h = 0)                                                                     # draw the central line
abline(h =  1.964, lty = 2)                                                                     # draw the central line
abline(h = -1.964, lty = 2)                                                                     # draw the central line
xy.current.pos = par("usr")
legend(xy.current.pos[1], xy.current.pos[4], lab.subgrp, bty = "n", cex = font.size[3])
box()

unlist(lambda)
