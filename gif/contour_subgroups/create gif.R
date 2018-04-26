data(prca)
dat <- prca
vars = data.frame(variable = names(dat), index = 1:length(names(dat)))
dat
covari.sel = c(8,9)
trt.sel = 3
resp.sel = c(1,2)
outcome.type = "survival"
effect = "HR"
setup.ss =  c(10,60,15,30)
n.grid = c(100,100)
# brk.es = c(-2, -1, 0, 1, 2),
brk.es = seq(-3,3,length.out = 31)
n.brk.axis =  7
para.plot = c(0.5, 2, 6)
font.size = c(1, 1, 0.7, 0.7, 0.75)
title = main.title
subtitle = sub.title
strip = paste("Treatment effect size (log hazard ratio)")
filled = T
names(dat)[trt.sel] = "trt"                            # rename the variable for treatment code
if (outcome.type == "continuous"){
  names(dat)[resp.sel] = "resp"                        # rename the response variable
}else if (outcome.type == "binary"){
  names(dat)[resp.sel] = "resp"                        # rename the response variable
}else if (outcome.type == "survival"){
  names(dat)[resp.sel[1]] = "time"                     # rename the response variable for survival time
  names(dat)[resp.sel[2]] = "status"                     # rename the response variable for survival right censoring status
}


# Calculate overall Treatment effect ### TODO Look for confidence intervals ----------
if (outcome.type == "continuous"){
  model.int = lm(resp ~ trt,  data = dat)
  model.sum = summary(model.int)
  overall.treatment.mean = model.sum$coefficients[2, 1]
  overall.treatment.upper = 0
  overall.treatment.lower = 0
}else if (outcome.type == "binary"){
  model.int = glm(resp ~ trt, family = "binomial", data = dat)
  model.sum = summary(model.int)
  overall.treatment.mean = model.sum$coefficients[2, 1]
  overall.treatment.upper = 0
  overall.treatment.lower = 0
}else if (outcome.type == "survival"){
  if (effect == "HR"){
    model.int = coxph(Surv(time, status) ~ trt, data = dat)
    model.sum = summary(model.int)
    overall.treatment.mean = model.sum$coef[1, 1]
    overall.treatment.upper = log(model.sum$conf.int[1, 4])
    overall.treatment.lower = log(model.sum$conf.int[1, 3])
  }
  if (effect == "RMST"){
    dat.subgr.i = dat
    rmst = survRM2::rmst2(time = dat.subgr.i$time, status = dat.subgr.i$status,
                          arm = dat.subgr.i$trt, tau = time)
    overall.treatment.mean = rmst$unadjusted.result[1,1]
    overall.treatment.upper = 0
    overall.treatment.lower = 0
  }
}

covari1.table = round(sort(dat[, covari.sel[1]]), 4)
lab.vars = names(dat)[covari.sel]

N1 = setup.ss[1]; N2 = setup.ss[2]
cutpoint.covar1 = list()

cutpoint.covar1[[1]] = vector()                 # the lower cutting points for the first covariate
cutpoint.covar1[[2]] = vector()                 # the upper cutting points for the first covariate

low.bd.covar1.idx = 1
upp.bd.covar1.idx = N2
ss.full = dim(dat)[1]
i = 0
while (upp.bd.covar1.idx < ss.full){
  i = i + 1
  low.bd.covar1.idx = 1  + (i-1) * (N2 - N1)
  upp.bd.covar1.idx = min(N2 + (i-1) * (N2 - N1), nrow(dat))
  cutpoint.covar1[[1]][i] = covari1.table[low.bd.covar1.idx]
  cutpoint.covar1[[2]][i] = covari1.table[upp.bd.covar1.idx]
}


png(file="gif/contour_subgroups/gif_%02d_acrossx.png", width=1000, height=1000)
# Create first gif
n.cutoffs = length(cutpoint.covar1[[1]])
for (i in 1: n.cutoffs){
  plot(dat[lab.vars], type = "n")
  abline(v=cutpoint.covar1[[1]][i], col = "red")
  abline(v=cutpoint.covar1[[2]][i], col = "red")
  sub = which(dat[lab.vars[1]]>=cutpoint.covar1[[1]][i] & dat[lab.vars[1]]<=cutpoint.covar1[[2]][i])
  if(i==n.cutoffs){
    sub = which(dat[lab.vars[1]]>=cutpoint.covar1[[1]][i])
  }
  points(dat[sub, lab.vars], col = "red", lwd = 3)
  points(dat[-sub, lab.vars], col = "black")
}
dev.off()
# convert the .png files to one .gif file using ImageMagick.
# The system() function executes the command as if it was done
# in the terminal. the -delay flag sets the time between showing
# the frames, i.e. the speed of the animation.

# setwd("/Users/nicoballarini/Desktop/SubgrPlots 2018 03 18/gif/")
# system("convert -delay 5 *acrossx.png example_1.gif")

















## searching the index set of subgroups over the first covariate

idx.covar1 = list()                              # the index set of subgroups over the first covariate
n.subgrp.covar1 = length(cutpoint.covar1[[1]])   # the number of subgroups over the first covariate
ss.subgrp.covar1 = vector()
for (i in 1 : n.subgrp.covar1 ){
  idx.covar1[[i]] = which((dat[, covari.sel[1]] >= cutpoint.covar1[[1]][i] &
                             dat[, covari.sel[1]] <= cutpoint.covar1[[2]][i] ) == T  )
  ss.subgrp.covar1[i] = length(idx.covar1[[i]])

}

## decide the cutting point over the second covariate

N3 = setup.ss[3]; N4 = setup.ss[4]
cutpoint.covar2 = list()
i=1
for (i in 1 : n.subgrp.covar1){
  covari2.table =  round(sort(dat[idx.covar1[[i]], covari.sel[2]]), 4)
  cutpoint.covar2[[i]] = list()
  cutpoint.covar2[[i]][[1]] = vector()          # the lower cutting points for the second covariate
  cutpoint.covar2[[i]][[2]] = vector()          # the upper cutting points for the second covariate

  low.bd.covar2.idx = 1
  upp.bd.covar2.idx = N4
  j = 0
  stop = 0
  while (stop == 0){
    j = j + 1
    low.bd.covar2.idx = 1  + (j - 1) * (N4 - N3)
    upp.bd.covar2.idx = min(N4 + (j - 1) * (N4 - N3), length(covari2.table))
    upp.bd.covar2.idx.stop = N4 + (j - 1) * (N4 - N3)
    cutpoint.covar2[[i]][[1]][j] = covari2.table[low.bd.covar2.idx]
    cutpoint.covar2[[i]][[2]][j] = covari2.table[upp.bd.covar2.idx]

    if (upp.bd.covar2.idx >= length(covari2.table)) {cutpoint.covar2[[i]][[2]][j] = max(covari2.table)}
    if (upp.bd.covar2.idx.stop > length(covari2.table)) {stop=1}
  }
}

dev.off()
i = 1
j = 1
png(file="gif/contour_subgroups/gif_%02d_acrossxy.png", width=2000, height=1000)
n.cutoffs = length(cutpoint.covar1[[1]])
for (i in 1:(n.cutoffs)){
    n.cutoffs.y = length(cutpoint.covar2[[i]][[1]])
    for (j in 1:n.cutoffs.y){
      layout(mat = matrix(c(1,2), ncol = 2, nrow = 1), widths = c(1,1))
      # First plot
      plot(dat[lab.vars], type = "n")
      abline(v=cutpoint.covar1[[1]][i], col = "red")
      abline(v=cutpoint.covar1[[2]][i], col = "red")
      sub = which(dat[lab.vars[1]]>=cutpoint.covar1[[1]][i] & dat[lab.vars[1]]<=cutpoint.covar1[[2]][i])
      if(i==n.cutoffs){
        sub = which(dat[lab.vars[1]]>=cutpoint.covar1[[1]][i])
      }
      points(dat[sub, lab.vars], col = "red", lwd = 3)
      points(dat[-sub, lab.vars], col = "black")

      # Second plot
      plot(dat[lab.vars], type = "n")
      abline(v=cutpoint.covar1[[1]][i], col = "red")
      abline(v=cutpoint.covar1[[2]][i], col = "red")
      points(dat[sub, lab.vars], col = "red", lwd = 3)
      points(dat[-sub, lab.vars], col = "black")
      sub = which(dat[lab.vars[1]]>=cutpoint.covar1[[1]][i] & dat[lab.vars[1]]<=cutpoint.covar1[[2]][i])
      sub.xy = which(dat[lab.vars[1]]>=cutpoint.covar1[[1]][i] &
                    dat[lab.vars[1]]<=cutpoint.covar1[[2]][i] &
                    dat[lab.vars[2]]>=cutpoint.covar2[[i]][[1]][j] &
                    dat[lab.vars[2]]<=cutpoint.covar2[[i]][[2]][j])
      if(i==n.cutoffs){
        sub = which(dat[lab.vars[1]]>=cutpoint.covar1[[1]][i])
      }
      abline(h=cutpoint.covar2[[i]][[1]][j], col = "green")
      abline(h=cutpoint.covar2[[i]][[2]][j], col = "green")
      points(dat[sub.xy, lab.vars], col = "green", lwd = 3)
    }
}
dev.off()

# setwd("/Users/nicoballarini/Desktop/SubgrPlots 2018 03 18/gif/")
# system("convert -delay 60 *acrossx.png gif_xy.gif")








## searching the index set of subgroups over the second covariate

idx.covar2 = list()
n.subgrp.covar2 = vector()
ss.subgrp.covar2 = list()
for (i in 1 : n.subgrp.covar1){
  idx.covar2[[i]] = list()
  ss.subgrp.covar2[[i]] = list()
  for (j in 1 :  length(cutpoint.covar2[[i]][[1]]) ){
    idx.covar2[[i]][[j]] = vector()
    ss.subgrp.covar2[[i]][[j]] = vector()
    idx.replace= which((dat[idx.covar1[[i]], covari.sel[2]] >= cutpoint.covar2[[i]][[1]][j] &
                          dat[idx.covar1[[i]], covari.sel[2]] <= cutpoint.covar2[[i]][[2]][j] ) == T  )
    idx.covar2[[i]][[j]] = idx.covar1[[i]][idx.replace]

    ss.subgrp.covar2[[i]][[j]] = length(idx.covar2[[i]][[j]])
  }
  n.subgrp.covar2[i] = length(idx.covar2[[i]])
}

## create the data set for subgroups over the first and second covariates


treatment.mean = vector()
ss.subgrp = vector()
x = vector()
y = vector()
k = 0
for (i in 1 : n.subgrp.covar1 ){
  for (j in 1 :  length(cutpoint.covar2[[i]][[1]])){
    k =  k + 1

    cond1 = sum(dat[idx.covar2[[i]][[j]],]$trt == "0") == 0
    cond2 = sum(dat[idx.covar2[[i]][[j]],]$trt == "1") == 0

    if (cond1 | cond2 ){
      treatment.mean[i] = NA
    }else{

      if (outcome.type == "continuous"){

        model.int = lm(resp ~ trt,  data = dat[idx.covar2[[i]][[j]],])
        model.sum = summary(model.int)
        treatment.mean[k] = model.sum$coefficients[2, 1]

      }else if (outcome.type == "binary"){

        model.int = glm(resp ~ trt,  family = "binomial", data = dat[idx.covar2[[i]][[j]],])
        model.sum = summary(model.int)
        treatment.mean[k] = model.sum$coefficients[2, 1]

      }else if (outcome.type == "survival"){

        model.int = coxph(Surv(time, status) ~ trt, data = dat[idx.covar2[[i]][[j]],])
        model.sum = summary(model.int)
        treatment.mean[k] = model.sum$coef[1, 1]

      }
    }

    x[k] = (cutpoint.covar1[[2]][i] + cutpoint.covar1[[1]][i])/2
    y[k] = (cutpoint.covar2[[i]][[2]][j] + cutpoint.covar2[[i]][[1]][j])/2
    ss.subgrp[k] = dim(dat[idx.covar2[[i]][[j]],] )[1]
  }
}

if(verbose){
  cat("The number of subgroups over the first covariate is", n.subgrp.covar1, "\n")
  cat("The subgroup sample sizes over the first covariate are actually", ss.subgrp.covar1, "\n")
  cat("The number of further divided subgroups over the second covariate is", n.subgrp.covar2, "\n")
  #print("The sampel sizes of the further divided subgroups over the second covariate are", ss.subgrp.covar2, "\n")
}
