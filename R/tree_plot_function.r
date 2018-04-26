#' Tree plot for subgroup effect size
#'
#' This function produces a tree plot showing the treatment effect size of subgroups defined by the categories of covariates. The left
#' side shows treatment effect size; the right side indicates what covariate is considered. Each level shows the 95% C.I. of subgroup
#' effect estimate and subgroup sample sizes (by the width of horizontal violet lines). Each subgroup is further divided into several
#' subgroups by categories of the covariate on the lower level. The horizontal line corresponding to the overal effect can be added into
#' each level so as to check homogeneity across subgroup effects with repective to the overall effect. In addition, the function uses log
#' odd ratio and log hazard ratio for displaying subgroup effect sizes in binary and survival data, respectively.
#'
#'@param dat:          a data set
#'@param covari.sel:   a vector of indices of the two covariates
#'@param trt.sel:      a covariate index specifying the treatment code
#'@param resp.sel:     a covariate index specifying the response variable
#'@param outcome.type: a string specifying the type of the response variable, it can be "continuous", or "binary" or  "survival".
#'@param add.aux.line: a logical operator displaying the auxiliary horizontal line for checking heterogeneity in treatment effects if TRUE
#'@param font.size:    a vector specifying the size of labels and text; the first element is for the main title and  the second element
#'@param               is for the text in the left, right and bottom labels; the third is for the unit labels on the y-axis.
#'@param title:        a string specifying the main title.
#'@param lab.y:        a string specifying the y-axis label
#
# eg.1          main.title = paste("Tree plot for treatment effect difference across subgroups", sep = "");
#               lab.y.title = paste("Effect size");
#               treeplt(dat = dat, covari.sel = c(4, 6), trt.sel = 2, resp.sel = 1, outcome.type = "continuous", title = main.title,
#               lab.y = lab.y.title)
#
# eg.2          main.title = paste("Tree plot for treatment effect difference across subgroups", sep = "");
#               lab.y.title = paste("Effect size (log odds ratio)");
#               treeplt(dat = dat2, covari.sel = c(2, 3), trt.sel = 4, resp.sel = 5, outcome.type = "binary", title = main.title,
#               lab.y = lab.y.title)
#
# eg.3          main.title = paste("Tree plot for treatment effect difference across subgroups", sep = "");
#               lab.y.title = paste("Effect size (log hazard ratio)");
#               treeplt(dat = dat3, covari.sel = c(6, 7), trt.sel = 1, resp.sel = c(4,3), outcome.type = "survival", title = main.title,
#               lab.y = lab.y.title)
#
# created by Yi-Da Chiu, 01/08/17
# revised by Yi-Da Chiu, 30/08/17
#' @export
treeplt <- function(dat, covari.sel, trt.sel, resp.sel, outcome.type, add.aux.line = FALSE,  font.size = c(15, 10, 0.5), title = NULL, lab.y = NULL)
{

  ################################################ 0. argument validity check  #################################################################

  if (missing(dat)) stop("Data have not been inputed!")
  if (!(is.data.frame(dat))) stop("The data set is not with a data frame!")

  if (missing(covari.sel)) stop("The variables for defining subgroups have not been specified!")
  if (!(is.numeric(covari.sel))) stop("The variables for defining subgroups are not numeric!")
  for (i in 1 : length(covari.sel)) if (!(is.factor(dat[,covari.sel[i]]))) stop("The variables for defining subgroups are not categorical!")
  if (length(covari.sel) > 6) stop("This function only considers 6 covariates at most for defining subgroups!")
  if (length(unique(covari.sel)) != length(covari.sel)) stop("The covariates inputed should be all different!")

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

  if (!(is.logical(add.aux.line))) stop("The arugument of displaying the auxiliary horizontal line is not logical!")

  if (!(is.numeric(font.size))) stop("The argument about the font sizes of the label and text is not numeric!")
  if (!(length(font.size) == 3)) stop("The font size setups for labels or text should have three components only!")

  ################################################ 1. create subgroup data  ####################################################################

  library(grid)
  library(survival)

  # define the colors we use
  col.line = c("blue", "red", "forestgreen", "orange", "darkorchid1", "darkgoldenrod3", "darkseagreen3", "chartreuse3", "cyan1", "deeppink1")


  n.covari = length(covari.sel)
  lab.vars = names(dat)[covari.sel]              # set the names of the covariates which relates to the defined subgroup; if a covariate
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

  for (i in 1: length(covari.sel)){
    cond = covari.sel == covari.sel[[i]]
    lab.vars[cond] = rep(lab.vars[i], length(which(cond == TRUE)))
  }
  lab.subgrp = c("Full", lab.vars)               # all labels of covariates with the one for the full population
  cats.var = list()                              # the names of the categories of the covariates we consider
  n.subgrp.tol = 1                               # the number of all the subgroups composed of the categories
  n.cat.var = vector()                           # the vector showing the number of the categories of each covariate
  for (i in 1 : length(covari.sel)){
    cats.var[[i]] = names(table(dat[,covari.sel[i]]))
    n.cat.var[i] = length(cats.var[[i]])
    n.subgrp.tol = n.subgrp.tol + sum(prod(n.cat.var[1:i]))
  }
  n.subgrp.acc = c(1, n.cat.var)
  if(max(n.cat.var) > 10) stop("This function only consider 10 categories at most for a variable!")

  cond = list()
  for (i in 1 : length(covari.sel)){
    cond[[i]] = list()
    for (j in 1 : length(cats.var[[i]])){
      cond[[i]][[j]] = which((dat[, covari.sel[i]] == cats.var[[i]][j])  == T )
    }
  }

  ss.subgrp = vector()
  data.subgrp = list()
  ss.subgrp[1] = dim(dat)[1]
  data.subgrp[[1]] = dat                      # the first subgroup is actually the full population
  ind = 1
  n.covari_acc = 1
  while(n.covari_acc <= n.covari)
  {
    if (n.covari_acc == 1){
      intersect.list = list()
      for (i in 1 : length(cats.var[[1]])){
        intersect.list[[1]] = cond[[1]][[i]]
        ind = ind + 1
        ss.subgrp[ind] = length(Reduce(intersect, intersect.list ))
        data.subgrp[[ind]] = dat[Reduce(intersect, intersect.list), ]
      }
    }else if (n.covari_acc == 2){
      intersect.list = list()
      for (i in 1 : length(cats.var[[1]])){
        for (j in 1 : length(cats.var[[2]])){
          intersect.list[[1]] = cond[[1]][[i]]
          intersect.list[[2]] = cond[[2]][[j]]
          ind = ind + 1
          ss.subgrp[ind] = length(Reduce(intersect, intersect.list ))
          data.subgrp[[ind]] = dat[Reduce(intersect, intersect.list), ]
        }
      }
    }else if (n.covari_acc == 3){
      intersect.list = list()
      for (i in 1 : length(cats.var[[1]])){
        for (j in 1 : length(cats.var[[2]])){
          for (k in 1 : length(cats.var[[3]])){
            intersect.list[[1]] = cond[[1]][[i]]
            intersect.list[[2]] = cond[[2]][[j]]
            intersect.list[[3]] = cond[[3]][[k]]
            ind = ind + 1
            ss.subgrp[ind] = length(Reduce(intersect, intersect.list ))
            data.subgrp[[ind]] = dat[Reduce(intersect, intersect.list), ]
          }
        }
      }
    }else if (n.covari_acc == 4){
      intersect.list = list()
      for (i in 1 : length(cats.var[[1]])){
        for (j in 1 : length(cats.var[[2]])){
          for (k in 1 : length(cats.var[[3]])){
            for (l in 1 : length(cats.var[[4]])){
              intersect.list[[1]] = cond[[1]][[i]]
              intersect.list[[2]] = cond[[2]][[j]]
              intersect.list[[3]] = cond[[3]][[k]]
              intersect.list[[4]] = cond[[4]][[l]]
              ind = ind + 1
              ss.subgrp[ind] = length(Reduce(intersect, intersect.list ))
              data.subgrp[[ind]] = dat[Reduce(intersect, intersect.list), ]
            }
          }
        }
      }
    }else if (n.covari_acc == 5){
      intersect.list = list()
      for (i in 1 : length(cats.var[[1]])){
        for (j in 1 : length(cats.var[[2]])){
          for (k in 1 : length(cats.var[[3]])){
            for (l in 1 : length(cats.var[[4]])){
              for (m in 1 : length(cats.var[[5]])){
                intersect.list[[1]] = cond[[1]][[i]]
                intersect.list[[2]] = cond[[2]][[j]]
                intersect.list[[3]] = cond[[3]][[k]]
                intersect.list[[4]] = cond[[4]][[l]]
                intersect.list[[5]] = cond[[5]][[m]]
                ind = ind + 1
                ss.subgrp[ind] = length(Reduce(intersect, intersect.list ))
                data.subgrp[[ind]] = dat[Reduce(intersect, intersect.list), ]
              }
            }
          }
        }
      }
    }else if (n.covari_acc == 6){
      intersect.list = list()
      for (i in 1 : length(cats.var[[1]])){
        for (j in 1 : length(cats.var[[2]])){
          for (k in 1 : length(cats.var[[3]])){
            for (l in 1 : length(cats.var[[4]])){
              for (m in 1 : length(cats.var[[5]])){
                for (n in 1 : length(cats.var[[6]])){
                  intersect.list[[1]] = cond[[1]][[i]]
                  intersect.list[[2]] = cond[[2]][[j]]
                  intersect.list[[3]] = cond[[3]][[k]]
                  intersect.list[[4]] = cond[[4]][[l]]
                  intersect.list[[5]] = cond[[5]][[m]]
                  intersect.list[[6]] = cond[[6]][[n]]
                  ind = ind + 1
                  ss.subgrp[ind] = length(Reduce(intersect, intersect.list ))
                  data.subgrp[[ind]] = dat[Reduce(intersect, intersect.list), ]
                }
              }
            }
          }
        }
      }
    }
    n.covari_acc = n.covari_acc + 1
  }

  # create matrices for treatment size and standard error of MLE

  treatment.mean = matrix(0, nrow = n.subgrp.tol, ncol = 1)
  treatment.std = matrix(0, nrow = n.subgrp.tol, ncol = 1)
  treatment.low = matrix(0, nrow = n.subgrp.tol, ncol = 1)
  treatment.upp = matrix(0, nrow = n.subgrp.tol, ncol = 1)
  for (i in 1 : n.subgrp.tol)
  {
    if (sum((data.subgrp[[i]]$trt == "1")) == 0 | sum((data.subgrp[[i]]$trt == "0")) == 0){
      treatment.mean[i] = NA
      treatment.std[i] = NA
      treatment.low[i] = NA
      treatment.upp[i] = NA
    }else{

      if (outcome.type == "continuous"){
        model.int = lm(resp ~ trt,  data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coefficients[2, 1]
        treatment.std[i] = model.sum$coefficients[2, 2]
        treatment.low[i] = model.sum$coefficients[2, 1] - 1.96 * treatment.std[i]
        treatment.upp[i] = model.sum$coefficients[2, 1] + 1.96 * treatment.std[i]
      }else if (outcome.type == "binary"){
        model.int = glm(resp ~ trt, family = "binomial", data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coefficients[2, 1]
        treatment.std[i] = model.sum$coefficients[2, 2]
        treatment.low[i] = model.sum$coefficients[2, 1] - 1.96 * treatment.std[i]
        treatment.upp[i] = model.sum$coefficients[2, 1] + 1.96 * treatment.std[i]
      }else if (outcome.type == "survival"){
        model.int = coxph(Surv(time, status) ~ trt, data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coef[1, 1]
        treatment.std[i] = model.sum$coef[1, 3]
        treatment.low[i] = model.sum$coefficients[1, 1] - 1.96 * treatment.std[i]
        treatment.upp[i] = model.sum$coefficients[1, 1] + 1.96 * treatment.std[i]
      }

    }
  }

  ################################################ 2. produce a graph  #################################################################

  dev.new(width=11,height=10,noRStudioGD = TRUE)

  vp <- viewport(x= 0.08, y = 0.08 + (1 - 2*0.08), width=(1 - 2*0.08), height=0.05, just = c("left", "bottom"))
  pushViewport(vp)
  grid.text(title, gp = gpar(fontsize=font.size[1], fontface = 2))
  upViewport()

  vp <- viewport(x=0.08, y =0.08, width= (1 - 2*0.08), height=(1 - 2*0.08), just = c("left", "bottom"))
  pushViewport(vp)
  grid.rect()

  axis.max = vector()
  axis.min = vector()

  ## the first top level

  vp <- viewport(x=0, y = 1 - 1/(n.covari + 1), width= 1, height= 1/(n.covari + 1), just = c("left", "bottom"))
  pushViewport(vp)

  vp <- viewport(width= 1, height= 0.9)              # the viewpoint's height becomes 0.9 time as large as the original one; without the top and the bottom areas with 0.1 in height
  pushViewport(vp)
  grid.rect(gp = gpar(fill= "gray89", col = NA))     # gray80
  length.ann.y = 9                                   # divided the area into length.ann.y - 1 smaller areas
  for (i in 1 : (length.ann.y - 2))                  # draw the lines for the area division
  {grid.lines(c(0,1), c(i * 1/(length.ann.y - 1), i * 1/(length.ann.y - 1)), gp=gpar(col = "gray100", lty = "solid", lwd = 2.5))  #gray95
  }
  for (i in 1 : (length.ann.y - 1))                  # draw the lines for the area division
  {
    st = 1/((length.ann.y - 1) * 2) + (i - 1) * 1/(length.ann.y - 1)
    grid.lines(c(0,1), c(st, st), gp=gpar(col = "gray98"))  #gray89
  }

  axis.max[1] = ceiling(treatment.upp[1])
  axis.min[1] = floor(treatment.low[1])
  y.max = (treatment.upp[1] - axis.min[1] )/(axis.max[1] - axis.min[1])
  y.min = (treatment.low[1] - axis.min[1] )/(axis.max[1] - axis.min[1])
  grid.lines(c(1/2, 1/2), c(y.min, y.max))                                                                              # draw the line representing the C.I. of the effect size for the full population
  grid.lines(c(1/2-0.15, 1/2+0.15), c(1/2 *(y.min + y.max), 1/2 *(y.min + y.max)), gp=gpar(col = "mediumvioletred"))    # draw the line representing the sample size of the full population
  grid.lines(c(1/2-0.01, 1/2+0.01), c(y.min, y.min))                                                                    # draw the line representing the top of the C.I.
  grid.lines(c(1/2-0.01, 1/2+0.01), c(y.max, y.max))                                                                    # draw the line representing the bottom of the C.I.
  upViewport(2)

  ## the second to the (n.covari + 1)-th top level

  for (j in (2 : (n.covari + 1)))
  {
    vp <- viewport(x=0, y = 1 - 1/(n.covari + 1) * j, width= 1, height = 1/(n.covari + 1), just = c("left", "bottom"))
    pushViewport(vp)


    vp <- viewport(width= 1, height= 0.9)
    pushViewport(vp)

    grid.rect(gp = gpar(fill= "gray89", col = NA))  #gray80
    length.ann.y = 9
    for (i in 1 : (length.ann.y - 2))
    {grid.lines(c(0,1), c(i * 1/(length.ann.y - 1), i * 1/(length.ann.y - 1)), gp=gpar(col = "gray100", lty = "solid", lwd = 2.5))  #gray95
    }
    for (i in 1 : (length.ann.y - 1))
    {
      st = 1/((length.ann.y - 1) * 2) + (i - 1) * 1/(length.ann.y - 1)
      grid.lines(c(0,1), c(st, st), gp=gpar(col = "gray98"))  #gray89
    }

    idx.floor = 1 : prod(n.subgrp.acc[1:j]) + sum(sapply(seq(1, j-1, 1), function(x) prod(n.subgrp.acc[1:x]))) # indicate what subgroups' effect size should be depicted
    axis.max[j] = max(ceiling(treatment.upp[idx.floor]),  na.rm = TRUE)
    axis.min[j] = min(floor(treatment.low[idx.floor]),  na.rm = TRUE)
    ind =  sum(sapply(seq(1, j-1, 1), function(x) prod(n.subgrp.acc[1:x])))                                    # indicate the number of subgroups whose effect sizes have been depicted
    n.idx.floor = length(idx.floor)
    x.gap = 1/ (n.idx.floor + 1)
    for (i in 1: n.idx.floor){
      ind = ind + 1
      if ( is.na(treatment.mean[ind])){
        y.max = 0.5
        y.min = 0.5
      }else{
        y.max = (treatment.upp[ind] - axis.min[j] )/(axis.max[j] - axis.min[j])
        y.min = (treatment.low[ind] - axis.min[j] )/(axis.max[j] - axis.min[j])
        grid.lines(c(x.gap * i -0.01, x.gap * i +0.01), c(y.min, y.min))
        grid.lines(c(x.gap * i -0.01, x.gap * i +0.01), c(y.max, y.max))
        grid.lines(c(x.gap * i, x.gap * i), c(y.min, y.max))
      }

      grid.lines(c(x.gap * i - 0.15*(ss.subgrp[ind])/ss.subgrp[1], x.gap * i + 0.15*(ss.subgrp[ind])/ss.subgrp[1]),
                 c(1/2 *(y.min + y.max), 1/2 *(y.min + y.max)), gp = gpar(col = "mediumvioletred"))
    }
    if (add.aux.line == TRUE){
      y.max = (treatment.upp[1] - axis.min[j] )/(axis.max[j] - axis.min[j])
      y.min = (treatment.low[1] - axis.min[j] )/(axis.max[j] - axis.min[j])
      grid.lines(c(0, 1), c(1/2 *(y.min + y.max), 1/2 *(y.min + y.max)), gp=gpar(col = "mediumvioletred", lty = 2))    # draw the line representing the sample size of the full population
    }


    upViewport(2)
  }

  upViewport()

  #### add covariate labels (right)

  vp <- viewport(x= 1 - 0.08, y =0.08, width= 0.05, height= 1 - 2*0.08, just = c("left", "bottom"))
  pushViewport(vp)

  for (i in 1 : (n.covari + 1))
  {
    vp <- viewport(x=0, y = 1 - 1/(n.covari + 1) * i, width= 1, height= 1/(n.covari + 1), just = c("left", "bottom"))
    pushViewport(vp)
    grid.text(lab.subgrp[i], rot = 90, gp = gpar(fontsize = font.size[2], fontface = 2))
    upViewport()
  }
  upViewport()


  #### add labels and axis (left)

  vp <- viewport(x= 0, y =0.08, width= 0.05, height= 1 - 2*0.08, just = c("left", "bottom"))
  pushViewport(vp)
  grid.text(lab.y, rot = 90, gp = gpar(fontsize = font.size[2], fontface = 2))
  upViewport()

  vp <- viewport(x=0.08, y =0.08, width= 1 - 2*0.08, height= 1 - 2*0.08, just = c("left", "bottom"))
  pushViewport(vp)
  j = 0
  for (i in (n.covari + 1) : 1)
  {
    j = j + 1
    vp <- viewport(x=0, y = (i - 1) * 1/(n.covari + 1), width= 1, height= 1/(n.covari + 1), just = c("left", "bottom"))
    pushViewport(vp)
    vp <- viewport(width= 1, height= 0.9, just = c("left", "center"))
    pushViewport(vp)
    grid.yaxis(seq(0,1, len = 9), vp=viewport(x=0), label = round(seq(axis.min[j], axis.max[j], len =9), 2), gp = gpar(cex = font.size[3]),
               edits = gEdit(gPath="labels", rot=0))
    upViewport(2)
  }
  upViewport()

  ######## add lines for subdivisions

  vp <- viewport(x=0.08, y = 0.08, width= 0.84, height= 0.84, just = c("left", "bottom"))
  pushViewport(vp)

  col.line1 = col.line[1: max(n.cat.var)]
  n.subgrp.acc.rv = rev(n.subgrp.acc)
  for (k in (n.covari + 1) : 2)
  {
    y.pos.st = 1/(n.covari + 1) * (k - 1) + 1/(n.covari + 1) * (1 - 0.9)/2
    y.pos.se = 1/(n.covari + 1) * (k - 1) - 1/(n.covari + 1) * (1 - 0.9)/2

    x.gap.st = 1/(prod(n.subgrp.acc.rv[k:(n.covari + 1)]) + 1)
    x.gap.se = 1/(prod(n.subgrp.acc.rv[(k-1):(n.covari + 1)]) + 1)
    n.idx.floor = prod(n.subgrp.acc.rv[k:(n.covari + 1)])                                                             # indicate the number of subgroups which point to their bifircation

    for (i in 1: n.idx.floor)
    {
      x.pos.st = x.gap.st * i

      for (m in 1 : n.subgrp.acc.rv[k-1] ){
        x.pos.se = m * x.gap.se +(i-1)* n.subgrp.acc.rv[k-1] * x.gap.se
        grid.lines(c(x.pos.st, x.pos.se), c(y.pos.st, y.pos.se), gp=gpar(col = col.line1[m], lty = "solid", lwd = 3))
      }
    }
  }

  upViewport()

  vp <- viewport(x=0.08, y =0.08, width= (1 - 2*0.08), height=(1 - 2*0.08), just = c("left", "bottom"))
  pushViewport(vp)
  grid.rect()

  upViewport()

  ######## add annotation of the colors of the division

  vp <- viewport(x=0.08, y = 0, width= 0.84, height= 0.08, just = c("left", "bottom"))
  pushViewport(vp)

  lab.line = vector()
  for (m in 1 : max(n.cat.var)){
    vp <- viewport(x= (m -1) * (1/max(n.cat.var)), y = 0, width= 1/max(n.cat.var), height= 1, just = c("left", "bottom"))
    pushViewport(vp)
    lab.line[m] = paste("Category", m)
    grid.lines(c(0.4, 0.6), c(0.7, 0.7), gp=gpar(col = col.line1[m], lty = "solid", lwd = 3))
    grid.text(lab.line[m], gp = gpar(fontsize = font.size[2], fontface = 2))
    upViewport()
  }
  upViewport()
}
