#' Contour plot for subgroup effect size
#'
#' this function produces a contour plot showing the treatment effect size of subgroups. The subgroups are first defined by certain
#' ranges of the first continuous covariate; and then further divided into smaller subgroup by certain ranges of the second covariate
#' . The subgroups over the first covariate have a sample size close to one pre-specified value (N2) and any neighboring subgroups
#' have an overlap size near the second pre-specified value (N1). Similarly, each subgroup over the first covariate has a sample size
#' near the third pre-specified value (N4), and any neighboring subgroups which are further divided over the second covariate have a
#' sample size near the fourth pre-specified value (N3). The x-coordinate and y-coordinate of a point indicates the middle point of
#' the range over the first covariate and that over the second covariate, respectively. The contours show approximate effect sizes
#' which are obtained by fitting grid points over the polynormial surface interpolating the points corresponding to subgroups.Note
#' that there are three parameters for controlling the setting of contours. In addition, the function uses log odd ratio and log
#' hazard ratio for displaying subgroup effect sizes in binary and survival data, respectively. Also, the actual subgroup sample
#' sizes over the covariates are shown on the console window.
#'
#' @param dat:            a data set
#' @param covari.sel:     a vector of indices of the two covariates
#' @param trt.sel:        a variable index specifying the treatment code
#' @param resp.sel:       a variable index specifying the response variable
#' @param outcome.type:   a string specifying the type of the response variable, it can be "continuous", or "binary" or  "survival".
#' @param setup.ss:       a vector specifying approximate subgroup sample size and neibourghing subgroup overlap sample size. The first and the second elements
#'                  are for overlap sizes and subgroup sample sizes over the first covariate; the third and thefourth are for further divided overlap sizes
#'                  and subgroup sample sizes over the second covariate.
#' @param n.grid:         a vector specifying the numbers of the grid points on the x-axis and the y-axis respectively.
#' @param brk.es:         a vector specifying the break points on effect size, where each range partition is given with a different colour on points.
#' @param para.plot:      a vector specifying the parameters of the contour plot; the first value is for controlling the degree of smoothing; the second
#'                  is for controlling the degree of the polynomials fitting to be used (normally 1 or 2); the third is for controlling the number of
#'                  contour lines.
#' @param font.size:      a vector specifying the size of labels and text; the first element is for the main title, the second is for for x-axis and y-axis
#'                  labels; the third is for the subtitle; the fourth is for the text in the legend; the fifth is for the labels on contour lines.
#' @param title:          a string specifying the main title.
#' @param subtitle:       strings specifying the subtitle
#
# e.g.            covari.sel = c(3, 9);
#                 main.title = paste("Effect sizes (ES) on the plane of ", names(dat)[covari.sel[1]], "and", names(dat)[covari.sel[2]]) ;
#                 setup.ss = c(35,40,15,20);
#                 sub.title = paste("(N1 approx.", setup.ss[1], "; N2 approx.", setup.ss[2], "; N3 approx.", setup.ss[3], "; N4 approx.", setup.ss[4], ")" )
#                 contour.plt(dat, covari.sel = c(3, 9), trt.sel = 2, resp.sel = 1, outcome.type = "continuous", setup.ss = c(35,40,15,20), title = main.title,
#                 subtitle = sub.title, n.grid = c(41, 41), brk.es = c(0, 1, 2, 3), para.plot = c(0.35, 2, 20), font.size = c(1.5,1.2,1,0.85,0.8))
#
# created by Yi-Da Chiu, 01/08/17
# revised by Yi-Da Chiu, 18/08/17
#' @export
contourplt <- function(dat, covari.sel, trt.sel, resp.sel, outcome.type, setup.ss, n.grid = c(41, 41), brk.es = c(0, 1, 2, 3),
                        para.plot = c(0.35, 2, 20), font.size = c(1.5,1.2,1,0.85,0.8), title = NULL, subtitle = NULL)
{


################################################ 0. argument validity check  #################################################################

  if (missing(dat)) stop("Data have not been inputed!")
  if (!(is.data.frame(dat))) stop("The data set is not with a data frame!")

  if (missing(covari.sel)) stop("The variables for defining subgroups have not been specified!")
  if (!(is.numeric(covari.sel))) stop("The variables for defining subgroups are not numeric!")
  if (length(covari.sel) > 2) stop("This function only considers 2 covariates at most for defining subgroups!")

  if (missing(trt.sel)) stop("The variable specifying the treatment code (for treatment / control groups) has not been specified!")
  if (!(length(trt.sel) == 1)) stop("The variable specifying the treatment code can not have more than one component!")
  if (!(is.factor(dat[, trt.sel]))) stop("The variable specifying the treatment code is not categorical!")
  if (length(names(table(dat[, trt.sel]))) > 2) stop("The variable specifying the treatment code is not binary!")
  if (sum(is.element(names(table(dat[, trt.sel])), c("0","1"))) != 2) stop("The treatment code is not 0 or 1!")

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

  if (missing(setup.ss)) stop("The setting for subgroup sample size and overlap have not been specified!")
  if (!(is.numeric(setup.ss))) stop("The setting for subgroup sample size and overlap are not numeric!")
  if (length(setup.ss) !=  4) stop("The setting for subgroup smaple size and overlap does not have four elements!")
  if ((setup.ss[1] > setup.ss[2]) || (setup.ss[3] > setup.ss[4]) || (setup.ss[4] > setup.ss[2])){
    stop("subgroup overlap sample sizes is larger than subgroup sample size! Or subgroup sample sizes over the first covariate are not
          larger than their further divided subgroup sample sizes over the second covariate!")
  }

  if (missing(n.grid)) stop("The vector specifying the numbers of the grid points has not been specified!")
  if (!(length(n.grid) == 2)) stop("The vector specifying the numbers of the grid points does not have two components only!")
  if (!(is.numeric(n.grid)) || (sum(n.grid < 2) != 0 )) stop("The vector specifying the numbers of the grid points is not numeric or has
                                                             a value less than 2!")

  if (missing(brk.es)) stop("The vector specifying the numbers of break points for effect sizes has not been specified!")
  if (length(brk.es) > 5) stop("The vector specifying the numbers of break points for effect sizes should have five components only!")
  if (!(is.numeric(brk.es))) stop("The vector specifying the numbers of break points for effect sizes is not numeric!")

  if (missing(para.plot)) stop("The vector specifying the parameters of the contour plot has not been specified!")
  if (!(length(para.plot) == 3)) stop("The vector specifying the parameters of the contour plot should have 3 components only!")
  if (!(is.numeric(para.plot)) || (sum(para.plot < 0) != 0 )) stop("The vector specifying the parameters of the contour plot is not numeric or has
                                                             a negative element!")
  if (!(para.plot[2] %in% c(0, 1, 2)) ) stop("The second plot parameter is given with a unallowable value!")
  if (!(para.plot[3]%%1==0) || (para.plot[3] < 0)  ) stop("The third plot parameter should be a positive integer!")

  if (!(is.numeric(font.size))) stop("The argument about the font sizes of the label and text is not numeric!")
  if (!(length(font.size) == 5)) stop("The length of the font size settings is not 5!!")

  ################################################ 1. create subgroup data  #################################################################

  require(geoR)
  library(survival)

  names(dat)[trt.sel] = "trt"                            # rename the variable for treatment code
  if (outcome.type == "continuous"){
    names(dat)[resp.sel] = "resp"                        # rename the response variable
  }else if (outcome.type == "binary"){
    names(dat)[resp.sel] = "resp"                        # rename the response variable
  }else if (outcome.type == "survival"){
    names(dat)[resp.sel[1]] = "time"                     # rename the response variable for survival time
    names(dat)[resp.sel[2]] = "status"                     # rename the response variable for survival right censoring status
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
    low.bd.covar1.idx = 1 + (i-1) * (N2 - N1)
    upp.bd.covar1.idx = N2+ (i-1) * (N2 - N1)
    cutpoint.covar1[[1]][i] = covari1.table[low.bd.covar1.idx]
    cutpoint.covar1[[2]][i] = covari1.table[upp.bd.covar1.idx]
  }


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
  for (i in 1 : n.subgrp.covar1){
    covari2.table =  round(sort(dat[idx.covar1[[i]], covari.sel[2]]), 4)
    cutpoint.covar2[[i]] = list()
    cutpoint.covar2[[i]][[1]] = vector()          # the lower cutting points for the second covariate
    cutpoint.covar2[[i]][[2]] = vector()          # the upper cutting points for the second covariate

    low.bd.covar2.idx = 1
    upp.bd.covar2.idx = N4
    j = 0
    while (upp.bd.covar2.idx < length(covari2.table)){
      j = j + 1
      low.bd.covar2.idx = 1 + (j - 1) * (N4 - N3)
      upp.bd.covar2.idx = N4+ (j - 1) * (N4 - N3)
      cutpoint.covar2[[i]][[1]][j] = covari2.table[low.bd.covar2.idx]
      cutpoint.covar2[[i]][[2]][j] = covari2.table[upp.bd.covar2.idx]

      if (upp.bd.covar2.idx >= length(covari2.table)) {cutpoint.covar2[[i]][[2]][j] = max(covari2.table)}
    }
  }


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


  cat("The number of subgroups over the first covariate is", n.subgrp.covar1, "\n")
  cat("The subgroup sample sizes over the first covariate are actually", ss.subgrp.covar1, "\n")
  cat("The number of further divided subgroups over the second covariate is", n.subgrp.covar2, "\n")
  #print("The sampel sizes of the further divided subgroups over the second covariate are", ss.subgrp.covar2, "\n")

  ################################################ 2. produce a graph  #################################################################

  dev.new(width = 10, height = 10, noRStudioGD = TRUE)

  treatment.df = data.frame(x, y, treatment.mean)
  treatment.df.model = loess(treatment.mean ~ x*y, data = treatment.df, span = para.plot[1], degree = para.plot[2])

  xy.fit.pt = expand.grid(list(x1 = seq(min(x) + 0.01, max(x) - 0.01, len = n.grid[1]), y1 = seq(min(y) + 0.01, max(y) - 0.01, len = n.grid[2]) ))
  treatment.df.model.fit = predict(treatment.df.model, newdata = xy.fit.pt)

  x.range = seq(min(x) + 0.01, max(x) - 0.01, len = n.grid[1])
  y.range = seq(min(y) + 0.01, max(y) - 0.01, len = n.grid[2])

  plot(x, y, type = "p", xlim = range(x.range), ylim = range(y.range), xlab = lab.vars[1], ylab = lab.vars[2], main = title,
       sub = subtitle, col = "gray80", cex.main = font.size[1],
       cex.lab = font.size[2], cex.sub = font.size[3])
  cutoff.es = rev(c(-Inf, brk.es, Inf))
  col.point = c("red", "blue", "orange", "green", "violet")
  for (i in 1 : (length(cutoff.es) - 2) ){
    points(x[setdiff(which((treatment.mean > cutoff.es[i + 1]) ),  which((treatment.mean > cutoff.es[i]) ))],
           y[setdiff(which((treatment.mean > cutoff.es[i + 1]) ),  which((treatment.mean > cutoff.es[i]) ))], col = col.point[i] )
  }

  contour(x.range, y.range, treatment.df.model.fit, levels = round(seq(min(treatment.mean), max(treatment.mean), len = para.plot[3]), 2),  vfont = c("sans serif", "plain"),
          labcex = font.size[5], col = "darkgreen", lty = "solid", add = TRUE)         # "len" in levels controls the number of levels
  lab0.es = paste("ES >", brk.es[length(brk.es)])
  lab1.es = vector()
  for (i in length(brk.es) : 2){
    lab.es.temp = paste(brk.es[i - 1], "< ES <", brk.es[i])
    lab1.es = c(lab1.es, lab.es.temp)
  }
  lab2.es =paste("ES <", brk.es[1])

  lab.es = c(lab0.es, lab1.es, lab2.es)
  legend("topleft", lab.es, cex = font.size[4], col= c(col.point[1:length(brk.es)], "gray80"), pch = 1, bg = "white")

}
