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
#' @param dat            a data set
#' @param covari.sel     a vector of indices of the two covariates
#' @param trt.sel        a variable index specifying the treatment code
#' @param resp.sel       a variable index specifying the response variable
#' @param outcome.type   a string specifying the type of the response variable, it can be "continuous", or "binary" or  "survival".
#' @param setup.ss       a vector specifying approximate subgroup sample size and neibourghing subgroup overlap sample size. The first and the second elements
#'                  are for overlap sizes and subgroup sample sizes over the first covariate; the third and thefourth are for further divided overlap sizes
#'                  and subgroup sample sizes over the second covariate.
#' @param n.grid         a vector specifying the numbers of the grid points on the x-axis and the y-axis respectively.
#' @param brk.es        a vector specifying the break points on effect size, where each range partition is given with a different colour on points.
#' @param n.brk.axis   a number specifying the number of breakpoints dividing the axis of the argument "range.strip".
#' @param para.plot      a vector specifying the parameters of the contour plot; the first value is for controlling the degree of smoothing; the second
#'                  is for controlling the degree of the polynomials fitting to be used (normally 1 or 2); the third is for controlling the number of
#'                  contour lines.
#' @param font.size      a vector specifying the size of labels and text; the first element is for the main title, the second is for for x-axis and y-axis
#'                  labels; the third is for the subtitle; the fourth is for the text in the legend; the fifth is for the labels on contour lines.
#' @param title          a string specifying the main title.
#' @param subtitle       strings specifying the subtitle
#' @param effect           either "HR" or "RMST". only when outcome.type = "survival"
#' @param point.size        size of points for raw data points
#' @param filled            a logical indicator whether to show filled contour plot. If FALSE, contour lines are drawn
#' @param strip             the title for the strip showing treatment effect size
#' @param show.overall     logical. whether to show or not the overall treatment effect in the strip
#' @param palette          either "divergent" or "hcl"
#' @param col.power        to be used when palette = "hcl". see colorspace package for reference
#' @param show.points      a logical indicator specifying whether to show the raw data points
# created by Yi-Da Chiu, 01/08/17
# revised by Yi-Da Chiu, 18/08/17
#' @export
contourplt_new2 <- function(dat, covari.sel, trt.sel, resp.sel, outcome.type,
                            setup.ss, n.grid = c(41, 41),
                            brk.es = c(0, 1, 2, 3),
                            n.brk.axis =  7,
                            para.plot = c(0.35, 2, 20),
                            font.size = c(1.5, 1.2, 1, 0.85, 0.8),
                            title = NULL, subtitle = NULL,
                            effect = "HR", point.size = 1.2, filled = FALSE,
                            strip = NULL, show.overall = FALSE,
                            palette = "divergent", col.power = 0.5,
                            show.points = FALSE){


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
  # if (length(brk.es) > 5) stop("The vector specifying the numbers of break points for effect sizes should have five components only!")
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
      model.int = survival::coxph(survival::Surv(time, status) ~ trt, data = dat)
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
  x.raw = dat[covari.sel[1]]
  y.raw = dat[covari.sel[2]]
  all.dat = NULL
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
          model.int = survival::coxph(survival::Surv(time, status) ~ trt, data = dat[idx.covar2[[i]][[j]],])
          model.sum = summary(model.int)
          treatment.mean[k] = model.sum$coef[1, 1]
        }
      }
      all.dat = rbind(all.dat, cbind(dat[idx.covar2[[i]][[j]], covari.sel], treatment.mean[k]))
      x[k] = (cutpoint.covar1[[2]][i] + cutpoint.covar1[[1]][i])/2
      y[k] = (cutpoint.covar2[[i]][[2]][j] + cutpoint.covar2[[i]][[1]][j])/2
      ss.subgrp[k] = dim(dat[idx.covar2[[i]][[j]],] )[1]
    }
  }
  colnames(all.dat) = c("x", "y", "treatment.mean")
    cat("The number of subgroups over the first covariate is", n.subgrp.covar1, "\n")
    cat("The subgroup sample sizes over the first covariate are actually", ss.subgrp.covar1, "\n")
    cat("The number of further divided subgroups over the second covariate is", n.subgrp.covar2, "\n")

  ################################################ 2. produce a graph  #################################################################
  treatment.df = data.frame(x, y, treatment.mean)
  treatment.df.model = loess(treatment.mean ~ x*y, data = treatment.df,
                             span = para.plot[1], degree = para.plot[2])
  head(all.dat)
  treatment.df.model = loess(treatment.mean ~ x*y, data = all.dat,
                             span = para.plot[1], degree = para.plot[2])

  min.x = min(dat[,covari.sel[1]]);max.x = max(dat[,covari.sel[1]])
  min.y = min(dat[,covari.sel[2]]);max.y = max(dat[,covari.sel[2]])

  xy.fit.pt = expand.grid(list(x = seq(min.x, max.x, len = n.grid[1]),
                               y = seq(min.y, max.y, len = n.grid[2])))
  treatment.df.model.fit = predict(treatment.df.model, newdata = xy.fit.pt)

  x.range = seq(min.x, max.x, len = n.grid[1])
  y.range = seq(min.y, max.y, len = n.grid[2])

  if(!filled){ # Contour lines --------------
    graphics::layout(matrix(c(1,2), ncol = 1), heights = c(9,1))
    graphics::par(mar=c(4, 4, 3, 2) + 0.1)
    graphics::plot(x, y, #type = "n",
         xlim = range(x.range), ylim = range(y.range),
         xlab = lab.vars[1], ylab = lab.vars[2],
         main = title, #sub = subtitle,
         col  = "gray80",
         cex.main = font.size[1],
         cex.lab  = font.size[2],
         cex.axis = font.size[2],
         cex.sub  = font.size[3])
    graphics::mtext(subtitle)
    cutoff.es = rev(c(-Inf, brk.es, Inf))
    if (palette == "divergent"){
      pal.2 = colorRampPalette(c("#91bfdb", "#ffffbf", "#fc8d59"), space = "rgb")
      pal.YlRd = colorRampPalette(c("#fee090", "#d73027"),  space = "rgb")
      pal.WhBl = colorRampPalette(c("#e0f3f8", "#4575b4"),  space = "rgb")
      col.vec.div.pos = pal.WhBl((length(brk.es)+1)/2)
      col.vec.div.neg = pal.YlRd((length(brk.es)+1)/2)
      col.vec = c(rev(col.vec.div.neg), col.vec.div.pos)
      if (!(outcome.type == "survival" & effect == "HR")) col.vec = rev(col.vec)
      col.point = col.vec
    }
    if (palette == "continuous"){
      colors = c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2')
      pal.all = colorRampPalette(colors,  space = "rgb")
      col.vec = pal.all((length(brk.es)+1))
      if (!(outcome.type == "survival" & effect == "HR")) col.vec = rev(col.vec)
      col.point = col.vec
    }

    for (i in 1:(length(cutoff.es) - 1)){
      graphics::points(x[setdiff(which((treatment.mean > cutoff.es[i + 1])),  which((treatment.mean > cutoff.es[i]) ))],
             y[setdiff(which((treatment.mean > cutoff.es[i + 1])),  which((treatment.mean > cutoff.es[i]) ))],
             col = col.point[i], pch = 16, cex = point.size)
    }
    breaks = pretty(c(-3,3), length(col.vec))
    graphics::contour(x.range, y.range, treatment.df.model.fit,
            levels = breaks,
            vfont = c("sans serif", "plain"),
            labcex = font.size[5],
            col = "darkgreen",
            lty = "solid",
            add = TRUE)         # "len" in levels controls the number of levels

    lab0.es = paste("ES >", brk.es[length(brk.es)])
    lab1.es = vector()
    for (i in length(brk.es) : 2){
      lab.es.temp = paste(brk.es[i - 1], "< ES <", brk.es[i])
      lab1.es = c(lab1.es, lab.es.temp)
    }
    lab2.es =paste("ES <", brk.es[1])

    lab.es = c(lab0.es, lab1.es, lab2.es)
    graphics::par(mar=c(0,0,0,0))
    graphics::plot(0,0, xaxt = "n", yaxt = "n", type ="n", frame.plot = FALSE)
    graphics::legend("bottom",
           rev(lab.es),
           horiz = T,
           cex = font.size[4],
           col = rev(col.point),
           pch = 16,
           bg = "white")
  }
  if(filled){ # Filled contour plot --------------------------------------------
    if (palette == "divergent"){
      cols = c('#d53e4f','#f46d43','#fdae61','#fee08b','#e6f598','#abdda4','#66c2a5','#3288bd')
      pal.YlRd = colorRampPalette(c("#fee090", "#d73027"),  space = "rgb")
      pal.WhBl = colorRampPalette(c("#e0f3f8", "#4575b4"),  space = "rgb")
      col.vec.div.pos = pal.WhBl((length(brk.es)-1)/2)
      col.vec.div.neg = pal.YlRd((length(brk.es)-1)/2)
      col.vec = c(rev(col.vec.div.neg), col.vec.div.pos)
      if (!(outcome.type == "survival" & effect == "HR")) col.vec = rev(col.vec)
      cols = col.vec
    }
    if (palette == "continuous"){
      cols = c('#d53e4f','#f46d43','#fdae61','#fee08b','#e6f598','#abdda4','#66c2a5','#3288bd')
      pal.all = colorRampPalette(cols,  space = "rgb")
      col.vec = pal.all((length(brk.es)-1))
      if (!(outcome.type == "survival" & effect == "HR")) col.vec = rev(col.vec)
      cols = col.vec
    }
    if (palette == "hcl"){
      col.vec = rev(colorspace::diverge_hcl(n = length(brk.es)-1,
                                            # h = c(218, 0),
                                            c = 100, l = c(50,90),
                                            power = col.power))
      if (!(outcome.type == "survival" & effect == "HR")) col.vec = rev(col.vec)
      cols = col.vec
    }

    graphics::layout(matrix(c(1, 2), nrow=1, ncol=2), widths=c(4,1))
    if (is.null(title)){
      graphics::par(mar=c(4,4,2,1))
    } else{
      graphics::par(mar=c(4,4,4,1))
    }
    axis.sep = 0
    graphics::plot(x.range, y.range, type = "n",
         xlim = c(min.x-axis.sep, max.x+axis.sep),
         ylim = c(min.y-axis.sep, max.y+axis.sep),
         xlab = lab.vars[1], ylab = lab.vars[2],
         main = title,
         col  = "gray80",
         cex.main = font.size[1],
         cex.lab  = font.size[2],
         cex.axis = font.size[2],
         cex.sub  = font.size[3])
    graphics::mtext(subtitle)
    breaks = seq(min(brk.es),max(brk.es),      length.out = length(cols)+1)
    breaks.axis = seq(min(brk.es),max(brk.es), length.out = n.brk.axis)
    graphics::.filled.contour(x.range, y.range, treatment.df.model.fit,
                    levels = breaks,
                    col = rev(cols))
    if(show.points) graphics::points(dat[, covari.sel], cex = 0.5, lwd = 0.1)
    if (is.null(title)){
      par(mar=c(4,2,2,2.5))
    } else{
      par(mar=c(4,2,4,2.5))
    }
    image.scale(brk.es,
                col= rev(cols),
                breaks = breaks,
                axis.pos = 4, add.axis = FALSE)
    graphics::axis(2, at = breaks.axis, labels = round(breaks.axis, 3), las = 0, cex.axis = font.size[5])
    graphics::mtext(strip, side=4, line=1, cex.lab = font.size[5])
    if(show.overall){
      cat("Overall Treatment effect is:",
          overall.treatment.mean, ", with confidence interval: (",
          overall.treatment.lower,";",overall.treatment.upper,")\n")
      graphics::points(x = 0.5,
             (overall.treatment.mean), pch = 20)
      graphics::points(x = 0.5, overall.treatment.lower, pch = "-")
      graphics::points(x = 0.5, overall.treatment.upper, pch = "-")
      graphics::segments(x0 = 0.5, x1 = 0.5,
               y0 = overall.treatment.lower,
               y1 = overall.treatment.upper)
    }
    graphics::par(mfrow=c(1,1))
  }
  }
