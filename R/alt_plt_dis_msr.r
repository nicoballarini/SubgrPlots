##################################################################################################################################################
#                                                                                                                                                #
#                                         an alternative plot for displaying dissimilarity distances of subgroups                                #
#                                                                                                                                                #
# alt.plt.dis.msr:  this function produces a plot for displaying dissimilarity distances of pairwise subgroups, where dissmiliarity distance is  #
#                   defined by 1 - |intersect(A, B)|/|A|, for any sets A and B and A is the baseline set. The letters represent subgroups defined#
#                   by the categories of the selected covariates.The vertical axis represents dissimilarity distance. Each column has a letter   #
#                   standing for a subgroup as the baseline for calculating dissimilarity distances. There are two modes of display - one is that#
#                   subgroup letters are located at the exact dissimilarity distance with different types and colours of points; the other is    #
#                   that lines connected to all the points.  The colour of lines or points indicates whether subgroups are from the same         #
#                   covariate or not. The number of types shows how many categories (from the same covariate) are considered as subgroups. The   #
#                   range of dissimilarity distances can be adjusted.                                                                            #                                                            #
#                                                                                                                                                #
##################################################################################################################################################

# dat:          a data set
# covari.sel:   a vector of indices of covariates
# mode:         a value specifying the type of display; either 1 or 2.
# range.ds:     a vector specifying the range of the dissimilarity distance
# font.size:    a vector specifying the size of labels and text; the first element is for the title; the second is for the x-axis label; the third
#               is for the labels of baseline subgroups; the fourth is for the remaining subgroup labels (except for the baseline subgroup).
# title:        a string specifying the main titles.
# lab.y:        a string specifying the y-axis label.
#
# e.g.          main.title = "Dissimilarity measure of pairwise subgroups";
#               label.y = paste("Dissimilarity distance ");
#               alt.plt.dis.msr(dat = dat, covari.sel = c(4,6,10), mode = 2, range.ds = c(0.3,1), font.size = c(1.3, 1, 1), title = main.title,
#               lab.y = label.y)
#
# created by Yi-Da Chiu, 01/08/17
# created by Yi-Da Chiu, 28/08/17
#' @export
alt.plt.dis.msr <- function(dat, covari.sel, mode, range.ds = c(0,1), font.size = c(1.3, 1, 1), title = NULL, lab.y = NULL)
{
  ################################################ 0. argument validity check  #################################################################

  if (missing(dat)) stop("Data have not been inputed!")
  if (!(is.data.frame(dat))) stop("The data set is not with a data frame!")

  if (missing(covari.sel)) stop("The variables for defining subgroups have not been specified!")
  if (!(is.numeric(covari.sel))) stop("The variables for defining subgroups are not numeric!")
  for (i in 1 : length(covari.sel)) if (!(is.factor(dat[,covari.sel[i]]))) stop("The variables for defining subgroups are not categorical!")

  if (missing(mode)) stop("The mode of display has not been specified!")
  if (!(mode %in% c(1, 2) )) stop("The type of display is unrecognisable!")

  if (!(is.numeric(range.ds))) stop("The range of dissimilarity distance not numeric!")
  if (!(length(range.ds) == 2)) stop("The range of dissimilarity distance should have four compoents specifying the minimum and maximum!")
  if ((sum(range.ds < 0) != 0) || (sum(range.ds > 1) != 0)) stop("The range should be only allowed within the interval [0, 1]!")

  if (!(is.numeric(font.size))) stop("The argument about the font sizes of the label and text is not numeric!")
  if (!(length(font.size) == 3)) stop("The font size set-ups of labels or text should have three compoents only!")

  ################################################ 1. create subgroup overlap data  #################################################################


  n.covari = length(covari.sel)                                                   # the number of the covariates which is used for defining subgroups
  lab.vars = names(dat)[covari.sel]                                               # the names of the covariates which is used for defining subgroups
  cats.var = list()                                                               # a list marking the categories of the selected covariates
  n.cat.var = vector()                                                            # a vector marking the category numbers of the selected covariates
  n.subgrp.tol = 0                                                                # the total number of subgroups
  for (i in 1 : n.covari){
    cats.var[[i]] = names(table(dat[,covari.sel[i]]))
    n.cat.var[i] = length(cats.var[[i]])
    n.subgrp.tol = n.subgrp.tol + length(cats.var[[i]])
  }

  cond = list()
  data.subgrp = list()
  ss.subgrp = matrix(rep(0, n.subgrp.tol * n.subgrp.tol), nrow = n.subgrp.tol)    # a matrix storing subgroup sample sizes
  k = 0
  for (i in 1 : length(covari.sel)) {
    for (j in 1 : length(cats.var[[i]])){
      k = k + 1
      cond[[k]] = which((dat[, covari.sel[i]] == cats.var[[i]][j])  == T )
      ss.subgrp[k, k] = length(cond[[k]])
      data.subgrp[[k]] = dat[cond[[k]], ]
    }
  }

  k = n.subgrp.tol
  r.prop = diag(n.subgrp.tol)                                                     # a matrix storing relative overlap proportions of pairwise subgroups
  for (i in 1 : (n.subgrp.tol - 1) ){
    for (j in (i + 1) : (n.subgrp.tol) ){
      k = k + 1
      cond[[k]] = intersect(cond[[i]], cond[[j]])
      ss.subgrp[i, j] = length(cond[[k]])
      ss.subgrp[j, i] = length(cond[[k]])

      r.prop[i, j] = ss.subgrp[i, j] / ss.subgrp[i, i]
      r.prop[j, i] = ss.subgrp[j, i] / ss.subgrp[j, j]
    }
  }

  lab.subgrp = vector()
  k = 0
  for (i in 1: length(covari.sel)){
    for (j in 1 : length(cats.var[[i]])){
      k = k + 1
      lab.subgrp[k] = paste(LETTERS[i], j, sep = "")
    }
  }

  ################################################ 2. produce a graph  #################################################################


  dev.new(width=10,height=10,noRStudioGD = TRUE)

  layout(matrix(c(1,2), byrow = TRUE, nrow=1, ncol=2), widths=c(6,1), heights=c(1,1))

  ## create the main figure

  par(mar = c(3,4,2,1))

  y.lim.min = range.ds[1]
  y.lim.max = range.ds[2]

  plot(0, 0, type='n',  xlab = "", ylab = "", ylim = c(y.lim.min, y.lim.max), xlim = c(1, n.subgrp.tol), xaxt="n", yaxt="n")
  axis(1, at = seq(1, n.subgrp.tol, 1), labels = c(lab.subgrp[1:(n.subgrp.tol)]))
  axis(2, at = seq(y.lim.min, y.lim.max, len = 11), labels = round(seq(y.lim.min, y.lim.max, len = 11), 2))
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =  "gray88")
  title(main = title, ylab = lab.y, cex.lab = font.size[2], cex.main = font.size[1])

  for (i in 0 : 10){
    abline(h = y.lim.min + i * (y.lim.max - y.lim.min)/ (11 -1), col = "gray99",lty = 1)
  }

  pch.lab = seq(0, max(n.cat.var), 1) + 2
  lty.lab = rep(3,  length(seq(0, max(n.cat.var), 1)))
  col.lab = colorRampPalette(c("red", "blue", "green", "yellow", "brown"))(n.covari)
  n.cat.var.acc = c(0, n.cat.var)
  k = 0
  for (i in 1: length(covari.sel)){
    for (j in 1 : length(cats.var[[i]])){
      k = k + 1
      idx.rm = 1:n.cat.var[i] + sum(n.cat.var.acc[1:i])
      x.pos = setdiff(seq(1, n.subgrp.tol, 1), idx.rm)
      y.pos = 1- r.prop[k,-idx.rm]

      if (mode == 1) {
        lines(x.pos, y.pos, type="o", lty = lty.lab[j], col = col.lab[i], lwd = 2, pch = pch.lab[j] )
      }else if (mode == 2){
        points(x.pos, y.pos, col = col.lab[i], pch = pch.lab[j] )}
    }
  }

  ## add the legend
  par(mar = c(2,0.2,2,2))
  col.lab2 <- pch.lab2 <- lty.lab2 <- vector()
  plot(0, 0, type='n',  xlab = "", ylab = "", ylim = c(0, 1), xlim = c(1, n.subgrp.tol), xaxt="n", yaxt="n", bty = "n")
  k = 0
  for (i in 1: length(covari.sel)){
    for (j in 1 : length(cats.var[[i]])){
      k = k + 1

      if (mode == 1) {
        lines(rep(-1, n.subgrp.tol), rep(-1, n.subgrp.tol), type="o", lty = lty.lab[j], col = col.lab[i], lwd = 2, pch = pch.lab[j] )
      }else if (mode == 2){
        points(rep(-1, n.subgrp.tol), rep(-1, n.subgrp.tol), col = col.lab[i], pch = pch.lab[j] )}

      col.lab2[k] = col.lab[i]
      pch.lab2[k] = pch.lab[j]
      lty.lab2[k] = lty.lab[j]
    }
  }

  if (mode == 1){
    legend("top", lab.subgrp[1:(n.subgrp.tol)], cex = font.size[3], col= col.lab2,  pch = pch.lab2, lty= lty.lab2,
           lwd = rep(2, n.subgrp.tol), horiz = FALSE)
  }else if (mode == 2){
    legend("top", lab.subgrp[1:(n.subgrp.tol)], cex = font.size[3], col= col.lab2,  pch = pch.lab2, horiz = FALSE)
  }


}
