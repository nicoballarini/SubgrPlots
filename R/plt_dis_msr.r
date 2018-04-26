##################################################################################################################################################
#                                                                                                                                                #
#                                            a plot for displaying dissimilarity distances of subgroups                                          #
#                                                                                                                                                #
#  plt.dis.msr:  this function produces a plot for displaying dissimilarity distances of pairwise subgroups, where dissmiliarity distance is     #
#                defined by 1 - |intersect(A, B)|/|A|, for any sets A and B and A is the baseline set. The horizontal axis represents dissimila- #
#                -rity distance. The letters represent subgroups defined by the categories of the selected covariates. The letter above the green#
#                triangle is the baseline for calculating dissimilarity distances with the others above the red cross. There are two modes of    #
#                display - one is that subgroup letters are located at the exact dissimilarity distance; the other is that letters are located   #
#                at the middle of the category of dissimilarity distances. Note that some dissimilarity distances are known (such as 0 and 1) and#
#                therefore they are not shown in the graphical display. Also, the range of dissimilarity distances can be adjusted.              #                                                  #
#                                                                                                                                                #
##################################################################################################################################################

# dat:          a data set
# covari.sel:   a vector of indices of covariates
# mode:         a value specifying the type of display; either 1 or 2.
# range.ds:     a vector specifying the range of the dissimilarity distance
# font.size:    a vector specifying the size of labels and text; the first element is for the title; the second is for the x-axis label; the third
#               is for the labels of baseline subgroups; the fourth is for the remaining subgroup labels (except for the baseline subgroup).
# title:        a string specifying the main titles.
# lab.x:        a string specifying the x-axis label.
#
# e.g.          main.title = "Dissimilarity measure of pairwise subgroups";
#               label.x = paste("Dissimilarity distance ");
#               plt.dis.msr(dat = dat, covari.sel = c(4,6, 10), mode = 2, range.ds = c(0,1), font.size = c(1.3, 0.9, 1, 0.7), title = main.title,
#               lab.x = label.x)
#
# created by Yi-Da Chiu, 01/08/17
# created by Yi-Da Chiu, 28/08/17
#' @export
plt.dis.msr <- function(dat, covari.sel, mode, range.ds = c(0,1), font.size = c(1, 0.9, 1, 0.7), title = NULL, lab.x = NULL)
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
  if (!(length(font.size) == 4)) stop("The font size set-ups of labels or text should have four compoents only!")

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


  ################################################ 2. create plots ########################################################################################

  ### produce a graph

  dev.new(width=10,height=10,noRStudioGD = TRUE)
  par(mar = c(4,2,2,2))

  x.lim.min = range.ds[1]
  x.lim.max = range.ds[2]

  plot(0, 0, type='n',  xlab = "", ylab = "", ylim = c(0, 11), xlim = c(x.lim.min, x.lim.max), xaxt="n", yaxt="n", bty = "n")
  axis(1, at = seq(0, 1, 0.2), labels = seq(0, 1, 0.2))
  title(main = title, xlab = lab.x, cex.lab = font.size[2], cex.main = font.size[1])

  y.origin = seq(10.8, 0.2, len = n.subgrp.tol )
  n.cat.var.acc = c(0, n.cat.var)

  if (mode == 1){
    k = 0
    for (i in 1: length(covari.sel)){
      for (j in 1 : length(cats.var[[i]])){
        k = k + 1

        lines(c(0, 1), c(y.origin[k], y.origin[k]), col = "royalblue")
        dd.range.cutoff = seq(0, 1, 0.2)
        points(dd.range.cutoff, rep(y.origin[k], length(dd.range.cutoff)), pch = "|", cex = 1, col = "royalblue")            # divide the line by the unit

        idx.rm = 1:n.cat.var[i] + sum(n.cat.var.acc[1:i])
        x.pos = 1- r.prop[k,-idx.rm]
        y.pos = rep(y.origin[k] - 0.1, length(r.prop[k,-idx.rm]))
        points(x.pos, y.pos, pch = 3, cex = 1, col = "red")                                                                  # add the notation for all the subgroups (except for the baseline and disjointed ones) under the line
        points(x.lim.min, y.pos[1], pch = 24, cex = 1.5, col = "blue", bg = "green")                                         # add the notation for the baseline subgroup
      }
    }

    dd.range.cutoff = seq(0, 1, 0.2)
    dd.range.grp.feq = matrix(rep(0, (n.subgrp.tol) * (length(dd.range.cutoff) - 1)), nrow = (n.subgrp.tol))
    dd.range.grp.idx = list()
    for (i in 1 : (n.subgrp.tol)){

      r.prop.rev = 1- r.prop[i,]
      idx.subgrp.w = intersect(which(r.prop.rev != 0), which(r.prop.rev != 1))                                               # the indices do not include subgroups which has dissimilarity distance of 0 or 1 with the baseline subgroup
      r.prop.adj = r.prop.rev[idx.subgrp.w]
      dd.range.grp.idx[[i]] = list()

      for (k in 1 : (length(dd.range.cutoff)-1)){

        cond1 = ( r.prop.adj >= dd.range.cutoff[k])
        cond2 = ( r.prop.adj <= dd.range.cutoff[k+1])
        dd.range.grp.feq[i, k] = sum(cond1 & cond2)
        dd.range.grp.idx[[i]][[k]] = which((cond1 & cond2) == T)

        dd.idx = sort.int(r.prop.adj[dd.range.grp.idx[[i]][[k]]], index.return=TRUE)$ix
        dd.idx.adj = dd.range.grp.idx[[i]][[k]][dd.idx]                                                                      # the order of subgrp labels, (in an increasing order)
        if (k == 1) dd.idx.adj = dd.idx.adj[-1]
        lab.temp = paste(lab.subgrp[idx.subgrp.w[dd.idx.adj]], collapse = "")
        text(1/2 * (dd.range.cutoff[k] + dd.range.cutoff[k + 1]),  rep(y.origin[i], length(dd.idx.adj)) + 0.05,
             labels = lab.temp, adj = c(0.5, 0), cex = font.size[4], col = "red")                                            # add the annotation of the subgroups except for
        # the baseline one.
      }
      text(x.lim.min, y.origin[i] + 0.05, labels = lab.subgrp[i], adj = c(0.5, 0), cex = font.size[3], col = "blue")         # add the annotation of the baseline subgroup
    }

  }else if (mode == 2){

    k = 0
    for (i in 1: length(covari.sel)){
      for (j in 1 : length(cats.var[[i]])){
        k = k + 1

        lines(c(0, 1), c(y.origin[k], y.origin[k]), col = "royalblue")
        dd.range.cutoff = seq(0, 1, 0.2)
        points(dd.range.cutoff, rep(y.origin[k], length(dd.range.cutoff)), pch = "|", cex = 1, col = "royalblue")            # divide the line by the unit

        idx.rm = 1:n.cat.var[i] + sum(n.cat.var.acc[1:i])
        x.pos = 1- r.prop[k,-idx.rm]
        y.pos = rep(y.origin[k] - 0.1, length(r.prop[k,-idx.rm]))
        text(x.pos, y.pos + 0.1, labels = lab.subgrp[-idx.rm], adj = c(0.5, 0), cex = font.size[4], col = "red")
        points(x.pos, y.pos, pch = 3, cex = 1, col = "red")                                                                  # add the notation for all the subgroups (except for the baseline and disjointed ones) under the line
        points(x.lim.min, y.pos[1], pch = 24, cex = 1.5, col = "blue", bg = "green")                                         # add the notation for the baseline subgroup
        text(x.lim.min, y.origin[k] + 0.05, labels = lab.subgrp[k], adj = c(0.5, 0), cex = font.size[3], col = "blue")       # add the annotation of the baseline subgroup
      }
    }
  }
}
