#' a graphical display for showing relative proportions of pairwise subgroup overlap with two
#' unidirectional arrowed lines around two circle
#'
#' this function produces a plot for displaying relative proportions of pairwise subgroup overlap, where the relative proportion
#' is defined by |intersect(A, B)|/|A|, for any sets A and B and A is the baseline set. The letters represent subgroups defined by
#' the categories of the selected covariates. Subgroup letters located around the two circles are considered as baselines; each has
#' unidirectional arrowed lines to the other. In addition, there are two modes of display - one is that arrowed lines have
#' different colours, reflecting the level of overlap proportions; the other is that lines have five thickness and types,
#' indicating five levels of overlap proportions.
#'
#' @param dat:          a data set
#' @param covari.sel:   a vector of indices of covariates
#' @param para:         a vector with three elements specifying the parameters of plot display; the first element is for specifying the length of the arrowhead;
#'  the second is for specifying the width of the arrowheads; the third is for specifying the adjustment of the arrowhead
#'  (there are three possible values, 0, 0.5 and 1)
#' @param mode:         a value specifying the type of display; either 1 or 2.
#' @param font.size:    a vector specifying the size of labels and text; the first element is for the title; the second is for the covariates labels.
#' @param title:        a string specifying the main title.
#
# e.g.          main.title = "Relative overlapping proportions of pairwise subgroups";
#               alt.unidir.aw.ovlp(dat = dat, covari.sel = c(4, 6, 10), mode = 1, para = c(0.5, 0.15, 1), font.size = c(1.5, 1.5), title = main.title)
#
# created by Yi-Da Chiu, 01/08/17
# created by Yi-Da Chiu, 29/08/17
#' @export
plot_overlap_alternative2 <- function(dat, covari.sel,
                                      para = c(0.5, 0.15, 1),
                                      mode, font.size, title = NULL)
{
  ################################################ 0. argument validity check  #################################################################

  if (missing(dat)) stop("Data have not been inputed!")
  if (!(is.data.frame(dat))) stop("The data set is not with a data frame!")

  if (missing(covari.sel)) stop("The variables for defining subgroups have not been specified!")
  if (!(is.numeric(covari.sel))) stop("The variables for defining subgroups are not numeric!")
  for (i in 1 : length(covari.sel)) if (!(is.factor(dat[,covari.sel[i]]))) stop("The variables for defining subgroups are not categorical!")

  if (missing(mode)) stop("The mode of display has not been specified!")
  if (!(mode %in% c(1, 2) )) stop("The type of display is unrecognisable!")

  if (!(is.numeric(para))) stop("The argument about the font sizes of the label and text is not numeric!")
  if (!(length(para) == 3)) stop("The set-up of the parameters for plot display should have three components only!")

  if (!(is.numeric(font.size))) stop("The argument about the font sizes of the label and text is not numeric!")
  if (!(length(font.size) == 2)) stop("The font size set-ups of labels or text should have two compoents only!")

  ################################################ 1. create subgroup overlap data  #################################################################

  lab.vars = names(dat)[covari.sel]                                               # the names of the covariates which is used for defining subgroups
  cats.var = list()                                                               # a list marking the categories of the selected covariates
  n.subgrp.tol = 0                                                                # the total number of subgroups
  for (i in 1 : length(covari.sel)){
    cats.var[[i]] = names(table(dat[,covari.sel[i]]))
    n.subgrp.tol = n.subgrp.tol + length(cats.var[[i]])
  }

  cond = list()
  data.subgrp = list()
  ss.subgrp = matrix(rep(0, n.subgrp.tol * n.subgrp.tol), nrow = n.subgrp.tol)    # a matrix storing subgroup sample sizes
  k = 0
  for (j in 1 : 2){
    for (i in 1 : length(covari.sel)){
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
  for (j in 1 : 2){
    for (i in 1: length(covari.sel)){
      k = k + 1
      # lab.subgrp[k] = paste(LETTERS[i], j, sep = "")
      lab.subgrp[k] = paste(lab.vars[i], "=", cats.var[[i]][j], sep = "")
    }
  }

  ################################################ 2. produce a graph  #################################################################

  ### linking lines between points on a circle

  layout(matrix(c(1, 1, 1, 1, 1, 1, 2, 2), byrow = TRUE, nrow=4, ncol=2), heights=c(4,1))

  library(shape)

  par(mar=c(0,2,4,2))
  plot(5,5, type='n', axes = FALSE, xlab = "", ylab = "", ylim = c(-10, 10), xlim = c(0,10))
  title(main= title, cex.main = font.size[1])
  box()

  angle.circles = seq(pi/2, pi/2 + (n.subgrp.tol-1)* (2*pi/n.subgrp.tol), 2*pi/n.subgrp.tol)
  x1 = 4*cos(angle.circles) + 5
  y1 = 4*sin(angle.circles) + 5
  x2 = 4*cos(angle.circles) + 5
  y2 = 4*sin(angle.circles) - 5

  st = vector()                                         # generate a sequence for the starting position of the lines
  for (i in 1 : (n.subgrp.tol-1)) {
    st = c(st, rep(i, (n.subgrp.tol-i)))
  }
  se = vector()                                         # generate a sequence for the ending position of the lines
  seq = 1:n.subgrp.tol
  for (i in 1 : (n.subgrp.tol-1)) {
    se = c(se, seq[-(1:i)])
  }


  if (mode == 1){
    r.prop.tol = c(0,1)
    pal.2=colorRampPalette(c("white", "yellow", "red"), space="rgb")
    breaks <- seq(min(r.prop.tol, na.rm = T), max(r.prop.tol, na.rm = T),length.out=100)
    levs=breaks

    col.vec = pal.2(length(breaks)-1)
    col.idx = vector()
    col.idx.rev = vector()
    ind = 0
    for (i in 1 : (n.subgrp.tol - 1)){
      for (j in (i+1) : n.subgrp.tol){
        ind = ind + 1
        col.idx1 = which(r.prop[i,j] < breaks)
        col.idx[ind] = col.vec[col.idx1[1] - 1]
        col.idx1 = which(r.prop[j,i] < breaks)
        col.idx.rev[ind] = col.vec[col.idx1[1] - 1]
      }
    }

    n.subgrp.pair = sum(sapply(2, function(x) choose(n.subgrp.tol, x)))
    for (i in 1:n.subgrp.pair){
      if (col.idx[i] == col.vec[1]) next()
      # Arrows(x1[st[i]], y1[st[i]], x1[se[i]], y1[se[i]],
      #        arr.length = para[1], arr.width = para[2], arr.adj = para[3],
      #        code = 2, arr.type = "curved",
      #        arr.col = col.idx[i], lcol = col.idx[i], lwd = 2)
      # Arrows(x2[se[i]], y2[se[i]], x2[st[i]], y2[st[i]],
      #        arr.length = para[1], arr.width = para[2], arr.adj = para[3],
      #        code = 2, arr.type = "curved",
      #        arr.col = col.idx.rev[i], lcol = col.idx.rev[i], lwd = 2)

      diagram::curvedarrow(from = c(x1[st[i]], y1[st[i]]),
                           to   = c(x1[se[i]], y1[se[i]]),
                           curve = para[1], arr.pos = para[2], arr.adj = para[3],
                           arr.type = "curved",
                           arr.col = col.idx[i], lcol =col.idx[i])
      diagram::curvedarrow(from = c(x2[se[i]], y2[se[i]]),
                           to   = c(x2[st[i]], y2[st[i]]),
                           curve = para[1], arr.pos = para[2], arr.adj = para[3],
                           arr.type = "curved",
                           arr.col = col.idx[i], lcol =col.idx[i])
    }

    circle1 = list()
    circle2 = list()
    for (i in 1: n.subgrp.tol){
      points(x1[i], y1[i])
      text.p = text.pos(x1[i], y1[i], 0.5, angle.circles[i])
      text(text.p[1], text.p[2],  labels= lab.subgrp[i], col = "black", cex = font.size[2])

      points(x2[i], y2[i])
      text.p = text.pos(x2[i], y2[i], 0.5, angle.circles[i])
      text(text.p[1], text.p[2],  labels= lab.subgrp[i], col = "black", cex = font.size[2])
    }

  }else if (mode == 2){
    r.prop.tol = c(0,1)
    breaks <- seq(min(r.prop.tol, na.rm = T), max(r.prop.tol, na.rm = T),length.out=6)

    lty.idx = vector()
    lty.idx.rev = vector()
    lwd.idx = vector()
    lwd.idx.rev = vector()
    ind = 0
    for (i in 1 : (n.subgrp.tol - 1)){
      for (j in (i+1) : n.subgrp.tol){
        ind = ind + 1

        if ((breaks[1] <= r.prop[i,j]) & (r.prop[i,j] < breaks[2])) {
          lty.idx[ind] = 2; lwd.idx[ind] = 1
        }else if ((breaks[2] <= r.prop[i,j]) & (r.prop[i,j] < breaks[3])) {
          lty.idx[ind] = 3; lwd.idx[ind] = 1
        }else if ((breaks[3] <= r.prop[i,j]) & (r.prop[i,j] < breaks[4])) {
          lty.idx[ind] = 1; lwd.idx[ind] = 1
        }else if ((breaks[4] <= r.prop[i,j]) & (r.prop[i,j] < breaks[5])) {
          lty.idx[ind] = 1; lwd.idx[ind] = 2
        }else if ((breaks[5] <= r.prop[i,j]) & (r.prop[i,j] < breaks[6])) {
          lty.idx[ind] = 1; lwd.idx[ind] = 3
        }

        if ((breaks[1] <= r.prop[j,i]) & (r.prop[j,i] < breaks[2])) {
          lty.idx.rev[ind] = 2; lwd.idx.rev[ind] = 1
        }else if ((breaks[2] <= r.prop[j,i]) & (r.prop[j,i] < breaks[3])) {
          lty.idx.rev[ind] = 3; lwd.idx.rev[ind] = 1
        }else if ((breaks[3] <= r.prop[j,i]) & (r.prop[j,i] < breaks[4])) {
          lty.idx.rev[ind] = 1; lwd.idx.rev[ind] = 1
        }else if ((breaks[4] <= r.prop[j,i]) & (r.prop[j,i] < breaks[5])) {
          lty.idx.rev[ind] = 1; lwd.idx.rev[ind] = 2
        }else if ((breaks[5] <= r.prop[j,i]) & (r.prop[j,i] < breaks[6])) {
          lty.idx.rev[ind] = 1; lwd.idx.rev[ind] = 3
        }
      }
    }

    n.subgrp.pair = sum(sapply(2, function(x) choose(n.subgrp.tol, x)))
    for (i in 1:n.subgrp.pair) {
      if (col.idx[i] == col.vec[1]) next()
      Arrows(x1[st[i]], y1[st[i]], x1[se[i]], y1[se[i]], arr.length = para[1], arr.width = para[2], arr.adj = para[3],
             code = 2, arr.type = "curved",  arr.col = "royalblue", lcol = "royalblue", lwd = lwd.idx[i], lty = lty.idx[i])
      Arrows(x2[se[i]], y2[se[i]], x2[st[i]], y2[st[i]], arr.length = para[1], arr.width = para[2], arr.adj = para[3],
             code = 2, arr.type = "curved",  arr.col = "royalblue", lcol = "royalblue", lwd = lwd.idx.rev[i], lty = lty.idx.rev[i])
    }

    for (i in 1: n.subgrp.tol){
      points(x1[i], y1[i])
      text.p = text.pos(x1[i], y1[i], 0.5, angle.circles[i])
      text(text.p[1], text.p[2],  labels= lab.subgrp[i], col = "black", cex = font.size[2])

      points(x2[i], y2[i])
      text.p = text.pos(x2[i], y2[i], 0.5, angle.circles[i])
      text(text.p[1], text.p[2],  labels= lab.subgrp[i], col = "black", cex = font.size[2])
    }
  }


  # create an image scale bar line labels for relative proportion

  par(mar=c(3.8,4,1,4))

  if (mode == 1){
    image.scale(r.prop.tol, col=pal.2(length(breaks)-1), breaks=breaks-1e-8,axis.pos=1)
    box()

  }else if (mode == 2){

    plot(5,5, type='n', axes = FALSE, xlab = "", ylab = "")
    lab.lines = c("0 <= p. < 0.2", "0.2 <= p. < 0.4", "0.4 <= p. < 0.6", "0.6 <= p. < 0.8", "0.8 <= p. <= 1" )
    legend("center", lab.lines, lty = c(2, 3, 1, 1, 1), lwd = c(1, 1, 1, 2, 3), col = "royalblue", ncol =2)
  }

}
