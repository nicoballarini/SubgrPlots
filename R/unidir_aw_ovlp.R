#########################################################################################################################################################
#                                                                                                                                                       #
#                               a graphical display for showing relative proportions of pairwise subgroup overlap with two unidirectional               #
#                               arrowed curved lines around a circle                                                                                    #
#                                                                                                                                                       #
# unidir.aw.ovlp:  this function produces a plot for displaying relative proportions of pairwise subgroup overlap, where the relative proportion is     #
#                  defined by |intersect(A, B)|/|A|, for any sets A and B and A is the baseline set. The letters represent subgroups defined by the     #
#                  categories of the selected covariates. Subgroup letters located around a circle have two directional arrowed curved lines to the     #
#                  other. The arrowed lines have different colours, reflecting the level of overlap proportions. Note that if a subgroup is a baseline  #
#                  subgroup, then arrows on the curves would be located close to the corresponding letter. Also, the conditions of arrowed curves can be#
#                  changed by setting different values on the argument "para".                                                                          #
#                                                                                                                                                       #
#########################################################################################################################################################

# dat:          a data set
# covari.sel:   a vector of indices of covariates
# para:         a vector with three elements specifying the parameters of plot display; the first element is for adjusting the curvature of curves;
#               the second is for placing the relative position of arrowheads on the lines; the third is for specifying the adjustment of the arrowhead
#               (there are three possible values, 0, 0.5 and 1)
# font.size:    a vector specifying the size of labels and text; the first element is for the title; the second is for the covariates labels.
# title:        a string specifying the main title.
#
# e.g.          main.title = "Relative overlapping proportions of pairwise subgroups";
#               unidir.aw.ovlp(dat = dat, covari.sel = c(4, 6, 10), para = c(0.2, 0.2, 1), font.size = c(1.5, 1.5), title = main.title)
#
# created by Yi-Da Chiu, 01/08/17
# created by Yi-Da Chiu, 29/08/17
#' @export
unidir.aw.ovlp <- function(dat, covari.sel, para = c(0.2, 0.2, 1), font.size = c(1.5, 1.5), title = NULL)
{
  ################################################ 0. argument validity check  #################################################################

  if (missing(dat)) stop("Data have not been inputed!")
  if (!(is.data.frame(dat))) stop("The data set is not with a data frame!")

  if (missing(covari.sel)) stop("The variables for defining subgroups have not been specified!")
  if (!(is.numeric(covari.sel))) stop("The variables for defining subgroups are not numeric!")
  for (i in 1 : length(covari.sel)) if (!(is.factor(dat[,covari.sel[i]]))) stop("The variables for defining subgroups are not categorical!")

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
  layout(matrix(c(1,1, 1,1, 1, 1, 2, 2), byrow = TRUE, nrow=4, ncol=2), heights=c(4,1))

  library(diagram)

  angle.circles = seq(pi/2, pi/2 + (n.subgrp.tol-1)* (2*pi/n.subgrp.tol), 2*pi/n.subgrp.tol)
  x = 4*cos(angle.circles) + 5
  y = 4*sin(angle.circles) + 5
  x1 = 3.95*cos(angle.circles) + 5  # the x-position for adjusting the line
  y1 = 3.95*sin(angle.circles) + 5  # the y-position for adjusting the line
  pos = cbind(x,y)/10
  pos2 = cbind(x1,y1)/10

  r.prop.tol = c(0,1)
  pal.2=colorRampPalette(c("black", "red", "yellow"), space="rgb")
  breaks <- seq(min(r.prop.tol, na.rm = T), max(r.prop.tol, na.rm = T),length.out= 100)
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


  # creat plot

  par(mar=c(0,2,4,2))
  openplotmat(main= title, cex.main = font.size[1])

  n.subgrp.pair = sum(sapply(2, function(x) choose(n.subgrp.tol, x)))
  arrow.pos.adj = matrix(rep(0, n.subgrp.tol * 2), ncol = 2)
  for(i in 1:n.subgrp.tol) { arrow.pos.adj[i, ] = pos[i, ] - pos2[i, ] }

  st = vector()                          # generate the sequence of the starting position of the lines
  for (i in 1 : (n.subgrp.tol-1)) st = c(st, rep(i, (n.subgrp.tol-i)))
  se = vector()                          # generate the sequence of the ending position of the lines
  seq = 1:n.subgrp.tol; for (i in 1 : (n.subgrp.tol-1)) { se = c(se, seq[-(1:i)])}

  for (i in 1:n.subgrp.pair) {
    curvedarrow(from = pos[st[i], ] + arrow.pos.adj[st[i]], to = pos[se[i], ] + arrow.pos.adj[se[i]], curve = para[1], arr.pos =para[2],
                arr.adj =para[3], arr.type = "curved", arr.col = col.idx[i], lcol =col.idx[i] )
    curvedarrow(from = pos[se[i], ] + arrow.pos.adj[se[i]], to = pos[st[i], ] + arrow.pos.adj[st[i]], curve = para[1], arr.pos =para[2],
                arr.adj =para[3], arr.type = "curved", arr.col = col.idx.rev[i], lcol =col.idx.rev[i] )
  }

  circle = list()
  for (i in 1: n.subgrp.tol){
    j = i + 1
    circle[[j]] = circleSP(x[i]/10, y[i]/10, 0.1/20, 1000)
    plot(circle[[j]], add=TRUE, col= "white")
    #points(x1[i], y1[i], col = "blue", cex = 1.5)
    text.p = text.pos(x[i]/10, y[i]/10, 0.5/10, angle.circles[i])
    text(text.p[1], text.p[2],  labels= lab.subgrp[i], col = "black", cex = font.size[2])
  }

  box()

  # create an image scale bar for relative overlap proportions
  par(mar=c(3.8,4,1,4))
  image.scale(r.prop.tol, col=pal.2(length(breaks)-1), breaks=breaks-1e-8,axis.pos=1)
  box()

}
