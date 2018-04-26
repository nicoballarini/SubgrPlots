#' matrix plots for relative overlap proportions of pairwise subgroups
#'
#' this function produces a matrix plot for displaying relative proportions of pairwise subgroup overlap, where the relative proportion
#' is defined by |intersect(A, B)|/|A|, for any sets A and B and A is the baseline set. The letters over (and beside) the matrix
#' represent subgroups defined by the categories of the selected covariates. The subgroup letters on the rows are regarded as baseline
#' subgroups for calculating  relative overlap proportions. There are two modes of display - one with circles which are
#' placed in the cells, where the size and the colour reflect how large the proportion is; the other with colours filled in the
#' cells, where the colour indicate the level of overlap proportions.
#'
#'
#'
#' @param dat:          a data set
#' @param covari.sel:   a vector of indices of covariates
#' @param mode:         a value specifying the type of display; either 1 or 2,
#' @param font.size:    a vector specifying the size of labels and text; the first element is for the title; the second is for the covariates labels.
#' @param title:        a string specifying the main title.
#
# e.g.          main.title = "Relative overlapping proportions of pairwise subgroups";
#               mat.subgrp.op(dat, c(4,6, 10), mode = 1, font.size = c(2, 1), title = main.title)
#
# created by Yi-Da Chiu, 01/08/17
# created by Yi-Da Chiu, 29/08/17
#' @export
plot_matrix_overlap <- function(dat, covari.sel, mode, font.size = c(2, 1), title = NULL, new = TRUE)
{

  ################################################ 0. argument validity check  #################################################################

  if (missing(dat)) stop("Data have not been inputed!")
  if (!(is.data.frame(dat))) stop("The data set is not with a data frame!")

  if (missing(covari.sel)) stop("The variables for defining subgroups have not been specified!")
  if (!(is.numeric(covari.sel))) stop("The variables for defining subgroups are not numeric!")
  for (i in 1 : length(covari.sel)) if (!(is.factor(dat[,covari.sel[i]]))) stop("The variables for defining subgroups are not categorical!")

  if (missing(mode)) stop("The mode of display has not been specified!")
  if (!(mode %in% c(1, 2) )) stop("The type of display is unrecognisable!")

  if (!(is.numeric(font.size))) stop("The argument about the font sizes of the label and text is not numeric!")
  if (!(length(font.size) == 2)) stop("The set-up of the font size for labels or text should have two elements only!")

  ################################################ 1. create subgroup overlap data  #################################################################


  lab.vars = names(dat[,covari.sel])                                               # the names of the covariates which is used for defining subgroups
  cats.var = list()                                                                # a list marking the categories of the selected covariates
  n.subgrp.tol = 0                                                                 # the total number of subgroups
  for (i in 1 : length(covari.sel)){
    cats.var[[i]] = names(table(dat[,covari.sel[i]]))
    n.subgrp.tol = n.subgrp.tol + length(cats.var[[i]])
  }

  cond = list()
  data.subgrp = list()
  ss.subgrp = matrix(rep(0, n.subgrp.tol * n.subgrp.tol), nrow = n.subgrp.tol)     # a matrix storing subgroup sample sizes
  k = 0
  for (j in 1 : 2){ ##length(cats.var[[i]])
    for (i in 1 : length(covari.sel)) {
      k = k + 1
      cond[[k]] = which((dat[, covari.sel[i]] == cats.var[[i]][j])  == T )
      ss.subgrp[k, k] = length(cond[[k]])
      data.subgrp[[k]] = dat[cond[[k]], ]
    }
  }

  k = n.subgrp.tol
  r.prop = diag(n.subgrp.tol)                                                      # a matrix storing relative overlap proportions of pairwise subgroups
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

  if (new) layout(matrix(c(1,1, 1,1, 1, 1, 2, 2), byrow = TRUE, nrow=4, ncol=2), heights=c(5.5,1))
  par(mar=c(0,2,2,2))

  plot(0, 0, type='n', ylim = c(0, 11), xlim = c(0, 11), axes = FALSE, main= title, cex.main = font.size[1])

  r.prop.tol = c(0,1)
  pal.2 = colorRampPalette(c("white", "yellow", "red"), space="rgb")
  breaks <- seq(min(r.prop.tol, na.rm = T), max(r.prop.tol, na.rm = T),length.out= 100)
  rect(0.5, 0.5, 10.5, 10.5, col = "white",  border = "black"  )
  cell.width = seq(10.5, 0.5, len = n.subgrp.tol + 1)[1] - seq(10.5, 0.5, len = n.subgrp.tol + 1)[2]
  cell.height = cell.width

  par(xpd=TRUE)
  text(rep(0.25, n.subgrp.tol),
       seq(10.5 - cell.height/2, 0.5 + cell.height/2, len = n.subgrp.tol),
       labels= lab.subgrp, cex = font.size[2], adj = 1)      # place subgroup labels vertically
  text(seq(0.5 + cell.width/2, 10.5 - cell.width/2, len = n.subgrp.tol),
       rep(10.75, n.subgrp.tol),
       labels= lab.subgrp, cex = font.size[2])      # place subgroup labels horizontally
  par(xpd=FALSE)
  if (mode == 1){

    col.vec = pal.2(length(breaks)-1)
    col.idx = vector()
    col.idx.rev = vector()
    ind = 0
    for (i in 1 : (n.subgrp.tol - 1)){
      for (j in (i+1) : (n.subgrp.tol)){
        ind = ind + 1
        col.idx1 = which(r.prop[i,j] < breaks)
        col.idx[ind] = col.vec[col.idx1[1] - 1]
        if (r.prop[i,j] == 0) {col.idx[ind] = NA}
        col.idx1 = which(r.prop[j,i] < breaks)
        col.idx.rev[ind] = col.vec[col.idx1[1] - 1]
        if (r.prop[i,j] == 0) {col.idx.rev[ind] = NA}
      }
    }

    radius.circle = cell.width/2
    ind = 0
    for (i in 1 : (n.subgrp.tol - 1)){
      for (j in (i + 1) : (n.subgrp.tol)){
        ind = ind + 1
        xl = 0.5 + (j-1) * cell.width
        yb = 10.5 - i * cell.height
        xr = 0.5 + j * cell.width
        yt = 10.5 - (i-1) * cell.height

        theta = seq(0, 2 * pi, len = 10000)
        xc = 1/2 * (xl + xr)
        yc = 1/2 * (yb + yt)
        r = radius.circle * r.prop[i,j]
        x = xc + r * cos(theta)
        y = yc + r * sin(theta)
        polygon(x, y, border = NA, col = col.idx[ind])
        rect(xl, yb, xr, yt,  border = "black"  )

        xl = 0.5 + (i-1) * cell.width
        yb = 10.5 - j * cell.height
        xr = 0.5 + i * cell.width
        yt = 10.5 - (j - 1) * cell.height

        theta = seq(0, 2 * pi, len = 10000)
        xc = 1/2 * (xl + xr)
        yc = 1/2 * (yb + yt)
        r = radius.circle * r.prop[j,i] #* 0.9
        x = xc + r * cos(theta)
        y = yc + r * sin(theta)
        polygon(x, y, border = NA, col =col.idx.rev[ind])
        rect(xl, yb, xr, yt,   border = "black" )
      }
    }

    for (i in 1 :(n.subgrp.tol)){
      xl = 0.5 + (i-1) * cell.width
      yb = 10.5 - i * cell.height
      xr = 0.5 + i * cell.width
      yt = 10.5 - (i-1) * cell.height

      theta = seq(0, 2 * pi, len = 10000)
      xc = 1/2 * (xl + xr)
      yc = 1/2 * (yb + yt)
      r = radius.circle #* 0.9
      x = xc + r * cos(theta)
      y = yc + r * sin(theta)
      polygon(x, y, border = NA, col = "red")
      rect(xl, yb, xr, yt,   border = "black")
    }

  } else if (mode == 2){

    col.vec = pal.2(length(breaks)-1)
    col.idx = vector()
    col.idx.rev = vector()
    ind = 0
    for (i in 1 : (n.subgrp.tol - 1)){
      for (j in (i+1) : (n.subgrp.tol)){ind = ind + 1
      col.idx1 = which(r.prop[i,j] < breaks)
      col.idx[ind] = col.vec[col.idx1[1] - 1]
      col.idx1 = which(r.prop[j,i] < breaks)
      col.idx.rev[ind] = col.vec[col.idx1[1] - 1]
      }
    }

    ind = 0
    for (i in 1 : (n.subgrp.tol - 1)){
      for (j in (i + 1) : (n.subgrp.tol)){
        ind = ind + 1
        xl = 0.5 + (j-1) * cell.width
        yb = 10.5 - i * cell.height
        xr = 0.5 + j * cell.width
        yt = 10.5 - (i-1) * cell.height

        rect(xl, yb, xr, yt,  border = "black", col=col.idx[ind])

        xl = 0.5 + (i-1) * cell.width
        yb = 10.5 - j * cell.height
        xr = 0.5 + i * cell.width
        yt = 10.5 - (j - 1) * cell.height

        rect(xl, yb, xr, yt,   border = "black", col=col.idx.rev[ind])
      }
    }

    for (i in 1 :(n.subgrp.tol)){
      xl = 0.5 + (i-1) * cell.width
      yb = 10.5 - i * cell.height
      xr = 0.5 + i * cell.width
      yt = 10.5 - (i-1) * cell.height

      rect(xl, yb, xr, yt,   border = "black", col="red")
    }
  }

  # create an image scale bar for relative overlap proportions
  par(mar=c(3.8,5,0,5))
  image.scale(r.prop.tol, col=pal.2(length(breaks)-1), breaks=breaks-1e-8,axis.pos=1)
  box()

}
