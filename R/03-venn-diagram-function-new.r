#' Venn diagram for subgroup effect size
#'
#' This function produces a Venn diagram showing the treatment effect size of subgroups defined by sets from the categories of covariates.
#' Also, it prints out the minimum and maximum of the treatment effect size on the console so as to set an approapriate range for effect
#' size on the colour strip . Note that there are two options of graphical display; whether show the subgroup effect size of the complement
#' of the union of all the considered subgroups or not. In addition, this function only works up to 5 sets and does not run an area-proportional
#' algorithms for displaying two or three set. In addition, the function uses log odd ratio and log hazard ratio for displaying
#' subgroup effect sizes in binary and survival data, respectively.
#'
#'@param dat:          a data set
#'@param covari.sel:   a vector of indices of covariates
#'@param cat.sel:      a vector of indices of the categories for each covariate
#'@param trt.sel:      a covariate index specifying the treatment code
#'@param resp.sel:     a covariate index specifying the response variable
#'@param outcome.type: a string specifying the type of the response variable, it can be "continuous", or "binary" or  "survival".
#'@param range.strip:  a vector with two elements specifying the range of treatment effect size for display
#'@param n.brk:        a number specifying the number of the points dividing the range of the argument "range.strip".
#'@param font.size:    a vector specifying the size of labels and text; the first element is for the main title; the second is for the category labels;
#'               the third is for the sample size labels; the fourth is for the legend text; the fifth is for the y-axis label of the colour strip;
#'               the sixth is for the unit label on the y axis.
#'@param title:        a string specifying the main title.
#'@param strip:        a string specifying the title of the colour strip.
#
# eg.1          main.title = paste("Treatment effect sizes across subgroups (N = 1000)", sep = "");
#               strip.title = paste("Treatment effect size");
#               vd(dat = dat, covari.sel = c(4, 5, 10), cat.sel = c(1, 2, 2), trt.sel = 2, resp.sel = 1, outcome.type = "continuous",
#               title = main.title, strip = strip.title)
#
# eg.2          main.title = paste("Treatment effect sizes across subgroups (N = 2985)", sep = "");
#               strip.title = paste("Treatment effect size (log odd ratio)");
#               vd(dat = dat2, covari.sel = c(2, 2, 3), cat.sel = c(1, 2, 1), trt.sel = 4, resp.sel = 5, outcome.type = "binary",
#               title = main.title, strip = strip.title)
#
# eg.3          main.title = paste("Treatment effect sizes across subgroups (N = 686)", sep = "");
#               strip.title = paste("Treatment effect size (log hazard ratio)");
#               vd(dat = dat3, covari.sel = c(6, 6, 7), cat.sel = c(1, 2, 1), trt.sel = 1, resp.sel = c(4,3), outcome.type = "survival",
#               title = main.title, strip = strip.title)
#
# created by Yi-Da Chiu, 01/08/17
# revised by Yi-Da Chiu, 30/08/17
#' @export
plot_venn <- function(dat, covari.sel, cat.sel, trt.sel, resp.sel, outcome.type, outside.area=FALSE, range.strip=c(-6, 6), n.brk=13,
               font.size = c(1, 1.5, 1, 0.9, 1, 1), title = NULL, strip = NULL,
               cat.dist = rep(0.04, 3)){

  ################################################ 0. argument validity check  #################################################################

  if (missing(dat)) stop("Data have not been inputed!")
  if (!(is.data.frame(dat))) stop("The data set is not with a data frame!")

  if (missing(covari.sel)) stop("The variables for defining subgroups have not been specified!")
  if (!(is.numeric(covari.sel))) stop("The variables for defining subgroups are not numeric!")
  for (i in 1 : length(covari.sel)) if (!(is.factor(dat[,covari.sel[i]]))) stop("The variables for defining subgroups are not categorical!")
  if ((length(covari.sel) > 5) || (length(covari.sel) < 1)){
    stop("The length of covari.sel (corresponding to the number of sets) should be at least 1 or at most 5!")
  }

  if (missing(cat.sel) || length(cat.sel) != length(covari.sel)){
    stop("The categories that define subgroups have not been specified or its length does not correspond to the length of the argument covari.sel!")
  }
  if (!(is.numeric(cat.sel))) stop("The category indices are not numeric!")

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

  if (!(is.numeric(range.strip))) stop("The argument about the range for displaying effect sizes is not a numeric vector!")
  if (length(range.strip) > 2) stop("The range for displaying effect sizes should be specified by two values!")
  if (range.strip[1] > range.strip[2]) stop("The range for displaying effect sizes is not specified correctly (the former compoent should be
                                            smaller than the latter)!")

  if (!(is.numeric(n.brk))) stop("The argument about the break points in the range for displaying effect sizes is not numeric!")
  if (length(n.brk) > 1) stop("The number of the break points in the range for displaying effect sizes should be greater than 1!")

  if (!(is.numeric(font.size))) stop("The argument about the font sizes of the label and text is not numeric!")
  if (!(length(font.size) == 6)) stop("The font size setups for labels or text should have six components only!")

  ################################################ 1. create subgroup data  #################################################################

  grid::grid.newpage()

  library(sp)
  # library(rgeos)
  library(survival)

  lab.vars = names(dat)[covari.sel]              # set the names of the covariates which relates to the defined subgroup; if a covariate
                                                 # are considered for multiple times, we make their name identical. (otherwise, the resulsting
                                                 # names are like var.1, var.2 and so on.)

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

  n.subgrp = length(cat.sel)
  n.subgrp.tol = sum(sapply(0:n.subgrp, function(x) choose(n.subgrp, x)))
  cats.var.all = list()
  for (i in 1 : length(covari.sel)){
    cats.var.all[[i]] = names(table(dat[, covari.sel[i]]))
  }

  if (n.subgrp == 1){
    A = dat[, covari.sel[1]] == cats.var.all[[1]][cat.sel[1]]

    cond = list()
    cond[[1]] = which( A  == T  );  n.1 = length(which( A == T  ))
    cond[[2]] = which( A  != T  );  n.compl = length(which( A != T  ))
    cat("Only one subgroup defining variable. Try with two or more.")

    VennDiagram::draw.single.venn(area = sum(A),
                                  category = names(dat)[covari.sel],
                                  lty = 1,
                                  fill = "white", title = title) -> vp
    grid::grid.draw(grid::gList(vp,
                                grid::textGrob(x=0.1, y=0.1 ,n.compl, hjust = 0, just = 0)))
  }else if (n.subgrp == 2){

    A = dat[, covari.sel[1]] == cats.var.all[[1]][cat.sel[1]]
    B = dat[, covari.sel[2]] == cats.var.all[[2]][cat.sel[2]]

    cond = list()
    cond[[1]] = which( A & !B == T  );  n.1 = length(which( A & !B == T  ))
    cond[[2]] = which( !A & B == T  );  n.2 = length(which( !A & B == T  ))
    cond[[3]] = which(A & B == T  );    n.12 = length(which(A & B == T  ))
    cond[[4]] = which(!A & !B == T  );  n.compl = length(which(!A & !B == T  ))

    VennDiagram::draw.pairwise.venn(area1 = sum(A),
                                  area2 = sum(B),
                                  n12 = sum(A&B),
                                  category = names(dat)[covari.sel],
                                  lty = 1,
                                  fill = "white", title = title) -> vp
    grid::grid.draw(grid::gList(vp,
                                grid::textGrob(x=0.1, y=0.1 ,n.compl, hjust = 0, just = 0)))
  }else if (n.subgrp == 3){

    A = dat[, covari.sel[1]] == cats.var.all[[1]][cat.sel[1]]
    B = dat[, covari.sel[2]] == cats.var.all[[2]][cat.sel[2]]
    C = dat[, covari.sel[3]] == cats.var.all[[3]][cat.sel[3]]

    cond = list()
    cond[[1]] = which( A & !B & !C == T  );  n.1 = length(which( A & !B & !C == T  ))
    cond[[2]] = which( !A & B & !C == T  );  n.2 = length(which( !A & B & !C == T  ))
    cond[[3]] = which( !A & !B & C == T  );  n.3 = length(which( !A & !B & C == T  ))
    cond[[4]] = which( A & B & !C == T  );   n.12 = length(which( A & B & !C == T  ))
    cond[[5]] = which( A & !B & C == T  );   n.13 = length(which( A & !B & C == T  ))
    cond[[6]] = which( !A & B & C == T  );   n.23 = length(which(!A & B & C == T  ))
    cond[[7]] = which( A & B & C == T  );    n.123 = length(which(A & B & C == T  ))
    cond[[8]] = which( !A & !B & !C == T  ); n.compl = length(which(!A & !B & !C == T  ))
    # test
    # print(n.1 + n.2 + n.3 + n.12 + n.13 + n.23 + n.123 + n.compl)
    # print(nrow(dat))
    # grid::grid.newpage()
    VennDiagram::draw.triple.venn(area1 = sum(A),
                                  area2 = sum(B),
                                  area3 = sum(C),
                                  n12 = sum(A&B),
                                  n23 = sum(B&C),
                                  n13 = sum(A&C),
                                  n123 = sum(A&B&C),
                                  rotation.degree = 60,
                                  category = names(dat)[covari.sel],
                                  cat.pos  = c(225, 0, 140),
                                  cat.dist = cat.dist,
                                  lty = 1, cex = 1, cat.cex = 1,
                                  fontfamily = rep("sans", 7),
                                  cat.fontfamily = rep("sans", 3),ind = FALSE,
                                  fill = "white", title = title) -> vp

    grid::pushViewport(grid::viewport(width = 0.8, height = 0.8))
    grid::grid.draw(grid::gList(vp,
                                grid::textGrob(x=0.9, y=0.9 ,n.compl, hjust = 0, just = 0)))
    grid::upViewport()
    grid::grid.rect(width = 0.97, height = 0.97, gp = grid::gpar(fill=NA))
  }else if(n.subgrp == 4){
    A = dat[, covari.sel[1]] == cats.var.all[[1]][cat.sel[1]]
    B = dat[, covari.sel[2]] == cats.var.all[[2]][cat.sel[2]]
    C = dat[, covari.sel[3]] == cats.var.all[[3]][cat.sel[3]]
    D = dat[, covari.sel[4]] == cats.var.all[[4]][cat.sel[4]]

    cond = list()
    cond[[1]] = which( A & !B & !C & !D == T  );   n.1 = length(which( A & !B & !C & !D == T  ))
    cond[[2]] = which( !A & B & !C & !D == T  );   n.2 = length(which( !A & B & !C & !D == T  ))
    cond[[3]] = which( !A & !B & C & !D == T  );   n.3 = length(which( !A & !B & C & !D == T  ))
    cond[[4]] = which( !A & !B & !C & D == T  );   n.4 = length(which( !A & !B & !C & D == T  ))
    cond[[5]] = which( A & B & !C & !D == T  );    n.12 = length(which( A & B & !C & !D == T  ))
    cond[[6]] = which( A & !B & C & !D == T  );    n.13 = length(which( A & !B & C & !D == T  ))
    cond[[7]] = which( A & !B & !C & D == T  );    n.14 = length(which( A & !B & !C & D == T  ))
    cond[[8]] = which( !A & B & C & !D == T  );    n.23 = length(which(!A & B & C & !D == T  ))
    cond[[9]] = which( !A & B & !C & D == T  );    n.24 = length(which(!A & B & !C & D == T  ))
    cond[[10]] = which( !A & !B & C & D == T  );   n.34 = length(which(!A & !B & C & D == T  ))
    cond[[11]] = which( A & B & C & !D == T  );    n.123 = length(which(A & B & C & !D == T  ))
    cond[[12]] = which( A & B & !C & D == T  );    n.124 = length(which(A & B & !C & D == T  ))
    cond[[13]] = which( A & !B & C & D == T  );    n.134 = length(which(A & !B & C & D == T  ))
    cond[[14]] = which( !A & B & C & D == T  );    n.234 = length(which(!A & B & C & D == T  ))
    cond[[15]] = which( A & B & C & D == T  );     n.1234 = length(which(A & B & C & D == T  ))
    cond[[16]] = which( !A & !B & !C & !D == T  ); n.compl = length(which(!A & !B & !C & !D == T  ))

    VennDiagram::draw.quad.venn(area1 = sum(A),
                                area2 = sum(B),
                                area3 = sum(C),
                                area4 = sum(D),
                                n12 = sum(A&B),
                                n13 = sum(A&C),
                                n14 = sum(A&D),
                                n23 = sum(B&C),
                                n24 = sum(B&D),
                                n34 = sum(C&D),
                                n123 = sum(A&B&C),
                                n124 = sum(A&B&D),
                                n134 = sum(A&C&D),
                                n234 = sum(B&C&D),
                                n1234 = sum(A&B&C&D),
                                category = names(dat)[covari.sel],
                                lty = 1,
                                fill = "white", title = title) -> vp
    grid::grid.draw(grid::gList(vp,
                                grid::textGrob(x=0.1, y=0.1 ,n.compl, hjust = 0, just = 0)))

  }else if(n.subgrp == 5){
    A = dat[, covari.sel[1]] == cats.var.all[[1]][cat.sel[1]]
    B = dat[, covari.sel[2]] == cats.var.all[[2]][cat.sel[2]]
    C = dat[, covari.sel[3]] == cats.var.all[[3]][cat.sel[3]]
    D = dat[, covari.sel[4]] == cats.var.all[[4]][cat.sel[4]]
    E = dat[, covari.sel[5]] == cats.var.all[[5]][cat.sel[5]]

    cond = list()
    cond[[1]] = which( A & !B & !C & !D & !E == T  );   n.1 = length(which( A & !B & !C & !D & !E == T  ))
    cond[[2]] = which( !A & B & !C & !D & !E == T  );   n.2 = length(which( !A & B & !C & !D & !E == T  ))
    cond[[3]] = which( !A & !B & C & !D & !E == T  );   n.3 = length(which( !A & !B & C & !D & !E == T  ))
    cond[[4]] = which( !A & !B & !C & D & !E == T  );   n.4 = length(which( !A & !B & !C & D & !E == T  ))
    cond[[5]] = which( !A & !B & !C & !D & E == T  );   n.5 = length(which( !A & !B & !C & !D & E == T  ))
    cond[[6]] = which( A & B & !C & !D & !E == T  );    n.12 = length(which( A & B & !C & !D & !E == T  ))
    cond[[7]] = which( A & !B & C & !D & !E == T  );    n.13 = length(which( A & !B & C & !D & !E == T  ))
    cond[[8]] = which( A & !B & !C & D & !E == T  );    n.14 = length(which( A & !B & !C & D & !E == T  ))
    cond[[9]] = which( A & !B & !C & !D & E == T  );    n.15 = length(which( A & !B & !C & !D & E == T  ))
    cond[[10]] = which( !A & B & C & !D & !E == T  );   n.23 = length(which( !A & B & C & !D & !E == T  ))
    cond[[11]] = which( !A & B & !C & D & !E == T  );   n.24 = length(which( !A & B & !C & D & !E == T  ))
    cond[[12]] = which( !A & B & !C & !D & E == T  );   n.25 = length(which( !A & B & !C & !D & E == T  ))
    cond[[13]] = which( !A & !B & C & D & !E == T  );   n.34 = length(which( !A & !B & C & D & !E == T  ))
    cond[[14]] = which( !A & !B & C & !D & E == T  );   n.35 = length(which( !A & !B & C & !D & E == T  ))
    cond[[15]] = which( !A & !B & !C & D & E == T  );   n.45 = length(which( !A & !B & !C & D & E == T  ))
    cond[[16]] = which( A & B & C & !D & !E == T  );    n.123 = length(which( A & B & C & !D & !E == T  ))
    cond[[17]] = which( A & B & !C & D & !E == T  );    n.124 = length(which( A & B & !C & D & !E == T  ))
    cond[[18]] = which( A & B & !C & !D & E == T  );    n.125 = length(which( A & B & !C & !D & E == T  ))
    cond[[19]] = which( A & !B & C & D & !E == T  );    n.134 = length(which( A & !B & C & D & !E == T  ))
    cond[[20]] = which( A & !B & C & !D & E == T  );    n.135 = length(which( A & !B & C & !D & E == T  ))
    cond[[21]] = which( A & !B & !C & D & E == T  );    n.145 = length(which( A & !B & !C & D & E == T  ))
    cond[[22]] = which( !A & B & C & D & !E == T  );    n.234 = length(which( !A & B & C & D & !E == T  ))
    cond[[23]] = which( !A & B & C & !D & E == T  );    n.235 = length(which( !A & B & C & !D & E == T  ))
    cond[[24]] = which( !A & B & !C & D & E == T  );    n.245 = length(which( !A & B & !C & D & E == T  ))
    cond[[25]] = which( !A & !B & C & D & E == T  );    n.345 = length(which( !A & !B & C & D & E == T  ))
    cond[[26]] = which( A & B & C & D & !E == T  );     n.1234 = length(which( A & B & C & D & !E == T  ))
    cond[[27]] = which( A & B & C & !D & E == T  );     n.1235 = length(which( A & B & C & !D & E == T  ))
    cond[[28]] = which( A & B & !C & D & E == T  );     n.1245 = length(which( A & B & !C & D & E == T  ))
    cond[[29]] = which( A & !B & C & D & E == T  );     n.1345 = length(which( A & !B & C & D & E == T  ))
    cond[[30]] = which( !A & B & C & D & E == T  );     n.2345 = length(which( !A & B & C & D & E == T  ))
    cond[[31]] = which( A & B & C & D & E == T  );      n.12345 = length(which( A & B & C & D & E == T  ))
    cond[[32]] = which( !A & !B & !C & !D & !E == T  ); n.compl = length(which( !A & !B & !C & !D & !E == T  ))

    VennDiagram::draw.quintuple.venn(area1 = sum(A),
                                     area2 = sum(B),
                                     area3 = sum(C),
                                     area4 = sum(D),
                                     area5 = sum(E),
                                     n12 = sum(A&B),
                                     n13 = sum(A&C),
                                     n14 = sum(A&D),
                                     n15 = sum(A&E),
                                     n23 = sum(B&C),
                                     n24 = sum(B&D),
                                     n25 = sum(B&E),
                                     n34 = sum(C&D),
                                     n35 = sum(C&E),
                                     n45 = sum(D&E),
                                     n123 = sum(A&B&C),
                                     n124 = sum(A&B&D),
                                     n125 = sum(A&B&E),
                                     n134 = sum(A&C&D),
                                     n135 = sum(A&C&E),
                                     n145 = sum(A&D&E),
                                     n234 = sum(B&C&D),
                                     n235 = sum(B&C&E),
                                     n245 = sum(B&D&E),
                                     n345 = sum(C&D&E),
                                     n1234 = sum(A&B&C&D),
                                     n1235 = sum(A&B&C&E),
                                     n1245 = sum(A&B&D&E),
                                     n1345 = sum(A&C&D&E),
                                     n2345 = sum(B&C&D&E),
                                     n12345 = sum(A&B&C&D&E),
                                     category = names(dat)[covari.sel],
                                     lty = 1, fill = "white", title = title) -> vp
    grid::grid.draw(grid::gList(vp,
                                grid::textGrob(x=0.1, y=0.1 ,n.compl, hjust = 0, just = 0)))
  }

  data.subgrp = list()
  for (i in 1 : n.subgrp.tol )  data.subgrp[[i]] =  dat[cond[[i]], ]

  # create matrices for treatment size and standard error of MLE

  treatment.mean = vector()
  for (i in 1 : n.subgrp.tol ){
    cond1 = sum(data.subgrp[[i]]$trt == "0") == 0
    cond2 = sum(data.subgrp[[i]]$trt == "1") == 0

    if (cond1 | cond2 ){
      treatment.mean[i] = NA
    }else{

      if (outcome.type == "continuous"){
        model.int = lm(resp ~ trt,  data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coefficients[2, 1]
      }else if (outcome.type == "binary"){
        model.int = glm(resp ~ trt,  family = "binomial", data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coefficients[2, 1]
      }else if (outcome.type == "survival"){
        model.int = coxph(Surv(time, status) ~ trt, data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coef[1, 1]
      }
    }
  }

  cat("The minimum of treatment effect sizes is", c(min(treatment.mean, na.rm = T)), "\n")
  cat("The maximum of treatment effect sizes is", c(max(treatment.mean, na.rm = T)), "\n")

  # ################################################ 2. produce a graph  #################################################################
  #
  #
  # pal.2=colorRampPalette(c("black", "red", "yellow"), space="rgb")
  # breaks <- seq(min(range.strip) - 0.0000001, max(range.strip) + 0.0000001, length.out= n.brk)
  # levs=breaks
  #
  # dev.new(width=10,height=10,noRStudioGD = TRUE)
  # layout(matrix(c(1, 2), nrow=1, ncol=2), widths=c(4,1))
  # par(mar=c(1,1,3,1))
  #
  # if (n.subgrp == 1){
  #   plot(0:10, 0:10, type='n', axes = FALSE, xlab = "", ylab = "", main = title, cex.main = font.size[1])
  #
  #   r = 1/ sin(2 * pi / 2 * 1 / 2)
  #   centre.x = 4  + r * sin(2 * pi / 2 * 1 / 2);  centre.y = 4  + r * cos(2 * pi / 2 * 1 / 2)
  #
  #   theta0 = seq(pi/2, pi/2 + (2 -1 )* (2 * pi / 2), 2/2 * pi )
  #
  #   circle.ox = rep(0, 1); circle.oy = rep(0, 1)
  #   circle.ox = r * cos(theta0) + centre.x;  circle.oy = r * sin(theta0) + centre.y
  #
  #   theta = seq(0,2*pi, length = 1000)
  #   circle.x <- circle.y <- matrix(rep(0, 1 * 1001), nrow = 1)
  #   picture = list()
  #   for (i in 1 : 1){
  #     circle.x[i, ] = c(2*cos(theta)+ circle.ox[i], 2*cos(theta[1000])+ circle.ox[i])
  #     circle.y[i, ] = c(2*sin(theta)+ circle.oy[i], 2*sin(theta[1000])+ circle.oy[i])
  #     picture[[i]] =  SpatialPolygons(list(Polygons(list(Polygon(cbind(rev(circle.x[i, ]), rev(circle.y[i, ])))), ID = i)))
  #   }
  #
  #   col.vec = pal.2(length(breaks)-1)
  #   if (outside.area){                                                                             # draw the color for the effect size for the region: !A & !B
  #     if (is.na(treatment.mean[n.subgrp.tol])){
  #       col.idx = n.subgrp.tol
  #       col.vec[n.subgrp.tol] = "white"
  #     }else{
  #       col.idx = which(treatment.mean[n.subgrp.tol] < breaks)
  #       col.idx = col.idx[1] - 1
  #     }
  #     col.outside.area = col.vec[col.idx]
  #     rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col=col.outside.area)       # the graphical object of the complement of the intersection of the all 5 sets
  #                                                                                                  # but actually it is the graphical object of the rectangle without deleting the 5 sets
  #     if (col.outside.area %in% col.vec[1: ceiling(n.brk/2)]){
  #       col.txt.outside.area = "lightblue"
  #     }else{
  #       col.txt.outside.area = "black"
  #     }
  #   }else{
  #     col.txt.outside.area = "black"
  #   }
  #
  #   for (i in 1 : (n.subgrp.tol - 1)){
  #     col.vec = pal.2(length(breaks)-1)
  #     if (is.na(treatment.mean[i])){
  #       col.idx = i
  #       col.vec[i] = "white"
  #     }else{
  #       col.idx = which(treatment.mean[i] < breaks)
  #       col.idx = col.idx[1] - 1
  #     }
  #
  #     ### color an area corresponding to the corresponding subgroup
  #
  #     plot(picture[[i]], add=TRUE, col=col.vec[col.idx], lty = 2, lwd = 2, border = "lightblue")
  #   }
  #   text(circle.ox[1], circle.oy[1], labels=c("A"), col = col.txt.outside.area, cex = font.size[2])
  #   text(circle.ox[1], circle.oy[1] +0.5, labels= n.1, col = "green", cex = font.size[3])
  #   text(9, 9, labels= n.compl, col = "green", cex = font.size[3])
  #   box()
  #
  # }else if (n.subgrp == 2){
  #
  #   plot(0:10, 0:10, type='n', axes = FALSE, xlab = "", ylab = "", main = title)
  #
  #   r = 1/ sin(2 * pi / 2 * 1 / 2)
  #   centre.x = 4  + r * sin(2 * pi / 2 * 1 / 2)
  #   centre.y = 4  + r * cos(2 * pi / 2 * 1 / 2)
  #
  #   theta0 = seq(pi/2, pi/2 + (2 -1 )* (2 * pi / 2), 2/2 * pi )
  #
  #   circle.ox = rep(0, 2); circle.oy = rep(0, 2);
  #   circle.ox = r * cos(theta0) + centre.x
  #   circle.oy = r * sin(theta0) + centre.y
  #
  #   theta = seq(0,2*pi, length= 800)
  #   circle.x = matrix(rep(0, 2 * 801), nrow = 2)
  #   circle.y = matrix(rep(0, 2 * 801), nrow = 2)
  #   picture = list()
  #   for (i in 1 : 2){
  #     circle.x[i, ] = c(2*cos(theta)+ circle.ox[i], 2*cos(theta[800])+ circle.ox[i])
  #     circle.y[i, ] = c(2*sin(theta)+ circle.oy[i], 2*sin(theta[800])+ circle.oy[i])
  #     picture[[i]] =  SpatialPolygons(list(Polygons(list(Polygon(cbind(rev(circle.x[i, ]), rev(circle.y[i, ])))), ID = i)))
  #   }
  #   picture[[3]] = gIntersection( picture[[1]], picture[[2]] )
  #
  #   col.vec = pal.2(length(breaks)-1)
  #   if (outside.area){                                                                             # draw the color for the effect size for the region: !A & !B
  #     if (is.na(treatment.mean[n.subgrp.tol])){
  #       col.idx = n.subgrp.tol
  #       col.vec[n.subgrp.tol] = "white"
  #     }else{
  #       col.idx = which(treatment.mean[n.subgrp.tol] < breaks)
  #       col.idx = col.idx[1] - 1
  #     }
  #     col.outside.area = col.vec[col.idx]
  #     rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col=col.outside.area)       # the graphical object of the complement of the intersection of the all 5 sets
  #                                                                                                  # but actually it is the graphical object of the rectangle without deleting the 5 sets
  #     if (col.outside.area %in% col.vec[1: ceiling(n.brk/2)]){
  #       col.txt.outside.area = "lightblue"
  #     }else{
  #       col.txt.outside.area = "black"
  #     }
  #   }else{
  #     col.txt.outside.area = "black"
  #   }
  #
  #   for (i in 1 : (n.subgrp.tol - 1)){
  #     col.vec = pal.2(length(breaks)-1)
  #     if (is.na(treatment.mean[i])){
  #       col.idx = i
  #       col.vec[i] = "white"
  #     }else{
  #       col.idx = which(treatment.mean[i] < breaks)
  #       col.idx = col.idx[1] - 1
  #     }
  #
  #     ### color an area corresponding to the corresponding subgroup
  #
  #     plot(picture[[i]], add=TRUE, col=col.vec[col.idx], lty = 2, lwd = 2, border = "lightblue")
  #   }
  #   text(circle.ox[1], circle.oy[1] +2.5, labels=c("A"), col = col.txt.outside.area, cex = font.size[2])
  #   text(circle.ox[2], circle.oy[2] -2.5,  labels=c("B"), col = col.txt.outside.area, cex = font.size[2])
  #   text(circle.ox[1], circle.oy[1] +0.5, labels= n.1, col = "green", cex = font.size[3])
  #   text(circle.ox[2], circle.oy[2] -0.5, labels= n.2, col = "green", cex = font.size[3])
  #   text(circle.ox[2], circle.oy[2] + 1, labels= n.12, col = "green", cex = font.size[3])
  #   text(9, 9, labels= n.compl, col = "green", cex = font.size[3])
  #   box()
  #
  # }else if (n.subgrp == 3){
  #
  #   plot(0:10, 0:10, type='n', axes = FALSE, xlab = "", ylab = "", main = title)
  #
  #   r = 1/ sin(2 * pi / 3 * 1 / 2)
  #   centre.x = 4  + r * sin(2 * pi / 3 * 1 / 2)
  #   centre.y = 4  + r * cos(2 * pi / 3 * 1 / 2)
  #
  #   theta0 = seq(pi/2, pi/2 + (3 -1 )* (2 * pi / 3), 2/3 * pi )
  #
  #   circle.ox = rep(0, 3); circle.oy = rep(0, 3);
  #   circle.ox = r * cos(theta0) + centre.x
  #   circle.oy = r * sin(theta0) + centre.y
  #
  #   theta = seq(0,2*pi, length= 800)
  #   circle.x = matrix(rep(0, 3 * 801), nrow = 3)
  #   circle.y = matrix(rep(0, 3 * 801), nrow = 3)
  #   picture = list()
  #   for (i in 1 : 3){
  #     circle.x[i, ] = c(2*cos(theta)+ circle.ox[i], 2*cos(theta[800])+ circle.ox[i])
  #     circle.y[i, ] = c(2*sin(theta)+ circle.oy[i], 2*sin(theta[800])+ circle.oy[i])
  #     picture[[i]] =  SpatialPolygons(list(Polygons(list(Polygon(cbind(rev(circle.x[i, ]), rev(circle.y[i, ])))), ID = i)))
  #   }
  #
  #   ind = 0
  #   for (i in 1 : (3 - 1)){
  #     for (j in (i + 1) : 3){
  #       ind = ind + 1
  #       k = ind + 3
  #       picture[[k]] = gIntersection( picture[[i]], picture[[j]] )
  #     }
  #   }
  #   picture[[n.subgrp.tol-1]] <- gIntersection(picture[[n.subgrp.tol - 2]], picture[[1]] )
  #
  #   col.vec = pal.2(length(breaks)-1)
  #   if (outside.area){                                                                             # draw the color for the effect size for the region: !A & !B & !C
  #     if (is.na(treatment.mean[n.subgrp.tol])){
  #       col.idx = n.subgrp.tol
  #       col.vec[n.subgrp.tol] = "white"
  #     }else{
  #       col.idx = which(treatment.mean[n.subgrp.tol] < breaks)
  #       col.idx = col.idx[1] - 1
  #     }
  #     col.outside.area = col.vec[col.idx]
  #     rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col=col.outside.area)       # the graphical object of the complement of the intersection of the all 5 sets
  #                                                                                                  # but actually it is the graphical object of the rectangle without deleting the 5 sets
  #     if (col.outside.area %in% col.vec[1: ceiling(n.brk/2)]){
  #       col.txt.outside.area = "lightblue"
  #     }else{
  #       col.txt.outside.area = "black"
  #     }
  #   }else{
  #     col.txt.outside.area = "black"
  #   }
  #
  #   for (i in 1 : (n.subgrp.tol - 1)){
  #     if (is.na(treatment.mean[i])){
  #       col.idx = i
  #       col.vec[i] = "white"
  #     }else{
  #       col.idx = which(treatment.mean[i] < breaks)
  #       col.idx = col.idx[1] - 1
  #     }
  #
  #     ### color an area corresponding to the corresponding subgroup
  #
  #     plot(picture[[i]], add=TRUE, col=col.vec[col.idx], lty = 2, lwd = 2, border = "lightblue")
  #   }
  #
  #   text(circle.ox[1], circle.oy[1] +2.5, labels=c("A"), col = col.txt.outside.area, cex = font.size[2])
  #   text(circle.ox[2]-2, circle.oy[2] -2,  labels=c("B"), col = col.txt.outside.area, cex = font.size[2])
  #   text(circle.ox[3]+2, circle.oy[3] -2, labels=c("C"), col = col.txt.outside.area, cex = font.size[2])
  #
  #   text(circle.ox[1], circle.oy[1] +0.5, labels= n.1, col = "green", cex = font.size[3])
  #   text(circle.ox[2] -0.5, circle.oy[2] -0.5, labels= n.2, col = "green", cex = font.size[3])
  #   text(circle.ox[3] +0.5, circle.oy[3] -0.5, labels= n.3, col = "green", cex = font.size[3])
  #   text(circle.ox[2], circle.oy[2] + 1.3, labels= n.12, col = "green", cex = font.size[3])
  #   text(circle.ox[3], circle.oy[3] + 1.3, labels= n.13, col = "green", cex = font.size[3])
  #   text(circle.ox[2] + 1, circle.oy[2] - 1, labels= n.23, col = "green", cex = font.size[3])
  #   text(circle.ox[1], circle.oy[1] - 1, labels= n.123, col = "green", cex = font.size[3])
  #   text(9, 9, labels= n.compl, col = "green", cex = font.size[3])
  #
  #   box()
  #
  # }else if (n.subgrp == 4){
  #
  #   plot(-4.5:4.5, -4.5:4.5, type='n', axes = FALSE, xlab = "", ylab = "", main = title)
  #
  #   picture = list()
  #   eSP1 <- ellipseSP(x0=0, y0=0, a=3, b=1.5, phi = pi/4)
  #   picture[[1]] = eSP1
  #   eSP2 <- ellipseSP(x0=0, y0=0, a=3, b=1.5, phi = -pi/4)
  #   picture[[2]] = eSP2
  #   eSP3 <- ellipseSP(x0=1.5, y0= -1, a=3, b=1.5, phi = pi/4)
  #   picture[[3]] = eSP3
  #   eSP4 <- ellipseSP(x0=-1.5, y0= -1, a=3, b=1.5, phi = -pi/4)
  #   picture[[4]] = eSP4
  #
  #   ind = 0
  #   for (i in 1 : (n.subgrp - 1)){
  #     for (j in (i + 1) : n.subgrp){
  #       ind = ind + 1
  #       k = ind + sum(sapply(1:(n.subgrp - 3), function(x) choose(n.subgrp, x)))
  #       picture[[k]] = gIntersection( picture[[i]], picture[[j]] )
  #     }
  #   }
  #
  #   ind = 0
  #   for (i in 1 : (n.subgrp - 2)){
  #     for (j in (i + 1) : (n.subgrp - 1) ){
  #       for (k in (j + 1) : n.subgrp){
  #         ind = ind + 1
  #         t = ind + sum(sapply(1:(n.subgrp - 2), function(x) choose(n.subgrp, x)))
  #         picture[[t]] = gIntersection(gIntersection( picture[[i]], picture[[j]]), picture[[k]])   # Reduce(intersect, list(a,b,c))
  #       }
  #     }
  #   }
  #
  #   picture[[n.subgrp.tol - 1]] = gIntersection(picture[[n.subgrp.tol - 2]], picture[[1]] )        # the graphical object of the intersection of the all 4 sets
  #
  #   col.vec = pal.2(length(breaks)-1)
  #   if (outside.area){                                                                             # draw the color for the effect size for the region: !A & !B & !C & !D
  #     if (is.na(treatment.mean[n.subgrp.tol])){
  #       col.idx = n.subgrp.tol
  #       col.vec[n.subgrp.tol] = "white"
  #     }else{
  #       col.idx = which(treatment.mean[n.subgrp.tol] < breaks)
  #       col.idx = col.idx[1] - 1
  #     }
  #     col.outside.area = col.vec[col.idx]
  #     rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col=col.outside.area)       # the graphical object of the complement of the intersection of the all 5 sets
  #                                                                                                  # but actually it is the graphical object of the rectangle without deleting the 5 sets
  #     if (col.outside.area %in% col.vec[1: ceiling(n.brk/2)]){
  #       col.txt.outside.area = "lightblue"
  #     }else{
  #       col.txt.outside.area = "black"
  #     }
  #   }else{
  #     col.txt.outside.area = "black"
  #   }
  #
  #   for (i in 1 : (n.subgrp.tol -1)){
  #     col.vec = pal.2(length(breaks)-1)
  #     if (is.na(treatment.mean[i])){
  #       col.idx = i
  #       col.vec[i] = "white"
  #     }else{
  #       col.idx = which(treatment.mean[i] < breaks)
  #       col.idx = col.idx[1] - 1
  #     }
  #
  #     ### color an area corresponding to the corresponding subgroup
  #
  #     plot(picture[[i]], add=TRUE, col=col.vec[col.idx], lty = 2, lwd = 2, border = "lightblue")
  #   }
  #
  #   Ellipse1 = eSP1@polygons[[1]]@Polygons[[1]]@coords
  #   text(Ellipse1[1,1], Ellipse1[1,2]+0.5, labels=c("A"), col = col.txt.outside.area, cex = font.size[2])
  #   Ellipse2 = eSP2@polygons[[1]]@Polygons[[1]]@coords
  #   text(Ellipse2[50,1], Ellipse2[50,2]+0.5, labels=c("B"), col = col.txt.outside.area, cex = font.size[2])
  #   Ellipse3 = eSP3@polygons[[1]]@Polygons[[1]]@coords
  #   text(Ellipse3[1,1], Ellipse3[1,2]+0.5, labels=c("C"), col = col.txt.outside.area, cex = font.size[2])
  #   Ellipse4 = eSP4@polygons[[1]]@Polygons[[1]]@coords
  #   text(Ellipse4[50,1], Ellipse4[50,2]+0.5, labels=c("D"), col = col.txt.outside.area, cex = font.size[2])
  #
  #   text(Ellipse1[1,1]-0.6, Ellipse1[1,2]- 0.5, labels= n.1, col = "green", cex = font.size[3])
  #   text(Ellipse2[50,1]+0.6, Ellipse2[50,2]- 0.5, labels= n.2, col = "green", cex = font.size[3])
  #   text(Ellipse3[1,1]-0.5, Ellipse3[1,2]- 0.5, labels= n.3, col = "green", cex = font.size[3])
  #   text(Ellipse4[50,1]+0.5, Ellipse4[50,2]- 0.5, labels= n.4, col = "green", cex = font.size[3])
  #   text(0, 0.5, labels= n.12, col = "green", cex = font.size[3])
  #   text(Ellipse1[1,1]-0.2, Ellipse1[1,2]- 1.5, labels= n.13, col = "green", cex = font.size[3])
  #   text(-1.5, -1.5, labels= n.14, col = "green", cex = font.size[3])
  #   text(1.5, -1.5, labels= n.23, col = "green", cex = font.size[3])
  #   text(Ellipse2[50,1]+0.2, Ellipse2[50,2]- 1.5, labels= n.24, col = "green", cex = font.size[3])
  #   text(0, -2.5, labels= n.34, col = "green", cex = font.size[3])
  #   text(1, -0.5, labels= n.123, col = "green", cex = font.size[3])
  #   text(-1, -0.5, labels= n.124, col = "green", cex = font.size[3])
  #   text(-0.5, -1.9, labels= n.134, col = "green", cex = font.size[3])
  #   text(0.5, -1.9, labels= n.234, col = "green", cex = font.size[3])
  #   text(0, -1.5, labels= n.1234, col = "green", cex = font.size[3])
  #   text(4, 4, labels= n.compl, col = "green", cex = font.size[3])
  #   box()
  #
  # }else if (n.subgrp == 5){
  #
  #   plot(1,1, type='n', axes = FALSE, xlab = "", ylab = "", xlim = c(-8,8), ylim = c(-10,7), main = title)
  #
  #   picture = list()
  #   eSP1 <- ellipseSP(x0=0, y0=0, a= 5.8, b= 2.3, phi = pi/2)
  #   picture[[1]] = eSP1
  #   eSP2 <- ellipseSP(x0= 1.9, y0= -1.15, a=5.9, b=2.35, phi = pi/8)
  #   picture[[2]] = eSP2
  #   eSP3 <- ellipseSP(x0= -2.15, y0= -2.1, a=6.1, b=2.45, phi = -pi/8)
  #   picture[[3]] = eSP3
  #   eSP4 <- ellipseSP(x0= -0.8, y0= -4.9, a=5.9, b=2.35, phi = pi* 11/36)
  #   picture[[4]] = eSP4
  #   eSP5 <- ellipseSP(x0= 2, y0= -3.85, a= 5.9, b=2.4, phi = -pi* 11/36)
  #   picture[[5]] = eSP5
  #
  #   ind = 0
  #   for (i in 1 : (n.subgrp - 1)){
  #     for (j in (i + 1) : n.subgrp){
  #       ind = ind + 1
  #       k = ind + sum(sapply(1:(n.subgrp - 4), function(x) choose(n.subgrp, x)))
  #       picture[[k]] = gIntersection( picture[[i]], picture[[j]] )
  #     }
  #   }
  #
  #   ind = 0
  #   for (i in 1 : (n.subgrp - 2)){
  #     for (j in (i + 1) : (n.subgrp - 1) ){
  #       for (k in (j + 1) : n.subgrp){
  #         ind = ind + 1
  #         t = ind + sum(sapply(1:(n.subgrp - 3), function(x) choose(n.subgrp, x)))
  #         picture[[t]] = gIntersection(gIntersection( picture[[i]], picture[[j]]), picture[[k]])  # Reduce(intersect, list(a,b,c))
  #       }
  #     }
  #   }
  #
  #   ind = 0
  #   for (i in 1 : (n.subgrp - 3)){
  #     for (j in (i + 1) : (n.subgrp - 2) ){
  #       for (k in (j + 1) : (n.subgrp - 1)){
  #         for (m in (k + 1) : n.subgrp){
  #           ind = ind + 1
  #           t = ind + sum(sapply(1:(n.subgrp - 2), function(x) choose(n.subgrp, x)))
  #           picture[[t]] = gIntersection(gIntersection(gIntersection( picture[[i]], picture[[j]]), picture[[k]]), picture[[m]])
  #         }
  #       }
  #     }
  #   }
  #
  #   picture[[n.subgrp.tol - 1]] = gIntersection(picture[[n.subgrp.tol - 2]], picture[[1]] )              # the graphical object of the intersection of the all 5 sets
  #
  #   col.vec = pal.2(length(breaks)-1)
  #   if (outside.area){                                                                                   # draw the color for the effect size of the region: !A & !B & !C & !D & !E
  #     if (is.na(treatment.mean[n.subgrp.tol]) == TRUE){
  #       col.idx = n.subgrp.tol
  #       col.vec[n.subgrp.tol] = "white"
  #     }else{
  #       col.idx = which(treatment.mean[n.subgrp.tol] < breaks)
  #       col.idx = col.idx[1] - 1
  #     }
  #
  #     col.outside.area = col.vec[col.idx]
  #     rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col=col.outside.area)             # the graphical object of the complement of the intersection of the all 5 sets
  #     # but actually it is the graphical object of the rectangle without deleting the 5 sets
  #     if (col.outside.area %in% col.vec[1: ceiling(n.brk/2)]){
  #       col.txt.outside.area = "lightblue"
  #     }else{
  #       col.txt.outside.area = "black"
  #     }
  #     #plot(picture[[n.subgrp.tol]], add=TRUE, col=col.vec[col.idx])
  #   }else{
  #     col.txt.outside.area = "black"
  #   }
  #
  #
  #   for (i in 1 : (n.subgrp.tol - 1)){
  #     col.vec = pal.2(length(breaks)-1)
  #     if (is.na(treatment.mean[i])){
  #       col.idx = i
  #       col.vec[i] = "white"
  #     }else{
  #       col.idx = which(treatment.mean[i] < breaks)
  #       col.idx = col.idx[1] - 1
  #     }
  #
  #     ### color an area corresponding to the corresponding subgroup
  #
  #     plot(picture[[i]], add=TRUE, col=col.vec[col.idx], lty = 2, lwd = 2, border = "lightblue")
  #   }
  #
  #   Ellipse1 = eSP1@polygons[[1]]@Polygons[[1]]@coords
  #   text(Ellipse1[1,1], Ellipse1[1,2]+0.5, labels=c("A"), col = col.txt.outside.area, cex = font.size[2])
  #   Ellipse2 = eSP2@polygons[[1]]@Polygons[[1]]@coords
  #   text(Ellipse2[1,1]+0.2, Ellipse2[1,2]+0.8, labels=c("B"), col = col.txt.outside.area, cex = font.size[2])
  #   Ellipse3 = eSP3@polygons[[1]]@Polygons[[1]]@coords
  #   text(Ellipse3[50,1]+0.2, Ellipse3[50,2]+1.2, labels=c("C"), col = col.txt.outside.area, cex = font.size[2])
  #   Ellipse4 = eSP4@polygons[[1]]@Polygons[[1]]@coords
  #   text(Ellipse4[50,1]-0.5, Ellipse4[50,2]-0.5, labels=c("D"), col = col.txt.outside.area, cex = font.size[2])
  #   Ellipse5 = eSP5@polygons[[1]]@Polygons[[1]]@coords
  #   text(Ellipse5[1,1]+0.5, Ellipse5[1,2]-0.5, labels=c("E"), col = col.txt.outside.area, cex = font.size[2])
  #
  #   text(Ellipse1[1,1], Ellipse1[1,2]- 2.5,     labels= n.1, col = "green", cex = font.size[3])
  #   text(Ellipse2[1,1]-2.5, Ellipse2[1,2]-1.5,  labels= n.2, col = "green", cex = font.size[3])
  #   text(Ellipse3[50,1]+2.5, Ellipse3[50,2]-1.5,labels= n.3, col = "green", cex = font.size[3])
  #   text(Ellipse4[50,1]+1.5, Ellipse4[50,2]+2.5,labels= n.4, col = "green", cex = font.size[3])
  #   text(Ellipse5[1,1]-1.5, Ellipse5[1,2]+2.5,  labels= n.5, col = "green", cex = font.size[3])
  #   text(1.5, 0.8,   labels= n.12, col = "green", cex = font.size[3])
  #   text(-2.1, 0,    labels= n.13, col = "green", cex = font.size[3])
  #   text(-0.3, -5.5, labels= n.14, col = "green", cex = font.size[3])
  #   text(-0.8, 0.5,  labels= n.15, col = "green", cex = font.size[3])
  #   text(-2.6, -2.35,labels= n.23, col = "green", cex = font.size[3])
  #   text(2.7, -0.55, labels= n.24, col = "green", cex = font.size[3])
  #   text(3.4, -2.35, labels= n.25, col = "green", cex = font.size[3])
  #   text(-2.2, -4.4, labels= n.34, col = "green", cex = font.size[3])
  #   text(2.8, -4.35, labels= n.35, col = "green", cex = font.size[3])
  #   text(1, -5.8,    labels= n.45, col = "green", cex = font.size[3])
  #   text(-2, -1.65,  labels= n.123, col = "green", cex = font.size[3])
  #   text(2.1, 0,     labels= n.124, col = "green", cex = font.size[3])
  #   text(0.3, 0.2,   labels= n.125, col = "green", cex = font.size[3])
  #   text(-0.8, -4.6, labels= n.134, col = "green", cex = font.size[3])
  #   text(-1.6, 0,    labels= n.135, col = "green", cex = font.size[3])
  #   text(0.45, -5.5, labels= n.145, col = "green", cex = font.size[3])
  #   text(-2.2, -3.5, labels= n.234, col = "green", cex = font.size[3])
  #   text(3.05, -3.1, labels= n.235, col = "green", cex = font.size[3])
  #   text(2.7, -1.55, labels= n.245, col = "green", cex = font.size[3])
  #   text(1.8, -4.35, labels= n.345, col = "green", cex = font.size[3])
  #   text(-1.4, -3.5, labels= n.1234, col = "green", cex = font.size[3])
  #   text(-1, -0.8,   labels= n.1235, col = "green", cex = font.size[3])
  #   text(1.5, -0.8,  labels= n.1245, col = "green", cex = font.size[3])
  #   text(0.45, -4.5, labels= n.1345, col = "green", cex = font.size[3])
  #   text(2.35, -2.9, labels= n.2345, col = "green", cex = font.size[3])
  #   text(0.4, -2.5,  labels= n.12345, col = "green", cex = font.size[3])
  #   text(7, 6,       labels= n.compl, col = "green", cex = font.size[3])
  #   box()
  # }
  #
  # xy.current.pos = par("usr")
  # lab.subgrp = vector()
  # for (i in 1 : n.subgrp){
  #   lab.subgrp[i] = paste(" (", LETTERS[i],") ", lab.vars[i], " = ", cats.var.all[[i]][cat.sel[i]], sep = "")
  # }
  # legend(xy.current.pos[1], xy.current.pos[4], lab.subgrp, bty = "n", cex = font.size[4], text.col = col.txt.outside.area )
  #
  # ### make a color scale to show effect sizes
  #
  # par(mar=c(1,4.8, 3, 2))
  # image.scale(treatmeant.mean, col=pal.2(length(breaks)-1), breaks = breaks-1e-8, axis.pos = 4, add.axis = FALSE)
  # axis(2, at = breaks, labels = round(breaks, 3), las = 0, cex.axis = font.size[6])
  # box()
  # abline(h=levs)
  # title(ylab = strip, cex.lab = font.size[5], font.lab = 2)
  # #mtext("Effect size", side = 4, line = 3, cex.lab = 1)

}
