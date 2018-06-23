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
#'
#'@examples
#'data(prca)
#'plot_venn_fill(prca,
#'         covari.sel = c(5, 7, 4),#vars,
#'         cat.sel = c(2,2,2),
#'         trt.sel = 3,
#'         resp.sel = c(1,2),
#'         outcome.type = "survival",
#'         outside.area = FALSE,
#'         range.strip = c(-3, 3),
#'         n.brk = 31,
#'         n.brk.axis = 7,
#'         font.size = c(0.5, 0.5, 0.7, 0.5, 0.6, 0.6),
#'         strip = paste("Treatment effect size (log hazard ratio)"), palette = "hcl")
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
#' @import graphics
#' @export
plot_venn_fill <- function(dat, covari.sel, cat.sel, trt.sel, resp.sel, outcome.type,
                           outside.area=FALSE, range.strip=c(-6, 6), n.brk=13,
                           n.brk.axis=7,
               font.size = c(1, 1.5, 1, 0.9, 1, 1), title = NULL, strip = NULL,
               effect = "HR", show.overall = TRUE,
               palette = "divergent", col.power = 0.5){

  ################################################ 0. argument validity check  #################################################################
  effect = match.arg(effect)
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
      model.int = survival::coxph(Surv(time, status) ~ trt, data = dat)
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
    vp = VennDiagram::draw.single.venn(area = sum(A),
                                  category = names(dat)[covari.sel],
                                  lty = 1, ind = F,
                                  fill = "white", title = title)
  }else if (n.subgrp == 2){

    A = dat[, covari.sel[1]] == cats.var.all[[1]][cat.sel[1]]
    B = dat[, covari.sel[2]] == cats.var.all[[2]][cat.sel[2]]
    cond = list()
    cond[[1]] = which( A & !B == T  );  n.1 = length(which( A & !B == T  ))
    cond[[2]] = which( !A & B == T  );  n.2 = length(which( !A & B == T  ))
    cond[[3]] = which(A & B == T  );    n.12 = length(which(A & B == T  ))
    cond[[4]] = which(!A & !B == T  );  n.compl = length(which(!A & !B == T  ))

    vp = VennDiagram::draw.pairwise.venn(area1 = sum(A),
                                        area2 = sum(B),
                                        cross.area = sum(A&B),
                                        # scaled = F,
                                        ext.pos = c(90,90),
                                        # inverted = sum(A)<sum(B),
                                        category = names(dat)[covari.sel],
                                        lty = 1,
                                        fill = "white", ind = F,
                                        title = title)

  }else if (n.subgrp == 3){

    A = dat[, covari.sel[1]] == cats.var.all[[1]][cat.sel[1]]
    B = dat[, covari.sel[2]] == cats.var.all[[2]][cat.sel[2]]
    C = dat[, covari.sel[3]] == cats.var.all[[3]][cat.sel[3]]
    sum(A)
    sum(B)
    sum(C)
    cond = list()
    cond[[1]] = which( A & !B & !C == T  );  n.1 = length(which( A & !B & !C == T  ))
    cond[[2]] = which( !A & B & !C == T  );  n.2 = length(which( !A & B & !C == T  ))
    cond[[3]] = which( !A & !B & C == T  );  n.3 = length(which( !A & !B & C == T  ))
    cond[[4]] = which( A & B & !C == T  );   n.12 = length(which( A & B & !C == T  ))
    cond[[5]] = which( A & !B & C == T  );   n.13 = length(which( A & !B & C == T  ))
    cond[[6]] = which( !A & B & C == T  );   n.23 = length(which(!A & B & C == T  ))
    cond[[7]] = which( A & B & C == T  );    n.123 = length(which(A & B & C == T  ))
    cond[[8]] = which( !A & !B & !C == T  ); n.compl = length(which(!A & !B & !C == T  ))

    vp = VennDiagram::draw.triple.venn(area1 = sum(A),
                                  area2 = sum(B),
                                  area3 = sum(C),
                                  n12 = sum(A&B),
                                  n13 = sum(A&C),
                                  n23 = sum(B&C),
                                  n123 = sum(A&B&C),
                                  rotation.degree = 60,
                                  cat.pos  = c(225, 0, 140),
                                  cat.dist = rep(0.04, 3),
                                  euler.d = F, scaled = F,
                                  category = names(dat)[covari.sel],
                                  fontfamily = rep("sans", 7),
                                  cat.fontfamily = rep("sans", 3),ind = FALSE,
                                  lty = 1, cex = 1, cat.cex = 1,
                                  fill = "white", title = title)
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

    vp = VennDiagram::draw.quad.venn(area1 = sum(A),
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
                                lty = 1, ind = F,
                                fill = "white", title = title)

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

    vp = VennDiagram::draw.quintuple.venn(area1 = sum(A),
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
                                     category = names(dat)[covari.sel], ind = F,
                                     lty = 1, fill = "white", title = title)
  }

  data.subgrp = list()
  for (i in 1 : n.subgrp.tol )  data.subgrp[[i]] =  dat[cond[[i]], ]

  # create matrices for treatment size and standard error of MLE
  treatment.mean = vector()
  for (i in 1 : n.subgrp.tol){
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
        model.int = survival::coxph(Surv(time, status) ~ trt, data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coef[1, 1]
      }
    }
  }

  cat("The minimum of treatment effect sizes is", c(min(treatment.mean, na.rm = T)), "\n")
  cat("The maximum of treatment effect sizes is", c(max(treatment.mean, na.rm = T)), "\n")

  ################################################ 2. produce a graph  #################################################################
  colors = numeric(n.subgrp.tol)
  pal.2 = colorRampPalette(c("#fc8d59", "#ffffbf", "#91bfdb"), space = "rgb")
  breaks <- seq(min(range.strip) - 0.0000001, max(range.strip) + 0.0000001, length.out= n.brk)
  breaks.axis <- seq(min(range.strip) - 0.0000001, max(range.strip) + 0.0000001, length.out= n.brk.axis)
  col.vec = pal.2((length(breaks)-1))
  levs = breaks

  pal.YlRd = colorRampPalette(c("#fee090", "#d73027"),  space = "rgb")
  pal.WhBl = colorRampPalette(c("#e0f3f8", "#4575b4"),  space = "rgb")

  breaks = seq(min(range.strip) - 1e-8, max(range.strip) + 1e-8, length.out = n.brk)
  col.vec.div.pos = pal.WhBl((length(breaks)-1)/2)
  col.vec.div.neg = pal.YlRd((length(breaks)-1)/2)
  col.vec = c(rev(col.vec.div.neg), col.vec.div.pos)
  if (outcome.type == "survival" & effect == "HR") col.vec = rev(col.vec)
  if (palette == "hcl"){
    col.vec = colorspace::diverge_hcl(n = length(breaks)-1,
                                      c = 100, l = c(50,90),
                                      power = col.power)
    if (!(outcome.type == "survival" & effect == "HR")) col.vec = rev(col.vec)
  }
  for (i in 1 : (n.subgrp.tol)){
    if (is.na(treatment.mean[i])){
      colors[i] = "white"
    }else{
      col.idx = which(treatment.mean[i] < breaks)
      if(length(col.idx)==0) warning("Check range of strip")
      col.idx = col.idx[1] - 1
      ### color an area corresponding to the corresponding subgroup
      colors[i]=col.vec[col.idx]
    }
  }
  if (n.subgrp == 1){
    A <- list(list(x = as.vector(vp[[2]][[1]]), y = as.vector(vp[[2]][[2]])))
    ix <- sapply(vp, function(x) grepl("text", x$name, fixed = TRUE))
    labs <- do.call(rbind.data.frame, lapply(vp[ix], `[`, c("x", "y", "label")))
    # Plot it!
    vpf <- function(){
      plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
           col =  colors[n.subgrp.tol])
      polygon(A[[1]], col = colors[1])
      text(x = labs$x, y = labs$y, labels = labs$label)
      box()
    }

  }else if (n.subgrp == 2){
    A <- list(list(x = as.vector(vp[[3]][[1]]), y = as.vector(vp[[3]][[2]])))
    B <- list(list(x = as.vector(vp[[4]][[1]]), y = as.vector(vp[[4]][[2]])))
    AintB <- polyclip::polyclip(A, B)
    ix <- sapply(vp, function(x) grepl("text", x$name, fixed = TRUE))
    labs <- do.call(rbind.data.frame, lapply(vp[ix], `[`, c("x", "y", "label")))
    # Plot it!
    vpf <- function(){
      plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
           col =  colors[n.subgrp.tol])
      text(x = labs$x, y = labs$y, labels = labs$label)
      polygon(A[[1]], col = colors[1])
      polygon(B[[1]], col = colors[2])
      polygon(AintB[[1]], col = colors[3])
      text(x = labs$x, y = labs$y, labels = labs$label)
      box()
    }

  }else if (n.subgrp == 3){
    A <- list(list(x = as.vector(vp[[1]][[1]]), y = as.vector(vp[[1]][[2]])))
    B <- list(list(x = as.vector(vp[[2]][[1]]), y = as.vector(vp[[2]][[2]])))
    C <- list(list(x = as.vector(vp[[3]][[1]]), y = as.vector(vp[[3]][[2]])))
    AintB <- polyclip::polyclip(A, B)
    AintC <- polyclip::polyclip(A, C)
    BintC <- polyclip::polyclip(B, C)
    AiBiC <- polyclip::polyclip(polyclip::polyclip(A, B), C)
    ix <- sapply(vp, function(x) grepl("text", x$name, fixed = TRUE))
    labs <- do.call(rbind.data.frame, lapply(vp[ix], `[`, c("x", "y", "label")))
    # Plot it!
    vpf <- function(){
      plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
           col =  colors[n.subgrp.tol])
      text(x = labs$x, y = labs$y, labels = labs$label)
      polygon(A[[1]], col = colors[1])
      polygon(B[[1]], col = colors[2])
      polygon(C[[1]], col = colors[3])
      polygon(AintB[[1]], col = colors[4])
      polygon(AintC[[1]], col = colors[5])
      polygon(BintC[[1]], col = colors[6])
      polygon(AiBiC[[1]], col = colors[7])
      text(x = labs$x, y = labs$y, labels = labs$label)
      text(x=0.9, y=0.9 , labels = n.compl)
      box()
    }
  }else if (n.subgrp == 4){
    A <- list(list(x = as.vector(vp[[2]][[1]]), y = as.vector(vp[[2]][[2]])))
    B <- list(list(x = as.vector(vp[[1]][[1]]), y = as.vector(vp[[1]][[2]])))
    C <- list(list(x = as.vector(vp[[4]][[1]]), y = as.vector(vp[[4]][[2]])))
    D <- list(list(x = as.vector(vp[[3]][[1]]), y = as.vector(vp[[3]][[2]])))
    AintB <- polyclip::polyclip(A, B)
    AintC <- polyclip::polyclip(A, C)
    AintD <- polyclip::polyclip(A, D)
    BintC <- polyclip::polyclip(B, C)
    BintD <- polyclip::polyclip(B, D)
    CintD <- polyclip::polyclip(C, D)
    AiBiC <- polyclip::polyclip(polyclip::polyclip(A, B), C)
    AiBiD <- polyclip::polyclip(polyclip::polyclip(A, B), D)
    AiCiD <- polyclip::polyclip(polyclip::polyclip(A, C), D)
    BiCiD <- polyclip::polyclip(polyclip::polyclip(B, C), D)
    AiBiCiD <- polyclip::polyclip(polyclip::polyclip(polyclip::polyclip(A, B), C), D)

    ix <- sapply(vp, function(x) grepl("text", x$name, fixed = TRUE))
    labs <- do.call(rbind.data.frame, lapply(vp[ix], `[`, c("x", "y", "label")))
    # Plot it!
    vpf <- function(){
      plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
           col =  colors[n.subgrp.tol])
      text(x = labs$x, y = labs$y, labels = labs$label)
      polygon(A[[1]], col = colors[1])
      polygon(B[[1]], col = colors[2])
      polygon(C[[1]], col = colors[3])
      polygon(D[[1]], col = colors[4])
      polygon(AintB[[1]], col = colors[5])
      polygon(AintC[[1]], col = colors[6])
      polygon(AintD[[1]], col = colors[7])
      polygon(BintC[[1]], col = colors[8])
      polygon(BintD[[1]], col = colors[9])
      polygon(CintD[[1]], col = colors[10])
      polygon(AiBiC[[1]], col = colors[11])
      polygon(AiBiD[[1]], col = colors[12])
      polygon(AiCiD[[1]], col = colors[13])
      polygon(BiCiD[[1]], col = colors[14])
      polygon(AiBiCiD[[1]], col = colors[15])
      text(x=0.1, y=0.1 , labels = n.compl)
      text(x = labs$x, y = labs$y, labels = labs$label)
      box()
    }
  }else if (n.subgrp == 5){
    A <- list(list(x = as.vector(vp[[1]][[1]]), y = as.vector(vp[[1]][[2]])))
    B <- list(list(x = as.vector(vp[[2]][[1]]), y = as.vector(vp[[2]][[2]])))
    C <- list(list(x = as.vector(vp[[3]][[1]]), y = as.vector(vp[[3]][[2]])))
    D <- list(list(x = as.vector(vp[[4]][[1]]), y = as.vector(vp[[4]][[2]])))
    E <- list(list(x = as.vector(vp[[5]][[1]]), y = as.vector(vp[[5]][[2]])))
    AintB <- polyclip::polyclip(A, B)
    AintC <- polyclip::polyclip(A, C)
    AintD <- polyclip::polyclip(A, D)
    AintE <- polyclip::polyclip(A, E)
    BintC <- polyclip::polyclip(B, C)
    BintD <- polyclip::polyclip(B, D)
    BintE <- polyclip::polyclip(B, E)
    CintD <- polyclip::polyclip(C, D)
    CintE <- polyclip::polyclip(C, E)
    DintE <- polyclip::polyclip(D, E)
    AiBiC <- polyclip::polyclip(polyclip::polyclip(A, B), C)
    AiBiD <- polyclip::polyclip(polyclip::polyclip(A, B), D)
    AiBiE <- polyclip::polyclip(polyclip::polyclip(A, B), E)
    AiCiD <- polyclip::polyclip(polyclip::polyclip(A, C), D)
    AiCiE <- polyclip::polyclip(polyclip::polyclip(A, C), E)
    AiDiE <- polyclip::polyclip(polyclip::polyclip(A, D), E)
    BiCiD <- polyclip::polyclip(polyclip::polyclip(B, C), D)
    BiCiE <- polyclip::polyclip(polyclip::polyclip(B, C), E)
    BiDiE <- polyclip::polyclip(polyclip::polyclip(B, D), E)
    CiDiE <- polyclip::polyclip(polyclip::polyclip(C, D), E)
    AiBiCiD <- polyclip::polyclip(polyclip::polyclip(polyclip::polyclip(A, B), C), D)
    AiBiCiE <- polyclip::polyclip(polyclip::polyclip(polyclip::polyclip(A, B), C), E)
    AiBiDiE <- polyclip::polyclip(polyclip::polyclip(polyclip::polyclip(A, B), D), E)
    AiCiDiE <- polyclip::polyclip(polyclip::polyclip(polyclip::polyclip(A, C), D), E)
    BiCiDiE <- polyclip::polyclip(polyclip::polyclip(polyclip::polyclip(B, C), D), E)
    AiBiCiDiE <- polyclip::polyclip(polyclip::polyclip(polyclip::polyclip(polyclip::polyclip(A, B), C), D), E)

    ix <- sapply(vp, function(x) grepl("text", x$name, fixed = TRUE))
    labs <- do.call(rbind.data.frame, lapply(vp[ix], `[`, c("x", "y", "label")))
    # Plot it!

    vpf <- function(){
      plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
           col =  colors[n.subgrp.tol])
      text(x = labs$x, y = labs$y, labels = labs$label)
      polygon(A[[1]], col = colors[1])
      polygon(B[[1]], col = colors[2])
      polygon(C[[1]], col = colors[3])
      polygon(D[[1]], col = colors[4])
      polygon(E[[1]], col = colors[5])
      polygon(AintB[[1]], col = colors[6])
      polygon(AintC[[1]], col = colors[7])
      polygon(AintD[[1]], col = colors[8])
      polygon(AintE[[1]], col = colors[9])
      polygon(BintC[[1]], col = colors[10])
      polygon(BintD[[1]], col = colors[11])
      polygon(BintE[[1]], col = colors[12])
      polygon(CintD[[1]], col = colors[13])
      polygon(CintE[[1]], col = colors[14])
      polygon(DintE[[1]], col = colors[15])
      polygon(AiBiC[[1]], col = colors[16])
      polygon(AiBiD[[1]], col = colors[17])
      polygon(AiBiE[[1]], col = colors[18])
      polygon(AiCiD[[1]], col = colors[19])
      polygon(AiCiE[[1]], col = colors[20])
      polygon(AiDiE[[1]], col = colors[21])
      polygon(BiCiD[[1]], col = colors[22])
      polygon(BiCiE[[1]], col = colors[23])
      polygon(BiDiE[[1]], col = colors[24])
      polygon(CiDiE[[1]], col = colors[25])
      polygon(AiBiCiD[[1]], col = colors[26])
      polygon(AiBiCiE[[1]], col = colors[27])
      polygon(AiBiDiE[[1]], col = colors[28])
      polygon(AiCiDiE[[1]], col = colors[29])
      polygon(BiCiDiE[[1]], col = colors[30])
      polygon(AiBiCiDiE[[1]], col = colors[31])
      text(x = labs$x, y = labs$y, labels = labs$label)
      box()
    }
  }

  layout(matrix(c(1, 2), nrow=1, ncol=2), widths=c(4,1))
  par(mar=c(1,1,1,1))
  vpf()
  par(mar=c(1,2, 1, 2))
  image.scale(treatment.mean, col=col.vec,
              breaks = breaks-1e-8, axis.pos = 4, add.axis = FALSE)
  if(show.overall){
    cat("Overall Treatment effect is:",
        overall.treatment.mean, ", with confidence interval: (",
        overall.treatment.lower,";",overall.treatment.upper,")")
    points(x = 0.5,
           (overall.treatment.mean), pch = 20)
    points(x = 0.5, overall.treatment.lower, pch = "-")
    points(x = 0.5, overall.treatment.upper, pch = "-")
    segments(x0 = 0.5, x1 = 0.5,
             y0 = overall.treatment.lower,
             y1 = overall.treatment.upper)
  }
  axis(2,
       at = breaks.axis,
       labels = round(breaks.axis, 3),
       las = 0, cex.axis = font.size[6])
  mtext(strip, side=4, line=1, cex.lab = font.size[5])
  par(mfrow=c(1,1))
}
