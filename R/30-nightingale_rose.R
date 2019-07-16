#' Nightingale plot
#'
#' This function produces a nightingale plot with a binary response.
#'
#' @param dat              a data set
#' @param covari.sel       a vector of indices of the two covariates
#' @param trt.sel          a covariate index specifying the treatment code
#' @param resp.sel         a covariate index specifying the response variable
#' @param outcome.type     a string specifying the type of the response variable, it can be "continuous", or "binary" or  "survival".
#' @param seq_by           a number specifying the breaks in the y-axis
#' @param title            a string specifying the main title.
#' @param lab.y            a character value specifying the label of the y axis
#' @param strip            title for the legend
#' @param palette_colors   the colors for the graphics, a two character vector.
#'
#' @examples
#' library(dplyr)
#' dat <- prca
#' levels(dat$age_group) = c("Young","Middle-aged","Old")
#' levels(dat$weight_group)  = c("Low","Mid","High")
#' comb_levels = c("Young - Low", "Young - Mid", "Young - High",
#'                 "Middle-aged - Low", "Middle-aged - Mid", "Middle-aged - High",
#'                 "Old - Low", "Old - Mid", "Old - High")
#' dat %>%
#'   mutate(AgeWeight = factor(sprintf("%s - %s", age_group, weight_group),
#'                             levels = comb_levels))  %>%
#'   mutate(survival = factor(ifelse(survtime > 24 , "Yes", "No"),
#'                            levels = c("No", "Yes"))) %>%
#'   mutate(rx = factor(rx, labels = c("Control", "Treatment"))) -> dat
#'
#' plot_nightingale(dat = dat, covari.sel = 16, resp.sel = 17,
#'                  strip = "2-year survival")
#' plot_nightingale(dat = dat, trt.sel = 3, covari.sel = 16,
#'                  resp.sel = 17,
#'                  seq_by = 50,
#'                  strip = "2-year survival")
#' @export
#' @import ggplot2
plot_nightingale <- function(dat, covari.sel, trt.sel = NULL, resp.sel,
                             outcome.type = "binary",
                             seq_by = 100,
                             title = NULL, lab.y = NULL,
                             strip = "Response",
                             palette_colors = c("#faa8d2", "#80b1d3")
                             ){

  #### 0. argument validity check  #############################################
  outcome.type = match.arg(outcome.type)
  if(!(length(covari.sel) %in% 1:3)) {stop("Only 1 to 3 covariates are allowed in covari.sel")}
  #### 1. data manipulation  #############################################
  if (!is.null(trt.sel)) {
    return(plot_nightingale_by_trt(dat, covari.sel, trt.sel, resp.sel,
                                   outcome.type,
                                   seq_by,
                                   title, lab.y,
                                   strip,
                                   palette_colors))
  }
  response = factor(dat[, resp.sel])
  y_lev = levels(response)
  if(length(covari.sel) == 1) {
    new_var = dat[,covari.sel]
  } else {
    new_var = apply(dat[,covari.sel, FALSE], 1, paste0, collapse = " - ")
  }

  table(new_var, dat$survival)-> datatable.
  data.frame(new_var = factor(rownames(datatable.),
                              levels = rownames(datatable.)),
             cbind(datatable.,
                   Total = rowSums(datatable.),
                   pos = 1:nrow(datatable.))) -> dt

  max_freq = ceiling(max(dt$Total)/100)*100 # Found the maximum frequency and round up to the nearest 100
  y_int = seq(0, max_freq, by = seq_by)


  names(palette_colors) = y_lev

  #### 2. draw plot using ggplot2 ##############################################
  ggplot(dt) +
    geom_hline(yintercept = y_int, colour = "grey80", size = 0.2) +
    geom_segment(aes(x = pos, xend = pos, y = 0, yend = max_freq), colour = "grey80", size = 0.2) +
    geom_bar(aes(x = pos, y = (Total), fill = y_lev[1]), color = "black", width = 1, stat = "identity") +
    geom_bar(aes(x = pos, y = (Yes),   fill = y_lev[2]), color = "black", width = 1, stat = "identity") +
    scale_y_continuous(name = lab.y, trans = "sqrt", limits = c(0, max_freq+100), breaks = y_int) +
    scale_fill_manual(values = palette_colors) +
    coord_polar(start = pi/4) +
    labs(title = title, x = NULL, y = lab.y, fill = strip) +
    theme_bw(base_size = 14) +
    theme(axis.text.x = element_blank(),
          plot.margin = unit(c(0,0,0,0),"cm"),
          legend.box.margin =  unit(c(0,0,0,0),"cm"),
          legend.margin = margin(-0.5,0,0,0, unit='cm'),
          panel.grid  = element_blank(),
          legend.position = "bottom",
          legend.direction = "horizontal") +
    geom_text(aes(x = pos, y = Total + 1,
                  label = new_var),
              hjust = 0,
              angle = 45 - 360/max(dt$pos)/2 - 360/max(dt$pos) * (dt$pos - 1))
}

plot_nightingale_by_trt <- function(dat, covari.sel, trt.sel, resp.sel,
                             outcome.type = "binary",
                             seq_by = 100,
                             title = NULL, lab.y = NULL,
                             strip = "Response",
                             palette_colors = c("#faa8d2", "#80b1d3")){

  #### 0. argument validity check  #############################################
  outcome.type = match.arg(outcome.type)
  if(!(length(covari.sel) %in% 1:3)) {stop("Only 1 to 3 covariates are allowed in covari.sel")}

  response = factor(dat[, resp.sel])
  treatment = factor(dat[, trt.sel])
  y_lev = levels(response)
  trt_lev = levels(treatment)
  if(length(covari.sel) == 1) {
    new_var = dat[,covari.sel]
  } else {
    new_var = apply(dat[,covari.sel, FALSE], 1, paste0, collapse = " - ")
  }

  table(new_var, response, treatment)-> datatable.
  dt.control = datatable.[,,1]
  dt.trt = datatable.[,,2]

  data.frame(Treatment = trt_lev[1],
             new_var = factor(rownames(dt.control), levels = rownames(dt.control)),
             cbind(dt.control,
                   Total = rowSums(dt.control),
                   pos = 1:nrow(dt.control))) -> dt.c
  data.frame(Treatment = trt_lev[2],
             new_var = factor(rownames(datatable.), levels = rownames(datatable.)),
             cbind(dt.trt,
                   Total = rowSums(dt.trt),
                   pos = 1:nrow(dt.trt))) -> dt.t
  dt = rbind(dt.c, dt.t)

  max_freq = ceiling(max(dt$Total)/50)*50 # Found the maximum frequency and round up to the nearest 100
  y_int = seq(0, max_freq, by = seq_by)


  names(palette_colors) = y_lev
  ggplot(dt) +
    geom_hline(yintercept = y_int, colour = "grey80", size = 0.2) +
    geom_segment(aes(x = pos, xend = pos, y = 0, yend = max_freq), colour = "grey80", size = 0.2) +
    geom_bar(aes(x = pos, y = (Total), fill = y_lev[1]), color = "black", width = 1, stat = "identity") +
    geom_bar(aes(x = pos, y = (Yes),   fill = y_lev[2]), color = "black", width = 1, stat = "identity") +
    scale_y_continuous(name = lab.y, trans = "sqrt", limits = c(0, max_freq+100), breaks = y_int) +
    scale_fill_manual(values = palette_colors) +
    coord_polar(start = pi/4) +
    labs(title = title, x = NULL, y = lab.y, fill = strip) +
    theme_bw(base_size = 14) +
    theme(axis.text.x = element_blank(),
          plot.margin = unit(c(0,0,0,0),"cm"),
          legend.box.margin =  unit(c(0,0,0,0),"cm"),
          legend.margin = margin(-0.5,0,0,0, unit='cm'),
          panel.grid  = element_blank(),
          legend.position = "bottom",
          legend.direction = "horizontal") +
    geom_text(aes(x = pos, y = Total + 1,
                  label = new_var),
              hjust = 0,
              angle = 45 - 360/max(dt$pos)/2 - 360/max(dt$pos) * (dt$pos - 1)) +
    facet_grid(~Treatment)
}






#' Nightingale plot with treatment effects
#'
#' This function produces a nightingale plot with treatment effects across subgroups
#'
#' @param dat              a data set
#' @param covari.sel       a vector of indices of the two covariates
#' @param trt.sel          a covariate index specifying the treatment code
#' @param resp.sel         a covariate index specifying the response variable
#' @param outcome.type     a string specifying the type of the response variable, it can be "continuous", or "binary" or  "survival".
#' @param seq_by           a number specifying the breaks in the y-axis
#' @param range.strip  a vector with two elements specifying the range of treatment effect size for display
#' @param n.brk        a number specifying the number of the points dividing the range of the argument "range.strip".
#' @param n.brk.axis   a number specifying the number of breakpoints dividing the axis of the argument "range.strip".
#' @param title            a string specifying the main title.
#' @param strip        a string specifying the title of the colour strip.
#' @param effect       either "HR" or "RMST". only when outcome.type = "survival"
#' @param time             time for calculating the RMST
#' @param show.overall A logical indicating whether to show the overall treatment effect and its confidence intervals in the reference strip
#' @param lab.y            a character value specifying the label of the y axis
#' @param palette      either "divergent" or "hcl"
#' @param col.power    to be used when palette = "hcl". see colorspace package for reference
#'
#' @examples
#' data(prca)
#' dat <- prca
#' vars <- data.frame(variable = names(dat), index = 1:length(names(dat)))
#' levels(dat$age_group) <- c("Young", "Middle-aged", "Old")
#' levels(dat$weight_group) <- c("Low", "Mid", "High")
#' names(dat)[c(14,15)] <- c("Age", "Weight")
#'
#' strip.title = "Treatment effect size (log hazard ratio)"
#' plot_nightingale_effect(dat,
#'                         covari.sel = c(14,15),
#'                         trt.sel = 3,
#'                         resp.sel = c(1, 2),
#'                         outcome.type = "survival",
#'                         range.strip=c(-3, 3),
#'                         n.brk = 31,
#'                         n.brk.axis =  7,
#'                         title = "Total sample size = 475",
#'                         strip = strip.title, effect = "HR",
#'                         show.overall = TRUE, palette = "hcl")
#'
#' @export
#' @import grid
#' @import ggplot2
plot_nightingale_effect <- function(dat, covari.sel, trt.sel, resp.sel, outcome.type,
                                    seq_by = 100,
                                    range.strip=c(-6, 6),
                                    n.brk = 30, n.brk.axis = NULL,
                                    title = NULL, strip = NULL,
                                    effect = c("HR","RMST"),
                                    time = NULL,
                                    show.overall = FALSE, lab.y = NULL,
                                    palette = "hcl", col.power = 0.5){
  ## 0. argument validity check  ###############################################
  effect = match.arg(effect)
  if(n.brk%%2 == 0) n.brk = n.brk+1
  if(is.null(n.brk.axis)) n.brk.axis = n.brk

  if (missing(dat)) stop("Data have not been inputed!")
  if (!(is.data.frame(dat))) stop("The data set is not with a data frame!")

  if (missing(covari.sel)) stop("The variables for defining subgroups have not been specified!")
  if (!(is.numeric(covari.sel))) stop("The variables for defining subgroups are not numeric!")
  for (i in 1 : length(covari.sel)) if (!(is.factor(dat[,covari.sel[i]]))) stop("The variables for defining subgroups are not categorical!")
  if (length(covari.sel) > 2) stop("This function only considers 2 covariates at most for defining subgroups!")

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

  ## 1. create subgroup data  ##################################################
  lab.vars = names(dat)[covari.sel]                 # set the names of the covariates which relates to the defined subgroup; if a covariate
  # are considered for multiple times, we make their name identical. (otherwise, the resulsting
  # names are like var.1, var.2 and so on.)
  names(dat)[trt.sel] = "trt"                 # rename the variable for treatment code
  if (outcome.type == "continuous"){
    names(dat)[resp.sel] = "resp"             # rename the response variable
  }else if (outcome.type == "binary"){
    names(dat)[resp.sel] = "resp"             # rename the response variable
  }else if (outcome.type == "survival"){
    names(dat)[resp.sel[1]] = "time"          # rename the response variable for survival time
    names(dat)[resp.sel[2]] = "status"        # rename the response variable for survival right censoring status
  }


  ## Calculate overall Treatment effect ----------------------------------------
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


  for (i in 1: length(covari.sel)){
    cond = covari.sel == covari.sel[[i]]
    lab.vars[cond] = rep(lab.vars[i], length(which(cond == TRUE)))
  }
  cats.var1 = names(table(dat[,covari.sel[1]]))     # the names of categories of the selected first cavariate
  cats.var2 = names(table(dat[,covari.sel[2]]))     # the names of categories of the selected second cavariate
  n.subgrp.var1 = dim(table(dat[,covari.sel[1]]))   # the number of levels for the first covariate (placed in the bottom of the figure)
  n.subgrp.var2 = dim(table(dat[,covari.sel[2]]))   # the number of levels for the second covariate (placed in the left of the figure)
  n.subgrp.tol = n.subgrp.var1 * n.subgrp.var2      # the total number of subgroups

  idx1 = list()
  idx2 = list()
  for (i in 1 : n.subgrp.var1 ) idx1[[i]] = which((dat[, covari.sel[1]] == cats.var1[i])  == T )
  for (i in 1 : n.subgrp.var2 ) idx2[[i]] = which((dat[, covari.sel[2]] == cats.var2[i])  == T )

  idx.subgrp = list()                               # the index set of the subgroups
  data.subgrp = list()                              # the data set of the subgroups
  data.mar.subgrp1 = list()                         # the data set of the mariginal subgroups (for variance 1, listed columnwisely)
  data.mar.subgrp2 = list()                         # the data set of the mariginal subgroups (for variance 2, listed rowwisely)
  ss.subgrp = matrix(0, nrow = n.subgrp.var2, ncol =n.subgrp.var1)  # the data set of the mariginal subgroups
  for (j in 1 : n.subgrp.var1){
    for (i in 1 : n.subgrp.var2){
      k = i + (j - 1) * n.subgrp.var2
      idx.subgrp[[k]] =  intersect(idx1[[j]], idx2[[i]])
      data.subgrp[[k]] =  dat[idx.subgrp[[k]], ]
      ss.subgrp[i, j] = dim(data.subgrp[[k]])[1]

      data.mar.subgrp2[[i]] = dat[idx2[[i]], ]
    }
    data.mar.subgrp1[[j]] = dat[idx1[[j]], ]
  }

  treatment.mean = matrix(rep(0, n.subgrp.tol), nrow = n.subgrp.var2, ncol = n.subgrp.var1)
  treatment.mean.mar = matrix(rep(0, max(n.subgrp.var1, n.subgrp.var2)*2 ), nrow = max(n.subgrp.var1, n.subgrp.var2), ncol = 2)
  for (j in 1 : n.subgrp.var1){
    for (i in 1 : n.subgrp.var2){
      k = i + (j - 1) * n.subgrp.var2
      if (sum((data.subgrp[[k]]$trt == "1")) == 0 | sum((data.subgrp[[k]]$trt == "0")) == 0){
        treatment.mean[i,j] = NA
      }else{
        if (outcome.type == "continuous"){
          model.int = lm(resp ~ trt,  data = data.subgrp[[k]])
          model.sum = summary(model.int)
          treatment.mean[i,j] = model.sum$coefficients[2, 1]
        }else if (outcome.type == "binary"){
          model.int = glm(resp ~ trt, family = "binomial", data = data.subgrp[[k]])
          model.sum = summary(model.int)
          treatment.mean[i,j] = model.sum$coefficients[2, 1]
        }else if (outcome.type == "survival"){
          if (effect == "HR"){
            model.int = survival::coxph(survival::Surv(time, status) ~ trt, data = data.subgrp[[k]])
            model.sum = summary(model.int)
            treatment.mean[i,j] = model.sum$coef[1, 1]
          }
          if (effect == "RMST"){
            dat.subgr.i = data.subgrp[[k]]
            rmst = survRM2::rmst2(time = dat.subgr.i$time, status = dat.subgr.i$status,
                                  arm = dat.subgr.i$trt, tau = time)
            treatment.mean[i,j] = rmst$unadjusted.result[1,1]
          }
        }
      }

      if (sum((data.mar.subgrp2[[i]]$trt == "1")) == 0 | sum((data.mar.subgrp2[[i]]$trt == "0")) == 0){
        treatment.mean.mar[i,2] = NA
      }else{
        if (outcome.type == "continuous"){
          model.int = lm(resp ~ trt,  data = data.mar.subgrp2[[i]])
          model.sum = summary(model.int)
          treatment.mean.mar[i,2] = model.sum$coefficients[2, 1]
        }else if (outcome.type == "binary"){
          model.int = glm(resp ~ trt,  family = "binomial", data = data.mar.subgrp2[[i]])
          model.sum = summary(model.int)
          treatment.mean.mar[i,2] = model.sum$coefficients[2, 1]
        }else if (outcome.type == "survival"){
          if (effect == "HR"){
            model.int = survival::coxph(survival::Surv(time, status) ~ trt, data = data.mar.subgrp2[[i]])
            model.sum = summary(model.int)
            treatment.mean.mar[i,2] = model.sum$coef[1, 1]
          }
          if (effect == "RMST"){
            dat.subgr.i = data.mar.subgrp2[[i]]
            rmst = survRM2::rmst2(time = dat.subgr.i$time, status = dat.subgr.i$status,
                                  arm = dat.subgr.i$trt, tau = time)
            treatment.mean.mar[i,2] = rmst$unadjusted.result[1,1]
          }
        }
      }
    }

    if (sum((data.mar.subgrp1[[j]]$trt == "1")) == 0 | sum((data.mar.subgrp1[[j]]$trt == "0")) == 0){
      treatment.mean.mar[j,1] = NA
    }else{
      if (outcome.type == "continuous"){
        model.int = lm(resp ~ trt,  data = data.mar.subgrp1[[j]])
        model.sum = summary(model.int)
        treatment.mean.mar[j,1] = model.sum$coefficients[2, 1]
      }else if (outcome.type == "binary"){
        model.int = glm(resp ~ trt,  family = "binomial", data = data.mar.subgrp1[[j]])
        model.sum = summary(model.int)
        treatment.mean.mar[j,1] = model.sum$coefficients[2, 1]
      }else if (outcome.type == "survival"){
        if (effect == "HR"){
          model.int = survival::coxph(survival::Surv(time, status) ~ trt, data = data.mar.subgrp1[[j]])
          model.sum = summary(model.int)
          treatment.mean.mar[j,1] = model.sum$coef[1, 1]
        }
        if (effect == "RMST"){
          dat.subgr.i = data.mar.subgrp1[[j]]
          rmst = survRM2::rmst2(time = dat.subgr.i$time, status = dat.subgr.i$status,
                                arm = dat.subgr.i$trt, tau = time)
          treatment.mean.mar[j,1] = rmst$unadjusted.result[1,1]
        }
      }
    }
  }

  lab.subgrp.col = cats.var1
  lab.subgrp.row = cats.var2
  lab.level = rep(0, max(n.subgrp.var1, n.subgrp.var2))
  for (i in 1 : max(n.subgrp.var1, n.subgrp.var2)) lab.level[i] = paste("Level", i, sep ="")

  dimnames(treatment.mean) = list(lab.subgrp.row, lab.subgrp.col )
  dimnames(treatment.mean.mar) = list(lab.level, lab.vars )
  dimnames(ss.subgrp) = list(lab.subgrp.row, lab.subgrp.col )

  cat("The minimum of treatment effect sizes is", c(min(treatment.mean, treatment.mean.mar)), "\n")
  cat("The maximum of treatment effect sizes is", c(max(treatment.mean, treatment.mean.mar)), "\n")

  ## 2. produce a graph  #######################################################
  if(length(covari.sel) == 1) {
    new_var = dat[,covari.sel]
  } else {
    new_var = apply(dat[,covari.sel, FALSE], 1, paste0, collapse = " - ")
  }

  table(new_var)-> datatable.

  new_var = apply(expand.grid(rownames(treatment.mean), colnames(treatment.mean))[,2:1],
                  1, paste0, collapse = " - ")
  trt_effect = as.vector(treatment.mean)

  merge(data.frame(new_var, trt_effect, pos = 1:9), datatable.) -> dt
  dt$new_var = factor(dt$new_var, levels = new_var)

  max_freq = ceiling(max(dt$Freq)/100)*100 # Found the maximum frequency and round up to the nearest 100
  y_int = seq(0, max_freq, by = seq_by)


  # names(palette_colors) = y_lev

  breaks = seq(min(range.strip) - 1e-8, max(range.strip) + 1e-8, length.out = n.brk)
  if (palette == "hcl"){
    col.vec = colorspace::diverge_hcl(n = length(breaks)-1,
                                      c = 100, l = c(50,90),
                                      power = col.power)
    if (!(outcome.type == "survival" & effect == "HR")) col.vec = rev(col.vec)
  }

  #### draw plot using ggplot2 ##############################################
  ggplot(dt) +
    geom_hline(yintercept = y_int, colour = "grey80", size = 0.2) +
    geom_segment(aes(x = pos, xend = pos, y = 0, yend = max_freq), colour = "grey80", size = 0.2) +
    geom_bar(aes(x = pos, y = (Freq), fill = trt_effect), color = "black", width = 1, stat = "identity") +
    scale_y_continuous(name = lab.y, trans = "sqrt", limits = c(0, max_freq+100), breaks = y_int) +
    scale_fill_gradientn(colours = col.vec, limits=range.strip,
                         guide = guide_colourbar(title.position = "right",
                                                 ticks = TRUE,
                                                 barheight = 17,
                                                 ticks.colour = "black",
                                                 label.position = "left"
                                                  )) +
    coord_polar(start = pi/4) +
    labs(title = title, x = NULL, y = lab.y, fill = strip) +
    theme_bw(base_size = 14) +
    theme(axis.text.x = element_blank(),
          plot.margin = unit(c(0,0,0.1,0),"cm"),
          legend.title = element_text(angle = 90),
          legend.box.margin =  unit(c(0,0,0,0),"cm"),
          legend.margin = margin(0,0,0,0, unit='cm'),
          panel.grid  = element_blank(),
          legend.position = "none") +
          # legend.direction = "horizontal") +
    geom_text(aes(x = pos, y = Freq + 1,
                  label = new_var),
              hjust = 0,
              angle = 45 - 360/max(dt$pos)/2 - 360/max(dt$pos) * (dt$pos - 1)) -> plot_main

  overall = data.frame(estimate = overall.treatment.mean,
                       ll = overall.treatment.lower,
                       ul = overall.treatment.upper)
  size. = breaks[2] - breaks[1]
  ggplot() +
    geom_tile(data = data.frame(y = breaks[-length(breaks)] + size./2, col.vec),
              aes(x = 1, y = y),
              fill = col.vec, height = 1*size.) -> ppl
  if(show.overall) {
    ppl = ppl +
      geom_point(data = overall, aes(x = 1, y = estimate)) +
      geom_errorbar(data = overall, aes(x = 1, ymin = ll, ymax = ul), width = 0.25)
  }
  ppl = ppl +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(name = NULL,
                       limits = range.strip+c(-.01, 0.01),
                       expand = c(0,0),
                       breaks = seq(min(range.strip), max(range.strip), len = n.brk.axis),
                       sec.axis = sec_axis(~., name = strip)) +
    theme_classic(base_size = 14) +
    theme(axis.line.x = element_blank(),
          axis.line.y.right = element_blank(),
          axis.ticks.y.right = element_blank(),
          axis.title.y.right= element_text(angle = 90),
          axis.text.y.left= element_text(size = 14, angle = 90, color = "black", hjust = .5),
          axis.text.y.right = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank()) -> plot_legend
  pm = ggplotGrob(plot_main)
  pl = ggplotGrob(plot_legend)
  pl$heights <- pm$heights
  grid.arrange(pm,pl,ncol = 2,newpage = TRUE, widths = c(5,1))
}
