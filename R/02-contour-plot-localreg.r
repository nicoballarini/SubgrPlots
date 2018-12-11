#' Contour plot for effect size via local regression.
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
#' @param outcome.type   a string specifying the type of the response variable,
#'  it can be "continuous", or "binary" or  "survival".
#' @param setup.ss       a vector specifying approximate subgroup sample size
#' and neibourghing subgroup overlap sample size. The first and the second elements
#' are for overlap sizes and subgroup sample sizes over the first covariate;
#' the third and thefourth are for further divided overlap sizes
#' and subgroup sample sizes over the second covariate.
#' @param n.grid         a vector specifying the numbers of the grid points on
#'  the x-axis and the y-axis respectively.
#' @param brk.es         a vector specifying the break points on effect size,
#' where each range partition is given with a different colour on points.
#' @param n.brk.axis   a number specifying the number of breakpoints dividing the axis of the argument "range.strip".
#' @param para.plot      a vector specifying the parameters of the contour plot;
#'  the first value is for controlling the degree of smoothing; the second
#'                  is for controlling the degree of the polynomials fitting to
#'                  be used (normally 1 or 2); the third is for controlling the
#'                  number of contour lines.
#' @param font.size      a vector specifying the size of labels and text; the
#'  first element is for the main title, the second is for for x-axis and y-axis
#'  labels; the third is for the subtitle; the fourth is for the text in the
#'  legend; the fifth is for the labels on contour lines.
#' @param title          a string specifying the main title.
#' @param subtitle       strings specifying the subtitle
#' @param unit.x step for the x variable to create the grid that will center the kernel to apply the local regression
#' @param unit.y step for the x variable to create the grid that will center the kernel to apply the local regression
#' @param effect           either "HR" or "RMST". only when outcome.type = "survival"
#' @param show.overall     logical. whether to show or not the overall treatment effect in the strip
#' @param strip        a string specifying the title of the colour strip.
#' @examples
#' \donttest{
#' library(dplyr)
#'
#' # Load the data to be used
#' data(prca)
#' dat <- prca
#' dat %>%
#'   rename(Weight = weight,
#'          Age = age) -> dat
#'
#' plot_contour_localreg(dat,
#'                       covari.sel = c(8,9),
#'                       trt.sel = 3,
#'                       resp.sel = c(1,2),
#'                       n.grid = c(100,100),
#'                       font.size = c(1, 1, 1, 1, 1),
#'                       brk.es = seq(-4.5,4.5,length.out = 101),
#'                       n.brk.axis =  7,
#'                       strip = "Treatment effect size (log hazard ratio)",
#'                       outcome.type = "survival")
#'}
#' @export
plot_contour_localreg <- function(dat, covari.sel, trt.sel, resp.sel, outcome.type,
                                  setup.ss, n.grid = c(41, 41),
                                  brk.es = c(0, 1, 2, 3),
                                  n.brk.axis = 3,
                                  para.plot = c(0.35, 2, 20),
                                  font.size = c(1.5,1.2,1,0.85,0.8),
                                  title = NULL, subtitle = NULL,
                                  unit.x = 1, unit.y = 1,
                                  effect = "HR", show.overall = TRUE,
                                  strip = "Effect Size") {
  old.par <- par(no.readonly=T)

  ## 1. create subgroup data  ##################################################
  names(dat)[trt.sel] = "trt"                            # rename the variable for treatment code
  if (outcome.type == "continuous"){
    names(dat)[resp.sel] = "resp"                        # rename the response variable
  }else if (outcome.type == "binary"){
    names(dat)[resp.sel] = "resp"                        # rename the response variable
  }else if (outcome.type == "survival"){
    names(dat)[resp.sel[1]] = "time"                     # rename the response variable for survival time
    names(dat)[resp.sel[2]] = "status"                   # rename the response variable for survival right censoring status
  }

  x.lim = range(dat[covari.sel[1]])
  y.lim = range(dat[covari.sel[2]])
  grid.pts.x = seq(x.lim[1], x.lim[2], unit.x)
  grid.pts.y = seq(y.lim[1], y.lim[2], unit.y)
  grid.xy = expand.grid(grid.pts.x, grid.pts.y)
  alpha = 0.8
  n.cutoff = 20

  # Now do a bivariate local regression ####
  results.xy = do.call(rbind, lapply(1:nrow(grid.xy), FUN = function(i){
    center.x = grid.xy[i, 1]
    center.y = grid.xy[i, 2]
    var = 1/(1-alpha)^2
    x.conf.up = center.x + 2 * sqrt(var)
    weight.up = exp(-mahalanobis(x = data.frame(x.conf.up, center.y),
                                 center = c(center.x, center.y),
                                 cov    = matrix(c(var,0,0,var), nrow = 2))/2)
    weight. = exp(-mahalanobis(x = data.frame(dat[covari.sel[1]],
                                              dat[covari.sel[2]]),
                               center = c(center.x, center.y),
                               cov    = matrix(c(var,0,0,var), nrow = 2))/2)

    if(sum(weight.>weight.up)>n.cutoff){
      sum((weight.>0.01))
      cox.result = survival::coxph(survival::Surv(time, status) ~ trt,
                         data = dat,
                         weights = weight.)
      summary(cox.result)
      data.frame(x = center.x, y = center.y, estimate = cox.result$coefficients)
    }else{
      data.frame(x = center.x, y = center.y, estimate = NA)
    }
  }))

  z.matrix = matrix(results.xy$estimate,
                    nrow = length(grid.pts.x),
                    ncol = length(grid.pts.y), byrow = F)
  rownames(z.matrix) = grid.pts.x
  colnames(z.matrix) = grid.pts.y

  lab.vars = names(dat)[covari.sel]

  col.power = 0.75
  col.vec = rev(colorspace::diverge_hcl(n = length(brk.es)-1,
                                        c = 100, l = c(50,90),
                                        power = col.power))

  ### Create a nice plot -------------------------------------------------------
  if (!(outcome.type == "survival" & effect == "HR")) col.vec = rev(col.vec)
  cols = col.vec
  layout(matrix(c(1, 2), nrow=1, ncol=2), widths=c(4,1))
  if (is.null(title)){
    par(mar=c(4,4,2,1))
  } else{
    par(mar=c(4,4,4,1))
  }
  plot(grid.xy[,1], grid.xy[,2], type = "n",
       xlim = range(grid.pts.x),
       ylim = range(grid.pts.y),
       xlab = lab.vars[1], ylab = lab.vars[2],
       main = title,
       col  = "gray80",
       cex.main = font.size[1],
       cex.lab  = font.size[2],
       cex.axis = font.size[2],
       cex.sub  = font.size[3])
  breaks = seq(min(brk.es),max(brk.es),      length.out = length(cols)+1)
  breaks.axis = seq(min(brk.es),max(brk.es), length.out = n.brk.axis)
  .filled.contour(grid.pts.x, grid.pts.y, z.matrix,
                  levels = breaks,
                  col = rev(cols))
  points(dat[, covari.sel[1]], dat[, covari.sel[2]], cex = 0.5)
  if (is.null(title)){
    par(mar=c(4,2,2,2.5))
  } else{
    par(mar=c(4,2,4,2.5))
  }
  image.scale(brk.es,
              col= rev(cols),
              breaks = breaks,
              axis.pos = 4, add.axis = FALSE)
  axis(2, at = breaks.axis, labels = round(breaks.axis, 3),
       las = 0, cex.axis = font.size[5])
  mtext(strip, side=4, line=1, cex.lab = font.size[5])

  # Calculate overall Treatment effect -----------------------------------------
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
  if(show.overall){
    cat("Overall Treatment effect is:",
        overall.treatment.mean, ", with confidence interval: (",
        overall.treatment.lower,";",overall.treatment.upper,")\n")
    points(x = 0.5,
           (overall.treatment.mean), pch = 20)
    points(x = 0.5, overall.treatment.lower, pch = "-")
    points(x = 0.5, overall.treatment.upper, pch = "-")
    segments(x0 = 0.5, x1 = 0.5,
             y0 = overall.treatment.lower,
             y1 = overall.treatment.upper)
  }
  par(old.par)

}

