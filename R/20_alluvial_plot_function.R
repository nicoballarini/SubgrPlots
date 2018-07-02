#' Alluvial plot
#'
#' this function produces an alluvial diagram. This function is a copy of alluvial::alluvial
#' but a 'rotate' option is added to rotate the labels.
#'
#' @inheritParams alluvial::alluvial
#' @param cex.axis cex value to be passed to the axis. See help(par)
#' @param rotate angle to rotate the labels. This argument is passed as an \code{srt} argument
#' @param las direction of the labels of the covariates. 1 is horizontal, 2 is vertical.
#' @param bottom.mar bottom margin to be passed as a first argument to \code{mar()}
#'
#' @examples
#' library(dplyr)
#'
#' # Alluvial plot
#' # Load the data to be used
#' data(prca)
#' dat <- prca
#' dat$trt = dat$rx
#' dat %>%
#'   dplyr::select(trt, bm, hx, pf) %>%
#'   dplyr::group_by(trt, bm, hx, pf) %>%
#'   dplyr::summarise(Freq = n()) -> alldat
#' alldat %>%
#'   ungroup() %>%
#'   mutate(trt = ifelse(trt == 0 , "Control", "Treatment"),
#'          bm = ifelse(bm == 0 , "No", "Yes"),
#'          hx = ifelse(hx == 0 , "No", "Yes"),
#'          pf = ifelse(pf == 0 , "No", "Yes"))-> alldat
#'
#' plot_alluvial(alldat[,c(1,3,2,4)], freq = alldat$Freq,
#'               xw=0.2,cw = 0.12,cex = 1,
#'               alpha  = 0.8,
#'               col=ifelse(alldat$trt == "Treatment","#1f78b4", "#a6cee3"),
#'               layer = alldat$trt  == 1, rotate = 90)
#'
#'
#' # Alluvial plot using survival rate at 24 months
#' data(prca)
#' dat <- prca
#' dat %>%
#'   mutate(survival = factor(ifelse(survtime > 24 , "Yes", "No"), levels = c("No", "Yes")),
#'          trt = rx) -> dat
#' dat %>%
#'   dplyr::select(trt, bm, hx, pf, survival) %>%
#'   dplyr::group_by(trt, bm, hx, pf, survival) %>%
#'   dplyr::summarise(Freq = n()) -> alldat
#' alldat %>%
#'   ungroup() %>%
#'   mutate(trt = ifelse(trt == 0 , "Control", "Treatment"),
#'          bm = ifelse(bm == 0 , "No", "Yes"),
#'          hx = ifelse(hx == 0 , "No", "Yes")) -> alldat
#'
#' plot_alluvial(alldat[,c(5,1,3,2,4)], freq = alldat$Freq,
#'               xw=0.2,cw = 0.12,cex = 1,
#'               alpha  = 0.8,
#'               col=ifelse(alldat$survival  == "Yes",
#'                          ifelse(alldat$trt  == "Treatment","#80b1d3","#d5e2eb"),
#'                          ifelse(alldat$trt  == "Treatment","#faa8d2","#fbe0ee")),
#'               layer = alldat$trt  == 1, rotate = 90, las = 2, bottom.mar = 5)
#'
#' @export
plot_alluvial <- function (..., freq, col = "gray", border = 0, layer, hide = FALSE,
          alpha = 0.5, gap.width = 0.05, xw = 0.1, cw = 0.1, blocks = TRUE,
          ordering = NULL, axis_labels = NULL, cex = par("cex"), cex.axis = par("cex.axis"),
          rotate = 0, las = 1, bottom.mar = 2){
  old.par <- par(no.readonly=T)

  p <- data.frame(..., freq = freq, col, alpha, border, hide,
                  stringsAsFactors = FALSE)
  np <- ncol(p) - 5
  if (!is.null(ordering)) {
    stopifnot(is.list(ordering))
    if (length(ordering) != np)
      stop("'ordering' argument should have ", np, " components, has ",
           length(ordering))
  }
  n <- nrow(p)
  if (missing(layer)) {
    layer <- 1:n
  }
  p$layer <- layer
  d <- p[, 1:np, drop = FALSE]
  p <- p[, -c(1:np), drop = FALSE]
  p$freq <- with(p, freq/sum(freq))
  col <- col2rgb(p$col, alpha = TRUE)
  if (!identical(alpha, FALSE)) {
    col["alpha", ] <- p$alpha * 256
  }
  p$col <- apply(col, 2, function(x) do.call(rgb, c(as.list(x),
                                                    maxColorValue = 256)))
  isch <- sapply(d, is.character)
  d[isch] <- lapply(d[isch], as.factor)
  if (length(blocks) == 1) {
    blocks <- if (!is.na(as.logical(blocks))) {
      rep(blocks, np)
    }
    else if (blocks == "bookends") {
      c(TRUE, rep(FALSE, np - 2), TRUE)
    }
  }
  if (is.null(axis_labels)) {
    axis_labels <- names(d)
  }
  else {
    if (length(axis_labels) != ncol(d))
      stop("`axis_labels` should have length ", names(d),
           ", has ", length(axis_labels))
  }
  getp <- function(i, d, f, w = gap.width) {
    a <- c(i, (1:ncol(d))[-i])
    if (is.null(ordering[[i]])) {
      o <- do.call(order, d[a])
    }
    else {
      d2 <- d
      d2[1] <- ordering[[i]]
      o <- do.call(order, d2[a])
    }
    x <- c(0, cumsum(f[o])) * (1 - w)
    x <- cbind(x[-length(x)], x[-1])
    gap <- cumsum(c(0L, diff(as.numeric(d[o, i])) != 0))
    mx <- max(gap)
    if (mx == 0)
      mx <- 1
    gap <- gap/mx * w
    (x + gap)[order(o), ]
  }
  dd <- lapply(seq_along(d), getp, d = d, f = p$freq)
  rval <- list(endpoints = dd)
  op <- par(mar = c(bottom.mar, 1, 1, 1))
  plot(NULL, type = "n", xlim = c(1 - cw, np + cw), ylim = c(0,
                                                             1), xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i",
       xlab = "", ylab = "", frame = FALSE)
  ind <- which(!p$hide)[rev(order(p[!p$hide, ]$layer))]
  for (i in ind) {
    for (j in 1:(np - 1)) {
      xspline(c(j, j, j + xw, j + 1 - xw, j + 1, j + 1,
                j + 1 - xw, j + xw, j) + rep(c(cw, -cw, cw),
                                             c(3, 4, 2)), c(dd[[j]][i, c(1, 2, 2)], rev(dd[[j +
                                                                                              1]][i, c(1, 1, 2, 2)]), dd[[j]][i, c(1, 1)]),
              shape = c(0, 0, 1, 1, 0, 0, 1, 1, 0, 0), open = FALSE,
              col = p$col[i], border = p$border[i])
    }
  }
  for (j in seq_along(dd)) {
    ax <- lapply(split(dd[[j]], d[, j]), range)
    if (blocks[j]) {
      for (k in seq_along(ax)) {
        rect(j - cw, ax[[k]][1], j + cw, ax[[k]][2])
      }
    }
    else {
      for (i in ind) {
        x <- j + c(-1, 1) * cw
        y <- t(dd[[j]][c(i, i), ])
        w <- xw * (x[2] - x[1])
        xspline(x = c(x[1], x[1], x[1] + w, x[2] - w,
                      x[2], x[2], x[2] - w, x[1] + w, x[1]), y = c(y[c(1,
                                                                       2, 2), 1], y[c(2, 2, 1, 1), 2], y[c(1, 1),
                                                                                                         1]), shape = c(0, 0, 1, 1, 0, 0, 1, 1, 0,
                                                                                                                        0), open = FALSE, col = p$col[i], border = p$border[i])
      }
    }
    for (k in seq_along(ax)) {
      text(j, mean(ax[[k]]), labels = names(ax)[k], cex = cex, srt = rotate)
    }
  }
  axis(1, at = rep(c(-cw, cw), ncol(d)) + rep(seq_along(d),
                                              each = 2), line = 0.5, col = "white", col.ticks = "black",
       labels = FALSE)
  axis(1, at = seq_along(d), tick = FALSE, labels = axis_labels,
       cex.axis = cex.axis, las = las)

  par(old.par)
  invisible(rval)
}
