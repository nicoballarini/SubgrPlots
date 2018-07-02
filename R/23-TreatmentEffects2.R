## Create data for boxplots of all intersections
IntersectionEffectPlot2 <- function(data1, data2, start_col, names, att, outcome.type){
  end_col <- ((start_col + length(names)) - 1)
  data.all <- data2
  #tagging because x axis values need to be 1:number of sets so they line up with their intersections
  data2$tag <- 1:nrow(data2)
  sets <- list()
  intersections <- list()
  effect_plot_data <- data.frame()
  for (i in 1:nrow(data1)){
    values = data1[i, 1:length(names)]
    sel.vars = which(values!=2)
    not.sel.vars = setdiff(1:length(names), sel.vars)
    if (length(sel.vars)==0) {
      intersections[[i]] = data2
      intersections[[i]]$x <- i
      next()
    }
    names[sel.vars]
    names[not.sel.vars]
    values[sel.vars]
    sel.subjects = apply(data2[names[sel.vars]],
                         MARGIN = 1,
                         function(x) all(x == values[sel.vars]))
    intersections[[i]] = data2[sel.subjects, ]
    intersections[[i]]$x <- i
  }

  for(i in 1:length(intersections)){
    effect_plot_data <- rbind(effect_plot_data, intersections[[i]])
  }

  # Calculate treatment effects
  if (outcome.type == "continuous"){
    col <- match(att[1], colnames(effect_plot_data))
    colnames(effect_plot_data)[col] <- "attribute"
    col <- match(att[1], colnames(data.all))
    colnames(data.all)[col] <- "attribute"
    model.sum = summary(lm(attribute ~ trt, data.all))

    effdat = do.call(rbind,
                     lapply(unique(effect_plot_data$x), function(i){
                       if (length(unique(effect_plot_data[which(effect_plot_data$x==i), "trt"]))==2){
                         model.sum = summary(lm(attribute ~ trt, effect_plot_data[which(effect_plot_data$x==i), ]))
                         data.frame(mean  = model.sum$coefficients[2, 1],
                                    lower = model.sum$coefficients[2, 1] - 1.96 * model.sum$coefficients[2, 2],
                                    upper = model.sum$coefficients[2, 1] + 1.96 * model.sum$coefficients[2, 2],
                                    x = i)
                       } else {
                         data.frame(mean  = NA,
                                    lower = NA,
                                    upper = NA,
                                    x = i)
                       }
                     })
    )
  } else if (outcome.type == "binary"){
    col <- match(att[1], colnames(effect_plot_data))
    colnames(effect_plot_data)[col] <- "attribute"
    col <- match(att[1], colnames(data.all))
    colnames(data.all)[col] <- "attribute"


    model.sum = summary(glm(attribute ~ trt,
                            family = "binomial", data.all))
    effdat = do.call(rbind,
                     lapply(unique(effect_plot_data$x), function(i){
                       if (length(unique(effect_plot_data[which(effect_plot_data$x==i), "trt"]))==2){
                         model.sum = summary(glm(attribute ~ trt,
                                                 family = "binomial",
                                                 effect_plot_data[which(effect_plot_data$x==i), ]))
                         data.frame(mean  = model.sum$coefficients[2, 1],
                                    lower = model.sum$coefficients[2, 1] - 1.96 * model.sum$coefficients[2, 2],
                                    upper = model.sum$coefficients[2, 1] + 1.96 * model.sum$coefficients[2, 2],
                                    x = i)
                       } else {
                         data.frame(mean  = NA,
                                    lower = NA,
                                    upper = NA,
                                    x = i)
                       }
                     })
    )
  } else if (outcome.type == "survival"){
    time.ind   <- match(att[1], colnames(effect_plot_data))
    status.ind <- match(att[2], colnames(effect_plot_data))
    colnames(effect_plot_data)[time.ind] <- "time"
    colnames(effect_plot_data)[status.ind] <- "status"
    time.ind   <- match(att[1], colnames(data.all))
    status.ind <- match(att[2], colnames(data.all))
    colnames(data.all)[time.ind] <- "time"
    colnames(data.all)[status.ind] <- "status"

    model.sum = summary(survival::coxph(survival::Surv(time, status) ~ trt,
                              data.all))

    effdat = do.call(rbind,
                     lapply(unique(effect_plot_data$x), function(i){
                       if (length(unique(effect_plot_data[which(effect_plot_data$x==i), "trt"]))==2){
                         model.sum = summary(survival::coxph(survival::Surv(time, status) ~ trt,
                                                   effect_plot_data[which(effect_plot_data$x==i), ]))
                         data.frame(mean  = model.sum$coef[1, 1],
                                    upper = model.sum$coef[1, 1] + 1.96 * model.sum$coef[1, 3],
                                    lower = model.sum$coef[1, 1] - 1.96 * model.sum$coef[1, 3],
                                    x = i)
                       } else {
                         data.frame(mean  = NA,
                                    lower = NA,
                                    upper = NA,
                                    x = i)
                       }
                     }))

  }
  return(effdat)
}

## Generate boxplot summary plots
EffectPlotsPlot <- function(effdat, att, att_color, outcome.type,  text_scale=1){
  if (outcome.type == "continuous"){
    yaxis <- "Treatment effect"
  } else if (outcome.type == "binary"){
    yaxis <- "Treatment effect\n(log odds ratio)"
  } else if (outcome.type == "survival"){
    yaxis <- "Treatment effect\n(log hazard ratio)"
  }

  upper_xlim <- as.numeric((max(effdat$x) + 1))
  plot_lims <- as.numeric(0:upper_xlim)
  effdat$x <- as.factor(effdat$x)

  ggobj = ggplot() +
    theme_bw() + ylab(yaxis) +
    scale_x_discrete(limits = plot_lims, expand = c(0,0)) +
    theme(plot.margin = unit(c(-0.7,0,0,0), "cm"),
          legend.position = "none", legend.justification = c(0,0),
          axis.title.y = element_text(vjust = -0.8),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.title.x = element_blank()) +
    geom_hline(yintercept = 0, colour = "gray50", linetype = 2) +
    geom_hline(yintercept = effdat$mean[1], colour = "gray50", linetype = 1) +
    geom_point(data = effdat, aes_string(x="x", y="mean"),
               colour = "gray20",
               shape = 15) +
    geom_errorbar(data = effdat, aes_string(x="x",
                                            ymax = "upper",
                                            ymin = "lower"),
                  width = 0.1,
                  colour = "gray20")+
    labs(color = NULL)

  effects <- suppressWarnings(ggplotGrob(ggobj))
  return(effects)
}

## Generate boxplot summary plots
#' @import ggplot2
EffectPlotsPlot_t <- function(effdat, att, att_color, outcome.type,  text_scale){
  if (outcome.type == "continuous"){
    yaxis <- "Treatment Effect"
  } else if (outcome.type == "binary"){
    yaxis <- "Treatment Effect (log odds ratio)"
  } else if (outcome.type == "survival"){
    yaxis <- "Treatment Effect (log hazard ratio)"
  }
  if(length(text_scale) == 1){
    name_size_scale <- text_scale
  }
  if(length(text_scale) > 1 && length(text_scale) <= 6){
    name_size_scale <- text_scale[5]
  }
  if(length(text_scale) > 1 && length(text_scale) <= 6){
    y_axis_title_scale <- text_scale[1]
    y_axis_tick_label_scale <- text_scale[2]
    intersection_size_number_scale <- text_scale[6]
  }
  else{
    y_axis_title_scale <- text_scale
    y_axis_tick_label_scale <- text_scale
    intersection_size_number_scale <- text_scale
  }
  bottom_margin <- (-1)*0.65
  upper_xlim <- as.numeric((max(effdat$x) + 1))
  plot_lims <- as.numeric(0:upper_xlim)
  effdat$x = max(effdat$x) - effdat$x + 1
  effdat$x <- as.factor(effdat$x)
  ymax = ceiling(max(effdat[, 1:3]))
  ymin = floor(min(effdat[, 1:3]))
  ggobj = ggplot() +
    theme_bw() + ylab(yaxis) +
    scale_x_discrete(limits = plot_lims, expand = c(0,0)) +
    scale_y_continuous(limits = c(ymin, ymax), position = "right", expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),
          legend.position = "none",
          axis.line.x = element_line(),
          axis.ticks.y = element_blank(),
          axis.text.y  = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(0.5, 0.5, 0.5,0.5), "lines"),
          axis.title.x.top = element_text(vjust =  0,   size = 8.3*y_axis_title_scale),
          axis.text.x.top  = element_text(vjust =  0.3, colour = "gray0", size = 7*y_axis_tick_label_scale),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()) +
    coord_flip() +
    geom_hline(yintercept = 0, colour = "gray50", linetype = 2) +
    geom_hline(yintercept = effdat$mean[1], colour = "gray50", linetype = 1) +
    geom_point(data = effdat, aes_string(x="x", y="mean"), colour = "gray20", shape = 15) +
    geom_errorbar(data = effdat, aes_string(x="x", ymax = "upper", ymin = "lower"),
                  width = 0.1, colour = "gray20") +
    labs(color = NULL)

  effects <- suppressWarnings(ggplotGrob(ggobj))
  return(effects)
}
