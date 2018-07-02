## Create data for boxplots of all intersections
IntersectionEffectPlot <- function(data1, data2, start_col, names, att, outcome.type){
  end_col <- ((start_col + length(names)) - 1)
  data.all <- data2
  data2 <- data2[which(rowSums(data2[ ,start_col:end_col]) != 0), ]
  #tagging because x axis values need to be 1:number of sets so they line up with their intersections
  data2$tag <- 1:nrow(data2)
  sets <- list()
  intersections <- list()
  effect_plot_data <- data.frame()
  for(i in 1:nrow(data1)){
    sets[[i]] <- colnames(data1)[which(data1[i, 1:length(names)] == 0)]
  }
  for(i in 1:length(sets)){
    intersections[[i]] <- data2[(rowSums(data2[ ,start_col:end_col]) == (length(names) - length(as.character(sets[[i]])))), ]
    intersections[[i]] <- Wanted(intersections[[i]], as.character(sets[[i]]))
    end <- ((start_col + (length(names) - length(as.character(sets[[i]]))))-1)
    if(start_col == end){
      intersections[[i]] <- intersections[[i]][(intersections[[i]][ ,start_col]) == 1, ]
      intersections[[i]] <- intersections[[i]]$tag
    }
    else{
      num <- length(names) - length(as.character(sets[[i]]))
      intersections[[i]] <- intersections[[i]][(rowSums(intersections[[i]][ ,start_col:end]) == num), ]
      intersections[[i]] <- intersections[[i]]$tag
    }
    intersections[[i]] <- data2[data2$tag %in% as.numeric(intersections[[i]]), ]
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
    overall.effect = data.frame(mean  = model.sum$coefficients[2, 1],
               lower = model.sum$coefficients[2, 1] - 1.96 * model.sum$coefficients[2, 2],
               upper = model.sum$coefficients[2, 1] + 1.96 * model.sum$coefficients[2, 2],
               x = 0)

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
    overall.effect = data.frame(mean  = model.sum$coefficients[2, 1],
               lower = model.sum$coefficients[2, 1] - 1.96 * model.sum$coefficients[2, 2],
               upper = model.sum$coefficients[2, 1] + 1.96 * model.sum$coefficients[2, 2],
               x = 0)
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
    overall.effect = data.frame(mean  = model.sum$coef[1, 1],
               upper = model.sum$coef[1, 1] + 1.96 * model.sum$coef[1, 3],
               lower = model.sum$coef[1, 1] - 1.96 * model.sum$coef[1, 3],
               x = 0)

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

  effdat = rbind(overall.effect, effdat)
  return(effdat)
}

## Generate boxplot summary plots
#' @import ggplot2
EffectPlotsPlot <- function(effdat, att, att_color, outcome.type){
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
    geom_hline(yintercept = effdat$x[1,], colour = "gray50", linetype = 1) +
    geom_point(data = effdat, aes_string(x="x", y="mean"),
               colour = "gray20",
               shape = 15) +
    geom_errorbar(data = effdat, aes_string(x="x",
                                            ymax = "upper",
                                            ymin = "lower"),
                  width = 0.1,
                  colour = "gray20")+
    geom_point(data = effdat[which(effdat$x==0),],
               aes_string(x="x", y="mean",
                          color = shQuote("Overall treatment effect")),
               shape = 15) +
    geom_errorbar(data = effdat[which(effdat$x==0),],
                  aes_string(x="x",
                             ymax = "upper",
                             ymin = "lower",
                             color = shQuote("Overall treatment effect")),
                  width = 0.1) +
    labs(color = NULL)

  effects <- suppressWarnings(ggplotGrob(ggobj))
  return(effects)
}
