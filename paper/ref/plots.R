library(sas7bdat)
library(parallel)
library(survival)
library(stats)
library(sampling)
library(ggplot2)
library(dplyr)

# Read the SAS dataset into R
prca0<-read.sas7bdat('adv_prostate_ca.sas7bdat')
head(prca0)
# Select the variables that we use for the analysis
prca <- prca0[,c("SURVTIME","CENS","RX","BM","HX","STAGE","PF", "AGE", "WT")]
head(prca)
names(prca)<- c("survtime","cens","rx","bm",
                "hx","stage","pf","age", "wt")

# Create subgroups for Age and Weight and Stage
prca$age1 <- 1 * (prca$age > 65 & prca$age <= 75)
prca$age2 <- 1 * (prca$age > 75)
prca$wt1  <- 1 * (prca$wt > 90 & prca$wt <= 110)
prca$wt2  <- 1 * (prca$wt > 110)
# Create subgroups for Age and Weight and Stage
prca$agegroup <- 1 + (1 * (prca$age > 65) + 1 * (prca$age > 75))
prca$wtgroup  <- 1 + (1 * (prca$wt > 90) + 1 * (prca$wt > 110))

formula=Surv(survtime, cens) ~ rx
reg <- survival::coxph(formula, data=prca, ties="breslow")
prca %>%
  survival::coxph(formula,
                data = ., ties="breslow") -> reg
exp(reg$coefficients)


## Level Plots -----
prca %>%
  group_by(agegroup, wtgroup) %>%
  do(data.frame(hr = exp(survival::coxph(formula, data = ., ties="breslow")$coefficients),
                broom::tidy(survival::coxph(formula, data = ., ties="breslow")),
                broom::tidy(survival::coxph(formula, data = .)),
                n  = nrow(.))) -> df1
prca %>%
  group_by(agegroup) %>%
  do(data.frame(hr = exp(survival::coxph(formula, data = ., ties="breslow")$coefficients),
                # broom::tidy(survival::coxph(formula, data = ., ties="breslow")),
                n  = nrow(.))) -> df2
prca %>%
  group_by(wtgroup) %>%
  do(data.frame(hr = exp(survival::coxph(formula, data = ., ties="breslow")$coefficients),
                # broom::tidy(survival::coxph(formula, data = ., ties="breslow")),
                n  = nrow(.))) -> df3


range(prca$age)
range(prca$wt)
agegroups = c("[48, 65]", "(65, 75]", "(75, 89]")
wtgroups  = c("[69, 90]", "(90, 110]", "(110, 152]")

ggplot() +
  geom_tile(aes(x = agegroup, y = wtgroup, fill = hr), df1, color = "white") +
  geom_text(aes(x = agegroup, y = wtgroup, label = n), df1, color = "white") +
  geom_tile(aes(x = agegroup, y = -0.25, fill = hr),   df2, color = "white", height = 0.35) +
  geom_tile(aes(x = -0.25, y = wtgroup, fill = hr),    df3, color = "white", width = 0.35) +
  ggtitle("Hazard ratio across subgroups (N = 475)") + 
  coord_fixed() +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        panel.border = element_blank(),
        axis.title = element_text(hjust = 0.63),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.title.align = 0.5,
        legend.title = element_text(face = "bold", angle = 90),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  guides(fill = guide_colourbar(title.position = "left")) +
  scale_fill_continuous(name = "Hazard Ratio", breaks = seq(0,2,0.5), limits=c(0,2)) +
  scale_x_continuous(expand = c(0,0.05)) +
  scale_y_continuous(expand = c(0,0.05)) +
  annotate(x=0.25, y=seq(1,3,1), label = wtgroups, angle = 90, geom="text") +
  annotate(x=0.4, xend=0.4, y=0.5, yend=3.5, lwd=0.75, geom="segment") +
  annotate(x=0.35, xend=0.4, y=seq(0.5,3.5,1), yend=seq(0.5,3.5,1), lwd=0.75, geom="segment") +
  annotate(x=seq(1,3,1), y=0.25, label = agegroups, geom="text") +
  annotate(x=0.5, xend=3.5, y=0.4, yend=0.4, lwd=0.75, geom="segment") +
  annotate(x=seq(0.5,3.5,1), xend=seq(0.5,3.5,1), y=0.35, yend=0.4, lwd=0.75, geom="segment") -> p1
p1



## Bar Plots -----
prca %>%
  group_by(agegroup, wtgroup) %>%
  do(data.frame(hr = exp(survival::coxph(formula, data = ., ties="breslow")$coefficients),
                broom::tidy(survival::coxph(formula, data = ., ties="breslow")),
                n  = nrow(.))) -> df1
df1 %>%
  ungroup() %>%
  mutate(weight = sqrt(n) / sqrt(nrow(prca)),
         cumsum = cumsum(weight),
         new_x = cumsum - weight/2) -> df1

agegroups = c("1" = "[48, 65]", "2" = "(65, 75]", "3" = "(75, 89]")
wtgroups  = c("[69, 90]", "(90, 110]", "(110, 152]")

ggplot(df1) +
  geom_errorbar(aes(x = new_x, ymin = hr, ymax = hr + std.error),
                width = .1) +
  geom_bar(aes(x = new_x, y = hr, fill = factor(wtgroup)), width = df1$weight-0.05, 
           stat = "identity", color = "black") +
  facet_grid(~ agegroup, scales = "free_x", space = "free_x",
             labeller = labeller(agegroup = agegroups)) +
  ggtitle("Hazard ratio across subgroups (N = 475)") + 
  scale_x_continuous(name = "Weight", breaks = df1$new_x, labels = rep(wtgroups, 3),
                     sec.axis = sec_axis(~ ., name = "Age", labels = NULL)) +
  scale_y_continuous(name = "Hazard ratio", limits = c(0, 2), expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        legend.position = "none",
        strip.background = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text = element_text(angle = 90, hjust = 1))
  

## Forest Plot -----
# Create subgroups for Age and Weight and Stage
prca$agegroup <- 1 * (prca$age > 75)
prca$wtgroup  <- 1 * (prca$wt > 100)
prca$stage <- prca$stage - 3


prca %>%
  group_by(agegroup) %>%
  do(data.frame(hr = exp(survival::coxph(formula, data = ., ties="breslow")$coefficients),
                broom::tidy(survival::coxph(formula, data = ., ties="breslow")),
                n  = nrow(.))) %>%
  mutate(variable = "agegroup") %>%
  rename(subgroup = agegroup)

prca %>%
  do(data.frame(subgroup = 2, 
                hr = exp(survival::coxph(formula, data = ., ties="breslow")$coefficients),
                broom::tidy(survival::coxph(formula, data = ., ties="breslow")),
                n  = nrow(.))) %>%
  mutate(variable = "Overall",
         hr.ll = exp(conf.low),
         hr.ul = exp(conf.high)) %>%
  group_by(subgroup) -> overall
overall

hrSubgr <- function(data, var){
  data %>%
    group_by_(var) %>%
    do(data.frame(hr = exp(survival::coxph(formula, data = ., ties="breslow")$coefficients),
                  broom::tidy(survival::coxph(formula, data = ., ties="breslow")),
                  n  = nrow(.))) %>%
    mutate(variable = var,
           hr.ll = exp(conf.low),
           hr.ul = exp(conf.high)) %>%
    rename_(subgroup = var)%>%
    ungroup()
}

bind_rows(overall,
hrSubgr(prca, "agegroup"),
hrSubgr(prca, "wtgroup"),
hrSubgr(prca, "pf"),
hrSubgr(prca, "bm"),
hrSubgr(prca, "hx"),
hrSubgr(prca, "stage")) -> fp.dat
fp.dat %>%
  mutate(variable = factor(variable,
                            levels = c("Overall", "agegroup", "wtgroup", "pf", "bm", "hx", "stage")),
         label = sprintf("%s, %s, %s, %s, %s", variable, subgroup, hr, hr.ll, hr.ul)) -> fp.dat


fp.dat %>%
  ggplot() +
  geom_errorbar(aes(ymin=hr.ll, ymax=hr.ul, x=subgroup), width = 0) +
  geom_point(aes(y=hr, x=subgroup, size = n, fill = as.factor(subgroup)), shape = 22) +
  geom_hline(yintercept = fp.dat$hr[1]) +
  geom_hline(yintercept = 1, linetype = 2) +
  scale_x_continuous(name = "",
                     labels = c("a", "b"),
                     breaks = c(0,1),
                     expand = c(0.5,0.5)) +
  scale_y_continuous(name = "Hazard Ratio") +
  coord_flip() +
  facet_grid(variable ~ ., switch = "y", 
             scales = "free_y") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.spacing = unit(0, units = "cm"), 
        legend.position = "none",
        strip.background = element_blank(),
        strip.text.y = element_text(face = "bold", angle = 180, hjust = 0),
        strip.placement = "outside",
        plot.title = element_text(face = "bold", hjust = 0.5), 
        axis.line.x = element_line(),
        axis.title = element_text(face = "bold"))


fp.dat %>%
  ggplot() +
  geom_errorbar(aes(ymin=hr.ll, ymax=hr.ul, x=subgroup), width = 0) +
  geom_point(aes(y=hr, x=subgroup, size = n, fill = as.factor(subgroup)), shape = 22) +
  geom_hline(yintercept = fp.dat$hr[1]) +
  geom_hline(yintercept = 1, linetype = 2) +
  scale_x_continuous(name = "",
                     labels = c("a", "b"),
                     breaks = c(0,1),
                     expand = c(0.5,0.5)) +
  scale_y_continuous(name = "Hazard Ratio") +
  coord_flip() +
  facet_grid(variable ~ ., switch = "y", 
             scales = "free_y") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.spacing = unit(0, units = "cm"), 
        legend.position = "none",
        strip.background = element_blank(),
        strip.text.y = element_text(face = "bold", angle = 180, hjust = 0),
        strip.placement = "outside",
        plot.title = element_text(face = "bold", hjust = 0.5), 
        axis.line.x = element_line(),
        axis.title = element_text(face = "bold"))


library(forestplot)
tabletext <- cbind(c("Biomarker","\n",as.character(fp.dat$variable)),
                   c("Subgroup","\n",as.character(fp.dat$subgroup)),
                   c("Estimate","\n",round(fp.dat$hr,2)),
                   c("n","\n",fp.dat$n))
forestplot(labeltext=tabletext, graph.pos=3,
           mean=c(NA,NA,drop(fp.dat$hr)),
           lower=c(NA,NA,drop(fp.dat$hr.ll)),
           upper=c(NA,NA,drop(fp.dat$hr.ul)),
           title="Treatment Effect",
           # hrzl_lines=sapply(indexes, function(x)
           #   gpar(lwd=1/length(variables.notrt)*600,
           #        lineend="butt",
           #        columns=c(1:4), col="#99999922"),simplify=FALSE, USE.NAMES=TRUE),
           # "31" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922")),
           txt_gp=fpTxtGp(label=gpar(cex=1.25),
                          ticks=gpar(cex=1.1),
                          xlab=gpar(cex = 1.2),
                          title=gpar(cex = 1.2)),grid = fp.dat$hr[1],
           col=fpColors(box="black", lines="black", zero = "gray50"),
           zero=0, cex=0.9, lineheight = "auto", boxsize=0.2, colgap=unit(6,"mm"),
           lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.1)


require("survival")
library(survminer)
model <- coxph( Surv(survtime, cens) ~ rx + pf + hx + rx*pf + rx * hx, data = prca )
ggforest(model)
