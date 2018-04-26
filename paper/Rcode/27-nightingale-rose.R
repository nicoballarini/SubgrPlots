library(SubgrPlots) # Loads this package. Install it first
library(survival)
library(ggplot2)
library(dplyr)

# # Load the data to be used
data(prca)
dat <- prca
vars = data.frame(variable = names(dat), index = 1:length(names(dat)))
vars
# levels(dat$agegroup) = c("[48,65]","(65,75]","(75,89]")
# levels(dat$wtgroup)  = c("[69,90]","(90,110]","(110,152]")
levels(dat$age_group) = c("Young","Middle-aged","Old")
levels(dat$weight_group)  = c("Low","Mid","High")

dat %>%
  rename(Age= age_group,
         Weight = weight_group)-> dat

library(ggplot2)

min(dat$weight)
max(dat$weight)

dat %>%
  mutate(Agecategory=cut(age, breaks=seq(45,90, by = 5)))   %>%
  mutate(Weightcategory=cut(weight, breaks=seq(65,155, by = 10)))   %>%
  mutate(survival = factor(ifelse(survtime > 24 , "Yes", "No"), levels = c("No", "Yes")))-> dat
dat
dat %>%
  mutate(AgeWeight = factor(sprintf("%s - %s", Age, Weight),
                            levels = c("Young - Low",
                                       "Young - Mid",
                                       "Young - High",
                                       "Middle-aged - Low",
                                       "Middle-aged - Mid",
                                       "Middle-aged - High",
                                       "Old - Low",
                                       "Old - Mid",
                                       "Old - High")))  %>%
  mutate(survival = factor(ifelse(survtime > 24 , "Yes", "No"), levels = c("No", "Yes"))) -> dat
dat
### OPtion 1
table(dat$AgeWeight, dat$survival)-> datatable.
data.frame(datatable.)
data.frame(AgeWeight = factor(rownames(datatable.), levels = rownames(datatable.)),
           cbind(datatable.,
                 Total = rowSums(datatable.),
                 pos = 1:nrow(datatable.))) -> dt

pdf("paper/figures/27-nightingale-rose-ageweight.pdf", width = 6, height = 6)
ggplot(dt) +
  geom_bar(aes(x=AgeWeight, y = (Total), fill = "No"), color = "black", width = 1, stat = "identity") +
  geom_bar(aes(x=AgeWeight, y = (Yes), fill = "Yes"), color = "black", width = 1, stat = "identity") +
  scale_y_continuous(trans = "sqrt", limits = c(0,200)) +
  scale_fill_manual(values = c("Yes"="#faa8d2","No"="#80b1d3")) +
  coord_polar(start = pi/4) +
  labs(x = "", y = "", fill = "2-year survival") +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal") +
  geom_text(aes(x = AgeWeight, y=Total+1,
                label = AgeWeight),
            hjust = 0,
            angle = 45-360/max(dt$pos)/2 - 360/max(dt$pos) * (dt$pos-1))
dev.off()

# Option 2
table(dat$AgeWeight, dat$survival, dat$rx)-> datatable.
data.frame(datatable.)
datatable.
dt.control = datatable.[,,1]
dt.trt = datatable.[,,2]


data.frame(Treatment = "Control",
           AgeWeight = factor(rownames(dt.control), levels = rownames(dt.control)),
           cbind(dt.control,
                 Total = rowSums(dt.control),
                 pos = 1:nrow(dt.control))) -> dt.c
data.frame(Treatment = "Treatment",
  AgeWeight = factor(rownames(datatable.), levels = rownames(datatable.)),
           cbind(dt.trt,
                 Total = rowSums(dt.trt),
                 pos = 1:nrow(dt.trt))) -> dt.t
dt = rbind(dt.c, dt.t)


pdf("paper/figures/27-nightingale-rose-ageweight-trt.pdf", width = 10, height = 6)
ggplot(dt) +
  geom_bar(aes(x=AgeWeight, y = (Total), fill = "No"), color = "black", width = 1, stat = "identity") +
  geom_bar(aes(x=AgeWeight, y = (Yes), fill = "Yes"), color = "black", width = 1, stat = "identity") +
  scale_y_continuous(trans = "sqrt", limits = c(0,150)) +
  scale_fill_manual(values = c("Yes"="#faa8d2","No"="#80b1d3")) +
  coord_polar(start = pi/4) +
  labs(x = "", y = "", fill = "2-year survival") +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal") +
  geom_text(aes(x = AgeWeight, y=Total+1,
                label = AgeWeight),
            hjust = 0,
            angle = 45-360/max(dt$pos)/2 - 360/max(dt$pos) * (dt$pos-1))+
  facet_grid(~Treatment)
dev.off()
