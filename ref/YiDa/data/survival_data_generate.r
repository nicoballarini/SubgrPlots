
# breastcancer Dataset (Tibshirani, 1982)
# 
# The breastcancer dataset contains information on patients treated for primary
# node positive breast cancer. The time of interest is the time in days from treatment to either
# recurrence or death (time). The variable fail indicates patients who died or had a tumour
# recurrence (1) or remained recurrence-free (0).
#
# The number of positive lymph nodes is contained in the variable "nodes". A grouped variable
# "nodesgrp" has been derived from it for plotting purposes (1 = 1-3 nodes, 2 = 4-9 nodes, 3 = 10+ nodes).
#
# The researchers also collected information on "progesterone" receptor involvement and
# whether the patient received "hormone" therapy.
#
# Note that this data can be used as binary data set for logistic regression analysis
#
# created by Yi-Da    29/08/2017


dat3 <-read.dta("breastcancer.dta")

# the variable "hormone" as treatment code
dat3[,1] <- as.numeric(dat3[,1])  
dat3[dat3[,1] == 1, 1] <- 0     #"mean "no"
dat3[dat3[,1] == 2, 1] <- 1     #"mean "yes"                
dat3[,1] <- as.factor(dat3[,1]) 

#the variable "progesterone"
dat3[,2] <- as.numeric(dat3[,2])
  
#the vailable "fail" 
dat3[,3] <- as.numeric(dat3[,3])   

#the variable "time"
dat3[,4] <- as.numeric(dat3[,4])

#the variable "nodes"
dat3[,5] <- as.numeric(dat3[,5])

#the variable "nodesgrp"
dat3[,6] <- as.factor(dat3[,6])

#the variable "progsgrp"
progsgrp <-rep(2,dim(dat3)[1])
progsgrp[dat3[,2] < 50] <-1
progsgrp[dat3[,2] > 130] <-3
progsgrp = as.factor(progsgrp)
dat3 <- data.frame(dat3, progsgrp)

save(dat3, file = "dat3.Rda")