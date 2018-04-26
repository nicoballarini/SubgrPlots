 
# CABG Dataset
# Study investigating risk factors for short term mortality following CABG surgery
# 
# Note: the purpose of testing plot functions, the covariate "mi" here is pretended as treatment code in clinical trials
# 
# created by Yi-Da    29/08/2017


dat2 <-read.dta("trial_binary.dta")
 
 # the variable "age"
 dat2[,1] <- as.numeric(dat2[,1])  
 
 #the variable "sex"
 dat2[,2] <- as.numeric(dat2[,2])
 dat2[dat2[,2] == 1, 2] <- 1     #"mean "male"
 dat2[dat2[,2] == 2, 2] <- 0     #"mean "female"                
 dat2[,2] <- as.factor(dat2[,2])     
 
 #the variable ef "ejection fraction (heart function)" has three levels "good", "fair" and "poor"
 
 #the variable mi "previous MI (heart attack)
 dat2[,4] <- as.numeric(dat2[,4])
 dat2[dat2[,4] == 1, 4] <- 0     #"mean "no" 
 dat2[dat2[,4] == 2, 4] <- 1     #"mean "yes" 
 dat2[,4] <- as.factor(dat2[,4])
 
 # the variable "outcome"
 dat2[,5] <- as.numeric(dat2[,5])
 dat2[dat2[,5] == 1, 5] <- 0     #"mean "alive"  
 dat2[dat2[,5] == 2, 5] <- 1     #"mean "died"  
 dat2[,5] <- as.factor(dat2[,5])
 
 #the variable "agegrp"
 
 agegrp <-rep(2,dim(dat2)[1])
 agegrp[dat2[,1]<40]<-1
 agegrp[dat2[,1]>65]<-3
 agegrp = as.factor(agegrp)
 
 dat2 <- data.frame(dat2, agegrp)
 
 save(dat2, file = "dat2.Rda")