### data generation for illustration ####
set.seed(932442)

n <- 1000

trt<-sample(0:1,1000,replace=TRUE)

age <- sample(19:93,n,replace=TRUE,prob=dnorm(seq(-2,2,len=75))) 

age_groups <-rep(2,n)
age_groups[age<40]<-1
age_groups[age>65]<-3

## some disease severity 0-5

severity <- sample(1:5,n,replace=T,prob=c(0.2,0.28,0.31,0.17,0.04))

gender <- sample(0:1,n,replace=T,prob=c(0.4,0.6))

race <- sample(c("white","black", "other"),1000, replace=TRUE, prob=c(65,20,15))


height<-weight<-bmi<-rep(0,1000)

for(i in 1:n){
  
  if(gender[i]==0){  #female
    
    height[i]<-rnorm(1,165,sd=11)    
    weight[i] <- rnorm(1, 72,sd=9) 
    
    if(height[i]<145 | height[i]>195) height[i]<-runif(1,155,175) 
    
  }else{
    
    height[i]<-rnorm(1,180,sd=11)
    weight[i] <- rnorm(1, 84,sd=12)
    if(height[i]<155 | height[i]>205) height[i]<-runif(1,165,195)
    
  }
  
}


bmi <-weight/(height/100)^2

bmi_cat <- rep("normal",n)

bmi_cat[bmi<18.5] <- "Underweight"
bmi_cat[bmi>18.5 & bmi<25] <- "Normal"
bmi_cat[bmi>25 & bmi<30] <- "Overweight"
bmi_cat[bmi>30] <- "Obese"

response <- 0.5*trt + 12*(race=="other") - 4.3*(race=="black") + 10.5*severity + 4.5*gender + 0.7*bmi + 0.2 * age + rnorm(n,m=0,sd=5)

dat <- data.frame(resp=response, trt=trt, age=age, age.groups=age_groups, sev=severity, gender=gender, height=height, weight=weight, bmi=bmi, bmi_cat=bmi_cat)
