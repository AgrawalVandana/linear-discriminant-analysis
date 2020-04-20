## Step 0 - Read in Data

data <- read.csv("depres.csv")
dim(data)
data[1:5,]
data=data[,3:38]
head(data)
depress=data[,29]
data=data[,-28]
data=data[,-(16:17)]
data =data[,-(8:14)]
data=data[,-(10:11)]
data=data[,-13]
data=data[,-16]

data=data[,-(8:28)]

## Step 1 - Explore the data
summary(data)
dep_no=which(depress==0)
dep_yes=which(depress==1)

dep_no_50= sample(dep_no,50,replace = FALSE) # to sample equal popu of depress and not depress


t.test(data$C16[dep_yes],data$C16[dep_no])
t.test(data$C17[dep_yes],data$C17[dep_no])
t.test(data$C18[dep_yes],data$C18[dep_no])
t.test(data$C19[dep_yes],data$C19[dep_no])
t.test(data$C20[dep_yes],data$C20[dep_no])
t.test(data$DRINK[dep_yes],data$DRINK[dep_no])
t.test(data$HEALTH[dep_yes],data$HEALTH[dep_no])
t.test(data$REGDOC[dep_yes],data$REGDOC[dep_no])
t.test(data$TREAT[dep_yes],data$TREAT[dep_no])
t.test(data$BEDDAYS[dep_yes],data$BEDDAYS[dep_no])
t.test(data$ACUTEILL[dep_yes],data$ACUTEILL[dep_no])
t.test(data$CHRONILL[dep_yes],data$CHRONILL[dep_no])

#step 3
xbar1=colMeans(data[dep_no,])
xbar2=colMeans(data[dep_yes,])
S1=cov(data[dep_no,])
S2=cov(data[dep_yes,])

Sp=(2*S1+2*S2)/4

y=(xbar1-xbar2)%*%solve(Sp)%*%t(as.matrix(data))
y=as.vector(y)
a=t((xbar1-xbar2)%*%solve(Sp))
astar=a/norm(a)
ystar=t(astar)%*%t(as.matrix(data))
ystar=as.vector(ystar)

plot(density(y[dep_no]),xlim=c(-50,50))
lines(density(y[dep_yes]),col=2)
abline(v=cutoff)

plot(density(ystar[dep_no]),xlim=c(-2,2))
lines(density(ystar[dep_yes]),col=2)
abline(v=cutoff/norm(a))


## Step 5 - Calculate cutoff and classify
cutoff=.5*(xbar1-xbar2)%*%solve(Sp)%*%(xbar1+xbar2)
cutoff=as.vector(cutoff)
classify=ifelse(y>cutoff,0,1)



## Step 6 - Inspect Results and calculate separation
plot(y,rep(1,294),col=depress,ylab="")
abline(v=cutoff)

depress
classify

## Step 7 - Calculate upperbound and separation
upper=(xbar1-xbar2)%*%solve(Sp)%*%((xbar1-xbar2))

sy=(sum((y[dep_no]-mean(y[dep_no]))^2)+sum((y[dep_yes]-mean(y[dep_yes]))^2))/(length(dep_no)+length(dep_yes)-2)
separation=abs(mean(y[dep_no])-mean(y[dep_yes]))/sy

t(a)%*%Sp%*%(a)

## Step 8 - Calculate accuracy metrics
library(forecast)
c_accuracy(depress,classify)


## Step 2 - t.test
t.test(data$EBITASS[1:12],data$EBITASS[13:24])
t.test(data$ROTC[1:12],data$ROTC[13:24])

## Step 3  - summary statistics
pi1=which(admired==1)
pi2=which(admired==2)
xbar1=colMeans(data[pi1,])
xbar2=colMeans(data[pi2,])

S1=cov(data[pi1,])
S2=cov(data[pi2,])
Sp=(2*S1+2*S2)/4

plot(density(data[pi1,]$EBITASS),xlim=c(-.5,.5))
lines(density(data[pi2,]$EBITASS),col=2)


plot(density(data[pi1,]$ROTC),xlim=c(-.5,.5))
lines(density(data[pi2,]$ROTC),col=2)

## Step 4 - Calculate the linear combination and weights

y=(xbar1-xbar2)%*%solve(Sp)%*%t(as.matrix(data))
y=as.vector(y)
a=t((xbar1-xbar2)%*%solve(Sp))
astar=a/norm(a)
ystar=t(astar)%*%t(as.matrix(data))

plot(density(y[pi1]),xlim=c(-20,30))
lines(density(y[pi2]),col=2)

## Step 5 - Calculate cutoff and classify
cutoff=.5*(xbar1-xbar2)%*%solve(Sp)%*%(xbar1+xbar2)
cutoff=as.vector(cutoff)
classify=ifelse(y>cutoff,1,2)

abline(v=cutoff)

## Step 6 - Inspect Results and calculate separation
plot(y,rep(1,24),col=admired,ylab="")
abline(v=cutoff)

admired
classify

## Step 7 - Calculate upperbound and separation
upper=(xbar1-xbar2)%*%solve(Sp)%*%((xbar1-xbar2))

sy=(sum((y[pi1]-mean(y[pi1]))^2)+sum((y[pi2]-mean(y[pi2]))^2))/(length(pi1)+length(pi2)-2)
separation=abs(mean(y[pi1])-mean(y[pi2]))/sy

t(a)%*%Sp%*%(a)

## Step 8 - Calculate accuracy metrics
library(forecast)
c_accuracy(depress,classify)

library(ROCR)
pred<-prediction(-y,admired)
perf <- performance(pred,"tpr","fpr")
plot(perf)



### other , 


install.packages("MASS")
library(MASS)
model=lda(data$CASES~., data =data)
summary(model)
predi =predict(model)
plot(model)
round((c_accuracy(depress,predi$class))
## Fisher maximizes the difference between the means, normalized by a measure of the within class scatter




## Function Below
## make sure actuals and classifications are 0 (no) or 1 (yes) only 
##  Built by Matthew J. Schneider

c_accuracy=function(actuals,classifications){
  df=data.frame(actuals,classifications);
  
  
  tp=nrow(df[df$classifications==1 & df$actuals==1,]);        
  fp=nrow(df[df$classifications==1 & df$actuals==0,]);
  fn=nrow(df[df$classifications==0 & df$actuals==1,]);
  tn=nrow(df[df$classifications==0 & df$actuals==0,]); 
  
  
  recall=tp/(tp+fn)
  precision=tp/(tp+fp)
  accuracy=(tp+tn)/(tp+fn+fp+tn)
  tpr=recall
  fpr=fp/(fp+tn)
  fmeasure=2*precision*recall/(precision+recall)
  scores=c(recall,precision,accuracy,tpr,fpr,fmeasure)
  names(scores)=c("recall","precision","accuracy","tpr","fpr","fmeasure")
  
  #print(scores)
  return(scores);
}



