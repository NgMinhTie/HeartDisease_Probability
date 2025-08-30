library(plyr)
heart_data <- read.csv(file = "CHDdata.csv", header = FALSE)  
colnames(heart_data) <- c("sbp", "tobacco", "ldl", "adiposity", "famhist", "typea", "obesity", "alcohol", "age", "chd")  
str(heart_data)  
heart_data [heart_data =="N/A"]<-NA   
apply(is.na(heart_data),2,sum)  
heart_data <-na.omit(heart_data)  
apply(is.na(heart_data),2,which)  
is.numeric(heart_data$sbp)  
  
is.numeric(heart_data$tobacco)  
  
is.numeric(heart_data$ldl)  
  
is.numeric(heart_data$adiposity)  
  
is.numeric(heart_data$famhist)  
heart_data <- heart_data [, -5]  
  
is.numeric(heart_data$typea)  
  
is.numeric(heart_data$obesity)  
  
is.numeric(heart_data$alcohol)  
  
is.numeric(heart_data$age)  
  
is.numeric(heart_data$chd)  
mean<-apply(heart_data[,c(1 : 9)],2,mean)  
  
median<-apply(heart_data[,c(1 : 9)],2,median)  
  
max<-apply(heart_data[,c(1 : 9)],2,max)  
  
min<-apply(heart_data[,c(1 : 9)],2,min)  
  
sd<-apply(heart_data[,c(1 : 9)],2,sd)  
  
t(data.frame(mean,sd,median,max,min))  
  
hist(heart_data$age,xlab="Age",ylab="Number of people",main="Histogram of researched age group",ylim=c(0,100),labels=T,col="pink")  
hist(heart_data$alcohol,xlab="alcohol",ylab="Amount of alcohol",main="Histogram of researched alcohol group",ylim=c(0,400),labels=T,col="pink")  
hist(heart_data$alcohol,xlab="alcohol",ylab="Amount of alcohol",main="Histogram of researched alcohol group",ylim=c(0,400),labels=T,col="pink")  
  
hist(heart_data$typea,xlab="typea",ylab="number of typea",main="Histogram of researched typea group",ylim=c(0,100),labels=T,col="pink")  
hist(heart_data$tobacco,xlab="tobacco",ylab="number of tobacco",main="Histogram of researched tobacco group",ylim=c(0,400),labels=T,col="pink")  
hist(heart_data$adiposity,xlab="adiposity",ylab="number of adiposity",main="Histogram of researched adiposity group",ylim=c(0,150),labels=T,col="pink")  
hist(heart_data$ldl,xlab="ldl",ylab="number of ldl",main="Histogram of researched ldl group",ylim=c(0,200),labels=T,col="pink")  
hist(heart_data$sbp,xlab="sbp",ylab="number of sbp",main="Histogram of researched sbp group",ylim=c(0,200),labels=T,col="pink")  
barplot(table(heart_data$famhist),xlab="Famhist",ylab="Frequency",main="Barplot of Famhist",col="salmon",ylim=c(0,300),xlim=c(0,12))  
Famhist_P<-heart_data[heart_data$famhist=="Present",]  
table(Famhist_P$chd)  
quantity<-c(96,95)  
presentchd<-c("No","Yes")  
percentage<-round(quantity/sum(quantity)*100,2)  
presentchd<-paste(presentchd,percentage)  
presentchd<-paste(presentchd,"%")  
Famhist_A<-heart_data[heart_data$famhist=="Absent",]  
table(Famhist_A$chd)  
quantity1<-c(206,64)  
absentchd<-c("No","Yes")  
percentage<-round(quantity/sum(quantity)*100,2)  
absentchd<-paste(absentchd,percentage)  
absentchd<-paste(absentchd,"%")  
par(mfrow=c(1,2))  
pie(quantity,labels = presentchd,main="Famhist present chd percentage",col=c("pink","seagreen"))  
pie(quantity1,labels = absentchd,main="Famhist absent chd percentage",col=c("pink","lightgreen"))  
library(ggplot2)  
mu_sbp<-ddply(heart_data,"chd",summarise,grp.mean=mean(sbp))  
ggplot(heart_data,aes(x=sbp,color=as.factor(chd),fill=as.factor(chd)))+  
geom_histogram(position="identity",alpha=0.5)+  
geom_vline(data=mu_sbp,aes(xintercept=grp.mean,color=as.factor(chd)),linetype="dashed")+  
scale_color_manual(values=c("999999","#E69F00","#56B4E9"))+  
scale_fill_manual(values=c("999999","#E69F00","#56B4E9"))+  
labs(title="Histogram of sbp for CHDDisease",x="sbp",y="Frequency")+  
theme_classic()  
library(ggplot2)  

mu_tobacco<-ddply(heart_data,"chd",summarise,grp.mean=mean(tobacco))  
ggplot(heart_data,aes(x=tobacco,color=as.factor(chd),fill=as.factor(chd)))+  
geom_histogram(position="identity",alpha=0.5)+  
geom_vline(data=mu_tobacco,aes(xintercept=grp.mean,color=as.factor(chd)),linetype="dashed")+  
scale_color_manual(values=c("999999","#E69F00","#56B4E9"))+  
scale_fill_manual(values=c("999999","#E69F00","#56B4E9"))+  
labs(title="Histogram of tobacco for CHDDisease",x="tobacco",y="Frequency")+  
theme_classic()  
library(ggplot2)  
mu_adiposity<-ddply(heart_data,"chd",summarise,grp.mean=mean(adiposity))  
ggplot(heart_data,aes(x=ldl,color=as.factor(chd),fill=as.factor(chd)))+  
geom_histogram(position="identity",alpha=0.5)+  
geom_vline(data=mu_adiposity,aes(xintercept=grp.mean,color=as.factor(chd)),linetype="dashed")+  
scale_color_manual(values=c("999999","#E69F00","#56B4E9"))+  
scale_fill_manual(values=c("999999","#E69F00","#56B4E9"))+  
labs(title="Histogram of adiposity for CHDDisease",x="adiposity",y="Frequency")+  
theme_classic()  
library(ggplot2)  

mu_typea<-ddply(heart_data,"chd",summarise,grp.mean=mean(typea))  
ggplot(heart_data,aes(x=ldl,color=as.factor(chd),fill=as.factor(chd)))+  
geom_histogram(position="identity",alpha=0.5)+  
geom_vline(data=mu_typea,aes(xintercept=grp.mean,color=as.factor(chd)),linetype="dashed")+  
scale_color_manual(values=c("999999","#E69F00","#56B4E9"))+  
scale_fill_manual(values=c("999999","#E69F00","#56B4E9"))+  
labs(title="Histogram of typea for CHDDisease",x="typea",y="Frequency")+  
theme_classic()  
library(ggplot2)  
mu_obesity<-ddply(heart_data,"chd",summarise,grp.mean=mean(obesity))  
ggplot(heart_data,aes(x=ldl,color=as.factor(chd),fill=as.factor(chd)))+  
geom_histogram(position="identity",alpha=0.5)+  
geom_vline(data=mu_obesity,aes(xintercept=grp.mean,color=as.factor(chd)),linetype="dashed")+  
scale_color_manual(values=c("999999","#E69F00","#56B4E9"))+  
scale_fill_manual(values=c("999999","#E69F00","#56B4E9"))+  
labs(title="Histogram of obesity for CHDDisease",x="obesity",y="Frequency")+  
theme_classic()  
  
library(ggplot2)  
mu_alcohol<-ddply(heart_data,"chd",summarise,grp.mean=mean(alcohol))  
ggplot(heart_data,aes(x=ldl,color=as.factor(chd),fill=as.factor(chd)))+  
geom_histogram(position="identity",alpha=0.5)+  
geom_vline(data=mu_alcohol,aes(xintercept=grp.mean,color=as.factor(chd)),linetype="dashed")+  
scale_color_manual(values=c("999999","#E69F00","#56B4E9"))+  
scale_fill_manual(values=c("999999","#E69F00","#56B4E9"))+  
labs(title="Histogram of alcohol for CHDDisease",x="alcohol",y="Frequency")+  
theme_classic()  
library(ggplot2)  
mu_age<-ddply(heart_data,"chd",summarise,grp.mean=mean(age))  
ggplot(heart_data,aes(x=ldl,color=as.factor(chd),fill=as.factor(chd)))+  
geom_histogram(position="identity",alpha=0.5)+  
geom_vline(data=mu_age,aes(xintercept=grp.mean,color=as.factor(chd)),linetype="dashed")+  
scale_color_manual(values=c("999999","#E69F00","#56B4E9"))+  
scale_fill_manual(values=c("999999","#E69F00","#56B4E9"))+  
labs(title="Histogram of age for CHDDisease",x="age",y="Frequency")+  
theme_classic()  
library(caTools)
library(dplyr)
library(ROCR)
library(zoo)
library(lmtest)
 model<-glm(chd~., family = binomial, data=heart_data)  
logistic<-step(model) 

summary(logistic)$coef
exp(cbind(OR=coef(logistic), confint(logistic)))

pred<-predict(logistic,heart_data,type="response")  
heart_data$pred_heart<-round(pred,digits=0)
head(heart_data,10) 

