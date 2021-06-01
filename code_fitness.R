rm(list=ls())
fitness<-read.csv("D:/fitness.csv",header=T)
sum(is.na(fitness)==TRUE)
fitness<-na.omit(fitness)#缺失值的填补
summary(fitness)

fitness<-data.frame(scale(fitness))#标准化
na<-c(colnames(fitness))
boxplot(fitness$Age,fitness$Weight,fitness$Oxygen,fitness$RunTime,fitness$RestPulse,fitness$RunPulse,fitness$MaxPulse,main="Box plot",names = na)
outlier_location <- sapply(fitness,function(X){which(X%in%boxplot.stats(X)$out)})
todel <- (sort(unique(unlist(outlier_location))))
fitness <- fitness[-todel,]
boxplot(fitness$Age,fitness$Weight,fitness$Oxygen,fitness$RunTime,fitness$RestPulse,fitness$RunPulse,fitness$MaxPulse,main="Box plot",names = na)
fit1<-lm(Oxygen~.,fitness)
#summary(fit1)
e<-resid(fit1)
plot(e,main = '残差散点图',ylab = '残差')
abline(h=0)
library(car)
ncvTest(fit1)
library(lmtest)
dwtest(fit1)
library(car)
vif(fit1)
#library(MASS)
#rid<-lm.ridge(Oxygen~.,lambda=seq(0,1,length=1000),fitness,model=TRUE)
#matplot(x=rid$lambda, y=t(rid$coef), xlab =expression(lamdba), ylab="Cofficients", type="l")
#k <- rid$lambda[which.min(rid$GCV)]
#abline(v = k)
#k<-0.1025847
#lm.ridge(Oxygen~., lambda=k, fitness, model=TRUE)
library(ridge)
plot(linearRidge(Oxygen~., fitness, lambda = seq(0,1,0.01)))
abline(h = 0)
linearRidge(Oxygen~., fitness, lambda = seq(0,1,0.01))
summary(linearRidge(Oxygen~Age+Weight+RunTime+RunPulse, fitness, lambda = "automatic"))
summary(linearRidge(Oxygen~Age+RunTime+RunPulse, fitness, lambda = "automatic"))


library(lars)
x <- fitness[ ,c(1,2,4,5,6,7)]
x <- as.matrix(x)
y <- fitness[ ,3]
y <- as.matrix(y)
la <- lars(x, y, type = "lasso")
plot(la)
summary(la)
coef <- coef.lars(la,mode="step",s=4)
coef
predict(la,data.frame(Age=0,Weight=0,Runtime=0,RestPulse=0,RunPulse=0,MaxPulse=0),s=4)

#岭回归拟合
Oxygen_ridge<--0.08322-0.14706*fitness$Age-0.62220*fitness$RunTime-0.22226*fitness$RunPulse
plot(fitness$Oxygen, type="o",pch=0, col="red", main="岭回归", ylab="Oxyfen",lwd=2)
lines(Oxygen_ridge,type="o",pch=15,col="blue",lwd=2)
legend("topright", legend=c("真实值","拟合值"),pch=c(0,15), col=c("red","blue"), bty="n", lty=1,lwd=2)
#Lasso回归拟合
Oxygen_lasso <- predict(la,x,s=4)$fit
plot(fitness$Oxygen, type="o",pch=0, col="red", main="Lasso回归", ylab="Oxyfen",lwd=2)
lines(Oxygen_lasso,type="o",pch=15,col="blue",lwd=2)
legend("topright", legend=c("真实值","拟合值"),pch=c(0,15), col=c("red","blue"), bty="n", lty=1,lwd=2)

#fitness_raw <- read.csv("D:/fitness.csv",header=T)
#fitness_mean <- apply(fitness_raw,2,mean)
#fitness_sd <- apply(fitness_raw,2,sd)
#fitness_scale <- data.frame(scale(fitness_raw))#标准化
#fitness_com <- t(t(fitness_scale)+fitness_mean)*fitness_sd

