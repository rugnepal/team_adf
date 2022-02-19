#probability Reduction Approach 
library(readxl)
Data <- read_excel("E:/Data fellowship code/Data.xlsx")
View(Data)
names(Data)
Y<-Data$`Adjusted savings: net forest depletion (% of GNI)`
X<-Data$`Industry (including construction), value added (% of GDP)`
Z<-Data$`GDP per capita (current US$)`
P<-Data$`Population, total`
T<-Data$Year


#EDA wihtout log

plot.ts(Y)# irregularity 
plot.ts(X) #Trend plus irregularity , hetrogenous data (mean trending/mean hetrogenous )
plot.ts(Z) #exponential data ,hetrogenous data (mean  trending)
plot.ts(P) #trend in the data 


#without log simulation 

cor(Y,P) 
model<-lm(Y~X+Z+P)
summary(model) 
plot.ts(model$residuals)


#log simulation data

#EDA of log data
plot.ts(log(X)) #trend in data  #hetrogenousdata
plot.ts(log(Y)) #Irregularity in data  
plot.ts(log(Z)) #linear trend in data 
plot.ts(log(P)) #Linear trend in data 

hist(log(P),breaks = 50)

#Taking log
Yt<-as.numeric(log(Y))
Xt<-as.numeric(log(X))
Zt<-as.numeric(log(Z))
Pt<-as.numeric(log(P))

cor(Yt,Xt) 
model<-lm(Yt~Xt+Zt+Pt)  #first model ,log model 
summary(model)
u.hat1<-model$residuals
plot.ts(u.hat1) # irregularityin the residuals 
hist(u.hat) #seemslike normal dist ,but check normality via skweness and kurtosis or JB  test 

#M-S test 
#Independence test  
AU1<-lm(Yt[2:49]~Yt[1:48])
summary(AU1) #siginificance estimate value 0.61 ,positive dependence,model is misspecified .


#t-invariance
#t-invariance in mean
t<-c(1:49)
length(t)
AU3 <- lm(u.hat1~t+Xt+Zt+Pt)
summary(AU3)#t is significant,model is misspecified 
plot.ts(u.hat1)



#t-invariance in variance
AU4 <- lm(u.hat1^2~t)
summary(AU4)#variance also significant,variance hetrogenous 
plot.ts(u.hat1^2)

#Variance independence
AU4 <-lm(u.hat1[2:49]^2~u.hat1[1:48])
summary(AU4)



length(Yt[2:49])
t<-2:49
length(t)


#Respecified part -1 
model2 <-lm (Yt[2:49]~t[2:49]+Zt[2:49]+Xt[2:49]+Pt[2:49]+Yt[1:48])#Hetrogenous AR(1)
summary(model2)
u.hat2<-model2$residuals
plot.ts(u.hat2)
abline(h=0,lwd=5)
hist(u.hat2)
pacf(u.hat2)
plot(rnorm(length(u.hat2)),type="l")#example 


#MSTest 

#Noramilty test 
T<-length(u.hat2)
library(tseries)
jarque.bera.test(u.hat2)

#Linearity
u.hat2 <- model2$residuals
AU1 <- lm(Yt[2:49]~t[2:49]+Zt[2:49]+Xt[2:49]+Pt[2:49]+Yt[1:48]+I(Zt[2:49]^2)+I(Xt[2:49]^2)+I(Pt[2:49]^2)+I(Yt[1:48]^2))   
summary(AU1) #insignificant model is specified . 

#Homoscedasticity
AU2 <- lm(u.hat2[2:49]^2~ Zt[2:49]+Xt[2:49]+Pt[2:49]+Yt[1:48])
summary(AU2) #insignificant, model specified 


#Indepndence
AU3<-lm(Yt[3:49]~t[3:49]+Zt[3:49]+Xt[3:49]+Pt[3:49]+Yt[1:47]+Yt[2:48]+Xt[1:47]+Xt[2:48])
summary(AU3) #X second lag is significant. 


#test of mean homogenity
AU4<-lm(u.hat2[2:49]~t[2:49]+I(t[2:49]^2)+Zt[2:49]+Xt[2:49]+Pt[2:49]+Yt[1:48])
summary(AU4)#t sqaure is not significant ,model is specified. 

#Variance
AU5 <- lm(u.hat2[1:49]^2~t[1:49])
summary(AU5)#variance hetrogenity 




#newmodel
model3<-lm(Yt[3:49]~t[3:49]+Zt[3:49]+Xt[3:49]+Pt[3:49]+Yt[2:48]+Xt[1:47]+Xt[2:48])
summary(model3)
u.hat3<-model3$residuals
plot.ts(u.hat3)
acf(u.hat3)
hist(u.hat3)



#test of normality
library(tseries)
jarque.bera.test(u.hat3)


#test of independence
AU6<-lm(Yt[3:49]~t[3:49]+Zt[3:49]+Xt[3:49]+Pt[3:49]+Yt[2:48]+Xt[1:47]+Xt[2:48]+Zt[2:48]+Pt[2:48])
summary(AU6)#insignificant,model is specified. 

#divide window into a 3X2 grid
par(mfrow = c(3, 1), mar = c(0.5, 4, 4, 20))  
# add plots to window
plot.ts( u.hat1 ,col="red")
plot.ts( u.hat2 ,col="blue")
plot.ts( u.hat3 ,green="green")



#divide window into a 3X2 grid
par(mfrow = c(2, 1))  
# add plots to window
plot.ts(P,col="red")
plot.ts(log(P) ,col="blue")





plot.ts(Z ,col="green")
plot.ts(P,col="yellow")





