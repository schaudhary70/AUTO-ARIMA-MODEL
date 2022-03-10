#Auto ARIMA, 
#We wll use seasonaly adjusted electrical equipment orders data
#STEP 1: Remove seasonality
library(fpp)
par(mfrow=c(1,2))
plot(elecequip)
elecdata<-seasadj(decompose(elecequip))#Return seasonaly adjusted data
plot(elecdata)

#STEP 2: ACF and PACf
plot(diff(elecdata))# no trend and no seaonality data
par(mfrow=c(1,2))
acf(diff(elecdata))
pacf(diff(elecdata))

#in ACF lag 1 and lag 3 outside the CI(Confidence interval) q=0
#in PACF lag 1, 2 and 3 are outside CI p=3
#d=1

#FIT model 
#ARIMA model 3
Arima(elecdata, order=c(3,1,0))

#ARIMA model 4
Arima(elecdata, order=c(4,1,0))


#ARIMA(4) is better than ARIMA(3) because LLE and sigm sqauere both r smaller in ARIMA(2)
#But AIC and BIC suggest there is not much improvement
#So nothing can be said here


#ARIMA model 2
Arima(elecdata, order=c(2,1,0))
#Not good


#ARIMA model 3,1,1
Arima(elecdata, order=c(3,1,1)) #MA1 model



#STEP 3:FIT the model 
fit<-Arima(elecdata, order=c(3,1,1))


#STEP 4: check residuals
checkresiduals(fit)
#Ljung box test, p<0.05 rehject null, H0: Stationary
#It is stationary













