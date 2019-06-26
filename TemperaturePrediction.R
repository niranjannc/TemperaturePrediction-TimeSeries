data = read.table("Temp_modified.csv",",",row.names = 1, header = TRUE)
data

require("zoo")

data<-na.locf(data)
data

boxplot(data)

Jan <- data$JAN

#January
jan.data = ts(Jan, start = 1796, frequency = 1)
class(jan.data)

plot(jan.data)
abline(lm(jan.data~time(jan.data)))

#Making Variance equal by applying log
plot(log(jan.data))
abline(lm(log(jan.data))~time(jan.data))

#Making mean constant
plot(diff(log(jan.data)))
abline(lm(diff(log(jan.data)))~time(jan.data))

#ARIMA Model

#Finding Q
acf(jan.data)
acf(diff(log(jan.data)))
#Q=0

#Finding p
pacf(diff(log(jan.data)))
#p=1

#Value of D => How many times diff() is applied d=1

#Create Model
janmodel<-arima(log(jan.data),c(0,1,1),seasonal = list(order=c(0,1,1),period=1))
janmodel

#Prediction
predjan<-predict(janmodel,n.ahead = 50)
predjan

#Converting logarithmic to decimal value where e=2.718
predjan.final<-2.718^predjan$pred
predjan.final

ts.plot(jan.data,2.718^predjan$pred,log="y",lty=c(1,3))

jantest<-ts(jan.data,frequency = 1,start = 1796,end = 2003)
jantest

#Once again creating model
jantest.model<-arima(log(jantest),c(0,1,1),seasonal = list(order=c(0,1,1),period=1))
predjantest<-predict(jantest.model,n.ahead = 50)
predjantest.final<-2.718^predjantest$pred
predjantest.final

predictedjan=head(predjantest.final,10)
predictedjan

#Now comparing with Original
originaljan<-tail(jan.data,10)
originaljan