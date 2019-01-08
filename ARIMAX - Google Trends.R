
google_var_orig = read.csv(file.choose())

#install.packages("plm")
#install.packages("fpp")
#install.packages("lmtest")
#library(plm)
library(fpp)
library (forecast)
library (lmtest)
head(google_var_orig)


corr_matrix = cor(google_var_orig[,3:8], use="complete.obs")
corr_matrix

  #removing rows without data in order to use in creating models without the problem of missing variables
  #removing rows that are missing zillowdata
google_var=google_var_orig[complete.cases(google_var_orig[ , 3]),]
  #removing rows that are missing google trends data, i.e. Lakeland
google_var=google_var[complete.cases(google_var[ , 4]),]


plot.ts(google_var)
  #Looking at ACF and PACF
acf(google_var$zillowhousesales, na.action=na.pass)
pacf(google_var$zillowhousesales, na.action=na.pass)
  #Testing for stationarity. However, keep in mind that it is testing it as if it is one time series for one location.
  #However the data has 5 locations.
adf.test(google_var$zillowhousesales, alternative="stationary")


  # Now lets do it the correct way and do tests by MSA. We will start with Charleston.

####### NOTE: Notes are only in Charleston's Method. Other MSA methods were created by copying Charleston's method and changing the name ofthe MSA. #########

#--------------------------  CHARLESTON - START --------------------------------------#

  #Take only Charleston's observations
Charleston_google_var= google_var[which (google_var$ï..MSA =="Charleston"),]
  #Declaring dataset as time series
Charleston_google_var$Month = ts(Charleston_google_var$Month, start=c(2008,03), end =c(2017,11), frequency=12)
  #Look at correlation matrix
Charleston_corr_matrix=cor(Charleston_google_var[,3:8], use="complete.obs")
Charleston_corr_matrix
  #Look at ACF and PACF for Charleston
acf(Charleston_google_var$zillowhousesales, na.action=na.pass)
pacf(Charleston_google_var$zillowhousesales, na.action=na.pass)
  #Check Stationarity for Charleston
adf.test(Charleston_google_var$zillowhousesales, alternative="stationary")
  #Do a basic model with only the lag of the dependent variables
Charleston_fit=auto.arima(Charleston_google_var$zillowhousesales)
Charleston_fit
  #Checking the residuals are white noise
tsdiag(Charleston_fit)
  #Forecasting 3 months ahead
Charleston_forecast= forecast(Charleston_fit,h=3)
plot(Charleston_forecast)

  #Now lets do an ARIMAX model with regressors added onto the basic model
  # Taking our other independent variables from google trends
Charleston_xreg= Charleston_google_var[,4:8]
  #ARIMAX model created
Charleston_fit_X=auto.arima(Charleston_google_var$zillowhousesales, xreg=Charleston_xreg)
tsdiag(Charleston_fit_X)
  #Lets forecast using the data from the next three months and fit it.
Charleston_forecast_xreg=google_var_orig[which (google_var_orig$ï..MSA =="Charleston" & 
                                                       (google_var_orig$Month=="2017-12")),4:8]
  #Graph our results
Charleston_forecast_X= forecast(Charleston_fit_X,xreg=Charleston_forecast_xreg, h=3)
plot(Charleston_forecast_X)


    #Now let's perform the evaluation as Professor Suggested. We will take the first 83 months to create the model initially.
    #We will predict the next three months and record the results
    #Then we will repeat the process by making the model with the first 86 months, and then predicting the next 3 months and recording the results
    #We will do this 8 times until we have reached the end of the data.
Charleston_Printed_Results=NULL
for (w in 1:8) {
  Charleston_Forecast_Results=NULL
  
  #Creating a dataset with only the rows for the model
  Charleston_ARIMAX <- Charleston_google_var[1:(90+(3*w)),]
  #Creating a dataset with only the rows for the forecasting and the google trends regressors needed
  Charleston_ARIMAX_Predict <- Charleston_google_var[(91+(3*w)):(93+(3*w)),4:8]
  #Creating a dataset with the actual results for the rows we are forecasting
  Charleston_ARIMAX_Actual <- Charleston_google_var[(91+(3*w)):(93+(3*w)),3]
  
  #Saving the google trends regressors to create the model
  Charleston_ARIMAX_xreg= Charleston_ARIMAX[,4:8]
  
  #Creating the model
  Charleston_fit_ARIMAX=auto.arima(Charleston_ARIMAX$zillowhousesales, xreg=Charleston_ARIMAX_xreg)
  #checking for white noise
  tsdiag(Charleston_fit_ARIMAX)
  #Forecasting the next 3 months
  Charleston_forecast_ARIMAX= forecast(Charleston_fit_ARIMAX,xreg=Charleston_ARIMAX_Predict, h=3)
  plot(Charleston_forecast_ARIMAX)
  Charleston_forecast_ARIMAX
  Charleston_ARIMAX_Actual
  #Obtain the accuracy measurement
  Charleston_Accuracy=data.frame(accuracy(Charleston_forecast_ARIMAX,Charleston_ARIMAX_Actual ))
  Charleston_Accuracy
  #Record the results
  Charleston_Forecast_Results<-cbind(Index_Point=c((91+(3*w)):(93+(3*w))), Charleston_forecast_ARIMAX$mean, MAE=Charleston_Accuracy[1,3])
  Charleston_Printed_Results =rbind(Charleston_Printed_Results,Charleston_Forecast_Results)
}
Charleston_Printed_Results

  #Attaching the forecasts that we made earlier for 2017-12
Charleston_Forecast_Now<-cbind(Index_Point=c(118), Charleston_forecast_X$mean, MAE=0)
Charleston_Printed_Results =rbind(Charleston_Printed_Results,Charleston_Forecast_Now)
Charleston_Printed_Results

  #Spitting out an excel file with the results
write.csv(Charleston_Printed_Results, file="Charleston Arima Results.csv")

#--------------------------  CHARLESTON - END --------------------------------------#




#--------------------------  Chattanooga - START --------------------------------------#

Chattanooga_google_var= google_var[which (google_var$ï..MSA =="Chattanooga"),]

Chattanooga_google_var$Month = ts(Chattanooga_google_var$Month, start=c(2008,03), end =c(2017,11), frequency=12)

Chattanooga_corr_matrix=cor(Chattanooga_google_var[,3:8], use="complete.obs")
Chattanooga_corr_matrix

acf(Chattanooga_google_var$zillowhousesales, na.action=na.pass)
pacf(Chattanooga_google_var$zillowhousesales, na.action=na.pass)

adf.test(Chattanooga_google_var$zillowhousesales, alternative="stationary")
Chattanooga_fit=auto.arima(Chattanooga_google_var$zillowhousesales)
Chattanooga_fit

tsdiag(Chattanooga_fit)

Chattanooga_forecast= forecast(Chattanooga_fit,h=3)
plot(Chattanooga_forecast)


Chattanooga_xreg= Chattanooga_google_var[,4:8]

Chattanooga_fit_X=auto.arima(Chattanooga_google_var$zillowhousesales, xreg=Chattanooga_xreg)
tsdiag(Chattanooga_fit_X)

Chattanooga_forecast_xreg=google_var_orig[which (google_var_orig$ï..MSA =="Chattanooga" & 
                                                       (google_var_orig$Month=="2017-12")),4:8]

Chattanooga_forecast_X= forecast(Chattanooga_fit_X,xreg=Chattanooga_forecast_xreg, h=3)
plot(Chattanooga_forecast_X)


Chattanooga_Printed_Results=NULL
for (w in 1:8) {
  Chattanooga_Forecast_Results=NULL
  Chattanooga_ARIMAX <- Chattanooga_google_var[1:(90+(3*w)),]

  Chattanooga_ARIMAX_Predict <- Chattanooga_google_var[(91+(3*w)):(93+(3*w)),4:8]

  Chattanooga_ARIMAX_Actual <- Chattanooga_google_var[(91+(3*w)):(93+(3*w)),3]
  
  Chattanooga_ARIMAX_xreg= Chattanooga_ARIMAX[,4:8]
  
  Chattanooga_fit_ARIMAX=auto.arima(Chattanooga_ARIMAX$zillowhousesales, xreg=Chattanooga_ARIMAX_xreg)

  tsdiag(Chattanooga_fit_ARIMAX)

  Chattanooga_forecast_ARIMAX= forecast(Chattanooga_fit_ARIMAX,xreg=Chattanooga_ARIMAX_Predict, h=3)
  plot(Chattanooga_forecast_ARIMAX)
  Chattanooga_forecast_ARIMAX
  Chattanooga_ARIMAX_Actual

  Chattanooga_Accuracy=data.frame(accuracy(Chattanooga_forecast_ARIMAX,Chattanooga_ARIMAX_Actual ))
  Chattanooga_Accuracy

  Chattanooga_Forecast_Results<-cbind(Index_Point=c(91+(3*w)):(93+(3*w)), Chattanooga_forecast_ARIMAX$mean, MAE=Chattanooga_Accuracy[1,3])
  Chattanooga_Printed_Results =rbind(Chattanooga_Printed_Results,Chattanooga_Forecast_Results)
}
Chattanooga_Printed_Results

Chattanooga_Forecast_Now<-cbind(Index_Point=c(118), Chattanooga_forecast_X$mean, MAE=0)
Chattanooga_Printed_Results =rbind(Chattanooga_Printed_Results,Chattanooga_Forecast_Now)
Chattanooga_Printed_Results

write.csv(Chattanooga_Printed_Results, file="Chattanooga Arima Results.csv")
#--------------------------  Chattanooga - END --------------------------------------#





#--

#--------------------------  Knoxville - START --------------------------------------#

Knoxville_google_var= google_var[which (google_var$ï..MSA =="Knoxville"),]

Knoxville_google_var$Month = ts(Knoxville_google_var$Month, start=c(2008,03), end =c(2017,11), frequency=12)

Knoxville_corr_matrix=cor(Knoxville_google_var[,3:8], use="complete.obs")
Knoxville_corr_matrix

acf(Knoxville_google_var$zillowhousesales, na.action=na.pass)
pacf(Knoxville_google_var$zillowhousesales, na.action=na.pass)

adf.test(Knoxville_google_var$zillowhousesales, alternative="stationary")
Knoxville_fit=auto.arima(Knoxville_google_var$zillowhousesales)
Knoxville_fit

tsdiag(Knoxville_fit)

Knoxville_forecast= forecast(Knoxville_fit,h=3)
plot(Knoxville_forecast)


Knoxville_xreg= Knoxville_google_var[,4:8]

Knoxville_fit_X=auto.arima(Knoxville_google_var$zillowhousesales, xreg=Knoxville_xreg)
tsdiag(Knoxville_fit_X)

Knoxville_forecast_xreg=google_var_orig[which (google_var_orig$ï..MSA =="Knoxville" & 
                                                       (google_var_orig$Month=="2017-12")),4:8]

Knoxville_forecast_X= forecast(Knoxville_fit_X,xreg=Knoxville_forecast_xreg, h=3)
plot(Knoxville_forecast_X)


Knoxville_Printed_Results=NULL
for (w in 1:8) {
  Knoxville_Forecast_Results=NULL
  Knoxville_ARIMAX <- Knoxville_google_var[1:(90+(3*w)),]
  
  Knoxville_ARIMAX_Predict <- Knoxville_google_var[(91+(3*w)):(93+(3*w)),4:8]
  
  Knoxville_ARIMAX_Actual <- Knoxville_google_var[(91+(3*w)):(93+(3*w)),3]
  
  Knoxville_ARIMAX_xreg= Knoxville_ARIMAX[,4:8]
  
  Knoxville_fit_ARIMAX=auto.arima(Knoxville_ARIMAX$zillowhousesales, xreg=Knoxville_ARIMAX_xreg)
  
  tsdiag(Knoxville_fit_ARIMAX)
  
  Knoxville_forecast_ARIMAX= forecast(Knoxville_fit_ARIMAX,xreg=Knoxville_ARIMAX_Predict, h=3)
  plot(Knoxville_forecast_ARIMAX)
  Knoxville_forecast_ARIMAX
  Knoxville_ARIMAX_Actual
  
  Knoxville_Accuracy=data.frame(accuracy(Knoxville_forecast_ARIMAX,Knoxville_ARIMAX_Actual ))
  Knoxville_Accuracy
  
  Knoxville_Forecast_Results<-cbind(Index_Point=c(91+(3*w)):(93+(3*w)), Knoxville_forecast_ARIMAX$mean, MAE=Knoxville_Accuracy[1,3])
  Knoxville_Printed_Results =rbind(Knoxville_Printed_Results,Knoxville_Forecast_Results)
}
Knoxville_Printed_Results

Knoxville_Forecast_Now<-cbind(Index_Point=c(118), Knoxville_forecast_X$mean, MAE=0)
Knoxville_Printed_Results =rbind(Knoxville_Printed_Results,Knoxville_Forecast_Now)
Knoxville_Printed_Results

write.csv(Knoxville_Printed_Results, file="Knoxville Arima Results.csv")

#--------------------------  Knoxville - END --------------------------------------#






#------------------------  Nashville - START --------------------------------------#

Nashville_google_var= google_var[which (google_var$ï..MSA =="Nashville"),]

Nashville_google_var$Month = ts(Nashville_google_var$Month, start=c(2008,03), end =c(2017,11), frequency=12)

Nashville_corr_matrix=cor(Nashville_google_var[,3:8], use="complete.obs")
Nashville_corr_matrix

acf(Nashville_google_var$zillowhousesales, na.action=na.pass)
pacf(Nashville_google_var$zillowhousesales, na.action=na.pass)

adf.test(Nashville_google_var$zillowhousesales, alternative="stationary")
Nashville_fit=auto.arima(Nashville_google_var$zillowhousesales)
Nashville_fit

tsdiag(Nashville_fit)

Nashville_forecast= forecast(Nashville_fit,h=3)
plot(Nashville_forecast)


Nashville_xreg= Nashville_google_var[,4:8]

Nashville_fit_X=auto.arima(Nashville_google_var$zillowhousesales, xreg=Nashville_xreg)
tsdiag(Nashville_fit_X)

Nashville_forecast_xreg=google_var_orig[which (google_var_orig$ï..MSA =="Nashville" & 
                                                       (google_var_orig$Month=="2017-12")),4:8]

Nashville_forecast_X= forecast(Nashville_fit_X,xreg=Nashville_forecast_xreg, h=3)
plot(Nashville_forecast_X)


Nashville_Printed_Results=NULL
for (w in 1:8) {
  Nashville_Forecast_Results=NULL
  Nashville_ARIMAX <- Nashville_google_var[1:(90+(3*w)),]
  
  Nashville_ARIMAX_Predict <- Nashville_google_var[(91+(3*w)):(93+(3*w)),4:8]
  
  Nashville_ARIMAX_Actual <- Nashville_google_var[(91+(3*w)):(93+(3*w)),3]
  
  Nashville_ARIMAX_xreg= Nashville_ARIMAX[,4:8]
  
  Nashville_fit_ARIMAX=auto.arima(Nashville_ARIMAX$zillowhousesales, xreg=Nashville_ARIMAX_xreg)
  
  tsdiag(Nashville_fit_ARIMAX)
  
  Nashville_forecast_ARIMAX= forecast(Nashville_fit_ARIMAX,xreg=Nashville_ARIMAX_Predict, h=3)
  plot(Nashville_forecast_ARIMAX)
  Nashville_forecast_ARIMAX
  Nashville_ARIMAX_Actual
  
  Nashville_Accuracy=data.frame(accuracy(Nashville_forecast_ARIMAX,Nashville_ARIMAX_Actual ))
  Nashville_Accuracy
  
  Nashville_Forecast_Results<-cbind(Index_Point=c(91+(3*w)):(93+(3*w)), Nashville_forecast_ARIMAX$mean, MAE=Nashville_Accuracy[1,3])
  Nashville_Printed_Results =rbind(Nashville_Printed_Results,Nashville_Forecast_Results)
}
Nashville_Printed_Results

Nashville_Forecast_Now<-cbind(Index_Point=c(118), Nashville_forecast_X$mean, MAE=0)
Nashville_Printed_Results =rbind(Nashville_Printed_Results,Nashville_Forecast_Now)
Nashville_Printed_Results


write.csv(Nashville_Printed_Results, file="Nashville Arima Results.csv")

#--------------------------  Nashville - END --------------------------------------#





#--------------------------  Tampa - START --------------------------------------#

Tampa_google_var= google_var[which (google_var$ï..MSA =="Tampa"),]

Tampa_google_var$Month = ts(Tampa_google_var$Month, start=c(2008,03), end =c(2017,11), frequency=12)

Tampa_corr_matrix=cor(Tampa_google_var[,3:8], use="complete.obs")
Tampa_corr_matrix

acf(Tampa_google_var$zillowhousesales, na.action=na.pass)
pacf(Tampa_google_var$zillowhousesales, na.action=na.pass)

adf.test(Tampa_google_var$zillowhousesales, alternative="stationary")
Tampa_fit=auto.arima(Tampa_google_var$zillowhousesales)
Tampa_fit

tsdiag(Tampa_fit)

Tampa_forecast= forecast(Tampa_fit,h=3)
plot(Tampa_forecast)


Tampa_xreg= Tampa_google_var[,4:8]

Tampa_fit_X=auto.arima(Tampa_google_var$zillowhousesales, xreg=Tampa_xreg)
tsdiag(Tampa_fit_X)

Tampa_forecast_xreg=google_var_orig[which (google_var_orig$ï..MSA =="Tampa" & 
                                                       (google_var_orig$Month=="2017-12")),4:8]

Tampa_forecast_X= forecast(Tampa_fit_X,xreg=Tampa_forecast_xreg, h=3)
plot(Tampa_forecast_X)


Tampa_Printed_Results=NULL
for (w in 1:8) {
  Tampa_Forecast_Results=NULL
  Tampa_ARIMAX <- Tampa_google_var[1:(90+(3*w)),]
  
  Tampa_ARIMAX_Predict <- Tampa_google_var[(91+(3*w)):(93+(3*w)),4:8]
  
  Tampa_ARIMAX_Actual <- Tampa_google_var[(91+(3*w)):(93+(3*w)),3]
  
  Tampa_ARIMAX_xreg= Tampa_ARIMAX[,4:8]
  
  Tampa_fit_ARIMAX=auto.arima(Tampa_ARIMAX$zillowhousesales, xreg=Tampa_ARIMAX_xreg)
  
  tsdiag(Tampa_fit_ARIMAX)
  
  Tampa_forecast_ARIMAX= forecast(Tampa_fit_ARIMAX,xreg=Tampa_ARIMAX_Predict, h=3)
  plot(Tampa_forecast_ARIMAX)
  Tampa_forecast_ARIMAX
  Tampa_ARIMAX_Actual
  
  Tampa_Accuracy=data.frame(accuracy(Tampa_forecast_ARIMAX,Tampa_ARIMAX_Actual ))
  Tampa_Accuracy
  
  Tampa_Forecast_Results<-cbind(Index_Point=c(91+(3*w)):(93+(3*w)), Tampa_forecast_ARIMAX$mean, MAE=Tampa_Accuracy[1,3])
  Tampa_Printed_Results =rbind(Tampa_Printed_Results,Tampa_Forecast_Results)
}
Tampa_Printed_Results

Tampa_Forecast_Now<-cbind(Index_Point=c(118), Tampa_forecast_X$mean, MAE=0)
Tampa_Printed_Results =rbind(Tampa_Printed_Results,Tampa_Forecast_Now)
Tampa_Printed_Results


write.csv(Tampa_Printed_Results, file="Tampa Arima Results.csv")

#--------------------------  Tampa - END --------------------------------------#