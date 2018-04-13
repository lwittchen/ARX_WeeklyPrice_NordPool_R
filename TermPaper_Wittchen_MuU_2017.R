# 2017/03
# Term paper
# Wittchen, Lennart
# Modul: Markt und Unternehmensspiel
# Fitting ARX and GARCH-Models to weekly electricity prices from Nord Pool AS

#import packages
library(lmtest)
library(tseries) #adf
library(timeSeries) #as.timeseries
library(forecast) #MAPE
library(faraway)
library(CircStats) #edf plotten
library(psych) #pairs.panel
library(bsts) #white-Test
library(leaps) #regsubsets
library(rugarch) #Garch-Modelle
library(forecast)

#############################################################################################

#import data
WPrice_data <- read.csv2("WeeklyPrices_data.csv")
Inflow_data <- read.csv2("RInflow_data.csv")
colnames(Inflow_data) <- c("year", "week", "dummy", "inf_norwary", "inf_sweden")
RLevel_data <- read.csv2("RLevel_data.csv")
colnames(RLevel_data) <- c("year", "week", "dummy", "max_level", "act_level" , "act_perc_level")

#Filter data
WPrice_data_2000 <- subset(WPrice_data, Year >= 2000 & Year <= 2010)
Inflow_data_2000 <- subset(Inflow_data, year >= 2000 & year <= 2010)
RLevel_data_2000 <- subset(RLevel_data, year >= 2000 & year <= 2010)
WPrice_data_2010 <- subset(WPrice_data, Year >= 2010 & Year <= 2015)
Inflow_data_2010 <- subset(Inflow_data, year >= 2010 & year <= 2015)
RLevel_data_2010 <- subset(RLevel_data, year >= 2010 & year <= 2015)
WPrice_data_2010 <- subset(WPrice_data, Year >= 2010 & Year <= 2015)
Inflow_data_2010 <- subset(Inflow_data, year >= 2010 & year <= 2015)
RLevel_data_2010 <- subset(RLevel_data, year >= 2010 & year <= 2015)
WPrice_data_2013 <- subset(WPrice_data, Year >= 2013 & Year <= 2014)

#Regressionszeitreihen erstellen für das Regressionsmodell von Kristiansen
#Lag von Price
n <- nrow(WPrice_data_2000)
RegPrice_2000 <- c(0,WPrice_data_2000$Price[-n])
WPrice_data_2000 <- data.frame(WPrice_data_2000, RegPrice_2000)
#Summe der Inflows mit Lag
n <- nrow(Inflow_data_2000)
RegInflow_2000 <- c(0,Inflow_data_2000$inf_norwary[-n]+Inflow_data_2000$inf_sweden[-n])
Inflow_data_2000 <- data.frame(Inflow_data_2000, RegInflow_2000)
#Delta der RLevel
n <- nrow(RLevel_data_2000)
RegLevel_2000 <- c(0,0,RLevel_data_2000$act_perc_level[2:(n-1)]/RLevel_data_2000$act_perc_level[1:(n-2)])
RLevel_data_2000 <- data.frame(RLevel_data_2000, RegLevel_2000)

#Regressionszeitreihen erstellen für das GARCH-Modell
#Lag von Price
n <- nrow(WPrice_data_2010)
LRegPrice_2010 <- c(0,WPrice_data_2010$Price[-n])
WPrice_data_2010 <- data.frame(WPrice_data_2010, LRegPrice_2010)
#Delta der Preise
DRegPrice_2010 <- c(0,0,WPrice_data_2010$Price[2:(n-1)]/WPrice_data_2010$Price[1:(n-2)])
WPrice_data_2010 <- data.frame(WPrice_data_2010, DRegPrice_2010)
#Summe der Inflows mit Lag
n <- nrow(Inflow_data_2010)
LRegInflow_2010 <- c(0,Inflow_data_2010$inf_norwary[-n]+Inflow_data_2010$inf_sweden[-n])
Inflow_data_2010 <- data.frame(Inflow_data_2010, LRegInflow_2010)
#Delta der Inflows
Inflow_2010 <- c(Inflow_data_2010$inf_norwary+Inflow_data_2010$inf_sweden)
Inflow_data_2010 <- data.frame(Inflow_data_2010, Inflow_2010)
DInflow_2010 <- c(0,0,Inflow_data_2010$Inflow_2010[2:(n-1)]/Inflow_data_2010$Inflow_2010[1:(n-2)])
Inflow_data_2010 <- data.frame(Inflow_data_2010, DInflow_2010)
#Delta der RLevel
n <- nrow(RLevel_data_2010)
DRegLevel_2010 <- c(0,0,RLevel_data_2010$act_perc_level[2:(n-1)]/RLevel_data_2010$act_perc_level[1:(n-2)])
RLevel_data_2010 <- data.frame(RLevel_data_2010, DRegLevel_2010)
#Lag von RLevel
LRegLevel_2010 <- c(0,RLevel_data_2010$act_perc_level[-1]) 
RLevel_data_2010 <- data.frame(RLevel_data_2010, LRegLevel_2010)


#Daten zusammenfassen
RegMod_data_2000 <- data.frame(WPrice_data_2000$Year, WPrice_data_2000$Week, WPrice_data_2000$RegPrice_2000, Inflow_data_2000$RegInflow_2000, RLevel_data_2000$RegLevel_2000)
colnames(RegMod_data_2000) <- c("year", "week",  "price", "inflow", "rlevel")

RegMod_data_2010 <- data.frame(WPrice_data_2010$Year, WPrice_data_2010$Week, WPrice_data_2010$LRegPrice_2010,WPrice_data_2010$DRegPrice_2010, Inflow_data_2010$LRegInflow_2010, Inflow_data_2010$DInflow_2010, RLevel_data_2010$LRegLevel_2010, RLevel_data_2010$DRegLevel_2010)
colnames(RegMod_data_2010) <- c("year", "week", "lag_price", "del_price", "lag_inflow", "del_inflow", "lag_rlevel", "del_rlevel")

#Ersten zwei Werte verbrennen, da Rlevel bis Lag 2 geht
RegMod_data_2000 = RegMod_data_2000[-(1:2),]
WPrice_data_2000 = WPrice_data_2000[-(1:2),]
RegMod_data_2010 = RegMod_data_2010[-(1:2),]

#log
RegMod_data_2000$price <- log(RegMod_data_2000$price)
RegMod_data_2000$inflow  <- log(RegMod_data_2000$inflow)
RegMod_data_2000$rlevel <- log(RegMod_data_2000$rlevel)
WPrice_data_2000$Price <- log(WPrice_data_2000$Price)
RegMod_data_2010$lag_price <- log(RegMod_data_2010$lag_price)
RegMod_data_2010$del_price <- log(RegMod_data_2010$del_price)
RegMod_data_2010$lag_inflow  <- log(RegMod_data_2010$lag_inflow)
RegMod_data_2010$del_inflow  <- log(RegMod_data_2010$del_inflow)
RegMod_data_2010$lag_rlevel <- log(RegMod_data_2010$lag_rlevel)
RegMod_data_2010$del_rlevel <- log(RegMod_data_2010$del_rlevel)
WPrice_data_2010$Price <- log(WPrice_data_2010$Price)
WPrice_data_2013$Price <- log(WPrice_data_2013$Price)

#demean
price_mean <- mean(WPrice_data_2000$Price)
price_mean_2010 <- mean(WPrice_data_2010$Price) 
price_mean_2013 <- mean(WPrice_data_2013$Price)
RegMod_data_2000$price_dm <- RegMod_data_2000$price - price_mean
WPrice_data_2000$Price_dm <- WPrice_data_2000$Price - price_mean
RegMod_data_2010$price_dm <- RegMod_data_2010$lag_price - price_mean_2010
WPrice_data_2010$Price_dm <- WPrice_data_2010$Price - price_mean_2010
WPrice_data_2013$Price_dm <- WPrice_data_2013$Price - price_mean_2013

#############################################################################################

#ADF und PP
ts_wprice <- as.timeSeries(WPrice_data_2000$Price_dm)
adf.test(ts_wprice)
pp.test(ts_wprice)

#ACF und PACF für Preise
layout(1:2)
acf(WPrice_data_2000$Price_dm)
pacf(WPrice_data_2000$Price_dm)

#Varianceinflationfactor / Multikollinearität
VIF_reg_price <- summary(lm(price_dm ~ inflow + rlevel, data = RegMod_data_2000))
VIF_price <- 1/(1- VIF_reg_price$r.squared)

VIF_reg_inflow <- summary(lm(inflow ~ price_dm + rlevel, data = RegMod_data_2000))
VIF_inflow <- 1/(1- VIF_reg_inflow$r.squared)

VIF_reg_rlevel <- summary(lm(rlevel ~ price_dm + inflow, data = RegMod_data_2000))
VIF_rlevel <- 1/(1- VIF_reg_rlevel$r.squared)

VIF_price
VIF_inflow
VIF_rlevel

cor(RegMod_data_2000$price, RegMod_data_2000$inflow)
cor(RegMod_data_2000$price, RegMod_data_2000$rlevel)
cor(RegMod_data_2000$inflow, RegMod_data_2000$rlevel)


#############################################################################################

#Regression Model erstellen

#Reg Model aufstellen
reg_model <- lm(WPrice_data_2000$Price_dm ~ price_dm + inflow + rlevel, data = RegMod_data_2000)
reg_model_sum <- summary(reg_model)
reg_model_sum


#MAPE berechnen
price_real_dm <- exp(WPrice_data_2000$Price_dm)
forecast_real_dm <- exp(reg_model$fitted.values)
n <- length(price_real_dm)
MAPE_reg_model <- 1/n * sum(abs((price_real_dm - forecast_real_dm)/price_real_dm))
MAPE_reg_model
accuracy(reg_model) #MAPE des Regressionsmodel, nicht gültig da Log(Price)

#Mean Error (ME)
ME_reg_model <- mean(exp(reg_model$fitted.values+price_mean) - exp(WPrice_data_2000$Price_dm+price_mean))
ME_reg_model

#Absolute Error (MAE)
MAE_reg_model <- 1/n * sum(abs((exp(reg_model$fitted.values+price_mean)) - (exp(WPrice_data_2000$Price_dm+price_mean))))
MAE_reg_model

#############################################################################################

#Myopic model
myop_model_resid <- WPrice_data_2000$Price_dm - RegMod_data_2000$price_dm

#MAPE
price_real_dm <- exp(WPrice_data_2000$Price_dm)
forecast_myop_real_dm <- exp(RegMod_data_2000$price_dm)
n <- length(price_real_dm)
MAPE_myop_model <- 1/n * sum(abs((price_real_dm - forecast_myop_real_dm)/price_real_dm))
MAPE_myop_model

#ME
ME_myop_model <- mean(exp(RegMod_data_2000$price_dm+price_mean) - exp(WPrice_data_2000$Price_dm+price_mean))
ME_myop_model

#MAE
MAE_reg_model <- 1/n * sum(abs((exp(RegMod_data_2000$price_dm+price_mean)) - (exp(WPrice_data_2000$Price_dm+price_mean))))
MAE_reg_model

#############################################################################################

#Vergleich der Modell
reg_model_sum


layout(1)
n <- nrow(WPrice_data_2000)
plot(WPrice_data_2000$Price_dm[(n-52):n], type = "l", col = "darkgreen", lwd = 2, ylab = "Log(Spotprice)", main = "Vergleich - Prognosewerte und reale Spotpreise im Jahr 2010")
lines(reg_model$fitted.values[(n-52):n], lty = 6 , col = "darkred", lwd = 3)
lines(RegMod_data_2000$price_dm[(n-52):n], lty = 5, col = "darkblue", lwd = 2)

#############################################################################################
#Residuen

par(mfcol=c(2,1), mar=c(2.5,4.5,2,1), font.lab = 2, font.axis = 1, cex = 0.9, ann = TRUE)
regmod_residuen <- ts(reg_model$residuals, start = c(2000), frequency = 52)
plot(regmod_residuen, ylab = "Residuen", type = "p")
acf(reg_model$residuals, main = "")
pacf(reg_model$residuals, main = "")

layout(1)
bp <- seq(-0.5,0.5,0.1)
hist(reg_model$residuals, bp)

x <- rnorm(10000)
plot.edf(reg_model$residuals)
par(new=TRUE)
plot.edf(x)

#Verteilung der Residuen
par(oma = c(0,0,0,0))
plot(density((reg_model$residuals-mean(reg_model$residuals) )/sd(reg_model$residuals)), col = "blue", main = "Dichtefunktion der Residuen")
x<-seq(-6,6,by=0.01)
y<-dnorm(x,0,1)
lines(x,y, col = "red")
legend(-5.5,0.5, legend =  c("Vert. der Residuen", "Normalverteilung"), lty=c(1,1),col = c("blue","red"))

#H0: Homoskedastizität
white.test(regmod_residuen)
#H0: Unkorreliert
Box.test(reg_model$residuals)

#############################################################################################
#Auswahl der Sample-Period
Spotprice <- ts(WPrice_data_2010$Price, start = c(2010), frequency = 52)
par(mfcol=c(2,1), mar=c(2.5,4.5,2,1), font.lab = 2, font.axis = 1, cex = 0.9)
plot(log(Spotprice))
plot(diff(log(Spotprice)))
abline(v=c(2011.5,2013,2015), lty = 2, col = 9)

#Zeitraum 2013 und 2014
RegMod_data_2013 <- subset(RegMod_data_2010, year>2012 &  year < 2015)
RegMod_data_2013$price_dm <- RegMod_data_2013$lag_price - price_mean_2013

#Auswahl der Regressoren
#Daten von 2010 bis 2015 nutzen
log_price_2013 <- WPrice_data_2013$Price
log_price_2013_dm <- log_price_2013 - price_mean_2013

#Streudiagrammmatrix
GARCHMod_StrDia <-  data.frame(log_price_2013_dm,RegMod_data_2013[,(3:(ncol(RegMod_data_2013)-1))])
colnames(GARCHMod_StrDia) <- c("price_t", "price_t-1",  "d_price_t-1", "inflow_t-1", "d_inflow_t-1", "rLevel_t-1", "d_rLevel_t-1" )
pairs.panels(GARCHMod_StrDia, main = "Streudiagrammmatrix")

#regsubs zeigt, welche Faktoren man nacheinander zu dem Modell hinzufügen sollte. 
regsubs <- regsubsets(log_price_2013_dm ~.,data=GARCHMod_StrDia[-1])
summary(regsubs)


#############################################################################################

# #ARIMA-Modelle erzeugen
# arima_111 <- arima(log_price_2010_dm, order = c(1,1,1))
# arima_111_fit <- fitted(arima_111)
# plot(arima_111_fit)
# n <- length(log_price_2010_dm)
# MAPE_arima_model <- 1/n * sum(abs((exp(log_price_2010_dm) - exp(arima_111_fit))/exp(log_price_2010_dm)))
# MAPE_arima_model
# 
# arma_111 <- arma(log_price_2010_dm, order = c(1,1))
# arma_111_fit <- arma_111$fitted.values
# MAPE_arma_model <- 1/n * sum(abs((exp(log_price_2010_dm[-1]) - exp(arma_111_fit[-1]))/exp(log_price_2010_dm[-1])))
# MAPE_arma_model

#############################################################################################

#GARCH-Modelle erzeugen
#Ex. Daten für das GARCH-Modell
garch_data_1 <- as.matrix(RegMod_data_2013$lag_inflow)
garch_data_2 <- as.matrix(RegMod_data_2013$lag_rlevel)
garch_data_3 <- as.matrix(cbind(RegMod_data_2013$lag_inflow, RegMod_data_2013$lag_rlevel))
garch_data_4 <- as.matrix(cbind(RegMod_data_2013$del_inflow, RegMod_data_2013$del_rlevel))
garch_data_5 <- as.matrix(cbind(RegMod_data_2013$lag_inflow, RegMod_data_2013$del_rlevel))


#############################################################################################
#3 verschiedene Modelle, Inflow und Rlevel, jeweils Delta oder Lag
fit11.spec <- ugarchspec(variance.model     = list(model = "sGARCH", garchOrder = c(0, 1)), 
                        mean.model         = list(armaOrder = c(1, 0),include.mean = FALSE,
                                                  external.regressors = garch_data_1), 
                        distribution.model = "norm")
fit12.spec <- ugarchspec(variance.model     = list(model = "sGARCH", garchOrder = c(0, 1)), 
                           mean.model         = list(armaOrder = c(1, 0),include.mean = FALSE,
                                                     external.regressors = garch_data_2), 
                           distribution.model = "norm")
fit13.spec <- ugarchspec(variance.model     = list(model = "sGARCH", garchOrder = c(0, 1)), 
                           mean.model         = list(armaOrder = c(1, 0),include.mean = FALSE,
                                                     external.regressors = garch_data_3), 
                           distribution.model = "norm")
fit14.spec <- ugarchspec(variance.model     = list(model = "sGARCH", garchOrder = c(0, 1)), 
                         mean.model         = list(armaOrder = c(1, 0),include.mean = FALSE,
                                                   external.regressors = garch_data_4), 
                         distribution.model = "norm")
fit15.spec <- ugarchspec(variance.model     = list(model = "sGARCH", garchOrder = c(0, 1)), 
                         mean.model         = list(armaOrder = c(1, 0),include.mean = TRUE,
                                                   external.regressors = garch_data_5), 
                         distribution.model = "norm")

garch_fit_11 <- ugarchfit(data = WPrice_data_2013$Price_dm, spec = fit11.spec, solver = "hybrid")
garch_fit_12 <- ugarchfit(data = WPrice_data_2013$Price_dm, spec = fit12.spec, solver = "hybrid")
garch_fit_13 <- ugarchfit(data = WPrice_data_2013$Price_dm, spec = fit13.spec, solver = "hybrid")
garch_fit_14 <- ugarchfit(data = WPrice_data_2013$Price_dm, spec = fit14.spec, solver = "hybrid")
garch_fit_15 <- ugarchfit(data = WPrice_data_2013$Price_dm, spec = fit15.spec, solver = "hybrid")

n <- length(garch_data_1)
MAPE_garch_11 <- 1/n * sum(abs((exp(WPrice_data_2013$Price_dm) - exp(fitted(garch_fit_11)))/exp(WPrice_data_2013$Price_dm)))
MAPE_garch_12 <- 1/n * sum(abs((exp(WPrice_data_2013$Price_dm) - exp(fitted(garch_fit_12)))/exp(WPrice_data_2013$Price_dm)))
MAPE_garch_13 <- 1/n * sum(abs((exp(WPrice_data_2013$Price_dm) - exp(fitted(garch_fit_13)))/exp(WPrice_data_2013$Price_dm)))
MAPE_garch_14 <- 1/n * sum(abs((exp(WPrice_data_2013$Price_dm) - exp(fitted(garch_fit_14)))/exp(WPrice_data_2013$Price_dm)))
MAPE_garch_15 <- 1/n * sum(abs((exp(WPrice_data_2013$Price_dm) - exp(fitted(garch_fit_15)))/exp(WPrice_data_2013$Price_dm)))

#Beispielergebnisse
garch_fit_13

#MAPE
MAPE_garch_13
#SSR
sum(residuals(garch_fit_13)^2)
#Bestimmtheitsma0
cor(fitted(garch_fit_33),WPrice_data_2013$Price_dm)^2 

par(mfcol=c(3,1),mar=c(2.5,4.5,2,1), ann = TRUE)
plot(ts(residuals(garch_fit_13), start = 2013, frequency = 52), ylab = "Residuen", main="Residuen des ARMAX(2,1)ARCH(1)-Modells")
acf(ts(residuals(garch_fit_13), start = 1, frequency = 1), main = "", ylab = "ACF")
pacf(ts(residuals(garch_fit_13), start = 1, frequency = 1), main = "", ylab = "PACF")


#############################################################################################
#Myopic model im neuen Zeitraum
myop_model_2013_resid <- WPrice_data_2013$Price_dm - RegMod_data_2013$price_dm 
#MAPE
n <- length(WPrice_data_2013$Price_dm)
MAPE_myop_model <- 1/n * sum(abs((exp(WPrice_data_2013$Price_dm) - exp(RegMod_data_2013$price_dm ))/exp(WPrice_data_2013$Price_dm)))
MAPE_myop_model
#SSR
sum(myop_model_2013_resid^2)
cor(WPrice_data_2013$Price_dm, RegMod_data_2013$price_dm)^2

# #Altes Regressionsmodell im neuen Zeitraum zum Vergleich
# n <- length(log_price_2013_dm)
# head(RegMod_data_2013)
# reg_model_2013 <- lm(WPrice_data_2013$Price_dm ~ price_dm + lag_inflow + del_rlevel, data = RegMod_data_2013)
# reg_model_2013_sum <- summary(reg_model_2013)
# MAPE_reg_model_2013 <- 1/n * sum(abs((exp(WPrice_data_2013$Price_dm) - exp(reg_model_2013$fitted.values)/exp(WPrice_data_2013$Price_dm))))
# MAPE_reg_model_2013
# head(RegMod_data_2013)
# head(WPrice_data_2013)

#############################################################################################
#Out of Sample forecast
#Daten
WPrice_data_2015 <- subset(WPrice_data_2010, Year >=2015)
garch_data_forecast <- subset(RegMod_data_2010, year >=2015)
garch_data_forecast <- as.matrix(cbind(garch_data_forecast$lag_inflow, garch_data_forecast$lag_rlevel))
Regdata_2013_2015 <- subset(RegMod_data_2010, year >=2013 & year <=2015)
Pricedata_2013_2015 <- subset(WPrice_data_2010, Year >=2013 & Year <=2015)
garch_data_33f <- as.matrix(cbind(Regdata_2013_2015$lag_inflow,Regdata_2013_2015$lag_rlevel))
#Specs
fit33f.spec <- ugarchspec(variance.model     = list(model = "sGARCH", garchOrder = c(0, 1)), 
                           mean.model         = list(armaOrder = c(2, 1),include.mean = FALSE,
                                                     external.regressors = garch_data_33f, 
                                                     include.mean = FALSE), 
                           distribution.model = "norm")
#Fit
garch_fit_33f      <- ugarchfit(data = Pricedata_2013_2015$Price_dm, 
                                spec = fit33f.spec, 
                                solver = "hybrid", 
                                out.sample = 52)

#Forecast
garch_33f_forecast <- ugarchforecast(garch_fit_33f, 
                                n.ahead = 1, 
                                n.roll = 52,
                                external.forecasts = list(mregfor = garch_data_forecast))
#Fitted Values
garch_33f_forecast_values <- ts(fitted(garch_33f_forecast)[1,], 
                                start = c(2015), 
                                frequency = 52)
ts_WPrice_data_2015 <- ts(WPrice_data_2015$Price_dm, start = c(2015), frequency = 52) 

#Plot of results
layout(1)
plot(ts_WPrice_data_2015, col = "blue", type = "b", ylab = "log(Spotpreis_dm)", main = "ARMAX-GARCH - Out-of-Sample-Prognose")
lines(garch_33f_forecast_values, col = "red")
legend(2015,-1,c("Actual Value","Forecast"), pch = c(1,4), col = c("blue", "red"))

n = length(garch_33f_forecast_values)
MAPE_garch33_forecast <- 1/n * sum(abs(exp(garch_33f_forecast_values)-exp(WPrice_data_2015$Price_dm))/exp(WPrice_data_2015$Price_dm))
MAPE_garch33_forecast
ME_garch33_forecast <- mean(exp(garch_33f_forecast_values+price_mean_2013)-exp(WPrice_data_2015$Price_dm+price_mean_2013))
ME_garch33_forecast
MAE_garch33_forecast <- mean(abs(exp(garch_33f_forecast_values+price_mean_2013)-exp(WPrice_data_2015$Price_dm+price_mean_2013)))
MAE_garch33_forecast

garch_33f_residuen <- garch_33f_forecast_values - WPrice_data_2015$Price_dm 

par(mfcol=c(2,1), mar=c(2.5,4.5,2,1), font.lab = 2, font.axis = 1, cex = 0.9)
plot(garch_33f_residuen)
acf(garch_33f_residuen)
pacf(garch_33f_residuen)
                  
################################################################################
#Zum Vergleich ein ARX-Modell
fitar1f.spec <- ugarchspec(variance.model     = list(model = "sGARCH", garchOrder = c(0, 1)), 
                          mean.model         = list(armaOrder = c(1, 0),include.mean = FALSE,
                                                    external.regressors = garch_data_33f, 
                                                    include.mean = FALSE), 
                          distribution.model = "norm")
#Fit
garch_fit_ar1f      <- ugarchfit(data = Pricedata_2013_2015$Price_dm, 
                                spec = fitar1f.spec, 
                                solver = "hybrid", 
                                out.sample = 52)
garch_fit_ar1f
#Forecast
garch_ar1_forecast <- ugarchforecast(garch_fit_ar1f, 
                                     n.ahead = 1, 
                                     n.roll = 52,
                                     external.forecasts = list(mregfor = garch_data_forecast))
#Fitted Values
garch_ar1_forecast_values <- ts(fitted(garch_ar1_forecast)[1,], 
                                start = c(2015), 
                                frequency = 52)
#Plot
layout(1)
plot(ts_WPrice_data_2015, col = "blue", type = "b", ylab = "log(Spotpreis_dm)", main = "ARX-GARCH - Out-of-Sample-Prognose")
lines(garch_ar1_forecast_values, col = "red")
legend(2015,-1,c("Actual Value","Forecast"), pch = c(1,4), col = c("blue", "red"))


n = length(garch_ar1_forecast_values)
MAPE_garchar1_forecast <- 1/n * sum(abs(exp(garch_ar1_forecast_values)-exp(WPrice_data_2015$Price_dm))/exp(WPrice_data_2015$Price_dm))
ME_garchar1_forecast <- mean(exp(garch_ar1_forecast_values+price_mean_2013)-exp(WPrice_data_2015$Price_dm+price_mean_2013))
MAE_garchar1_forecast <- mean(abs(exp(garch_ar1_forecast_values+price_mean_2013)-exp(WPrice_data_2015$Price_dm+price_mean_2013)))

