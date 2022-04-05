
# Import libraries ------------------------------------------------------


## Install packages --------------------------------------------------------


install.packages('ggplot2') # visualisation
install.packages('scales') # visualisation
install.packages('patchwork') # visualisation
install.packages('RColorBrewer') # visualisation
install.packages('corrplot') # visualisation

install.packages('readr') # input/output
install.packages('vroom') # input/output
install.packages('skimr') # overview
install.packages('tibble') # data wrangling
install.packages('tidyr') # data wrangling
install.packages('purrr') # data wrangling
install.packages('stringr') # string manipulation
install.packages('forcats') # factor manipulation
install.packages('fuzzyjoin') # data wrangling
install.packages('alluvial') # visualisation
install.packages('ggrepel') # visualisation
install.packages('ggforce') # visualisation
install.packages('ggridges') # visualisation
install.packages('gganimate') # animations
install.packages('GGally') # visualisation
install.packages('ggthemes') # visualisation
install.packages('wesanderson') # visualisation
install.packages('kableExtra') # display
install.packages('lubridate') # date and time
install.packages('forecast') # time series analysis
install.packages('timetk') # time series analysis
install.packages('crosstalk')
install.packages('plotly')
install.packages("foreach")
install.packages("doParallel")
install.packages("rlang")

install.packages('dplyr') # data manipulation
install.packages("rlang")
install.packages("xts")
install.packages("forecast")
install.packages("tsutils")
install.packages("imputES")
install.packages("tseries")
install.packages("readxl")
install.packages("xts")
install.packages("seastests")
install.packages("tinytex")
install.packages("tsibble")
install.packages("dplyr")
install.packages("outliers")
install.packages("moments")
install.packages("VIM")
install.packages("naniar")
install.packages("ggplot2")
install.packages("imputeTS")
install.packages("knitr")
install.packages("regclass")

## Import Libraries --------------------------------------------------------

# general visualisation
library('ggplot2') # visualisation
library('scales') # visualisation
library('patchwork') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation
# general data manipulation
library('dplyr') # data manipulation
library('readr') # input/output
library('vroom') # input/output
library('skimr') # overview
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('purrr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation
library('fuzzyjoin') # data wrangling
# specific visualisation
library('alluvial') # visualisation
library('ggrepel') # visualisation
library('ggforce') # visualisation
library('ggridges') # visualisation
library('gganimate') # animations
library('GGally') # visualisation
library('ggthemes') # visualisation
library('wesanderson') # visualisation
library('kableExtra') # display
# Date + forecast
library('lubridate') # date and time
library('forecast') # time series analysis
library('timetk') # time series analysis
# Interactivity
library('crosstalk')
library('plotly')
# parallel
library('foreach')
library('doParallel')
library('tseries')
library('xts')
library('SWMPr')
library("forecast")
library("tsutils")
library("imputES")
library("tseries")
library("readxl")
library("xts")
library("seastests")
library("tinytex")
library("tsibble")
library("dplyr")
library("outliers")
library("moments")
library("VIM")
library("naniar")
library("ggplot2")
library("imputeTS")
library("knitr")
library("regclass")

# Load the dataset --------------------------------------------------------

#dataset <- vroom(str_c('sales_train_evaluation.csv'), delim = ",", col_types = cols())
dataset <- read.csv("dota2 Players.csv", stringsAsFactors = TRUE)
View(dataset)



dota2_daily <- ts(dataset$Players, frequency = 365, start=c(2011,265))

View(dota2_daily)
str(dota2_daily)

# Create monthly and daily data ------------------------------------------

dates <- seq(as.Date("2011-09-22"), length = 3814, by = "days")
data <- dataset[,c(1:5)]

xts_lxy <- xts(x=data,order.by = dates)
#View(xts_lxy)

monthly_mean <- apply.monthly(xts_lxy,mean)
#View(monthly_mean)
head(monthly_mean)
summary(daily_mean)

daily_mean <- apply.daily(xts_lxy,mean)
#View(daily_mean)
head(daily_mean)

forecast <- read

yearly_mean <- apply.yearly(xts_lxy,mean)
#View(yearly_mean)

# Plot the monthly and daily dataset -------------------------------------

par(mfrow=c(2,2))
ggseasonplot(ts(monthly_mean,frequency = 12)[,1],year.labels=TRUE)

# Convert the dataset into time series


# Create training and testing dataset -------------------------------------

## Train and test dataset for daily series --------------------------------
dota2_var <- as.data.frame(dota2_var)
train_daily_dota2 <- ts(dota2_var$Players[c(1:1815)],frequency = 365)
test_daily_dota2 <- (dota2_var$Players[c(1816:2420)])

daily_dota2 <- ts(dota2_var$Players,frequency = 365, start=c(2015,198))




train_daily_dota2
str(test_daily_dota2)

# Set the horizon
h <- 605


# Summary of dataset
summary(dota2_daily)

# Box plot the outliers
boxplot(dota2_daily)

# Check how many are the outliers
boxplot.stats(dota2_daily)

# Frequency of dataset 
frequency(dota2_daily)

# Decomposition of the dataset using decompose function -------------------

decomp_daily<- decompose(dota2_daily)
plot(decomp_daily)

ggseasonplot(dota2_daily)

# Plot the season
seasplot(dota2_daily,m = 365)

#ACF and PACF analysis on the entire dataset

## ACF and PACF analysis on the daily dataset -----------------------------

tsdisplay(dota2_daily)

# Perform KPSS and ADF test on time series

## Perform KPSS and ADF test on daily time series -----------------------------
kpss.test(dota2_daily)

adf.test(dota2_daily)

test_daily_dota2

# Seasonal Naive Model for daily Series ----------------------------------

fc_snaive_dota2_daily <- snaive(train_daily_dota2, h=h)$mean
accuracy(fc_snaive_dota2_daily, test_daily_dota2)

# ETS Model for daily series ---------------------------------------------

ets_dota2_daily <- es(train_daily_dota2, model = "ANA")
fc_ets_dota2_daily <- forecast(ets_dota2_daily, h=h)$mean
accuracy(fc_ets_dota2_daily, test_daily_dota2)
ets_dota2_daily
fc_ets_dota2_daily

# Seasonal ARIMA Model for daily series  --------------------------------

arima_dota2_daily <- auto.arima(train_daily_dota2)
fc_arima_dota2_daily <- forecast(arima_dota2_daily, h=h)$mean
accuracy(fc_arima_dota2_daily, test_daily_dota2)
tsdisplay(fc_arima_dota2_daily)
arima_dota2_daily




# VAR Model ---------------------------------------------------------------
dota2 <- read.csv("dota2 with only twitch.csv")
View(dota2)

dota2_var <- cbind(dota2$Players, dota2$Twitch.Viewers)
colnames(dota2_var) <- c("Players","Twitch.Viewers")
View(dota2_var)

train_daily_dota2_var <- ts(dota2_var[c(1:1815),],frequency = 365)
test_daily_dota2_var <- (dota2_var[c(1816:2420),])
h_var <- 605
View(test_daily_dota2)
View(train_daily_dota2)

test_daily_dota2_var <- as.data.frame(test_daily_dota2_var)

varselect_para <- VARselect(dota2_var, type="both", season=365)
varselect_para

varselect_para$selection
VAR_model <- VAR(train_daily_dota2_var, p=10, type="both", season=365)
VAR_model
plot(VAR_model)

varforecast <- predict(VAR_model,n.ahead=h_var)$fcst
varforecastt <- data.frame(forecast(VAR_model,h=h_var)$fcst)
varforecast
varforecastt
par(mfcol=c(1,1))
plot(predict(VAR_model,n.ahead=14))


forecast1 <- data.frame(predict(VAR_model,n.ahead=h_var)$fcst)
forecast1$Lo.80 <- NULL
forecast1$Hi.80 <- NULL
forecast1$Lo.95 <- NULL
forecast1$Hi.95 <- NULL

forecast1$Players.lower <- NULL
forecast1$Players.upper <- NULL
forecast1$Twitch.Viewers.lower <- NULL
forecast1$Twitch.Viewers.upper <- NULL
forecast1$Players.CI <- NULL
forecast1$Twitch.Viewers.CI <- NULL
forecast1

accuracy(forecast1$Players.fcst,test_daily_dota2_var$Players)

forecast(VAR_model, h=14)

?forecast()

VAR_model_IRF <- irf(VAR_model)
plot(VAR_model_IRF)

# ETSX Model --------------------------------------------------------------

adamETSX <- adam(dota2_var, "ANA", lags=365, formula=Players~log(Twitch.Viewers), regressors="adapt")
summary(adamETSX)
forecast_ETSX <- pre
par(mfcol=c(3,3))
plot(adamETSX,c(1,2,3,4,6,7,8,10,11))


# Summary tables ----------------------------------------------------------

## Summary table for DOTA 2 ----------------------------------------------------------
summary_daily_dota2 <- matrix(c(accuracy(fc_snaive_dota2_daily, test_daily_dota2)), ncol=5, byrow=TRUE)
summary_daily_dota2 <- as.table(summary_daily_dota2)
summary_daily_dota2 <- rbind(summary_daily_dota2,c(accuracy(fc_ets_dota2_daily, test_daily_dota2)))
summary_daily_dota2 <- rbind(summary_daily_dota2,c(accuracy(fc_arima_dota2_daily, test_daily_dota2)))
summary_daily_dota2 <- rbind(summary_daily_dota2,c(accuracy(forecast1$Players.fcst,test_daily_dota2_var$Players)))
colnames(summary_daily_dota2) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_daily_dota2) <- c('Seasonal Naive Model for daily freq. of DOTA 2 dataset','ETS Model for daily freq. of DOTA 2 dataset','ARIMA Model for daily freq. of DOTA 2 dataset','VAR Model for daily freq. of DOTA 2 dataset')
summary_daily_dota2
knitr::kable(summary_daily_dota2)



dates <- seq(as.Date("2011-01-29"), length = 1941, by = "days")
data <- total_2

ts_data <- ts(monthly_mean,frequency = 12)
ts_data

train <- ts(ts_data[c(1:45),],frequency = 12)
train
test <- ts(ts_data[c(45:65),],frequency = 12)

diff_data <- diff(train,differences=1)
head(diff_data)

a <- VARselect(diff_data, type="both", season=12)
a
a$selection

VAR_model <- VAR(train, p=1, type="both", season=12)
VAR_model

library(forecast)
library(tseries)

forecast(VAR_model,h= 6)

forecast1 <- data.frame(forecast(VAR_model,n.ahead=21)$fcst)
forecast1

forecast1$dota2.lower <- NULL 
forecast1$dota2.upper <- NULL
forecast1$dota2.CI <- NULL

forecast1

summary(total_2$HOUSEHOLD_1)
summary(forecast1$HOUSEHOLD_1)

test

ts(forecast1,frequency = 12)

data.frame(test)$HOUSEHOLD_1

accuracy(forecast1$dota2.fcst,data.frame(test)$dota2)
accuracy(forecast1$FOODS_2.fcst,data.frame(test)$FOODS_2)
accuracy(forecast1$FOODS_3.fcst,data.frame(test)$FOODS_3)

# Regression model - ELM Model --------------------------------------------

elmmodel <- elm(BJsalesInsample,xreg=as.matrix(window(BJsales.lead,1,120)))
BJELMForecast <- forecast(BJELM, h=30, xreg=as.matrix(BJsales.lead))

elmmodel <- elm(dota2_daily_dota2)
fc_elmmodel <- forecast(elmmodel, h=week_h)$mean
accuracy(fc_elmmodel, test_daily_dota2)

?elm
plot(fc_elmmodel)
elmmodel
str(total)

BJELMForecast <- forecast(BJELM, h=30, xreg=as.matrix(BJsales.lead))
dota2_daily

# Forecast for 6 months ---------------------------------------------------

## Seasonal Naive Model for daily Series ----------------------------------

fcshort_snaive_dota2_daily_dota2 <- snaive(dota2_daily_dota2, h=weekshort_h)$mean

## Seasonal Naive Model for Monthly Series ----------------------------------

fcshort_snaive_dota2_monthly_dota2 <- snaive(dota2_monthly_dota2, h=month_h)$mean

## Seasonal ARIMA Model for daily series ----------------------------------

fcshort_arima_dota2_daily_dota2 <- forecast(arima_dota2_daily_dota2, h=weekshort_h)$mean
plot(fcshort_arima_dota2_daily_dota2 <- forecast(arima_dota2_daily_dota2, h=weekshort_h),main = "Forecasts from Seasonal ARIMA (0,1,2)[1](2,1,2)[52] with drift model of FOODS 1", xlab="Time frame", ylab="Sales volume")
arima_dota2_daily_dota2



# Final forecast ----------------------------------------------------------

fcVar <- data.frame(predict(VAR_model,n.ahead=14)$fcst)
forecastVAR <- fcVar$Players.fcst
forecastVAR <- as.ts(forecastVAR)
forecastVAR

fcSnaive<- snaive(daily_dota2, h=14)$mean
fcSnaive

ets_dota2 <- es(daily_dota2, model = "ANA")
fcETS <- forecast(ets_dota2, h=14)$mean
fcETS

fcARIMA <- forecast(arima_dota2_daily, h=14)$mean
fcARIMA

actual <- read.csv("Actual values.csv")
actual <- as.data.frame(actual)
actual

accuracy(fcSnaive,as.ts(actual))
accuracy(fcETS,as.ts(actual))
accuracy(fcARIMA,as.ts(actual))
accuracy(forecastVAR,as.ts(actual))

MAPE(as.ts(actual),fcSnaive)*100
MAPE(as.ts(actual),fcETS)*100
MAPE(as.ts(actual),fcARIMA)*100
MAPE(as.ts(actual),forecastVAR)*100


