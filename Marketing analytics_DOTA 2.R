

# Load the libraries ------------------------------------------------------

library("greybox")
install.packages("fastDummies")
library("fastDummies")
library("dplyr")
library("vars")
library("forecast")
library("tseries")
library("xts")


# Import the dataset ------------------------------------------------------


dota2 <- read.csv("dota2 with only twitch.csv")
dota2 <- read.csv("dota2.csv")
View(dota2)
summary(dota2)
#dota2 <- read.csv("dota2 copy.csv")

dota2 <- read.csv("dota2 without NA.csv") 
View(dota2)

dota2_monthly <- read.csv("Dota2-Monthly.csv")
View(dota2_monthly)

dota2 <- ts(weekly_mean, frequency = 52, start = c(2015,38))

str(dota2)
View(dota2)

dates <- seq(as.Date("2011-08-17"), length = 3815, by = "days")
data <- dota2[,c(1:5)]

ts_dota2 <- xts(x=data,order.by = dates)
ts_dota2$DateTime <- NULL
View(ts_dota2)
plot(ts_dota2)
str(ts_dota2)

ts_dota2 <- as.ts(ts_dota2)


dates <- seq(as.Date("2011-08-17"), length = 2420, by = "days")
data <- dota2[,c(1:5)]

xts_lxy <- xts(x=data,order.by = dates)
View(xts_lxy)

monthly_mean <- apply.monthly(xts_lxy,mean)
#View(monthly_mean)
head(monthly_mean)
summary(weekly_mean)

weekly_mean <- apply.weekly(xts_lxy,mean)
#weekly_mean <- as.data.frame(weekly_mean)
View(weekly_mean)
head(weekly_mean)


# Stationery check ------------------------------------------------------

adf.test(ts_dota2[,1])
adf.test(ts_dota2[,2])

# Simple regression model -------------------------------------------------
model <- lm(Players~ log(Twitch.Viewers),data=dota2)
summary(model)
modelResiduals <- resid(model)
adf.test(modelResiduals)
plot.ts(modelResiduals)

# Regression diagnostics
par(mfcol=c(3,2))
plot(model,c(1,2,4,6,7,12))


# Create dummy variables for price ----------------------------------------

dota2_event_dummy <- dummy_cols(as.data.frame(dota2), select_columns = "Events")
dota2_event_dummy$Events_ <- NULL

dota2_event_dummy

colnames(dota2_event_dummy)[6:46] <- c("Asia_Championship","China_Dota2_Super_Major","DreamLeague_Season8","DreamLeague_Season11","DreamLeague_Season12","DreamLeague_Season13",
                                       "DreamLeague_Season14","DreamLeague_Season15","DreamLeague_Season16","DreamLeague_Season17","DreamLeague_Season18","DreamLeague_Season19","DreamLeague_Season20","DreamLeague_Season21",
                                       "EPICENTER_Major_2019","EPICENTER_Major_2020","EPICENTER_Major_2021","EPICENTER_Major_2022","EPICENTER_Major_2023","EPICENTER_Major_2024",
                                       "EPICENTER_Major_2025","EPICENTER_Major_2026","EPICENTER_Major_2027","EPICENTER_Major_XL","ESL_One_Birmingham","ESL_One_Hamburg","ESL_One_Katowice",
                                       "MDL_Changsha_Major","MDL_Chengdu_Major","MDL_Disneyland_Paris_Major","One_Esports_Singapore_Major","The_Boston_Major",
                                       "The_Bucharest_Major","The_Chongqing_Major","The_Frankfurt_Major","The_International","The_Kiev_Major","The_Kuala_Lumpur_Major",
                                       "The_Manila_Major","The_Shanghai_Major","WePlay_AniMajor" )


colnames(dota2_event_dummy)
View(dota2_event_dummy)


# Regression model with event dummy variable ------------------------------

event_dummy_model <- alm(Players~ log(Twitch.Viewers) + The_International,data=dota2_event_dummy )
summary(event_dummy_model)

# Regression diagnostics
par(mfcol=c(3,2))
plot(event_dummy_model,c(1,2,4,6,7,12))

# Twitch users lag regression model ------------------------------

twitch_users_Xreg <- xregExpander(dota2[,3], lags=c(-7:7), gaps = "zero")
twitch_users_Xreg <- as.data.frame(twitch_users_Xreg)
View(twitch_users_Xreg)
colnames(twitch_users_Xreg)[1:15] <- c("Twitch.Viewers","Twitch.Viewers_Lag7","Twitch.Viewers_Lag6","Twitch.Viewers_Lag5","Twitch.Viewers_Lag4","Twitch.Viewers_Lag3","Twitch.Viewers_Lag2","Twitch.Viewers_Lag1","Twitch.Viewers_Lead1","Twitch.Viewers_Lead2","Twitch.Viewers_Lead3","Twitch.Viewers_Lead4","Twitch.Viewers_Lead5","Twitch.Viewers_Lead6","Twitch.Viewers_Lead7")

#reorder by column index
#twitch_users_Xreg <- twitch_users_Xreg[, c(1,8,7,6,5,4,3,2)] # leave the row index blank to keep all rows

# Now merge the variables and correct the name of the response variable:
dota2_expanded <- cbind(Players=as.data.frame(dota2)$Players,twitch_users_Xreg)
View(dota2_expanded)

model_dota2_expanded <- alm(Players ~ log(Twitch.Viewers)  + log(Twitch.Viewers_Lag7) + log(Twitch.Viewers_Lag6)+ log(Twitch.Viewers_Lag5)+ log(Twitch.Viewers_Lag4) + log(Twitch.Viewers_Lag3) + log(Twitch.Viewers_Lag2) + log(Twitch.Viewers_Lag1) + (Twitch.Viewers_Lead1)+(Twitch.Viewers_Lead2)+ (Twitch.Viewers_Lead3)+(Twitch.Viewers_Lead4)+(Twitch.Viewers_Lead5)+ (Twitch.Viewers_Lead6)+(Twitch.Viewers_Lead7),data=dota2_expanded)
summary(model_dota2_expanded)


#log(Twitch.Viewers_Lag7)+log(Twitch.Viewers_Lag6)+log(Twitch.Viewers_Lag5)+log(Twitch.Viewers_Lag4)+log(Twitch.Viewers_Lag3)+log(Twitch.Viewers_Lag2)+log(Twitch.Viewers_Lag1)+log(Twitch.Viewers_Lead1)+log(Twitch.Viewers_Lead2)+ log(Twitch.Viewers_Lead3)+log(Twitch.Viewers_Lead4)+log(Twitch.Viewers_Lead5)+ log(Twitch.Viewers_Lead6)+log(Twitch.Viewers_Lead7)

# Regression diagnostics
par(mfcol=c(3,2))
plot(model_dota2_expanded,c(1,2,4,6,7,12))

# AR Models -------------------------------------------------------------

dota2ARModels <- vector("list",4)
dota2ARModels[[1]] <- alm(Players~log(Twitch.Viewers),dota2_expanded)
dota2ARModels[[2]] <- alm(Players~Twitch.Viewers+Twitch.Viewers_Lag1,dota2_expanded)
dota2ARModels[[3]] <- alm(Players~Twitch.Viewers,dota2_expanded,orders=c(1,0,0))
dota2ARModels[[4]] <- alm(Players~Twitch.Viewers+Twitch.Viewers_Lag1,dota2_expanded,orders=c(1,0,0))
names(dota2ARModels) <- c("AR(0,0)","AR(0,1)","AR(1,0)","AR(1,1)")
sapply(dota2ARModels,AICc)
dota2ARModels[[5]] <- alm(Players~Twitch.Viewers+Twitch.Viewers_Lag1,dota2_expanded,orders=c(2,0,0))
dota2ARModels[[6]] <- alm(Players~Twitch.Viewers+Twitch.Viewers_Lag1+Twitch.Viewers_Lag2,dota2_expanded,orders=c(1,0,0))
dota2ARModels[[7]] <- alm(Players~Twitch.Viewers+Twitch.Viewers_Lag1+Twitch.Viewers_Lag2,dota2_expanded,orders=c(2,0,0))
dota2ARModels[[8]] <- alm(Players~Twitch.Viewers+Twitch.Viewers_Lag1+Twitch.Viewers_Lag2+ Twitch.Viewers_Lag3 ,dota2_expanded,orders=c(3,0,0))
dota2ARModels[[9]] <- alm(Players~Twitch.Viewers+Twitch.Viewers_Lag1+Twitch.Viewers_Lag2+ Twitch.Viewers_Lag3+ Twitch.Viewers_Lag4 ,dota2_expanded,orders=c(4,0,0))
dota2ARModels[[10]] <- alm(Players~Twitch.Viewers+Twitch.Viewers_Lag1+Twitch.Viewers_Lag2+ Twitch.Viewers_Lag3+ Twitch.Viewers_Lag4 + Twitch.Viewers_Lag5 ,dota2_expanded,orders=c(5,0,0))
dota2ARModels[[11]] <- alm(Players~Twitch.Viewers+Twitch.Viewers_Lag1+Twitch.Viewers_Lag2+ Twitch.Viewers_Lag3+ Twitch.Viewers_Lag4 + Twitch.Viewers_Lag5 + Twitch.Viewers_Lag6 ,dota2_expanded,orders=c(6,0,0))
dota2ARModels[[12]] <- alm(Players~Twitch.Viewers+Twitch.Viewers_Lag1+Twitch.Viewers_Lag2+ Twitch.Viewers_Lag3+ Twitch.Viewers_Lag4 + Twitch.Viewers_Lag5 + Twitch.Viewers_Lag6 + Twitch.Viewers_Lag7,dota2_expanded,orders=c(7,0,0))
names(dota2ARModels)[5:12] <- c("AR(2,1)","AR(1,2)","AR(2,2)","AR(3,3)","AR(4,4)","AR(5,5)","AR(6,6)","AR(7,7)")
sapply(dota2ARModels,AICc)

par(mfcol=c(3,3))
plot(dota2ARModels[[1]],c(1,2,4,6,7,8,10,11,12))

a0 <- coef(dota2ARModels[[1]])[2]
a1 <- coef(dota2ARModels[[1]])[3]
phi1 <- coef(dota2ARModels[[1]])[4]
cValues <- vector("numeric",13)
cValues[1] <- a0
cValues[2] <- a1 + phi1 * cValues[1]
for(i in 3:13){cValues[i] <- phi1 * cValues[i-1]}

plot(c(0:12), cValues, type="l",ylab="Multipliers", xlab="Lags", main="Twitch viewers effect", col="darkblue")
grid()
sum(cValues)

dota2Forecasts <- sapply(lapply(dota2ARModels,predict,newdata=tail(dota2_expanded,14)),"[[","mean")
sqrt(apply((dota2Forecasts-as.vector(tail(dota2_expanded[,1],14)))^2,2,mean))
apply(abs(dota2Forecasts-as.vector(tail(dota2_expanded[,1],14))),2,mean)
dota2ARModelFinal <- alm(Players~Twitch.Viewers,dota2_expanded,ar=1)
dota2Forecasts <- predict(dota2ARModelFinal,newdata=data.frame(Twitch.Viewers=rep(mean(dota2_expanded[,2]),400)),interval="prediction")
par(mfcol=c(1,1))
plot(dota2Forecasts)

# Model with Lagged and event variable ------------------------------------

dota2_event_lagged <- cbind(dota2_event_dummy, dota2_expanded$Twitch.Viewers_Lag1, dota2_expanded$Twitch.Viewers_Lag2, dota2_expanded$Twitch.Viewers_Lag3, dota2_expanded$Twitch.Viewers_Lag4, dota2_expanded$Twitch.Viewers_Lag5, dota2_expanded$Twitch.Viewers_Lag6, dota2_expanded$Twitch.Viewers_Lag7)
View(dota2_event_lagged)
colnames(dota2_event_lagged)
colnames(dota2_event_lagged)[47:53] <- c("Twitch.Viewers_Lag1","Twitch.Viewers_Lag2","Twitch.Viewers_Lag3","Twitch.Viewers_Lag4","Twitch.Viewers_Lag5","Twitch.Viewers_Lag6","Twitch.Viewers_Lag7")



# ARDL Models -------------------------------------------------------------

lagged_theinternational <- xregExpander(dota2_event_dummy[,41], lags=c(-3:0), gaps = "zero")
dota2_event_lagged <- cbind(dota2_event_lagged,lagged_theinternational)
dota2_event_lagged$The_International <- NULL
colnames(dota2_event_lagged)[53:56] <- c("The_International","The_International_Lag3","The_International_Lag2","The_International_Lag1" )
dota2_event_lagged <- dota2_event_lagged[, c(1,8,7,6,5,4,3,2)] # leave the row index blank to keep all rows
colnames(dota2_event_lagged)
View(dota2_event_lagged)


dota2ARDLModels <- vector("list",5)
names(dota2ARDLModels) <- c("ARIDL(2,0,3)","ARIDL(1,0,3)","ARIDL(2,0,2)","ARIDL(1,0,2)","ARIDL(2,1,3)")
dota2ARDLModels[[1]] <- alm(Players~Twitch.Viewers+Twitch.Viewers_Lag2+Twitch.Viewers_Lag3+The_International+ The_International_Lag1 + The_International_Lag2 + The_International_Lag3,dota2_event_lagged,orders=c(2,0,0))
dota2ARDLModels[[2]] <- alm(Players~Twitch.Viewers+Twitch.Viewers_Lag2+Twitch.Viewers_Lag3,dota2_event_lagged,orders=c(1,0,0))
dota2ARDLModels[[3]] <- alm(Players~Twitch.Viewers+Twitch.Viewers_Lag2,dota2_event_lagged,orders=c(2,0,0))
dota2ARDLModels[[4]] <- alm(Players~Twitch.Viewers+Twitch.Viewers_Lag2,dota2_event_lagged,orders=c(1,0,0))
dota2ARDLModels[[5]] <- alm(Players~Twitch.Viewers+Twitch.Viewers_Lag2+Twitch.Viewers_Lag3,dota2_event_lagged,orders=c(2,1,0))
sapply(dota2ARDLModels,AICc)
dota2ARDLModels[[6]] <- stepwise(dota2_event_lagged,orders=c(1,1,0))
dota2ARDLModels[[7]] <- stepwise(dota2_event_lagged,orders=c(2,1,0))
dota2ARDLModels[[8]] <- stepwise(dota2_event_lagged,orders=c(1,2,0))
dota2ARDLModels[[9]] <- stepwise(dota2_event_lagged,orders=c(2,2,0))
names(dota2ARDLModels)[6:9] <- c("ARIDL(1,1,q)","ARIDL(1,2,q)","ARIDL(2,1,q)","ARIDL(2,2,q)")
sapply(dota2ARDLModels,AICc)

i <- which.min(sapply(dota2ARDLModels,AICc))
par(mfcol=c(3,3))
plot(dota2ARDLModels[[i]],c(1,2,4,6,7,8,10,11,12))

plot(forecast(dota2ARDLModels[[i]], h=10, interval="prediction", level=c(0.9,0.95,0.99)))

# ETSX Model --------------------------------------------------------------

adamETSX <- adam(dota2_expanded, "ANA", lags=52, formula=Players~log(Twitch.Viewers)+ Twitch.Viewers_Lag1 +Twitch.Viewers_Lag2+Twitch.Viewers_Lag3+Twitch.Viewers_Lag4+Twitch.Viewers_Lag5 + Twitch.Viewers_Lag6+ Twitch.Viewers_Lag7, regressors="adapt")
summary(adamETSX)
par(mfcol=c(3,3))
plot(adamETSX,c(1,2,3,4,6,7,8,10,11))

# VAR Model ---------------------------------------------------------------

dota2_var <- cbind(as.data.frame(dota2)$Players, as.data.frame(dota2)$Twitch.Viewers)
colnames(dota2_var) <- c("Players","Twitch.Viewers")
View(dota2_var)
varselect_para <- VARselect(dota2_var, type="both", season=52)
varselect_para

varselect_para$selection
VAR_model <- VAR(dota2_var, p=2, type="both", season=52)
VAR_model
plot(VAR_model)

varforecast <- predict(VAR_model,n.ahead=14)
varforecast <- data.frame(forecast(VAR_model,n.ahead=14))
varforecast
par(mfcol=c(1,1))
plot(predict(VAR_model,n.ahead=14))

forecast(VAR_model,n.ahead=14)$fcst
forecast1 <- data.frame(forecast(VAR_model,h=14))
forecast1

forecast(VAR_model, h=14)

?forecast()

VAR_model_IRF <- irf(VAR_model)
plot(VAR_model_IRF)


# Regression model with all variables -------------------------------------

modelWith_allVariables <- alm(Twitch.Viewers~ Players+Asia_Championship+China_Dota2_Super_Major+DreamLeague_Season8+DreamLeague_Season11+DreamLeague_Season12+
                                DreamLeague_Season13+DreamLeague_Season14+DreamLeague_Season15+DreamLeague_Season16+DreamLeague_Season17+DreamLeague_Season18+DreamLeague_Season19+
                                DreamLeague_Season20+DreamLeague_Season21+EPICENTER_Major_2019+EPICENTER_Major_2020+EPICENTER_Major_2021+EPICENTER_Major_2022+EPICENTER_Major_2023+
                                EPICENTER_Major_2024+EPICENTER_Major_2025+EPICENTER_Major_2026+EPICENTER_Major_2027+EPICENTER_Major_XL+ESL_One_Birmingham+
                                ESL_One_Hamburg+ESL_One_Katowice+MDL_Changsha_Major+MDL_Chengdu_Major+MDL_Disneyland_Paris_Major+One_Esports_Singapore_Major+The_Boston_Major+The_Bucharest_Major+
                                The_Chongqing_Major+The_Frankfurt_Major+The_International+The_Kiev_Major+The_Kuala_Lumpur_Major+The_Manila_Major+The_Shanghai_Major+Twitch.Viewers_Lag1+
                                Twitch.Viewers_Lag2+Twitch.Viewers_Lag3+Twitch.Viewers_Lag4+Twitch.Viewers_Lag5+Twitch.Viewers_Lag6, data =dota2_event_lagged)
summary(modelWith_allVariables)


# Regression diagnostics
par(mfcol=c(3,2))
plot(modelWith_allVariables,c(1,2,4,6,7,12))
