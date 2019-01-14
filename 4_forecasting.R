rm(list = ls())
#### LIBRARIES ####
library(dplyr)
library(forecast)
library(lubridate)
library(xts)
library(ggplot2)
library(imputeTS)
library(TTR)
library(astsa)
#### WD & DATA ####
setwd("C:/Users/David/Google Drive/Github/task-3-1-define-a-data-science-process-dgibert17")

df = read.table("C:/Users/David/Google Drive/Ubiqum/6_EnergyConsumption/power_consumption.txt",
                sep = ";",
                dec = ".",
                col.names = c("date", "time", "global_active_power",
                              "global_reactive_power", "voltage", "global_intensity",
                              "kitchen", "laundry_room", "heater_conditioner"),
                na.strings = c("?", "-", "NA"),
                stringsAsFactors = F,
                header = T)

df = df %>%
  mutate(global_active_power = (global_active_power/60),
         global_reactive_power = (global_reactive_power/60),
         total_consump = global_active_power+global_reactive_power,
         #Las tres en KiloWatt hour (kWh)
         kitchen = kitchen/1000,
         laundry_room = laundry_room/1000,
         heater_conditioner = heater_conditioner/1000,
         #Las submeters ya esta en kWh
         total_rest = total_consump -kitchen -laundry_room -heater_conditioner,
         total_submeter = kitchen + laundry_room + heater_conditioner,
         #En kWh
         
         eur_total_consump = total_consump*0.1472,
         eur_active = global_active_power*0.1472,
         eur_reactive = global_reactive_power*0.1472,
         eur_kitchen = kitchen*0.1472,
         eur_laundry_room = laundry_room*0.1472,
         eur_heater_conditioner = heater_conditioner*0.1472,
         eur_submeters = eur_heater_conditioner+eur_kitchen+eur_laundry_room,
         #Variables que contienen el coste de $ por kWh
         
         date = as.Date(date, format = "%d/%m/%Y"),
         month = as.Date(cut(date, breaks = "month")),
         week = as.Date(cut(date, breaks = "week", start.on.monday = T))
         
         #total rest es el gasto de energia que no contempla submetering
         
         ### IMPORTANTE ###
         ## EL TOTAL REST MUESTRA UN GASTO ENORME QUE NO SABEMOS DE DONDE SALE ##
         
         ) %>%
  #Gasto de submetering
  filter(date > "2006-12-31") %>% # & date < "2010-01-01"
  select(-time)

#### NA VALUES BARPLOT ####

barplot(table(df[rowSums(is.na(df)) >= 1 & rowSums(is.na(df)) < length(colnames(df))-1, 1]),
        ylab = "Amount of NA values",
        xlab = "Date",
        main = "Distribution of NA values",
        col = "lightblue")
#Distribution of NA per day

tab = table(df[rowSums(is.na(df)) >= 1 & rowSums(is.na(df)) < length(colnames(df))-1, 1])

#### NA VALUES REPLACEMENT AND GROUP DATA BY MONTH SUMMARISED BY SUM ####
df.mean = df %>%
  select(-date, -week, -month) %>%
  na.mean(option = "mean")

df.mean = cbind(df.mean, month = df$month)

df.mean = df.mean %>%
  group_by(month) %>%
  summarise_all(sum) %>%
  mutate(month = as.yearmon(month))

#### DF-TS ####
total.ts = ts(df.mean$total_consump, frequency = 12, start = 2007)
plot(total.ts, col = "darkblue", lwd = 3, main = "Total energy consumption Time Series")


#### TRAINING & TEST WITH ALL DATA ####
train = df.mean %>%
  filter(month > "Dec 2006" & month < "Jan 2010")

test = df.mean %>%
  filter(month >= "Jan 2010")


#### TRAINING & TEST TS - TOTAL CONSUMPTION ####
totCons.train.ts = ts(train$total_consump, frequency = 12, start = 2007)
totCons.test.ts = ts(test$total_consump, frequency = 12, start = 2010)

par(mfrow = c(2,1))
plot(totCons.train.ts, col = "darkblue", lwd = 3, main = "Total energy consumption Time Series\nTraining set")
plot(totCons.test.ts, col = "darkblue", lwd = 3, main = "Total energy consumption Time Series\nTest set")

#### TRAINING & TEST TS - ACTIVE POWER ####
active.train.ts = ts(train$global_active_power, frequency = 12, start = 2007)
active.test.ts = ts(test$global_active_power, frequency = 12, start = 2010)

par(mfrow = c(2,1))
plot(active.train.ts, col = "darkblue", lwd = 3, main = "Active energy consumption Time Series\nTraining set")
plot(active.test.ts, col = "darkblue", lwd = 3, main = "Active energy consumption Time Series\nTest set")

#### TRAINING & TEST TS - REACTIVE POWER ####
reactive.train.ts = ts(train$global_reactive_power, frequency = 12, start = 2007)
reactive.test.ts = ts(test$global_reactive_power, frequency = 12, start = 2010)

par(mfrow = c(2,1))
plot(reactive.train.ts, col = "darkblue", lwd = 3, main = "Reactive energy consumption Time Series\nTraining set")
plot(reactive.test.ts, col = "darkblue", lwd = 3, main = "Reactive energy consumption Time Series\nTest set")


#### PLOTING YEARS SEASONALITY BY MONTH ####

ggseasonplot(total.ts, year.labels=TRUE, year.labels.left=TRUE) +
  geom_line(size=2) +
  geom_point(size=6) +
  ylab("Amount consumed") +
  ggtitle("Seasonal plot: Energy consumption")


#### FORECASTING MODELS - TOTAL CONSUMPTION ####
total.lm.fit = tslm(formula = totCons.train.ts ~ trend + season)
total.lm.for = forecast(total.lm.fit, h = 12)

total.hw.fit <- HoltWinters(totCons.train.ts)
plot(total.hw.fit, main = "Forecast", xlab = "Date", ylab = "Amount", lwd = 3)
total.hw.for = forecast(total.hw.fit, h = 12)

total.arima.fit = auto.arima(totCons.train.ts)
total.arima.for = forecast(total.arima.fit, h = 12)

par(mfrow = c(3,1))
plot(total.lm.for)
plot(total.hw.for)
plot(total.arima.for)

#### FORECASTING MODELS - ACTIVE POWER CONSUMPTION ####
active.lm.fit = tslm(formula = active.train.ts ~ trend + season)
active.lm.for = forecast(active.lm.fit, h = 12)

active.hw.fit <- HoltWinters(active.train.ts)
plot(active.hw.fit)
active.hw.for = forecast(active.hw.fit, h = 12)

active.arima.fit = auto.arima(active.train.ts)
active.arima.for = forecast(active.arima.fit, h = 12)

par(mfrow = c(3,1))
plot(active.lm.for)
plot(active.hw.for)
plot(active.arima.for)

#### FORECASTING MODELS - REACTIVE POWER CONSUMPTION ####
reactive.lm.fit = tslm(formula = reactive.train.ts ~ trend + season)
reactive.lm.for = forecast(reactive.lm.fit, h = 12)

reactive.hw.fit <- HoltWinters(reactive.train.ts)
plot(reactive.hw.fit)
reactive.hw.for = forecast(reactive.hw.fit, h = 12)

reactive.arima.fit = auto.arima(reactive.train.ts)
reactive.arima.for = forecast(reactive.arima.fit, h = 12)

par(mfrow = c(3,1))
plot(reactive.lm.for)
plot(reactive.hw.for)
plot(reactive.arima.for)


#### CHECKING THAT THE OUTLIER PEAK COMES FROM WHITE NOISE ####

dec = decompose(total.ts)
par(mfrow = c(3,1))
plot(dec$x, lwd = 3, col = "darkblue", main = "TS", ylab = "", xlab = "")
plot(dec$seasonal, lwd = 3, col = "darkblue", main = "Seasonality", ylab = "", xlab = "")
plot(dec$random, lwd = 3, col = "darkblue", main = "White Noise", ylab = "", xlab = "")


#### PLOTING MODELS - TEST VS TOTAL CONSUMPTION ####
autoplot(totCons.test.ts, ylab="Amount of power consumed", xlab="", main="Total power consumption (active + reactive) forecast") +
  geom_line(size = 8) +
  geom_point(size = 14) +
  autolayer(total.arima.for, PI = F, series = "Arima", size = 2, showgap = F) +
  autolayer(total.hw.for, PI = F, series = "Holt Winters", size = 2, showgap = F) +
  autolayer(total.lm.for, PI = F, series = "Linear Model", size = 2, showgap = F)

#### PLOTING MODELS - TEST VS ACTIVE CONSUMPTION ####
autoplot(active.test.ts, ylab="Amount of power consumed", xlab="", main = "Active power consumption forecast") +
  geom_line(size = 8) +
  geom_point(size = 14) +
  autolayer(active.arima.for, PI = F, series = "Arima", size = 2, showgap = F) +
  autolayer(active.hw.for, PI = F, series = "Holt Winters", size = 2, showgap = F) +
  autolayer(active.lm.for, PI = F, series = "Linear Model", size = 2, showgap = F)

#### PLOTING MODELS - TEST VS REACTIVE CONSUMPTION ####
autoplot(reactive.test.ts, ylab="Amount of power consumed", xlab="", main="Reactive power consumption forecast") +
  geom_line(size = 8) +
  geom_point(size = 14) +
  autolayer(reactive.arima.for, PI = F, series = "Arima", size = 2, showgap = F) +
  autolayer(reactive.hw.for, PI = F, series = "Holt Winters", size = 2, showgap = F) +
  autolayer(reactive.lm.for, PI = F, series = "Linear Model", size = 2, showgap = F)

#### ERROR METRICS FOR MODELS VS TEST DATA - TOTAL CONSUMPTION ####
acc.hw.tot = as.data.frame(accuracy(f = total.hw.for, totCons.test.ts))[2,]
acc.lm.tot = as.data.frame(accuracy(f = total.lm.for, totCons.test.ts))[2,]
acc.arima.tot = as.data.frame(accuracy(f = total.arima.for, totCons.test.ts))[2,]

error.total = rbind(acc.hw.tot, acc.lm.tot, acc.arima.tot)
rownames(error.total) <- c("Holt Winters", "Linear Model", "ARIMA")

#### ERROR METRICS FOR MODELS VS TEST DATA - ACTIVE CONSUMPTION ####
acc.hw.active = as.data.frame(accuracy(f = active.hw.for, active.test.ts))[2,]
acc.lm.active = as.data.frame(accuracy(f = active.lm.for, active.test.ts))[2,]
acc.arima.active = as.data.frame(accuracy(f = active.arima.for, active.test.ts))[2,]

error.active = rbind(acc.hw.active, acc.lm.active, acc.arima.active)
rownames(error.active) <- c("Holt Winters", "Linear Model", "ARIMA")

#### ERROR METRICS FOR MODELS VS TEST DATA - ACTIVE CONSUMPTION ####
acc.hw.reactive = as.data.frame(accuracy(f = reactive.hw.for, reactive.test.ts))[2,]
acc.lm.reactive = as.data.frame(accuracy(f = reactive.lm.for, reactive.test.ts))[2,]
acc.arima.reactive = as.data.frame(accuracy(f = reactive.arima.for, reactive.test.ts))[2,]

error.reactive = rbind(acc.hw.reactive, acc.lm.reactive, acc.arima.reactive)
rownames(error.reactive) <- c("Holt Winters", "Linear Model", "ARIMA")

error.total
error.active
error.reactive

#### FORECAST CON MEJOR MODELO - TOTAL CONSUMPTION ####
total.fit.2011 <- HoltWinters(total.ts)
plot(total.fit.2011)
total.for.2011 = forecast(total.fit.2011, h = 12)
plot(total.for.2011)

#### FORECAST CON MEJOR MODELO - ACTIVE CONSUMPTION ####
active.ts = ts(df.mean$global_active_power, frequency = 12, start = 2007)

active.fit.2011 <- HoltWinters(active.ts)
plot(active.fit.2011)
active.for.2011 = forecast(active.fit.2011, h = 12)
plot(active.for.2011)

#### FORECAST CON MEJOR MODELO - REACTIVE CONSUMPTION ####
reactive.ts = ts(df.mean$global_reactive_power, frequency = 12, start = 2007)

reactive.fit.2011 <- HoltWinters(reactive.ts)
plot(reactive.fit.2011)
reactive.for.2011 = forecast(reactive.fit.2011, h = 12)
plot(reactive.for.2011)

#### ESTUDIO REACTIVE ####
plot.ts(reactive.ts, col = "darkblue", lwd = 3, main = "Reactive energy consumption")

plot(x = df.mean$month, y = df.mean$eur_reactive, type = "l", col = "darkblue", lwd=3,
     xlab = "Time", ylab = "$", main = "$ for reactive power consumption")
