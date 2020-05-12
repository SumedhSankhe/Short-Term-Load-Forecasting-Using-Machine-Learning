library(data.table)
library(ggplot2)
cleanWeather <- readRDS("CleanNoaaData.rds")
meterData <- readRDS("meter.rds")
weatherData <- cleanWeather$noaaData

dt <- merge(meterData, weatherData[,.(dttm, wetBulbTemp, RH)], "dttm")
dt

newDt <- dt[dttm < min(dt$dttm)+ 60*60*24*20]
newDt[,day := wday(dttm)]
newDt

#we will try to forecast 24 hours using only 20 days of data
plotData <- melt.data.table(newDt, "dttm", c("RH","wetBulbTemp","totDemand"))
ggplot(data = plotData, aes(x = dttm ,  y = value))+
  geom_line()+
  labs(x = "Date", y = "")+
  scale_x_datetime(date_breaks = "4 days", date_labels = "%b %d")+
  facet_wrap(~variable, scales = "free", ncol = 1,
             labeller = labeller(variable = c(`totDemand` = "Total Load in kW",
                                              `wetBulbTemp` = "Wet Bulb Temperature",
                                              `RH` = "Relative Humidity")))+
  theme_bw()

#autocorrelation of the series
ac <- acf(newDt$totDemand, lag.max = length(newDt$totDemand), plot = F)

ggplot(data = data.frame(ac = ac$acf, lag = ac$lag), aes(x = lag, y = ac, fill = ac))+
  geom_line()+
  labs(y = "ACF", x = "Lags", title = "Autocorrelation of the Load Series")+
  geom_hline(yintercept = 0)+
  theme_bw()


N <- nrow(newDt)
period <- 24
window <- N/period

makePlot <- function(plotDt, mName, s){
  ggplot(data = plotDt, 
         aes(x = dttm , y = value, color = variable))+
    geom_line()+
    labs(x = "Date", 
         y = "Load in kW",
         color = "Type", 
         title = paste0("Load Forecast using : ", mName, " Model"),
         subtitle = s)+
    scale_x_datetime(date_breaks = "4 days", date_labels = "%b %d")+
    scale_color_discrete(labels = c("Actual Load", "Predicted Load"))
}

mape <- function(real, pred){
  return(100* mean(abs((real - pred)/real)))
}


modelThat <- function(traindata, dttm, alg, form, ...){
  args <- list(...)
  
  mName <- as.character(substitute(alg))
  mName <- switch(mName,
                  "lm" = "Linear",
                  "gam" = "General Additive",
                  "ARMA" = 'ARMA',
                  "ARIMA" = "ARIMA")
  
  m <- alg(formula = form, data = traindata, ...)
  
  dt <- data.table(dttm = dttm,
                   actual = traindata[,1],
                   pred = m$fitted.values)
  
  plotDt <- melt.data.table(dt, "dttm")
  g1 <- makePlot(plotDt, mName, "Train Data")
  
  
  outList <- list(model = m,
                  modelSummary = summary(m),
                  train_plot = g1,
                  map = mape(dt$actual.demand, m$fitted.values))
  
  
  if('test' %in% names(args)){
    newDttm <- test$dttm
    testT$dttm <- NULL
    
    fcast <-  predict(m, test, -1, with = F)
    dt1 <- data.table(dttm = newDttm, actual = test$totDemand, pred = fcast$fit)
    
    newPlotdt <- melt.data.table(dt1,"dttm")
    
    g2 <- makePlot(newPlotdt, mName, "Test Data")
    
    outList <- append(outList, list(fcastModel = fcast, testPlot = g2))
  }
  return(outList)
}

train_matrix <- data.table(demand = newDt[,totDemand],
                           hour = as.factor(rep(1:period, window)),
                           day = newDt[,day],
                           RH = newDt[,RH],
                           wetBulbTemp = newDt[,wetBulbTemp])


formList <- list(l1 = demand ~ 0 +.,
                 l2 = demand ~ 0 + hour + day,
                 l3 = demand ~ 0 + hour + day + hour:day,
                 l4 = demand ~ 0 + hour + wetBulbTemp,
                 l5 = demand ~ 0 + hour + RH,
                 l6 = demand ~ 0 + hour + RH + wetBulbTemp,
                 l7 = demand ~ 0 + hour + RH + wetBulbTemp + RH:wetBulbTemp,
                 l8 = demand ~ 0 + hour + RH + wetBulbTemp + RH:wetBulbTemp + hour:day,
                 l9 = demand ~ 0 + hour + RH + wetBulbTemp + RH:hour + wetBulbTemp:hour)

modelList <- lapply(formList, function(x){
  modelThat(train_matrix, newDt$dttm, lm, x)
})

d <- do.call("cbind",lapply(modelList, function(x){
  x$model$fitted.values
}))

testDt <- dt[dttm <= max(newDt$dttm)+60*60*24 & dttm > max(newDt$dttm)]
test_matrix = copy(testDt)
test_matrix[,':='(hour = as.factor(hour(dttm)+1), day = wday(dttm))]
test_matrix[,dttm := NULL]

d <- data.table(do.call("cbind", lapply(modelList, function(x){
  predict(x$model, test_matrix, -1 , with = F)$fit
})))


m <- do.call('rbind',sapply(d, function(x){
  list(MAPE = round(mape(testDt$totDemand, x), 4))
}))

d2 <- data.table(d,  dttm = testDt$dttm)
#d3 <- data.table(dttm = testDt$dttm, xyf = xyforecat(newDt = newDt))
plotData2 <- melt(d2,"dttm")


ggplot()+
  geom_line(data = plotData2, aes(x = dttm , y = value, color = variable), alpha = 0.5)+
  #geom_line(data = d3, aes(x = dttm, y = xyf), color = "red", size = 0.75)+
  geom_line(data = testDt, aes(x = dttm , y = totDemand), color = "black", size = 0.75)+
  labs(x = "Hour", y = "kW", title = "Actual vs LM Forecast", color = "Type")+
  scale_x_datetime(date_breaks = "2 hour", date_labels = "%H")


####### GAM
library(mgcv)

train_matrix <- data.table(demand = newDt[,totDemand],
                           hour = rep(1:period, window),
                           RH = newDt[,RH],
                           day = newDt[,day],
                           wetBulbTemp = newDt[,wetBulbTemp])

gamForm <- list(g1 = demand ~ s(hour, bs = 'cr', k = period) + s(wetBulbTemp, bs = 'cr', k = period) + s(RH, bs = 'cr', k = period),
                g2 = demand ~ s(hour, bs = 'cr', k = period) + s(day, bs = 'ps', k = 7) + s(wetBulbTemp, bs = 'cr', k = period) + s(RH, bs = 'cr', k = period),
                g3 = demand ~ s(hour, wetBulbTemp, RH),
                g4 = demand ~ te(hour, day, k = c(period, 7), bs = c('cr','ps')),
                g5 = demand ~ te(hour, day, k = c(period, 7), bs = c('cr','ps'), fx = T),
                g6 = demand ~ s(hour)+ s(RH, wetBulbTemp))



gamList <- lapply(gamForm, function(x){
    p <- gam(x, data = train_matrix)
    plot <- ggplot(data = cbind(newDt, pred = p$fitted.values), aes(x = dttm))+
      geom_line(aes(y = totDemand, color = "Acutal"))+
      geom_line(aes(y = pred, color = "Predicted"))
    
    oL <- list(model = p, plot = plot)
    return(oL)
})

testDt <- dt[dttm <= max(newDt$dttm)+60*60*24 & dttm > max(newDt$dttm)]
test_matrix = copy(testDt)
test_matrix[,':='(hour = hour(dttm)+1, day = wday(dttm))]
test_matrix[,dttm := NULL]

d2 <- data.table(do.call("cbind", lapply(gamList, function(x){
   predict(x$model, test_matrix)
})))

do.call("rbind",sapply(d2, function(x){
  list(round(mape(testDt$totDemand, x ),4))
}))
######## Random Forest
library(forecast)
library(randomForest)


data_ts <- ts(newDt[,totDemand], freq = 7)
decomp_ts <- stl(data_ts, s.window = "periodic", robust = TRUE)$time.series

trend_part <- ts(decomp_ts[,2])

trend_fit <- auto.arima(trend_part) # ARIMA
trend_for <- as.vector(forecast(trend_fit, period)$mean) # trend forecast

data_msts <- msts(newDt$totDemand, seasonal.periods = c(24, 24*7))
K <- 2
fuur <- fourier(data_msts, K = c(K, K))

perios <- 24
N <- nrow(newDt)
window <- (N / period) - 1

new_load <- rowSums(decomp_ts[, c(1,3)]) # detrended original time series
lag_seas <- decomp_ts[1:(period*window), 1] # lag feature to model

matrix_train <- data.table(Load = tail(new_load, window*period),
                           fuur[(period + 1):N,],
                           Lag = lag_seas)

# create testing data matrix
test_lag <- decomp_ts[((period*window)+1):N, 1]
fuur_test <- fourier(data_msts, K = c(K, K), h = period)

matrix_test <- data.table(fuur_test,
                          Lag = test_lag)

# random forest


forest_model <- randomForest(Load ~., data = data.frame(matrix_train),
                             ntree = 1000, mtry = 3, nodesize = 5, importance = T)

pred_rf <- predict(forest_model, data.frame(matrix_test)) + mean(trend_for)

plotDt <- data.frame(dttm = testDt$dttm, actual = testDt$totDemand, pred = pred_rf)

makePlot(melt(plotDt,"dttm"),"Random Forest","Test")+theme_bw()


rbind(do.call("rbind",lapply(modelList, function(x){
 round(x$modelSummary$adj.r.squared,4)
})), do.call("rbind",lapply(gamList, function(x){
 round(summary(x$model)$r.sq,4)
})))


combDT <- cbind.data.frame(dttm  = testDt$dttm,d,d2)
mlt <- melt.data.table(combDT,"dttm")

ggplot(data = testDt)+
  geom_line(data = mlt, aes(x = dttm , y = value, color = variable),size = 0.4, alpha = 0.7)+
  geom_line(aes(x = dttm , y = totDemand, color = "Acutal"),
            color = "black", size = 0.75)+
  labs(x = "Hour", y = "kW", color = "Models", caption = "The black line represents actual kW",
       title = "MLR and GAM models forecasting")+
  scale_x_datetime(date_breaks = "2 hour", date_labels = "%H")+
  theme_bw()+
  theme(legend.position = "bottom")



