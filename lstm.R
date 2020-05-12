library(data.table)
# Core Tidyverse
library(tidyverse)
library(glue)
library(forcats)

# Time Series
library(timetk)
library(tidyquant)
library(tibbletime)

# Visualization
library(cowplot)

# Preprocessing
library(recipes)

# Sampling / Accuracy
library(rsample)
library(yardstick) 

reticulate::use_virtualenv('C:/Users/sumed/Anaconda3/envs/py35')
reticulate::use_python('C:/Users/sumed/Anaconda3/envs/py35/python.exe')

# Modeling
library(keras)
library(tfruns)

library(recipes)



meterData <- readRDS("cleanedMeter.rds")
weatherData <- readRDS("CleanNoaaData.rds")

meter <- meterData$combData[,]
weather <- weatherData$noaaData

mergedData <- merge(meter, weather[,.(wetBulbTemp, RH, dttm)],"dttm" )

partition <- mergedData[dttm < min(dttm)+ 60*60*24*9]
partition[1:168, type := "train"]
partition[, type := ifelse(is.na(type), 'test', type)]

rec_obj <- recipe(totDemand~. , data = partition) %>%
  step_sqrt(totDemand) %>%
  step_center(totDemand) %>%
  step_scale(totDemand) %>%
  prep()

processed_partition <- bake(rec_obj, partition)

center_history <- rec_obj$steps[[2]]$means["totDemand"]
scale_history  <- rec_obj$steps[[3]]$sds["totDemand"]


lag_setting <- nrow(partition[type == "test"])
batchsize <- 28
train_length <- 168
tstep <- 1
epoch <- 100


lag_train_tbl <- processed_partition %>%
  mutate(value_lag = lag(totDemand, n = lag_setting)) %>%
  filter(!is.na(value_lag)) %>%
  filter(type== "train") %>%
  tail(train_length)

x_train_vec <- lag_train_tbl$value_lag
x_train_arr <- array(data = x_train_vec, dim = c(length(x_train_vec), 1, 1))

y_train_vec <- lag_train_tbl$value
y_train_arr <- array(data = y_train_vec, dim = c(length(y_train_vec), 1))

# Testing Set
lag_test_tbl <- processed_partition %>%
  mutate(
    value_lag = lag(value, n = lag_setting)
  ) %>%
  filter(!is.na(value_lag)) %>%
  filter(key == "testing")

x_test_vec <- lag_test_tbl$value_lag
x_test_arr <- array(data = x_test_vec, dim = c(length(x_test_vec), 1, 1))

y_test_vec <- lag_test_tbl$value
y_test_arr <- array(data = y_test_vec, dim = c(length(y_test_vec), 1))









createMat <- function(data, cl, ch, nstep, batch_size){
  data %>%
    filter(type == ch)%>%
    select(cl) %>%
    pull() -> val
  
  totalSteps <- nstep*2
  
  newMat <- t(sapply(1:(length(val) - (totalSteps + 1)), function(x) 
    val[x:(x + totalSteps - 1)]))
  
  x <- newMat[, 1:nstep]
  y <- newMat[, (nstep+1) : (nstep*2)]
  
  x <- x[1:(nrow(x) %/% batch_size * batch_size), ]
  y <- y[1:(nrow(y) %/% batch_size * batch_size), ]
  
  return(list(x = x, y = y))
}


demandTrain <- createMat(processed_partition, "totDemand", "train", n_timesteps, bsz)
rhTrain <- createMat(processed_partition,"RH", "train", n_timesteps, bsz)
wbTTrain <- createMat(processed_partition, "wetBulbTemp", "train", n_timesteps, bsz)

demandTest <- createMat(processed_partition, "totDemand", "test", n_timesteps, bsz)
rhTest <- createMat(processed_partition,"RH", "test", n_timesteps, bsz)
wbTTest <- createMat(processed_partition, "wetBulbTemp", "test", n_timesteps, bsz)



X_train <- array(c(demandTrain$x, rhTrain$x, wbTTrain$x),
                 dim = c(dim(rhTrain$x)[1], dim(rhTrain$x)[2], 3))
Y_train <- array(c(demandTrain$y, rhTrain$y, wbTTrain$y),
                 dim = c(dim(rhTrain$y)[1], dim(rhTrain$y)[2], 3))

X_test <- array(c(demandTest$x, rhTest$x, wbTTest$x),
                dim = c(dim(rhTest$x)[1], dim(rhTest$x)[2], 3))
Y_test <- array(c(demandTest$x, rhTest$x, wbTTest$x),
                dim = c(dim(rhTest$x)[1], dim(rhTest$x)[2], 3))



# create the model
model <- keras_model_sequential()

model %>%
  layer_lstm(
    units = 128, 
    input_shape  = c(n_timesteps, n_features),
    return_sequences = TRUE,
    stateful = TRUE) %>% 
  layer_lstm(
    units = 128,
    return_sequences = FALSE,
    stateful = TRUE)
 
model %>% time_distributed(layer_dense(units = 1))


model %>%
  compile(
    loss = "mae",
    optimizer = "adam",
    metrics = list("mean_squared_error")
  )

for (i in 1:100){
  model %>%
    fit(x          = X_train,
        y          = Y_train,
        batch_size = bsz,
        # epochs     = n_epochs,
        shuffle = FALSE)
  
  model %>% reset_states()
  cat("Epoch: ", i)

}  

pred_train <- model %>%
  predict(X_train, batch_size = FLAGS$batch_size) %>%
  .[, , 3]

# Retransform values to original scale
pred_train <- (pred_train * scale_history + center_history) ^2
compare_train <- partition[type == "train"]

# build a dataframe that has both actual and predicted values
for (i in 1:nrow(pred_train)) {
  varname <- paste0("pred_train", i)
  compare_train <- mutate(compare_train,
                          !!varname := c(rep(NA, FLAGS$n_timesteps + i - 1),
                                         pred_train[i,],
                                         rep(NA, nrow(compare_train) - FLAGS$n_timesteps * 2 - i + 1)
    ))
}
# 
# 
# coln <- colnames(compare_train)[4:ncol(compare_train)]
# cols <- map(coln, quo(sym(.)))
# rsme_train <-  map_dbl(cols, function(col)
#   rmse(
#     compare_train,
#     truth = value,
#     estimate = !!col,
#     na.rm = TRUE
#   )) %>% mean()
# 
# 
# 
# ggplot(compare_train, aes(x = dttm, y = totDemand)) + geom_line() +
#   geom_line(aes(y = pred_train1), color = "cyan") +
#   geom_line(aes(y = pred_train50), color = "red") +
#   geom_line(aes(y = pred_train100), color = "green") +
#   geom_line(aes(y = pred_train150), color = "violet") +
#   geom_line(aes(y = pred_train200), color = "cyan") +
#   geom_line(aes(y = pred_train250), color = "red") +
#   geom_line(aes(y = pred_train300), color = "red") +
#   geom_line(aes(y = pred_train350), color = "green") +
#   geom_line(aes(y = pred_train400), color = "cyan") +
#   geom_line(aes(y = pred_train450), color = "red") +
#   geom_line(aes(y = pred_train500), color = "green") +
#   geom_line(aes(y = pred_train550), color = "violet") +
#   geom_line(aes(y = pred_train600), color = "cyan") +
#   geom_line(aes(y = pred_train650), color = "red") +
#   geom_line(aes(y = pred_train700), color = "red") +
#   geom_line(aes(y = pred_train750), color = "green") +
#   ggtitle("Predictions on the training set")