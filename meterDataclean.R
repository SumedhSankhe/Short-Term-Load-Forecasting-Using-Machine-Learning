library(data.table)
library(lubridate)

meterData <- readRDS("isone072718365.rds")

# changing data granularity to hour from 5 minutes
meterData[, dttm := floor_date(dttm, "hour")]
# remove unwanted columns
meterData[,':='(baseline = NULL, ds = NULL, est = NULL)]
# get an average for the entire hour from the 5 minutes interval data
meterData[, demand := mean(demand, na.rm = T), .(id, dttm)]
meterData <- unique(meterData)

# only ids with no missing meter data are being considered for the purpose of this exercise
reqIds <- meterData[demand != 0,.N,id][N > 8760][,id]
meterData <- meterData[id %in% reqIds]


library(ggplot2)

# Combining data to a region level total, we will try to forecast at different levels
# 1. for a combine load of the region
# 2. for every id/customer that we have
combData <- meterData[,.(dttm,demand)]
combData[, ':='(totDemand = sum(demand)), dttm]
combData$demand <- NULL
combData <- unique(combData)

ggplot(data = combData, aes(x = dttm , y = totDemand))+
  geom_line()+
  labs(x = "Date", y = "Load in kW")

rdsList <- list(meterData = meterData, combData = combData)
saveRDS(rdsList, "cleanedMeter.rds")