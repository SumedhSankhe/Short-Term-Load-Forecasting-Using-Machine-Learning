library(data.table)
library(ggplot2)
library(lubridate)

# Read NOAA data, get the required columns out format them
NOAA_data <- as.data.table(read.csv("NOAA_data.csv"))
colsReq <- c('DATE','HOURLYWETBULBTEMPC','HOURLYRelativeHumidity')
noaaData <- NOAA_data[,.(DATE,HOURLYWETBULBTEMPC,HOURLYRelativeHumidity)]
setnames(noaaData, colsReq, c("date","wetBulbTemp", "RH"))

noaaData[,':='(time = gsub(".* ","",date),
               date = as.Date(gsub(" .*$", "", date)),
               RH = as.numeric(RH))]

noaaData[, hour := as.integer(gsub("\\:.*", "", time))]
noaaData$time <- NULL
noaaData[,':='(wetBulbTemp = mean(wetBulbTemp, na.rm = T),
               RH = mean(RH, na.rm = T)), .(date, hour)]
noaaData[, dttm := ymd_h(paste(date,hour))]

noaaData <- unique(noaaData)

plotData <- melt.data.table(noaaData, "dttm", c("wetBulbTemp", "RH"))

ggplot(data = plotData, aes(x = dttm, y = value, color = variable))+
  geom_line()+
  labs(x = "Date", y = "Value", color = "") + scale_color_discrete(guide = F) +
  facet_wrap(~variable, nrow = 2, scales = "free",
             labeller = labeller(variable = c(`wetBulbTemp` = "Wet Bulb Temperature",
                                              `RH` = "Relative Humidity")))+
  theme_bw( )

rdsList <- list(plotData = plotData, noaaData = noaaData)
saveRDS(rdsList, "CleanNoaaData.rds")
