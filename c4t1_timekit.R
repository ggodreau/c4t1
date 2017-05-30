# imports
library(tidyquant)
library(timekit)
library(ggplot2)
library(reshape2) #melt

data <- read.table(
            'house/household_power_consumption.txt',
            sep=";",
            header=TRUE,
            as.is=TRUE)

#remove NAs
data <- na.omit(data)

# create dateTime column
data <- cbind(
  data,paste(data$Date,data$Time), 
  stringsAsFactors=FALSE)
colnames(data)[10] <-"dateTime"

# move dateTime col to col1 of table
data <- data[,c(ncol(data), 1:(ncol(data)-1))]

# remove the old date and time cols
data <- data[,-c(2:3)]

# convert dateTime to POSIXlt
data$dateTime <- as.POSIXct(strptime(
  data[,1],
  format="%d/%m/%Y %H:%M:%S",
  tz=""
))

# convert data types
data$Global_active_power <- as.double(data$Global_active_power)
data$Global_reactive_power <- as.double(data$Global_reactive_power)
data$Voltage <- as.double(data$Voltage)
data$Global_intensity <- as.double(data$Global_intensity)
data$Sub_metering_1 <- as.integer(data$Sub_metering_1)
data$Sub_metering_2 <- as.integer(data$Sub_metering_2)
data$Sub_metering_3 <- as.integer(data$Sub_metering_3)

# back up of raw data
dataBak <- data
# restore
#data <- dataBak

# create detail metric timeseries data
dataTsYWH <- 
  cbind(tk_get_timeseries_signature(data[,1]),data) %>%
    group_by(year, week, hour) %>%
    summarise(SM1 = sum(Sub_metering_1)/1000,
              SM2 = sum(Sub_metering_2)/1000,
              SM3 = sum(Sub_metering_3)/1000) %>% 
                arrange(year, week, hour)

### chart #1, consumption across whole time span

# create year/week/hour group data for plotting

plotDataYWH <- 
  cbind(
    data.frame(
      dataTsYWH,
      data.frame(
        interaction(
          dataTsYWH$year, 
          dataTsYWH$week, 
          dataTsYWH$hour)
      )
    )
  )

# rename new col to id and move to col idx 1
colnames(plotDataYWH)[ncol(plotDataYWH)] <- "id"
plotDataYWH <- 
  plotDataYWH[,c(ncol(plotDataYWH), 1:(ncol(plotDataYWH)-1))]

# create stacked df for charting
plotDataYWHStack <- 
  melt(
    plotDataYWH, 
    id="id", 
    measure=c("SM1", "SM2", "SM3"))

# generate plot
ggplot(plotDataYWHStack, aes(id, value, color=variable)) + 
  geom_point(alpha = 0.3) +
  labs(title = "Consumption by Submeter, Whole Timespan", 
       x = "yyyy.m",
       y = "Consumption [kWh]") +
  theme(legend.position="none") +
  facet_grid(. ~ variable)

### chart #2, stacked bar by year/month

# create YM group data for plotting
dataTsYM <- 
  cbind(tk_get_timeseries_signature(data[,1]),data) %>%
  group_by(year, month) %>%
  summarise(SM1 = sum(Sub_metering_1)/1000,
            SM2 = sum(Sub_metering_2)/1000,
            SM3 = sum(Sub_metering_3)/1000) %>% 
  arrange(year, month)

# generate all distinct factors for year/month combinations
plotDataYM <- 
  cbind(
    data.frame(
      dataTsYM,
      data.frame(
        interaction(
          dataTsYM$year, 
          dataTsYM$month)
      )
    )
  )

# rename new col to id and move to col idx 1
colnames(plotDataYM)[ncol(plotDataYM)] <- "id"
plotDataYM <- 
  plotDataYM[,c(ncol(plotDataYM), 1:(ncol(plotDataYM)-1))]


plotDataYM <- plotDataYM[,c(6,1,2,3,4,5)]

# create stacked df for charting
plotDataYMStack <- 
  melt(
    plotDataYM, 
    id="id", 
    measure=c("SM1", "SM2", "SM3"))

# generate plot
ggplot(plotDataYMStack, aes(id, value, fill=variable)) + 
  geom_bar(stat="identity", alpha=0.8) +
  labs(title = "Submeter Consumption by Year / Month", 
       x = "yyyy.m",
       y = "Consumption [kWh]",
       fill = "Submeter")
