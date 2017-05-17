# dependency checking
dependencies <- c("dplyr", "caret")
staged <- dependencies[!(dependencies %in% installed.packages()[,"Package"])]
if(length(staged)) install.packages(staged)
dependencies <- NULL
staged <- NULL

# imports
library(dplyr)

# globals
dataUrl = paste("http://archive.ics.uci.edu/ml/machine-learning-databases/00235/household_power_consumption.zip")

# read data set from internets
temp <- tempfile()
download.file(dataUrl, temp)
data <- read.table(
          unz(
            temp,
            "household_power_consumption.txt"
          ),
          sep=";",
          header=TRUE
        )

# create dateTime column
data <- cbind(
      data,paste(data$Date,data$Time), 
      stringsAsFactors=FALSE)
colnames(data)[10] <-"dateTime"

# move dateTime col to col1 of table
data <- data[,c(ncol(data), 1:(ncol(data)-1))]

# remove the old date and time cols
data <- data[,-c(2:3)]

# convert data types
data$Global_active_power <- as.numeric(as.character(data$Global_active_power))
data$Global_reactive_power <- as.numeric(as.character(data$Global_reactive_power))
data$Voltage <- as.numeric(as.character(data$Voltage))
data$Global_intensity <- as.numeric(as.character(data$Global_intensity))
data$Sub_metering_1 <- as.numeric(as.character(data$Sub_metering_1))
data$Sub_metering_2 <- as.numeric(as.character(data$Sub_metering_2))

# back up df
databak <- data

# get counts of NA rows
data.frame(sapply(x, function(y) sum(length(which(is.na(y))))))

# remove NA's
data <- na.omit(data)

# convert dateTime from chr to POSIXlt object
data$dateTime <- strptime(
  data[,1],
  format="%d/%m/%Y %H:%M:%S",
  tz=""
)

# create year / month / day features
data$year <- as.factor(strftime(data[,1], format="%Y"))
data$month <- as.factor(strftime(data[,1], format="%b"))
data$dayOfMonth <- as.factor(strftime(data[,1], format="%e"))
data$hour <- as.factor(strftime(data[,1], format="%k"))

# group submeters [Wh] by year / month / day / hour
gpdata <- data[,2:12] %>%
  group_by(year, month, dayOfMonth, hour) %>%
    summarise(SM1=sum(Sub_metering_1, na.rm=TRUE),
              SM2=sum(Sub_metering_3, na.rm=TRUE),
              SM3=sum(Sub_metering_2, na.rm=TRUE))
