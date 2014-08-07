plot2 <- function () {
  
  print(Sys.time(), format="%H:%M:%S")
  consumption_data <- data.frame(Date=character(),
                                 Time=character(),
                                 Global_active_power=numeric(),
                                 Global_reactive_power=numeric(),
                                 Voltage=numeric(),
                                 Global_intensity=numeric(),
                                 Sub_metering_1=numeric(),
                                 Sub_metering_2=numeric(),
                                 Sub_metering_3=numeric())
  
  consumption_data <- read.table("household_power_consumption.txt",
                                 header=TRUE,
                                 sep=";",
                                 na.string="?",
                                 stringsAsFactor=FALSE)
  
  ## convert Date and Time to R Date and Time objects
  
  consumption_data[,1] <- as.Date(consumption_data[,1], format="%d/%m/%Y")
  consumption_data[,2] <- paste(consumption_data[,1],
                                consumption_data[,2],
                                sep=" ")
  consumption_data[,2] <- as.POSIXct(consumption_data[,2],
                                     format="%Y-%m-%d %H:%M:%S")
  
  print(Sys.time())
  
  start_date <- strptime("2007-02-01 00:00:01", format="%Y-%m-%d %H:%M:%S")
  end_date <- strptime("2007-02-03 00:00:01", format="%Y-%m-%d %H:%M:%S")
  
  selected_consumption_data <- consumption_data[consumption_data$Time >= start_date,]
  selected_consumption_data <- selected_consumption_data[selected_consumption_data$Time <= end_date,]
  
  daterange=c(start_date, end_date)
  
  print(Sys.time())
  
  png("plot2.png", 
      width=480, 
      height=480)
  plot(selected_consumption_data$Time,
       selected_consumption_data$Global_active_power,
       type="l",
       xlab="",
       ylab="Global Active Power (kilowatts)",
       xaxt="n")
  axis.POSIXct(1, 
               at=seq(daterange[1], 
                      daterange[2], 
                      by="day"), 
               format="%a")  
  
  dev.off() 
  print(Sys.time())
}