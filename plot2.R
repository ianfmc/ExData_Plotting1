plot2 <- function () {
  
  print(Sys.time(), format="%H:%M:%S")
  consumption_data = data.frame(HDate=character(),
                                Time=character(),
                                Global_active_power=numeric(),
                                Global_reactive_power=numeric(),
                                Voltage=numeric(),
                                Global_intensity=numeric(),
                                Sub_metering_1=numeric(),
                                Sub_metering_2=numeric(),
                                Sub_metering_3=numeric())
  
  consumption_data = read.table("household_power_consumption.txt",
                                header=TRUE,
                                nrows=500000,
                                sep=";",
                                na.string="?")
  
  ## convert HDate and Time to R Date and Time objects
  
  consumption_data[["HDate"]] <- as.Date(consumption_data[["HDate"]], format="%d/%m/%Y")
  consumption_data[["Time"]] <- paste(consumption_data[["HDate"]],
                                      consumption_data[["Time"]],
                                      sep=" ")
  consumption_data[["Time"]] <- strptime(consumption_data[["Time"]], 
                                         format="%Y-%m-%d %H:%M:%S")   
  print(Sys.time())
  
  start_date <- strptime("2007-02-01 00:00", 
                         format="%Y-%m-%d %H:%M")
  end_date <- strptime("2007-02-03 00:00", 
                       format="%Y-%m-%d %H:%M")
  
  selected_consumption_data <- consumption_data[!is.na(consumption_data$Time),]
  
  selected_consumption_data <- consumption_data[(consumption_data$Time >= start_date),]
  selected_consumption_data <- selected_consumption_data[selected_consumption_data$Time <= end_date,]
  
  low <- min(selected_consumption_data$Time)
  daterange=c(as.POSIXlt(min(selected_consumption_data$Time)),
              as.POSIXlt(max(selected_consumption_data$Time)))   
  
  print(Sys.time())
  
  ## png("plot2.png", width=480, height=480)
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
  
  ## dev.off() 
  print(Sys.time())
}