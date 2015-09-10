CreatePlot3 <- function(DAT,...) {
      png("plot3.png")
      MinutesPerDay <- 60*24
      Weekdays <- unique(weekdays(c(DAT$Time),abbreviate=TRUE))
      Extra <- 0
      if(nrow(DAT) %% MinutesPerDay == 0) {
            NextWeekDay <- weekdays(as.POSIXct(tail(DAT$Time,n=1))+60,
                  abbreviate=TRUE)
            Weekdays = c(Weekdays,NextWeekDay)
            Extra <- 1
      }
      plot.ts(DAT$Sub_metering_1,axes = FALSE,ylab = "Energy sub metering",xlab = NULL)
      lines(DAT$Sub_metering_2,col="red")
      lines(DAT$Sub_metering_3,col="blue")
      axis(2)
      axis(1, labels = Weekdays, seq(from=0, by=MinutesPerDay, 
                                     length.out=(nrow(DAT)+Extra)/MinutesPerDay))
      box()
      legend("topright", legend = c("Sub_metering_1","Sub_metering_2",
            "Sub_metering_3"), col = c("black","red","blue"), lty = 1)
      dev.off()
      print("Plot saved to plot3.png")
}

DownloadData <- function() {
      dataFile = "household_power_consumption.txt"
      if(!file.exists(dataFile)) {
            url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
            tmpfile <- tempfile()
            download.file(url,tmpfile)
            unzip(tmpfile,files = dataFile,exdir = getwd())
      }
      skip <- 66637
      last <- 69517
      nmax <- last - skip
      variables <- c("Date","Time","Global_active_power","Global_reactive_power",
                     "Voltage","Global_intensity","Sub_metering_1","Sub_metering_2",
                     "Sub_metering_3")
      types <- c(rep("character", each = 2), rep("numeric", each=7))
      data <- read.table(dataFile,sep = ";", quote = "", col.names = variables, 
                         colClasses = types, nrows = nmax, skip = skip, stringsAsFactors = FALSE,
                         na.strings = "?")
      Date_Time = do.call(paste, c(data[,c("Date", "Time")],sep = " "))
      data$Time = strptime(Date_Time,"%d/%m/%Y %H:%M:%S")
      data$Date <- NULL
      data
}

CreatePlot3(DownloadData)