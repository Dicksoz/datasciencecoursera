CreatePlot1 <- function(DAT) {
      main = "Global Active Power"
      ylim = c(0,1200)
      xlab = "Global Active Power (kilowatts)"
      png("plot1.png")
      hist(DAT$Global_active_power,main = main, ylim = ylim, xlab = xlab, col = "red")
      dev.off()
      print("Plot saved to plot1.png")
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

CreatePlot1(DownloadData)
