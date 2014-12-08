
retrieveUnzipData <- function(){
  download.file("http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",destfile="datafile.zip")
  dataTxtFile = unzip("datafile.zip")
  read.table(dataTxtFile,header=TRUE,sep=";")
}

parseDates <- function(data){
  data$Date = as.Date(strptime(data$Date,format="%d/%m/%Y"))
  data
}

getBetween <- function(from,to,data){
  data[(data$Date >= from & data$Date <= to) == TRUE,]
}

plot1 <- function(data){
  hist(as.double(as.character(data$Global_active_power)),col="red",xlab="Global Active Power (kilowatts)",main="Global Active Power")
}

plot2 <- function(data,main="Global Active Power",ylab="Global Active Power (kilowatts)"){
  plot(addDateAndTime(data$Date,data$Time),as.double(as.character(data$Global_active_power)),xlab="",ylab=ylab,type="l",main=main)
  
}

plot3 <- function(data,yintersp = 1,box.col="black"){  
  plot(addDateAndTime(data$Date,data$Time),month$Sub_metering_1,type="l",xlab="",ylab="Energy sub metering",pin = c(3,3))
  lines(addDateAndTime(data$Date,data$Time),month$Sub_metering_2,type="l",col="blue")
  lines(addDateAndTime(data$Date,data$Time),month$Sub_metering_3,type="l",col="red")
  legend("top",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=c(1,1,1),lwd=c(2.5,2.5,2.5),col=c("black","blue","red"),xjust=1,yjust=1,y.intersp=yintersp,x.intersp=0)
  
}


plot4 <- function(data  = getBetween("2007-02-01","2007-02-02",parseDates(retrieveUnzipData()))){
  par(mfrow = c(2,2))
  with(data, {
    plot2(data,main="",ylab="Global Active Power")
    plot(addDateAndTime(data$Date,data$Time),as.double(as.character(data$Voltage)),ylab="Voltage",xlab="datetime",type="l")
    plot3(data,yintersp=0.3,box.col=0)
    plot(addDateAndTime(data$Date,data$Time),as.double(as.character(data$Global_reactive_power)),ylab="Global_reactive_power",xlab="datetime",type="l")
  })
}

