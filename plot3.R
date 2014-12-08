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

plot3 <- function(data  = getBetween("2007-02-01","2007-02-02",parseDates(retrieveUnzipData())),yintersp = 1,box.col="black"){
  plot(addDateAndTime(data$Date,data$Time),month$Sub_metering_1,type="l",xlab="",ylab="Energy sub metering")
  lines(addDateAndTime(data$Date,data$Time),month$Sub_metering_2,type="l",col="blue")
  lines(addDateAndTime(data$Date,data$Time),month$Sub_metering_3,type="l",col="red")
  legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=c(1,1,1),lwd=c(2.5,2.5,2.5),col=c("black","blue","red"),xjust=1,yjust=1,y.intersp=yintersp,x.intersp=0)
}