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

plot2 <- function(data  = getBetween("2007-02-01","2007-02-02",parseDates(retrieveUnzipData())),main="Global Active Power",ylab="Global Active Power (kilowatts)"){
  plot(addDateAndTime(data$Date,data$Time),as.double(as.character(data$Global_active_power)),xlab="",ylab=ylab,type="l",main=main)
  
}