plot2 <- function() {
  #if the current working directory does not have data folder then create one
  if(!file.exists("data")) {
    dir.create("data")
  }
  #assuming the zip file for Household Power Consumption is placed under current workign directory
  #set unzip path, and unzip file to the directory
  zipdir <- "./data"
  unzip("exdata-data-household_power_consumption.zip",exdir=zipdir)
  
  #assuming there should only be one file in orginal zipped file
  #read in the txt file as it is
  files <- list.files(zipdir)
  if(length(files)>1) stop("More than one data file inside.")
  filepath <- paste(zipdir,files[1],sep="/")
  #considering ? as NA at reading time
  data <- read.table(filepath,header=TRUE,sep=";",na.string = c("?"))
  
  #getting a subset of data from 2007-2-1 to 2007-2-2
  dataset <- subset(data, Date == '1/2/2007' | Date == '2/2/2007')
  #convert Date Time field into date/time class
  datetime <- paste(dataset$Date, dataset$Time)
  datetime <- strptime(datetime,format="%d/%m/%Y %H:%M:%S")
  dataset <- cbind(dataset, datetime)
  
  #open device for PNG file output
  png(filename="plot2.png", width = 480, height = 480)
  plot(dataset$datetime,dataset$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)")
  dev.off()
  
}