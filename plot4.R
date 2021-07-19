# get the size of the data frame
getObjectSize <- function() {
  
  tmp_df <- read.delim("household_power_consumption.txt", header = TRUE, sep = ";")
  tmp_size <-  object.size(tmp_df)
  rm(tmp_df)
  
  return(tmp_size)
  
}

# We will only be using data from the dates 2007-02-01 and 2007-02-02
get_df_subset <- function(){
  
  tmp_df <- read.delim("household_power_consumption.txt", header = TRUE, sep = ";")
  
  # now convert factor > posixlt > date
  tmp_posix_lt <- strptime(tmp_df$Date,"%d/%m/%Y")
  tmp_df$Date <- as.Date(tmp_posix_lt, "%Y-%m-%d")
  rm(tmp_posix_lt)
  
  #now filter dates 
  tmp_df_2_days <- tmp_df[tmp_df$Date >= as.Date("2007-02-01", format= "%Y-%m-%d") & tmp_df$Date <= as.Date("2007-02-02", format= "%Y-%m-%d"),]
  rm(tmp_df)
  
  #handle NA values
  #tmp_df_2_days[ tmp_df_2_days == "?" ] <- NA
  
  return(tmp_df_2_days)
  
}



plot4 <- function() {
  
  library("lubridate")
  
  tmp_df <- get_df_subset()
  
  #Y axis
  tmp_y_1 <- as.numeric(tmp_df$Sub_metering_1)
  tmp_y_2 <- as.numeric(tmp_df$Sub_metering_2)
  tmp_y_3 <- as.numeric(tmp_df$Sub_metering_3)
  
  #X axis
  tmp_Date <- as.character(tmp_df$Date)
  tmp_Time <- as.character(tmp_df$Time)
  tmp_DOW <-as.character(wday(tmp_Date, label = TRUE))
  
  #merge the date and time
  tmp_posixCT <- as.POSIXct(strptime(paste(tmp_Date, tmp_Time ), format = "%Y-%m-%d %H:%M:%S"))
  tmp_posixCT <- as.numeric(tmp_posixCT) # get seconds since EPOCH
  
  # generate the chart matrix
  par(mfrow = c(2,2), mar=c(4, 4, 2,1)) # setting matrix and margins
  
  # Write scatter plot to Png
  #png("Plot4.png", width = 620, height = 800, res = 100)
  
  for(i in 1:4) 
  {
    
    # Plot 1
    #Y axis
    if ( i == 1){
      tmp_y <- as.character(tmp_df$Global_active_power)
      tmp_y <- as.numeric(tmp_y) # now convert to numeric
      plot(tmp_posixCT, tmp_y, type = "l", ylab = "Global active power (kwatts)",xlab = "",   xaxt = "n")
      #x axis ticks
      axis(1, at= quantile(tmp_posixCT), labels = c("Thr", " ", "Fri", " ", "Sat"))
   }
    # Plot 2
    #Y axis
    if ( i == 2){
      tmp_y <- as.character(tmp_df$Voltage)
      tmp_y <- as.numeric(tmp_y) # now convert to numeric
      plot(tmp_posixCT, tmp_y, type = "l", ylab = "Voltage", xlab = "",  xaxt = "n")
      #x axis ticks
      axis(1, at= quantile(tmp_posixCT), labels = c("Thr", " ", "Fri", " ", "Sat"))
    }
    
    
    # Plot 3
    if ( i == 3){
      plot(tmp_posixCT, tmp_y_1, ylab = "Energy sub metering", xlab = "", type = "n", xaxt = "n")
      points(tmp_posixCT, tmp_y_1, type = "l", col ="black")
      points(tmp_posixCT, tmp_y_2, type = "l", col ="red")
      points(tmp_posixCT, tmp_y_3, type = "l", col ="blue")
      legend("topright", pch = "-", col = c("black", "red", "blue"), legend = c("SM 1", "SM 2", "SM 3"))
      #x axis ticks
      axis(1, at= quantile(tmp_posixCT), labels = c("Thr", " ", "Fri", " ", "Sat"))
    }
    
    # Plot 4
    #Y axis
    if ( i == 4){
      tmp_y <- as.character(tmp_df$Global_reactive_power)
      tmp_y <- as.numeric(tmp_y) # now convert to numeric
      plot(tmp_posixCT, tmp_y, type = "l", ylab = "Global_reacive_power", xlab = "", xaxt = "n")
      #x axis ticks
      axis(1, at= quantile(tmp_posixCT), labels = c("Thr", " ", "Fri", " ", "Sat"))
    }
  
   }
  
  #dev.off() # close device
  
  # delete variables
  rm(tmp_df)
  
}
