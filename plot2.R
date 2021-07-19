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

plot2 <- function() {
  
  library("lubridate")
  
  tmp_df <- get_df_subset()
  
  #Y axis
  tmp_y <- as.character(tmp_df$Global_active_power)
  tmp_y <- as.numeric(tmp_y) # now convert to numeric
  
  #X axis
  tmp_Date <- as.character(tmp_df$Date)
  tmp_Time <- as.character(tmp_df$Time)
  tmp_DOW <-as.character(wday(tmp_Date, label = TRUE))
  
  #merge the date and time
  tmp_posixCT <- as.POSIXct(strptime(paste(tmp_Date, tmp_Time ), format = "%Y-%m-%d %H:%M:%S"))
  tmp_posixCT <- as.numeric(tmp_posixCT) # get seconds since EPOCH
  
  # Write scatter plot to Png
  png("Plot2.png", width = 480, height = 480)
  plot(tmp_posixCT, tmp_y, type = "l", xlab = "DOW", ylab = "Global active power (kwatts)", xaxt = "n")
  axis(1, at= quantile(tmp_posixCT), labels = c("Thr", " ", "Fri", " ", "Sat"))
  dev.off() # close device
  
  # delete variables
  rm(tmp_df)
  
}