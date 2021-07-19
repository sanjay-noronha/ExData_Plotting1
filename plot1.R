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

plot1 <- function() {
  
  tmp_df <- get_df_subset()
  
  #get the relevant column and convert to character
  tmp_v <- as.character(tmp_df$Global_active_power)
  
  #remove "?" characters
  tmp_v <- tmp_v[tmp_v != "?"]
  
  # now convert to numeric
  tmp_v <- as.numeric(tmp_v)
  
  
  # Write hist to Png
  png("Plot1.png", width = 480, height = 480)
  hist(tmp_v, col = "red", main = "Global Active Power",  xlab = "Global Active Power (kwatts)")
  dev.off() # close device
  
  # delete variables
  rm(tmp_df)
  rm(tmp_v)

}