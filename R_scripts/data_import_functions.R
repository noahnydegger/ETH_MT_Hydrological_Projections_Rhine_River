


# Function to process data: combine date columns and filter by year range
process_data <- function(data) {
  # Combine date columns into a Date object
  data$Date <- as.Date(paste(data$YYYY, data$MM, data$DD, sep = "-"), format = "%Y-%m-%d")
  
  # Replace '.' with '_' in column names
  colnames(data) <- gsub("\\.", "_", colnames(data))
  
  # Create a YearMonth column in "YYYY-MM" format
  data <- data %>% mutate(YearMonth = format(Date, "%Y-%m"))
  
  # Filter rows between 1991 and 2020
  data <- subset(data, format(Date, "%Y") >= 1991 & format(Date, "%Y") <= 2020)
  
  # Return processed data
  return(data)
}