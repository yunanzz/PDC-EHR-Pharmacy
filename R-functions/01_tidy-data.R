
## Step1: Data Manipulation ---------

# Use the following function to get a cleaned data set without `NA` for 
# medication start date and end date

pdc_date <- function(ID, # column name of patient id in both data sets
                     
                     # Data file: Medication Order
                     data_order, # name of the Medication Order Data
                     Med, # column name of the medication class
                     Order.Date, # column name of the ordering date
                     Start.Date,  # column name of the start date
                     End.Date, # column name of the end date
                     
                     # Data file: Office Visit
                     data_visit, # name of the visit data
                     Visit.Date # column name of the clinic visit day
                     ){

  # Set up R library
  library(readxl)
  library(dplyr)
  library(stringr)
  library(readr)
  
  #########
  `%>%` <- dplyr::`%>%`
  
  # Get the start date and end date of the study period
  interest.start <- as.Date(min(as.matrix(data_visit[,Visit.Date])))
  interest.end <- as.Date(max(as.matrix(data_visit[,Visit.Date])))
  
  # Use "ordering date" to substitute the missing "start date"
  # Use `interest.end` to substitute the missing "end date"

  # Format dates in data
  data_order[,Start.Date] <- lapply(
    data_order[,Start.Date], function(date) {
      if (inherits(date, "POSIXt")) as.Date(date) else date
    })
  data_order[,Order.Date] <- lapply(
    data_order[,Order.Date], function(date) {
      if (inherits(date, "POSIXt")) as.Date(date) else date
    })
  data_order[,End.Date] <- lapply(
    data_order[,End.Date], function(date) {
      if (inherits(date, "POSIXt")) as.Date(date) else date
    })
  data_visit[,Visit.Date] <- lapply(
    data_visit[,Visit.Date], function(date) {
      if (inherits(date, "POSIXt")) as.Date(date) else date
    })
  
  data_order <- as.data.frame(data_order)
  data_order[,Start.Date] <- ifelse(is.na(data_order[,Start.Date]),
                                    data_order[,Order.Date],
                                    data_order[,Start.Date])
  data_order[,Start.Date] <- as.Date(data_order[,Start.Date])
  
  data_order[,End.Date] <- ifelse(is.na(data_order[,End.Date]),
                                  interest.end,
                                    data_order[,End.Date])
  data_order[,End.Date] <- as.Date(data_order[,End.Date])
  data_order[,'pat_med_id'] <- paste(data_order[,ID], data_order[,Med])
  
  # Get the Medication Order Data that are related to the study period
  # `data_order_temp` does not contain stockpiled order
  data_order <- filter(data_order,
                       data_order[,Start.Date] <= data_order[,End.Date])
  data_order_temp <- filter(data_order,
                        data_order[,End.Date] >= interest.start &
                          data_order[,Start.Date] <= interest.end)
  
  # Get the Office Visit Data for patients with medication order
  data_visit <- as.data.frame(data_visit)
  data_visit <- filter(data_visit, data_visit[,ID] %in% data_order_temp[,ID])
  data_visit[,"PatDate_id"] <- paste(data_visit[,ID],data_visit[,Visit.Date])

  return(list(data_order, data_order_temp, data_visit))
}
