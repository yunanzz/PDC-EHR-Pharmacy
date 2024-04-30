
########################################################################
## Step4: Data Manipulation for Pharmacy Dispensed Data ----------

pdc_dispensed <- function(ID, # column name of patient id in both data sets
                          Pat.Med.ID, # column name for specific patient med
                          
                          # Data file: Medication Dispensed
                          data_dispensed, # name of the Medication Dispensed Data
                          Fill.Date, # column name of the medication fill date
                          Days.Supplied, # number of days supplied by the record
                          
                          # Data file: Med Order data with denominator
                          data_denom, # 3rd element from Step 3-4
                          Visit.Date, # column name of the clinic visit day
                          Denom.Start, # denominator start date for calculation
                          Denominator # column name of unadjusted denominator
                          ){
  
  ## Set up R library ----------
  library(readxl)
  library(dplyr)
  library(stringr)
  library(readr)
  
  #########
  
  `%>%` <- dplyr::`%>%`
  
  # Format dates in data
  data_dispensed[,Fill.Date] <- lapply(
    data_dispensed[,Fill.Date], function(date) {
      if (inherits(date, "POSIXt")) as.Date(date) else date
    })
  
  ActiveOrderList <- data_denom[,Pat.Med.ID]
  data_dispensed <- as.data.frame(data_dispensed)
  data_dispensed <- filter(data_dispensed, data_dispensed[,Pat.Med.ID] %in%
                             ActiveOrderList)
  data_dispensed <- filter(data_dispensed, data_dispensed[,Days.Supplied] >= 0)
  
  data_dispensed[,"drug_disp_end"] <- data_dispensed[,Fill.Date] + 
    data_dispensed[,Days.Supplied] - 1
  data_dispensed <- unique(inner_join(data_dispensed[
    ,c(ID, Pat.Med.ID, Fill.Date,"drug_disp_end")],
    data_denom[,c(Pat.Med.ID, Visit.Date, Denom.Start, Denominator)]))
  
  data_dispensed[,"stock_start_date"] <- data_dispensed[,Denom.Start] - 90
  data_dispensed <- filter(data_dispensed, data_dispensed[,Fill.Date] >=
                             data_dispensed[,"stock_start_date"] &
    data_dispensed[,Fill.Date] <= data_dispensed[,Visit.Date])

  # sort the data set by Pat.Med.ID and Fill.Date
  data_dispensed <- data_dispensed[order(data_dispensed[,Pat.Med.ID],
                                         data_dispensed[,Fill.Date]),]
  
  return(data_dispensed)
}
