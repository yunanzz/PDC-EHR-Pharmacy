
########################################################################
## Step5: Work on Stockpiling ----------

pdc_stockpiled <- function(ID, # column name of patient id in both data sets
                           Pat.Med.ID, # column name for specific patient med
                           
                           # Data file: Medication Dispensed
                           data_dispensed, # result from Step 4
                           Fill.Date, # column name of the medication fill date
                           Fill.End, # column name of the medication fill end
                           
                           # Data file: Medication Order
                           data_order, # 2nd element from Step 3-4
                           Start.Date, # column name of the start date
                           End.Date, # column name of the end date
                           
                           # Data file: Med Order data with denominator
                           data_denom, # 3rd element from Step 3-4
                           Denom.Start, # column name of denominator start date
                           Denominator, # column name of unadjusted denominator
                           Stock.Start # column name of the stockpiled start date
                           ){
  
  ## Set up R library ----------
  library(readxl)
  library(dplyr)
  library(stringr)
  library(readr)
  
  #########
  `%>%` <- dplyr::`%>%`
  
  data_stockpiled <- filter(
    data_dispensed,
    data_dispensed[,Fill.Date] >= data_dispensed[,Stock.Start] &
      data_dispensed[,Fill.Date] < data_dispensed[,Denom.Start])
  
  colnames <- names(data_denom)
  
  ## Keep one medical record for each patient for Stockpiled denominator
  data_order_stockpiled <- data_order %>%
    group_by(data_order[[Pat.Med.ID]]) %>%
    slice_head() %>%
    ungroup %>%
    dplyr::select(any_of(colnames))
  
  data_stockpiled <- left_join(data_stockpiled,
                               data_order_stockpiled[
                                 ,c(Pat.Med.ID, Start.Date, End.Date)])
  
  data_stockpiled [,"check"] <- ifelse(data_stockpiled[,Stock.Start]>=
                                         data_stockpiled[,Fill.Date]&
                                         data_stockpiled [,Stock.Start]<=
                                         data_stockpiled [,Fill.End],
                                       TRUE,FALSE)

  data_stockpiled_denom <- data_stockpiled %>%
    group_by(data_stockpiled[[Pat.Med.ID]]) %>%
    slice_head() %>%
    ungroup %>%
    dplyr::select(1:11)

  data_stockpiled_denom <- as.data.frame(data_stockpiled_denom)
  data_stockpiled_denom[,Stock.Start] <- ifelse(
    data_stockpiled_denom[,Stock.Start] < data_stockpiled_denom[,Fill.Date],
    data_stockpiled_denom[,Fill.Date], data_stockpiled_denom[,Stock.Start]
  )
  data_stockpiled_denom[,Stock.Start] <- as.Date(
    data_stockpiled_denom[,Stock.Start])
  data_stockpiled_denom[,'check'] <- ifelse(
    data_stockpiled_denom[,Stock.Start] >= data_stockpiled_denom[,Fill.Date] &
      data_stockpiled_denom[,Stock.Start] <= data_stockpiled_denom[,Fill.End],
    TRUE, FALSE)
  
  data_stockpiled_denom <- filter(data_stockpiled_denom,
                                  data_stockpiled_denom[,'check']==TRUE)
  return(list(data_stockpiled, data_stockpiled_denom))
}
