
########################################################################
## Step3-1: Number of days eligible for the medication (the denominator) -------

# Start.Date is (Visit.Date-180 days) if the Med.Start <= (Visit.Date-180 days)
# Start.Date is Med.Start if the Med.Start > (Visit.Date-180 days)

pdc_denominator_start <- function(
    ID, # column name of patient id in both data sets
    Pat.Med.ID, # column name for specific patient med
    
    # Data file: Medication Order
    data_order, # result from Step2-2
    Med, # column name of the medication class
    Period.Start, # follow-up period start
    Visit.Date, # column name of the visit day
    Med.Start, # column name of the start date
    End.Date, # column name of the end date
    Status # medication order status 
    ){
  
  ## Set up R library
  library(readxl)
  library(dplyr)
  library(stringr)
  library(readr)
  
  ##########
  `%>%` <- dplyr::`%>%`
  
  # selecting the relevant columns
  data_order <- data_order[,c(ID, Med, Pat.Med.ID,
                              Period.Start, Visit.Date,
                              Med.Start, End.Date, Status)]
  
  # start date of PDC calculation
  data_order[,"denominator_start_date"] <- data_order[,Period.Start]
  data_order <- as.data.frame(data_order)
  data_order[,"denominator_start_date"] <- ifelse(
    data_order[,Med.Start] > data_order[,Period.Start],
    data_order[,Med.Start],
    data_order[,Period.Start])
  data_order[,"denominator_start_date"] <- as.Date(
    data_order[,"denominator_start_date"])
  
  data_order[,Status] <- ifelse(
    data_order[,Visit.Date] > data_order[,Med.Start] &
      data_order[,Visit.Date] <= data_order[,End.Date],
    "Active", "Non-active")
  
  # Exclude record with Med.Start after the Visit.Date
  data_order <- filter(data_order,
                       data_order[,Med.Start] <= data_order[,Visit.Date])
  
  # Keep one medical record for each patient to calculate the denominator
  # The record is based on the active order med with earliest Med.Start
  ActiveOrderList <- data_order[data_order[,Status] == 'Active',
                                Pat.Med.ID]
  data_active <- filter(data_order, data_order[,Pat.Med.ID] %in%
                          ActiveOrderList)
  
  # Sort the data set by Pat.Med.ID and Med.Start
  data_active <- data_active[order(data_active[,Pat.Med.ID],
                                   data_active[,Med.Start]),]
  data_active2 <- filter(data_active,
                         data_active[,End.Date] >= data_active[,Period.Start])
  data_active2 <- data_active2 %>%
    group_by(data_active2[[Pat.Med.ID]]) %>%
    slice_head() %>%
    ungroup %>%
    select(1:9) 
  
  data_active2 <- as.data.frame(data_active2)
  data_active2[,"denominator_unadjusted"] <- difftime(
    data_active2[,Visit.Date],
    data_active2[,"denominator_start_date"],
    units = c("days"))
  
  return(data_active2)
}

########################################################################
##  Step3-2: Adjusting gaps between med orders: ----------

pdc_Gap.Adjust <- function(ID, # column name of patient id in both data sets
                           Pat.Med.ID, # column name for specific patient med
                           
                           # Data file: Medication Order
                           data_order, # result from Step 2-2
                           Med.Start, # column name of the start date
                           End.Date, # column name of the end date
                           
                           # Data file: Unadjusted Denominator
                           data_denominator, # result from Step 3-1
                           Period.Start, # follow-up period start
                           Denominator # column name of unadjusted denominator
){
  # Get the Medication Order Data that are related to the follow up period
  data_order <- as.data.frame(data_order)
  Pat_Gap <- unique(data_denominator[,Pat.Med.ID])
  Days_Gap <- NA

  for (i in 1:length(Pat_Gap)) {
    pat_med_id_i <- Pat_Gap[i]
    order_i <- data_order[data_order[,Pat.Med.ID] == pat_med_id_i,]
    StartDate_i <- data_denominator[
      data_denominator[,Pat.Med.ID] == pat_med_id_i, Period.Start]
    OrderPeriod_i <- as.numeric(data_denominator[
      data_denominator[,Pat.Med.ID] == pat_med_id_i, Denominator]-1)
    days_i <- 0
    if (dim(order_i)[1] == 1){
      days_i <- 0
    }
    if (dim(order_i)[1] > 1){
      for (j in 0:OrderPeriod_i) {
        date_j <- StartDate_i + j
        N_med_j <- sum(order_i[,Med.Start] <= date_j &
                         order_i[,End.Date] >= date_j)
        days_i <- ifelse(N_med_j > 0, days_i, days_i+1)
      }
    }
    Days_Gap <- append(Days_Gap, days_i)
  }
  
  data_denominator[,"DaysGap"] <- Days_Gap[-1]
  data_denominator[,"denominator"] <- data_denominator[,Denominator] -
    data_denominator[,"DaysGap"]
  return(data_denominator)
}

########################################################################
## Step3-3: Adjusting denominator start date of orders: ----------

pdc_DenomStart.Adjust <- function(ID, # column name of patient id in both data sets
                                  Pat.Med.ID, # column name for specific patient med
                                  
                                  # Data file: Medication Order
                                  data_order, # result from Step 2-2
                                  Med, # column name of the medication class
                                  Med.Start, # column name of the start date
                                  End.Date, # column name of the end date
                                  Period.Start, # follow-up period start
                                  Visit.Date, # column name of the clinic visit day
                                  Status, # medication order status 
                                  
                                  # Data file: Med Order data with denominator
                                  data_denominator, # result from Step 3-2
                                  Denom.Start, # column name of denominator start date
                                  Denominator # column name of unadjusted denominator
                                  ){
  # start date of PDC calculation
  data_order[,"denominator_start_date"] <- data_order[,Period.Start]
  data_order <- as.data.frame(data_order)
  data_order[,"denominator_start_date"] <- ifelse(
    data_order[,Med.Start] > data_order[,Period.Start],
    data_order[,Med.Start],
    data_order[,Period.Start])
  data_order[,"denominator_start_date"] <- as.Date(
    data_order[,"denominator_start_date"])
  
  data_order[,Status] <- ifelse(
    data_order[,Visit.Date] > data_order[,Med.Start] &
      data_order[,Visit.Date] <= data_order[,End.Date],
    "Active", "Non-active")
  
  dat <- unique(inner_join(data_order[
    ,c(ID, Med, Pat.Med.ID, Period.Start, Med.Start, End.Date, Status,
       Denom.Start)],
    data_denominator[,c(Pat.Med.ID,Visit.Date, Denominator)],
    by = Pat.Med.ID))
  
  OrderDispensed_DenomStart <- dat %>%
    group_by(dat[[Pat.Med.ID]]) %>%
    slice_head() %>%
    ungroup() %>%
    select(1:10)
  
  OrderDispensed_DenomStart <- as.data.frame(OrderDispensed_DenomStart)
  OrderDispensed_DenomStart[,Denom.Start] <- ifelse(
    OrderDispensed_DenomStart[,Period.Start] >= OrderDispensed_DenomStart[,Med.Start],
    OrderDispensed_DenomStart[,Period.Start] ,OrderDispensed_DenomStart[,Med.Start]
  )
  OrderDispensed_DenomStart[,Denom.Start]  <- as.Date(OrderDispensed_DenomStart[,Denom.Start] )
  
  # Use inner_join to combine "ordered_exclude" and "dispensed" data set by `pat_med_id`:
  data_denominator <- left_join(data_denominator[
    ,!names(data_denominator) == Denom.Start],
    OrderDispensed_DenomStart[,c(Pat.Med.ID, Denom.Start)])
  
  return(data_denominator)
}


########################################################################
## Step3-4: Adjusting for Hospitalization ----------

pdc_hosp <- function(ID, # column name of patient id in both data sets
                     Pat.Med.ID, # column name for specific patient med
                     
                     # Data file: Medication Order
                     data_order, # result from Step 2-2
                     Med.Start, # column name of the start date
                     End.Date, # column name of the end date
                     Visit.Date, # column name of the clinic visit day
                     
                     # Data file: Med Order data with denominator
                     data_denom, # result from Step 3-3
                     Denom.Start, # denominator start date for calculation
                     Denominator, # column name of unadjusted denominator
                     
                     # Data file: Hospitalization
                     data_hosp, # name of the Hospitalization Data
                     Adm.Date, # column name of the admission date
                     Disch.Date # column name of the discharge date
                     ){
  
  # Format dates in data
  data_hosp[,Adm.Date] <- lapply(
    data_hosp[,Adm.Date], function(date) {
      if (inherits(date, "POSIXt")) as.Date(date) else date
    })
  data_hosp[,Disch.Date] <- lapply(
    data_hosp[,Disch.Date], function(date) {
      if (inherits(date, "POSIXt")) as.Date(date) else date
    })
  data_hosp <- as.data.frame(data_hosp)
  
  # Sort the data set by ID, Adm.Date, Disch.Date
  data_hosp <- data_hosp[order(data_hosp[,ID],
                               data_hosp[,Adm.Date],
                               data_hosp[,Disch.Date]),]
  
  # Adjustment for Overlapping Hospitalization Records
  data_hosp2 <- data_hosp[1,]
  hospIDs <- as.matrix(unique(data_hosp[,ID]))
  for (i in 1:length(hospIDs)) {
    if (sum(data_hosp[,ID] == hospIDs[i]) == 1){
      data_hosp2 <- rbind(data_hosp2,
                          data_hosp[data_hosp[,ID]==hospIDs[i],])
    }
    if (sum(data_hosp[,ID] == hospIDs[i]) > 1){
      hospID_temp <- filter(data_hosp, data_hosp[,ID]==hospIDs[i])
      r <- dim(hospID_temp)[1] - 1
      for (j in 1:r) {
        if(hospID_temp[,Adm.Date][j+1] <= hospID_temp[,Disch.Date][j]+1){
          hospID_temp[,Adm.Date][j+1] <- hospID_temp[,Adm.Date][j]
        }
        if(hospID_temp[,Disch.Date][j+1] < hospID_temp[,Disch.Date][j]){
          hospID_temp[,Disch.Date][j+1] <- hospID_temp[,Disch.Date][j]
          }
      }
      data_hosp2 <- rbind(data_hosp2, hospID_temp)
      }
  }
  data_hosp2 <- data_hosp2[-1,] 
  data_hosp3 <- data_hosp2[order(data_hosp2[,ID],
                                 data_hosp2[,Adm.Date],
                                 data_hosp2[,Disch.Date]),]
  
  data_hosp3[,"group"] <- paste(data_hosp3[,ID],data_hosp3[,Adm.Date])
  data_hosp3 <- data_hosp3 %>%
    group_by(data_hosp3[["group"]]) %>%
    slice_tail() %>%
    ungroup %>%
    select(1:3) 
    
  ActiveOrderList <- data_denom[,Pat.Med.ID]
  data_order <- as.data.frame(data_order)
  data_order <- filter(data_order, data_order[,Med.Start] <=
                         data_order[,Visit.Date])
  data_order <- filter(data_order, data_order[,Pat.Med.ID] %in%
                         ActiveOrderList)
  data_denom_hosp <- left_join(data_order,
                               data_denom[,c(Pat.Med.ID, Denom.Start)])
  
  data_denom_hosp <- left_join(data_denom_hosp, data_hosp3)
  
  data_denom_hosp <- filter(
    data_denom_hosp, data_denom_hosp[,Adm.Date] < data_denom_hosp[,Visit.Date]
    & data_denom_hosp[,Disch.Date] >= data_denom_hosp[,Denom.Start]
    & data_denom_hosp[,Adm.Date] <= data_denom_hosp[,End.Date]
    & data_denom_hosp[,Disch.Date] >= data_denom_hosp[,Med.Start]
    & data_denom_hosp[,End.Date] >= data_denom_hosp[,Denom.Start]
  )
  
  data_denom_hosp[,Adm.Date] <- ifelse(
    data_denom_hosp[,Adm.Date] < data_denom_hosp[,Denom.Start],
    data_denom_hosp[,Denom.Start],data_denom_hosp[,Adm.Date])
  data_denom_hosp[,Adm.Date] <- ifelse(
    data_denom_hosp[,Adm.Date] < data_denom_hosp[,Med.Start],
    data_denom_hosp[,Med.Start],data_denom_hosp[,Adm.Date])
  data_denom_hosp[,Adm.Date] <- as.Date(data_denom_hosp[,Adm.Date])
  
  data_denom_hosp[,Disch.Date] <- ifelse(
    data_denom_hosp[,Disch.Date] >= data_denom_hosp[,Visit.Date],
    data_denom_hosp[,Visit.Date]-1,data_denom_hosp[,Disch.Date])
  data_denom_hosp[,Disch.Date] <- ifelse(
    data_denom_hosp[,Disch.Date] >= data_denom_hosp[,End.Date],
    data_denom_hosp[,End.Date],data_denom_hosp[,Disch.Date])
  data_denom_hosp[,Disch.Date] <- as.Date(data_denom_hosp[,Disch.Date])
  
  data_denom_hosp[,"hosp_length"] <- data_denom_hosp[,Disch.Date]-
    data_denom_hosp[,Adm.Date]+1
  hosp_temp <- aggregate(data_denom_hosp[,"hosp_length"],
                         list(data_denom_hosp[,Pat.Med.ID]), FUN = sum)
  names(hosp_temp) <- c("pat_med_id", "hosp_length")
  
  data_denom <- left_join(data_denom, hosp_temp)
  data_denom[,"hosp_length"] <- ifelse(is.na(data_denom[,"hosp_length"] ),
                                       0,data_denom[,"hosp_length"] )
  data_denom[,Denominator] <- data_denom[,Denominator] -
    data_denom[,"hosp_length"]
  
  return(list(data_hosp3, data_order, data_denom))
}
