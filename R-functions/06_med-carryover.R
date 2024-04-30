
########################################################################
## Step6: From Stockpiled to Carryover --------

pdc_carryover <- function(ID, # column name of patient id in both data sets
                          Pat.Med.ID, # column name for specific patient med
                          
                          # Data file: Medication Order
                          data_order, # 2nd element from Step 3-4
                          Med, # column name of the medication class

                          # Data file: Med Order data with denominator
                          data_denom, # 3rd element from Step 3-4
                          Start.Date, # column name of the start date
                          End.Date, # column name of the end date
                          Denom.Start, # column name of denominator start date
                          
                          # Data file: Stockpiled Data
                          data_stockpiled, # 1st element from Step 5
                          Stock.Start, # column name of the stockpiled start date
                          data_stockpiled_denom, # 2nd element from Step 5
                          Visit.Date, # column name of the clinic visit day
                          
                          # Data file: Medication Dispensed
                          data_dispensed, # result from Step 4
                          Fill.Date, # column name of the medication fill date
                          Fill.End  # column name of the medication fill end
                          ){
  
  ## Set up R library ----------
  library(readxl)
  library(dplyr)
  library(stringr)
  library(readr)
  
  #########
  `%>%` <- dplyr::`%>%`
  
  # Order Data for Stockpiled
  colnames <- names(data_denom)
  Order.s <- data_order %>%
    dplyr::select(any_of(colnames))
  
  Order.s <- left_join(Order.s, data_denom[,c(Pat.Med.ID,Denom.Start)])
  Order.s <- filter(Order.s, Order.s[,Start.Date] <
                      Order.s[,Denom.Start])
  
  data_stockpiled_denom[,"StockpiledPeriod"] <- as.numeric(
    difftime(data_stockpiled_denom[,Denom.Start],
             data_stockpiled_denom[,Stock.Start],
             units = c("days")))
  
  Dispense.s <- left_join(data_stockpiled[
    ,c(ID, Pat.Med.ID, Fill.Date, Fill.End, Visit.Date, Denom.Start)],
    data_stockpiled_denom[
      ,c(Pat.Med.ID, Stock.Start,"StockpiledPeriod")], by = Pat.Med.ID)

  temp <- aggregate(Dispense.s[,ID],
                    list(Dispense.s[,Pat.Med.ID]),
                    FUN=function(x){NROW(x)})
  names(temp)[1] <- Pat.Med.ID
  temp <- filter(temp, x > 1)
  PatMed_Stockpiled <- unique(Dispense.s[,Pat.Med.ID])
  
  Days_Stockpiled <- NA
  Gaps_Stockpiled <- NA
  
  for (i in 1:length(PatMed_Stockpiled)) {
    pat_med_i <- PatMed_Stockpiled[i]
    order_i <- Order.s[Order.s[,Pat.Med.ID] == pat_med_i,]
    dispense_i <- Dispense.s[Dispense.s[,Pat.Med.ID] == pat_med_i,]
    StartDate_i <- unique(Dispense.s[Dispense.s[,Pat.Med.ID] == pat_med_i,
                                     Stock.Start])
    StockpiledPeriod_i <- unique(Dispense.s[
      Dispense.s[,Pat.Med.ID] == pat_med_i, "StockpiledPeriod"]) -1
      days_i <- 0
      gap_i <- 0
      
    for (j in 0:StockpiledPeriod_i) {
      date_j <- StartDate_i + j
      Order_med_j <- ifelse(sum(order_i[,Start.Date] <= date_j &
                                  order_i[,End.Date] >= date_j) > 0,1,0)
      N_med_j <- sum(dispense_i[,Fill.Date] <= date_j & 
                       dispense_i[,Fill.End] >= date_j)
      gap_i <- ifelse(Order_med_j == 0, gap_i+1, 0)
      days_i <- ifelse(days_i+N_med_j-Order_med_j < 0, 0,
                       days_i + N_med_j - Order_med_j)
      if (gap_i>30){
        days_i <- 0
        gap_i <- 0
      }
    }
    Days_Stockpiled <- append(Days_Stockpiled, days_i)
    Gaps_Stockpiled <- append(Gaps_Stockpiled, gap_i)
  }
  
  Stockpiled_days <- as.data.frame(cbind(PatMed_Stockpiled,
                                         Days_Stockpiled[-1],
                                         Gaps_Stockpiled[-1]))
  names(Stockpiled_days) <- c(Pat.Med.ID,"days","gaps")
  Stockpiled_days[,'days'] <- as.numeric(Stockpiled_days[,'days'])
  Stockpiled_days[,'gaps'] <- as.numeric(Stockpiled_days[,'gaps'])
  Stockpiled_days[,'days'] <- ifelse(Stockpiled_days[,'days'] < 0, 0,
                                     Stockpiled_days[,'days'])
  Stockpiled_days[,'gaps'] <- ifelse(Stockpiled_days[,'gaps'] < 0, 0,
                                     Stockpiled_days[,'gaps'])
  
  data_dispensed <- filter(data_dispensed,
                           data_dispensed[,Fill.End] >=
                             data_dispensed[,Denom.Start])
  data_dispensed <- left_join(data_dispensed,
                              unique(data_order[,c(Med,Pat.Med.ID)]))
  PatMed_Order <- as.data.frame(unique(data_order[,Pat.Med.ID]))
  names(PatMed_Order) <- Pat.Med.ID
  Carryover_days0 <- left_join(PatMed_Order, Stockpiled_days)
  Carryover_days <- left_join(Carryover_days0,
                              as.data.frame(unique(data_order[
                                ,c(ID,Pat.Med.ID)])))
  Carryover_days[,'days'] <- ifelse(is.na(Carryover_days[,'days']),0,
                                    Carryover_days[,'days'])
  Carryover_days[,'gaps'] <- ifelse(is.na(Carryover_days[,'gaps']),0,
                                    Carryover_days[,'gaps'])
  
  return(list(data_dispensed,Carryover_days))
}

