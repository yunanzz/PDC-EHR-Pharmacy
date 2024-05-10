
########################################################################
## Step7: PDC Calculation ----------

pdc_cal <- function(ID, # column name of patient id in both data sets
                    Pat.Med.ID, # column name for specific patient med
                    
                    # Data file: Medication Order
                    data_order, # result from Step 3-4
                    Med, # column name of the medication class
                    Period.Start, # follow-up period start
                    
                    # Data file: Med Order data with denominator
                    data_denom, # result from Step 3-4
                    Med.Start, # column name of the start date
                    End.Date, # column name of the end date
                    Visit.Date, # column name of the clinic visit day
                    Denom.Start, # denominator start date for calculation
                    Denominator, # column name of PDC denominator
                    
                    # Data file: Medication Dispensed
                    data_dispensed, # name of the Medication Dispensed Data
                    Fill.Date, # column name of the medication fill date
                    Fill.End,  # column name of the medication fill end
                    
                    # Data file: Hospitalization
                    data_hosp, # result from Step 3-4
                    Adm.Date, # column name of the admission date
                    Disch.Date, # column name of the discharge date
                    hosp.length, # length of hospital stay
                    
                    # Data file: Carrryover
                    data_carryover, # result from Step 6
                    days, # column name of number of stockpiled days
                    gaps # column name of number of gaps for stockpiled days
                    ){
  
  ## Set up R library ----------
  library(readxl)
  library(dplyr)
  library(stringr)
  library(writexl)
  library(readr)
  
  #########
  
  `%>%` <- dplyr::`%>%`
  
  Pat_Complete <- as.data.frame(unique(data_order[,ID]))
  names(Pat_Complete)[1] <- ID
  pdc_patmed <- NA
  pdc_numerator <- NA
  
  for (i in 1:dim(Pat_Complete)[1]) {
    pat_i <- Pat_Complete[i,1]
    order_i <- data_order[data_order[,ID]==pat_i,]
    dispense_i <- data_dispensed[data_dispensed[,ID]==pat_i,]
    hosp_i <- as.data.frame(data_hosp[data_hosp[,ID]==pat_i,])
    StartDate_i <- order_i[,Period.Start]
    med_i <- unique(order_i[,Med])
    N_med <- length(med_i)
    pdc_i <- rep(0, N_med)
    Carryover_i <- data_carryover[data_carryover[,ID]==pat_i,]
    
    for(j in 1:N_med){
      pdc_j <- 0
      order_j <- order_i[order_i[,Med]==med_i[j],]
      dispense_j <- dispense_i[dispense_i[,Med] == med_i[j],]
      StartDate_j <- StartDate_i[j]
      patmed_j <- paste(pat_i,med_i[j])
      gap_j <- Carryover_i[Carryover_i[,Pat.Med.ID]==patmed_j,gaps]
      
      for (k in 0:179) {
        # Task 1: pdc score on the date
        # Task 2: is carryover increase/decrease/no change?
        
        date_j <- StartDate_j + k
        order_k <- sum(date_j >= order_j[,Med.Start] &
                         date_j <= order_j[,End.Date])
        hosp_k <- sum(date_j >= hosp_i[,Adm.Date] &
                        date_j <= hosp_i[,Disch.Date])
        dispense_jk <- sum(date_j >= dispense_j[,Fill.Date] &
                             date_j <= dispense_j[,Fill.End])
        carryover_jk <- Carryover_i[Carryover_i[,Pat.Med.ID]==patmed_j,days]
        
        # If the med is NOT active on the day
        if(order_k == 0){
          if(carryover_jk >0){
            gap_j <- gap_j+1
          }
          if (gap_j >30){
            Carryover_i[Carryover_i[,Pat.Med.ID]==patmed_j,days] <- 0
            carryover_jk <- 0
          }
          pdc_jk <- 0
          Carryover_i[Carryover_i[,Pat.Med.ID]==patmed_j,days] <-
            carryover_jk+dispense_jk
        }
        
        # If the med is active on the day
        if(order_k > 0){
          gap_j  <- 0 
          
          # if the patient is NOT in hospital
          if(hosp_k == 0){
            pdc_jk <- ifelse(dispense_jk>0 | carryover_jk>0, 1, 0)
            Carryover_i[Carryover_i[,Pat.Med.ID]==patmed_j,days]  <-
              ifelse(dispense_jk>0|carryover_jk>0,
                     carryover_jk+dispense_jk-1,carryover_jk)
          }
          # if the patient is in hospital
          if(hosp_k > 0){
            pdc_jk <- 0
            Carryover_i[Carryover_i[,Pat.Med.ID]==patmed_j,days] <-
              carryover_jk+dispense_jk
          }
        }
        pdc_j <- pdc_j + pdc_jk
      }
      
      pdc_patmed <- append(pdc_patmed, patmed_j)
      pdc_numerator <- append(pdc_numerator, pdc_j)
    }
  }
  
  pdc_numerator2 <- as.data.frame(cbind(pdc_patmed[-1],pdc_numerator[-1]))
  names(pdc_numerator2) <- c(Pat.Med.ID, "numerator")
  
  #pdc per patient medication
  pat_pdc <- left_join(data_denom, pdc_numerator2)
  pat_pdc$numerator <- as.numeric(pat_pdc$numerator)
  
  pat_pdc <- filter(pat_pdc[,c(ID,Pat.Med.ID,Denom.Start,Visit.Date,hosp.length,
       Denominator,"numerator")])
  
  pat_pdc[,"PDCpat"] <- round(pat_pdc[,"numerator"]*100/as.numeric(
    pat_pdc[,Denominator]), 4)
  pat_pdc[,"PDCpat"] <- ifelse(pat_pdc[,"PDCpat"] > 100,100,pat_pdc[,"PDCpat"])
  pat_pdc[,"PDCpat"] <- ifelse(is.na(pat_pdc[,"PDCpat"]),0,pat_pdc[,"PDCpat"])
  
  # pdc per patient
  pdc_nmed <- aggregate(pat_pdc[,Pat.Med.ID], list(pat_pdc[,ID]),
                        FUN=function(x){NROW(x)})
  pat_pdc[,Denominator] <- as.numeric(pat_pdc[,Denominator])
  pat_pdc_28 <- filter(pat_pdc, pat_pdc[,Denominator] >= 28)
  pdc_temp <- aggregate(pat_pdc_28[,"PDCpat"], list(pat_pdc_28[,ID]), FUN = mean)
  names(pdc_temp) <- c(ID,'PDCavg')
  pdc_temp <- left_join(pdc_temp, unique(pat_pdc[,c(ID,Visit.Date)]))

  return(list(pat_pdc,pdc_temp))
}

