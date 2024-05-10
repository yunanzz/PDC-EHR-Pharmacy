
### Step2-1: Active Medication ----------

# Need to make sure the patient has at least 1 active medicine on visit day

pdc_med_active <- function(ID, # column name of patient id in both data sets
                           Pat.Med.ID, # column name for specific patient med
                           
                           # Data file: Medication Order
                           data_order, # 1st element from previous result
                           data_order_temp, # 2nd element from previous result
                           Start.Date,  # column name of the start date
                           End.Date, # column name of the end date
                           
                           # Data file: Office Visit
                           data_visit, # 3rd element from previous result
                           Visit.Date # column name of the clinic visit day
                           ){
  
  # Set up R library
  library(readxl)
  library(dplyr)
  library(stringr)
  library(readr)
  
  #########
  `%>%` <- dplyr::`%>%`
  
  # Join matching records from `data_visit` to `data_order_temp`
  dat <- left_join(data_order_temp, data_visit[,c(ID,Visit.Date)])
  
  # Visit.Date in `data_visit` is between Start.Date and End.Date
  dat <- filter(dat, dat[,Visit.Date] >= dat[,Start.Date] &
                  dat[,Visit.Date] <= dat[,End.Date])
  
  Pat_active <- unique(dat[,ID])
  PatMed_active <- unique(dat[,Pat.Med.ID])
  data_visit <- filter(data_visit, data_visit[,ID] %in% Pat_active)
  data_order <- filter(data_order, data_order[,ID] %in% Pat_active)
  data_order[,"status"] <- ifelse(data_order[,Pat.Med.ID] %in% PatMed_active,
                                  "Active", "Non-active")
  ordered_active <- left_join(data_order, data_visit)
  
  # We are looking at whether the patient taking medicine in the past 180 day
  # with 90 days stockpilling period.
  # Records with End.Date earlier than (Visit.Date-270 days) is not useful
  ordered_active <- filter(ordered_active,
                           ordered_active[,End.Date] >=
                             ordered_active[,Visit.Date] - 270)
  
  # earliest denominator start date
  ordered_active[,"threshold_start"] <- ordered_active[,Visit.Date] - 180
  
  return(list(Pat_active, PatMed_active, data_visit, ordered_active))  
}

## Step2-2: Contiguous Medication Orders ----------

# According to the result from previous code chunk
# the length of records and number of unique Pat.Med.ID are not equal
# This is because one Pat.Med.ID might have multiple order records
# To find the "denominator start date", need to combine the ordering records
# and get one unique record for each `pat_med_id`

pdc_order_contiguous <- function(
    ID, # column name of patient id
    Pat.Med.ID, # column name for specific patient med
    
    # Data file: Medication Order
    data_order, # 4th element from Step2-1
    Start.Date,  # column name of the start date
    End.Date # column name of the end date
    ){  
  
  # Set up R library
  library(readxl)
  library(dplyr)
  library(stringr)
  library(readr)
  
  #########
  `%>%` <- dplyr::`%>%`
  
  # Sort the data set by Pat.Med.ID and Start.Date
  data_order <- data_order[order(data_order[,Pat.Med.ID],
                                 data_order[,Start.Date],
                                 data_order[,End.Date]),]
  data_order2 <- vector("list", length = nrow(data_order))
  pmIDs <- as.matrix(unique(data_order[,Pat.Med.ID]))
  
  for (i in 1:length(pmIDs)) {
    ind <- data_order[,Pat.Med.ID] == pmIDs[i]
    if (sum(ind) == 1){
      data_order2[[i]] <- data_order[ind,]
    }
    
    if (sum(ind) > 1){
      pmID_temp <- data_order[ind,]
      r <- sum(ind)-1
      for (j in 1:r) {
        current_end <- pmID_temp[,End.Date][j]
        if (pmID_temp[,Start.Date][j+1] <= current_end+1) {
          pmID_temp[,Start.Date][j+1] <- pmID_temp[,Start.Date][j]
        }
        if(pmID_temp[,End.Date][j+1] < current_end){
          pmID_temp[,End.Date][j+1] <- current_end
        }
      }
      data_order2[[i]] <- pmID_temp
    }
  }
  
  data_order_df <- dplyr::bind_rows(data_order2)
  data_order3 <- data_order_df[order(data_order_df[,Pat.Med.ID],
                                     data_order_df[,Start.Date],
                                     data_order_df[,End.Date]),]
  
  data_order3[,"group"] <- paste(data_order3[,ID],data_order3[,Start.Date])
  data_order3 <- data_order3  %>%
    group_by(data_order3[[Pat.Med.ID]], group) %>%
    slice_tail() %>%
    ungroup
  
  return(data_order3)
}
