
#########
## Load the Data ----------

library(readxl)

#data_visit
data_visit <- read_excel("Data/data_visit.xlsx")
#data_order
data_order <- read_excel("Data/data_order.xlsx")
#data_dispensed 
data_dispensed <- read_excel("Data/data_dispensed.xlsx")
#data_hosp
data_hosp <- read_excel("Data/data_hosp.xlsx")

## Step1: Load the `tidy-data` function  ----------

source(here::here("01_tidy-data.R"))
result_step1 <- pdc_date(
  ID = 'id', # column name of patient id in both data sets
  
  # Data file: Medication Order
  data_order = data_order, # name of the Medication Order Data
  Med = 'med_class', # column name of the medication class
  Order.Date = 'ordering_date', # column name of the ordering date
  Start.Date = 'start_date', # column name of the start date
  End.Date = 'end_date', # column name of the end date
  
  # Data file: Office Visit
  data_visit = data_visit, # name of the visit data
  Visit.Date = 'contact_date') # column name of the clinic visit day

# Medication Order Data that are related to the study period
data_order <- result_step1[[1]] 
# `data_order_temp` does not contain stockpiled order
data_order_temp <- result_step1[[2]] 
# Get the Office Visit Data for patients with medication order
data_visit <- result_step1[[3]]

## Step2: Medication Order ----------

source(here::here("02_med-order-active.R"))

### Step2-1: Active Medication ----------

# Need to make sure the patient has at least 1 active medicine on the visit day

result_step2 <- pdc_med_active(
  ID = 'id', # column name of patient id in both data sets
  Pat.Med.ID = 'pat_med_id', # column name for specific patient med
  
  # Data file: Medication Order
  data_order = data_order, # 1st element from previous result
  data_order_temp = data_order_temp, # 2nd element from previous result
  Start.Date = 'start_date',  # column name of the start date
  End.Date = 'end_date', # column name of the end date
  
  # Data file: Office Visit
  data_visit = data_visit, # 3rd element from previous result
  Visit.Date = 'contact_date' # column name of the clinic visit day
  )

Pat.active <- result_step2[[1]] 
PatMed.active <- result_step2[[2]]
data_visit <- result_step2[[3]]
data_order <- result_step2[[4]]

### Step2-2: Contiguous Medication Orders ----------

# According to the result from previous code chunk
# the length of records and number of unique Pat.Med.ID are not equal
# This is because one Pat.Med.ID might have multiple order records
# To find the "denominator start date", need to combine the ordering records
# and get one unique record for each `pat_med_id`

result_order_Contiguous <- pdc_order_contiguous(
 # Data file: Medication Order
  data_order = data_order, # 4th element from Step2-1
  ID = 'id', # column name of patient id
  Pat.Med.ID = 'pat_med_id', # column name for specific patient med
  Start.Date = 'start_date', # column name of the start date
  End.Date = 'end_date' # column name of the end date
  )

## Step3: pdc denominator ----------

source(here::here("03_pdc-denominator.R"))

### Step3-1: Number of days eligible for the medication (the denominator) -----

# Start.Date is (Visit.Date-180 days) if the Med.Start <= (Visit.Date-180 days)
# Start.Date is Med.Start if the Med.Start > (Visit.Date-180 days)

result_Start.Date <- pdc_denominator_start(
  # Data file: Medication Order
  data_order = result_order_Contiguous, # result from Step2-2
  ID = 'id', # column name of patient id
  Pat.Med.ID = 'pat_med_id', # column name for specific patient med
  Med = 'med_class', # column name of the medication class
  Period.Start = 'threshold_start', # follow-up period start
  Visit.Date = 'contact_date', # column name of the visit day
  Med.Start = 'start_date', # column name of the start date
  End.Date = 'end_date', # column name of the end date
  Status = 'status' # medication order status 
)


### Step3-2: Adjusting gaps between med orders: ----------

result_Gap.Adjust <- pdc_Gap.Adjust(
  ID = 'id', # column name of patient id in both data sets
  Pat.Med.ID = 'pat_med_id', # column name for specific patient med
  
  # Data file: Medication Order
  data_order = result_order_Contiguous, # result from Step 2-2
  Med.Start = 'start_date', # column name of the start date
  End.Date = 'end_date', # column name of the end date
  
  # Data file: Unadjusted Denominator
  data_denominator = result_Start.Date, # result from Step 3-1
  Period.Start = 'denominator_start_date', # follow-up period start
  Denominator = 'denominator_unadjusted' # column name of unadjusted denominator
)

### Step3-3: Adjusting denominator start date of orders: ----------

result_DenomStart.Adjust <- pdc_DenomStart.Adjust(
  ID = 'id', # column name of patient id in both data sets
  Pat.Med.ID = 'pat_med_id',  # column name for specific patient med
  
  # Data file: Medication Order
  data_order = result_order_Contiguous, # result from Step 2-2
  Med = 'med_class', # column name of the medication class
  Med.Start = 'start_date', # column name of the start date
  End.Date = 'end_date', # column name of the end date
  Period.Start = 'threshold_start', # follow-up period start
  Visit.Date = 'contact_date', # column name of the clinic visit day
  Status = 'status',  # medication order status
  
  # Data file: Med Order data with denominator
  data_denominator = result_Gap.Adjust, # result from Step 3-2
  Denom.Start = 'denominator_start_date', # column name of denominator start date
  Denominator = 'denominator' # column name of unadjusted denominator
) 

### Step3-4: Adjusting for Hospitalization ----------

result_hosp <- pdc_hosp(
  ID = 'id',  # column name of patient id in both data sets
  Pat.Med.ID = 'pat_med_id', # column name for specific patient med
  
  # Data file: Medication Order
  data_order = result_order_Contiguous, # result from Step 2-2
  Med.Start = 'start_date', # column name of the start date
  End.Date = 'end_date', # column name of the end date
  Visit.Date = 'contact_date', # column name of the clinic visit day
  
  # Data file: Med Order data with denominator
  data_denom = result_DenomStart.Adjust, # result from Step 3-3
  Denom.Start = 'denominator_start_date', # denominator start date for calculation
  Denominator = 'denominator', # column name of unadjusted denominator
  
  # Data file: Hospitalization
  data_hosp = data_hosp,  # name of the Hospitalization Data
  Adm.Date = 'adm_date', # column name of the admission date
  Disch.Date = 'disch_date' # column name of the discharge date
)

data_hosp3 <- result_hosp [[1]] 
data_order_All <- result_hosp [[2]]
data_denom_hosp <- result_hosp [[3]] 


## Step4: Data Manipulation for Pharmacy Dispensed Data ----------

source(here::here("04_med-dispensed.R"))

data_dispensed <- pdc_dispensed(
  ID = 'id',  # column name of patient id in both data sets
  Pat.Med.ID = 'pat_med_id', # column name for specific patient med
  
  # Data file: Medication Dispensed
  data_dispensed = data_dispensed, # name of the Medication Dispensed Data
  Med = 'med_class', # column name of the medication class
  Fill.Date = 'fill_date', # column name of the medication fill date
  Days.Supplied = 'days_supplied', # number of days supplied by the record
  
  # Data file: Med Order data with denominator
  data_denom = data_denom_hosp, # 3rd element from Step 3-4
  Visit.Date = 'contact_date', # column name of the clinic visit day
  Denom.Start = 'denominator_start_date', # denominator start date for calculation
  Denominator = 'denominator') # column name of unadjusted denominator

## Step5: Work on Stockpiling ----------

source(here::here("05_med-stockpiled.R"))

result_stockpiled <- pdc_stockpiled(
  ID = 'id', # column name of patient id in both data sets
  Pat.Med.ID = 'pat_med_id', # column name for specific patient med
  
  # Data file: Medication Dispensed
  data_dispensed = data_dispensed, # result from Step 4
  Fill.Date = 'fill_date', # column name of the medication fill date
  Fill.End = 'drug_disp_end', # column name of the medication fill end
  
  # Data file: Medication Order
  data_order = data_order_All, # 2nd element from Step 3-4
  Start.Date = 'start_date', # column name of the start date
  End.Date = 'end_date', # column name of the end date
  
  # Data file: Med Order data with denominator
  data_denom = data_denom_hosp,  # 3rd element from Step 3-4
  Denom.Start = 'denominator_start_date', # column name of denominator start date
  Denominator = 'denominator', # column name of unadjusted denominator
  Stock.Start = 'stock_start_date' # column name of the stockpiled start date
)

# Stockpiled Start Date the Same
data_stockpiled <- result_stockpiled[[1]]
data_stockpiled_denom <- result_stockpiled[[2]]


## Step6: From Stockpiled to Carryover --------

source(here::here("06_med-carryover.R"))

result_carryover <- pdc_carryover(
  ID = 'id', # column name of patient id in both data sets
  Pat.Med.ID = 'pat_med_id', # column name for specific patient med
  
  # Data file: Medication Order
  data_order = data_order_All, # 2nd element from Step 3-4
  Med = 'med_class', # column name of the medication class
  
  # Data file: Med Order data with denominator
  data_denom = data_denom_hosp, # 3rd element from Step 3-4
  Start.Date = 'start_date', # column name of the start date
  End.Date = 'end_date', # column name of the end date
  Denom.Start = 'denominator_start_date', # column name of denominator start date
  
  # Data file: Stockpiled Data
  data_stockpiled = data_stockpiled, # 1st element from Step 5
  Stock.Start = 'stock_start_date', # column name of the stockpiled start date
  data_stockpiled_denom = data_stockpiled_denom, # 2nd element from Step 5
  Visit.Date = 'contact_date', # column name of the clinic visit day
  
  # Data file: Medication Dispensed
  data_dispensed = data_dispensed, # result from Step 4
  Fill.Date = 'fill_date', # column name of the medication fill date
  Fill.End = 'drug_disp_end' # column name of the medication fill end
)

data_dispensed <- result_carryover[[1]]
data_carryover <- result_carryover[[2]]

## Step7: PDC Calculation -------------

source(here::here("07_pdc-calculation.R"))

result_pdc <- pdc_cal(
  ID = 'id', # column name of patient id in both data sets
  Pat.Med.ID = 'pat_med_id', # column name for specific patient med
  
  # Data file: Medication Order
  data_order = data_order_All, # result from Step 3-4
  Med = 'med_class', # column name of the medication class
  Period.Start = 'threshold_start', # follow-up period start
  
  # Data file: Med Order data with denominator
  data_denom = data_denom_hosp, # result from Step 3-4
  Med.Start = 'start_date', # column name of the start date
  End.Date = 'end_date', # column name of the end date
  Visit.Date = 'contact_date', # column name of the clinic visit day
  Denom.Start = 'denominator_start_date', # denominator start date for calculation
  Denominator = 'denominator', # column name of PDC denominator
  
  # Data file: Medication Dispensed
  data_dispensed = data_dispensed, # name of the Medication Dispensed Data
  Fill.Date = 'fill_date', # column name of the medication fill date
  Fill.End = 'drug_disp_end',  # column name of the medication fill end
  
  # Data file: Hospitalization
  data_hosp = data_hosp3, # result from Step 3-4
  Adm.Date = 'adm_date', # column name of the admission dates
  Disch.Date = 'disch_date', # column name of the discharge date
  hosp.length = 'hosp_length', # length of hospital stay
  
  # Data file: Carrryover
  data_carryover = data_carryover, # result from Step 6
  days = 'days', # column name of number of stockpiled days
  gaps = 'gaps' # column name of number of gaps for stockpiled days
  )

PDC_by_Med <- result_pdc[[1]]
PDC_by_Pat <- result_pdc[[2]]


