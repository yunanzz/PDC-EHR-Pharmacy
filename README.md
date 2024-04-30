# EHR-Pharmacy PDC Calculation

This directory contains the R function to calculate EHR-Pharmacy PDC
The algorithm to calculate PDC is based on the PDC definition updated Apr 2024 
[link of the paper if published]

To make full use of the code, it is important to have the following data files:
1. `Data Order`  
    [1] `ID` unique ID of the patient record    
    [2] `Medication Class` category of the medication class   
    [3] `Ordering Date` date when the medication order was placed   
    [4] `Start Date` date when the medication order started    
    [5] `End Date` date when the medication order is to end
 
2. `Data Dispensed`     
    [1] `ID` unique ID of the patient record   
    [2] `Medication Class` category of the medication class    
    [3] `Fill Date` the dispense instant   
    [4] `Days Supplied` the number of days the dispense is written for   

3. `Data Visit`  
    [1] `ID` unique ID of the patient record  
    [2] `Visit Date` date of the contact  

4. `Data Hosp`  
    [1] `ID` unique ID of the patient record  
    [2] `Admission Date` date of the inpatient admission          
    [3] `Discharge Date` hospital discharge date 

To successfully use the code, it is important to do the following steps:  
1. the `Medication Class` is based on medication generic not the medication name
2. Make sure each patient only has one record in `Data Visit`
3. Make sure the number of `Days Supplied` >= 0 

## Notes:
- Patient does not consume pills if the date is not included in order data during stockpiled period
- Gaps between medication order are not included in the denominator

# Author  
* Yunan Zhao (Yunan.Zhao@nyulangone.org)
* Xiyue Li (Xiyue.Li@nyulangone.org)
* Tyrel Stokes (Tyrel.Stokes@nyulangone.org)

