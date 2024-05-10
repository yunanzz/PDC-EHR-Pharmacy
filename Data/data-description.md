# PDC Sample Data (10 patients)

### Patient 1: simple case

### Patient 2: missing `start_date`

### Patient 3: missing `end_date`

### Patient 4: with non-active medications
  Med A: not active (doesn’t count)  
  Med B: active
  
### Patient 5: patient with contiguous medication orders
  Med A: two orders next to each other (contiguous)  
  Med B: two orders with 1 day difference (contiguous)  
  Med C: two orders overlapped each other (contiguous)  
  Med D: two orders with gap in between (gap days removed from denominator)
  
### Patient 6: patient with hospital stay

### Patient 7: patient with dispensed 90 days before the follow-up period not counted

### Patient 8: patient with stockpiled

### Patient 9: patient with overlapped dispensed to carryover

### Patient 10: patient with multiple medications (take average)
  Med A: medication with denominator < 28 days (doesn’t count)  
  Med B: denominator >= 28 days (count)  
  Med C: denominator >= 28 days (count)
