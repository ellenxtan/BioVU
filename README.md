# BioVU Project Code

The goal of the project is to study the association between mtDNA haplogroups and delirium in sepsis patients.  The initial steps have been done here:
1. Identify hospital encounters with at least one CAM-ICU assessments.
2. Determine daily status during hospitalization. 
3. Identify encounters with sepsis.

## Step 1. Determine daily status and identify CAM-ICU encounters
### Code:
* [reconstruct_daily_visit_data.R](https://https://github.com/meerkatR/BioVU/blob/master/reconstruct_daily_visit_data.R)  
### What:
1. Clean and combine CAM-ICU data and RASS data by GRID and assessment time, resolve discrepancy
    * RASS measures how awake and alert patient is, usually assesed every 4 hours in ICU. Valid RASS scores:   
      * from -5 to 4
      * If RASS score is -5 or -4, patient is in coma, and CAM-ICU value should be UA 
    * CAM-ICU is a tool to detect delirium in ICU patients, usually assesed every 8 hours in ICU. Valid CAM-ICU values:   
      * Positive - Delirium present
      * Negative - No deliruim
      * UA - Patient in coma, cannot assess CAM-ICU score
      * Unk - This value is assigned in anlysis for conflicting CAM-ICU values at the same time point
 
### Input data: 
* Girard_BioVU/output/data_raw.RData
* Girard_BioVU/output/changed_grid_dob_20190924.csv  
### Output data:
* Girard_BioVU/output/daily_status_20190925.csv
* Girard_BioVU/output/cam_stay_20190925.csv
