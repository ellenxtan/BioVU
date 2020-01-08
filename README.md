# BioVU Project Code

The goal of the project is to study the association between mtDNA haplogroups and delirium in sepsis patients.  The initial steps have been done here:
1. Determine daily status during hospitalization. 
2. Identify hospital encounters with at least one CAM-ICU assessments.
3. Identify encounters with sepsis.

## Step 1 & 2. Determine daily status and identify CAM-ICU encounters
### Code:
* [reconstruct_daily_visit_data.R](https://https://github.com/meerkatR/BioVU/blob/master/reconstruct_daily_visit_data.R)  
### Report:
Refer to these reports for general ideas and more details. However, note that none of them correct for changed GRIDs.
* _Girard_BioVU/code/no_git/20190619_cam_gap.html_ 
* _Girard_BioVU/code/no_git/20190319_daily_status.html_
* _Girard_BioVU/code/no_git/20190716_visit_summary.html_
### What:
1. Clean and combine CAM-ICU data and RASS data, resolve discrepancy
    * CAM-ICU is a tool to detect delirium in ICU patients, usually assesed every 8 hours in ICU. Valid CAM-ICU values:   
      * Positive - Delirium present
      * Negative - No deliruim
      * UA - Patient in coma, cannot assess CAM-ICU score
      * Unk - This value is assigned in anlysis for conflicting CAM-ICU values at the same time point
    * RASS measures how awake and alert patient is, usually assesed every 4 hours in ICU. Obtaining a RASS score is the first step in administering CAM-ICU. Valid RASS scores are from -5 to 4. 
      * If RASS score is -3 to 4, CAM-ICU is assessable and should be either Positive or Negative.
      * If RASS score is -5 or -4, patient is in coma, and CAM-ICU value should be UA. 
     * CAM-ICU and RASS data was joined by GRID and assessment time.
2. Determine daily status  
      * Delirious if any CAM-ICU value was Positive.
      * Otherwise Unknown: conflicting CAM if any CAM-ICU value was Unk.
      * Otherwise comatose if any RASS value was -5 or -4.
      * Otherwise Normal if any CAM-ICU value was Negative.
      * Otherwise Unknown: RASS only if all CAM-ICU values were missing and at least one non-missing RASS value.
      * Otherwise Unknown: No CAM nor RASS if all CAM-ICU and RASS values were missing.
      * Output _Girard_BioVU/output/daily_status_20190925.csv_
3. Identify encounters with at least one CAM-ICU assessment and get encounter/visit level summary
      * For each admission/discharge record, find all dates from admission date to discharge dates.
      * Merge with daily status obtained above (keep Comatose, Delirious, Normal only) and redefine admission/discharge dates
         * Consecutive dates were considered as one encounter, and the first/last dates of the group of consectuvie dates were taken as admission/discharge dates.
         * The reason we did all this was because 1) some admission/discharge record did not have a discharge date 2) around 20,000 hospital dates with daily status did not fall into any of the admission/discharge records and we do not want to throw them away. 
      * Calculate a few summary statistics at encounter level and remove encounters without any CAM-ICU.  
      * Output _Girard_BioVU/output/cam_stay_20190925.csv_
### Input data: 
* _Girard_BioVU/output/data_raw.RData_
   * Including admission/discharge data, CAM data, and RASS data
   * Produced by Girard_BioVU/code/no_git/20181211_import_data.R
* _Girard_BioVU/output/changed_grid_dob_20190924.csv_  
   * Data used for correct GRIDs and dates
   * Produced by Girard_BioVU/code/changed_grid_dob.R
### Output data:
* _Girard_BioVU/output/daily_status_20190925.csv_
* _Girard_BioVU/output/cam_stay_20190925.csv_
   * find total # of days in hospital, total # of days with CAM-ICU (to be accurate, known daily status, including Comatose, Delirious, Normal), first and last CAM-ICU and # of days inbetween, total # of com/delirium/normal

## Step 3. Identify sepsis
There are three ways to identify sepsis.
1. Rhee definition (currently used)
2. Sepsis-3 definition
3. Sepsis ICD code
### Rhee definition
1. Identify CAM-ICU encounters that meet Rhee's presumed serious infection definition.
   * __Code__: [rhee_infection.R](https://github.com/meerkatR/BioVU/blob/master/rhee_infection.R) 
   * __Output Data:__ _Girard_BioVU/output/rhee_infection_20191015.csv_

2. Among the presumed serious infections identified above, find which ones met Rhee's acute organ dysfunction definition. 
   * __Code__: [rhee_organ_dysfunction.R](https://github.com/meerkatR/BioVU/blob/master/rhee_organ_dysfunction.R)
   * __Output Data:__ _Girard_BioVU/output/sepsis_rhee_20191217.csv_
#### Report:

### Sepsis-3 definition
1. Identify CAM-ICU encounters that meet Sepsis-3's suspected infection definition.
   * __Code__: [sepsis3_infection.R](https://github.com/meerkatR/BioVU/blob/master/sepsis3_infection.R) 
   * __Output Data:__ _Girard_BioVU/output/sepsis3_all_infections_20190927.csv_
2. calculate daily SOFA score for all CAM-ICU encounters.
   * __Code__: [daily_sofa.R](https://github.com/meerkatR/BioVU/blob/master/daily_sofa.R) 
   * __Output Data:__ _Girard_BioVU/output/daily_sofa_score_20191010.csv_
3. Among the suspected infections identified above, find which ones met Sepsis-3's organ dysfunction definition.
   * __Code__: [sepsis3.R](https://github.com/meerkatR/BioVU/blob/master/sepsis3.R) 
   * __Output Data:__ _Girard_BioVU/output/sepsis3_20191014.csv_

### Compare three criteria
* __Code__: [compare_sepsis.R](https://github.com/meerkatR/BioVU/blob/master/compare_sepsis.R) 
* __Output Data:__ 
   * _Girard_BioVU/output/sepsis_compare_20191217.csv_


## Misc.
### Changed GRIDs
