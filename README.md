# BioVU Project Code

The goal of the project is to study the association between mtDNA haplogroups and delirium in sepsis patients.  The initial steps have been done here:
1. Determine daily status during hospitalization. 
2. Identify hospital encounters with at least one CAM-ICU assessments.
3. Identify encounters with sepsis.

## Step 1 & 2. Determine daily status and identify CAM-ICU encounters
The code [reconstruct_daily_visit_data.R](https://github.com/meerkatR/BioVU/blob/master/reconstruct_daily_visit_data.R) does the following: 
1. Clean and combine CAM-ICU data and RASS data, resolves discrepancy
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
   * For each day, the daily status is:
      * Delirious if any CAM-ICU value was Positive.
      * Otherwise Unknown: conflicting CAM if any CAM-ICU value was Unk.
      * Otherwise comatose if any RASS value was -5 or -4.
      * Otherwise Normal if any CAM-ICU value was Negative.
      * Otherwise Unknown: RASS only if all CAM-ICU values were missing and at least one non-missing RASS value.
      * Otherwise Unknown: No CAM nor RASS if all CAM-ICU and RASS values were missing.
    * __Output Data:__ _Girard_BioVU/output/daily_status_20190925.csv_
3. Identify encounters with at least one CAM-ICU assessment and get encounter/visit level summary
      * For each admission/discharge record, find all dates from admission date to discharge dates.
      * Merge with daily status obtained above (keep Comatose, Delirious, Normal only) and redefine admission/discharge dates
         * Consecutive dates were considered as one encounter, and the first/last dates of the group of consectuvie dates were taken as admission/discharge dates.
         * The reason we did all this was because 1) some admission/discharge record did not have a discharge date 2) around 20,000 hospital dates with daily status did not fall into any of the admission/discharge records and we do not want to throw them away. 
      * Calculate a few summary statistics at encounter level and remove encounters without any CAM-ICU.  
      * __Output Data:__ _Girard_BioVU/output/cam_stay_20190925.csv_
### Input data: 
* _Girard_BioVU/output/data_raw.RData_
   * Including admission/discharge data, CAM data, and RASS data
   * Produced by Girard_BioVU/code/no_git/20181211_import_data.R
* _Girard_BioVU/output/changed_grid_dob_20190924.csv_  
   * Data used for correct GRIDs and dates
   * Produced by Girard_BioVU/code/changed_grid_dob.R
   * find total # of days in hospital, total # of days with CAM-ICU (to be accurate, known daily status, including Comatose, Delirious, Normal), first and last CAM-ICU and # of days inbetween, total # of com/delirium/normal
### Report:
Refer to these reports for general ideas and more details. However, note that none of them correct for changed GRIDs.
* _Girard_BioVU/code/no_git/20190619_cam_gap.html_ 
* _Girard_BioVU/code/no_git/20190319_daily_status.html_
* _Girard_BioVU/code/no_git/20190716_visit_summary.html_

## Step 3. Identify sepsis
There are three ways to identify sepsis.
1. Rhee definition (currently used)
2. Sepsis-3 definition
3. Sepsis ICD code
### Rhee definition
1. Identify CAM-ICU encounters that meet Rhee's presumed serious infection definition.
   * To find >= 4 QADs starting within 2 days of blood culture day: 
      * Find whether an antibiotic was new, i.e., not given in the prior 2 calendar days.
      * Keep only new antibiotics given within 2 days of blood culture day, these are the starting dates of QADs.
      * Check whether there are 4 QADs counting from the starting dates. 
         * For starting daysCalculate # of calender days, 
   * __Code__: [rhee_infection.R](https://github.com/meerkatR/BioVU/blob/master/rhee_infection.R) 
   * __Output Data:__ _Girard_BioVU/output/rhee_infection_20191015.csv_

2. Among the presumed serious infections identified above, find which ones met Rhee's acute organ dysfunction definition. 
   * __Code__: [rhee_organ_dysfunction.R](https://github.com/meerkatR/BioVU/blob/master/rhee_organ_dysfunction.R)
   * __Output Data:__ _Girard_BioVU/output/sepsis_rhee_20191217.csv_


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
The code [compare_sepsis.R](https://github.com/meerkatR/BioVU/blob/master/compare_sepsis.R) does the following:
1. Compare three criteria at encounter level.
   * __Output Data:__ _Girard_BioVU/output/sepsis_compare_20191217.csv_
2. Find distinct GRIDs with sepsis and see which ones have genotype data
   * __Output Data:__ 
      * _Girard_BioVU/output/grid_not_in_genotype_status_20200106.csv_
      * _Girard_BioVU/output/sepsis_grids_20200106.xlsx_
3. Check the encounters with sepsis code but negative for both sepsis definitions
4. We Decide to use Rhee definition only to identify sepsis for now.

### Report
* _Girard_BioVU/code/20191120_sepsis_compare.html_
   * Having missing data summary for Sepsis-3 definition.
* _Girard_BioVU/code/20200106_sepsis_compare.html_
   * Most current version of comparing three criteria.

## Misc.
### Changed GRIDs
2000+ GRIDs were changed due to EHR system switching.  Since the dates were shifted by different amount for each GRID, not only the GRIDs but also the dates need to be corrected.  The code [changed_grid_dob.R](https://github.com/meerkatR/BioVU/blob/master/changed_grid_dob.R) outputs the DOBs for old and updated GRIDs.  When correcting for changed GRIDs, all the dates should be converted using date - old_dob + updated_dob.  Technically, only the older data had the changed GRIDs problem, however, I recommend __always__ check for old GRIDs for any data used.
* __Input Data:__
   * _Girard_BioVU/output/data_raw.RData_
      * static_raw had all GRIDs in old EHR system and DOB.
   * _Mito Delirium BioVU Data/Data/Changed_GRIDS.xlsx_ 
      * Old and updated GRIDs only, no DOB.
   * _Mito Delirium BioVU Data/Demographics/Set\_*\_20180830_demo.txt_
      * GRID, primary GRID (if GRID was old and changed), and DOB for all GRIDs.
      * DOB discrepancy between this file and the other two sources.
   * _Mito Delirium BioVU Data/Demographics/Sample_Genotyping_Status.xlsx_
      * GRID and DOB. 
* __Output Data:__ 
   * _Girard_BioVU/output/changed_grid_dob_20190924.csv_ 
   * _Girard_BioVU/output/dob_discrepancy.csv_
      * discrepancy in DOB between _Mito Delirium BioVU Data/Demographics/Set_*_20180830_demo.txt_ and other two sourcese for DOB, can ignore.
      
### Check Respiratory Ratio
The code [check_resp_ratio.R](https://github.com/meerkatR/BioVU/blob/master/check_resp_ratio.R) calculates respiration ratios for SOFA score.  I believe we will get more respiration data in the future.
1. Calculate PaO2/FiO2 and compare with already available ratio data.
   * Decide to use calculated PaO2/FiO2 instead of already available ratio data.
   * __Input Data:__
      * _Mito Delirium BioVU Data/Lab values/PO2_FIO2_ratio/*.xlsx_ is the already available ratio data.
      * _Mito Delirium BioVU Data/Lab values/FIO2/*.xlsx_
      * _Mito Delirium BioVU Data/Lab values/Arterial pO2/*.xlsx_
   * __Output Data:__ _Girard_BioVU/output/pao2_fio2_ratio_calc_20190927.csv_
2. Check and correct FiO2 values
   * FiO2 is a fraction and should be 0-1. 
   * Any FiO2 >= 100 was divided by 100.
   * Check FiO2 < 0.21 with Nasal O2 data. 
3. Calculate SpO2/FiO2
   * __Input Data:__ 
      * _Mito Delirium BioVU Data/Lab values/FIO2/*.xlsx_
      * _Mito Delirium BioVU Data/Lab values/O2Sat/*.xlsx_
    * __Output Data:__ _Girard_BioVU/output/spo2_fio2_ratio_calc_20191010.csv_


### Check Sepsis Discrepancy
The code [check_sepsis_discrepancy.R](https://github.com/meerkatR/BioVU/blob/master/check_sepsis_discrepancy.R) checks why some encounters only met the Rhee definition but not the Sepsis-3 definition.

### Check patient location
The code [check_pt_loc.R](https://github.com/meerkatR/BioVU/blob/master/check_pt_loc.R) tabulates patient location datato see whether it will help to identify whether they were in ICU. Decide not to use for now. 
* __Input Data:__ _Mito Delirium BioVU Data/Lab values/patient_Location/*.xlsx_ 
* __Output Data:__ _Girard_BioVU/output/patient_cam_visit_location_count.csv_
