* Produced by [compare_sepsis.R](https://github.com/meerkatR/BioVU/blob/master/compare_sepsis.R)
* GRID level data
* The _Rhee Sepsis GRIDs_ sheet listed only GRIDs that met the Rhee definition; the _All GRIDs_ listed all grids in our cohort.
* Fields in _Rhee Sepsis GRIDs_ sheet
  * __grid__
  * __rhee__ - 1 if any of the CAM-ICU encounters of this GRID met the Rhee definition
  * __sepsis_code__ - 1 if any of the CAM-ICU enouncters of this GRID had ICD sepsis code within ±1 day of admission date
  * __sepsis3__ - 1 if any of the CAM-ICU encounters of this GRID met the Sepsis-3 definition 
  * All other variables taken from genotyping status data.
* Fields in _All GRIDs_ sheet
  * __grid__
  * __n_cam_icu_encounter__ - total # of CAM-ICU encounters of this GRID
  * __n_rhee__ - # of CAM-ICU encounters of this GRID that met the Rhee definition
  * __n_sepsis_code__ - # of CAM-ICU encounters of this GRID that had ICD sepsis code within ±1 day of admission date
  * __n_sepsis3__ - # of CAM-ICU encounters of this GRID that met the Sepsis-3 definition 
  * __sepsis__ - indicates which sepsis criteria this GRID met 
    * Rhee - if n_rhee > 0
    * Sepsis-3 but not Rhee - if n_sepsis3 > 0
    * Sepsis code only - if n_sepsis_code > 0
    * No sepsis - if n_rhee = n_sepsis_code = n_sepsis3 = 0
  * All other variables taken from genotyping status data.
  
  
