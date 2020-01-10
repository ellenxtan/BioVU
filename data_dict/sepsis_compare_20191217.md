* Produced by [compare_sepsis.R](https://github.com/meerkatR/BioVU/blob/master/compare_sepsis.R)
* Encounter level data
* Fields
  * __grid__
  * __adm_id__ - admission/encounter ID
  * __adm_date, dc_date__ - date of admission/discharge
  * __sofa__ - SOFA score
  * __sepsis3__ - 1 if encounter meets Sepsis-3 definition
  * __data_type__ - SOFA score missing status
    * Complete data - all SOFA scores for 6 organ systems are available for that day
    * All missing - all SOFA scores for 6 organ systems are missing for that day
    * Missing > 1 system - SOFA scores of more than 1 organ system is missing.
    * No Cardio/CNS/Coagulation/Liver/Renal/Respiration SOFA - Only SOFA score of 1 organ system is missing.
  * __rhee__ - 1 if encounter meets Rhee definition
  * __sepsis_code_adm__ - 1 if there is a ICD sepsis code during the admission/encounter
  * __sepsis_code_w1d__ - 1 if there is a ICD sepsis code within ±1 day of admission.
  * __sepsis3_infection__ - 1 if the encounter meets Sepsis-3's suspected infection criteria
  * __rhee_infection__ - 1 if the encounter meets Rhee's presumed serious infection criteria
