* Produced by [daily_sofa.R](https://github.com/meerkatR/BioVU/blob/master/daily_sofa.R)
* Daily level data
  * Note that a lab_date may belongs to more than one encounter because we count from 2 days before admission date to discharge date.
* Fields
  * __grid__
  * __adm_id__ - admission/encounter ID the lab_date belongs to
  * __day__ - # of days between lab_date and admission date, could be up to 2 days before admission date (-2)
  * __lab_date__ - date of lab values
  * __bilirubin, creatinine, platelet, rass__ - worst lab values of the day
  * __map, pf_ratio, sf_ratio__ - worst calculated MAP, PaO2/FiO2, SpO2/FiO2 values of the day
  * __pressor__ - name of the worst vasopressor taken on this day.
    * Worse order same as alphabetical order for the purpose of identifying sepsis: NOREPINEPHRINE > EPINEPHRINE > DOPAMINE > DOBUTAMINE
  * __vent__ - 1 if mechnical ventional was used on this day
  * __sofa_liver, sova_renal, sofa_coagulation, sofa_cns, sofa_cardio, sofa_respiration__ - SOFA score for 6 organ systems.
  * __sofa__ - sum of non-missing SOFA scores of 6 organ systems.
  * __data_type__ - SOFA score missing status
    * Complete data - all SOFA scores for 6 organ systems are available for that day
    * All missing - all SOFA scores for 6 organ systems are missing for that day
    * Missing > 1 system - SOFA scores of more than 1 organ system is missing.
    * No Cardio/CNS/Coagulation/Liver/Renal/Respiration SOFA - Only SOFA score of 1 organ system is missing.
