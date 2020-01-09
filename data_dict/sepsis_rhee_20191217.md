* Produced by [rhee_organ_dysfunction.R](https://github.com/meerkatR/BioVU/blob/master/rhee_organ_dysfunction.R)
* Encounter level data
* Fields
  * __grid__
  * __adm_id__ - admission/encounter ID
  * __adm_date, dc_date__ - admission and discharge date
  * __blood_date__ - date of blood culture obtained
  * __new_pressor__ - 1 if there is vasopressor initiation
  * __new_vent__ - 1 if there is iniaitio of mechanical ventilation
  * __renal_dysfct__ - 1 if meeting Rhee's renal dysfunction criteria
  * __bil_dbl__ - 1 if bilirubin level doubled from baseline and >= 2.0 mg/dL
  * __plt_dcl__ - 1 if platelet count < 100x10^3/uL and >= 50% decline from baseline (baseline must be >= 100x10^3/uL)
  * __lactate_ge2__ - 1 if lactage >= 2.0 mmol/L
  * __rhee__ - 1 if any of __new_pressor, new_vent, renal_dysfct, bil_dbl, plt_dcl, lactate_ge2__ is 1.
  
