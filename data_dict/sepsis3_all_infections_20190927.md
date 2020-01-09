* Produced by [sepsis3_infection.R](https://github.com/meerkatR/BioVU/blob/master/sepsis3_infection.R)
* Encounter level data
* Fields
  * __grid__
  * __adm_id__ - admission/encounter ID
  * __adm_date, dc_date__ - admission and discharge date
  * __onset_date__ - onset date of suspected infection, as defined by Sepsis-3 paper, was whichever earlier of antibiotics administration date and body fuild culture date
  * __onset_day__ - # of days from admission date to onset date.
    * calculation: onset_date - adm_date + 1
