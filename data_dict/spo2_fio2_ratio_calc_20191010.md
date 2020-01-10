* Produced by [check_resp_ratio.R](https://github.com/meerkatR/BioVU/blob/master/check_resp_ratio.R)
* Assessment level data
* Fields
  * grid
  * lab_date
  * lab_time
  * so2 - original SpO2 (O2 saturation) value
  * so2_c - cleaned SpO2 (O2 saturation) value, all of range 0-100 
  * fio2 - original FiO2 value
  * fio2_c - cleaned FiO2 value, all of range 0-1
  * ratio_c - calculated SpO2/FiO2 ratio
    * Calculation: so2_c/fio2_c
