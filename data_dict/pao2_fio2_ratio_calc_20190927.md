* Produced by [check_resp_ratio.R](https://github.com/meerkatR/BioVU/blob/master/check_resp_ratio.R)
* Assessment level data
* Fields
  * grid
  * lab_date
  * lab_time
  * po2 - PaO2 value
  * fio2 - original FiO2 value
  * fio2_c - cleaned FiO2 value, all of range 0-1
  * ratio_c - calculated PaO2/FiO2 ratio
    * calculation: po2/fio2_c
