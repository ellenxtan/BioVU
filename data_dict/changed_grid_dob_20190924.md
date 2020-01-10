* Produced by [changed_grid_dob.R](https://github.com/meerkatR/BioVU/blob/master/changed_grid_dob.R).
* GRID level data
  * Note that an updated_grid may correspond to more than one old_grid.
* Whenever there are old GRIDs in the data
  * Convert the old GRIDs to updated GRIDs.
  * Convert all dates of the old GRIDs by __date - old_dob + updated_dob__.
* Fields
  * __old_grid__ - The GRID that needs to be changed to updated_grid
  * __old_dob__ - DOB of the old GRID
  * __updated_grid__ - The new GRID used now
  * __updated_dob__ - DOB of the updated GRID
  
  
