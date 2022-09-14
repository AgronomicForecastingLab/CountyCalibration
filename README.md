# CountyCalibration
This workflow is to generated 4 different sets of simulations starting from calibration to validation and performing SDA at the county level. 

- The first set of simulations that we need to generate is a grid of samples for a set of parameters for a county. This type of simulations will be used for developing emulators at the pixel level for each county. Next, using the output of this round, emulators will be developed for maximum NDVI and date of maximum NDVI and crop parameters will be calibrated using MODIS NDVI . In this round we run maize for all county regardless of the actual rotation.

- The second set of simulations, will use the calibrated parameters to run a series of simulations for yield comparison with county yield values from NASS. 

- The third set of simulations uses APSIM default cultivars for performing baseline scenario and comparison to the calibrated simulations mentioned in the previous step. 

- Finally, calibrated parameters will be used with the real crop rotation at each pixel to perform SDA. 

The workflow is designed in a way that all simulations from all files , will be generated and placed in the simulation directory. Calibration outputs for emulator development will go to the `Outputs` directory and yield validation outputs will go the `YieldOutputs` dir. By the end of calibration (`2-Optimization.R`) each folder in the `Outputs` (for each county) will have 3 important files. `FinalGrid.RDS`, which has the grid of parameters and the desired target variable (NDVI max, etc), `Optim_Ready.RDS` has included the observed data as well into the `FinalGrid` and `Optim_results.RDS` will hold the optimized parameters for each pixel in the county. 

This directory contains all the necessary workflows for :
#### 1) Generating simulations for emulator development across a county 
Using `0-County_Calibration_Maize` we can specify a set of APSIM parameters and their corresponding range, to generate a grid of samples of then generate pSIMS simulations that allows.

#### 2) Performing the optimization and evaluating the optimized parameters 
#### 3) Generating base_line simulations for a county for comparison against the calibrated parameters 
#### 4) Using the calibrated parameters to run SDA over a county


#### Before running any large simulations, one needs to double check few different things:
1) Each pixel needs to follow it's common rotation extracted from CDL (fallow seasons are removed from the APSIM manager). 
2) Each pixel needs to have correct values for calibrated crop parameters and planting date.
3) Both crop modules (Soybean, Maize) are presented and have correct assignments to their cultivar file.
