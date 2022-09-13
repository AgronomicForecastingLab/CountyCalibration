# CountyCalibration

This directory contains all the necessary workflows for :
#### 1) Generating simulations for emulator development across a county 
Using `0-County_Calibration_Maize` we can specify a set of APSIM parameters and their corresponding range, to generate a grid of samples of then generate pSIMS simulations that allows 

#### 2) Performing the optimization and evaluating the optimized parameters 
#### 3) Generating base_line simulations for a county for comparison against the calibrated parameters 
#### 4) Using the calibrated parameters to run SDA over a county


#### Before running any large simulations, one needs to double check few different things:
1)Each pixel needs to follow it's common rotation extracted from CDL. 
2) Each pixel needs to have correct values for calibrated crop parameters and planting date