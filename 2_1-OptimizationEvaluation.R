library(mgcv)
library(ggplot2)
library(dplyr)
library(purrr)
library(lubridate)
library(rtop)
library(tidyr)

list.dirs("Outputs", recursive = FALSE)[2:3] %>%
  map(function(ss){
    print(ss)
   
    county <- strsplit(basename(ss), "_")[[1]][2]
    
    Rot_Rasters <- readRDS(file.path(getwd(),"Rotations", paste0("illinois_",county, ".RDS")))

})
