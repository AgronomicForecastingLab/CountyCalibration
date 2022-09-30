library(mgcv)
library(ggplot2)
library(dplyr)
library(purrr)
library(lubridate)
library(rtop)
library(tidyr)
library(furrr)
plan(multisession)
setwd("/mnt/iccp_storage/Regional_Calibration")

list.dirs("Outputs", recursive = FALSE)[1]%>%
  future_map(possibly(
    function(ss){
      print(ss)
      
      county <- strsplit(basename(ss), "_")[[1]][2]
      
      Rot_Rasters <- readRDS(file.path(getwd(),"Rotations", paste0("illinois_",county, ".RDS")))
      # Optim ready file
      comparison <- readRDS(file.path(ss, 'Optim_Ready.RDS')) %>%
        mutate(tt_emerg_to_endjuv= as.numeric(tt_emerg_to_endjuv), 
               leaf_init_rate = as.numeric(leaf_init_rate), 
               leaf_app_rate1=as.numeric(leaf_app_rate1)
        )
      
      grid <- comparison %>%
        select(Year, Pixel, lat, lon) %>%
        group_by(Year, Pixel) %>%
        slice_head(n=1)
      
      grid <- grid %>%
        split(.$Year) %>%
        map_dfr(function(one.year.crop){
          
          one.year.crop$Crop <- Rot_Rasters[[unique(one.year.crop$Year) %>% as.character()]] %>%
            raster::extract(one.year.crop[,c('lon','lat')])
          one.year.crop
        })
      
      
      comparison <- comparison %>%
        left_join(grid %>% select(Year, Pixel, Crop), by=c('Pixel', 'Year'))%>%
        filter(Crop==1)%>%
        na.omit()
      
      #------------Emulator for difference in peak days-----#############
      length(comparison$Pixel%>%unique())
      comparison$Pixel <- as.factor(comparison$Pixel)
      
      names(comparison)
      attributes(comparison)$na.action <- NULL
      
      date_emulator <- comparison%>%
        split(.$Pixel)%>%
        map(possibly(function(x){
          
          bam(NDVIday~ti(PDOY, by=Year)+
                ti(tt_emerg_to_endjuv)+
                ti(leaf_app_rate1)+
                ti(leaf_init_rate),
              data = x)
        }, otherwise = NULL))
      
      #------------Emulator for difference in peak NDVI-----#############
      
      ndvi_emulator <- comparison%>%
        split(.$Pixel)%>%
        map(function(x){
          
          bam(NDVI~ti(PDOY, by=Year)+
                ti(tt_emerg_to_endjuv)+
                ti(leaf_app_rate1)+
                ti(leaf_init_rate),
              data = x)
        })
      
      #-------------Observed data--------------#
      
      # str(comparison)
      obs_data <- comparison%>%
        ungroup() %>%
        dplyr::select(Pixel,Year, NewObsNDVI, NDVIObsDay)%>%
        group_by(Pixel,Year)%>%
        mutate(NewNDVI=mean(NewObsNDVI),date_obs=mean(NDVIObsDay))%>%
        as.data.frame()%>%
        distinct()
      ##-----------Numerical Optimization---------##
      params <- data.frame()
      
      for(i in 1:length(unique(obs_data$Pixel))){
        
        obj_fun <- function(prms){
          
          data <- obs_data%>%
            filter(Pixel==(obs_data$Pixel%>%unique())[i])%>%
            bind_cols(PDOY = prms[1],
                      tt_emerg_to_endjuv=prms[2],
                      leaf_app_rate1=prms[3],
                      leaf_init_rate=prms[4]) %>%
            mutate(Year=as.numeric(as.character(Year))) %>%
            complete(Year=2010:2020)
          
          
          data$PDOY <- data$PDOY +  prms[5:15]
          
          rmse_date <- (sqrt(sum((mgcv::predict.bam(date_emulator[[i]], newdata = data)-data$date_obs)^2,
                                 na.rm=TRUE)/length(data$date_obs)))/
            mean(data$date_obs, na.rm=TRUE)*100
          
          rmse_NDVI <- (sqrt(sum((mgcv::predict.bam(ndvi_emulator[[i]], newdata = data)-data$NewNDVI)^2, 
                                 na.rm=TRUE)/length(data$NewNDVI)))/
            mean(data$NewNDVI,  na.rm=TRUE)*100
          
          
          rmse <- rmse_date + rmse_NDVI
          if(!is.finite(rmse)) {
            rmse <- 1e5
          }
          rmse 
        }
        
        pars <- sceua(obj_fun,
                      pars = c(130,270,45,20, rep(0, 11)),
                      lower = c(120,200,40,15, rep(-20, 11)),
                      upper = c(170,500,65,30, rep(20, 11))
        )
        
        params <- rbind(params, c(as.character(obs_data$Pixel%>%unique())[i],round(pars$par,2)))
        
        print(i)
        
      }
      
      colnames(params) <- c('pixel','pdate','tt_emerg_to_endjuv','leaf_app_rate1','leaf_init_rate',
                            '2010','2011','2012','2013','2014','2015',
                            '2016','2017','2018','2019','2020')
      
      params <-  cbind(Pixel = params[,1], apply(params[2:16], 2, as.numeric) %>% as.data.frame())
      
      saveRDS(list(comparison= comparison, obs_data = obs_data, date_emulator= date_emulator, ndvi_emulator = ndvi_emulator, params = params), file.path(ss, "Optim_results.RDS"))
    }
    , otherwise = NULL), .progress = TRUE)
