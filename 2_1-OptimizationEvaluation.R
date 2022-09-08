library(mgcv)
library(ggplot2)
library(dplyr)
library(purrr)
library(lubridate)
library(rtop)
library(tidyr)
library(raster)
library(furrr)
library(pSIMCampaignManager)
library(pSIMSSiteMaker)
library(tidyverse)
library(lattice)
library(ggplot2)
library(stringr)
library(ncdf4)
library(cowplot)
library(ggplotify)
library(rasterVis)
#### MAKE 
library(furrr)
plan(multiprocess)

list.dirs("Outputs", recursive = FALSE)%>%
  map(function(ss){
    print(ss)
   
    county <- strsplit(basename(ss), "_")[[1]][2]
    
    Rot_Rasters <- readRDS(file.path(getwd(),"Rotations", paste0("illinois_",county, ".RDS")))
  

    Optim_res <- readRDS(file.path(getwd(),"Outputs", paste0("illinois_",county, "/Optim_results.RDS")))
    # get the objects we need from the optimization results
    comparison <- Optim_res[[1]]
    obs_data <-  Optim_res[[2]]
    date_emulator<- Optim_res[[3]]
    ndvi_emulator<- Optim_res[[4]]
    params <-  Optim_res[[5]]

  
 All_Eval <-    params %>%
      split(.$pixel) %>%
      future_map_dfr(function(pix){
 
        
        tmpdata <- pix %>%
          mutate(tt_emerg_to_endjuv = as.numeric(tt_emerg_to_endjuv),
                 leaf_app_rate1 = as.numeric(leaf_app_rate1),
                 leaf_init_rate = as.numeric(leaf_init_rate), 
                 pdate= as.numeric(pdate)
                 ) %>%
          select(PDOY= pdate, tt_emerg_to_endjuv, leaf_app_rate1, leaf_init_rate) %>%
          bind_cols(Year = 2010:2020)
        
        tmpdata$PDOY <- tmpdata$PDOY +  (pix[,6:16] %>% as.numeric())
        
        tmpdata$OptimNDVI <- mgcv::predict.bam(ndvi_emulator[[as.character(pix$pixel)]], newdata = tmpdata)
        tmpdata$OptimNDVIday <- mgcv::predict.bam(date_emulator[[as.character(pix$pixel)]], newdata = tmpdata)
        
        tmpdata %>%
          mutate(Pixel = unique(pix$pixel)) %>%
          left_join(
            
            obs_data %>%
              filter(Pixel == pix$pixel) %>%
              select(Pixel, Year, NewObsNDVI, NDVIObsDay), 
            by=c('Pixel', 'Year')
          ) %>%
          na.omit()
        
      
      }, .progress = TRUE)
 
 dir.create(file.path(getwd(),"Plots", paste0("illinois_",county)))

 
 
 metricNDVI <- hydroGOF::gof(All_Eval$OptimNDVI %>% as.numeric(),
               All_Eval$NewObsNDVI %>% as.numeric())[,1]
 
 
 metricNDVIday <- hydroGOF::gof(All_Eval$OptimNDVIday %>% as.numeric(),
               All_Eval$NDVIObsDay %>% as.numeric())[,1]
All_Eval %>%
  ggplot(aes(OptimNDVI, NewObsNDVI))+
  geom_point()+
  geom_text(aes(x=0.72, y=0.92, label=paste0("RMSE = ", metricNDVI[4]))) +
  geom_text(aes(x=0.72, y=0.91, label=paste0("d-index = ", metricNDVI[12]))) +
  scale_x_continuous(limits = c(0.7, 0.95))+
  geom_smooth(method = "lm")+
  labs(title = county)+
  geom_abline(slope = 1, intercept = 0, linetype=2)+
  scale_y_continuous(limits = c(0.7, 0.95)) -> p1
 
All_Eval %>%
  ggplot(aes(OptimNDVIday, NDVIObsDay))+
  geom_point()+
  geom_text(aes(x=174, y=240, label=paste0("RMSE = ", metricNDVIday[4]))) +
  geom_text(aes(x=174, y=238, label=paste0("d-index = ", metricNDVIday[12]))) +
  geom_smooth(method = "lm")+
  geom_abline(slope = 1, intercept = 0, linetype=2)+
  scale_x_continuous(limits = c(170, 250))+
  scale_y_continuous(limits = c(170, 250)) -> p2

  ggsave(cowplot::plot_grid(p1, p2), filename = file.path(getwd(),"Plots", paste0("illinois_",county,"/EmulatorEvaluation.png")), 
         height = 6, width = 13)


par_plots <- params %>%
  mutate(tt_emerg_to_endjuv = as.numeric(tt_emerg_to_endjuv),
         leaf_app_rate1 = as.numeric(leaf_app_rate1),
         leaf_init_rate = as.numeric(leaf_init_rate), 
         pdate= as.numeric(pdate)) %>%
  gather(Year, Yeareff, -pixel, -pdate, -tt_emerg_to_endjuv, -leaf_app_rate1, -leaf_init_rate) %>%
  split(.$Year) %>%
  map(function(one.year.param){
  
    comparison %>%
      ungroup() %>%
      select(Pixel, lat, lon) %>%
      distinct(Pixel, .keep_all = TRUE) %>%
      left_join(one.year.param %>% 
                  rename(Pixel=pixel), by='Pixel') %>%
      mutate(pdate = pdate + as.numeric(Yeareff)) %>%
      select(-Yeareff) %>%
      gather(Var, Val, -Pixel, -lat, -lon, -Year) %>%
      split(.$Var) %>%
      map(~ .x %>% 
            ggplot(aes(lon, lat))+
            geom_raster(aes(fill=Val))+
            labs(title=unique(.x$Year))+
            scale_fill_gradient2(
              low = "#3366CC", 
              mid = "white", 
              high = "#FF3300", 
              midpoint = median(.$Val)
            )+
            theme_gray(base_size = 14)+
            facet_wrap(~ Var, scales = "free"))

  })%>% 
  flatten()


  ggsave(plot_grid(plotlist = par_plots, align = 'hv', ncol = 4), filename = file.path(getwd(),"Plots", paste0("illinois_",county,"/Params.png")), 
  height = 26, width = 17)
})
