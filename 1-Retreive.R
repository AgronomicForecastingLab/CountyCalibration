library(pSIMSSiteMaker)
library(pSIMCampaignManager)
library(dplyr)
library(RSQLite)
library(purrr)
library(furrr)
library(jsonlite)
plan(multisession)
sqlite.driver <- dbDriver("SQLite")
setwd("/mnt/iccp_storage/Regional_Calibration/")

host <-
  list(name = 'cc-login.campuscluster.illinois.edu',
       user = 'hamzed',
       tunnel = '~/tunnel/tunnel',
       from=paste0('/home/hamzed/pSIMS/'),
       to='/projects/aces/hamzed/psims/Data')
#-----------------------------------------------------------------------------------
#-------------------------------------------- Find the runs/dirs that starts with illinois and has sqlite3 files
#-----------------------------------------------------------------------------------
paths_withruns <- pSIMSSiteMaker::remote.execute.cmd(host, 'find  /projects/aces/hamzed/psims/Data/sims -type d -name "illinois_*"')
# Monitor jobs with RTM 
Finished_runs <-paths_withruns %>%
  map(~ suppressWarnings({remote.execute.cmd(host, paste0('find ',.x,' -name output.RDS | wc -l'))}) %>% as.numeric) %>%
  setNames(paths_withruns) %>%
  discard(~.x==0) 

Finished_runs

#-----------------------------------------------------------------------------------
#---- find the zip files and bring them back---------
#-----------------------------------------------------------------------------------
walk(names(Finished_runs)[c(2)], function(.xx){
# 

  paths_withruns_zip <- remote.execute.cmd(host,
                                           paste0('find ',.xx,' -type f -name "output.RDS"'))
  dir.create(file.path(getwd(),'/Outputs', basename(.xx)))

  #Bring them back
  walk(paths_withruns_zip, function(ss){

    
    ddname <- dirname(gsub(.xx, "", ss)) %>%
      gsub("/","_", .) %>%
      substr(2, 20)
    dir.create(file.path(getwd(), '/Outputs', basename(.xx), ddname))
    
    remote.copy.from(host=host,
                       src=ss,
                       dst=file.path(getwd(), '/Outputs', basename(.xx), ddname),
                       delete = TRUE)
  })
  
  paths_withruns_json<- remote.execute.cmd(host,
                                           paste0('find ',.xx,' -type f -name "experiment.json"'))
  # dir.create(file.path(getwd(),'/Outputs', basename(.xx)))
  
  #Bring them back
  walk(paths_withruns_json, function(ss){
 
    remote.copy.from(host=host,
                     src=ss,
                     dst=file.path(getwd(),'/Outputs', basename(.xx), gsub("/","_",dirname(gsub(.xx,"",ss))%>%substr(2, 20))),
                     delete = TRUE)
  })
})


#----------------------------------------------------------------------------------
#----------------------------------- Calibration ---------------------------------
#----------------------------------------------------------------------------------
list.dirs("Outputs", recursive = FALSE) [1]%>%
  map(function(ss){
  print(ss)

    # Reading results
    tmpall <- list.files(ss, "output.RDS", recursive = TRUE, full.names = TRUE) %>%
      map_dfr(~ readRDS(.x))

    outcounty <- tmpall %>%
      mutate(lat=as.numeric(lat), 
             lon= as.numeric(lon)) %>%
      dplyr::select(Year=year, Ens=ens, Pixel, NDVI, Yield=yield, lat, lon) %>%
      mutate(Pixel=map_chr(Pixel, ~ gsub("_projects_aces_hamzed_psims_Data_sims_illinois_gallatin_illinois_gallatin_","", .x))
             )


    #---------- Reading param
    allexp <- list.files(ss,"experiment.json", recursive = TRUE, full.names = TRUE)
    one.exp <- jsonlite::fromJSON(allexp[1])
    
    
    #Same across all years
    crop.param <- one.exp$experiments$planting[,c("tt_emerg_to_endjuv","leaf_init_rate","leaf_app_rate1")] %>%
      mutate(Ens=1:n())
    
    # All pixels share the same properties for plants for each ensemble
    # Now we have to make maize planting dates for each year and add that to this above dataframe
    
    maizedates <- allexp %>%
      purrr::map_dfr(function(ss){
     
        tmp_exp <- jsonlite::fromJSON(ss)$experiments$management$events
        
        tmp_exp_df <- seq_len(length(tmp_exp)) %>%
          purrr::map_df(~ tmp_exp[[.x]] %>% mutate(Ens=.x)) %>%
          filter(crid=='maize', event=='planting') %>%
          mutate(Year=lubridate::year(anytime::anydate(date)), 
                 DOY=lubridate::yday(anytime::anydate(date))) %>%
          dplyr::select(Year, DOY, date, Ens) %>%
          mutate(Pixel=substr(gsub("_RTM","",strsplit(ss,'\\/')[[1]][c(3)]),1, 20))
        
      })
    
    Maize.df <- maizedates %>%
      left_join(crop.param, by='Ens') %>%
      dplyr::select(-date)

    #----- Joining to make the final grid for the emulator
    FGrid <- outcounty %>%
      left_join(Maize.df, by=c('Year'='Year', 'Ens'='Ens','Pixel'='Pixel'))
    
    saveRDS(FGrid, file.path(ss, "FinalGrid.RDS"))
  })


