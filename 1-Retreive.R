library(pSIMSSiteMaker)
library(pSIMCampaignManager)
library(dplyr)
library(RSQLite)
library(purrr)
library(furrr)
library(jsonlite)
library(phenex)
plan(multisession)
sqlite.driver <- dbDriver("SQLite")
setwd("/mnt/iccp_storage/Regional_Calibration/")

host <-
  list(name = 'cc-login.campuscluster.illinois.edu',
       user = 'hamzed',
       tunnel = '~/tunnel/tunnel',
       from=paste0('/home/hamzed/pSIMS/'),
       to='/projects/aces/hamzed/psims/Data')

outdir <- '/Outputs'
#-----------------------------------------------------------------------------------
#-------------------------------------------- Find the runs/dirs that starts with illinois and has sqlite3 files
#-----------------------------------------------------------------------------------
paths_withruns <- pSIMSSiteMaker::remote.execute.cmd(host, 'find  /projects/aces/hamzed/psims/Data/sims -type d -name "illinois_*"')
# Monitor jobs with RTM 
Finished_runs <-paths_withruns %>%
  map(~ suppressWarnings({remote.execute.cmd(host, paste0('find ',.x,' -name *.sqlite3 | wc -l'))}) %>% as.numeric) %>%
  setNames(paths_withruns) %>%
  discard(~.x==0) 

Finished_runs
#-----------------------------------------------------------------------------------
#----------------------------------------- Zip all the sql files on the culster
#-----------------------------------------------------------------------------------
# Remove the reduandant dirs
# find . -type d -empty -delete
walk(names(Finished_runs)[c(6)], function(.x){

  subdirs <- pSIMSSiteMaker::remote.execute.cmd(host, paste0('find  ', .x ,' -type d ')) %>%
   discard(function(xx){
        (gsub(.x,"", xx) %>%
       strsplit("/"))[[1]] %>%
       length() <3
    })

  subdirs %>%
    walk (function(xx){
      tmp_cmd<-paste0('find ',
                      xx,
                      " -type f  -iname '*.sqlite3' | zip ", file.path(xx, gsub("/","_", gsub(.x,"", xx))),"_RTM.zip -@")

      tmp_file <- tempfile(fileext = ".sh")
      writeLines(tmp_cmd, tmp_file)
      #
      remote.copy.to(host, tmp_file, paste0(xx, "/ziper.sh"))
      remote.execute.cmd(host, paste0(". ", xx,"/ziper.sh"))
    })

})
#-----------------------------------------------------------------------------------
#---- find the zip files and bring them back---------
#-----------------------------------------------------------------------------------
walk(names(Finished_runs)[c(6)], function(.xx){
  # 

  paths_withruns_zip <- remote.execute.cmd(host,
                                           paste0('find ',.xx,' -type f -name "*_RTM.zip"'))
  dir.create(file.path(getwd(),outdir, basename(.xx)))
  
  #Bring them back
  walk(paths_withruns_zip, function(ss){

    remote.copy.from(host=host,
                     src=ss,
                     dst=file.path(getwd(), outdir, basename(.xx)),
                     delete = TRUE)
  })
  

  list.files(paste0(getwd(), "/Outputs/", basename(.xx)),".zip", full.names = TRUE, recursive = TRUE)%>%
    walk(~ unzip(.x, junkpaths=TRUE,
                 exdir = file.path(getwd(),"/Outputs", basename(.xx), gsub("_RTM","", tools::file_path_sans_ext(basename(.x)))%>%substr(2, 20))
                                   )
                 )
 
  paths_withruns_json<- remote.execute.cmd(host,
                                           paste0('find ',.xx,' -type f -name "experiment.json"'))
  
  #Bring them back
  walk(paths_withruns_json, function(ss){
    
    remote.copy.from(host=host,
                     src=ss,
                     dst=file.path(getwd(),outdir, basename(.xx), gsub("/","_",dirname(gsub(.xx,"",ss))%>%substr(2, 20))),
                     delete = TRUE)
  })
})


#-----------------------------------------------------------------------------------
#------------------------------------------ UNZIP all the outputs
#-----------------------------------------------------------------------------------

list.files(paste0(getwd(), "/Outputs/"),".zip", full.names = TRUE, recursive = TRUE) %>%
  walk(~ unlink(.x))
#------------------------------------------------ AGGregate SQL files from ens to pixel level
#-------------------------------------- Read SQLites
list.dirs(file.path(getwd(), "Outputs"), recursive = FALSE) [3]%>%
  walk(function(path.sim){

    print(path.sim)
    
    list.dirs(path.sim, recursive = FALSE) %>%
      walk(function(final.path){


        output <- list.files(final.path,'.sqlite3', recursive = FALSE, full.names = TRUE) %>%
          future_map_dfr(function(sqlfile){
            db <- dbConnect(sqlite.driver, dbname = sqlfile)

            mytable <- dbReadTable(db,"Outputs") %>%
              dplyr::select(lon=longitude, lat=latitude, sim, yield, NDVI, CropName, day , year) %>%
              mutate(ens = basename(sqlfile),
                     Pixel = gsub('/','_',gsub('/projects/aces/hamzed/psims/Data/sims/|illinois|lee|moultrie','',sim))
              )%>%
              dplyr::select(-sim)

          }, .progress=TRUE)

        out <- output %>%
          group_by(year, ens, Pixel) %>%
          slice_max(order_by=c(NDVI)) %>%
          mutate(lat = as.numeric(lat),
                 lon = as.numeric(lon))
        

        saveRDS(out,file.path(final.path, paste0(basename(final.path),'_output.RDS')))
        gc()
      })

  })


#----------------------------------------------------------------------------------
#----------------------------------- Making the final grid ---------------------------------
#----------------------------------------------------------------------------------
list.dirs("Outputs", recursive = FALSE)[2:3] %>%
  map(function(ss){
  print(ss)

    # Reading results
    tmpall <- list.files(ss, "output.RDS", recursive = TRUE, full.names = TRUE) %>%
      future_map_dfr(~ readRDS(.x))

    outcounty <- tmpall %>%
      mutate(lat=as.numeric(lat), 
             lon= as.numeric(lon)) %>%
      dplyr::select(Year=year, Ens=ens, Pixel, NDVI, NDVIday=day, lat, lon) %>%
      mutate(Pixel=map_chr(Pixel, ~ gsub(paste0("_",strsplit(basename(ss),"_")[[1]][2]),"", .x)) %>%
               substr(3,20)
             ) %>%
      mutate(Ens = ifelse(Ens=="Generic.sqlite3","Generic0.sqlite3", Ens)) %>%
      mutate(Ens = gsub("Generic|.sqlite3","", Ens) %>% as.numeric() + 1)


    #---------- Reading param
    allexp <- list.files(ss,"experiment.json", recursive = TRUE, full.names = TRUE)
    one.exp <- jsonlite::fromJSON(allexp[1])
    
    
    #Same across all years
    crop.param <- one.exp$experiments$planting[,c("tt_emerg_to_endjuv","leaf_init_rate","leaf_app_rate1")] %>%
      mutate(Ens=1:n())
    
    # All pixels share the same properties for plants for each ensemble
    # Now we have to make maize planting dates for each year and add that to this above dataframe
    
    maizedates <- allexp %>%
      future_map_dfr(function(ss){
     
        tmp_exp <- jsonlite::fromJSON(ss)$experiments$management$events
        
        tmp_exp_df <- seq_len(length(tmp_exp)) %>%
          purrr::map_df(~ tmp_exp[[.x]] %>% mutate(Ens=.x)) %>%
          filter(crid=='maize', event=='planting') %>%
          mutate(Year=lubridate::year(anytime::anydate(date)), 
                 PDOY=lubridate::yday(anytime::anydate(date))) %>%
          dplyr::select(Year, PDOY, date, Ens) %>%
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
#----------------------------------------------------------------------------------
#-------------------------------- Preparing Obs-------------------------------- 
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
list.dirs("Outputs", recursive = FALSE)[1] %>%
  map(function(ss){
    print(ss)
    
    county <- strsplit(basename(ss), "_")[[1]][2]
    FullGrid<- readRDS(file.path(ss, "FinalGrid.RDS"))
    
    grid <- FullGrid%>%
      group_by(Pixel) %>%
      slice_head(n=1) %>%
      dplyr::select(lat, lon, Pixel)

    ### Reading the observed data
    observed <- brick(file.path('NDVI', paste0(stringr::str_to_title(county),".nc"))
                      )
    
    ### Extract observed values from the points of simulation
    obs_raster <- rasterFromXYZ(cbind(grid[,c('lon','lat')],
                                      raster::extract(observed, grid[,c('lon','lat')])),
                                crs = '+proj=longlat +datum=WGS84 +no_defs')%>%
      rasterToPoints()%>% 
      as.data.frame()%>%
      tidyr::pivot_longer(!c('x','y'))%>%
      rename(lon=x,lat=y)%>%
      mutate(date = as.Date(as.numeric(gsub('X','',name)),origin='1970-01-01'),
             year = as.factor(format(date,"%Y")))%>%
      dplyr::select(-name)%>%
      rename(NDVIObs=value)
    
    ### Rounding off is important to have an exact match of lat & lon, and joining the two data frames together later
    grid$lon <- round(grid$lon,5)
    grid$lat <- round(grid$lat,5)  
    
    obs_raster$lon <- round(obs_raster$lon,5)
    obs_raster$lat <- round(obs_raster$lat,5)
    
    obs_raster <- obs_raster%>%
      left_join(grid,by=c('lon','lat'))
  
    ### Fitting a double logistic model to cleanup the NDVI values for every cell and evry year
    raw_data <- obs_raster %>%
      split(list(.$Pixel,.$year))%>%
      future_map_dfr(function(x){
        library(phenex)
        ndvi <- new("NDVI", values=x$NDVIObs, year=as.integer(x$year%>%unique()))
        ndvi <- phenex::bise(ndvi, slidingperiod=10) #bias correction
        x$NewObsNDVI= phenex::modelValues(ndvi, method="DLogistic")@modelledValues #fitting the double logitic
        x
      }, .progress = TRUE)
    
    obs_NDVI <- raw_data%>%
      split(list(.$Pixel,.$year))%>%
      purrr::map_dfr(function(x){
        max_sim <- x%>%slice_max(NewObsNDVI)
      })%>%
      filter(NewObsNDVI!=0)%>%
      rename(Date=date, Year=year) %>%
      mutate(Year= as.numeric(as.character(Year)))

    
    comparison <- FullGrid%>%
      left_join(obs_NDVI%>%dplyr::select(-c(lon,lat), Pixel, Year, NewObsNDVI, Date),by=c('Pixel','Year'))%>%
      mutate(NDVIObsDay = lubridate::yday(Date))%>%
       mutate(date.diff=as.numeric(NDVIday-NDVIObsDay)) %>%
      na.omit()
    
    
     saveRDS(comparison,file.path(ss, 'Optim_Ready.RDS'))
    
  })

