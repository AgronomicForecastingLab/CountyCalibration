library(pSIMSSiteMaker)
library(pSIMCampaignManager)
library(dplyr)
library(RSQLite)
library(purrr)
library(furrr)
library(jsonlite)
library(phenex)
library(rnassqs)
library(ggplot2)
library(rlang)
library(mvtnorm)
library(stringr)
plan(multisession)

setwd("/mnt/iccp_storage/Regional_Calibration")
sqlite.driver <- dbDriver("SQLite")

state <- "illinois"
county <- "gallatin"
outdir <- '/YieldOutputs'
sim_name <- paste0(state, "_", county,"_SDA")
sim_path <- paste0(getwd(),outdir,'/',sim_name)
sim_path
#getwd()
#setwd(paste0(sim_path))

#delete empty directories or ones with no sql files
a <- sub('DA.sqlite3','',list.files(sim_path,'.sqlite3',recursive = T, full.names = T))
b <- sub('experiment.json','',list.files(sim_path,'experiment.json',recursive = T, full.names = T))
b[!b %in% a]
unlink(b[!b %in% a],recursive = T)


#-----------------------------------------------------------------------------------

#------------------------------------------------ Weighing ensembles SQL files from ens to pixel level
Ens_Weighting <- function(SDA.out){
  
  SDA <- SDA.out %>% discard(~ length(.x)==1)
  
  Allw <- data.frame(Date = SDA %>% map_chr(~ .x$Obs$Date %>% as.character) %>% as.Date()) %>%
    mutate(id=1:n()) %>%
    mutate(Year=lubridate::year(Date)) %>%
    split(.$Year) %>%
    future_map_dfr(function(one.year){
      
      SDA.year <- SDA[one.year$id]
      # Estimate the weights for each year
      SDA.year %>%
        map_dfr(function(ss){
          flux.weights <- dmvnorm(ss$Forecast$X,
                                  mean =ss$mu.a,
                                  sigma = ss$Pa,
                                  log = FALSE)
          
          
          data.frame(Weights= scales::rescale(flux.weights), 
                     Date=ss$Obs$Date) %>%
            mutate(Ens= 1:n())
        })
    }, .progress = TRUE)
  
  Annual.W <- Allw %>%
    mutate(Year = lubridate::year(Date)) %>%
    group_by(Ens, Year) %>%
    summarise(
      Weight= sum(Weights)
    )
  
  Annual.W %>%
    split(.$Year) %>%
    map_dfr(function(yr){
      yr$ScaledW <- yr$Weight/sum(yr$Weight)
      yr %>%
        dplyr::select(-Weight)
    })
}


for (i in 1:length(list.files(sim_path,'.sqlite3',recursive = T))) {
  loc <- sub('.{11}$','',list.files(sim_path, ".sqlite3", recursive = TRUE, full.names = TRUE)[i])
  setwd(loc)
  load('sda.out.RData')
  ens_weights <- Ens_Weighting(SDA.out)%>%
    mutate(Pixel = str_sub(getwd(),-9,-1))
  saveRDS(ens_weights,'ens_weights.RDS')
  print(i)
#  setwd(paste0("/mnt/iccp_storage/Regional_Calibration/"))
}
#------------------------------------------------ AGGregate SQL files from ens to pixel level
#-------------------------------------- Read and summarize SQLites



list.files(sim_path, ".sqlite3", recursive = TRUE, full.names = TRUE)%>%
  map(function(sqlfile){
    print(sqlfile)
    db <- dbConnect(sqlite.driver, dbname = sqlfile)
    
    mytable <- dbReadTable(db, "Outputs") %>%
      dplyr::select(longitude, latitude, yield, NDVI, CropName, ensemble, day , year) %>%
      #filter(CropName=='maize')%>%
      mutate(year = as.factor(year),
             Pixel = gsub(paste0(sim_path,'/'),'',gsub('/DA.sqlite3','',sqlfile))
             )%>%
      mutate(ensemble = ifelse(ensemble=='paddock','paddock0',ensemble))%>%
      mutate(ensemble = as.numeric(gsub('paddock','',ensemble))+1)
    
    out <- aggregate(cbind(yield,NDVI)~longitude+latitude+year+Pixel+ensemble+CropName, data=mytable,max)
    
    saveRDS(out,paste0(gsub('DA.sqlite3','',sqlfile),out$Pixel[1],'_output.RDS'))
    
  })

