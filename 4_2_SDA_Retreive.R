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
plan(multisession)
sqlite.driver <- dbDriver("SQLite")
setwd("/mnt/iccp_storage/Regional_Calibration/")

state <- "illinois"
county <- "gallatin"
sim_name <- paste0(state, "_", county,"_SDA")

host <- list(name = 'cc-login.campuscluster.illinois.edu',
             user = 'tsrai',
             tunnel = '~/tunnel/tunnel',
             from= file.path(getwd(), "Simulations", sim_name),
             to='/scratch/users/tsrai/')

outdir <- '/YieldOutputs'
Var <- 'yield'
#-----------------------------------------------------------------------------------
#-------------------------------------------- Find the runs/dirs that starts with illinois and has sqlite3 files
#-----------------------------------------------------------------------------------
paths_withruns <- pSIMSSiteMaker::remote.execute.cmd(host, 'find  /scratch/users/tsrai/illinois_gallatin_SDA/ -type d -name "illinois_*"')
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
walk(names(Finished_runs)[c(2)], function(.x){
  
  #Delete empty dirs
  pSIMSSiteMaker::remote.execute.cmd(host, paste0('find  ', .x ,' -type d -empty -delete'))

  subdirs <- pSIMSSiteMaker::remote.execute.cmd(host, paste0('find  ', .x ,' -type d ')) %>%
   discard(function(xx){
        (gsub(.x,"", xx) %>%
       strsplit("/"))[[1]] %>%
       length() <3
    })

  subdirs %>%
    future_walk (function(xx){
      tmp_cmd<-paste0('find ',
                      xx,
                      " -type f -name '*.RData' -o -name '*.json' -o -name '*.sqlite3' | zip ",
                      file.path(xx, gsub("/","_", gsub(.x,"", xx))),"_RTM.zip -@")

      tmp_file <- tempfile(fileext = ".sh")
      writeLines(tmp_cmd, tmp_file)
      #
      remote.copy.to(host, tmp_file, paste0(xx, "/ziper.sh"))
      remote.execute.cmd(host, paste0(". ", xx,"/ziper.sh"))
    }, .progress = TRUE)

})

#-----------------------------------------------------------------------------------
#---- find the zip files and bring them back---------
#-----------------------------------------------------------------------------------
walk(names(Finished_runs)[c(2)], function(.xx){
  # 

  paths_withruns_zip <- remote.execute.cmd(host,
                                           paste0('find ',.xx,' -type f -name "*.zip"'))
  
  dir.create(file.path(getwd(),outdir, basename(.xx)))

  #Bring them back
  walk(paths_withruns_zip, function(ss){

    remote.copy.from(host=host,
                     src=ss,
                     dst=file.path(getwd(), outdir, basename(.xx)),
                     delete = TRUE)
  })

#---------------
#--- UNZIP all the outputs
#----------------

  list.files(paste0(getwd(), paste0(outdir, "/"), basename(.xx)),".zip", full.names = TRUE, recursive = TRUE)%>%
    walk(~ unzip(.x, junkpaths=TRUE,
                 exdir = file.path(getwd(),outdir, basename(.xx),
                                   gsub("_RTM","", tools::file_path_sans_ext(basename(.x)))%>%substr(2, 20))
                                   )
                 )

  
  #Bring experiment .json back
  walk(dirname(paths_withruns_zip), function(ss){

    remote.copy.from(host=host,
                     src=file.path(ss, "experiment.json"),
                     dst=file.path(getwd(),outdir, basename(.xx), gsub("/","_",gsub(paste0(.xx),"",ss)) %>%
                                    # gsub("_illinois_crawford_", "", .) %>%
                                     substr(2, 20),
                                   "experiment.json"),
                     delete = TRUE)
  })
})




list.files(paste0(getwd(), paste0(outdir, "/")),".zip", full.names = TRUE, recursive = TRUE) %>%
  walk(~ unlink(.x))

# Remove empty files
file.remove(
  list.files(paste0(getwd(),'/YieldOutputs/',state,'_',county,'_SDA'),'.sqlite3', recursive = T, full.names = TRUE)[
    file.size(list.files(paste0(getwd(),'/YieldOutputs/',state,'_',county,'_SDA'),'.sqlite3', recursive = T, full.names = TRUE))==0
  ]
)
