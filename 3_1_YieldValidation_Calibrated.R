# This script creates campaign for calibration.
# Spatial boundaries of the campaign are determined by county limits
# Same crop is grown at each pixel for all the years

library(pSIMSSiteMaker)
library(pSIMCampaignManager)
setwd("/mnt/iccp_storage/Regional_Calibration/")

state <- "illinois"
county <- "lee"
sim_name <- paste0(state, "_", county)
sim_years <- 2010:2020


pSIMS_extent<-read.csv(system.file("Utils", "pSIMS_extents.csv", package = "pSIMSSiteMaker"))
fname <-  file.path(getwd(), "Simulations", sim_name, "Campaign.nc4")

# If it exists delete it
if(dir.exists(file.path("Simulations", sim_name))) unlink(file.path("Simulations", sim_name) , recursive = TRUE)

dir.create(file.path("Simulations", sim_name))

Sys.chmod(file.path("Simulations", sim_name), mode = "0777", use_umask = TRUE)
#--------------------------------------------------------------------------------------------------
#------------------------------- Start creating the Campaign 
#-------------------------------------------------------------------------------------------------
#--- ------------Add calibrated parameters to the campaign
Optim_res <- readRDS(file.path(getwd(),"Outputs", paste0("illinois_",county, "/Optim_results.RDS")))
# get the objects we need from the optimization results
comparison <- Optim_res[[1]]
params <-  Optim_res[[5]]


Allparams.df <- params %>%
  mutate(tt_emerg_to_endjuv = as.numeric(tt_emerg_to_endjuv),
         leaf_app_rate1 = as.numeric(leaf_app_rate1),
         leaf_init_rate = as.numeric(leaf_init_rate), 
         pdate= as.numeric(pdate)) %>%
  tidyr::gather(Year, Yeareff, -pixel, -pdate, -tt_emerg_to_endjuv, -leaf_app_rate1, -leaf_init_rate) %>%
  split(.$Year) %>%
  map_dfr(function(one.year.param){
    
    comparison %>%
      ungroup() %>%
      select(Pixel, lat, lon) %>%
      distinct(Pixel, .keep_all = TRUE) %>%
      left_join(one.year.param %>% 
                  rename(Pixel=pixel), by='Pixel') %>%
      mutate(pdate = pdate + as.numeric(Yeareff))
    
  })

crop.params <- Allparams.df %>%
  filter(Year == Allparams.df$Year[1])
#------------------------------------------
Create_Empty_Campaign(lat=unique(crop.params$lat),
                      lon=unique(crop.params$lon),
                      num_scen=1,
                      filename =fname)

Add_Scenario(fname, 59)
prop <- Inspect_Camp(fname)
num_scen <- Get_Camp_dim(fname)$Scen
count <- length(prop$Lat)*length(prop$Lon)
Inspect_Camp(fname)

for(param in c("tt_emerg_to_endjuv","leaf_app_rate1","leaf_init_rate")) {
  print(param)
  
  mat.par <-  rasterFromXYZ(crop.params[,c('lon','lat',param)]) %>% as.matrix()

  new.values <-  seq_along(prop$Scen) %>% purrr::map(~mat.par %>% t )

  AddVar_Campaign(fname,
                  Variable = list(Name=param,
                                  Unit="C/day",
                                  missingValue=-99,
                                  value= new.values,
                                  longname="",
                                  prec="float"
                  ),
                  attr = list('long_name',"")
  )
}

#----------------Adding met
new.values <-seq_along(prop$Scen) %>%
  purrr::map(~matrix(sample(c(1:9), prop$Count,TRUE), nrow = length(prop$Lat), ncol = length(prop$Lon)))

AddVar_Campaign(fname,
                Variable = list(Name='file',
                                Unit='Mapping',
                                missingValue=-99,
                                value= new.values,
                                longname="",
                                prec="float"
                ),
                attr = list('long_name',"met00000.met,met00001.met,met00002.met,met00003.met,
                            met00004.met,met00005.met,met00006.met,met00007.met,met00008.met,
                            met00009.met"))

Edit_mapping_var (fname, 'file' , 'long_name', "met00000.met,met00001.met,met00002.met,met00003.met,met00004.met,met00005.met,met00006.met,met00007.met,met00008.met,met00009.met")


###### Fertilizer Amount
num_years <- length(sim_years)



for (j in 1:num_years) {
  
  new.values <-seq_along(prop$Scen) %>%
    purrr::map(~matrix(runif(prop$Count,180,220), nrow = length(prop$Lat), ncol = length(prop$Lon)))
  
  AddVar_Campaign(fname,
                  Variable = list(Name=paste0('feamn_',j),
                                  Unit='kg/ha',
                                  missingValue=-99,
                                  value= new.values,
                                  longname="",
                                  prec="float"
                  ),
                  attr = list('long_name',"")
  )
  
  print(j)
}



############################################################################################
###### Planting Date and fertilizer date




for (j in 1:num_years) {
  
  pdate.param <- Allparams.df %>%
    filter(Year == sim_years[j])
  
  mat.par <-  rasterFromXYZ(pdate.param[,c('lon','lat','pdate')]) %>% as.matrix() %>% round()
  
  inds <- matrix(1:(nrow(mat.par)* ncol(mat.par)), 
                 ncol= ncol(mat.par), nrow=  nrow(mat.par), byrow = FALSE)
  

  new.values <-seq_along(prop$Scen) %>% purrr::map(~inds %>% t )
  
  AddVar_Campaign(fname,Variable = list(Name=paste0('date_',4*j-1),
                                        Unit='Mapping',
                                        missingValue=-99,
                                        prec='float',
                                        longname="",
                                        value= new.values))
  
  Edit_mapping_var(fname, paste0('date_',4*j-1), 'long_name', paste(gsub('-','',
                                                                         as.Date(mat.par %>% as.numeric(),origin = paste0(sim_years[j]-1,'-12-31'))),
                                                                    collapse = ','))
  
  AddVar_Campaign(fname,Variable = list(Name=paste0('date_',4*j-2),
                                        Unit='Mapping',
                                        missingValue=-99,
                                        prec='float',
                                        longname="",
                                        value= new.values))
  Edit_mapping_var(fname, paste0('date_',4*j-2), 'long_name', paste(gsub('-','',
                                                                         as.Date(mat.par %>% as.numeric(),origin = paste0(sim_years[j]-1,'-12-31'))),
                                                                    collapse = ','))
  
  print(j)
  
}


############################################################################################
######## Water Fraction

new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix(fname,
                                                                    runif(Get_Camp_dim(fname)$Count, 0.05,0.95)
)[[1]])

AddVar_Campaign(fname,
                Variable = list(Name='water_fraction_full',
                                Unit='mm/mm',
                                missingValue=-99,
                                value= new.values,
                                longname="",
                                prec="float"
                ),
                attr = list('long_name',"")
)



############################################################################################
######## Residue Weight

# new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix(fname,
#                                                                     runif(Get_Camp_dim(fname)$Count, 2000,2500)
# )[[1]])
# 
# AddVar_Campaign(fname,
#                 Variable = list(Name='icrag',
#                                 Unit='Kg/ha',
#                                 missingValue=-99,
#                                 value= new.values
#                 ),
#                 attr = list('long_name',"")
# )
# 
# GetCamp_VarMatrix(fname,'icrag')
# 
# 
# ############################################################################################
# ######## Residue type
# 
# new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix(fname,
#                                                                     sample(c(1,2),Get_Camp_dim(fname)$Count, T)
# )[[1]])
# 
# AddVar_Campaign(fname,
#                 Variable = list(Name='residue_type',
#                                 Unit='Mapping',
#                                 missingValue=-99,
#                                 value= new.values
#                 ),
#                 attr = list('long_name',"Maize,Soybean")
# )
# 
# Edit_mapping_var (fname, 'residue_type' , 'long_name', "Maize,Soybean")
# 
# plot(GetCamp_VarMatrix(fname,'residue_type')$Raster[[1]])


#########################Creating a mask for the county
county_boundry <- sf::st_as_sf( maps::map('county', state, fill=TRUE, plot =FALSE)) %>%
  filter(grepl(county, ID)) %>%
  dplyr::select(-ID) %>%
  as(., "Spatial")

il_mask <- raster(file.path(getwd(), "Templates", 'crop_mask_IL.nc'))
county_mask2 <- mask(il_mask,county_boundry)

writeRaster(
  county_mask2,
  file.path("Simulations", sim_name, 'mask.nc'),
  overwrite = T,
  format = 'CDF',
  varname = 'mask',
  varunit = 'boolean',
  longname = 'cropland',
  xname = 'lon',
  yname = 'lat'
)

tile_number <- pSIMS_extent %>%
  filter(ymin <= extent( trim(county_mask2))[3], ymax>= extent( trim(county_mask2))[4], 
         xmin <= extent( trim(county_mask2))[1], xmax>= extent( trim(county_mask2))[2]
  ) %>%
  pull(name)
######################################################################################################

remove_var_campaign(fname, varnames=c('myvar'))

######################################################################################################
#------------------------------- Creating the simulation
######################################################################################################
#---- Param File
tmp_param <- Read_param_template(file.path(getwd(), "Templates", "params.apsim.sample"))
tmp_param$ref_year <- min(sim_years)%>% as.integer()
tmp_param$num_years <- length(sim_years)%>% as.integer()
tmp_param$scen_years <- length(sim_years)%>% as.integer()
tmp_param$scens <- 60L
#tmp_param$tappinp$cultivarfile <- c(file.path(getwd(), "Templates", "Maize_template.xml"))
tmp_param$delta <- "2.5,2.5"
tmp_param$soils <- '/pysims/data/soils/Soils'
tmp_param$weather <- "/pysims/data/clim/NewMet/"
tmp_param$checker$simgfile <- "../../mask.nc"
tmp_param$Pre_run_command <- "Rscript ../../SoilFixer.R"
tmp_param$Post_run_command <- "Rscript ../../Replace_sql_files.R"

#Modifying the campaign json file 
tmp_camp <- Read_Campaign_template(file.path(getwd(), "Templates", "exp_template.json"))  # This is different from one that was used in the rotation exp.
tmp_camp$reporting_frequency <- "daily"

# Point 1
tmp_camp$crop_name <- c("maize") 

# Point2 
tmp_camp$fertilizer$crop <- "Maize"
tmp_camp$fertilizer$type <- "NH4NO3"
tmp_camp$planting$crop <- "Maize"
tmp_camp$initial_condition$residue_type <- "Maize"
tmp_camp$reset$date <- "01-jan"

#-----Creating the operations for each crop separately
tmp_camp$management$events  <- sim_years %>%
  map(function(syear) {
    Mangment_campaign_json_maker(
      PlantingDate = as.Date(paste0(syear, "-05-03")),
      HarvestDate = as.Date(paste0(syear, "-10-16")),
      Crop = "maize",
      Cultivar = "?",
      # psims will ensemblize the cultivar
      Population = "8",
      Depth = "40",
      RowSpacing = "762",
      fert_date = as.Date(paste0(syear,"-05-03")),
      fertamnt = "200",
      fertdepth = "40"
    ) 
  }) %>%
  flatten()


host <-
  list(name = 'cc-login.campuscluster.illinois.edu',
       user = 'hamzed',
       tunnel = '~/tunnel/tunnel',
       from= file.path(getwd(), "Simulations", sim_name),
       to='/projects/aces/hamzed/psims/Data/sims')

#debugonce(pSIMS_Site_Make)

pSIMS_Site_Make(
  dirname = file.path(getwd(), "Simulations", sim_name),
  Project_name = sim_name,
  Lat = 41.7125,
  Lon = -89.204167,
  Tile = tile_number,
  #Auxiliary_files = c(), # This would put files in the campign dir, as results all will be copied to all runs
  Campaign_Path = c(file.path(getwd(), "Simulations",sim_name, 'Campaign.nc4'),
                    file.path(getwd(), "Templates", 'EnKF.R')),
  APSIM_Template_Path = file.path(getwd(), "Templates", 'template.apsim'),
  Param_template_Obj = tmp_param,
  Campaign_json_Obj = tmp_camp,
  APSIM_Cultivar_Path = c(file.path(getwd(), "Templates", 'Maize_template.xml'),
                          system.file("templates", "Soybean_template.xml", package = "pSIMSSiteMaker"),
                          file.path("Simulations", sim_name, 'mask.nc'),
                          file.path(getwd(), "Templates", 'SoilFixer.R'),
                          file.path(getwd(), "Templates", 'Replace_sql_files.R')),
  # Point 3
  host = host,
  Bash_control = list(
    pSIMS_Data_Path = file.path("/pysims/data/sims/", sim_name),
    # No need to edit this
    pSIMS_server_Path = "/projects/aces/hamzed/psims/Data",
    pSIMS_Sing_Image = "/projects/aces/hamzed/psims/Data/SingularityImg/pSIMSFull.simg"
  )
)


######################################################################################################
# # Transfering the files to lab server
# project_name <- "DA_maize_cal"
# remote.copy.from(host=host,
#                  src=paste0('/scratch/users/tsrai/',project_name),
#                  dst=file.path("/mnt/iccp_storage/trai/DA_Results/Lee"),
#                  delete = TRUE)
# 
# ######################################################################################################
# 
# # Delete the empty directories
# setwd("/mnt/iccp_storage/trai/")
# 
# library(TAF)
# rmdir('/mnt/iccp_storage/trai/DA_Results/Lee/DA_maize_cal',recursive=T)
# 
