# This script creates campaign for calibration.
# Spatial boundaries of the campaign are determined by county limits
# Same crop is grown at each pixel for all the years

library(pSIMSSiteMaker)
library(pSIMCampaignManager)
setwd("/mnt/iccp_storage/Regional_Calibration/")

state <- "illinois"
county <- "gallatin"
sim_name <- paste0(state, "_", county)
sim_years <- 2010:2020

Rot_Rasters <- readRDS(file.path(getwd(),"Rotations", paste0(sim_name, ".RDS")))

fname <-  file.path(getwd(), "Simulations", sim_name, "Campaign.nc4")

# If it exists delete it
if(dir.exists(file.path("Simulations", sim_name))) unlink(file.path("Simulations", sim_name) , recursive = TRUE)
  
dir.create(file.path("Simulations", sim_name))

Sys.chmod(file.path("Simulations", sim_name), mode = "0777", use_umask = TRUE)


Create_Empty_Campaign(lat=seq(37, 43.5, by=0.25),
                      lon=seq(-86, -92.75, by=-0.25),
                      num_scen=1,
                      filename =fname)

params <- list (
  pars = c("tt_emerg_to_endjuv","leaf_app_rate1","leaf_init_rate"),
  Upper.limit = c(500,65,30),
  lower.limit = c(200,40,15),
  unit=c('C/day','C/day','C/day')
)

# lenght.out.grid <- 5 # the space between points in each dimension in the parameter space
# #----------------------------------- FULL grid
# grid <- purrr::pmap(params[c(2,3)], function(Upper.limit, lower.limit) {
#   seq(Upper.limit, lower.limit, length.out = lenght.out.grid)
# }) %>%
#   setNames(params$pars) %>%
#   expand.grid()
#----------------------------------------- LHC
n.knot <- length(params$pars)*33

grid.lhc <- pmap_dfc(params[c(1,2,3)], function(pars, Upper.limit, lower.limit, probs){
  
  probs <- gen_latin_nD(t(matrix(0:1, ncol = 1, nrow = 2)), n.knot)
  
  eval(parse(text = paste0("qunif", "(p,", lower.limit, ",", Upper.limit, ")")), list(p = probs)) %>%
    as.data.frame()
}) %>%
  `colnames<-`(params$pars)

grid <- grid.lhc

# names(grid)[-8] %>%
#   combn(2) %>%
#   as.data.frame()%>%
#   map(function(mm){
#     #browser()
#     plot(grid[,mm[1]], grid[,mm[2]])
#   })
#--------------------------------------------------- 
saveRDS(grid, file=file.path("Simulations", sim_name,"grid_emulator_DA_leafparams.RDS"))
#------------------------------------------

Add_Scenario(fname, nrow(grid)-1)
prop <- Inspect_Camp(fname)
num_scen <- Get_Camp_dim(fname)$Scen
count <- length(prop$Lat)*length(prop$Lon)
Inspect_Camp(fname)

for(param in params$pars) {
  print(param)
  new.values <-  seq_along(prop$Scen) %>%
    purrr::map(~matrix(  grid[[param]][.x], nrow = length(prop$Lat), ncol = length(prop$Lon)))
  
  #debugonce(AddVar_Campaign)
  AddVar_Campaign(fname,
                  Variable = list(Name=param,
                                  Unit=params$unit[which(params$pars==param)],
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

GetCamp_VarMatrix(fname,'file')

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


plot(GetCamp_VarMatrix(fname,'feamn_2')$Raster[[5]])

############################################################################################
###### Planting Date and fertilizer date



for (j in 1:num_years) {
  
  new.values <-seq_along(prop$Scen) %>%
    purrr::map(~matrix(sample(1:50,1,T), nrow = length(prop$Lat), ncol = length(prop$Lon)))
  
  
  AddVar_Campaign(fname,Variable = list(Name=paste0('date_',4*j-1),
                                                   Unit='Mapping',
                                                   missingValue=-99,
                                                   prec='float',
                                                    longname="",
                                                   value= new.values))
  
  Edit_mapping_var(fname, paste0('date_',4*j-1), 'long_name', paste(gsub('-','',
                                                                                    as.Date(1:50,origin = paste0(j+min(sim_years)-1,'-04-29'))),
                                                                               collapse = ','))
  
  AddVar_Campaign(fname,Variable = list(Name=paste0('date_',4*j-2),
                                                   Unit='Mapping',
                                                   missingValue=-99,
                                                   prec='float',
                                                   longname="",
                                                   value= new.values))
  Edit_mapping_var(fname, paste0('date_',4*j-2), 'long_name', paste(gsub('-','',
                                                                                    as.Date(1:50,origin = paste0(j+min(sim_years)-1,'-04-29'))),
                                                                               collapse = ','))
  
  print(j)
  
}

plot(GetCamp_VarMatrix(fname,'date_2')$Raster[[5]])
GetCamp_VarMatrix(fname,'date_2')[[1]][,,8]


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

GetCamp_VarMatrix(fname,'water_fraction_full')

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
  Tile = "0027/0046",
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
