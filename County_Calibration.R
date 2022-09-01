# This script creates campaign for calibration.
# Spatial boundaries of the campaign are determined by county limits
# Same crop is grown at each pixel for all the years

library(pSIMSSiteMaker)
library(pSIMCampaignManager)

setwd("~/DA")

Rot_Rasters <- readRDS('Created_rasters/Rot_Rasters.RDS')

fname <-  "MyCampaign.nc4"
Create_Empty_Campaign(lat=unique(raster::xyFromCell(Rot_Rasters[[1]], 1:length(Rot_Rasters[[1]]))[,2]),
                      lon=unique(raster::xyFromCell(Rot_Rasters[[1]], 1:length(Rot_Rasters[[1]]))[,1]),
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

names(grid)[-8] %>%
  combn(2) %>%
  as.data.frame()%>%
  map(function(mm){
    #browser()
    plot(grid[,mm[1]], grid[,mm[2]])
  })
#--------------------------------------------------- 
saveRDS(grid, file="grid_emulator_DA_leafparams.RDS")
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
                                  value= new.values
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
                                value= new.values
                ),
                attr = list('long_name',"met00000.met,met00001.met,met00002.met,met00003.met,
                            met00004.met,met00005.met,met00006.met,met00007.met,met00008.met,
                            met00009.met"))

Edit_mapping_var (fname, 'file' , 'long_name', "met00000.met,met00001.met,met00002.met,met00003.met,met00004.met,met00005.met,met00006.met,met00007.met,met00008.met,met00009.met")


###### Fertilizer Amount
num_years <- 11

GetCamp_VarMatrix("MyCampaign.nc4",'file')

for (j in 1:num_years) {
  
  new.values <-seq_along(prop$Scen) %>%
    purrr::map(~matrix(runif(prop$Count,180,220), nrow = length(prop$Lat), ncol = length(prop$Lon)))
  
  AddVar_Campaign("MyCampaign.nc4",
                  Variable = list(Name=paste0('feamn_',j),
                                  Unit='kg/ha',
                                  missingValue=-99,
                                  value= new.values
                  ),
                  attr = list('long_name',"")
  )
  
  print(j)
}

GetCamp_VarMatrix('MyCampaign.nc4','feamn_5')[[1]][,,4]
plot(GetCamp_VarMatrix('MyCampaign.nc4','feamn_1')$Raster[[5]])

############################################################################################
###### Planting Date and fertilizer date



for (j in 1:num_years) {
  
  new.values <-seq_along(prop$Scen) %>%
    purrr::map(~matrix(sample(1:50,1,T), nrow = length(prop$Lat), ncol = length(prop$Lon)))
  
  
  AddVar_Campaign("MyCampaign.nc4",Variable = list(Name=paste0('date_',4*j-1),
                                                   Unit='Mapping',
                                                   missingValue=-99,
                                                   prec='float',
                                                   value= new.values))
  Edit_mapping_var('MyCampaign.nc4', paste0('date_',4*j-1), 'long_name', paste(gsub('-','',
                                                                                    as.Date(1:50,origin = paste0(j+2009,'-04-29'))),
                                                                               collapse = ','))
  
  AddVar_Campaign("MyCampaign.nc4",Variable = list(Name=paste0('date_',4*j-2),
                                                   Unit='Mapping',
                                                   missingValue=-99,
                                                   prec='float',
                                                   value= new.values))
  Edit_mapping_var('MyCampaign.nc4', paste0('date_',4*j-2), 'long_name', paste(gsub('-','',
                                                                                    as.Date(1:50,origin = paste0(j+2009,'-04-29'))),
                                                                               collapse = ','))
  
  print(j)
  
}

plot(GetCamp_VarMatrix('MyCampaign.nc4','date_2')$Raster[[5]])
GetCamp_VarMatrix('MyCampaign.nc4','date_2')[[1]][,,8]


############################################################################################
######## Water Fraction

new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix("MyCampaign.nc4",
                                                                    runif(Get_Camp_dim("MyCampaign.nc4")$Count, 0.05,0.95)
)[[1]])

AddVar_Campaign("MyCampaign.nc4",
                Variable = list(Name='water_fraction_full',
                                Unit='mm/mm',
                                missingValue=-99,
                                value= new.values
                ),
                attr = list('long_name',"")
)

GetCamp_VarMatrix('MyCampaign.nc4','water_fraction_full')

############################################################################################
######## Residue Weight

new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix("MyCampaign.nc4",
                                                                    runif(Get_Camp_dim("MyCampaign.nc4")$Count, 2000,2500)
)[[1]])

AddVar_Campaign("MyCampaign.nc4",
                Variable = list(Name='icrag',
                                Unit='Kg/ha',
                                missingValue=-99,
                                value= new.values
                ),
                attr = list('long_name',"")
)

GetCamp_VarMatrix('MyCampaign.nc4','icrag')


############################################################################################
######## Residue type

new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix("MyCampaign.nc4",
                                                                    sample(c(1,2),Get_Camp_dim("MyCampaign.nc4")$Count, T)
)[[1]])

AddVar_Campaign("MyCampaign.nc4",
                Variable = list(Name='residue_type',
                                Unit='Mapping',
                                missingValue=-99,
                                value= new.values
                ),
                attr = list('long_name',"Maize,Soybean")
)

Edit_mapping_var ("MyCampaign.nc4", 'residue_type' , 'long_name', "Maize,Soybean")

plot(GetCamp_VarMatrix('MyCampaign.nc4','residue_type')$Raster[[1]])


######################################################################################################################




######################################################################################################

remove_var_campaign("MyCampaign.nc4", outfile="Campaign.nc4", varnames=c('myvar'))

######################################################################################################
#------------------------------- Creating the simulation
######################################################################################################

host <-
  list(name = 'cc-login.campuscluster.illinois.edu',
       user = 'tsrai',
       tunnel = '~/tunnel/tunnel',
       from='/home/trai/pSIMS/DA',
       to='/projects/aces/tsrai/psims/Data/sims')



tmp_param <- Read_param_template('~/DA/params.apsim.sample')
tmp_param$ref_year <- 2010L
tmp_param$num_years <- 11L
tmp_param$scen_years <- 11L
tmp_param$scens <- 60L
tmp_param$tappinp$cultivarfile <- c("Maize_template.xml")
tmp_param$delta <- "2.5,2.5"
tmp_param$soils <- '/pysims/data/soils/Soils'
tmp_param$weather <- "/pysims/data/clim/NewMet/"

#Modifying the campaign json file 
tmp_camp <- Read_Campaign_template('~/DA/exp_template.json')  # This is different from one that was used in the rotation exp.
tmp_camp$reporting_frequency <- "daily"

# Point 1
tmp_camp$crop_name <- c("maize", "soybean")

# Point2 
tmp_camp$fertilizer$crop <- "Maize"
tmp_camp$fertilizer$type <- "NH4NO3"
tmp_camp$planting$crop <- "Maize"
tmp_camp$initial_condition$residue_type <- "Maize"
tmp_camp$reset$date <- "01-jan"


#-----Creating the operations for each crop separately
crop1 <- Mangment_campaign_json_maker(
  PlantingDate=as.Date("2010-05-03"),
  HarvestDate=as.Date("2010-10-16"),
  Crop = "maize",
  Cultivar = "?", # psims will ensemblize the cultivar
  Population="8",
  Depth="40",
  RowSpacing="762",
  fert_date = as.Date("2010-05-03"),
  fertamnt= "200",
  fertdepth= "40"
)

#-----Creating the operations for each crop separately
crop2 <- Mangment_campaign_json_maker(
  PlantingDate=as.Date("2011-05-03"),
  HarvestDate=as.Date("2011-10-16"),
  Crop = "maize",
  Cultivar = "?", # psims will ensemblize the cultivar
  Population="8",
  Depth="40",
  RowSpacing="762",
  fert_date = as.Date("2011-05-03"),
  fertamnt= "200",
  fertdepth= "40"
)

crop3 <- Mangment_campaign_json_maker(
  PlantingDate=as.Date("2012-05-03"),
  HarvestDate=as.Date("2012-10-16"),
  Crop = "maize",
  Cultivar = "?", # psims will ensemblize the cultivar
  Population="8",
  Depth="40",
  RowSpacing="762",
  fert_date = as.Date("2012-05-03"),
  fertamnt= "200",
  fertdepth= "40"
)

crop4 <- Mangment_campaign_json_maker(
  PlantingDate=as.Date("2013-05-03"),
  HarvestDate=as.Date("2013-10-16"),
  Crop = "maize",
  Cultivar = "?", # psims will ensemblize the cultivar
  Population="8",
  Depth="40",
  RowSpacing="762",
  fert_date = as.Date("2013-05-03"),
  fertamnt= "200",
  fertdepth= "40"
)

crop5 <- Mangment_campaign_json_maker(
  PlantingDate=as.Date("2014-05-03"),
  HarvestDate=as.Date("2014-10-16"),
  Crop = "maize",
  Cultivar = "?", # psims will ensemblize the cultivar
  Population="8",
  Depth="40",
  RowSpacing="762",
  fert_date = as.Date("2014-05-03"),
  fertamnt= "200",
  fertdepth= "40"
)

crop6 <- Mangment_campaign_json_maker(
  PlantingDate=as.Date("2015-05-03"),
  HarvestDate=as.Date("2015-10-16"),
  Crop = "maize",
  Cultivar = "?", # psims will ensemblize the cultivar
  Population="8",
  Depth="40",
  RowSpacing="762",
  fert_date = as.Date("2015-05-03"),
  fertamnt= "200",
  fertdepth= "40"
)

crop7 <- Mangment_campaign_json_maker(
  PlantingDate=as.Date("2016-05-03"),
  HarvestDate=as.Date("2016-10-16"),
  Crop = "maize",
  Cultivar = "?", # psims will ensemblize the cultivar
  Population="8",
  Depth="40",
  RowSpacing="762",
  fert_date = as.Date("2016-05-03"),
  fertamnt= "200",
  fertdepth= "40"
)

crop8 <- Mangment_campaign_json_maker(
  PlantingDate=as.Date("2017-05-03"),
  HarvestDate=as.Date("2017-10-16"),
  Crop = "maize",
  Cultivar = "?", # psims will ensemblize the cultivar
  Population="8",
  Depth="40",
  RowSpacing="762",
  fert_date = as.Date("2017-05-03"),
  fertamnt= "200",
  fertdepth= "40"
)


crop9 <- Mangment_campaign_json_maker(
  PlantingDate=as.Date("2018-05-03"),
  HarvestDate=as.Date("2018-10-16"),
  Crop = "maize",
  Cultivar = "?", # psims will ensemblize the cultivar
  Population="8",
  Depth="40",
  RowSpacing="762",
  fert_date = as.Date("2018-05-03"),
  fertamnt= "200",
  fertdepth= "40"
)


crop10 <- Mangment_campaign_json_maker(
  PlantingDate=as.Date("2019-05-03"),
  HarvestDate=as.Date("2019-10-16"),
  Crop = "maize",
  Cultivar = "?", # psims will ensemblize the cultivar
  Population="8",
  Depth="40",
  RowSpacing="762",
  fert_date = as.Date("2019-05-03"),
  fertamnt= "200",
  fertdepth= "40"
)

crop11 <- Mangment_campaign_json_maker(
  PlantingDate=as.Date("2020-05-03"),
  HarvestDate=as.Date("2020-10-16"),
  Crop = "maize",
  Cultivar = "?", # psims will ensemblize the cultivar
  Population="8",
  Depth="40",
  RowSpacing="762",
  fert_date = as.Date("2020-05-03"),
  fertamnt= "200",
  fertdepth= "40"
)


tmp_camp$management$events <-  c(crop1, crop2, crop3,
                                 crop4, crop5, crop6,
                                 crop7, crop8, crop9,
                                 crop10, crop11)


remove_var_campaign("MyCampaign.nc4", outfile="Campaign.nc4", varnames=c('myvar'))

Inspect_Camp("Campaign.nc4")[[4]]

plot(GetCamp_VarMatrix('MyCampaign.nc4','crid_6')$Raster[[1]])
GetCamp_VarMatrix('MyCampaign.nc4','date_10')

pSIMS_Site_Make(
  dirname = "/home/trai/pSIMS/DA",
  Project_name = "DA_maize_cal",
  Lat = 41.7125,
  Lon = -89.204167,
  Campaign_Path = c('Campaign.nc4','EnKF.R'),
  APSIM_Template_Path = "/home/trai/DA/template.apsim",
  Param_template_Obj = tmp_param,
  Campaign_json_Obj = tmp_camp,
  APSIM_Cultivar_Path = c(system.file("templates", "Maize_template.xml", package = "pSIMSSiteMaker"),
                          system.file("templates", "Soybean_template.xml", package = "pSIMSSiteMaker")),
  # Point 3
  host = host,
  Bash_control = list(
    pSIMS_Data_Path = "/pysims/data/sims/DA",
    # No need to edit this
    pSIMS_server_Path = "/projects/aces/tsrai/psims/Data",
    pSIMS_Sing_Image = "/projects/aces/hamzed/psims/Data/SingularityImg/pSIMSFull.simg"
  )
)


######################################################################################################
# Transfering the files to lab server
project_name <- "DA_maize_cal"
remote.copy.from(host=host,
                 src=paste0('/scratch/users/tsrai/',project_name),
                 dst=file.path("/mnt/iccp_storage/trai/DA_Results/Lee"),
                 delete = TRUE)

######################################################################################################

# Delete the empty directories
setwd("/mnt/iccp_storage/trai/")

library(TAF)
rmdir('/mnt/iccp_storage/trai/DA_Results/Lee/DA_maize_cal',recursive=T)

