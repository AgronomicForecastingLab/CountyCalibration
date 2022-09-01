# Sets up simulations after the calibration has been performed
# The loops start from 6 to factor in 5 years of spin-up period

library(pSIMSSiteMaker)
library(pSIMCampaignManager)

setwd("~/DA")

Rot_Rasters <- readRDS('Created_rasters/Rot_Rasters.RDS')
Rot_Rasters[is.na(Rot_Rasters)] <- -1 ## NA are turned into -1

fname <-  "MyCampaign.nc4"
Create_Empty_Campaign(lat=unique(raster::xyFromCell(Rot_Rasters[[1]], 1:length(Rot_Rasters[[1]]))[,2]),
                      lon=unique(raster::xyFromCell(Rot_Rasters[[1]], 1:length(Rot_Rasters[[1]]))[,1]),
                      num_scen=50,
                      filename =fname)

num_scen <- Get_Camp_dim("MyCampaign.nc4")$Scen

num_years <- 16  ## Can vary upto 11 (2010-2020)


Inspect_Camp("MyCampaign.nc4")

############################################################################################
######## Crop Rotation


for (j in 6:num_years) {
  
  MyVariable <- Rot_Rasters[[j-5]]
  MyVariable[raster::values(MyVariable)==0] <- 2
  MyVariable[raster::values(MyVariable)==-1] <- 3
  MyVariable[is.na(MyVariable)] <- 3
  new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix("MyCampaign.nc4",
                                                                      MyVariable)[[1]])
  
  AddVar_Campaign("MyCampaign.nc4",
                  Variable = list(Name=paste0('crid_',j*3),
                                  Unit='Mapping',
                                  missingValue=-99,
                                  prec = 'float',
                                  value= new.values
                  )
  )
  
  Edit_mapping_var("MyCampaign.nc4", paste0('crid_',j*3) , 'long_name', "maize,soybean,fallow")
  
  AddVar_Campaign("MyCampaign.nc4",
                  Variable = list(Name=paste0('crid_',j*3-1),
                                  Unit='Mapping',
                                  missingValue=-99,
                                  prec = 'float',
                                  value= new.values
                  )
  )
  
  Edit_mapping_var("MyCampaign.nc4", paste0('crid_',j*3-1) , 'long_name', "maize,soybean,fallow")
  
  AddVar_Campaign("MyCampaign.nc4",
                  Variable = list(Name=paste0('crid_',j*3-2),
                                  Unit='Mapping',
                                  missingValue=-99,
                                  prec = 'float',
                                  value= new.values
                  )
  )
  
  Edit_mapping_var("MyCampaign.nc4", paste0('crid_',j*3-2) , 'long_name', "maize,soybean,fallow")
  
  print(j)
  
}


Inspect_Camp('MyCampaign.nc4')
GetCamp_VarMatrix('MyCampaign.nc4','crid_16')

plot(GetCamp_VarMatrix('MyCampaign.nc4','crid_16')$Raster[[3]],col=c('orange','darkgreen','black'))


############################################################################################
######## Cultivar id

cultivar_list <- "?,Elgin_2.7,HiSoy2846_2.8,IA_2008_2.0,K283_2.0,Krucr_2.7,Pioneer_92M61_2.6,U01390224_2.5"

new.values <- list()

for (j in 6:num_years) {  # j is equal to the number of years
  
  MyVariable <- Rot_Rasters[[j-5]]
  MyVariable[raster::values(MyVariable)==0] <- 2
  MyVariable[raster::values(MyVariable)==-1] <- NA
  MyVariable[is.na(MyVariable)] <- NA
  
  
  
  new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix("MyCampaign.nc4",
                                                                      MyVariable)[[1]])
  
  new.values2 <- rapply(new.values, function(x) ifelse(x==2,sample(2:8,200, replace = T),x),how = 'replace')
  
  AddVar_Campaign("MyCampaign.nc4",Variable = list(Name=paste0('cul_id_',j),
                                                   Unit='Mapping',
                                                   missingValue=-99,
                                                   prec='float',
                                                   value= new.values2)
  )
  
  Edit_mapping_var("MyCampaign.nc4", paste0('cul_id_',j) , 'long_name', cultivar_list)
  
  print(j)
  
}

GetCamp_VarMatrix('MyCampaign.nc4',paste0('cul_id_6'))[[1]][,,3]

plot(GetCamp_VarMatrix('MyCampaign.nc4','cul_id_9')$Raster[[1]])
plot(GetCamp_VarMatrix('MyCampaign.nc4','cul_id_9')$Raster[[2]])

############################################################################################
###### Plant population

for (j in 6:num_years) {
  
  MyVariable <- Rot_Rasters[[j-5]]
  MyVariable[raster::values(MyVariable)==0] <- 2
  MyVariable[raster::values(MyVariable)==-1] <- NA
  MyVariable[is.na(MyVariable)] <- NA
  
  MyVariable[raster::values(MyVariable) == 1] <- 8
  MyVariable[raster::values(MyVariable) == 2] <- 30
  
  
  new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix("MyCampaign.nc4",
                                                                      MyVariable)[[1]])
  
  AddVar_Campaign("MyCampaign.nc4",
                  Variable = list(Name=paste0('plpop_',j),
                                  Unit='seeds/m2',
                                  missingValue=-99,
                                  value= new.values
                  ),
                  attr = list('long_name',"")
  )
  
  print(j)
}

plot(GetCamp_VarMatrix('MyCampaign.nc4','plpop_8')$Raster[[1]])


############################################################################################
###### Fertilizer Amount

Inspect_Camp("MyCampaign.nc4")

for (j in 6:num_years) {
  
  MyVariable <- Rot_Rasters[[j-5]]
  MyVariable[raster::values(MyVariable)==-1] <- 0
  MyVariable[is.na(MyVariable)] <- 0
  
  new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix("MyCampaign.nc4",
                                                                      MyVariable)[[1]])
  
  new.values2 <- rapply(new.values,function(x) ifelse(x==1,runif(200,180,220),x),how = 'replace')
  
  AddVar_Campaign("MyCampaign.nc4",
                  Variable = list(Name=paste0('feamn_',j),
                                  Unit='kg/ha',
                                  missingValue=-99,
                                  value= new.values2
                  ),
                  attr = list('long_name',"")
  )
  
  print(j)
}

GetCamp_VarMatrix('MyCampaign.nc4','feamn_9')[[1]][,,5]
plot(GetCamp_VarMatrix('MyCampaign.nc4','feamn_9')$Raster[[5]])

############################################################################################
###### Planting Date and fertilizer date


for (j in 6:num_years) {
  
  MyVariable <- Rot_Rasters[[j-5]]
  
  MyVariable[raster::values(MyVariable)==0] <- 2
  MyVariable[raster::values(MyVariable)==-1] <- NA
  #MyVariable[raster::values(MyVariable)==1] <- planting_dates$pdate
  
  
  new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix("MyCampaign.nc4",
                                                                      MyVariable)[[1]])
  
  new.values2 <- rapply(new.values,function(x) ifelse(x==2,
                                                      sample(136:151,200,replace = T),
                                                      x),how = 'replace')
  new.values2 <- rapply(new.values2,function(x) ifelse(x==1,
                                                       sample(121:135,200,replace = T),
                                                       x),how = 'replace')
  
  AddVar_Campaign("MyCampaign.nc4",Variable = list(Name=paste0('date_',4*j-1),
                                                   Unit='Mapping',
                                                   missingValue=-99,
                                                   prec='float',
                                                   value= new.values2))
  Edit_mapping_var('MyCampaign.nc4', paste0('date_',4*j-1), 'long_name', paste(gsub('-','',
                                                                                    as.Date(1:max(unique(unlist(new.values2)),na.rm = T),
                                                                                            origin = paste0((j-5)+2008,'-12-31'))),
                                                                               collapse = ','))
  
  AddVar_Campaign("MyCampaign.nc4",Variable = list(Name=paste0('date_',4*j-2),
                                                   Unit='Mapping',
                                                   missingValue=-99,
                                                   prec='float',
                                                   value= new.values2))
  Edit_mapping_var('MyCampaign.nc4', paste0('date_',4*j-2), 'long_name', paste(gsub('-','',
                                                                                    as.Date(1:max(unique(unlist(new.values2)),na.rm = T),
                                                                                            origin = paste0((j-5)+2008,'-12-31'))),
                                                                               collapse = ','))
  
  print(j)
  
}
Inspect_Camp("MyCampaign.nc4")
plot(GetCamp_VarMatrix('MyCampaign.nc4','date_55')$Raster[[5]])
GetCamp_VarMatrix('MyCampaign.nc4','date_15')[[1]][,,8]

GetCamp_VarMatrix('MyCampaign.nc4','date_3')

#-----This chink will replace the values of planting date with calibrated pdate-----#
pdate_raster <- brick('pdate_raster.gri')

#j <- 1
for (j in 6:num_years) {
  
  MyVariable <- Rot_Rasters[[j-5]]
  
  planting_dates <- data.frame(rasterToPoints(MyVariable))%>%filter(crp==1)%>%
    left_join(as.data.frame(cbind(rasterToPoints(MyVariable)[,c('x','y')],
                                  pdate = as.integer(raster::extract(pdate_raster[[j-5]],
                                                                     rasterToPoints(MyVariable)[,c('x','y')])))),
              by=c('x','y'))%>%na.omit()
  
  nc <- nc_open('MyCampaign.nc4', write = T)
  TempFile.var <- raster::stack('MyCampaign.nc4', varname=paste0('date_',4*j-1))
  TempFile.var[cellFromXY(TempFile.var,planting_dates[,c('x','y')])] <- planting_dates$pdate
  new.var <- seq_len(nlayers(TempFile.var)) %>% purrr::map(~t(raster::as.matrix(TempFile.var[[.x]],byrow=T))) %>% 
  simplify2array ### transpose of the matrix is important
  ncvar_put(nc, paste0('date_',4*j-1), new.var)
  nc_sync(nc)
  
  TempFile.var <- raster::stack('MyCampaign.nc4', varname=paste0('date_',4*j-2))
  TempFile.var[cellFromXY(TempFile.var,planting_dates[,c('x','y')])] <- planting_dates$pdate
  new.var <- seq_len(nlayers(TempFile.var)) %>% purrr::map(~t(raster::as.matrix(TempFile.var[[.x]],byrow=T))) %>% 
    simplify2array ### transpose of the matrix is important
  ncvar_put(nc, paste0('date_',4*j-2), new.var)
  
  nc_sync(nc)
  nc_close(nc)
  
  print(j)
  
}

#----------Adjust the longname later as well---------#
for (j in 6:num_years) {
  
  Edit_mapping_var('MyCampaign.nc4', paste0('date_',4*j-1), 'long_name', paste(gsub('-','',
                                                                                    as.Date(1:max(unlist(GetCamp_VarMatrix('MyCampaign.nc4',paste0('date_',4*j-1))$Matrix),na.rm=TRUE),
                                                                                            origin = paste0((j-5)+2008,'-12-31'))),
                                                                               collapse = ','))
  
  
  Edit_mapping_var('MyCampaign.nc4', paste0('date_',4*j-2), 'long_name', paste(gsub('-','',
                                                                                    as.Date(1:max(unlist(GetCamp_VarMatrix('MyCampaign.nc4',paste0('date_',4*j-2))$Matrix),na.rm=TRUE),
                                                                                            origin = paste0((j-5)+2008,'-12-31'))),
                                                                               collapse = ','))
  
  print(j)
  
  
}

Inspect_Camp("MyCampaign.nc4")
plot(GetCamp_VarMatrix('MyCampaign.nc4','date_46')$Raster[[5]])
plot(GetCamp_VarMatrix('MyCampaign.nc4','date_47')$Raster[[5]])

############################################################################################
###### Harvesting and termination date

for (j in 6:num_years) {
  
  MyVariable <- Rot_Rasters[[j-5]]
  MyVariable[raster::values(MyVariable)==0] <- 2
  MyVariable[raster::values(MyVariable)==-1] <- NA
  
  
  new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix("MyCampaign.nc4",MyVariable)[[1]])
  
  AddVar_Campaign("MyCampaign.nc4",Variable = list(Name=paste0('date_',4*j),
                                                   Unit='Mapping',
                                                   missingValue=-99,
                                                   prec='float',
                                                   value= new.values))
  Edit_mapping_var('MyCampaign.nc4', paste0('date_',4*j), 'long_name', paste(gsub('-','',as.Date(sort(unique(unlist(new.values))),origin = paste0((j-5)+2009,'-10-15'))),
                                                                             collapse = ','))
  
  
  AddVar_Campaign("MyCampaign.nc4",Variable = list(Name=paste0('date_',4*j+1),
                                                   Unit='Mapping',
                                                   missingValue=-99,
                                                   prec='float',
                                                   value= new.values))
  Edit_mapping_var('MyCampaign.nc4', paste0('date_',4*j+1), 'long_name', paste(gsub('-','',as.Date(sort(unique(unlist(new.values))),origin = paste0((j-5)+2009,'-10-15'))),
                                                                               collapse = ','))
  
  print(j)
}

Inspect_Camp("MyCampaign.nc4")
GetCamp_VarMatrix('MyCampaign.nc4','date_45')
plot(GetCamp_VarMatrix('MyCampaign.nc4','date_45')$Raster[[1]])

############################################################################################
#### ADDING SOME WEATHER FILES TO THE CAMPAIGN

new.values <- purrr::map(Get_Camp_dim("MyCampaign.nc4")$Scen,~Campaign_emptyMatrix("MyCampaign.nc4",
                                                                                   sample(c(1:9), Get_Camp_dim("MyCampaign.nc4")$Count,TRUE)
)[[1]])


AddVar_Campaign("MyCampaign.nc4",
                Variable = list(Name='file',
                                Unit='Mapping',
                                missingValue=-99,
                                value= new.values
                ),
                attr = list('long_name',"met00000.met,met00001.met,met00002.met,met00003.met,
                            met00004.met,met00005.met,met00006.met,met00007.met,met00008.met,
                            met00009.met"))



Edit_mapping_var ("MyCampaign.nc4", 'file' , 'long_name', "met00000.met,met00001.met,met00002.met,met00003.met,met00004.met,met00005.met,met00006.met,met00007.met,met00008.met,met00009.met")

plot(GetCamp_VarMatrix('MyCampaign.nc4','file')$Raster[[1]])

############################################################################################
######## Initial Conditions

############################################################################################
##### MAIZE PARAMETERS


new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix("MyCampaign.nc4",
                                                                    runif(Get_Camp_dim("MyCampaign.nc4")$Count, 780,860)
)[[1]])

AddVar_Campaign("MyCampaign.nc4",
                Variable = list(Name='tt_flower_to_maturity',
                                Unit='C-d',
                                missingValue=-99,
                                value= new.values
                ),
                attr = list('long_name',""))

GetCamp_VarMatrix('MyCampaign.nc4','tt_flower_to_maturity')

############################################################################################

new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix("MyCampaign.nc4",
                                                                    runif(Get_Camp_dim("MyCampaign.nc4")$Count, 750,900)
)[[1]])

AddVar_Campaign("MyCampaign.nc4",
                Variable = list(Name='head_grain_no_max',
                                Unit='Kernel/ear',
                                missingValue=-99,
                                value= new.values
                ),
                attr = list('long_name',""))

GetCamp_VarMatrix('MyCampaign.nc4','head_grain_no_max')

############################################################################################

new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix("MyCampaign.nc4",
                                                                    runif(Get_Camp_dim("MyCampaign.nc4")$Count, 7.1,8.57)
)[[1]])

AddVar_Campaign("MyCampaign.nc4",
                Variable = list(Name='grain_gth_rate',
                                Unit='ggraing/day',
                                missingValue=-99,
                                value= new.values
                ),
                attr = list('long_name',""))

GetCamp_VarMatrix('MyCampaign.nc4','grain_gth_rate')

############################################################################################

new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix("MyCampaign.nc4",
                                                                    runif(Get_Camp_dim("MyCampaign.nc4")$Count, 150,200)
)[[1]])

AddVar_Campaign("MyCampaign.nc4",
                Variable = list(Name='tt_flower_to_start_grain',
                                Unit='C/day',
                                missingValue=-99,
                                value= new.values
                ),
                attr = list('long_name',""))

GetCamp_VarMatrix('MyCampaign.nc4','tt_flower_to_start_grain')

############################################################################################

new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix("MyCampaign.nc4",
                                                                    runif(Get_Camp_dim("MyCampaign.nc4")$Count, 150,250)
)[[1]])

AddVar_Campaign("MyCampaign.nc4",
                Variable = list(Name='tt_maturity_to_ripe',
                                Unit='C/day',
                                missingValue=-99,
                                value= new.values
                ),
                attr = list('long_name',""))

GetCamp_VarMatrix('MyCampaign.nc4','tt_maturity_to_ripe')

############################################################################################



new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix("MyCampaign.nc4",
                                                                    runif(Get_Camp_dim("MyCampaign.nc4")$Count, 240,260)
)[[1]])

AddVar_Campaign("MyCampaign.nc4",
                Variable = list(Name='tt_emerg_to_endjuv',
                                Unit='C/day',
                                missingValue=-99,
                                value= new.values
                ),
                attr = list('long_name',""))

plot(GetCamp_VarMatrix('MyCampaign.nc4','tt_emerg_to_endjuv')$Raster[[5]])

#--- add calibrated values of the 'tt_emerg_to_end_juv'

tt_emerg_to_endjuv <- brick('tt_emerg_to_endjuv.gri')

MyVariable <- Rot_Rasters[[1]]

tt_emerg_to_endjuv <- data.frame(rasterToPoints(MyVariable))%>%
  left_join(as.data.frame(cbind(rasterToPoints(Rot_Rasters[[1]])[,c('x','y')],
                                tt_emerg_to_endjuv = as.integer(raster::extract(tt_emerg_to_endjuv,
                                                                   rasterToPoints(MyVariable)[,c('x','y')])))),
            by=c('x','y'))%>%na.omit()

nc <- nc_open('MyCampaign.nc4', write = T)
TempFile.var <- raster::stack('MyCampaign.nc4', varname='tt_emerg_to_endjuv')
TempFile.var[cellFromXY(TempFile.var,tt_emerg_to_endjuv[,c('x','y')])] <- tt_emerg_to_endjuv$tt_emerg_to_endjuv
new.var <- seq_len(nlayers(TempFile.var)) %>% purrr::map(~t(raster::as.matrix(TempFile.var[[.x]],byrow=T))) %>% 
  simplify2array ### transpose of the matrix is important
ncvar_put(nc, 'tt_emerg_to_endjuv', new.var)
nc_sync(nc)
nc_close(nc)
plot(GetCamp_VarMatrix('MyCampaign.nc4','tt_emerg_to_endjuv')$Raster[[8]])

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
tmp_param$ref_year <- 2005L
tmp_param$num_years <- 16L
tmp_param$scen_years <- 16L
tmp_param$scens <- 50L
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
  PlantingDate=as.Date("2005-05-03"),
  HarvestDate=as.Date("2005-10-16"),
  Crop = "maize",
  Cultivar = "?", # psims will ensemblize the cultivar
  Population="8",
  Depth="40",
  RowSpacing="762",
  fert_date = as.Date("2005-05-03"),
  fertamnt= "200",
  fertdepth= "40"
)

#-----Creating the operations for each crop separately
crop2 <- Mangment_campaign_json_maker(
  PlantingDate=as.Date("2006-05-03"),
  HarvestDate=as.Date("2006-10-16"),
  Crop = "maize",
  Cultivar = "?", # psims will ensemblize the cultivar
  Population="8",
  Depth="40",
  RowSpacing="762",
  fert_date = as.Date("2006-05-03"),
  fertamnt= "200",
  fertdepth= "40"
)

crop3 <- Mangment_campaign_json_maker(
  PlantingDate=as.Date("2007-05-03"),
  HarvestDate=as.Date("207-10-16"),
  Crop = "maize",
  Cultivar = "?", # psims will ensemblize the cultivar
  Population="8",
  Depth="40",
  RowSpacing="762",
  fert_date = as.Date("2007-05-03"),
  fertamnt= "200",
  fertdepth= "40"
)

crop4 <- Mangment_campaign_json_maker(
  PlantingDate=as.Date("2008-05-03"),
  HarvestDate=as.Date("2008-10-16"),
  Crop = "maize",
  Cultivar = "?", # psims will ensemblize the cultivar
  Population="8",
  Depth="40",
  RowSpacing="762",
  fert_date = as.Date("2008-05-03"),
  fertamnt= "200",
  fertdepth= "40"
)

crop5 <- Mangment_campaign_json_maker(
  PlantingDate=as.Date("2009-05-03"),
  HarvestDate=as.Date("2009-10-16"),
  Crop = "maize",
  Cultivar = "?", # psims will ensemblize the cultivar
  Population="8",
  Depth="40",
  RowSpacing="762",
  fert_date = as.Date("2009-05-03"),
  fertamnt= "200",
  fertdepth= "40"
)
crop6 <- Mangment_campaign_json_maker(
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
crop7 <- Mangment_campaign_json_maker(
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

crop8 <- Mangment_campaign_json_maker(
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

crop9 <- Mangment_campaign_json_maker(
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

crop10 <- Mangment_campaign_json_maker(
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

crop11 <- Mangment_campaign_json_maker(
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

crop12 <- Mangment_campaign_json_maker(
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

crop13 <- Mangment_campaign_json_maker(
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


crop14 <- Mangment_campaign_json_maker(
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


crop15 <- Mangment_campaign_json_maker(
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

crop16 <- Mangment_campaign_json_maker(
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
                                 crop10, crop11, crop12,
                                 crop13, crop14, crop15)


remove_var_campaign("MyCampaign.nc4", outfile="Campaign.nc4", varnames=c('myvar'))

Inspect_Camp("Campaign.nc4")[[4]]

plot(GetCamp_VarMatrix('MyCampaign.nc4','crid_6')$Raster[[1]])
GetCamp_VarMatrix('MyCampaign.nc4','date_10')

pSIMS_Site_Make(
  dirname = "/home/trai/pSIMS/DA",
  Project_name = "Calibrated_DA",
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
project_name <- "Calibrated_DA"
remote.copy.from(host=host,
                 src=paste0('/scratch/users/tsrai/',project_name),
                 dst=file.path("/mnt/iccp_storage/trai/DA_Results/Lee"),
                 delete = TRUE)

######################################################################################################


setwd("/mnt/iccp_storage/trai/")

library(TAF)
rmdir('/mnt/iccp_storage/trai/DA_Results/Lee/DA_maize_cal',recursive=T)

