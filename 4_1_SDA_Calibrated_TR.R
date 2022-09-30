# This script creates campaign for calibration.
# Spatial boundaries of the campaign are determined by county limits
# Same crop is grown at each pixel for all the years

library(pSIMSSiteMaker)
library(pSIMCampaignManager)
setwd("/mnt/iccp_storage/Regional_Calibration/")

state <- "illinois"
county <- "gallatin"
sim_name <- paste0(state, "_", county,"_SDA")
sim_years <- 2010:2020
num_years <- length(sim_years)


pSIMS_extent<-read.csv(system.file("Utils", "pSIMS_extents.csv", package = "pSIMSSiteMaker"))
fname <-  file.path(getwd(), "Simulations", sim_name, "Campaign.nc4")

# If it exists delete it
if(dir.exists(file.path("Simulations", sim_name))) unlink(file.path("Simulations", sim_name) , recursive = TRUE)
dir.create(file.path("Simulations", sim_name))
Sys.chmod(file.path("Simulations", sim_name), mode = "0777", use_umask = TRUE)

Rot_Rasters <- readRDS(file.path(getwd(),"Rotations", paste0(paste0(state, "_", county), ".RDS")))
#--------------------------------------------------------------------------------------------------
#------------------------------- Start creating the Campaign 
#-------------------------------------------------------------------------------------------------
#--- ------------Add calibrated parameters to the campaign
Optim_res <- readRDS(file.path(getwd(),"Outputs", paste0("illinois_",county, "/Optim_results.RDS")))
# get the objects we need from the optimization results
comparison <- Optim_res[[1]]
params <-  Optim_res[[5]]
head(params)

Allparams.df <- params %>%
  mutate(tt_emerg_to_endjuv = as.numeric(tt_emerg_to_endjuv),
         leaf_app_rate1 = as.numeric(leaf_app_rate1),
         leaf_init_rate = as.numeric(leaf_init_rate), 
         pdate= as.numeric(pdate)) %>%
  rename(pixel = `params[, 1]`)%>%
  tidyr::gather(Year, Yeareff, -pixel, -pdate, -tt_emerg_to_endjuv, -leaf_app_rate1, -leaf_init_rate) %>%
  split(.$Year) %>%
  map_dfr(function(one.year.param){
    
    comparison %>%
      ungroup() %>%
      dplyr::select(Pixel, lat, lon) %>%
      distinct(Pixel, .keep_all = TRUE) %>%
      left_join(one.year.param %>% 
                  rename(Pixel=pixel), by='Pixel') %>%
      mutate(pdate = pdate + as.numeric(Yeareff))
    
  })

crop.params <- Allparams.df %>%
  filter(Year == Allparams.df$Year[1])

#------------------------------------------ Adding the paramters to the Campaign
#Create_Empty_Campaign(lat=unique(crop.params$lat),
#                      lon=unique(crop.params$lon),
#                      num_scen=1,
#                      filename =fname)
#-----Campaign extent comes from the rotation raster now
# In case cells were not equally placed with earlier code
Create_Empty_Campaign(lat=unique(raster::xyFromCell(Rot_Rasters[[1]], 1:length(Rot_Rasters[[1]]))[,2]),
                      lon=unique(raster::xyFromCell(Rot_Rasters[[1]], 1:length(Rot_Rasters[[1]]))[,1]),
                      num_scen=1,
                      filename =fname)

Add_Scenario(fname, 59)
prop <- Inspect_Camp(fname)
num_scen <- Get_Camp_dim(fname)$Scen
count <- length(prop$Lat)*length(prop$Lon)
Inspect_Camp(fname)
summary(params)
#--------Calibrated parameters-------------------#

#mat.par <-  rasterFromXYZ(crop.params[,c('lon','lat',param)]) %>% as.matrix()
#new.values <-  seq_along(prop$Scen) %>% purrr::map(~mat.par %>% t )

new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix(fname, runif(Get_Camp_dim(fname)$Count, 200,450))[[1]])

AddVar_Campaign(fname, Variable = list(Name='tt_emerg_to_endjuv',
                                  Unit="C/day",
                                  missingValue=-99,
                                  value= new.values,
                                  longname="",
                                  prec="float"),
                attr = list('long_name',""))
  
new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix(fname, runif(Get_Camp_dim(fname)$Count, 40, 60))[[1]])
  
AddVar_Campaign(fname, Variable = list(Name='leaf_app_rate1',
                                  Unit="C/day",
                                  missingValue=-99,
                                  value= new.values,
                                  longname="",
                                  prec="float"),
                attr = list('long_name',""))
  
new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix(fname, runif(Get_Camp_dim(fname)$Count, 14,30))[[1]])

AddVar_Campaign(fname, Variable = list(Name='leaf_init_rate',
                                  Unit="C/day",
                                  missingValue=-99,
                                  value= new.values,
                                  longname="",
                                  prec="float"),
                attr = list('long_name',""))


plot(GetCamp_VarMatrix(fname,'tt_emerg_to_endjuv')$Raster[[5]])
plot(GetCamp_VarMatrix(fname,'leaf_app_rate1')$Raster[[5]])
plot(GetCamp_VarMatrix(fname,'leaf_init_rate')$Raster[[5]])
#--- add calibrated values of the 'tt_emerg_to_end_juv'

tt_emerg_to_endjuv <- brick(Allparams.df%>%
                        dplyr::select(lon,lat,tt_emerg_to_endjuv,Year)%>%
                        split(.$Year)%>%purrr::map(function(x) rasterFromXYZ(x%>%dplyr::select(-Year))))

MyVariable <- Rot_Rasters[[1]]

tt_emerg_to_endjuv <- data.frame(rasterToPoints(MyVariable))%>%
  left_join(as.data.frame(cbind(rasterToPoints(Rot_Rasters[[1]])[,c('x','y')],
                                tt_emerg_to_endjuv = as.integer(raster::extract(tt_emerg_to_endjuv,
                                                                                rasterToPoints(MyVariable)[,c('x','y')])))),
            by=c('x','y'))%>%na.omit()

nc <- nc_open(fname, write = T)
TempFile.var <- raster::stack(fname, varname='tt_emerg_to_endjuv')
TempFile.var[cellFromXY(TempFile.var,tt_emerg_to_endjuv[,c('x','y')])] <- tt_emerg_to_endjuv$tt_emerg_to_endjuv
new.var <- seq_len(nlayers(TempFile.var)) %>% purrr::map(~t(raster::as.matrix(TempFile.var[[.x]],byrow=T))) %>% 
  simplify2array ### transpose of the matrix is important
ncvar_put(nc, 'tt_emerg_to_endjuv', new.var)
nc_sync(nc)
nc_close(nc)
plot(GetCamp_VarMatrix(fname,'tt_emerg_to_endjuv')$Raster[[8]])

#--- add calibrated values of the 'leaf_app_rate1'

leaf_app_rate1 <- brick(Allparams.df%>%
                              dplyr::select(lon,lat,leaf_app_rate1,Year)%>%
                              split(.$Year)%>%purrr::map(function(x) rasterFromXYZ(x%>%dplyr::select(-Year))))

MyVariable <- Rot_Rasters[[1]]

leaf_app_rate1 <- data.frame(rasterToPoints(MyVariable))%>%
  left_join(as.data.frame(cbind(rasterToPoints(Rot_Rasters[[1]])[,c('x','y')],
                                leaf_app_rate1 = as.integer(raster::extract(leaf_app_rate1,
                                                                                rasterToPoints(MyVariable)[,c('x','y')])))),
            by=c('x','y'))%>%na.omit()

nc <- nc_open(fname, write = T)
TempFile.var <- raster::stack(fname, varname='leaf_app_rate1')
TempFile.var[cellFromXY(TempFile.var,leaf_app_rate1[,c('x','y')])] <- leaf_app_rate1$leaf_app_rate1
new.var <- seq_len(nlayers(TempFile.var)) %>% purrr::map(~t(raster::as.matrix(TempFile.var[[.x]],byrow=T))) %>% 
  simplify2array ### transpose of the matrix is important
ncvar_put(nc, 'leaf_app_rate1', new.var)
nc_sync(nc)
nc_close(nc)
plot(GetCamp_VarMatrix(fname,'leaf_app_rate1')$Raster[[8]])

#--- add calibrated values of the 'leaf_init_rate'

leaf_init_rate <- brick(Allparams.df%>%
                          dplyr::select(lon,lat,leaf_init_rate,Year)%>%
                          split(.$Year)%>%purrr::map(function(x) rasterFromXYZ(x%>%dplyr::select(-Year))))

MyVariable <- Rot_Rasters[[1]]

leaf_init_rate <- data.frame(rasterToPoints(MyVariable))%>%
  left_join(as.data.frame(cbind(rasterToPoints(Rot_Rasters[[1]])[,c('x','y')],
                                leaf_init_rate = as.integer(raster::extract(leaf_init_rate,
                                                                            rasterToPoints(MyVariable)[,c('x','y')])))),
            by=c('x','y'))%>%na.omit()

nc <- nc_open(fname, write = T)
TempFile.var <- raster::stack(fname, varname='leaf_init_rate')
TempFile.var[cellFromXY(TempFile.var,leaf_init_rate[,c('x','y')])] <- leaf_init_rate$leaf_init_rate
new.var <- seq_len(nlayers(TempFile.var)) %>% purrr::map(~t(raster::as.matrix(TempFile.var[[.x]],byrow=T))) %>% 
  simplify2array ### transpose of the matrix is important
ncvar_put(nc, 'leaf_init_rate', new.var)
nc_sync(nc)
nc_close(nc)
plot(GetCamp_VarMatrix(fname,'leaf_init_rate')$Raster[[8]])

############################################################################################
######## Crop Rotation
for (j in 1:num_years) {
  
  MyVariable <- Rot_Rasters[[j]]
  MyVariable[raster::values(MyVariable)==0] <- 2
  MyVariable[raster::values(MyVariable)==-1] <- 3
  MyVariable[is.na(MyVariable)] <- 3
  new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix(fname,
                                                                      MyVariable)[[1]])
  
  AddVar_Campaign(fname,
                  Variable = list(Name=paste0('crid_',j*3),
                                  Unit='Mapping',
                                  missingValue=-99,
                                  prec = 'float',
                                  longname="",
                                  value= new.values
                  )
  )
  
  Edit_mapping_var(fname, paste0('crid_',j*3) , 'long_name', "maize,soybean,fallow")
  
  AddVar_Campaign(fname,
                  Variable = list(Name=paste0('crid_',j*3-1),
                                  Unit='Mapping',
                                  missingValue=-99,
                                  prec = 'float',
                                  longname="",
                                  value= new.values
                  )
  )
  
  Edit_mapping_var(fname, paste0('crid_',j*3-1) , 'long_name', "maize,soybean,fallow")
  
  AddVar_Campaign(fname,
                  Variable = list(Name=paste0('crid_',j*3-2),
                                  Unit='Mapping',
                                  missingValue=-99,
                                  prec = 'float',
                                  longname="",
                                  value= new.values
                  )
  )
  
  Edit_mapping_var(fname, paste0('crid_',j*3-2) , 'long_name', "maize,soybean,fallow")
  
  print(j)
  
}

######## Cultivar id

cultivar_list <- "?,Elgin_2.7,HiSoy2846_2.8,IA_2008_2.0,K283_2.0,Krucr_2.7,Pioneer_92M61_2.6,U01390224_2.5"

new.values <- list()

for (j in 1:num_years) {  # j is equal to the number of years
  
  MyVariable <- Rot_Rasters[[j]]
  MyVariable[raster::values(MyVariable)==0] <- 2
  MyVariable[raster::values(MyVariable)==-1] <- NA
  MyVariable[is.na(MyVariable)] <- NA
  
  
  
  new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix(fname,
                                                                      MyVariable)[[1]])
  
  new.values2 <- rapply(new.values, function(x) ifelse(x==2,sample(2:8,200, replace = T),x),how = 'replace')
  
  AddVar_Campaign(fname,Variable = list(Name=paste0('cul_id_',j),
                                                   Unit='Mapping',
                                                   missingValue=-99,
                                                   prec='float',
                                                   longname="",
                                                   value= new.values2)
  )
  
  Edit_mapping_var(fname, paste0('cul_id_',j) , 'long_name', cultivar_list)
  
  print(j)
  
}
###### Plant population

for (j in 1:num_years) {
  
  MyVariable <- Rot_Rasters[[j]]
  MyVariable[raster::values(MyVariable)==0] <- 2
  MyVariable[raster::values(MyVariable)==-1] <- NA
  MyVariable[is.na(MyVariable)] <- NA
  
  MyVariable[raster::values(MyVariable) == 1] <- 8
  MyVariable[raster::values(MyVariable) == 2] <- 30
  
  
  new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix(fname,
                                                                      MyVariable)[[1]])
  
  AddVar_Campaign(fname,
                  Variable = list(Name=paste0('plpop_',j),
                                  Unit='seeds/m2',
                                  missingValue=-99,
                                  longname="",
                                  prec='float',
                                  value= new.values
                  ),
                  attr = list('long_name',"")
  )
  
  print(j)
}
#--------------------------------Adding met
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

for (j in 1:num_years) {
  
  MyVariable <- Rot_Rasters[[j]]
  MyVariable[raster::values(MyVariable)==-1] <- 0
  MyVariable[is.na(MyVariable)] <- 0
  
  new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix(fname,
                                                                      MyVariable)[[1]])
  
  new.values2 <- rapply(new.values,function(x) ifelse(x==1,runif(200,180,220),x),how = 'replace')
  
  AddVar_Campaign(fname,
                  Variable = list(Name=paste0('feamn_',j),
                                  Unit='kg/ha',
                                  missingValue=-99,
                                  longname="",
                                  prec='float',
                                  value= new.values2
                  ),
                  attr = list('long_name',"")
  )
  
  print(j)
}

GetCamp_VarMatrix(fname,'feamn_9')[[1]][,,5]
plot(GetCamp_VarMatrix(fname,'feamn_9')$Raster[[5]])


############################################################################################
###### Planting Date and fertilizer date-TR

for (j in 1:num_years) {
  
  MyVariable <- Rot_Rasters[[j]]
  
  MyVariable[raster::values(MyVariable)==0] <- 2
  MyVariable[raster::values(MyVariable)==-1] <- NA
  #MyVariable[raster::values(MyVariable)==1] <- planting_dates$pdate
  
  
  new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix(fname,
                                                                      MyVariable)[[1]])
  
  new.values2 <- rapply(new.values,function(x) ifelse(x==2,
                                                      sample(136:151,200,replace = T),
                                                      x),how = 'replace')
  new.values2 <- rapply(new.values2,function(x) ifelse(x==1,
                                                       sample(121:135,200,replace = T),
                                                       x),how = 'replace')
  
  AddVar_Campaign(fname,Variable = list(Name=paste0('date_',4*j-1),
                                        Unit='Mapping',
                                        missingValue=-99,
                                        prec='float',
                                        longname="",
                                        value= new.values2))
  Edit_mapping_var(fname, paste0('date_',4*j-1), 'long_name', paste(gsub('-','',
                                                                         as.Date(1:max(unique(unlist(new.values2)),na.rm = T),
                                                                                 origin = paste0((j)+2008,'-12-31'))),
                                                                    collapse = ','))
  
  AddVar_Campaign(fname,Variable = list(Name=paste0('date_',4*j-2),
                                        Unit='Mapping',
                                        missingValue=-99,
                                        prec='float',
                                        longname="",
                                        value= new.values2))
  Edit_mapping_var(fname, paste0('date_',4*j-2), 'long_name', paste(gsub('-','',
                                                                         as.Date(1:max(unique(unlist(new.values2)),na.rm = T),
                                                                                 origin = paste0((j)+2008,'-12-31'))),
                                                                    collapse = ','))
  
  print(j)
  
}

Inspect_Camp(fname)
plot(GetCamp_VarMatrix(fname,'date_38')$Raster[[5]])
GetCamp_VarMatrix(fname,'date_15')[[1]][,,8]

#-----This chink will replace the values of planting date with calibrated pdate-----#
pdate_raster <- brick(Allparams.df%>%
                        dplyr::select(lon,lat,pdate,Year)%>%
                        split(.$Year)%>%purrr::map(function(x) rasterFromXYZ(x%>%dplyr::select(-Year))))


#j <- 1
for (j in 1:num_years) {
  
  MyVariable <- Rot_Rasters[[j]]
  
  planting_dates <- data.frame(rasterToPoints(MyVariable))%>%filter(crp==1)%>%
    left_join(as.data.frame(cbind(rasterToPoints(MyVariable)[,c('x','y')],
                                  pdate = as.integer(raster::extract(pdate_raster[[j]],
                                                                     rasterToPoints(MyVariable)[,c('x','y')])))),
              by=c('x','y'))%>%na.omit()
  
  nc <- nc_open(fname, write = T)
  TempFile.var <- raster::stack(fname, varname=paste0('date_',4*j-1))
  TempFile.var[cellFromXY(TempFile.var,planting_dates[,c('x','y')])] <- planting_dates$pdate
  new.var <- seq_len(nlayers(TempFile.var)) %>% purrr::map(~t(raster::as.matrix(TempFile.var[[.x]],byrow=T))) %>% 
    simplify2array ### transpose of the matrix is important
  ncvar_put(nc, paste0('date_',4*j-1), new.var)
  nc_sync(nc)
  
  TempFile.var <- raster::stack(fname, varname=paste0('date_',4*j-2))
  TempFile.var[cellFromXY(TempFile.var,planting_dates[,c('x','y')])] <- planting_dates$pdate
  new.var <- seq_len(nlayers(TempFile.var)) %>% purrr::map(~t(raster::as.matrix(TempFile.var[[.x]],byrow=T))) %>% 
    simplify2array ### transpose of the matrix is important
  ncvar_put(nc, paste0('date_',4*j-2), new.var)
  nc_sync(nc)
  nc_close(nc)
  
  print(j)
  
}

#----------Adjust the longname later as well---------#
for (j in 1:num_years) {
  
  Edit_mapping_var(fname, paste0('date_',4*j-1), 'long_name', paste(gsub('-','',
                                                                                    as.Date(1:max(unlist(GetCamp_VarMatrix(fname,paste0('date_',4*j-1))$Matrix),na.rm=TRUE),
                                                                                            origin = paste0((j)+2008,'-12-31'))),
                                                                               collapse = ','))
  
  
  Edit_mapping_var(fname, paste0('date_',4*j-2), 'long_name', paste(gsub('-','',
                                                                                    as.Date(1:max(unlist(GetCamp_VarMatrix(fname,paste0('date_',4*j-2))$Matrix),na.rm=TRUE),
                                                                                            origin = paste0((j)+2008,'-12-31'))),
                                                                               collapse = ','))
  
  print(j)
  
  
}

Inspect_Camp(fname)
plot(GetCamp_VarMatrix(fname,'date_39')$Raster[[5]])


###### Harvesting and termination date

for (j in 1:num_years) {
  
  MyVariable <- Rot_Rasters[[j]]
  MyVariable[raster::values(MyVariable)==0] <- 2
  MyVariable[raster::values(MyVariable)==-1] <- NA
  
  
  new.values <- purrr::map(seq_along(num_scen), ~Campaign_emptyMatrix(fname,MyVariable)[[1]])
  
  AddVar_Campaign(fname,Variable = list(Name=paste0('date_',4*j),
                                                   Unit='Mapping',
                                                   missingValue=-99,
                                                   prec='float',
                                                    longname="",
                                                   value= new.values))
  Edit_mapping_var(fname, paste0('date_',4*j), 'long_name', paste(gsub('-','',as.Date(sort(unique(unlist(new.values))),origin = paste0((j)+2009,'-10-15'))),
                                                                             collapse = ','))
  
  
  AddVar_Campaign(fname,Variable = list(Name=paste0('date_',4*j+1),
                                                   Unit='Mapping',
                                                   missingValue=-99,
                                                   prec='float',
                                                    longname="",
                                                   value= new.values))
  Edit_mapping_var(fname, paste0('date_',4*j+1), 'long_name', paste(gsub('-','',as.Date(sort(unique(unlist(new.values))),origin = paste0((j)+2009,'-10-15'))),
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
plot(county_mask2)

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
tmp_param <- Read_param_template(file.path(getwd(), "Templates", "params.apsim.sample_SDA"))
tmp_param$ref_year <- min(sim_years)%>% as.integer()
tmp_param$num_years <- length(sim_years)%>% as.integer()
tmp_param$scen_years <- length(sim_years)%>% as.integer()
tmp_param$scens <- 60L
tmp_param$tappinp$templatefile <- "template_SDA.apsim"
tmp_param$delta <- "2.5,2.5"
tmp_param$soils <- '/pysims/data/soils/Soils'
tmp_param$weather <- "/pysims/data/clim/NewMet/"
tmp_param$checker$simgfile <- "../../mask.nc"
tmp_param$Pre_run_command <- "Rscript ../../SoilFixer.R"
tmp_param$Post_run_command <- "Rscript ../../Replace_sql_files.R"

#Modifying the campaign json file 
tmp_camp <- Read_Campaign_template(file.path(getwd(), "Templates", "exp_template_SDA.json"))  # This is different from one that was used in the rotation exp.
tmp_camp$reporting_frequency <- "daily"

# Point2 
tmp_camp$fertilizer$crop <- "Maize"
tmp_camp$fertilizer$type <- "NH4NO3"
tmp_camp$planting$crop <- "Maize"
tmp_camp$initial_condition$residue_type <- "Maize"
tmp_camp$reset$date <- "01-jan"

#-----Creating the operations for each crop separately
tmp_camp$management$events  %>%
  flatten()


host <- list(name = 'cc-login.campuscluster.illinois.edu',
             user = 'tsrai',
             tunnel = '~/tunnel/tunnel',
             from= file.path(getwd(), "Simulations", sim_name),
             to='/scratch/users/tsrai/')
#host <- list(name = 'cc-login.campuscluster.illinois.edu',
#             user = 'tsrai',
#             tunnel = '~/tunnel/tunnel',
#             from= file.path(getwd(), "Simulations", sim_name),
#             to='/projects/aces/hamzed/psims/Data/sims')

#debugonce(pSIMS_Site_Make)

pSIMS_Site_Make(
  dirname = file.path(getwd(), "Simulations", sim_name),
  Project_name = sim_name,
  Lat = 41.7125,
  Lon = -89.204167,
  Tile = tile_number,
  #Auxiliary_files = c(), # This would put files in the campign dir, as results all will be copied to all runs
  Campaign_Path = c(file.path(getwd(), "Simulations",sim_name, 'Campaign.nc4'),
                    file.path(getwd(), "Templates", 'EnKF.R'),
                    file.path(getwd(), "Templates", 'MakeObs_ESA_CCI.R')),
  APSIM_Template_Path = file.path(getwd(), "Templates", 'template_SDA.apsim'),
  Param_template_Obj = tmp_param,
  Campaign_json_Obj = tmp_camp,
  APSIM_Cultivar_Path = c(file.path(getwd(), "Templates", 'Maize_template.xml'),
                          file.path(getwd(), "Templates", 'Soybean_template.xml'),
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
