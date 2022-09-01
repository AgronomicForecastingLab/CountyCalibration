# The aim is to extract and store crop rotaions as rasters for a county
library(lubridate)
library(ncdf4)
library(pSIMSSiteMaker)
library(pSIMCampaignManager)
library(furrr)
plan(multisession)
#-------
library(tidyverse)
library(raster)
library(stars)
library(maps)
library(sf)

county <- "lee"
years <- 2010:2020
state <- 'Illinois'

mapping_guid <- list(corn=1, soybeans = 2)
# ------------------------------------- Find the boundry of our county 
IL_counties <- st_as_sf( map('county', state, fill=TRUE, plot =FALSE)) %>%
  filter(grepl(county, ID)) %>%
  dplyr::select(-ID) %>%
  as(., "Spatial")
plot(IL_counties)
#------------------------------------ Generate empty raster layer and rasterize points for our county
county_raster <- raster(crs = crs(IL_counties), vals = 1,
                        resolution = c(0.01, 0.01), # 1 km by 1 km . every degree is ~ 100 km
                        ext = extent(IL_counties)) %>%
  rasterize(IL_counties, .)
plot(county_raster)
#convert the raster to polygon to loop over each pixel
county_polys <- rasterToPolygons(county_raster)
#------------------------------------ For each pixel extract the rotation
rotations_df <- county_polys@polygons %>%
  future_map_dfr(possibly(function(x){
    
    xring <- x@Polygons[[1]]@coords[,1]
    yring <- x@Polygons[[1]]@coords[,2]
    
    pSIMSSiteMaker::historic_rotation_CropScape(xring, yring, years) %>%
      purrr::map_chr( ~ .x[[1]]) %>%
      as.data.frame() %>%
      mutate(years, 
             xcenter =  mean(xring),
             ycenter = mean(yring)
      ) %>%
      `colnames<-`(c("Crop", "Years", "xcenter", "ycenter"))
  }, otherwise = NULL), .progress = TRUE)


Rot_Rasters <- rotations_df %>%
  split(.$Years) %>%
  purrr::map(function(raster.oneyr){
    raster.oneyr$crp<- raster.oneyr$Crop %>%
      tolower() %>%
      purrr::map(~ ifelse(is.null(mapping_guid[[.x]]), 3, mapping_guid[[.x]]))
    
    raster::rasterFromXYZ(raster.oneyr[,c("xcenter","ycenter","crp")])
  }) %>%
  setNames(unique(rotations_df$Years))


names(Rot_Rasters) %>%
  purrr::map(~ plot(Rot_Rasters[[.x]], main=.x))

#saveRDS(Rot_Rasters, "Rot_Rasters.RDS")
#-------------------------------------------------------------------
MyVariable <- Rot_Rasters[[1]]
MyVariable[is.na(MyVariable)] <- -1 # this is fallow

fname <-  "MyCampaign.nc4"
Create_Empty_Campaign(lat=unique(raster::xyFromCell(MyVariable, 1:length(MyVariable))[,2]),
                      lon=unique(raster::xyFromCell(MyVariable, 1:length(MyVariable))[,1]),
                      num_scen=50,
                      filename =fname)


new.values <- purrr::map(seq_along(num_scen),
                         ~Campaign_emptyMatrix("MyCampaign.nc4", raster::rasterToPoints(MyVariable)[,3])[[1]]
)

AddVar_Campaign("MyCampaign.nc4",
                Variable = list(Name=paste0('crid_',2),
                                Unit='Mapping',
                                missingValue=-99,
                                prec="float",
                                longname= "maize,soybean,fallow", 
                                value= new.values
                )
)


Inspect_Camp("MyCampaign.nc4")

########## Visualize 
library(tmap)

tm_shape(GetCamp_VarMatrix('MyCampaign.nc4','crid_2')$Raster[[1]])+
  tm_raster("X1", palette = RColorBrewer::brewer.pal(3, "Set1"),
            title = "", alpha =0.85, style="cat")  + 
  tm_layout(scale=1.2, bg.color = "white", legend.outside = TRUE)+
  tm_grid()