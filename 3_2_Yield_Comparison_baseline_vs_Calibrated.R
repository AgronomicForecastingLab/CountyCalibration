library(tidyverse)
library(rnassqs)
library(tidyverse)
library(furrr)
plan(multisession)
api_key <- '7B70B1F9-871F-3971-B191-3A84F4AC95B0' # https://quickstats.nass.usda.gov/api
nassqs_auth(key = api_key)

# Set parameters
params <- list(
  commodity_desc = c("CORN","SOYBEANS"),
  domaincat_desc = "NOT SPECIFIED",
  agg_level_desc = "COUNTY",
  state_alpha = "IL", # please make this an argument 
  year = 2010:2020 # please set this based on min max year in the simulated # you man need to read the simulated first
)

Obs_yields <- nassqs_yields(params)

Sim.Yields <- list.dirs("YieldOutputs", recursive = FALSE)%>%
  future_map_dfr(function(ss){

    Allout <- list.dirs(ss, recursive = FALSE) %>%
      map_dfr(~readRDS(file.path(.x, paste0(basename(.x), "_output.RDS")))) %>%
      filter(day >1) %>%
      mutate(Pixel = gsub("__","", Pixel))

    Allout%>%
      filter(yield> 0) %>%
      group_by(year) %>%
      summarise(
        YieldM = median(yield),
        YieldUL = quantile(yield, probs=0.975),
        YieldLL = quantile(yield, probs=0.025)
      ) %>%
      # left_join(Allout %>% ungroup() %>% select(Pixel, lat, lon) %>% distinct(Pixel, .keep_all=TRUE), 
      #           by=c('Pixel')) %>%
      mutate(County =  strsplit(basename(ss), "_")[[1]][2], 
             Scenario =  strsplit(basename(ss), "_")[[1]][3])
    
  }, .progress = TRUE)



# Yields and Acres
yields <- Obs_yields%>%
  dplyr::select(commodity_desc, statisticcat_desc, unit_desc, county_name, year, Value) %>%
  filter(county_name %in% toupper(c(unique(Sim.Yields$County))), commodity_desc == "CORN") %>%
  mutate(value_kg_ha = Value * 62.77, 
         year=as.numeric(year)) %>%
  mutate(value_kg_ha_dry = purrr::map2_dbl(commodity_desc, value_kg_ha,  ~ ifelse(.x == 'CORN', .y * 0.845, .y * 0.87))) %>%
  mutate(county_name = tolower(county_name))  



AllYield <- Sim.Yields %>%
  left_join(yields %>%
              select(-commodity_desc, -statisticcat_desc, 
                     -unit_desc), by=c('year', 'County'='county_name'))


AllYield%>%
  ggplot(aes(year,YieldM ))+
  geom_pointrange(aes(ymax =YieldUL,  ymin =YieldLL, color=Scenario), position = position_dodge(width = 0.25))+
  geom_line(aes(color=Scenario), position = position_dodge(width = 0.5))+
  geom_point(aes(y= value_kg_ha_dry), size=3) +
  geom_line(aes(y= value_kg_ha_dry)) +
  facet_wrap(~ County, ncol=1)+
  theme_gray(base_size = 18)+
  scale_x_continuous(breaks = seq(2010,2020,1))+
  scale_y_continuous(breaks = seq(0,12000,2000))+
  theme(legend.position = "top")
  


AllYield %>%
  split(list(.$County, .$Scenario)) %>%
  map_dfr(~ hydroGOF::gof(.x$YieldM, .x$value_kg_ha_dry) %>%
        t %>% as.data.frame %>%
        select(RMSE, KGE, d, R2) %>%
        mutate(County= unique(.x$County), 
               Scen=  unique(.x$Scenario))
      ) %>%
  gather(Var, Val, -County, -Scen) %>%
  ggplot(aes(County, Val))+
  geom_bar(aes(fill=Scen), stat="identity", position = "dodge")+
  facet_wrap(~ Var, scales = "free")+
  theme(legend.position = "top")

#-------------------------------------------------------------------------------------------

Sim.Yields.wens <- list.dirs("YieldOutputs", recursive = FALSE)[2]%>%
  map_dfr(function(ss){
    
    Allout <- list.dirs(ss, recursive = FALSE) %>%
      map_dfr(~readRDS(file.path(.x, paste0(basename(.x), "_output.RDS")))) %>%
      mutate(Pixel = gsub("__","", Pixel))
    browser()
    
    pix.year <- Allout%>%
      filter(yield> 0) %>%
      group_by(year, Pixel) %>%
      summarise(
        YieldM = mean(yield),
        YieldUL = quantile(yield, probs=0.975),
        YieldLL = quantile(yield, probs=0.025)
      ) %>%
      left_join(Allout %>% ungroup() %>% select(Pixel, lat, lon) %>% distinct(Pixel, .keep_all=TRUE),
                by=c('Pixel')) %>%
      mutate(County =  strsplit(basename(ss), "_")[[1]][2], 
             Scenario =  strsplit(basename(ss), "_")[[1]][3]) %>%
      mutate(Pixel=gsub("Validation_","", Pixel))
    
    pars <- file.path(paste0('Outputs/illinois_', gsub("illinois|Validation|base|_", "", basename(ss))), "Optim_results.RDS") %>%
      readRDS() %>%
      {.[[5]]} %>%
      mutate(
        pdate = as.numeric(pdate),
        tt_emerg_to_endjuv= as.numeric(tt_emerg_to_endjuv),
        leaf_app_rate1= as.numeric(leaf_app_rate1),
        leaf_init_rate= as.numeric(leaf_init_rate)
      ) %>%
      gather(Year, Yearef, -pixel, -pdate, -tt_emerg_to_endjuv,-leaf_app_rate1,-leaf_init_rate) %>%
      mutate(Yearef=as.numeric(Yearef)) %>%
      split(.$Year) %>%
      map_dfr(~ .x %>% mutate(pdate= pdate +Yearef ) )
    
    ss <- pix.year %>%
      left_join(pars, by=c('Pixel'='pixel'))
  })
