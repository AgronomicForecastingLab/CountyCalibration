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
plan(multisession)

setwd("/mnt/iccp_storage/Regional_Calibration")

state <- "illinois"
county <- "gallatin"
outdir <- '/YieldOutputs'
sim_name <- paste0(state, "_", county,"_SDA")
sim_path <- paste0(getwd(),outdir,'/',sim_name)
crop_name <- 'maize'
Var <- 'yield'


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


# Yields and Acres
yields <- Obs_yields%>%
  dplyr::select(commodity_desc, statisticcat_desc, unit_desc, county_name, year, Value) %>%
  filter(county_name %in% toupper(county), commodity_desc == "CORN") %>%
  mutate(value_kg_ha = Value * 62.77, 
         year=as.numeric(year)) %>%
  mutate(value_kg_ha_dry = purrr::map2_dbl(commodity_desc, value_kg_ha,  ~ ifelse(.x == 'CORN', .y * 0.845, .y * 0.87))) %>%
  mutate(county_name = tolower(county_name))  


weights <- list.files(sim_path, "ens_weights.RDS", recursive = TRUE, full.names = TRUE) %>%
  future_map_dfr(function(x)
    readRDS(paste0(x))
    ) %>%
  mutate(Year = as.factor(Year), Pixel = as.factor(Pixel)) %>%as.data.frame()%>%
  rename(year = Year,ensemble=Ens)

output <- list.files(sim_path,'_output.RDS',full.names = T, recursive = T)%>%
  future_map_dfr(function(outfile){
    readRDS(outfile)
  })%>%
  mutate(year = as.factor(year))%>%
  left_join(weights, by=c('Pixel','year','ensemble'))%>% 
  mutate(w_yield = yield * ScaledW) %>%
  filter(w_yield!=0)%>%
  group_by(Pixel, year) %>%
  dplyr::summarise(avg_yield = sum(w_yield))%>%
  group_by(year) %>%
  summarise(YieldM = median(avg_yield),
            YieldUL = quantile(avg_yield, probs=0.975),
            YieldLL = quantile(avg_yield, probs=0.025))%>%
  mutate(County = paste0(county),
         Scenario = 'SDA')
  


str(yields%>%mutate(year=as.factor(year)))
AllYield <- output%>%
  left_join(yields%>%mutate(year=as.factor(year)) %>%
              dplyr::select(-commodity_desc, -statisticcat_desc, 
                            -unit_desc), by=c('year', 'County'='county_name'))

head(AllYield)
AllYield%>%
  ggplot(aes(year,YieldM ))+
  geom_pointrange(aes(ymax =YieldUL,  ymin =YieldLL), color='darkred', position = position_dodge(width = 0.2))+
  geom_line(aes(color=Scenario,group=Scenario), color='darkred', position = position_dodge(width = 0.5))+
  geom_point(aes(y= value_kg_ha_dry), size=3) +
  geom_line(aes(y= value_kg_ha_dry,group=1)) +
  theme_bw(base_size = 18)+
  #scale_x_continuous(breaks = seq(2010,2020,1))+
  scale_y_continuous(breaks = seq(0,12000,2000))+
  theme(legend.position = "top")


# Yield comparisons

output <- list.files(sim_path,'_output.RDS',full.names = T, recursive = T)%>%
  future_map_dfr(function(outfile){
    readRDS(outfile)
  })%>%
  mutate(year = as.factor(year))%>%
  left_join(weights, by=c('Pixel','year','ensemble'))%>% 
  mutate(w_yield = yield * ScaledW) %>%
  filter(w_yield!=0)%>%
  group_by(longitude, latitude, Pixel, year) %>%
  dplyr::summarise(avg_yield = sum(w_yield))%>%
  left_join(yields%>%mutate(year=as.factor(year))%>%dplyr::select(year,value_kg_ha_dry))%>%
  mutate(diff = value_kg_ha_dry-avg_yield,
         longitude = round(as.numeric(longitude),2),
         latitude = round(as.numeric(latitude),2))


head(output) 

p1 <- ggplot(data = output, aes(x=longitude,y=latitude,fill=diff))+
  geom_raster()+
  facet_wrap(~year)+theme_bw()+
  scale_fill_viridis_c()+
  labs(title='Gallatin Yield: Observed - Weighed simulated',
       subtitle = 'Note: Observed is just a single value for each year')
p1

ggsave(p1, filename = file.path(getwd(),"Plots", paste0("illinois_",county,'_SDA',"/SDA_yieldEvaluation.png")), 
       height = 6, width = 13)

# Filter divergence

#------- Forecast Analysis

read_FA_SDA <- function(SDA, Siten="ss"){
  SDA %>% discard(~ length(.x)==1) %>%
    purrr::map_dfr(function(x){
      
      x$Forecast$X %>%
        mutate(Site=Siten,
               Type='Forecast')%>%
        `colnames<-`(c(x$Obs$Obs.name[which(colnames(.) %in% x$Obs$Forecast.name)],colnames(.)[(!(colnames(.) %in% x$Obs$Forecast.name))]))%>%
        tidyr::gather(Variable, Value, -c(Site, Type)) %>%
        group_by(Site,Variable, Type) %>%
        summarise(
          Means=mean(Value, na.rm=T),
          Lower=quantile(Value,0.025, na.rm=T),
          Upper = quantile(Value, 0.975,  na.rm = TRUE)
        ) %>%
        bind_rows(
          x$Adj %>%
            apply(2, as.numeric)%>%
            as.data.frame()%>%
            mutate(Site=Siten,
                   Type='Analysis') %>%
            `colnames<-`(c(x$Obs$Obs.name[which(colnames(.) %in% x$Obs$Forecast.name)],colnames(.)[(!(colnames(.) %in% x$Obs$Forecast.name))]))%>%
            tidyr::gather(Variable, Value, -c(Site, Type)) %>%
            group_by(Site,Variable, Type) %>%
            summarise(
              Means=mean(Value, na.rm=T),
              Lower=quantile(Value,0.025, na.rm=T),
              Upper = quantile(Value, 0.975,  na.rm = TRUE)
            )
        ) %>%
        mutate(
          day=x$Obs$Date %>% as.Date()
        )%>%
        mutate(Year=lubridate::year(day),
               mday=format(day, "%d-%m"))%>%filter(Year>2009)
      
    })
}


read_Obs_SDA <- function(SDA, Siten="ss"){
  SDA %>% discard(~ length(.x)==1) %>%
    purrr::map_dfr(function(x){
      
      diag(x$Obs$R) %>%
        sqrt %>%
        as.data.frame() %>%
        `colnames<-`(c('Sd')) %>%
        mutate(Means=x$Obs$Y, 
               day=x$Obs$Date %>% as.Date(),
               Variable= 'ESA_CCI',
               Site=Siten,
               Type='Obs'
        ) %>%
        group_by(Variable, day, Site) %>%
        mutate(Upper=Means+(Sd*1.96),
               Means=Means,
               Lower=Means-(Sd*1.96)) %>%
        dplyr::select(-Sd)%>%
        mutate(Year=lubridate::year(day),
               mday=format(day, "%d-%m"))%>%filter(Year>2009)
      
    })
}

# function to visualize results... need to work on 

list.files(sim_path,'sda.out.RData',recursive = T, full.names = T)[1]%>%
  future_map(function(sdafile){
    load(sdafile)%>%plot_APSIM_DA(Siten = '1151',fname = 'SDA.png')
  })

plot_APSIM_DA <- function(SDA, Siten="ss", fname="SDA.png"){
  
  all <- read_FA_SDA(SDA, Siten)
  
  Obs <- read_Obs_SDA(SDA, 'test')
  
  p <- all %>%
    ggplot(aes(x=day))+
    geom_ribbon(aes(y=Means, ymax=Upper, ymin=Lower, fill=Type), alpha=0.25)+
    geom_line(aes(y=Means, color=Type))+
    geom_point(aes(y=Means, color=Type))+
    geom_pointrange(aes(x=day, ymax=Upper, ymin=Lower, y=Means),alpha=0.5, data=Obs  )+
    facet_wrap(~Year, ncol = 2, scales = "free")+
    theme(legend.position = "top")
  
  #return(list(all,Obs,p))
  ggsave(fname, p, height = 6, width = 13)
  
}

a <- read_FA_SDA(SDA.out,'1156/2169')
b <- read_Obs_SDA(SDA.out,'1156/2169')
c <- plot_APSIM_DA(SDA.out,'1156/2169')
c


