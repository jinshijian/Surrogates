
library(drake)  # 6.1.0
pkgconfig::set_config("drake::strings_in_dots" = "literals")

library(tidyr)
library(ggplot2)
theme_set(theme_bw())
library(lubridate)
library(kableExtra)
library(piecewiseSEM)
library(cowplot)
source('functions.R')
library(dplyr)   # needs to come after MASS above so select() isn't masked

OUTPUT_DIR		<- "outputs/"
DATA_DIR <- 'data'
# Create output and log folders if they do not exist
if(!file.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

#*****************************************************************************************************************
# function
#*****************************************************************************************************************
get_clayOCBD <- function (sdata1, j, var_name) {
  f <- here::here(c("data/S_BULK_DEN.nc4", "data/S_CLAY.nc4", "data/S_OC.nc4"))
  # sdata1 = swc_results
  sdata2 <- nc_open( f[j] )
  var_factor <- c("S_BULK_DEN", "S_CLAY", "S_OC")
  
  # i = 1
  for (i in 1:nrow(sdata1) ) {
    
    # get the lat and lon from sdata1
    target_lat <- sdata1$Lat[i]
    target_lon <- sdata1$Lon[i]
    
    if (!is.na(target_lat) & !is.na(target_lon)) {
      # get the closest lat and lon from koeppen
      
      lat <- ncvar_get(sdata2, "lat")
      ilat <- which.min(abs(lat - target_lat))
      
      lon <- ncvar_get(sdata2, "lon")
      ilon <- which.min(abs(lon - target_lon))
      
      # get the koeppen information
      target_value <- ncvar_get(sdata2, var_factor[j], start = c(ilon, ilat), count = c(1, 1))
      
      sdata1[i, var_name] <- target_value
    }
    
    else {next}
    print(paste0("====================", i))
  }
  return (sdata1)
}



#*****************************************************************************************************************
# drake
#*****************************************************************************************************************
plan = drake_plan(
  # load data
  SRDB_raw = read.csv(file_in(!!file.path(DATA_DIR,"srdb-data.csv"))),
  DGRsD_raw = read.csv(file_in(!!file.path(DATA_DIR, "DGRsD.csv"))),
  GPT = read.csv(file_in(!!file.path(DATA_DIR, "summarized_climate.csv"))),
  
  # data filtration
  SRDB = make_groups_srdb(SRDB_raw),
  DGRsD = make_groups_dgrsd(DGRsD_raw),
  DGRsD_TS = rm_dgrsd_ts_null(DGRsD),
  DGRsD_SWC = rm_dgrsd_swc_null(DGRsD),
  
  # Model statistics for air temperature (Tm) as a surrogate of soil temperature (Ts) in soil respiration (Rs) modeling 
  # We used simple linear regression and polynomial regression to simulate the relationship between Tm vs Rs, Ts vs Rs
  # Model statistics (p, R2, RMSE etc) will be recorded
  # We also calculated several other model evaluation statics (E, d, EF etc) for model comparison
  t_results = t_model_sum(DGRsD_TS), 
  # Precipitation for precipitation (Pm) as a surrogate of soil water content (swc), same logic as above
  # swc_results = pm_model_sum(DGRsD_SWC)
  
  # outputs with residue
  # t_residual_results = t_residual_sum(DGRsD_TS),
  # swc_residual_results = swc_residual_sum(DGRsD_SWC)
)

# rm(list = ls())
# clean(plan)
# make(plan)
# config <- make(plan)
# vis_drake_graph(config)
