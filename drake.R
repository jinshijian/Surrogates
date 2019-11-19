
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


# load data
# SRDB_raw = read.csv(file.path(DATA_DIR,"srdb-data.csv"))
# DGRsD_raw = read.csv(file.path(DATA_DIR, "DGRsD.csv"))
# GPT = read.csv(file.path(DATA_DIR, "summarized_climate.csv"))
# 
# SRDB = make_groups_srdb(SRDB_raw)
# DGRsD = make_groups_dgrsd(DGRsD_raw)
# DGRsD_TS = rm_dgrsd_ts_null(DGRsD)
# DGRsD_SWC = rm_dgrsd_swc_null(DGRsD)


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
  swc_results = pm_model_sum(DGRsD_SWC),
  
  t_residual_results = t_residual_sum(DGRsD_TS),
  swc_residual_results = swc_residual_sum(DGRsD_SWC)
)

# rm(list = ls())
make(plan)
# config <- make(plan)
# vis_drake_graph(config)
