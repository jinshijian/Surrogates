
#*****************************************************************************************************************
# functions for drake
#*****************************************************************************************************************

# Data processing function for srdb - group biome, ecosystem_type, leaf_habit, and soil_drainage
make_groups_srdb <- function(sdata) {
  sdata %>% 
    mutate(Biome_group = case_when(Biome == "Boreal" ~ "Boreal"
                                   ,Biome == "Temperate" ~ "Temperate"
                                   ,Biome == "Arctic" | Biome == "Polar" ~ "Arctic"
                                   ,Biome == "Tropical" ~ "Tropical"
                                   ,Biome == "Mediterranean" ~ "Mediterranean"
                                   ,Biome == "Subtropical" | Biome == "Subtropic" ~ "Subtropical"
                                   ,TRUE ~ "Not reported")) %>%
    mutate(Ecosystem_group = case_when(Ecosystem_type == "" ~ "Not reported"
                                       , Ecosystem_type == "Bare" | Ecosystem_type == "Bare land" | Ecosystem_type == "Bare soil" ~ "BSV"
                                       , Ecosystem_type == "Desert" ~ "Desert"
                                       , Ecosystem_type %in% c("Forest", "Natural")  ~ "Forest"
                                       , Ecosystem_type %in% c("Garden", "Orchard", "Plantation")  ~ "Plantation"
                                       , Ecosystem_type %in% c("Grassland", "Pasture", "Meadow") ~ "Grassland"
                                       , Ecosystem_type == "Peatland" ~ "Peatland"
                                       , Ecosystem_type == "Savanna" ~ "Savanna"
                                       , Ecosystem_type %in% c("Shrubland", "Woodland")  ~ "Shrubland"
                                       , Ecosystem_type == "Tundra" ~ "Tundra"
                                       , Ecosystem_type == "Urban" ~ "Urban"
                                       , Ecosystem_type == "Wetland" ~ "Wetland"
                                       , TRUE ~ "Agriculture")) %>%
    mutate(Leaf_group = case_when (Leaf_habit == "Deciduous" ~ "Deciduous"
                                   , Leaf_habit %in% c("Evergreen", "Evergreen broadleaf")  ~ "Evergreen"
                                   , Leaf_habit %in% c("Mixed", "Mixed of evergreen and deciduous") ~ "Mixed"
                                   , TRUE ~ "Not reported" )) %>% 
    mutate(Drainage_group = case_when (Soil_drainage == "Dry" ~ "Dry"
                                       , Soil_drainage %in% c("Wet")  ~ "Wet"
                                       , Soil_drainage %in% c("Mixed", "Medium") ~ "Medium"
                                       , TRUE ~ "Not reported")) -> sdata
  
  return(sdata)
}


# Data processing function for DGRsD - group biome, ecosystem_type, leaf_habit, and soil_drainage
make_groups_dgrsd <- function(sdata) {
  # DGRsD data processing
  # Group climate
  sdata %>% mutate(Climate_type = case_when(
    Climate %in% c("Af", "Am", "As", "Aw") ~ "(a) Tropic",
    Climate %in% c("BSh", "BSk", "BWh", "BWk") ~ "(b) Arid",
    Climate %in% c("Dfa", "Dfb", "Dfc", "Dsc", "Dwa", "Dwb", "Dwc", "EF", "ET") ~ "(d) Snow", 
    TRUE ~ "(c) Temperate")) -> sdata
  sdata$Ecosystem_type <- ifelse(is.na(sdata$Ecosystem_type), "Not available", sdata$Ecosystem_type)
  
  return(sdata)
}

# remove rows when soil temperature is null
rm_dgrsd_ts_null <- function (sdata) {
  sdata %>% filter(!is.na(Tsoil) & !is.na(Tm_Del) & !is.na(RS_Norm)) -> sdata
  return(sdata)
}

# remove rows when soil water content is null
rm_dgrsd_swc_null <- function (sdata) {
  sdata %>% filter(SWC_Type == "VWC" | SWC_Type == "GWC") %>% filter (!is.na(SWC) & !is.na(Pm_Del) & !is.na(RS_Norm)) -> sdata
  return(sdata)
}

#*****************************************************************************************************************
# functions for air temperature (Tm) as surrogate of soil temperature (Ts) in Rs modeling
# For each site (using for loop), first plot scatter plot of Tm vs Rs, and Ts vs Rs and print in pdf file
# The using 
#*****************************************************************************************************************

t_model_sum <- function (DGRsD_TS) {
  # test question 1: whether Ts is good surrogate for Ta?
  # plot Tair vs Rs, Ts vs Rs relationship by site and plot at 'SI-1. Tair for Tsoil.pdf'
  pdf( paste(OUTPUT_DIR,'SI-1. Tair for Tsoil.pdf', sep = "/" ), width=8, height=4)
  par( mar=c(2, 0.2, 0.2, 0.2)
       , mai=c(0.6, 0.7, 0.0, 0.1)  # by inches, inner margin
       , omi = c(0.0, 0.1, 0.4, 0.1)  # by inches, outer margin
       , mgp = c(0.5, 0.5, 0) # set distance of axis
       , tcl = 0.4
       , cex.axis = 1.0
       , las = 1
       , mfrow=c(1,2) )
  
  var_study <- unique (DGRsD_TS$Study_number) %>% sort()
  t_results <- data.frame()
  
  # i = 1
  # j = 1
  
  for (i in 1:length(var_study)) {
    sub_study <- subset(DGRsD_TS, Study_number == var_study[i])
    var_site <- unique (sub_study$SiteID)
    
    for (j in 1:length(var_site)) {
      sub_siteID <- subset(sub_study, SiteID == var_site[j])
      # aggregate by month - make sure Tsoil and Tair all both in monthly time scale
      sub_siteID %>% select(Study_number, SiteID, Measure_Month, RS_Norm, Tsoil, Tm_Del) %>% 
        group_by(Study_number, SiteID, Measure_Month) %>%
        summarise_each(funs(mean = mean), RS_Norm,
                  Tsoil,
                  Tm_Del) -> sub_site
      colnames(sub_site) <- c("Study_number", "SiteID", "Measure_Month", "RS_Norm", "Tsoil", "Tm_Del")
      
      if (nrow(sub_site) < 6) {next} else {
        
        # Ts vs Rs: first order lm model
        first_lm <- lm(RS_Norm ~ Tsoil, data = sub_site)
        first_a <- summary(first_lm)$coefficients[1,1] %>% round(6)
        first_b <- summary(first_lm)$coefficients[2,1] %>% round(6)
        p_b <- summary(first_lm)$coefficients[2,4]%>% round(6)
        first_R2 <- summary(first_lm)$adj.r.squared %>% round(6)
        obs <- nrow(sub_site)
        # Rs <- mean(sub_site$RS_Norm)
        # Rs_Ts_lm <- mean(first_lm$fitted.values)
        
        # Yang et al, 2014. An evaluation of the statistical methods for testing the performance of crop models with observed data. Agricultural Systems, 127: 81-89. 
        sub_site %>% mutate(slr_pred = predict(first_lm), slr_resid = residuals(first_lm), slr_S_M = (slr_pred-RS_Norm)) -> sub_site
        slr_E <- round(sum(sub_site$slr_S_M) / obs, 6)
        slr_RMSE <- (sum(sub_site$slr_S_M^2) / obs)^0.5 %>% round(3)
        slr_d <- (1- sum(sub_site$slr_S_M^2)/sum((abs(sub_site$slr_pred - mean(sub_site$RS_Norm)) + abs(sub_site$RS_Norm-mean(sub_site$slr_pred)))^2)) %>% round(3)
        slr_EF <- 1 - sum(sub_site$slr_S_M^2)/sum( (sub_site$RS_Norm - mean(sub_site$RS_Norm))^2 )
        
        # polynomial model
        poly_lm <- lm(RS_Norm ~ Tsoil + I(Tsoil^2), data = sub_site)
        poly_a <- summary(poly_lm)$coefficients[1,1] %>% round(6)
        poly_b <- summary(poly_lm)$coefficients[2,1] %>% round(6)
        poly_c <- summary(poly_lm)$coefficients[3,1] %>% round(6)
        p_poly_b <- summary(poly_lm)$coefficients[2,4] %>% round(6)
        p_c <- summary(poly_lm)$coefficients[3,4] %>% round(6)
        poly_R2 <- summary(poly_lm)$adj.r.squared %>% round(6)
        # Rs_Ts_poly <- mean(poly_lm$fitted.values)
        
        # Yang et al, 2014. An evaluation of the statistical methods for testing the performance of crop models with observed data. Agricultural Systems, 127: 81-89.
        sub_site %>% mutate(poly_pred = predict(poly_lm), poly_resid = residuals(poly_lm), poly_S_M = (poly_pred-RS_Norm)) -> sub_site
        poly_E <- round(sum(sub_site$poly_S_M) / obs, 6)
        poly_RMSE <- (sum(sub_site$poly_S_M^2) / obs)^0.5 %>% round(3)
        poly_d <- (1- sum(sub_site$poly_S_M^2)/sum((abs(sub_site$poly_pred - mean(sub_site$RS_Norm)) + abs(sub_site$RS_Norm-mean(sub_site$poly_pred)))^2)) %>% round(3)
        poly_EF <- 1 - sum(sub_site$poly_S_M^2)/sum( (sub_site$RS_Norm - mean(sub_site$RS_Norm))^2 )
        
        
        # Ts vs Tair: first order lm model
        TsTair_lm <- lm(Tsoil ~ Tm_Del, data = sub_site)
        TsTair_a <- summary(TsTair_lm)$coefficients[1,1] %>% round(6)
        TsTair_b <- summary(TsTair_lm)$coefficients[2,1] %>% round(6)
        p_TsTair_b <- summary(TsTair_lm)$coefficients[2,4]%>% round(6)
        TsTair_R2 <- summary(TsTair_lm)$adj.r.squared %>% round(6)
        
        # plot all study-site Ts vs Rs results
        plot(sub_site$RS_Norm ~ sub_site$Tsoil
             , main = ""
             , xlab = ""
             , ylab = ""
             , pch = 16
             , col = "gray" )
        
        # add SLR curve
        curve( first_a + first_b * x, min(sub_site$Tsoil), max(sub_site$Tsoil), col = "black", lty = 3, lwd = 2, add = T )
        # add polynomial curve, predict y using modp
        curve (poly_a + poly_b * x + poly_c * x^2, min(sub_site$Tsoil), max(sub_site$Tsoil), col = "black", lty = 1, lwd = 2, add = T)
        
        mtext(side = 1, text = expression(Soil~temperature~"("~degree~C~")"), line = 1.75, cex=1, outer = F)
        mtext(side = 2, text = expression(Soil~respiration~"("~g~C~m^{-2}~day^{-1}~")"), line = 2.0, cex=1.0, outer = F, las = 0)
        
        
        # Built statistic models and output statistic parameters ***************************************************
        
        # first order lm model of Tair ~ Rs
        first_Tm <- lm(RS_Norm ~ Tm_Del, data = sub_site)
        first_Tm_a <- summary(first_Tm)$coefficients[1,1] %>% round(6)
        first_Tm_b <- summary(first_Tm)$coefficients[2,1] %>% round(6)
        p_Tm_b <- summary(first_Tm)$coefficients[2,4]%>% round(6)
        first_Tm_R2 <- summary(first_Tm)$adj.r.squared %>% round(6)
        # Rs_Ta_lm <- mean(first_Tm$fitted.values)
        
        # Yang et al, 2014. An evaluation of the statistical methods for testing the performance of crop models with observed data. Agricultural Systems, 127: 81-89. 
        sub_site %>% mutate(slr_Tm_pred = predict(first_Tm), slr_Tm_resid = residuals(first_Tm), slr_Tm_S_M = (slr_Tm_pred-RS_Norm)) -> sub_site
        slr_Tm_E <- round(sum(sub_site$slr_Tm_S_M) / obs, 6)
        slr_Tm_RMSE <- (sum(sub_site$slr_Tm_S_M^2) / obs)^0.5 %>% round(3)
        slr_Tm_d <- (1- sum(sub_site$slr_Tm_S_M^2)/sum((abs(sub_site$slr_Tm_pred - mean(sub_site$RS_Norm)) + abs(sub_site$RS_Norm-mean(sub_site$slr_Tm_pred)))^2)) %>% round(3)
        slr_Tm_EF <- 1 - sum(sub_site$slr_Tm_S_M^2)/sum( (sub_site$RS_Norm - mean(sub_site$RS_Norm))^2 )
        
        # polynomial equation of Tair vs Rs
        # poly_pm <- try(lm(RS_Norm ~ poly(Pm_Del,2) , data = sub_site))
        poly_Tm <- try(lm(RS_Norm ~ Tm_Del + I(Tm_Del^2), data = sub_site))
        poly_parameter_c <- try(summary(poly_Tm)$coefficients[3,1] )
        
        # error handle, sometimes there are no polynomial model can be simulated, then set model parameters to NA
        if (is(poly_Tm, "try-error") | is(poly_parameter_c, "try-error")) {
          poly_Tm_a <- NA
          poly_Tm_b <- NA
          poly_Tm_c <- NA
          p_poly_Tm_b <- NA
          p_pm_c <- NA
          poly_Tm_R2 <- NA 
          
          poly_Tm_E <- NA
          poly_Tm_RMSE <- NA
          poly_Tm_d <- NA
          poly_Tm_EF <- NA } 
        
        else {
          poly_Tm_a <- summary(poly_Tm)$coefficients[1,1] %>% round(6)
          poly_Tm_b <- summary(poly_Tm)$coefficients[2,1] %>% round(6)
          poly_Tm_c <- summary(poly_Tm)$coefficients[3,1] %>% round(6)
          p_poly_Tm_b <- summary(poly_Tm)$coefficients[2,4] %>% round(6)
          p_Tm_c <- summary(poly_Tm)$coefficients[3,4] %>% round(6)
          poly_Tm_R2 <- summary(poly_Tm)$adj.r.squared %>% round(6) 
          
          # Yang et al, 2014. An evaluation of the statistical methods for testing the performance of crop models with observed data. Agricultural Systems, 127: 81-89.
          sub_site %>% mutate(poly_Tm_pred = predict(poly_Tm), poly_resid = residuals(poly_Tm), poly_Tm_S_M = (poly_Tm_pred-RS_Norm)) -> sub_site
          poly_Tm_E <- round(sum(sub_site$poly_Tm_S_M) / obs, 6)
          poly_Tm_RMSE <- (sum(sub_site$poly_Tm_S_M^2) / obs)^0.5 %>% round(3)
          poly_Tm_d <- (1- sum(sub_site$poly_Tm_S_M^2)/sum((abs(sub_site$poly_Tm_pred - mean(sub_site$RS_Norm)) + abs(sub_site$RS_Norm-mean(sub_site$poly_Tm_pred)))^2)) %>% round(3)
          poly_Tm_EF <- 1 - sum(sub_site$poly_Tm_S_M^2)/sum( (sub_site$RS_Norm - mean(sub_site$RS_Norm))^2 )
          
          lm_sum = data.frame(sub_site$Study_number[1], sub_siteID$Measure_Year[1], var_site[j]
                              , first_a, first_b, p_b, first_R2
                              , slr_E,  slr_RMSE, slr_d, slr_EF
                              
                              , poly_a, poly_b, p_poly_b, poly_c, p_c, poly_R2
                              , poly_E, poly_RMSE, poly_d, poly_EF
                              
                              , first_Tm_a, first_Tm_b, p_Tm_b, first_Tm_R2
                              , slr_Tm_E, slr_Tm_RMSE, slr_Tm_d, slr_Tm_EF
                              
                              , poly_Tm_a, poly_Tm_b, p_poly_Tm_b, poly_Tm_c, p_Tm_c, poly_Tm_R2
                              , poly_Tm_E, poly_Tm_RMSE, poly_Tm_d, poly_Tm_EF
                              
                              , TsTair_a, TsTair_b, p_TsTair_b, TsTair_R2
                              , obs )
          
          # output all statistic results to "t_results"
          t_results = rbind(t_results, lm_sum)
          
        }
        
        plot(sub_site$RS_Norm ~ sub_site$Tm_Del
             , main = ""
             , xlab = ""
             , ylab = ""
             , pch = 16
             , col = "gray" )
        
        # add simple linear model and polynomial model curves
        curve( first_Tm_a + first_Tm_b * x, min(sub_site$Tm_Del), max(sub_site$Tm_Del), col = "black", lty = 3, lwd = 2, add = T )
        # add polynomial curve, predict y using modp
        curve (poly_Tm_a + poly_Tm_b * x + poly_Tm_c * x^2, min(sub_site$Tm_Del), max(sub_site$Tm_Del), col = "black", lty = 1, lwd = 2, add = T)
        
        mtext(side = 1, text = expression(Air~temperature~"("~degree~C~")"), line = 1.75, cex=1, outer = F)
        mtext(side = 2, text = expression(Soil~respiration~"("~g~C~m^{-2}~day^{-1}~")"), line = 2.0, cex=1.0, outer = F, las = 0)
        mtext(side = 3, text = paste0("Study=", var_study[i], ", Site=", var_site[j]), line = 0.75, cex=1.0, outer = T, las = 0)
      }
      print(paste0("*****", i, "-----", j))
    }
  }
  
  dev.off()
  return (t_results)
}


#*****************************************************************************************************************
# function for precipitation surrogates
#*****************************************************************************************************************
pm_model_sum <- function (DGRsD_SWC) {
  
  # test question 2: wheterh pm is good surrogate of vwc
  # plot Precipitation vs Rs, SWC vs Rs relationship by site and plot at 'SI-1. Tair for Tsoil.pdf'
  pdf( paste(OUTPUT_DIR,'SI-2. Pm for vwc.pdf', sep = "/" ), width=8, height=4)
  
  par( mar=c(2, 0.2, 0.2, 0.2)
       , mai=c(0.6, 0.7, 0.0, 0.1)  # by inches, inner margin
       , omi = c(0.0, 0.1, 0.4, 0.1)  # by inches, outer margin
       , mgp = c(0.5, 0.5, 0) # set distance of axis
       , tcl = 0.4
       , cex.axis = 1.0
       , las = 1
       , mfrow=c(1,2) )
  
  # plot all study-site vwc vs Rs results
  
  var_study <- unique (DGRsD_SWC$Study_number) %>% sort()
  results <- data.frame()
  
  for (i in 1:length(var_study)) {
    # for (i in 3) {
    sub_study <- subset(DGRsD_SWC, Study_number == var_study[i])
    var_site <- unique (sub_study$SiteID)
    
    for (j in 1:length(var_site)) {
      
      sub_siteID <- subset(sub_study, SiteID == var_site[j])
      var_swc <- sub_siteID$SWC_Type[1]
      
      # aggregate by month - make sure Tsoil and Tair all both in monthly time scale
      sub_siteID %>% select(Study_number, SiteID, Measure_Month, RS_Norm, SWC, Pm_Del) %>% 
        group_by(Study_number, SiteID, Measure_Month) %>% 
        summarise_each (funs(mean = mean), RS_Norm,
                  SWC,
                  Pm_Del) -> sub_site
      
      colnames(sub_site) <- c("Study_number", "SiteID", "Measure_Month", "RS_Norm", "SWC", "Pm_Del")
      
      if (nrow(sub_site) < 6) {next} else {
        
        # swc vs Rs: simple linear regression
        first_lm <- lm(RS_Norm ~ SWC, data = sub_site)
        first_a <- summary(first_lm)$coefficients[1,1] %>% round(6)
        first_b <- summary(first_lm)$coefficients[2,1] %>% round(6)
        p_b <- summary(first_lm)$coefficients[2,4]%>% round(6)
        first_R2 <- summary(first_lm)$adj.r.squared %>% round(6)
        obs <- nrow(sub_site)
        
        # swc vs Rs
        # Yang et al, 2014. An evaluation of the statistical methods for testing the performance of crop models with observed data. Agricultural Systems, 127: 81-89. 
        sub_site %>% mutate(slr_pred = predict(first_lm), slr_resid = residuals(first_lm), slr_S_M = (slr_pred-RS_Norm)) -> sub_site
        slr_E <- round(sum(sub_site$slr_S_M) / obs, 6)
        slr_RMSE <- (sum(sub_site$slr_S_M^2) / obs)^0.5 %>% round(3)
        slr_d <- (1- sum(sub_site$slr_S_M^2)/sum((abs(sub_site$slr_pred - mean(sub_site$RS_Norm)) + abs(sub_site$RS_Norm-mean(sub_site$slr_pred)))^2)) %>% round(3)
        slr_EF <- 1 - sum(sub_site$slr_S_M^2)/sum( (sub_site$RS_Norm - mean(sub_site$RS_Norm))^2 )
          
        # swc vs Rs: polynomial model
        poly_lm <- lm(RS_Norm ~ SWC + I(SWC^2), data = sub_site)
        poly_a <- summary(poly_lm)$coefficients[1,1] %>% round(6)
        poly_b <- summary(poly_lm)$coefficients[2,1] %>% round(6)
        poly_c <- summary(poly_lm)$coefficients[3,1] %>% round(6)
        p_poly_b <- summary(poly_lm)$coefficients[2,4] %>% round(6)
        p_c <- summary(poly_lm)$coefficients[3,4] %>% round(6)
        poly_R2 <- summary(poly_lm)$adj.r.squared %>% round(6)
        
        # Yang et al, 2014. An evaluation of the statistical methods for testing the performance of crop models with observed data. Agricultural Systems, 127: 81-89.
        sub_site %>% mutate(poly_pred = predict(poly_lm), poly_resid = residuals(poly_lm), poly_S_M = (poly_pred-RS_Norm)) -> sub_site
        poly_E <- round(sum(sub_site$poly_S_M) / obs, 6)
        poly_RMSE <- (sum(sub_site$poly_S_M^2) / obs)^0.5 %>% round(3)
        poly_d <- (1- sum(sub_site$poly_S_M^2)/sum((abs(sub_site$poly_pred - mean(sub_site$RS_Norm)) + abs(sub_site$RS_Norm-mean(sub_site$poly_pred)))^2)) %>% round(3)
        poly_EF <- 1 - sum(sub_site$poly_S_M^2)/sum( (sub_site$RS_Norm - mean(sub_site$RS_Norm))^2 )
        
        # SWC vs Pm: first order lm model
        SWCPm_lm <- lm(SWC ~ Pm_Del, data = sub_site)
        SWCPm_a <- summary(SWCPm_lm)$coefficients[1,1] %>% round(6)
        SWCPm_b <- summary(SWCPm_lm)$coefficients[2,1] %>% round(6)
        p_SWCPm_b <- summary(SWCPm_lm)$coefficients[2,4]%>% round(6)
        SWCPm_R2 <- summary(SWCPm_lm)$adj.r.squared %>% round(6)
        
        # Add SWC vs Rs scatter plot dots
        plot(sub_site$RS_Norm ~ sub_site$SWC
             , main = ""
             , xlab = ""
             , ylab = ""
             , pch = 16
             , col = "gray" )
        
        mtext(side = 1, text = paste0( var_swc, " ( % )" ), line = 1.75, cex=1, outer = F)
        mtext(side = 2, text = expression(Soil~respiration~"("~g~C~m^{-2}~day^{-1}~")"), line = 2.0, cex=1.0, outer = F, las = 0)
        
        # add SLR curve
        curve( first_a + first_b * x, min(sub_site$SWC), max(sub_site$SWC), col = "black", lty = 3, lwd = 2, add = T )
        
        # add polynomial curve, predict y using modp
        curve (poly_a + poly_b * x + poly_c * x^2, min(sub_site$SWC), max(sub_site$SWC), col = "black", lty = 1, lwd = 2, add = T)
        
        # https://stats.stackexchange.com/questions/95939/how-to-interpret-coefficients-from-a-polynomial-model-fit
        # calculate the new x values using predict.poly()
        # x_poly <- stats:::predict.poly(object = poly(x,2), newdata = 23.4)
        # coef(poly_lm)[1] + coef(poly_lm)[2] * x_poly[1] + coef(poly_lm)[3] * x_poly[2]
        
        # Compare poly with I()
        # summary(lm(RS_Norm ~ SWC + I(SWC^2), data = sub_site)) 
        # -6.84 + 0.43*23.4 + -0.0054*23.4^2
        
        # plot all study-site pm vs Rs results *********************************************************************
        
        # first order lm model of Pm ~ Rs
        first_pm <- lm(RS_Norm ~ Pm_Del, data = sub_site)
        first_pm_a <- summary(first_pm)$coefficients[1,1] %>% round(6)
        first_pm_b <- summary(first_pm)$coefficients[2,1] %>% round(6)
        p_pm_b <- summary(first_pm)$coefficients[2,4]%>% round(6)
        first_pm_R2 <- summary(first_pm)$adj.r.squared %>% round(6)
        
        # Yang et al, 2014. An evaluation of the statistical methods for testing the performance of crop models with observed data. Agricultural Systems, 127: 81-89. 
        sub_site %>% mutate(slr_pm_pred = predict(first_pm), slr_pm_resid = residuals(first_pm), slr_pm_S_M = (slr_pm_pred-RS_Norm)) -> sub_site
        slr_pm_E <- round(sum(sub_site$slr_pm_S_M) / obs, 6)
        slr_pm_RMSE <- (sum(sub_site$slr_pm_S_M^2) / obs)^0.5 %>% round(3)
        slr_pm_d <- (1- sum(sub_site$slr_pm_S_M^2)/sum((abs(sub_site$slr_pm_pred - mean(sub_site$RS_Norm)) + abs(sub_site$RS_Norm-mean(sub_site$slr_pm_pred)))^2)) %>% round(3)
        slr_pm_EF <- 1 - sum(sub_site$slr_pm_S_M^2)/sum( (sub_site$RS_Norm - mean(sub_site$RS_Norm))^2 )
        
        # polynomial equation of Pm vs Rs
        # poly_pm <- try(lm(RS_Norm ~ poly(Pm_Del,2) , data = sub_site))
        poly_pm <- try(lm(RS_Norm ~ Pm_Del + I(Pm_Del^2), data = sub_site))
        poly_parameter_c <- try(summary(poly_pm)$coefficients[3,1] )
        
        if (is(poly_pm, "try-error") | is(poly_parameter_c, "try-error")) {
          poly_pm_a <- NA
          poly_pm_b <- NA
          poly_pm_c <- NA
          p_poly_pm_b <- NA
          p_pm_c <- NA
          poly_pm_R2 <- NA 
          
          poly_pm_E <- NA
          poly_pm_RMSE <- NA
          poly_pm_d <- NA
          poly_pm_EF <- NA  } 
        
        else {
          poly_pm_a <- summary(poly_pm)$coefficients[1,1] %>% round(6)
          poly_pm_b <- summary(poly_pm)$coefficients[2,1] %>% round(6)
          poly_pm_c <- summary(poly_pm)$coefficients[3,1] %>% round(6)
          p_poly_pm_b <- summary(poly_pm)$coefficients[2,4] %>% round(6)
          p_pm_c <- summary(poly_pm)$coefficients[3,4] %>% round(6)
          poly_pm_R2 <- summary(poly_pm)$adj.r.squared %>% round(6) 
          
          # Yang et al, 2014. An evaluation of the statistical methods for testing the performance of crop models with observed data. Agricultural Systems, 127: 81-89.
          sub_site %>% mutate(poly_pm_pred = predict(poly_pm), poly_resid = residuals(poly_pm), poly_pm_S_M = (poly_pm_pred-RS_Norm)) -> sub_site
          poly_pm_E <- round(sum(sub_site$poly_pm_S_M) / obs, 6)
          poly_pm_RMSE <- (sum(sub_site$poly_pm_S_M^2) / obs)^0.5 %>% round(3)
          poly_pm_d <- (1- sum(sub_site$poly_pm_S_M^2)/sum((abs(sub_site$poly_pm_pred - mean(sub_site$RS_Norm)) + abs(sub_site$RS_Norm-mean(sub_site$poly_pm_pred)))^2)) %>% round(3)
          poly_pm_EF <- 1 - sum(sub_site$poly_pm_S_M^2)/sum( (sub_site$RS_Norm - mean(sub_site$RS_Norm))^2 )
          
          # put results together and hold it at results
          lm_sum = data.frame(sub_site$Study_number[1], sub_siteID$Measure_Year[1], var_site[j], var_swc
                              , first_a, first_b, p_b, first_R2
                              , slr_E,  slr_RMSE, slr_d, slr_EF
                              
                              , poly_a, poly_b, p_poly_b, poly_c, p_c, poly_R2
                              , poly_E, poly_RMSE, poly_d, poly_EF
                              
                              , first_pm_a, first_pm_b, p_pm_b, first_pm_R2
                              , slr_pm_E, slr_pm_RMSE, slr_pm_d, slr_pm_EF
                              
                              , poly_pm_a, poly_pm_b, p_poly_pm_b, poly_pm_c, p_pm_c, poly_pm_R2
                              , poly_pm_E, poly_pm_RMSE, poly_pm_d, poly_pm_EF
                              
                              , SWCPm_a, SWCPm_b, p_SWCPm_b, SWCPm_R2
                              
                              , obs)
          
          results = rbind(results, lm_sum)
          
        }
        
        plot(sub_site$RS_Norm ~ sub_site$Pm_Del
             , main = ""
             , xlab = ""
             , ylab = ""
             , pch = 16
             , col = "gray"
        )
        
        # add simple linear regression, polynomial regression curve
        curve( first_pm_a + first_pm_b * x, min(sub_site$Pm_Del), max(sub_site$Pm_Del), col = "black", lty = 3, lwd = 2, add = T )
        # add polynomial curve, predict y using modp
        if (is.na(poly_pm_c)) {next} else {
          curve (poly_pm_a + poly_pm_b * x + poly_pm_c * x^2, min(sub_site$Pm_Del), max(sub_site$Pm_Del), col = "black", lty = 1, lwd = 2, add = T)
        }
        
        mtext(side = 1, text = expression( Pm~"( mm )" ), line = 1.75, cex=1.0, outer = F)
        mtext(side = 2, text = expression(Soil~respiration~"("~g~C~m^{-2}~day^{-1}~")"), line = 2.0, cex=1.0, outer = F, las = 0)
        mtext(side = 3, text = paste0("Study=", var_study[i], ", Site=", var_site[j]), line = 0.75, cex=1.0, outer = T, las = 0)
      }
      print(paste0("*****", i, "-----", j))
    }
  }
  
  dev.off()
  return (results)
}

# function: group R2 difference
# comparing R2 difference and seperate it into three groups: (g1) R2 difference < -0.1, (g2) 0.1 > R2 difference > -0.1, (g3) R2 difference > 0.1
set_r2_dif_group <- function (low, high, sdata) {
  
  sdata %>% mutate(Dif_group = case_when( Diff < low ~ "Low",
                                          Diff >= low & Diff <= high ~ "No",
                                          TRUE ~ "High")  ) -> sdata
  return (sdata)
}

#*****************************************************************************************************************
# Other functions
#*****************************************************************************************************************

# Plot precipitation coverage of samples compare with global mean precipitation
model_p_plot <- function () {
  # plot out the proportion of p<0.05 and p>0.05 for first order model
  blank_theme <- theme_minimal()+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank(),
      plot.title=element_text(size=14, face="bold")  )
  
  # swc first order model
  swc_results %>% mutate(p_group = case_when(  p_b > 0.05 ~ "B005",  TRUE ~ "S005")) %>% select(p_group) %>% count(p_group) %>% 
    ggplot(aes(x="", y=n, fill=p_group))+
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0) + blank_theme +
    theme(axis.text.x=element_blank()) +
    geom_text(aes(y = n/2 + c(0, cumsum(n)[-length(n)]), 
                  label = c("44.34%", "55.66%") ), size=5) +
    theme (legend.position = "none") -> plot_swc_lm
  
  # Pm first model
  swc_results %>% mutate(p_group = case_when(  p_pm_b > 0.05 ~ "B005",  TRUE ~ "S005")) %>% select(p_group) %>% count(p_group) %>% 
    ggplot(aes(x="", y=n, fill=p_group))+
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0) + blank_theme +
    theme(axis.text.x=element_blank()) +
    geom_text(aes(y = n/2 + c(0, cumsum(n)[-length(n)]), 
                  label = c("55.42%", "44.58%") ), size=5) +
    theme (legend.position = "none") -> plot_pm_lm 
  
  # SWC polynomial model
  swc_results %>% mutate(p_group = case_when( p_c > 0.05 ~ "B005",  TRUE ~ "S005")) %>% select(p_group) %>% count(p_group) %>% 
    ggplot(aes(x="", y=n, fill=p_group))+
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0) + blank_theme +
    theme(axis.text.x=element_blank()) +
    geom_text(aes(y = c(250,50), 
                  # label = c("44.34%", "55.66%") ), size=5)
                  label = paste0(round((n/424*100),2), "%") ), size=5) +
    theme (legend.position = "none") -> plot_poly_swc
  
  # SWC polynomial model
  swc_results %>% mutate(p_group = case_when( p_pm_c > 0.05 ~ "B005",  TRUE ~ "S005")) %>% select(p_group) %>% count(p_group) %>% 
    ggplot(aes(x="", y=n, fill=p_group))+
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0) + blank_theme +
    theme(axis.text.x=element_blank()) +
    geom_text(aes(y = c(250,50), 
                  # label = c("44.34%", "55.66%") ), size=5)
                  label = paste0(round((n/424*100),2), "%") ), size=5) +
    theme (legend.position = "none") -> plot_poly_pm
  
  plot_grid(plot_swc_lm, plot_pm_lm, plot_poly_swc, plot_poly_pm
            , labels = c("(a) SLR-SWC", "(b) SLR-Pm", "(c) Poly-SWC", "(d) Poly-Pm")
            , vjust = c(1.25), hjust = c(-0.5)
            , ncol = 2)
  
  # ggsave("outputs/Figure-S1.jpg", width = 6, height = 6, dpi = 300, units = "in")
}



# function for temperature surrogate model residual analysis *****************************************************************
t_residual_sum <- function (DGRsD_TS) {
  
  var_study <- unique (DGRsD_TS$Study_number) %>% sort()
  results <- data.frame()
  
  for (i in 1:length(var_study)) {
    # for (i in 3) {
    sub_study <- subset(DGRsD_TS, Study_number == var_study[i])
    var_site <- unique (sub_study$SiteID)
    
    for (j in 1:length(var_site)) {
      
      sub_site <- subset(sub_study, SiteID == var_site[j])
      
      # if less than 6 observations, do not do the model simulation 
      if (nrow(sub_site) < 6) {next} else {
        
        # swc vs Rs: simple linear regression
        first_ts <- lm(RS_Norm ~ Tsoil, data = sub_site)
        poly_ts <- lm(RS_Norm ~ Tsoil + I(Tsoil^2), data = sub_site)
        first_tm <- lm(RS_Norm ~ Tm_Del, data = sub_site)
        poly_tm <- try(lm(RS_Norm ~ Tm_Del + I(Tm_Del^2), data = sub_site))
        
        obs <- nrow(sub_site)
        
        # get residual
        sub_site %>% mutate(slr_pred = predict(first_ts), slr_resid = residuals(first_ts), 
                            poly_pred = predict(poly_ts), poly_resid = residuals(poly_ts),
                            slr_tm_pred = predict(first_tm), slr_tm_resid = residuals(first_tm),
                            poly_tm_pred = predict(poly_tm), poly_tm_resid = residuals(poly_tm) ) -> sub_site
        
        results = rbind(results, sub_site)
        
      }
      print(paste0("*****", i, "-----", j))
    }
  }
  return (results)
}


# function for moisture surrogate model residual analysis *****************************************************************
swc_residual_sum <- function (DGRsD_SWC) {
  
  var_study <- unique (DGRsD_SWC$Study_number) %>% sort()
  results <- data.frame()
  
  for (i in 1:length(var_study)) {
    # for (i in 3) {
    sub_study <- subset(DGRsD_SWC, Study_number == var_study[i])
    var_site <- unique (sub_study$SiteID)
    
    for (j in 1:length(var_site)) {
      
      sub_site <- subset(sub_study, SiteID == var_site[j])
      
      if (nrow(sub_site) < 6) {next} else {
        
        # swc vs Rs: simple linear regression
        first_swc <- lm(RS_Norm ~ SWC, data = sub_site)
        poly_swc <- lm(RS_Norm ~ SWC + I(SWC^2), data = sub_site)
        first_pm <- lm(RS_Norm ~ Pm_Del, data = sub_site)
        poly_pm <- try(lm(RS_Norm ~ Pm_Del + I(Pm_Del^2), data = sub_site))
        
        obs <- nrow(sub_site)
        
        # get residual
        sub_site %>% mutate(slr_pred = predict(first_swc), slr_resid = residuals(first_swc), 
                            poly_pred = predict(poly_swc), poly_resid = residuals(poly_swc),
                            slr_pm_pred = predict(first_pm), slr_pm_resid = residuals(first_pm),
                            poly_pm_pred = predict(poly_pm), poly_pm_resid = residuals(poly_pm) ) -> sub_site
          
        results = rbind(results, sub_site)
       
      }
      print(paste0("*****", i, "-----", j))
    }
  }
  return (results)
}


# function: group R2 difference ********** **********
# comparing R2 difference and seperate it into three groups: (g1) R2 difference < -0.1, (g2) 0.1 > R2 difference > -0.1, (g3) R2 difference > 0.1
set_r2_dif_group <- function (low, high, sdata) {
  
  sdata %>% mutate(Dif_group = case_when( Diff < low ~ "Low",
                                          Diff >= low & Diff <= high ~ "No",
                                          TRUE ~ "High")  ) -> sdata
  return (sdata)
}


# funtion for outlier testing   ********** **********
outlier_test <- function (sdata, x, y){
  m <- lm( y ~ x, data=sdata )
  sdata$standardized_resids <- rstandard( m )
  sdata$cooks_dist <- cooks.distance( m ) # when > 0.5
  sdata$outlier <- ifelse(abs(sdata$standardized_resids) > 2 | sdata$cooks_dist > 0.1, "Yes", "No")
  return(sdata)
}
