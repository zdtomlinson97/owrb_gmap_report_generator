library(lubridate)
library(tidyverse)
library(dplyr)
############################ RAW AWQMS processing function ######################################################
# This function calculates additional parameters (SAR, carbonate, bicarbonate) and reformats raw awqms data
# Below reporting limit values are replaced with the reporting limit and a < sign is used for the detection condition
awqms_process_calc = function(GW){

  # Reformat dates
  # Assign a sample year to all data
  GW$ActivityStartDate = ymd(GW$StartDate)
  GW$sampleyear<-year(GW$ActivityStartDate)

  # take the main dataframe and assigns the reporting limit to the result value
  GW$ResultValue = gsub("<", "", GW$ResultValue)
  
  # Remove asterisks from results
  GW$ResultValue = gsub("\\*", "", GW$ResultValue)

  #Removing all quality control samples
  GW<-GW[!grepl("Quality", GW$ActivityType),]


  #Changes present below quantification limit to <
  GW$DetectionCondition<- (gsub("Not Detected at Reporting Limit", "<", GW$DetectionCondition))
  GW$DetectionCondition<- (gsub("Present Below Quantification Limit", "<", GW$DetectionCondition))

  # need to add in info from any attached files such as mcl and smcl
  # Rename field
  colnames(GW)[which(colnames(GW) == "MonitoringLocationIdentifier")] = "WellID"

  #Filter out hydrosleeve results
  GW = subset(GW, SampleCollectionMethodIdentifier != "Hydrasleeve")

  # Subset parameters
  GW2 <- GW %>% select(WellID, sampleyear, ActivityStartDate,CharacteristicName, DetectionCondition, ResultValue, ResultUnit, ProjectIdentifier, WatershedManagementUnit)

  #Changes name of nitrate/nitrite and ammonia
  GW2$CharacteristicName <- gsub("Inorganic nitrogen \\(nitrate and nitrite\\)", "Nitrate and nitrite as N", GW2$CharacteristicName)
  GW2$CharacteristicName <- gsub("Ammonia","Ammonia as N", GW2$CharacteristicName)

  # Change unit to greek letters
  GW2$ResultUnit <- gsub("ug/l", paste0("\U00B5","g/L"), GW2$ResultUnit)
  GW2$ResultUnit <- gsub("mg/l", "mg/L", GW2$ResultUnit)
  GW2$ResultUnit <- gsub("uS/cm",paste0("\U00B5","S/cm"), GW2$ResultUnit)

  # Reclassify to numeric
  GW2$ResultValue = as.numeric(as.character(GW2$ResultValue))

  GW2 = na.omit(GW2)
  
  GW2_a = GW2
  # Average repeat measurements 
  row.names(GW2_a) = seq(1, nrow(GW2_a))
  
  unique_rows = as.numeric(row.names(unique(select(GW2_a, ActivityStartDate, WellID, CharacteristicName))))
  
  GW2_b = GW2_a[-unique_rows,]
  colnames(GW2_b)[6] = "ResultValue2"
  
  GW2_c = left_join(GW2_a, GW2_b)
  
  GW2_c$ResultValue = apply(cbind(GW2_c$ResultValue,GW2_c$ResultValue2), 1, mean, na.rm = TRUE)
  
  # Remove the other columns used in the averages 
  repeats = which(GW2_c$ResultValue == GW2_c$ResultValue2)
  
  GW2_averaged = unique(GW2_c[-repeats,-10])

  # Invert the data to wide format to calculate additional parameters
  GW_wide = GW2_averaged %>% filter(CharacteristicName %in% c("Temperature, water", 
        "Alkalinity, total", "Specific conductance","Sodium","Calcium","Magnesium", "pH", "WatershedManagementUnit")) %>% # Filter parameters needed for calculations
  select(-ResultUnit)%>% # Get rid of unneeded parameters
  spread(key = "CharacteristicName", value = "ResultValue") # Separate characteristic names row into columns

  # Convert to meq
  GW_wide$Na_meq = GW_wide$Sodium*1/22.9897693
  GW_wide$Ca_meq = GW_wide$Calcium*2/40.08
  GW_wide$Mg_meq = GW_wide$Magnesium*2/24.305
  # Calculate SAR
  GW_wide$SAR = round(GW_wide$Na_meq / sqrt((GW_wide$Mg_meq+GW_wide$Ca_meq)/2),1)

  # Estimate bicarbonate and carbonate
  Temp_C = c(0,5,10,15,20,25,30)
  KHCO3 = c(2.34423E-11, 2.81838E-11, 3.23594E-11, 3.71535E-11, 4.16869E-11, 4.67735E-11, 5.12861E-11)
  Kw = c(1.139E-15, 1.846E-15, 2.92E-15, 4.505E-15, 6.81E-15, 1.01E-14, 1.47E-14)
  A = c(0.4883, 0.4921, 0.496, 0.5, 0.5042, 0.5085, 0.513)
  B = c(0.3241, 0.3249, 0.3258, 0.3262, 0.3273, 0.3281, 0.329)
  fetter_parameters=as.data.frame(cbind(Temp_C, KHCO3, Kw, A, B))

  # Enter an empty bicarbonate vector and carbonate vector
  GW_wide$Bicarbonate = rep(NA, nrow(GW_wide))
  GW_wide$Carbonate = rep(NA, nrow(GW_wide))

  # Calculate the bicarbonate and carbonate for each entry
  # Parameters/formula from https://or.water.usgs.gov/alk/methods.html#advanced
  for(i in 1:nrow(GW_wide)){
    khco3 = approx(x=fetter_parameters$Temp_C, y = fetter_parameters$KHCO3, xout = GW_wide$`Temperature, water`[i])$y
    kw = approx(x=fetter_parameters$Temp_C, y = fetter_parameters$Kw, xout = GW_wide$`Temperature, water`[i])$y
    a = approx(x=fetter_parameters$Temp_C, y = fetter_parameters$A, xout = GW_wide$`Temperature, water`[i])$y
    b = approx(x=fetter_parameters$Temp_C, y = fetter_parameters$B, xout = GW_wide$`Temperature, water`[i])$y
    alk0 = GW_wide$`Alkalinity, total`[i] * (1/100.087) * (2) * (1/1000)
    I = 0.000025 * 0.59 * GW_wide$`Specific conductance`[i]
    gamma_OH = exp(-(a*(-1)^2*sqrt(I))/(1+(3*b*sqrt(I))))
    gamma_H = exp(-(a*(1)^2*sqrt(I))/(1+(9*b*sqrt(I))))
    gamma_HCO3 = exp(-(a*(-1)^2*sqrt(I))/(1+(4*b*sqrt(I))))
    gamma_CO3 = exp(-(a*(-2)^2*sqrt(I))/(1+(5*b*sqrt(I))))
    HCO3_mol_l = (alk0-kw/gamma_OH*10^(GW_wide$pH[i])+10^(-GW_wide$pH[i])/gamma_H)/(1+2*(khco3*gamma_HCO3/gamma_CO3)*10^(GW_wide$pH[i]))
    CO3_mol_l = (alk0-kw/gamma_OH*10^(GW_wide$pH[i])+10^(-GW_wide$pH[i])/gamma_H)/(2+(10^-(GW_wide$pH[i]))/(khco3*gamma_HCO3/gamma_CO3))
    GW_wide$Bicarbonate[i] = round(HCO3_mol_l*1000*61.016,1) # Convert to mg/l
    GW_wide$Carbonate[i] = round(CO3_mol_l*1000*60.01,1) # Convert to mg/l
  }
  
  #Convert conductivity to specific conductance
  #Reference: https://pubs.usgs.gov/tm/09/a6.3/tm9-a6_3.pdf
  GW_wide$`Electrical Conductivity (EC)` = round(GW_wide$`Specific conductance`*
                                                 (1+0.02*(GW_wide$`Temperature, water`-25)),1)

  # Convert back to long format, cut out parameters that have already been calculated
  GW_long = GW_wide %>% select(WellID, sampleyear, ActivityStartDate, DetectionCondition, ProjectIdentifier, WatershedManagementUnit, SAR, 
                             Bicarbonate, Carbonate, `Electrical Conductivity (EC)`) %>%
    gather(key = "CharacteristicName", value = "ResultValue", -WellID, -sampleyear, -ActivityStartDate, 
         -DetectionCondition, -ProjectIdentifier, -WatershedManagementUnit)

  # Add in missing columns
  GW_long$ResultQualifier = rep("", nrow(GW_long))
  GW_long$ResultUnit = rep(NA, nrow(GW_long))
  GW_long$ResultUnit[which(GW_long$CharacteristicName == "SAR")] = "None" #For SAR, put no units
  GW_long$ResultUnit[which(GW_long$CharacteristicName == "Bicarbonate" | GW_long$CharacteristicName == "Carbonate")] = "mg/L" # For bicarbonate/carbonate, put mg/l
  GW_long$ResultUnit[which(GW_long$CharacteristicName == "Electrical Conductivity (EC)")] = paste0("\U00B5","S/cm") # For EC, put uS/cm

  # Move columns into the same order as GW2 columns
  GW_long = GW_long %>% select(colnames(GW2))

  # Combine GW2 with the new calculated parameters
  GW3 = rbind(GW2_averaged, GW_long)
  
  return(GW3)
}

############## Generate summary statistics by aquifer for groundwater data, takes groundwater data and data frame of detection limits#########
aquifer_stat_calc = function(GW, bdl){
  #First, duplicate the rows with values below reporting limit and replace the 
  #duplicate value with 0
  GW_sum = rbind(GW, GW %>% filter(DetectionCondition == "<") %>%
                 mutate(ResultValue = as.numeric(0)))
  # Make result value numeric
  GW_sum$ResultValue = as.numeric(as.character(GW_sum$ResultValue))

  # Summarize the median (with bootstrapping), maximum, and minimum 
  GW_stat = GW_sum %>% group_by(CharacteristicName) %>%
    summarise(Aquifer_min = round(min(na.omit(ResultValue)),2), Aquifer_median = round(boot(ResultValue, median, R = 10000)$t0,2), 
    Aquifer_max = round(max(na.omit(ResultValue)),2))

  bdl$DetectionLimitValue1 = as.numeric(bdl$DetectionLimitValue1)

  #Tie in the detection limits for the minimum and possibly maximum
  GW_stat2 = left_join(GW_stat, bdl, .by = "CharacteristicName")

  GW_stat2$Aquifer_max = as.numeric(GW_stat2$Aquifer_max)
  GW_stat2$Aquifer_min = as.numeric(GW_stat2$Aquifer_min)

  # Set detection limit values for bicarbonate, carbonate and water level
  GW_stat2[which(GW_stat2$CharacteristicName == 
                 "Bicarbonate"),"DetectionLimitValue1"] = 10
  GW_stat2[which(GW_stat2$CharacteristicName == 
                  "Carbonate"),"DetectionLimitValue1"] = 10
  GW_stat2[which(GW_stat2$CharacteristicName == 
      "Depth, from ground surface to well water level"),
         "DetectionLimitValue1"] = 0.03

  # Remove other NA values (all represent unwanted parameters)
  GW_stat2 = na.omit(GW_stat2)
  
  # Replace the numbers with the detection limits.
  # Add in range vector
  GW_stat2$Aquifer_Range = rep(NA, nrow(GW_stat2))
    
  for(i in 1:nrow(GW_stat2)){
    if(as.numeric(GW_stat2$Aquifer_max[i]) <= as.numeric(GW_stat2$DetectionLimitValue1[i])){
      #if(GW_stat2$CharacteristicName[i] != "Chloride"){ # For some reason Chloride gets thrown off 
        GW_stat2$Aquifer_max[i] = paste("<",GW_stat2$DetectionLimitValue1[i])
      #}
    }
    if(as.numeric(GW_stat2$Aquifer_min[i]) <= as.numeric(GW_stat2$DetectionLimitValue1[i])){
      GW_stat2$Aquifer_min[i] = paste("<",GW_stat2$DetectionLimitValue1[i])
    }
    if(GW_stat2$Aquifer_min[i] != GW_stat2$Aquifer_max[i]){
      GW_stat2$Aquifer_Range[i] = paste(GW_stat2$Aquifer_min[i],"-",GW_stat2$Aquifer_max[i])
    } else{GW_stat2$Aquifer_Range[i] = GW_stat2$Aquifer_max[i]}
  }

  GW_stat2 = unique(GW_stat2)

  #Remove the last vector and the min, max vectors, round median
  GW_stat3 = GW_stat2 %>% select(-DetectionLimitValue1,-Aquifer_min,-Aquifer_max)
  GW_stat3$Aquifer_median = round(GW_stat3$Aquifer_median, 2)
    
  # Add data to all data if the loop has ran more than once; else, initiate all data with data
  
  return(GW_stat3)
}