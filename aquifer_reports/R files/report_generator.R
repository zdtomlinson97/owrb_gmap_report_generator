#' GMAP report generator
#' 
#' Main code for generating gmap reports for state aquifers from public awqms data, including all
#' relevant statistics and figures. General template for the report is general_aquifer_report.rmd
#' Code also updates an excel sheet with relevant figures so that non-R users can write web content
#' 
#' Written by: Zachary Tomlinson
#' Zachary.Tomlinson@owrb.ok.gov

library(tidyverse)
library(dplyr)
library(ggplot2)
library(DT)
library(lubridate)
library(boot)
library(formattable)
library(knitr)
library(markdown)
library(rmarkdown)
#library(smwrGraphs)
library(devtools)
library(rlist)
library(crosstalk)
library(plotly)
library(wql)
library(knitr)
library(kableExtra)
library(xts)
library(mice)
library(stats)
library(htmltools)
library(webshot)
library(htmlwidgets)
library(openxlsx)
library(xfun)
library(hydrogeo)

source("R files/OKAWQMSWeb.R")
source("R files/AWQMS_processing_functions.R")
source("R files/plotly_piper.R")

# Pull in data from awqms export by parameter

# Make a list of parameters to pull out
params = c("Phosphorus","Inorganic nitrogen (nitrate and nitrite)",
    "Total dissolved solids","Depth, from ground surface to well water level",
    "Ammonia","Alkalinity, total","Bromide","Specific conductance","Chloride",
    "Sulfate","Total hardness","Zinc","Vanadium","Sodium","Silver",
    "Selenium","Potassium","Nickel","Molybdenum","Mercury","Manganese",
    "Magnesium","Lead","Iron","Copper","Cobalt","Chromium",
    "Calcium","Cadmium","Boron","Beryllium","Barium",
    "Arsenic","Antimony","Aluminum","Dissolved oxygen (DO)",
    "pH","Temperature, water", "Fluoride", "Uranium"
)
# Make the web request
GW <- readOKAWQMS(type="results",
    OrganizationIdentifiersCsv="OWRB-GMAP", Characteristic = params)

# Assign aquifer to results
sites=readOKAWQMS(type="sites",OrganizationIdentifiersCsv="OWRB-GMAP")
GW = left_join(GW, sites)

# Create downloadable dataset, remove un-used columns
col_index_remove = vector(mode = "numeric")
for(i in 1:ncol(GW)){
  n_unique = length(unique(GW[,i]))
  if(n_unique == 1){
    col_index_remove = append(col_index_remove, i)
  }
}

# Remove the unused columns
GW_data_for_download = GW[,-col_index_remove]
# Remove asteriscs from results
GW_data_for_download$ResultValue = gsub("\\*", "", GW_data_for_download$ResultValue)
# Replace wordy qualifier with the less than symbol
GW_data_for_download$DetectionCondition[grep("<", GW_data_for_download$ResultValue)] = 
  "Present Below Quantification Limit"

# Round latitude and longitude
GW_data_for_download$LatitudeMeasure = round(GW_data_for_download$LatitudeMeasure, 3)
GW_data_for_download$LongitudeMeasure = round(GW_data_for_download$LongitudeMeasure, 3)

# Obtain sample dates. Any sample parameter would work, pH was chosen
sample_dts = GW_data_for_download[which(GW_data_for_download$CharacteristicName == "pH"),
        c("MonitoringLocationIdentifier", "StartDate")]

sample_dts$Sampled = rep("Yes", nrow(sample_dts))
# Filter such that only sampled dates are present in the chem dataset
GW_chem_for_download = left_join(GW_data_for_download, sample_dts) %>% filter(! is.na(Sampled)) %>% 
  select(-Sampled, -WaterbodyName)

# Pull out water levels specifically for the water level download (still keeps all columns)
GW_disc_wls_download = GW_data_for_download %>% filter(CharacteristicName == "Depth, from ground surface to well water level")

# Fix formatting and filter unneeded things for analysis (see function documentation)
GW2 = awqms_process_calc(GW)

colnames(sample_dts)[1] = "WellID"

# Determine the most recent sample date for each well, list as the most recent sample date
most_recent_sampled = sample_dts %>% group_by(WellID) %>% summarise(ActivityStartDate = max(StartDate))
most_recent_sampled$Sampled = rep("Yes", nrow(most_recent_sampled))

most_recent_sampled$ActivityStartDate = ymd(most_recent_sampled$ActivityStartDate)

GW_chem_tbls = left_join(GW2, most_recent_sampled) %>% filter(! is.na(Sampled)) %>% 
  select(-ProjectIdentifier, -Sampled, -sampleyear)

MCL<-read.csv(file="static_r_data/MaximumLevels_cpy.csv", na="")

pip_comp = read.csv(file="static_r_data/piper_components.csv")
colnames(pip_comp)[1] = "CharacteristicName"

export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2,
                               width_svg=720, height_svg=740){
  w <- as.htmlwidget(f, width = width_svg, height = height_svg)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

######################Make Report#########################################
# Make an aquifer list
aqfrs = unique(GW2$WatershedManagementUnit)
# Read in detection limits
bdl = read.csv("static_r_data/Detection Limit Values.csv")[,-1] # Modified to only take the highest detection limit for each parameter. 

excel_wb = loadWorkbook("static_r_data/website_interpretations.xlsx")

excel_comments = read.xlsx("static_r_data/website_interpretations.xlsx",1)

excel_wb$drawings = list(list())
excel_wb$drawings_rels = list(list())

for(n in 16:length(aqfrs)){
  # Generation_dt
  current_dt = Sys.Date()
  
  # Calculate stats for each aquifer
  GW_sub = subset(GW2, WatershedManagementUnit == aqfrs[n])
  all_stats = aquifer_stat_calc(GW_sub, bdl)
  
  current_aquifer = aqfrs[n]
  
  GW_chem_tbl = subset(GW_chem_tbls, WatershedManagementUnit == aqfrs[n])
  
  write.csv(subset(GW_chem_for_download, WatershedManagementUnit == aqfrs[n]), 
            "R files/aquifer_chem_user_download.csv")
  
  aqfr_use_comments = excel_comments$Description[which(
    excel_comments$Aquifer == aqfrs[n])]
  
  aqfr_trend_comments = excel_comments$Notes_Observations[which(
    excel_comments$Aquifer == aqfrs[n])]
  
  aqfr_tbl1_comments = excel_comments$tbl1_notes_obs[which(
    excel_comments$Aquifer == aqfrs[n])]
  
  aqfr_tbl2_comments = excel_comments$tbl2_notes_obs[which(
    excel_comments$Aquifer == aqfrs[n])]
  
  aqfr_tbl3_comments = excel_comments$tbl3_notes_obs[which(
    excel_comments$Aquifer == aqfrs[n])]
  
  aqfr_piper_comments = excel_comments$piper_notes_obs[which(
    excel_comments$Aquifer == aqfrs[n])]
  
  writeData(excel_wb,sheet = 1, x = aqfrs[n], startCol = 1, startRow = n + 1)
  
  # Add in a map of the aquifer
  files = list.files(path = paste0(getwd(), "/R files"))
  img_file = files[grep(current_aquifer, files)]
  
  # Water Use #########################################
  water_use = read.csv("static_r_data/tortorelli_water_use_2005.csv")
  aqfr_conversion = read.csv("static_r_data/usgs_owrb_aquifer_conversion.csv")
  
  colnames(water_use)[1] = "Aquifer"
  
  usgs_aqfr = aqfr_conversion$use_nm[
    which(aqfr_conversion$WatershedManagementUnit == aqfrs[n])]
  
  # Separate out proper aquifer and turn data to long
  water_use_current = water_use[,c(1:5,8,10)] %>% filter(Aquifer == usgs_aqfr) %>% gather(key = "Use", value = "value", 
    -Aquifer, -subdivision, -full_ref)
  
  if(length(unique(water_use_current$subdivision)) > 1){
    disclaimer = 
    "Note: This aquifer was broken into different regions for water use, which were averaged together for the plot above"
  } else disclaimer = ""
  
  citation = unique(water_use_current$full_ref)
  
  use_plot = plot_ly(water_use_current, labels = ~Use, values = ~value, type = 'pie')
  
  if(n >= 10) use_plot_name = paste0("R files/",n, "use_plot.png") else use_plot_name = 
    paste0("R files/0",n, "use_plot.png")
  
  export(use_plot, file = use_plot_name)
  
  writeData(excel_wb, 1, x = current_aquifer, startCol = 1, startRow = n+1)
  
  insertImage(excel_wb, 1, use_plot_name, startRow = n+1, startCol = 2, height = 4.61, width = 6.46)
  
  # Trends (from continuous sites) #########################################
  cont_wl_trends = read.csv("R files/sen_slopes.csv")[,-1] %>% filter(WatershedManagementUnit == aqfrs[n])
  cont_wl_trends$site_no = as.character(cont_wl_trends$site_no)
  cont_wl_trends$sen_slope = as.numeric(cont_wl_trends$sen_slope)
  cont_wl_trends$p_value = as.numeric(cont_wl_trends$p_value)
  
  if(nrow(cont_wl_trends) > 1){
    new_rid = nrow(cont_wl_trends)+1
    cont_wl_trends[new_rid,1] = "Median"
    cont_wl_trends[new_rid,2] = median(na.omit(cont_wl_trends[,"sen_slope"]))
    cont_wl_trends[new_rid,3] = median(na.omit(cont_wl_trends[,"p_value"]))
  }
  
  cont_wl_trends$p_value = as.character(signif(cont_wl_trends$p_value, 3))
  
  trend_slopes = knitr::kable(cont_wl_trends %>% select(-"aqfr_nm", -"WatershedManagementUnit"), format = "html", 
               align = "l", row.names=NA, col.names = c("Well",
        "Trend Slope (ft/year)", "p value"), digits = 3) %>% 
    kableExtra::kable_styling(full_width = FALSE)
  
  if(n >= 10) ts_name = paste0("R files/",n, "trend_slopes.png") else ts_name = 
    paste0("R files/0",n, "trend_slopes.png")
  
  save_kable(trend_slopes,ts_name)
  
  insertImage(excel_wb, 1, ts_name, startRow = n+1, 
          startCol = 4, height = 0.65, width = 2.84)
  
  # Make drinking water tables ################################################
  GW_dm<- inner_join(all_stats, MCL)
  
  # Primary contaminant level table
  SummaryPrimaryCon <- GW_dm %>%filter(ConLev=="Primary")%>%
    select(CharacteristicName, Aquifer_median, Aquifer_Range, MaxLev, ConResultUnit) 
  
  SummaryPrimaryCon$MaxLev = as.numeric(SummaryPrimaryCon$MaxLev)
  
  
  formattble_table1 <- formattable(SummaryPrimaryCon,align =c("l","l","l","c","c"), col.names =
        c("Parameter", "Median*", "Aquifer Range", "MCL","Unit"),
        list(
          `CharacteristicName` = formatter("span", style = ~ formattable::style(color = "black",font.weight = "bold")),
          `Aquifer_median`= formatter("span", style = ~ formattable::style(color=ifelse(
          `Aquifer_median` > `MaxLev`, "red", "green")),
          ~ icontext(ifelse(`Aquifer_median` >`MaxLev`,"arrow-up", "arrow-down"), paste(Aquifer_median)))))
  if(n >=10) tbl1_file_name = paste0(n, "formattble_table1.png") else tbl1_file_name = 
    paste0("0",n, "formattble_table1.png")
  
  export_formattable(formattble_table1, paste0("R files/",tbl1_file_name))
  
  insertImage(excel_wb, 1, paste0("R files/",tbl1_file_name), startRow = n+1, startCol = 6, height = 3.77, width = 7, unit = "in")
  
  # Secondary contaminant level table
  SummarySecondaryCon <- GW_dm %>%filter(ConLev=="Secondary")%>%
    select(CharacteristicName, Aquifer_median, Aquifer_Range, MaxLev, ConResultUnit) 
  
  SummarySecondaryCon$MaxLev = as.numeric(SummarySecondaryCon$MaxLev)
  
  
  formattble_table2 <- formattable(SummarySecondaryCon,align =c("l","l","l","c","c"), col.names =
    c("Parameter", "Median*", "Aquifer Range", "SMCL","Unit"),
        list(
          `CharacteristicName` = formatter("span", style = ~ formattable::style(color = "black",font.weight = "bold")),
          `Aquifer_median`= formatter("span", style = ~ formattable::style(color=ifelse(
          `Aquifer_median` > `MaxLev`, "red", "green")),
          ~ icontext(ifelse(`Aquifer_median` >`MaxLev`,"arrow-up", "arrow-down"), paste(Aquifer_median)))))
  
  if(n >= 10) tbl2_file_name = paste0(n, "formattble_table2.png") else tbl2_file_name = 
    paste0("0",n, "formattble_table2.png")
  
  export_formattable(formattble_table2, paste0("R files/",tbl2_file_name))
  
  insertImage(excel_wb, 1, paste0("R files/",tbl2_file_name), startRow = n+1, startCol = 8, height = 2.5, width = 7)
  
  # Health advisory contaminant level label
  SummaryHealthCon <- GW_dm %>%filter(Health == "Health")%>%
    select(CharacteristicName, Aquifer_median, Aquifer_Range, HealthValue, HealthUnit) 
  
  SummaryHealthCon$HealthValue = as.numeric(SummaryHealthCon$HealthValue)
  
  formattble_table3 <- formattable(SummaryHealthCon,align =c("l","l","l","c","c"), col.names =
        c("Parameter", "Median*", "Aquifer Range", "Health Advisory","Unit"),
        list(
          `CharacteristicName` = formatter("span", style = ~ formattable::style(color = "black",font.weight = "bold")),
          `Aquifer_median`= formatter("span", style = ~ formattable::style(color=ifelse(
          `Aquifer_median` > `HealthValue`, "red", "green")),
          ~ icontext(ifelse(`Aquifer_median` >`HealthValue`,"arrow-up",
          "arrow-down"),paste(Aquifer_median)))))
  
  if(n >= 10) tbl3_file_name = paste0(n, "formattble_table3.png") else tbl3_file_name = 
    paste0("0",n, "formattble_table3.png")
  
  export_formattable(formattble_table3, paste0("R files/",tbl3_file_name))
  
  insertImage(excel_wb, 1, paste0("R files/",tbl3_file_name), startRow = n+1, startCol = 10, height = 2.5, width = 7)
  
  # Piper plot ##########################################################################
  gw_piper = filter(GW_sub, CharacteristicName %in% c("Calcium", "Magnesium", "Sodium", "Potassium",
                                                   "Chloride", "Sulfate", "Carbonate", "Bicarbonate"))
  
  gw_piper$Result_meq = rep(NA, nrow(gw_piper))  # Make an empty vector to eventually hold converted results
  pip_all = left_join(gw_piper, pip_comp)  # Combine component info with the actual measurements to help with conversions
  
  #Convert to miliequivalents
  pip_all$Result_meq = pip_all$ResultValue / pip_all$FMU * pip_all$Charge
  
  gw_piper_wide = pip_all %>% select(-ResultValue, -FMU, -Charge) %>% spread(key = 
    "CharacteristicName", value = "Result_meq")  # Convert to wide format, 1 parameter per column for USGS function
  
  pip_final = na.omit(gw_piper_wide[,c(1:2,8:15)])  # Subset to only include miliequivalent concentrations and year (for coloring)
  
  pip_final = pip_final[order(pip_final$sampleyear),]  # Order measurements by year
  
  colnames(pip_final) = c("WellID", "SampleYear", "HCO3", "Ca", "CO3", "Cl", "Mg", "K", "Na","SO4") # Rename columns for ggplot function
  
  percents = as.data.frame(toPercent(pip_final))
  percents$observation = c(1:nrow(percents))
  
  piper_data = transform_piper_data(Ca = percents$Ca, Mg = percents$Mg, Cl = percents$Cl, SO4 = percents$SO4, name = percents$obs)
  
  piper_data = left_join(piper_data, percents[,c("WellID", "SampleYear", "observation")])
  
  piper_data = piper_data[order(piper_data$SampleYear),]
  

  piper_data$WellID = as.character(piper_data$WellID)
  
  
  plotly_pip_web = plotly_piper() %>% add_trace (data = piper_data, x = ~x, y = ~y, split = ~SampleYear, marker = list(size = 6)) %>% 
    add_trace(name = "Individual Wells")
  
  for(site in pip_final$WellID){
    if(nrow(subset(pip_final, WellID == site)) > 1){
      plotly_pip_web = plotly_pip_web %>% add_trace(
        data = subset(piper_data, WellID == site), x = ~x, y = ~y, marker = list(symbol = 'circle-open', color = I("black")), visible = "legendonly",
        size = 6.5, name = site)
    }
  }
  if(n >= 10) plotly_pip_name = paste0("R files/",n, "plotly_pip.png") else plotly_pip_name = 
    paste0("R files/0",n, "plotly_pip.png")
  
  export(plotly_pip_web, file = plotly_pip_name)
  
  insertImage(excel_wb, 1, plotly_pip_name, startRow = n+1, startCol = 12, height = 2.5, width = 3.5)
  
  
  rmarkdown::render('R files/general_aquifer_report.Rmd', 
                    html_document(pandoc_args = "--self-contained"),
                    output_file =  paste0(aqfrs[n],"_", Sys.Date(), ".html", sep=''), 
                    output_dir = paste0('Reports_',year(Sys.Date())))
}
saveWorkbook(excel_wb, "static_r_data/website_interpretations.xlsx", overwrite = TRUE)
unlink("R files/*formattble*.png")
unlink("R files/*use_plot.png")
unlink("R files/*trend_slopes.png")
unlink("R files/*plotly_pip.png")
