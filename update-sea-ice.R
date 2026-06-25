# Download, parse, and plot NSIDC daily Antarctic sea-ice data #

# Last updated substantially on 2026-06-25
# Matt Harris
# matt.harris@earthsciences.nz
# https://www.github.com/MRPHarris

# Written and run in:
#   Rstudio v2024.12.1
#   R v4.4.0
#   Windows 11 (Enterprise)

##### SETUP #####

## Install/uninstall lines
# detach('package:ecmwfr', unload = TRUE)
# installr::uninstall.packages('ecmwfr')
# install.packages('ecmwfr')
# devtools::install_github('MRPHarris/PACKAGENAME')

## Required packages
library(pacman)
pacman::p_load(tidyverse, dplyr, magrittr, here, openxlsx, ggtext,ggh4x)

# Notes on setup
# Nothing to see here
proj_dir <- paste0(here(),"/")
raw_data_dir <- paste0(proj_dir,"raw-data/")
proc_data_dir <- paste0(proj_dir,"processed-data/")
plots_dir <- paste0(proj_dir,"plots/")
#setwd(wd)

# Source functions from a package
# scripts_dir <- paste0(proj_dir,"R/")
# invisible(sapply(list.files(scripts_dir, full.names = TRUE),
#                  function(x){source(x)}))

##### Obtain recent sea-ice data #####

## Get the current sea-ice file
target_URL <- "https://noaadata.apps.nsidc.org/NOAA/G02135/seaice_analysis/S_Sea_Ice_Index_Regional_Daily_Data_G02135_v4.0.xlsx"
NSIDC_file <- paste0(raw_data_dir,"NSIDC_S_Sea_Ice_Index_Regional_Daily_Data_G02135_v4.0.xlsx")
download.file(url = target_URL, destfile = NSIDC_file, mode = "wb")

# Format, save as processed file.
# ross_area_km2 <- readWorkbook(NSIDC_file, sheet = "Ross-Area-km^2")
# # ross_extent_km2 <- readWorkbook(NSIDC_file, sheet = "Ross-Extent-km^2")
 
##### Formatting and processing incl. totals, climatologies, anomalies #####

format_seaice_sheet <- function(workbook, sheet, destination){
  # data = ross_area_km2
  # destination = paste0(proc_data_dir,"ross_area_km2.csv")
  # get workbook
  data = readWorkbook(workbook, sheet = sheet)
  # Add sequential days
  data_proc <- data %>%
    mutate(day_seq = seq(1,366,1)) %>%
    select(-c(month,day)) %>% relocate(day_seq) %>%
    pivot_longer(cols = c(2:ncol(.))) %>% rename(year = name) %>%
    mutate(across(everything(), as.numeric)) %>%
    mutate(decimal_year = year + (day_seq * 1/366)) %>%
    relocate(decimal_year) %>%
    arrange(decimal_year)
  # Anomaly 
  # Calculate climatology
  climtology <- data %>% 
    select(-c(1,2)) %>%
    rowMeans(.,na.rm = T)
  data_proc <- data_proc %>%
    mutate(climatology = rep(climtology, times = ((max(data_proc$year) - min(data_proc$year)) + 1))) %>%
    mutate(anomaly = value - climatology)
  if(!is.null(destination)){
    if(!is.character(destination)){
      stop('destination must be either NULL or a character vector containing a path to a file destination.')
    } else {
      write.csv(data_proc, file = destination, row.names = F)
    }
  }
}

# Get sheet names
sheetNames <- getSheetNames(NSIDC_file)
sheetNames <- sheetNames[!sheetNames %in% "Documentation"]
# explicit loop
for(s in seq_along(sheetNames)){
  sname <- sheetNames[s]
  message(sname)
  update_sname <- str_replace_all(sname,"\\^","")
  update_sname <- str_replace_all(update_sname,"-","_")
  format_seaice_sheet(workbook = NSIDC_file,
                      sheet = sname,
                      destination = paste0(proc_data_dir,paste0(update_sname,".csv")))
}

# TOTAL AREA
area_files <- list.files(proc_data_dir,"_Area_", full.names = T)
area_files_short <- list.files(proc_data_dir,"_Area_", full.names = F)
file_list <- vector('list', length = length(area_files)) %>% 'names<-'(c(area_files_short))
for(f in seq_along(area_files)){
  file_list[[f]] <- read.csv(area_files[f], header = T) %>% 
    as.data.frame() %>% dplyr::select(value)
}
total_area <- rlist::list.cbind(file_list) %>%
  rowSums(., na.rm = F) %>% as.data.frame() %>% 'colnames<-'(c('value')) %>%
  bind_cols(.,read.csv(area_files[f], header = T) %>% 
              as.data.frame() %>% dplyr::select(-c(value,climatology,anomaly))) %>%
  relocate(value, .after = year)
climtology <- total_area %>% 
  select(-c(1)) %>% 
  pivot_wider(., names_from = year) %>%
  select(-c(1)) %>%
  rowMeans(.,na.rm = T)
total_area <- total_area %>%
  mutate(climatology = rep(climtology, times = ((max(total_area$year) - min(total_area$year)) + 1))) %>%
  mutate(anomaly = value - climatology)
write.csv(total_area, file = paste0(proc_data_dir,"Total_Area_km2.csv"), row.names = F)

# TOTAL EXTENT 
extent_files <- list.files(proc_data_dir,"_Extent_", full.names = T)
extent_files_short <- list.files(proc_data_dir,"_Extent_", full.names = F)
file_list <- vector('list', length = length(extent_files)) %>% 'names<-'(c(extent_files_short))
for(f in seq_along(extent_files)){
  file_list[[f]] <- read.csv(extent_files[f], header = T) %>% 
    as.data.frame() %>% dplyr::select(value)
}
total_extent <- rlist::list.cbind(file_list) %>%
  rowSums(., na.rm = F) %>% as.data.frame() %>% 'colnames<-'(c('value')) %>%
  bind_cols(.,read.csv(extent_files[f], header = T) %>% 
              as.data.frame() %>% dplyr::select(-c(value,climatology,anomaly))) %>%
  relocate(value, .after = year)
climtology <- total_extent %>% 
  select(-c(1)) %>% 
  pivot_wider(., names_from = year) %>%
  select(-c(1)) %>%
  rowMeans(.,na.rm = T)
total_extent <- total_extent %>%
  mutate(climatology = rep(climtology, times = ((max(total_extent$year) - min(total_extent$year)) + 1))) %>%
  mutate(anomaly = value - climatology)
write.csv(total_extent, file = paste0(proc_data_dir,"Total_Extent_km2.csv"), row.names = F)

# Create metadata file with some info on when all this was last created
textlines <- c("NSIDC sea ice area and extents by region",
               "Data from: https://noaadata.apps.nsidc.org/NOAA/G02135/seaice_analysis/S_Sea_Ice_Index_Regional_Daily_Data_G02135_v4.0.xlsx",
               "Individual files created using https://github.com/GNS-NICRF/Daily-sea-ice-live",
               paste0("Last updated: ",Sys.time()))
writeLines(textlines, paste0(proc_data_dir,"data_readme.txt"))

##### Plots #####

# source themes
source(paste0(proj_dir,"scripts/themes.R"))

# list files: area
flist <- list.files(proc_data_dir,".csv", full.names = T)
flist_short <- list.files(proc_data_dir,".csv", full.names = F)

for(f in seq_along(flist)){
  this_file <- read.csv(flist[[f]])
  name <- flist_short[f] %>% str_remove_all(., ".csv") %>% str_replace_all(., "_"," ")
  # Get sector name
  sector <- ifelse(str_detect(name, "Area"),
                yes = str_sub(name, start = 1, end = gregexpr(pattern ='Area',name)[[1]] - 2),
                no = ifelse(str_detect(name,"Extent"),
                            yes = str_sub(name, start = 1, end = gregexpr(pattern ='Extent',name)[[1]] - 2),
                            no = stop("Unknown file type: .csv filenames in processed data dir must contain the string 'Area' or 'Extent'")))
  # Some sector tweaks
  if(sector == "Bell Amundsen"){
    sector = "Amundsen-Bellingshausen"
  } else if(sector == "Total"){
    sector = "Antarctic"
  }
  message("Producing plots for ",sector)
  # one filled, one not filled 
  reg_plot <- ggplot() + 
    geom_vline(xintercept = seq(1978,2026,1), linetype = 'dashed', colour = 'grey60', alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = 'solid', colour = 'grey60') +
    geom_line(data = this_file, aes(x = decimal_year, y = anomaly/1000000)) +
    # stat_difference(data = total_area_km2, aes(x = decimal_year, y = anomaly/1000000, ymin = 0, ymax = anomaly/1000000)) +
    scale_x_continuous(expand = c(0,0), breaks = seq(1980, 2025, 5)) +
    scale_y_continuous() +
    scale_fill_manual(values = c('Royal Blue','Fire Brick 1')) +
    theme_general(textsize_vsmall * 0.75) +
    theme(legend.position = 'none') #+
    # labs(x = "Year", y = "Area anomaly (10<sup>6</sup> km<sup>2</sup>)", title = 'Antarctic sea-ice area anomaly')
  filled_plot  <- ggplot() + 
    geom_vline(xintercept = seq(1978,2026,1), linetype = 'dashed', colour = 'grey60', alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = 'solid', colour = 'grey60') +
    geom_line(data = this_file, aes(x = decimal_year, y = anomaly/1000000)) +
    stat_difference(data = this_file, aes(x = decimal_year, y = anomaly/1000000, ymin = 0, ymax = anomaly/1000000)) +
    scale_x_continuous(expand = c(0,0), breaks = seq(1980, 2025, 5)) +
    scale_y_continuous() +
    scale_fill_manual(values = c('Royal Blue','Fire Brick 1')) +
    theme_general(textsize_vsmall * 0.75) +
    theme(legend.position = 'none') #+
    # labs(x = "Year", y = "Area anomaly (10<sup>6</sup> km<sup>2</sup>)", title = 'Antarctic sea-ice area anomaly')
  if(str_detect(name, "Area")){
    unit = 'Area'
    reg_plot <- reg_plot + 
      labs(x = "Year", y = "Area anomaly (10<sup>6</sup> km<sup>2</sup>)", title = paste0(sector,' sea-ice area anomaly'))
    filled_plot <- filled_plot + 
        labs(x = "Year", y = "Area anomaly (10<sup>6</sup> km<sup>2</sup>)", title = paste0(sector,' sea-ice area anomaly'))
  } else if(str_detect(name, "Extent")){
    unit = 'Extent'
    reg_plot <- reg_plot + 
      labs(x = "Year", y = "Extent anomaly (10<sup>6</sup> km<sup>2</sup>)", title = paste0(sector,' sea-ice extent anomaly'))
    filled_plot <- filled_plot + 
      labs(x = "Year", y = "Extent anomaly (10<sup>6</sup> km<sup>2</sup>)", title = paste0(sector,' sea-ice extent anomaly'))
  }
  ggsave(paste0(plots_dir,gsub(" ","-",name),".png"), plot = reg_plot, width = 24, height = 10, units = "cm", dpi = 300, bg = "white")
  ggsave(paste0(plots_dir,gsub(" ","-",name),"-shaded",".png"), plot = filled_plot, width = 24, height = 10, units = "cm", dpi = 300, bg = "white")
}
