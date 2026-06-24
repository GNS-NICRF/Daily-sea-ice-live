# Script template for general use #

# Last updated YYYY-MM-DD
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
pacman::p_load(tidyverse, dplyr, magrittr, here, openxlsx)

# Notes on setup
# Nothing to see here
proj_dir <- paste0(here(),"/")
raw_data_dir <- paste0(proj_dir,"raw-data/")
proc_data_dir <- paste0(proj_dir,"processed-data/")
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
 
##### Formatting and processing #####

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

# Process area and extent files
format_seaice_sheet(workbook = NSIDC_file,
                    sheet = "Ross-Area-km^2",
                    destination = paste0(proc_data_dir,"ross_area_km2.csv"))
format_seaice_sheet(workbook = NSIDC_file,
                    sheet = "Ross-Extent-km^2",
                    destination = paste0(proc_data_dir,"ross_extent_km2.csv"))

##### Reading in data ####

ross_area_km2 <- read.csv(file = paste0(proc_data_dir,"ross_area_km2.csv"))
ross_extent_km2 <- read.csv(file = paste0(proc_data_dir,"ross_extent_km2.csv"))

##### Plots #####

ggplot(0)


