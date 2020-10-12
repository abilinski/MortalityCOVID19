#************************************* DATA PROCESSING ************************************#
#                                                                                          #
#                                                                                          #
# This file details several data-processing steps taken prior to main analysis.            #
#******************************************************************************************#

#### SETUP ####
  # main directory
  wd = "~/Dropbox/Mortality/Public code/"
  setwd(wd)
  
  # source global options
  source("global_options.R")

#### COVID-19 Mortality Data ####
# We used the sars2pack library to access ECDC data.
# For replication purposes, we downloaded the dataset 
# with the following script on October 4, 2020.
# The dataset is directly available:
# https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

  # ONE TIME INSTALLATION, instructions from GitHub
  # If you do not have BiocManager installed:
  #install.packages('BiocManager')

  # Then, if sars2pack is not already installed:
  #BiocManager::install('seandavi/sars2pack')
  library(sars2pack)
  ecdc = ecdc_data()
  save(ecdc, file = "./1 - Data/ECDC_replication.RData")
  
#### ISRAEL ####
# This script collates Israel's weekly epidemiological reports.
  
# set working directory
  setwd("./0 - Raw Data/Israel")
  
# process data
  
  # identify files
  files = list.files()
  out = data.frame()
  
  # read in files
  for(i in files){
    df = read.xlsx(i)
    temp = which(df[,1]=="Death - Total (all causes)")
    out = bind_rows(out, cbind(i, df[temp,]))
  }
  
  # process data
  out2 = out %>% mutate(temp = gsub("IWER\\_", "", i),
                        temp = gsub("IWER", "", temp),
                        temp = gsub(".xlsx", "", temp)) %>% 
    separate(temp, into = c("Week", "Year")) %>%
    mutate(Week = as.numeric(Week), Year = as.numeric(Year),
           deaths = as.numeric(X17)) %>% 
    filter(Week > 1 & Week <= 31) %>%
    dplyr::select(Week, Year, deaths) %>%
    # fill in file that was missing on the Internet
    bind_rows(data.frame(Week = 14, Year = 2015, deaths = 629))
  
# reset working directory
  setwd(paste0(wd,"/1 - Data"))
  
# save data frame
  save(out2, file = "Israel.RData")
  
  