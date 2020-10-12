#**************************************** Table 1 *****************************************#
#                                                                                          #
#                                                                                          #
# This code generates Table 1.                                                             #
#******************************************************************************************#

#### SOURCE GLOBAL OPTIONS ####
# main directory
wd = "~/Dropbox/Mortality/Public code/"
setwd(wd)

# source global options
source("global_options.R")

# for loading data
setwd("./1 - Data")
load("ECDC_replication.RData")

# end date of analysis 
date_end = as.Date("2020-09-19")

#### 1. IDENTIFY COUNTRIES FOR INCLUSION ####

# List of OECD countries, obtained from OECD website
oecd = unlist(read.csv("oecd_countries.csv") %>% dplyr::select(Country.formatted))

# GDP per capita, from World Bank
gdp = read.csv("wb_per_capita_GDP.csv") %>% mutate(gdp_2019 = X2019) %>% dplyr::select(Country.Code, gdp_2019)

#### 2. SELECT COUNTRIES IN ECDC DATA ####

# OECD
e1a = ecdc %>% filter(location_name%in%oecd) 
#CHECK: length(oecd)==length(unique(e1a$location_name))

# Match to GDP
e1b = e1a %>% left_join(gdp, c("iso3c" = "Country.Code"))
#CHECK: sum(is.na(e1b$gdp_2019))
#CHECK: table(e1b$gdp_2019)

# FILTER ON GDP AND POPULATION
e1 = e1b %>% filter(population_2019>5e6) %>% filter(gdp_2019 > 25000) %>% mutate(location_name = gsub("_", " ", location_name))

#### 4. CLEAN ECDC FOR TARGET QUANTITIES ####
e2 = e1 %>% 
  # subset only on deaths
  filter(subset=='deaths') %>% 
  group_by(location_name) %>%
  mutate(
    # keep old count in case needed
    count_orig = count,
    # smooth for Spain (discussed in Technical Notes)
    count = ifelse(location_name=="Spain" & date>=as.Date("2020-05-22") & date<=as.Date("2020-06-18"), count[date=="2020-06-19"], count),
    # estimate per 100K
    rate_per_100k = 1e5*count/population_2019,
    
    # add daily (non-cumulative estimates)
    chg = c(NA, diff(rate_per_100k)),
    # rolling average
    roll7 = zoo::rollmean(chg, k = 7, fill = NA),
    # repeat for just counts
    chg_raw = c(NA, diff(count)),
    roll7_raw = zoo::rollmean(chg_raw, k = 7, fill = NA))  %>%
  # Finland has no date reported March 3,6,8, and 11, but also had no deaths on these days
  bind_rows(data.frame(location_name = "Finland", 
                       date = c(as.Date("2020-03-03"), as.Date("2020-03-05"), as.Date("2020-03-08"), as.Date("2020-03-11")), 
                       rate_per_100k = 0))

#### 5. CREATE FUNCTIONS FOR RATES AND DEATH ESTIMATES

# extract US population
pop = e2$population_2019[e2$location_name=="United States of America"][1]
US_max = e2$count[e2$location_name=="United States of America" & e2$date==date_end]

est_date = function(e2, date_end = as.Date("2020-09-19"), US_pop = pop){
  
  # deaths at end of period
  max = e2$count[e2$location_name=="United States of America" & e2$date==date_end]
  
  # deaths day before "switch" time
  US_crit = e2$count[e2$location_name=="United States of America" & e2$date==(e2$date_start-1)]
  #print(US_crit)
  
  e3 = e2 %>% group_by(location_name) %>% 
    # calculate death rate
    mutate(pop = population_2019[1],
           count = count[date==date_end] - count[date==(date_start-1)],
           rate = rate_per_100k[date==date_end] - rate_per_100k[date==(date_start-1)]) %>% 
    filter(date==date_end) %>%
    group_by(location_name, pop, count, rate) %>%
    # calculate death difference
    summarize(total =  max - (US_crit +  rate/1e5*US_pop))
  return(e3)
}

#### 6. IDENTIFY MEAURES OF EPIDEMIC START IN EACH COUNTRY ####
e_cases = e1 %>% group_by(location_name, subset) %>%
  summarize(
    # first 
    date_first = min(date[count>0]),
    
    # over 5
    date_5 = min(date[count>5]),
    
    # 1 per million
    date_1_per_mil = min(date[count/population_2019>=1/1e6], na.rm = T),
    date_1_per_mil_plus_2mos = date_1_per_mil + 60)

start = e_cases %>% filter(subset == 'deaths') %>% ungroup() %>% summarize(min = min(date_first))

#### 7. RUN ESTIMATES FOR TIME POINTS OF INTEREST ####

# select dates
dates = as.Date(c("2020-02-01", "2020-05-10", "2020-06-07"))

# run dates
est_date_out = data.frame()
for(i in 1:length(dates)){
  temp = est_date(e2 %>% mutate(date_start = dates[i]), date_end = date_end) %>% mutate(date = as.character(dates[i]))
  est_date_out = est_date_out %>% bind_rows(temp)
}

  #### 7a. ADD SENSITIVITY ANALYSIS ####
  temp2 = est_date(e2 %>% left_join(e_cases %>% filter(subset=="confirmed"), "location_name") %>%
                     mutate(date_start = date_1_per_mil_plus_2mos)) %>% mutate(date = "sens_analysis1")
  
  #### 7b. ADD SENSITIVITY ANALYSIS ####
  temp3 = est_date(e2 %>% left_join(e_cases %>% filter(subset=="deaths"), "location_name") %>%
                     mutate(date_start = date_5+60)) %>% mutate(date = "sens_analysis2")


# reshape data frame
tbl1 = est_date_out %>% bind_rows(temp2) %>% bind_rows(temp3) %>%
  dplyr::select(-pop, -count) %>% gather(var, value, rate, total) %>%
  mutate(lab = paste0(var, date),
         # format values
         value = ifelse(var=="rate", round(value,1), 
                        paste(comma(value,1), " (", round(value/US_max*100), ")", sep = ""))) %>% 
  dplyr::select(-var, -date) %>% spread(lab, value)


#### 8. ADD COLUMN FOR PANDEMIC START AND FORMAT TABLE ####
tbl1_out = e_cases %>% filter(subset=="confirmed") %>%
  dplyr::select(location_name, date_1_per_mil) %>% left_join(tbl1, "location_name") %>%
  mutate(is_US = location_name=="United States of America") %>% dplyr::arrange(is_US, as.numeric(`rate2020-02-01`)) %>%
  dplyr::select(-is_US)

write.csv(tbl1_out, file = "tbl1_out.csv")

#### 9. RUN REGRESSIONS ####
# (These are pretty boring)
regs = est_date_out %>% 
  # reorder factor levels so US is first in factor
  mutate(location_name = factor(location_name, levels = rev(sort(unique(location_name)))))

# ever
summary(glm(formula = count~location_name, offset = log(pop), 
            data = regs %>% filter(date=="2020-02-01"), family = poisson()))

# since May
summary(glm(formula = count~location_name, offset = log(pop), 
            data = regs %>% filter(date=="2020-05-10"), family = poisson()))

# since June
summary(glm(formula = count~location_name, offset = log(pop), 
            data = regs %>% filter(date=="2020-06-07"), family = poisson()))
