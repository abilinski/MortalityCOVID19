#**************************************** Table 2 *****************************************#
#                                                                                          #
#                                                                                          #
# This code generates Table 2.                                                             #
#******************************************************************************************#

#### SOURCE GLOBAL OPTIONS ####
  # main directory
  wd = "~/Dropbox/Mortality/Public code/"
  setwd(wd)
  
  # source global options
  source("global_options.R")
  source("./2 - Scripts/1_table1.R")
  
  # for loading data
  load("countries_combined.RData")

#### 1. PROCESS COUNTRIES ####
end_week = 30
countries2 = countries %>% left_join(e_cases %>% ungroup() %>% filter(subset=="confirmed") %>% 
                                       mutate(start_week = epiweek(date_1_per_mil),
                                              week_sens1 =  epiweek(date_1_per_mil + 60)) %>% 
                                       dplyr::select(location_name, start_week, week_sens1)) %>%
  filter(Week >= start_week & Week <= end_week) %>% 
  #filter(Year>=2018) %>%
  left_join(e1 %>% group_by(location_name) %>% summarize(population_2019 = population_2019[1])) %>%
  mutate(Time.Period = ifelse(Year==2020, "2020", "Comparison")) %>% 
  # reorder factors so that US is first
  mutate(location_name = factor(location_name, levels = rev(unique(e1$location_name))))

#### 2. FUNCTION TO ESTIMATE DEATH RATES + EXCESS DEATHS ####
est_week = function(countries2, US_pop = pop){
    
    # summarize outcomes
    countries3 = countries2 %>% group_by(location_name, Year, Time.Period) %>%
      summarize(deaths_total = sum(deaths),
             deaths_after = sum(deaths[Week>=week_start]),
             rate_per_100K = deaths_after/population_2019*1e5,
             deaths_before = sum(deaths[Week<week_start])) %>%
        gather(var, value, deaths_total, deaths_after, rate_per_100K, deaths_before) %>%
        group_by(location_name, Time.Period, var) %>%
        summarize(value = mean(value)) %>% group_by(location_name, var) %>%
        summarize(excess = -diff(value))
      
    # deaths at end of period
    max = countries3$excess[countries3$location_name=="United States of America" & countries3$var=="deaths_total"]
    print(max)
    
    # deaths day before "switch" time
    US_crit = countries3$excess[countries3$location_name=="United States of America" & countries3$var=="deaths_before"]
    print(US_crit)
    
    out = countries3 %>% filter(var=="rate_per_100K") %>% 
      # calculate death rate
      group_by(location_name, excess) %>%
      # calculate death difference
      summarize(total =  max - (US_crit + ifelse(excess<0, 0, excess)/1e5*US_pop))
    return(out)
  }
  
#### 3. ESTIMATE OVER RELEVANT TIME PERIODS ####

# select dates
weeks = c(2, 20, 24)

# run dates
est_week_out = data.frame()
for(i in 1:length(weeks)){
  temp = est_week(countries2 %>% mutate(week_start = weeks[i])) %>% mutate(week = as.character(weeks[i]))
  est_week_out = est_week_out %>% bind_rows(temp)
}

  # 1a. sensitivity analysis by time
  temp2 = est_week(countries2 %>% mutate(week_start = week_sens1)) %>% mutate(week = "sens1") 
  
  # 1b. sensitivity analysis by deatjs
  temp3 = est_week(countries2 %>% left_join(e_cases %>% ungroup() %>% filter(subset=="deaths"), "location_name") %>%
                     mutate(week_start = epiweek(date_5 + 60))) %>% mutate(week = "sens2")
  
# reshape data frame
US_max = 235610.2 #212702, if 2018-19
tbl2 = est_week_out %>% bind_rows(temp2) %>% bind_rows(temp3) %>%
  gather(var, value, excess, total) %>%
  mutate(lab = paste0(var, week),
         # format values
         value = ifelse(var=="excess", round(value,1), 
                        paste(comma(value,1), " (", round(value/US_max*100), ")", sep = ""))) %>% 
  dplyr::select(-var, -week) %>% spread(lab, value) %>% 
  mutate(is_US = location_name=="United States of America") %>% 
  arrange(is_US, as.numeric(excess2)) %>%
  dplyr::select(-is_US)

write.csv(tbl2, file = "tbl2_out.csv")
#write.csv(tbl2, file = "tbl2_out_post_2017.csv")

#### 4. RUN REGRESSIONS ####
# confirm levels
levels(countries2$location_name)

# ever
summary(glm(deaths~location_name*Time.Period + Week, offset = log(population_2019), 
            data = countries2, family = poisson()))

# since May
summary(glm(deaths~location_name*Time.Period + Week, offset = log(population_2019), 
            data = countries2 %>% filter(Week>=20), family = poisson()))

# since June
summary(glm(deaths~location_name*Time.Period + Week, offset = log(population_2019), 
            data = countries2 %>% filter(Week>=24), family = poisson()))


