#**************************************** Table 2 -- Part 1 *******************************#
#                                                                                          #
#                                                                                          #
# This formats country data to generate Table 2.                                           #
#******************************************************************************************#

#### SETUP ####
# main directory
wd = "~/Dropbox/Mortality/Public code/"
setwd(wd)

# source global options
source("global_options.R")

# for loading data
setwd("./1 - Data")

#### AUSTRIA ####
austria = read.csv("Austria.csv") %>% 
  mutate(deaths = as.numeric(Number.of.deaths)) %>%
  dplyr::select(Week, Year, deaths) %>%
  mutate(location_name = "Austria")

#### BELGIUM ####
belgium = read.csv("Belgium.csv") %>%
  mutate(date = as.Date(DT_DATE, format = "%d/%m/%Y"),
         Week = epiweek(date),
         Year = NR_YEAR) %>%
  group_by(Year, Week) %>% 
  filter(Year>=2015) %>%
  summarize(deaths = sum(MS_NUM_DEATH)) %>%
  dplyr::select(Week, Year, deaths) %>%
  mutate(location_name = "Belgium")
  
#### CANADA ####
canada = read.csv("Canada.csv") %>% 
  filter(GEO=="Canada, place of occurrence") %>%
  filter(Age.at.time.of.death=="Age at time of death, all ages") %>%
  filter(Sex=="Both sexes") %>%
  mutate(date = as.Date(REF_DATE, format = "%m/%d/%y"),
         Week = epiweek(date),
         Year = year(date),
         deaths = VALUE) %>%
  filter(Year >= 2015) %>%
  dplyr::select(Week, Year, deaths) %>%
  mutate(location_name = "Canada")

#### DENMARK ####
denmark = read.csv("denmark.csv") %>% 
  mutate(date = as.Date(Date_mod, format = "%Y-%m-%d"),
         Year = year(date),
         Week = epiweek(date)) %>%
  dplyr::select(Week, Year, deaths) %>%
  mutate(location_name = "Denmark")

#### FINLAND ####
finland = read.csv("finland.csv") %>%
  mutate(Year = as.numeric(substring(Week, 1, 4)),
         Week = as.numeric(substring(Week, 6, 7))) %>%
  dplyr::select(Week, Year, deaths) %>%
  mutate(location_name = "Finland")

#### FRANCE ####
#CHECK WEEK START

# Define function that extracts dept_code from France's historical deaths register
# We use this as a 'check' on valid deaths
# as France_2018.csv has slightly higher deaths than France_2018To_2020.csv
# for corresponding weeks
# Pushes total closer to aggregated version in French file
# But difference is small
# Taken from Economist GitHub
get_french_dept_code <- function(x) { dept_code <- substr(x,1,nchar(x)-3) }

#france18 = read.csv("France_2018.csv") %>% 
#  mutate(date = as.Date(as.character(datedeces), format = "%Y%m%d"),
#         chk = get_french_dept_code(lieudeces),
#         chk2 = chk%in%c(1:95, "2a", "2b", 971:974, 976),
#         Week = epiweek(date),
#         Year = year(date)) %>% filter(Year%in%c(2018)) 
#  f1 = france18 %>% group_by(Week) %>% summarize(d = n())
#  f2 = france18 %>% filter(chk2) %>% group_by(Week) %>% summarize(d = n())
#  View(france %>% filter(Year==2018) %>% left_join(f1, "Week") %>% left_join(f2, "Week"))
# Approach is slightly conservative, can see when running code on line 83.

france1 = read.csv("France_2015.csv") %>% 
  bind_rows(read.csv("France_2016.csv")) %>%
  bind_rows(read.csv("France_2017.csv")) %>%
   mutate(date = as.Date(as.character(datedeces), format = "%Y%m%d"),
         chk = get_french_dept_code(lieudeces),
         chk2 = chk%in%c(1:95, "2a", "2b", 971:974, 976),
         Week = epiweek(date),
         Year = year(date)) %>%
  filter(Year%in%c(2015:2017) & lieudeces!="" & chk2) %>%
  dplyr::group_by(Week, Year) %>% summarize(deaths = length(date))

france = read.csv("France_2018To_2020.csv") %>% filter(Zone=="France") %>%
  gather(var, deaths, Deaths_2020, Deaths_2019, Deaths_2018) %>%
  separate(var, into = c("Misc", "Year")) %>%
  mutate(Year = as.numeric(Year),
         date = as.Date(Date, format = "%m/%d/%y"),
         date = as.Date(paste(Year, "-", month(date), "-", day(date), sep = "", format = "%Y-%m%-d")),
         Week = epiweek(date)) %>%
  group_by(Year, Week) %>% summarize(deaths = sum(as.numeric(deaths))) %>%
  bind_rows(france1) %>%
  dplyr::select(Year, Week, deaths) %>%
  mutate(location_name = "France")
         
#### GERMANY ####
germany = read.csv("Germany.csv") %>%
  mutate(date = as.Date(Date, format = "%m/%d/%y"),
         Year = year(date),
         Week = epiweek(date)) %>%
  group_by(Year, Week) %>% summarize(deaths = sum(as.numeric(deaths))) %>%
  dplyr::select(Year, Week, deaths) %>% 
  mutate(location_name = "Germany")

#### ISRAEL ####
load("Israel.RData")
israel = out2 %>% dplyr::select(Year, Week, deaths) %>%
  mutate(location_name = "Israel")

#### NETHERLANDS ####
netherlands = read.csv("Netherlands.csv") %>%
  dplyr::select(Year, Week, deaths) %>%
  mutate(location_name = "Netherlands")
  
#### NORWAY ####
norway = read.csv("Norway.csv") %>% 
  gather(var, value, 3:8) %>%
  mutate(Year = as.numeric(substring(var, 8,11)),
         deaths = value,
         Week = as.numeric(sub("Week ", "", Week))) %>%
  dplyr::select(Week, Year, deaths) %>%
  mutate(location_name = "Norway")

#### SPAIN ####
spain = read.csv("Spain.csv") %>%
  mutate(Year = as.numeric(substring(Week, 1, 4)),
         Week = as.numeric(substring(Week, 6, 7))) %>%
  dplyr::select(Year, Week, deaths) %>%
  mutate(location_name = "Spain") 

#### SWEDEN ####
sweden = read.csv("Sweden.csv") %>% 
  gather(var, value, 3:8) %>%
  mutate(Year = as.numeric(substring(var, 2,5)),
         date = as.Date(Date, format = "%m/%d/%y"))
year(sweden$date) = sweden$Year
sweden = sweden %>% mutate(Week = epiweek(date)) %>%
  group_by(Year, Week) %>% summarize(deaths = sum(value)) %>%
  mutate(location_name = "Sweden")

#### SWITZERLAND ####
# 2020
switzerland1 = read.csv("Switzerland_2020.csv") %>%
  mutate(date = as.Date(Ending, format = "%d.%m.%Y"), 
         Year = year(date),
         Week = as.numeric(Week),
         deaths = as.numeric(NoDeaths_EP)) %>% dplyr::select(Year, Week, deaths)

# prior to 2020
switzerland2 = read.csv("Switzerland_2010To2019.csv") %>%
  mutate(Year = as.numeric(CY),
         deaths = NumberOfDeaths) %>% filter(Year>=2015) %>% 
  dplyr::select(Year, Week, deaths)

# combine files
switzerland = bind_rows(switzerland1, switzerland2) %>%
  group_by(Year, Week) %>% summarize(deaths = sum(deaths)) %>%
  mutate(location_name = "Switzerland")

#### UNITED KINGDOM ####
gb = read.csv("Great_Britain.csv")  %>% 
  dplyr::select(Year, Week, deaths)

# Northern Ireland
ni = read.csv("Northern_Ireland.csv") %>%
  mutate(date = as.Date(Week.End, format = "%m/%d/%y"),
         Year = year(date)) %>%
  dplyr::select(Year, Week, deaths)

# Scotland
scotland = read.csv("Scotland.csv") %>% 
  gather(var, value, X2015, X2016, X2017, X2018, X2019, X2020) %>%
  mutate(Year = as.numeric(substring(var, 2, 5)),
         Week = as.numeric(substring(Week, 2, 3)),
         deaths = as.numeric(value)) %>%
  filter(Year >= 2015) %>%
  dplyr::select(Year, Week, deaths)

uk = bind_rows(gb, ni, scotland) %>%
  group_by(Year, Week) %>% summarize(deaths = sum(deaths)) %>%
  mutate(location_name = "United Kingdom")

#### UNITED STATES ####
usa = read.csv("USA.csv") %>% 
  group_by(Jurisdiction, Type, Year, Week, Week.Ending.Date) %>% 
  summarize(deaths = sum(Number.of.Deaths)) %>% ungroup() %>%
  filter(Jurisdiction=="United States") %>%
  filter(Type=="Unweighted") %>%
  dplyr::select(Year, Week, deaths) %>%
  mutate(location_name = "United States of America")

#### COMBINE & SAVE ####
countries = bind_rows(austria, belgium, canada,
                      denmark, finland, france, 
                      germany, israel, netherlands,
                      norway, spain, sweden, switzerland,
                      uk, usa)
save(countries, file = "countries_combined.RData")

