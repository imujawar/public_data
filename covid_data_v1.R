
# Playing around with the available data and pulling in additional datastreams

# Pulling in Google mobility data
gpath <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"

gmob <- read.csv(gpath)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Creating basic functions to show top few rows of data
View50 <- function(x){View(x[1:50,])}
View100 <- function(x){View(x[1:100,])}


# Creating the 'not in' function
`%ni%` <- Negate(`%in%`) 


# Pulling in the load package function R file
# Load function to install list of packages
ldpkg <- dget("ldpkg.R")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


ldpkg(c("tidyverse", 
        "openxlsx",
        "maps",
        "ggmap",
        "mapdata",
        "passport",
        "readxl",
        "xlsx",
        "fuzzyjoin"))

# set.seed(19)

#SETTING DIRECTORY FOR INTERNATIONAL TASK FORCE - if James, defaults to his own account, otherwise appends users' name to the path
if(Sys.getenv("USERNAME")=="kux9") {
  dir.root <- "C:/Users/kux9/OneDrive - CDC/COVID19/"
} else dir.root <- paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/Fuller, James (CDC DDPHSIS CGH DGHP) (CTR) - COVID19/")
dir.root #check

#Define Directories
dir.data <- paste0(dir.root,"Data/")
#dir.export <- paste0(dir.root,"Percent Positive/")

## read in Johns Hopkins Data
#Import Country Time Series
jhu <- read_excel(paste0(dir.data, "JHU Data/JHU Data by Country - Long.xlsx"))
jhu$Date <- as.Date(jhu$Date)

View50(jhu)

# creating country variable in google mobility data
g <- gmob %>% 
  # Remove all non-alpanumeric characters
  mutate(matchid =  tolower(
    str_replace_all(country_region, "[^[:alnum:]]", ""))) %>% 
  select(matchid, country_region) %>% 
  unique()


# creating country variable in google mobility data
j <- jhu %>% 
  # Remove all non-alpanumeric characters
  mutate(matchid =  tolower(
    str_replace_all(Country, "[^[:alnum:]]", ""))) %>% 
  select(matchid, Country) %>% 
  unique()

nmatch <- left_join(j, g)


# Matching the country names for the Google and JHU data.
jhu_c <- sort(unique(j$matchid))
gmb_c <- sort(unique(as.vector(g$matchid)))

# checking difference
setdiff(jhu_c, gmb_c)

# Changing matchids in JHU dataset to match Google
jhu1 <- jhu %>% 
  # Remove all non-alpanumeric characters
  mutate(matchidx =  tolower(
    str_replace_all(Country, "[^[:alnum:]]", ""))) %>% 
  mutate(matchidx = case_when(
    matchid %in% c("boliviaplurinationalstateof") ~ "bolivia",
    matchid %in% c("caboverde") ~ "capeverde",
    matchid %in% c("côtedivoire") ~ "cãtedivoire",
    matchid %in% c("dominica") ~ "dominicanrepublic",
    matchid %in% c("internationalconveyancejapan") ~ "japan",
    matchid %in% c("laopeoplesdemocraticrepublic") ~ "laos",
    matchid %in% c("myanmar") ~ "myanmarburma",
    matchid %in% c("republicofmoldova") ~ "moldova",
    matchid %in% c("unitedkingdomofgreatbritainandnorthernireland") ~ "unitedkingdom",
    matchid %in% c("unitedrepublicoftanzania") ~ "tanzania",
    matchid %in% c("unitedstatesofamerica") ~ "unitedstates",
    matchid %in% c("venezuelabolivarianrepublicof") ~ "venezuela",
    TRUE ~ matchid))

gmob1 <- gmob %>% 
  # Remove all non-alpanumeric characters
  mutate(matchidx =  tolower(
    str_replace_all(country_region, "[^[:alnum:]]", "")))  
  

# Getting the google mobility data that is stakable
