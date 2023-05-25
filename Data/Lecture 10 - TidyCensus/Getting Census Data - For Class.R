install.packages("walkscoreAPI")
install.packages("googleAuthR")
install.packages("tidycensus")
install.packages("tidyverse")
install.packages("sp")
install.packages("maptools")
install.packages("sf")
install.packages("rgdal")
install.packages("tigris")
install.packages("ggplot2")
install.packages("readxl")
install.packages("tmap")
install.packages("tmaptools")
install.packages("tibble")
install.packages("httr")
install.packages("curl")
library(walkscoreAPI)
library(googleAuthR)
library(tidycensus)
library(tidyverse)
library(sp)
library(maptools)
library(sf)
library(rgdal)
library(tigris)
library(ggplot2)
library(readxl)
library(tmap)
library(tmaptools)
library(tibble)
library(httr)
library(curl)

#Go to https://api.census.gov/data/key_signup.html to request a Census key.
census_api_key("e82cfb560240b2716c3b4309fc6560018a7ca462", overwrite=TRUE)
readRenviron("~/.Renviron")
options(tigris_use_cache = TRUE)
options(tigris_class="sf")

#Here, let's try to extract a bunch of variables at the block group level for a few counties
#In different states (Philadelphia & Montgomery in PA; Mercer in NJ). We want these variables
#From the 2015 American Community Survey (ACS).

#First, let's figure out what variables are available.
#This gives a list of all variables that are available in the 2015 ACS survey.
VariableNames <-load_variables(2015, "acs5", cache=TRUE)
View(VariableNames)

#Note that some of the variables may NOT be available for the block group geography; they're 
#only available for larger geographies such as tracts. The suffix 'E' stands for estimate;
#The suffix 'M' stands for margin of error. We only deal with estimates.
vars = c("B00001_001", "B00002_001", "B00001_001", "B00002_001",
    "B02001_002", "B02001_003", "B19013_001", "B25001_001", "B25002_001", 
    "B25002_002", "B25002_003", "B25003_001", "B25003_002", "B25003_003", 
    "B25058_001", "B25064_001", "B25077_001")

#Let's get the data!
#Block Group Level Data for Philadelphia County, PA
Philadelphia <- data.frame(tidycensus::get_acs(geography="block group", variables = vars, state="PA", county="Philadelphia", geometry=TRUE, year=2015, output="wide"))
#Let's calculate a few new variables from existing ones.
#pct_white is number of people who are white divided by total number of people in block group
Philadelphia$pct_white <- Philadelphia$B02001_002E/Philadelphia$B00001_001E
#pct_black is number of people who are black divided by total number of people in block group
Philadelphia$pct_black <- Philadelphia$B02001_003E/Philadelphia$B00001_001E
#pct_vacant is number of vacant housing units divided by total housing units in block group
Philadelphia$pct_vacant <- Philadelphia$B25002_003E/Philadelphia$B25002_001E

#Block Group Level Data for Montgomery County, PA
Montgomery <- data.frame(tidycensus::get_acs(geography="block group", variables = vars, state="PA", county="Montgomery", geometry=TRUE, output="wide"))
#Let's calculate a few new variables from existing ones (same as for Philadelphia)
Montgomery$pct_white <- Montgomery$B02001_002E/Montgomery$B00001_001E
Montgomery$pct_black <- Montgomery$B02001_003E/Montgomery$B00001_001E
Montgomery$pct_vacant <- Montgomery$B25002_003E/Montgomery$B25002_001E

#Block Group Level Data for Mercer County, PA
Mercer <- data.frame(tidycensus::get_acs(geography="block group", variables = vars, state="NJ", county="Mercer", geometry=TRUE, output="wide"))
#Let's calculate a few new variables from existing ones (same as for Philadelphia)
Mercer$pct_white <- Mercer$B02001_002E/Mercer$B00001_001E
Mercer$pct_black <- Mercer$B02001_003E/Mercer$B00001_001E
Mercer$pct_vacant <- Mercer$B25002_003E/Mercer$B25002_001E



#Now, let's get block group boundary files for Philadelphia, Montgomery and Mercer Counties
Philadelphia_BG <- tigris::block_groups(state="PA", county="Philadelphia", class="sf")
Montgomery_BG <- tigris::block_groups(state="PA", county="Montgomery", class="sf")
Mercer_BG <- tigris::block_groups(state="NJ", county="Mercer", class="sf")



#For each county, let's join the data tables to the boundary files by GEOID
#(Variable GEOID is found in both the data tables and boundary tables - it's like 
#Doing a 'Join by attribute' in ArcGIS)
Philadelphia_geojoin <-tigris::geo_join(Philadelphia_BG, Philadelphia, by="GEOID", how="inner")
Montgomery_geojoin <-tigris::geo_join(Montgomery_BG, Montgomery, by="GEOID", how="inner")
Mercer_geojoin <-tigris::geo_join(Mercer_BG, Mercer, by="GEOID", how="inner")



#Let's create a single shapefile containing the 2015 ACS variables we extracted for all
#block groups in Philadelphia, Montgomery and Mercer counties.
CensusData <- rbind(Philadelphia_geojoin, Montgomery_geojoin, Mercer_geojoin)
#Let's see what kind of R object CensusData is.
class(CensusData)



#Let's export the CensusData file as a shapefile in the working directory.
#There are some changes in variable names (we can use warnings() command to figure them out).
setwd("C:/Class Data/Census Data")
library(sf)
sf::st_write(CensusData, "CensusData.shp")



#We can always read the shapefile back into R
CensusData <-rgdal::readOGR("CensusData.shp") 

#We can play around with just the data part of the shapefile as well if we want to.
CensusData.data <- CensusData@data



