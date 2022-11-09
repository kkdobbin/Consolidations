# CONSOLIDATED SYSTEMS ----------------------------------------------------

#Demographic analysis of consolidated systems (point to polygon)

#Load libraries
library(tidyverse)
library(ggmap)
library(censusapi)

#skip this step when re-doing
#Load data
Addresses <- read_csv("Data/Consolidated_addresses.csv")
Addresses <- Addresses %>% select(PWSID, Vetted_address)
Addresses <- Addresses[complete.cases(Addresses),]#get rid of three park WC ones which are NAs (already excluding them from data set so doesn't matter)

#Geocode consolidated systems
latlong <- as.data.frame(geocode(Addresses$Vetted_address))

#Join
latlong$Vetted_address <- Addresses$Vetted_address
latlong <- left_join(latlong, Addresses, by = "Vetted_address")

#Save as csv
write_csv(latlong, "Outputs/consolidated_systems_geocoded.csv")

#get census data
library(tidycensus)
#census_api_key("84059d7faf015832a99ef159379684476b2ec4a7", overwrite = TRUE, install = TRUE)
#readRenviron("~/.Renviron")
#Sys.getenv("CENSUS_API_KEY")

Demographics_blockg <- get_acs(geography = "block group", variables = c("B03002_001E", "B03002_003E", "B03002_004E", "B03002_005E", "B03002_006E", "B03002_007E", "B03002_012E", "B19013_001E", "B25003_001E", "B25003_002E", "B25003_003E", "B11001_001E"), state = "CA", year =2020, output = "wide", survey = "acs5", geometry = TRUE)

#rename variables
Demographics_blockg <- Demographics_blockg %>% rename(Race.estimate.total = B03002_001E, Race.white.alone = B03002_003E, Race.black.alone = B03002_004E, Race.native.alone = B03002_005E, Race.asian.alone = B03002_006E, Race.PI.alone = B03002_007E, Race.hispanicorlatino = B03002_012E, Median.hh.income = B19013_001E, Tenure.estimate.total = B25003_001E, Tenure.owner = B25003_002E, Tenure.renter = B25003_003E, Households.total = B11001_001E)

##Load consolidated system points
Consolidated_Points <- read_csv("Outputs/consolidated_systems_geocoded.csv")

library(sf)
library(sp)
Consolidated_Points <- st_as_sf(Consolidated_Points, coords = c("lon", "lat"))
Consolidated_Points <- st_set_crs(Consolidated_Points, "EPSG:4269")
st_crs(Consolidated_Points) 

Demographics_blockg <- st_as_sf(Demographics_blockg)
st_transform(Demographics_blockg)
st_crs(Demographics_blockg)

Joined_consolidatedsystems_blockgroup2020 <- st_join(Consolidated_Points, Demographics_blockg)
#Checked and it seems accurate!

write_csv(Joined_consolidatedsystems_blockgroup2020, "Outputs/Consolidated_withblockgroup2020.csv")


# receiving systems -------------------------------------------------------

#Make median household income into a count by multiplying median income by total households (see https://crd230.github.io/lab3.html)
Demographics_blockg$Income.aggregatecount <- Demographics_blockg$Median.hh.income*Demographics_blockg$Households.total

#load in boundary polygons for CWS
PWS_boundary <- st_read("Data/PWS/")
str(PWS_boundary)
#plot(st_geometry(PWS_boundary)) #takes a while but works!

#join consolidation data with boundary data so I only have to work with boundaries I actually need
Cases <- read_csv("Outputs/Compiledfinaldata.csv")
Cases <- left_join(Cases, Joined_consolidatedsystems_blockgroup2020, by = c("SystemID" = "PWSID")) #first add consolidated systems with census data in too
PWS_boundary$SABL_PWSID <- as.factor(PWS_boundary$SABL_PWSID)
PWS_boundary$WATER_SYST <- as.factor(PWS_boundary$WATER_SYST)
PWS_boundary <- PWS_boundary %>% filter(BOUNDARY_T != "Jurisdictional") #remove jurisidictional boundaires to get rid of duplicates
Cases <- left_join(Cases, PWS_boundary, by = c("Receiving_System_ID" = "SABL_PWSID"))
Cases <- Cases[,-c(74:104)] #Reduce data frame a bit to be more manageable, don't need most of the boundary variables just added
#rename variables for clarity on which spatial goes which system system
Cases <- Cases %>% rename(geometry_receiving = geometry.y, geometry_consolidated = geometry.x, SHAPE_Area_receiving = SHAPE_Area, SHAPE_Leng_receiving = SHAPE_Leng)

#areal interpolation (https://crd230.github.io/lab3.html)
#make data set of just boundaries to play with
Recieving_boundaries <- Cases %>% select(Receiving_System_ID, geometry_receiving)
Recieving_boundaries <- Recieving_boundaries %>% unique() #check later but I think this removed duplicates
Recieving_boundaries <- Recieving_boundaries %>% rename(geometry = geometry_receiving)

library(areal)
Recieving_boundaries <- st_as_sf(Recieving_boundaries) #make sf object

#make to be the same crs
Recieving_boundaries <- st_transform(Recieving_boundaries, crs = 3857)
st_crs(Recieving_boundaries) 
Recieving_boundaries <- st_make_valid(Recieving_boundaries)
Demographics_blockg <- st_transform(Demographics_blockg, crs = 3857)
st_crs(Demographics_blockg) 

#now try areal interpolation
Interpolation <- aw_interpolate(Recieving_boundaries, tid = Receiving_System_ID, source = Demographics_blockg, sid = GEOID, weight = "total", output = "tibble", extensive = c("Race.estimate.total", "Race.white.alone", "Race.black.alone", "Race.native.alone", "Race.asian.alone", "Race.PI.alone", "Race.hispanicorlatino", "Tenure.estimate.total", "Tenure.owner", "Tenure.renter", "Households.total", "Income.aggregatecount"))

#trouble shooting aw_interpolate command
#ar_validate(source = Demographics_blockg, target = Recieving_boundaries, varList = c("Race.estimate.total", "Race.white.alone", "Race.black.alone", "Race.native.alone", "Race.asian.alone", "Race.PI.alone", "Race.hispanicorlatino", "Tenure.estimate.total", "Tenure.owner", "Tenure.renter", "Households.total", "Income.aggregatecount"), verbose = TRUE)

#now add into cases, first remove geometry columns
Cases <- Cases[,-c(47,76)]
Cases <- left_join(Cases, Interpolation, by = "Receiving_System_ID")

#Check and rewrite with new name
write_csv(Cases, "Outputs/Fullfinaldata.csv")

#need to clean up all the demographic variables for receiving system are .y now and consolidated systems are .x also all are aggregate counts so need to make percentages to compare. Lastly reduce to a more manageable data set with only needed data for descriptive analysis

Cases$Percent.renter.consolidated <- ((Cases$Tenure.renter.x)/(Cases$Tenure.estimate.total.x))*100
summary(Cases$Percent.renter.consolidated)

Cases$Percent.owner.consolidated <- ((Cases$Tenure.owner.x)/(Cases$Tenure.estimate.total.x))*100
summary(Cases$Percent.owner.consolidated)

Cases$Percent.white.consolidated <- ((Cases$Race.white.alone.x)/(Cases$Race.estimate.total.x))*100
summary(Cases$Percent.white.consolidated)

Cases$Percent.black.consolidated <- ((Cases$Race.black.alone.x)/(Cases$Race.estimate.total.x))*100
summary(Cases$Percent.black.consolidated)

Cases$Percent.asian.consolidated <- ((Cases$Race.asian.alone.x)/(Cases$Race.estimate.total.x))*100
summary(Cases$Percent.asian.consolidated)

Cases$Percent.native.consolidated <- ((Cases$Race.native.alone.x)/(Cases$Race.estimate.total.x))*100
summary(Cases$Percent.native.consolidated)

Cases$Percent.PI.consolidated <- ((Cases$Race.PI.alone.x)/(Cases$Race.estimate.total.x))*100
summary(Cases$Percent.PI.consolidated)

Cases$Percent.hispanicorlatino.consolidated <- ((Cases$Race.hispanicorlatino.x)/(Cases$Race.estimate.total.x))*100
summary(Cases$Percent.hispanicorlatino.consolidated)

Cases$MHI.consolidated <- Cases$Median.hh.income
summary(Cases$MHI.consolidated)

#Receiving variables
Cases$Percent.renter.receiving <- ((Cases$Tenure.renter.y)/(Cases$Tenure.estimate.total.y))*100
summary(Cases$Percent.renter.receiving)

Cases$Percent.owner.receiving <- ((Cases$Tenure.owner.y)/(Cases$Tenure.estimate.total.y))*100
summary(Cases$Percent.owner.receiving)

Cases$Percent.white.receiving <- ((Cases$Race.white.alone.y)/(Cases$Race.estimate.total.y))*100
summary(Cases$Percent.white.receiving)

Cases$Percent.black.receiving <- ((Cases$Race.black.alone.y)/(Cases$Race.estimate.total.y))*100
summary(Cases$Percent.black.receiving)

Cases$Percent.asian.receiving <- ((Cases$Race.asian.alone.y)/(Cases$Race.estimate.total.y))*100
summary(Cases$Percent.asian.receiving)

Cases$Percent.native.receiving <- ((Cases$Race.native.alone.y)/(Cases$Race.estimate.total.y))*100
summary(Cases$Percent.native.receiving)

Cases$Percent.PI.receiving <- ((Cases$Race.PI.alone.y)/(Cases$Race.estimate.total.y))*100
summary(Cases$Percent.PI.receiving)

Cases$Percent.hispanicorlatino.receiving <- ((Cases$Race.hispanicorlatino.y)/(Cases$Race.estimate.total.y))*100
summary(Cases$Percent.hispanicorlatino.receiving)

Cases$MHI.receiving <- ((Cases$Income.aggregatecount)/(Cases$Households.total.y))
summary(Cases$MHI.receiving)

#Reduce to more manageable set
Small <- Cases
Small <- Small[,-c(46:86)]

write_csv(Small, "Outputs/Fullfinaldata_smallclean.csv")
