# CONSOLIDATED SYSTEMS ----------------------------------------------------

#Demographic analysis of consolidated systems (point to polygon)

#Load libraries
library(tidyverse)
library(ggmap)
library(censusapi)

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

Demographics_blockg <- get_acs(geography = "block group", variables = c("B02001_001E", "B02001_002E", "B02001_003E", "B02001_004E", "B02001_005E", "B02001_006E", "B02001_007E", "B02001_008E", "B19013_001E", "B25003_001E", "B25003_002E", "B25003_003E", "B11001_001E"), state = "CA", year =2020, output = "wide", survey = "acs5", geometry = TRUE)

#rename variables
Demographics_blockg <- Demographics_blockg %>% rename(Race.estimate.total = B02001_001E, Race.white.alone = B02001_002E, Race.black.alone = B02001_003E, Race.native.alone = B02001_004E, Race.asian.alone = B02001_005E, Race.PI.alone = B02001_006E, Race.someother.alone = B02001_007E, Race.twoplus = B02001_008E, Median.hh.income = B19013_001E, Tenure.estimate.total = B25003_001E, Tenure.owner = B25003_002E, Tenure.renter = B25003_003E, Households.total = B11001_001E)

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
Cases <- read_csv("Outputs/CompiledApril20.csv")
Cases <- left_join(Cases, Joined_consolidatedsystems_blockgroup2020, by = c("SystemID" = "PWSID")) #first add consolidated systems with census data in too
PWS_boundary$SABL_PWSID <- as.factor(PWS_boundary$SABL_PWSID)
PWS_boundary$WATER_SYST <- as.factor(PWS_boundary$WATER_SYST)
PWS_boundary <- PWS_boundary %>% filter(BOUNDARY_T != "Jurisdictional") #remove jurisidictional boundaires to get rid of duplicates
Cases <- left_join(Cases, PWS_boundary, by = c("Receiving_System_ID" = "SABL_PWSID"))
Cases <- Cases[,-c(67:97)] #Reduce data frame a bit to be mroe manageable, don't need most of the boundary variables just added
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
Interpolation <- aw_interpolate(Recieving_boundaries, tid = Receiving_System_ID, source = Demographics_blockg, sid = GEOID, weight = "total", output = "tibble", extensive = c("Race.estimate.total", "Race.white.alone", "Race.black.alone", "Race.native.alone", "Race.asian.alone", "Race.PI.alone", "Race.someother.alone", "Race.twoplus", "Tenure.estimate.total", "Tenure.owner", "Tenure.renter", "Households.total", "Income.aggregatecount"))

#trouble shooting aw_interpolate command
#ar_validate(source = Demographics_blockg, target = Recieving_boundaries, varList = c("Race.estimate.total", "Race.white.alone", "Race.black.alone", "Race.native.alone", "Race.asian.alone", "Race.PI.alone", "Race.someother.alone", "Race.twoplus", "Tenure.estimate.total", "Tenure.owner", "Tenure.renter", "Households.total", "Income.aggregatecount"), verbose = TRUE)

#now add into cases, first remove geometry columns
Cases <- Cases[,-c(40,71)]
Cases <- left_join(Cases, Interpolation, by = "Receiving_System_ID")

write_csv(Cases, "Outputs/FulldataApril20.csv")

#need to clean up all the demographic variables for receiving sytems are .y now and consolidated systems are .x I think
#Why are so many income aggregate counts NAs?
#Finish looking over data to make sure nothing else looks wierd

#quick summary stats for fun
mean((na.omit(Cases$Tenure.owner.x))/mean((na.omit(Cases$Tenure.estimate.total.x))))
mean((na.omit(Cases$Tenure.owner.y))/mean((na.omit(Cases$Tenure.estimate.total.y))))

mean((na.omit(Cases$Race.white.alone.x))/mean((na.omit(Cases$Race.estimate.total.x))))
mean((na.omit(Cases$Race.white.alone.y))/mean((na.omit(Cases$Race.estimate.total.y))))

mean(na.omit(Cases$Median.hh.income))
mean((na.omit(Cases$Income.aggregatecount))/mean((na.omit(Cases$Households.total))))


# Distance from point to polygon ------------------------------------------

st_crs(Consolidated_Points) 
st_crs(Recieving_boundaries) 
Consolidated_Points <- st_transform(Consolidated_Points, "EPSG:3857")

Distance <- st_distance(x = Consolidated_Points, y = Recieving_boundaries, by_element = TRUE) #Not working because I need the two dfs to be the same length

Recieving_boundaries_all <- Cases %>% select(Receiving_System_ID)
Recieving_boundaries_all <- left_join(Recieving_boundaries_all, Recieving_boundaries)
Recieving_boundaries_all <- st_as_sf(Recieving_boundaries_all)
st_crs(Recieving_boundaries_all) 

Consolidated_points_all <- Cases %>% select(SystemID)
Consolidated_points_all <- left_join(Consolidated_points_all, Consolidated_Points, by = c("SystemID" = "PWSID"))
Consolidated_points_all <- st_as_sf(Consolidated_points_all)
st_crs(Consolidated_points_all) 

Distance <- st_distance(x = Consolidated_points_all, y = Recieving_boundaries_all, by_element = TRUE, which = "Euclidean")
Distance <- as.data.frame(Distance) #Just doesn't seem right


library(geosphere)
Test <- dist2Line(Consolidated_points_all, Recieving_boundaries_all, distfun = distGeo) #doesn't work

plot(st_geometry(Recieving_boundaries))
plot(st_geometry(Consolidated_Points))
