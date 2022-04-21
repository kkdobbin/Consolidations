
# CONSOLIDATED SYSTEMS ----------------------------------------------------

#Demographic analysis of consolidated systems (point to polygon)

#Load libraries
library(tidyverse)
library(ggmap)
library(censusapi)

#Load data
Addresses <- read_csv("Data/Consolidated_addresses.csv")
Addresses <- Addresses %>% select(PWSID, Vetted_address)
Addresses <- Addresses[complete.cases(Addresses),]#get rid of three park WC ones which are NAs (already expluding them from dataset so doesn't matter)

#Geocode consolidated systems
latlong <- as.data.frame(geocode(Addresses$Vetted_address))

#Join
latlong$Vetted_address <- Addresses$Vetted_address
latlong <- left_join(latlong, Addresses, by = "Vetted_address")

#Save as csv
write_csv(latlong, "Outputs/consolidated_systems_geocoded.csv")

#Get census API key and load it to computer
# Add key to .Renviron
Sys.setenv(CENSUS_KEY= "4059d7faf015832a99ef159379684476b2ec4a7")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

#Get census endpoints
apis <- listCensusApis()
View(apis)

#Get metadata for 5-year estimates 2020 to figure out variables. See https://www.census.gov/data/developers/data-sets/acs-5year.html
Fiveyearvars <- listCensusMetadata(name = "acs/acs5", vintage = 2020, type = "variables")
head(Fiveyearvars)

#Get geographies
Geographies <- listCensusMetadata(name =  "acs/acs5", vintage = 2020, type = "geography")

#Try to get median household income (B19013)
Test <- getCensus(
  name = "acs/acs5",
  vintage = 2020,
  key = Sys.getenv("84059d7faf015832a99ef159379684476b2ec4a7"),
  vars = "group(B19013)",
  region = "tract:*", 
  regionin = "state:06")
head(Test)

#Try and get race (B02001)
Test2 <- getCensus(
  name = "acs/acs5",
  vintage = 2020,
  key = Sys.getenv("84059d7faf015832a99ef159379684476b2ec4a7"),
  vars = "group(B02001)",
  region = "blockgroup:*", 
  regionin = "state:06")
head(Test2)

Racevariables <- listCensusMetadata(name =  "acs/acs5", vintage = 2020, type = "variables", group = "B02001")

Incomevariables <- listCensusMetadata(name =  "acs/acs5", vintage = 2020, type = "variables", group = "B19013")

Tenurevariables <- listCensusMetadata(name =  "acs/acs5", vintage = 2020, type = "variables", group = "B25003")

#Seems as though to directly use the Census API in order to get block groups I need to fetch them by county or census track. TO get around this going to try tidycensus package


# Census grab starts officially here --------------------------------------


library(tidycensus)
#census_api_key("84059d7faf015832a99ef159379684476b2ec4a7", overwrite = TRUE, install = TRUE)
#readRenviron("~/.Renviron")
#Sys.getenv("CENSUS_API_KEY")

Demographics_block <- get_acs(geography = "block group", variables = c("B02001_001E", "B02001_002E", "B02001_003E", "B02001_004E", "B02001_005E", "B02001_006E", "B02001_007E", "B02001_008E", "B19013_001E", "B25003_001E", "B25003_002E", "B25003_003E", "B11001_001E"), state = "CA", year =2020, output = "wide", survey = "acs5", geometry = TRUE)

#rename variables
Demographics_block <- Demographics_block %>% rename(Race.estimate.total = B02001_001E, Race.white.alone = B02001_002E, Race.black.alone = B02001_003E, Race.native.alone = B02001_004E, Race.asian.alone = B02001_005E, Race.PI.alone = B02001_006E, Race.someother.alone = B02001_007E, Race.twoplus = B02001_008E, Median.hh.income = B19013_001E, Tenure.estimate.total = B25003_001E, Tenure.owner = B25003_002E, Tenure.renter = B25003_003E, Households.total = B11001_001E)

##Load consolidated system points
Consolidated_Points <- read_csv("Outputs/consolidated_systems_geocoded.csv")

library(sf)
library(sp)
Consolidated_Points <- st_as_sf(Consolidated_Points, coords = c("lon", "lat"))
Consolidated_Points <- st_set_crs(Consolidated_Points, "EPSG:4269")
st_crs(Consolidated_Points) 

Demographics_tract <- st_as_sf(Demographics_block)
st_transform(Demographics_block)
st_crs(Demographics_block)


Joined_consolidatedsystems_censustract2020 <- st_join(Consolidated_Points, Demographics_tract)
#Checked and it seems accurate!

write_csv(Joined_consolidatedsystems_censustract2020, "Outputs/Consolidated_withcensustract2020.csv")

# receiving systems -------------------------------------------------------

#Make median household income into a count by multiplying median income by total households (see https://crd230.github.io/lab3.html)
Demographics_tract$Income.aggregatecount <- Demographics_tract$Median.hh.income*Demographics_tract$Households.total

#load in boundary polygons for CWS
PWS_boundary <- st_read("Data/PWS/")
str(PWS_boundary)
#plot(st_geometry(PWS_boundary)) #takes a while but works!

#join consolidation data with boundary data so I only have to work with boundaries I actually need
Cases <- read_csv("Outputs/CompiledApril20.csv")
Cases <- left_join(Cases, Joined_consolidatedsystems_censustract2020, by = c("SystemID" = "PWSID")) #first add consolidated systems with census data in too
PWS_boundary$SABL_PWSID <- as.factor(PWS_boundary$SABL_PWSID)
PWS_boundary$WATER_SYST <- as.factor(PWS_boundary$WATER_SYST)
TEST <- PWS_boundary
TEST$duplicated <- duplicated(TEST$SABL_PWSID) #There are duplicates because a few have jurisdictional and water service area boundaries. Try removing jurisidctional ones and make sure don't loose any matches. Doesnot so just do this with full data
PWS_boundary <- PWS_boundary %>% filter(BOUNDARY_T != "Jurisdictional")
Cases <- left_join(Cases, PWS_boundary, by = c("Receiving_System_ID" = "SABL_PWSID"))
Cases <- Cases[,-c(67:97)] #Reduce data frame a bit to be mroe manageable, don't need most of the boundary variables just added
#rename variables for clarity on which spatial goes which system system
Cases <- Cases %>% rename(geometry_receiving = geometry.y, geometry_consolidated = geometry.x, SHAPE_Area_receiving = SHAPE_Area, SHAPE_Leng_receiving = SHAPE_Leng)

#areal interpolation (https://crd230.github.io/lab3.html)
#make data set of just mostly boundaries to play with
Recieving_boundaries <- Cases %>% select(Receiving_System_ID, geometry_receiving)
Recieving_boundaries <- Recieving_boundaries %>% unique() #check later but I think this removed duplicates
Recieving_boundaries <- Recieving_boundaries %>% rename(geometry = geometry_receiving)

library(areal)
Recieving_boundaries <- st_as_sf(Recieving_boundaries) #make sf object

#make to be the same crs
Recieving_boundaries <- st_transform(Recieving_boundaries, crs = 26915)
st_crs(Recieving_boundaries) 
Recieving_boundaries <- st_make_valid(Recieving_boundaries)
Demographics_tract <- st_transform(Demographics_tract, crs = 3857)
st_crs(Demographics_tract) 

#now try areal interpolation
Interpolation <- aw_interpolate(Recieving_boundaries, tid = Receiving_System_ID, source = Demographics_tract, sid = GEOID, weight = "total", output = "tibble", extensive = c("Race.estimate.total", "Race.white.alone", "Race.black.alone", "Race.native.alone", "Race.asian.alone", "Race.PI.alone", "Race.someother.alone", "Race.twoplus", "Tenure.estimate.total", "Tenure.owner", "Tenure.renter", "Households.total", "Income.aggregatecount"))

#trouble shooting
#ar_validate(source = Demographics_tract, target = Recieving_boundaries, varList = c("Race.estimate.total", "Race.white.alone", "Race.black.alone", "Race.native.alone", "Race.asian.alone", "Race.PI.alone", "Race.someother.alone", "Race.twoplus", "Tenure.estimate.total", "Tenure.owner", "Tenure.renter", "Households.total", "Income.aggregatecount"), verbose = TRUE)

#now add into cases
Cases <- left_join(Cases, Interpolation, by = "Receiving_System_ID")

#need to clean up all the demographic variables for recieving sytems are .y now and consolidated systems are .x I think

#quick summary stats for fun
mean((na.omit(Cases$Tenure.owner.x))/mean((na.omit(Cases$Tenure.estimate.total.x))))
mean((na.omit(Cases$Tenure.owner.y))/mean((na.omit(Cases$Tenure.estimate.total.y))))

mean((na.omit(Cases$Race.white.alone.x))/mean((na.omit(Cases$Race.estimate.total.x))))
mean((na.omit(Cases$Race.white.alone.y))/mean((na.omit(Cases$Race.estimate.total.y))))

mean((na.omit(Cases$Income.aggregatecount))/mean((na.omit(Cases$Households.total))))
mean(na.omit(Cases$Median.hh.income))
