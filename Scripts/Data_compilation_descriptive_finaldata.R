#Data compilation for descriptive consolidations analysis with updated final data 

#Load libraries
library(tidyverse)

#load data
Master <- read_csv("Data/Master_copy_6.22.22.csv")
Master <- Master[-226,-c(36:151)]
Consolidatedextra <- read_csv("Data/Consolidatedsystems_extradata_SWRCB.csv")
Receivingextra <- read_csv("Data/Recievingsystem_extradata_EPA.csv")

#Fix types necessary for joining
#PWSID as factor
Master$SystemID <- as.factor(Master$SystemID)
Master$Receiving_System_ID <- as.factor(Master$Receiving_System_ID)
Consolidatedextra$PWSID <- as.factor(Consolidatedextra$PWSID)
Receivingextra$PWS.ID <- as.factor(Receivingextra$PWS.ID)

#Add in extra data for consolidated systems (namely source)
#reduce and rename variables to add into master with join, combine the state and federal primary source fields to reduce NAs
Consolidatedextra$Consolidatedsystem_primary_source_combined <- ifelse(is.na(Consolidatedextra$D_FED_PRIM_SRC_CD), Consolidatedextra$D_ST_PRIM_SRC_CD, Consolidatedextra$D_FED_PRIM_SRC_CD) 
Consolidatedextra_small <- Consolidatedextra %>% select(PWSID, Consolidatedsystem_primary_source_combined)
Consolidatedextra_small <- Consolidatedextra_small[!duplicated(Consolidatedextra_small$PWSID), ] #remove duplicates
Master <- left_join(Master, Consolidatedextra_small, by = c("SystemID" = "PWSID")) #still thirty or so that are NA because are still active systems (ie I didn't ask SWRCB for data for these systems) so try to add remaining ones using 2021 epa data

Activecosnolidatedsystems <- Receivingextra %>% select('PWS ID', 'Primary Source Code')
Activecosnolidatedsystems <- rename(Activecosnolidatedsystems, PWSID = `PWS ID`)
Activecosnolidatedsystems <- rename(Activecosnolidatedsystems, EPA_2021_source = 'Primary Source Code')

Master <- left_join(Master, Activecosnolidatedsystems, by = c("SystemID" = "PWSID"))

Master$Consolidatedsystem_primary_source_combinedFINAL <- ifelse(is.na(Master$Consolidatedsystem_primary_source_combined), Master$EPA_2021_source, Master$Consolidatedsystem_primary_source_combined) 
Master$Consolidatedsystem_primary_source_combinedFINAL[is.na(Master$Consolidatedsystem_primary_source_combinedFINAL)] = "GW"
#Make five NAs groundwater (all private wells/state smalls plus kettleman city elementary school which I checked in EPA back records (there was a space in the PWSID for that one that I sent the board so didn't get it back I think))

#add receiving extra data in (source and population)
Receivingextra_small <- Receivingextra %>% select('PWS ID', 'Primary Source Code', 'Population Served Count', 'PWS Type')
Receivingextra_small <- rename(Receivingextra_small, PWSID = `PWS ID`)
Receivingextra_small <- rename(Receivingextra_small, Receiving_2021_source = 'Primary Source Code')
Receivingextra_small <- rename(Receivingextra_small, Receiving_2021_pop = 'Population Served Count')
Receivingextra_small <- rename(Receivingextra_small, Receiving_2021_systemtype = 'PWS Type')

Master <- left_join(Master, Receivingextra_small, by = c( "Receiving_System_ID" =  "PWSID"))

#Filter to remove cases tagged for removal
Master$`Remove?` <- as.factor(Master$`Remove?`)
Master <- Master %>% filter(`Remove?` == "No") #This removes 18 cases, note this in analysis write up and paper, describe cases and rationale briefly

#Add in distance analysis data. NA means not analyzed (see notes distance_analysis_dataandnotes excel sheet)


#write CSV
write.csv(Master, file = "Outputs/Compiledfinaldata.csv", row.names = FALSE)
