#Searching for additional consolidaiton cases using EPA SDWIS data

#Load data and libraries
Data2015 <- read.csv("Data/2015 active CWS in CA Q1 from EPA records.csv")
Data2021 <- read.csv("Data/2021 active CWS in CA Q3 from EPA records.csv")
library(dplyr)
library(tidyverse)

#Reduce and join data
Data2015 <- Data2015[,c(1,2,5,8)]
Data2015$PWS.Type.2015 <- Data2015$PWS.Type
Data2015 <- Data2015[,c(1,2,4,5)]
Data2021 <- Data2021[,c(1,2,5,8)]
Data_joined <- full_join(Data2015, Data2021, by="PWS.ID")

#Deactivated systems Q1 2015 to Q3 2021
Deactivated <- Data_joined %>% filter(is.na(PWS.Name.y))
Newsystems <- Data_joined %>% filter(is.na(PWS.Name.x))
Remain <- Data_joined %>% filter(!is.na(PWS.Name.x)) %>% filter(!is.na(PWS.Name.y))

write.csv(Deactivated, "Outputs/Deactivated2015to2021.csv")
write.csv(Newsystems, "Outputs/Newsystems2015to2021.csv")

#Compare names from each data set
Data_joined_small <- Remain
library(stringdist)
Data_joined_small$match <- stringsim(Data_joined_small$PWS.Name.x, Data_joined_small$PWS.Name.y)

summary(Data_joined_small$match)
lowmatch <- Data_joined_small %>% filter(match < 0.5) 

write.csv(lowmatch, "Outputs/lownamematch2015to2021.csv")#REVIEW ALL OF THESE, look at NAs too

#Compare ownertype
Owner2015 <- read.csv("Data/2015CWSownertype.csv")
Owner2021 <- read.csv("Data/2021CWSownertype.csv")

Owner_joined <- full_join(Owner2015, Owner2021, by="PWS.ID")
Owner_joined <- Owner_joined %>% filter(complete.cases(Owner_joined))
Owner_joined$Owner.Type.x <- as.factor(Owner_joined$Owner.Type.x)
Owner_joined$Owner.Type.y <- as.factor(Owner_joined$Owner.Type.y)

Diffowner <- Owner_joined %>% filter(Owner_joined$Owner.Type.x != Owner_joined$Owner.Type.y)

write.csv(Diffowner, "Outputs/Ownershipchange2015to2021.csv")
