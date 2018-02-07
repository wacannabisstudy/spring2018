library(dplyr)
library(ggplot2)

options(scipen = 100)

plants <- read.csv("~/CMU/Cannabis Study/plants_twoday.csv", stringsAsFactors = F)

organizations <- read.csv("~/CMU/Cannabis Study/organizations_full.csv", stringsAsFactors = F)
  
locations <- read.csv("~/CMU/Cannabis Study/locations_full.csv", stringsAsFactors = F)

active <- read.csv("~/CMU/Cannabis Study/activelicense.csv", stringsAsFactors = F)

locations_active <- merge(locations, active, by.x = 'licensenum', by.y = 'License..')


locations_active %>% 
  group_by(locationtype) %>% 
  summarize(tier1 = length(which(PrivDesc == 'MARIJUANA PRODUCER TIER 1')),
            tier2 = length(which(PrivDesc == 'MARIJUANA PRODUCER TIER 2')),
            tier3 = length(which(PrivDesc == 'MARIJUANA PRODUCER TIER 3')))


locations_plants <- merge(plants, locations, by.x = "location", by.y = "id")




plants_tier  <-     locations_plants %>% 
                      group_by(locationtype) %>% 
                        summarize(plants = length(id))


ggplot(plants_tier, aes(x = locationtype, y = plants)) + geom_bar(stat = 'identity')
