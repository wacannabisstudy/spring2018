#Load libraries
library(ggplot2)
library(dplyr)

#Set scientific notation threshold
options(scipen = 100)

#Import data, subset desired columns, rename variables for consistency
plants <- read.csv("~/Documents/Cannabis Study/plants_2016.csv") %>%
              select(id, location) %>%
                rename(plantid = id, locationid = location)

locations <- read.csv("~/Documents/Cannabis Study/locations_full.csv") %>%
                select(orgid, locationtype, licensenum, id) %>%
                  rename(locationid = id)

#organizations <- read.csv("~/Documents/Cannabis Study/organizations_full.csv")

#grow_type <- read.csv("~/Documents/Cannabis Study/grow_type.csv") %>%
#                select(License, Grow.Type, Canopy) %>%
#                  rename(licensenum = License, grow_type = Grow.Type, canopy = Canopy)

#grow_type <- grow_type[which(grow_type$licensenum %in% locations$licensenum),]


#Join the data sets
#locations_type <- merge(locations, grow_type, by = 'licensenum')

locations_plants <- merge(plants, locations, by = "locationid")

#test <- distinct(locations_plants)


#Grouping by licensenum or locationid produce the same results here, as expected
plants_tier  <-  locations_plants %>% 
                    group_by(licensenum, locationtype) %>% 
                      summarize(plants = length(plantid))

#boxplots
ggplot(plants_tier, aes(x = factor(locationtype), y = plants, fill = factor(locationtype))) + geom_boxplot(alpha=0.5)

ggplot(plants_tier, aes(x = factor(locationtype), y = plants, fill = factor(locationtype))) + 
  geom_boxplot(alpha=0.5, notch = T) + ylim(c(0,10000))

#Violin plots
ggplot(plants_tier, aes(x = factor(locationtype), y = plants, fill = factor(locationtype))) + 
  geom_violin(trim = F, alpha = 0.5) + ylim(c(0,5000))

ggplot(plants_tier, aes(x = factor(locationtype), y = plants, fill = factor(locationtype))) + 
  geom_violin(trim = F, alpha = 0.5) + ylim(c(0,5000)) + geom_jitter(height= 0, width = 0.1)

#Density plots
ggplot(plants_tier, aes(x = plants, fill = factor(locationtype), color = factor(locationtype))) + 
  geom_density(alpha = 0.1)

ggplot(plants_tier, aes(x = plants, fill = factor(locationtype), color = factor(locationtype))) + 
  geom_density(alpha = 0.1) + xlim(c(0, 10000))



#outlier is orgid, 788
#organizations[which(organizations$orgid == 788),]
#            orgname           orgid    orgactive orglicense  fifteendaystart  orgstatus
#146 ARTIZEN CANNABIS COMPANY   788         1      603355143    1422350105         1








grow_type <- read.csv("~/Documents/Cannabis Study/grow_type.csv") %>%
                select(License, Grow.Type, Canopy) %>% distinct() %>%
                 rename(licensenum = License, grow_type = Grow.Type, canopy = Canopy)

grow_type <- grow_type[-which(is.na(grow_type$grow_type)),] 
grow_type <- grow_type[which(grow_type$licensenum %in% locations$licensenum),]
grow_type <- grow_type[-which(grow_type$canopy == 0),]           


#grow_type <- grow_type %>% arrange(grow_type, licensenum, desc(canopy)) %>% 
#               group_by(licensenum, grow_type) %>% 
#                     filter(n()==1)



#Merge the grow_type data set with the locations_plants data set
plants_grow_type <- merge(locations_plants, grow_type, by = "licensenum")



ggplot(plants_grow_type, aes(x = factor(grow_type), y = canopy, fill = factor(grow_type))) + geom_boxplot(alpha=0.5)

#You can see the different tiers in this plot
ggplot(plants_grow_type, aes(x = factor(grow_type), y = canopy, fill = factor(grow_type))) + geom_violin(alpha = 0.5, trim = F) 

#Not pretty to look at but shows that Indoor tends to have a smaller canopy
ggplot(plants_grow_type, aes(x = factor(grow_type), y = canopy, fill = factor(grow_type))) + geom_boxplot(alpha = 0.5) + 
  facet_wrap(~locationtype)


#
canopy  <-  plants_grow_type %>% 
                    group_by(locationtype, grow_type) %>% 
                    summarize(plants = length(plantid))



