library(stringr)

#Import Locations
locations <- read.csv("~/Documents/Cannabis Study/locations_full.csv") %>%
  select(orgid, locationtype, licensenum, id, producer, deleted) %>%
  rename(locationid = id)
#Subset for producers
locations <- locations[which(locations$producer == 1),]
locations <- locations[which(locations$deleted == 0),]

#Import grow type 
grow_type <- read.csv("~/Documents/Cannabis Study/grow_type.csv") %>%
  select(License, Grow.Type, Canopy) %>% distinct() %>%
  rename(licensenum = License, grow_type = Grow.Type, canopy = Canopy)

grow_type <- grow_type[-which(is.na(grow_type$grow_type)),]

#Join tables
locations_type <- merge(locations, grow_type, by = 'licensenum')

locations_type$tier <- plyr::mapvalues(locations_type$locationtype, from = c(4, 5, 6), to = c(1, 2, 3))

locations_type$licensed <- plyr::mapvalues(locations_type$tier, from = c(1, 2, 3), to = c(2000, 10000, 30000))

locations_type$unused <- locations_type$licensed - locations_type$canopy

locations_type$harvests <- plyr::mapvalues(locations_type$grow_type, 
                              from = c('Indoor', 'Indoor/Outdoor', 'Indoor/Greenhouse', 'Outdoor/Greenhouse', 'Greenhouse', 'Indoor/Outdoor/Greenhouse', 'Outdoor'), 
                              to = c(5, 2, 5, 2, 2, 2, 2))

locations_type$potential <- as.numeric(as.character(locations_type$harvests)) * 40 * locations_type$unused


locations_type <- locations_type[-which(locations_type$canopy < 100),]
locations_type <- locations_type[-which(locations_type$potential < 0),]
locations_type$utilization <- locations_type$canopy / locations_type$licensed

dupes <- locations_type$licensenum[which(duplicated(locations_type$licensenum))]

no_dupes <- locations_type[-which(locations_type$licensenum %in% dupes),]

#Sales data in weight from LCB data portal
monthly_flower <- read.csv("~/Documents/Cannabis Study/Presentation/monthly_flower.csv")
monthly_flower$year <- str_sub(monthly_flower$Sales.Year.Month, 1, 4)
monthly_flower$grams <- monthly_flower$Usable.Weight.Pounds * 453.592
monthly_flower %>% group_by(year) %>% summarize(grams = sum(grams))

#Sales data in weight from LCB portal
monthly_extract <- read.csv("~/Documents/Cannabis Study/Presentation/monthly_extract.csv")
monthly_extract$year <- str_sub(monthly_extract$Sales.Year.Month, 1, 4)
monthly_extract$grams <- monthly_extract$Usable.Weight.Pounds * 453.592
monthly_extract %>% group_by(year) %>% summarize(grams = sum(grams))


ggplot(no_dupes, aes(x = utilization, fill = factor(tier))) + geom_density(alpha = 0.25) + 
  xlab("Utilization of Canopy (%)") + ylab("Density") + ggtitle("Utilization of Licensed Square Footage") +
  theme(text = element_text(size = 15)) + scale_fill_discrete(name = 'Tier') + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#how many licenses are out there
locations_type %>% group_by(tier) %>% summarize(num_license = length(licensenum))

#what is utilization rate
sum(no_dupes$canopy) / sum(no_dupes$licensed)

#how many grow types are out there
locations_type %>% group_by(grow_type) %>% summarize(count = length(licensenum))



locations_type$grow_type <- factor(locations_type$grow_type,levels = c('Greenhouse', 'Indoor/Greenhouse', 'Indoor/Outdoor/Greenhouse', 'Outdoor/Greenhouse', 'Outdoor', 'Indoor/Outdoor', 'Indoor'))

ggplot(locations_type, aes(x = grow_type, fill = factor(tier))) + geom_bar(stat = 'count') + xlab("Growing Method") + ylab("Frequency") +
  ggtitle("Number of Producers by Growing Method and License Tier") + theme_bw() + theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  scale_fill_discrete(name = "Tier") + theme(plot.title = element_text(hjust = 0.5))  + theme(text = element_text(size = 15))




indoor <- locations_type[which(locations_type$grow_type == 'Indoor'),]

ggplot(indoor, aes(x=canopy, fill = factor(tier))) + geom_density(alpha = 0.6) + xlab('Canopy') + ylab('Density') + 
  theme_bw() + ggtitle('Canopy Usage for Indoor Only Growers')






