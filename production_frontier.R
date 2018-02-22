library(dplyr)
library(ggplot2)
library(lubridate)




##Use all plants, not just those that were harvested
tier3 <- merge(locations[locations$locationtype == 3 | locations$locationtype == 6,2:4], 
               grow_type[grow_type$grow_type == 'Outdoor',1:2], by = 'licensenum')

tier3_plants <- merge(tier3, plants, by = 'locationid')


tier3_plants$sessiontime <- as_datetime(tier3_plants$sessiontime)
tier3_plants$month <- month(tier3_plants$sessiontime)
tier3_plants$year <- year(tier3_plants$sessiontime)


tier3_production <- tier3_plants %>% group_by(licensenum, locationid, year) %>% summarize(num_plants = length(plantid))

ggplot(tier3_production, aes(x = factor(year), y = num_plants, color = factor(licensenum))) + 
  geom_point(size = 3, alpha = 0.5) + theme_bw() + theme(legend.position = 'none') + 
  xlab("Year") + ylab("Number of Plants") + ggtitle("Number of Plants per Producer Year",
                                                    subtitle = "Tier 3 (30,000 sq ft.) Producers")

ggplot(tier3_production, aes(x = factor(year), y = num_plants)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = 'none') + 
  xlab("Year") + ylab("Number of Plants") + ggtitle("Distribution of Plants Produced per Year",
                                                    subtitle = "Tier 3 (30,000 sq ft.) Producers")

ggplot(tier3_production, aes(x = num_plants)) + 
  geom_histogram(bins = 274) + theme_bw() + theme(legend.position = 'none') + 
  xlab("Year") + ylab("Number of Plants") + ggtitle("Distribution of Plants per Producer Year",
                                                    subtitle = "Tier 3 (30,000 sq ft.) Producers")


ggplot(tier3_production, aes(x = num_plants)) + 
  geom_density(alpha = 0.5, adjust = 1/1, fill = "seagreen", color = "midnightblue") + 
  theme_bw() + xlab("Plant Production per Year") + ylab("Number of Plants") + 
  ggtitle("Distribution of Plants per Producer Year", subtitle = "Tier 3 (30,000 sq ft.) Producers")






##Use only those plants that were harvested, where harvested plants go into the 
#plants derivatives table

harvested_plants <- plants[which(plants$plantid %in% derivatives_flower$plantid),]


tier3 <- merge(locations[locations$locationtype == 3 | locations$locationtype == 6,2:4], 
               grow_type[grow_type$grow_type == 'Outdoor',1:2], by = 'licensenum')

tier3_plants <- merge(tier3, plants, by = 'locationid')


tier3_plants$sessiontime <- as_datetime(tier3_plants$sessiontime)
tier3_plants$month <- month(tier3_plants$sessiontime)
tier3_plants$year <- year(tier3_plants$sessiontime)


tier3_production <- tier3_plants %>% group_by(licensenum, locationid, year) %>% summarize(num_plants = length(plantid))

ggplot(tier3_production, aes(x = factor(year), y = num_plants, color = factor(licensenum))) + 
  geom_point(size = 3, alpha = 0.5) + theme_bw() + theme(legend.position = 'none') + 
  xlab("Year") + ylab("Number of Plants") + ggtitle("Number of Plants per Producer Year",
                                                    subtitle = "Tier 3 (30,000 sq ft.) Producers")

ggplot(tier3_production, aes(x = factor(year), y = num_plants)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = 'none') + 
  xlab("Year") + ylab("Number of Plants") + ggtitle("Distribution of Plants Produced per Year",
                                                    subtitle = "Tier 3 (30,000 sq ft.) Producers")

ggplot(tier3_production, aes(x = num_plants)) + 
  geom_density(alpha = 0.5, adjust = 1/1, fill = "seagreen", color = "midnightblue") + 
  theme_bw() + xlab("Plant Production per Year") + ylab("Number of Plants") + 
  ggtitle("Distribution of Plants per Producer Year", subtitle = "Tier 3 (30,000 sq ft.) Producers")



##











