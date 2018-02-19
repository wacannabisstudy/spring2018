


harvest_weights <- calyx_derivatives %>% group_by(transactionid, inventorytype) %>% summarize(weight = sum(dweight))

harvest_weights$type <- plyr::mapvalues(harvest_weights$inventorytype, from = c(6, 9, 27, 29), to = c('Flower', 'Other', 'Waste', 'Wet Flower'))

ggplot(harvest_weights, aes(x = fct_reorder(type, weight), y = weight, fill = type)) + geom_bar(stat = 'identity') +
  xlab("Inventory Type") + ylab("Total Weight(g)") + scale_fill_discrete(name = "Type") +
  ggtitle("Harvest Output for Tier 2 Indoor Grower") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))  + theme(text = element_text(size = 15)) 
  
#1,113 Plants
harvest_weights %>% group_by(type) %>% summarize(weight = sum(weight))

harvest_table <- harvest_weights %>% group_by(type) %>% summarize(weight = sum(weight), avg_weight = sum(weight) / 1113)

##Look at inventory
calyx_inventory <- read.csv("~/Documents/Cannabis Study/calyx_inventory.csv")
codes <- read.csv("~/Documents/Cannabis Study/inventory_codes.csv")

calyx_inventory <- merge(calyx_inventory, codes, by = 'inventorytype')

inventory_count <- calyx_inventory %>% group_by(inventorycode) %>% summarize(count = length(id))


ggplot(inventory_count, aes(x = fct_reorder(inventorycode, count), y = count, fill = inventorycode)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 15, hjust = 1)) + coord_flip() + 
  ylab("Quantity") + xlab("Inventory Type")  +
  ggtitle("Quantity in Inventory for Tier 2 Indoor Producer") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))  + theme(text = element_text(size = 15)) + 
  theme(legend.position="none")






#look at plantslog
strains <- calyx_plantslog %>% group_by(strain) %>% summarize(count = length(id))





#Look at entire plants data set
plants <- read.csv("/Volumes/Backup/WALCB Data/plants_subset_allyears.csv") %>%
                rename(plantid = id, locationid = location)

plants_locations <- merge(plants, locations, by = 'locationid')

plants_locations$locationtype <- plyr::mapvalues(plants_locations$locationtype, from = c(4, 5, 6), to = c(1, 2, 3))  

plants_locations <- filter(plants_locations, locationtype < 4)

plants_firm <- plants_locations %>% group_by(locationid, locationtype) %>% summarize(num_plants = length(plantid)) 

ggplot(plants_firm, aes(x = factor(locationtype), y = num_plants, fill = factor(locationtype))) + geom_boxplot(notch = T) + 
  ylim(c(0, 20000)) + xlab("License Tier") + ylab("Number of Plants") + theme_bw() + 
  ggtitle('Numbe of Plants by License Tier') + 
  theme(plot.title = element_text(hjust = 0.5))  + theme(text = element_text(size = 15)) + scale_fill_discrete(name = 'Tier')
  


#Look at number of plants over time
plants_locations$sessiontime <- as_datetime(plants_locations$sessiontime)

plants_locations$year <- year(plants_locations$sessiontime)
plants_locations$month <- month(plants_locations$sessiontime)

plants_time <- plants_locations %>% group_by(locationtype, locationid, year, month) %>% summarize(num_plants = length(plantid)) 

plants_time <- plants_time[which(plants_time$year > 2013 & plants_time$year < 2018),]

ggplot(plants_time, aes(x = factor(month), y = num_plants, fill = factor(locationtype))) + 
  geom_bar(stat = 'identity') + facet_wrap(~year, nrow = 1) + xlab("Month") + ylab("Number of Plants") + 
  theme(text = element_text(size = 15)) + scale_fill_discrete(name = 'Tier') + theme_bw() +
  ggtitle("Number of Plants per Month by License Tier") + theme(plot.title = element_text(hjust = 0.5))
  
