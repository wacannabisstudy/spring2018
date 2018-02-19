library(dplyr)
library(ggplot2)
library(lubridate)
library(forcats)

options(scipen = 100)


seattle <- read.csv("~/Documents/Cannabis Study/location_org_seattle.csv") %>% 
              rename(locationnum = locationid, locationid = id)
                
seattle_plants <- merge(plants, seattle, by = 'locationid')

seattle_plant_num <- seattle_plants %>% group_by(locationid) %>% 
                          summarize(num_plants = length(plantid))




calyx_growrooms <- read.csv("~/Documents/Cannabis Study/calyx_growrooms.csv")
calyx_inventory_fifteen <- read.csv("~/Documents/Cannabis Study/calyx_inventory_fifteen.csv")
calyx_inventoryconversions <- read.csv("~/Documents/Cannabis Study/calyx_inventoryconversions.csv")
calyx_inventorysamples <- read.csv("~/Documents/Cannabis Study/calyx_inventorysamples.csv")
calyx_inventoryadjustments <- read.csv("~/Documents/Cannabis Study/calyx_inventoryadjustments.csv")
calyx_inventoryrooms <- read.csv("~/Documents/Cannabis Study/calyx_inventoryrooms.csv")
calyx_inventory_statistics <- read.csv("~/Documents/Cannabis Study/calyx_inventory_statistics.csv")
calyx_inventorytransfers <- read.csv("~/Documents/Cannabis Study/calyx_inventorytransfers.csv")
calyx_labresults_samples <- read.csv("~/Documents/Cannabis Study/calyx_labresults_samples.csv")
calyx_tax_obligation_reports <- read.csv("~/Documents/Cannabis Study/calyx_tax_obligation_reports.csv")
calyx_plantslog <- read.csv("~/Documents/Cannabis Study/calyx_plantslog.csv")


calyx_derivatives <- read.csv("~/Documents/Cannabis Study/calyx_derivatives.csv") %>%
                            rename(derivativeid = id)
                  
calyx_plants <- read.csv("~/Documents/Cannabis Study/calyx_plants.csv") %>%
                      rename(plantid = id)
#Convert date field
calyx_plants$sessiontime <- as_datetime(calyx_plants$sessiontime)
calyx_plants$month <- month(calyx_plants$sessiontime)
calyx_plants$year <- year(calyx_plants$sessiontime)
                  

calyx_plant_derivative <- merge(calyx_derivatives, calyx_plants, by = 'plantid')

calyx_weights <- calyx_plant_derivative %>% group_by(plantid) %>% 
                      summarize(derivatives = length(derivativeid), tot_weight = sum(dweight),
                                avg_weight = sum(dweight) / length(derivativeid))

ggplot(calyx_weights, aes(x = avg_weight)) + geom_histogram(binwidth = 50)


plant_weights <- calyx_plant_derivative %>% group_by(year, month, inventorytype) %>% 
                      summarize(weight_type = sum(dweight), num_plants = length(unique(plantid)),
                                avg_yield = weight_type / num_plants)


grow_type <- read.csv("~/Documents/Cannabis Study/grow_type.csv")
calyx_grow_type <- grow_type[c(2859,3138),]




count <- calyx_derivatives %>% group_by(plantid) %>% summarize(count = length(derivativeid))

calyx_derivatives$sessiontime <- as_datetime(calyx_derivatives$sessiontime)
one_plant <- calyx_derivatives[which(calyx_derivatives$plantid == '2152920637158672'),]
one_transaction <- calyx_derivatives[which(calyx_derivatives$transactionid == '52133513'),]

calyx_transactions <- calyx_derivatives %>% group_by(transactionid, inventorytype) %>%
                        summarize(dweight = sum(dweight), dwholeweight = mean(dwholeweight))

ggplot(calyx_transactions, aes(x = fct_reorder(factor(inventorytype), dweight), y = dweight)) + 
  geom_bar(stat='identity') 





one_plant_wet <- one_plant[which(one_plant$inventorytype == 29),]

harvests <- calyx_derivatives %>% group_by(plantid) %>% 
                summarize(harvests = length(unique(transactionid)))



dry_plants <- calyx_derivatives[(which(calyx_derivatives$inventorytype == 6 & calyx_derivatives$deleted == 0)),]

dry_plants %>% group_by(transactionid) %>% summarize(total_flower = sum(dweight), yield = total_flower/length(unique(plantid)))

dry_plants %>% group_by(plantid) %>% summarize(yield = total_flower/length(unique(plantid)))







flower <- read.csv("~/Documents/Cannabis Study/inventory_flower.csv")
derivatives_flower <- read.csv("~/Documents/Cannabis Study/derivatives_flower.csv")

dry_weight <- derivatives_flower %>% group_by(transactionid) %>% summarize(weight = sum(dweight)) 


      
                                               