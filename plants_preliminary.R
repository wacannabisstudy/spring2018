library(dplyr)

#Set length 20 as limit use scientific notation
options(scipen = 20)

#Note the import line has to start with Volumes for the external drive
plantslog <- read.csv('~/Documents/Cannabis Study/plantslog_twoday.csv')
plantderivatives <- read.csv('~/Documents/Cannabis Study/plantderivatives_twoday.csv')


#Group by id which is PK (I think)
summary.id <- plantderivatives %>% 
                group_by(id) %>%
                  summarize(transactionid = length(unique(transactionid)), plantid = length(unique(plantid)), 
                            sessiontime = length(unique(sessiontime)), deleted = length(unique(deleted)),
                            created = length(unique(created)), inventorytype = length(unique(inventorytype)), 
                            dweight = length(unique(dweight)), daccountedfor = length(unique(daccountedfor)), 
                            inventoryid = length(unique(inventoryid)), dwholeweight = length(unique(dwholeweight)), 
                            orgid = length(unique(orgid)), location = length(unique(location)), 
                            droom = length(unique(droom)), addlcollections = length(unique(addlcollections)), 
                            transactionid_original = length(unique(transactionid_original)))



#Group by plantid
summary.plantid <- plantderivatives %>% 
                      group_by(plantid) %>%
                      summarize(transactionid = length(unique(transactionid)), id = length(unique(id)), 
                                sessiontime = length(unique(sessiontime)), deleted = length(unique(deleted)),
                                created = length(unique(created)), inventorytype = length(unique(inventorytype)), 
                                dweight = length(unique(dweight)), daccountedfor = length(unique(daccountedfor)), 
                                inventoryid = length(unique(inventoryid)), dwholeweight = length(unique(dwholeweight)), 
                                orgid = length(unique(orgid)), location = length(unique(location)), 
                                droom = length(unique(droom)), addlcollections = length(unique(addlcollections)), 
                                transactionid_original = length(unique(transactionid_original)))



