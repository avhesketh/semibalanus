## April 2020 community analysis

infauna <- read_csv("infauanal.csv")

infauna <- infauna %>% 
  select(-6) %>% 
  rename(species = Species, site = Site, plot = Plot)

infauna$species <- gsub("Littorina scutulata/plena adult","Littorina scutulata", infauna$species)
infauna$species <- gsub("Littorina scutulata/plena","Littorina scutulata", infauna$species)
infauna$species <- gsub("Insecta","Formicidae", infauna$species)
infauna$species <- gsub("Pagurus hirsuitusculus","Pagurus hirsutiusculus", infauna$species)
infauna$species <- gsub("Nereis vexillosa", "Nereidae", infauna$species)

# add in zero cases where data were not explicitly recorded
infauna_zd <- infauna %>% 
  complete(nesting(site, plot), species) %>% 
  filter(species != "Flatworm")

# replace NA with zero
infauna_zd[is.na(infauna_zd)] <- 0

# now data are clean!


