# making them maps
require(ggplot2)
require(ggmap)
library(ggsn)

register_google(key = "AIzaSyDXb9xA_MSl7GzYF5Vcl9sedNZrawnNX1Q")

site_info <- read_csv("./raw_data/SBC_siteinformation.csv") %>% filter(mortality == 1) %>% 
  select(site_code, latitude_degrees, longitude_degrees)

base <- get_map(location=c(-123.75,48.5), zoom=9, maptype = "terrain-background")
quartz()
study_sites <- ggmap(base) +
  theme(panel.border = element_rect(fill = NA, colour= "black", size = 1)) +
  geom_point(aes(y = latitude_degrees, x = longitude_degrees, fill = site_code), data = site_info, size = 4, alpha = 0.9, pch = 21,
             color = "black") +
  theme(legend.position = "none") +
  labs(x = "Longitude (degrees)", y = "Latitude (degrees)")
study_sites

ggsave(filename = "./outputs/sitemap.png", study_sites)

angle_sites <- read_csv("./raw_data/SBC_siteinformation.csv") %>% filter(angles == 1) %>% 
  select(site_code, latitude_degrees, longitude_degrees)

base <- get_map(location=c(-123.25,48.7), zoom=9, maptype = "terrain-background")
quartz()
study_sites <- ggmap(base) +
  theme(panel.border = element_rect(fill = NA, colour= "black", size = 1)) +
  geom_point(aes(y = latitude_degrees, x = longitude_degrees, fill = site_code), data = angle_sites, size = 4, alpha = 0.9, pch = 21,
             color = "black") +
  theme(legend.position = "none") +
  labs(x = "Longitude (degrees)", y = "Latitude (degrees)")
study_sites

ggsave(filename = "./outputs/sitemap_angles.png", study_sites)

infauna_sites <- read_csv("./raw_data/SBC_siteinformation.csv") %>% filter(infauna == 1) %>% 
  select(site_code, latitude_degrees, longitude_degrees)

base <- get_map(location=c(-123.75,48.5), zoom=9, maptype = "terrain-background")
quartz()
study_sites <- ggmap(base) +
  theme(panel.border = element_rect(fill = NA, colour= "black", size = 1)) +
  geom_point(aes(y = latitude_degrees, x = longitude_degrees, fill = site_code), data = infauna_sites, size = 4, alpha = 0.9, pch = 21,
             color = "black") +
  theme(legend.position = "none") +
  labs(x = "Longitude (degrees)", y = "Latitude (degrees)")
study_sites

ggsave(filename = "./outputs/sitemap_infauna.png", study_sites)
