# making them maps
require(ggplot2)
require(ggmap)
library(ggsn)
library(RColorBrewer)

register_google(key = "AIzaSyDXb9xA_MSl7GzYF5Vcl9sedNZrawnNX1Q")

site_info <- read_csv("./raw_data/SBC_siteinformation.csv") %>% filter(mortality == 1) %>% 
  select(site_code, latitude_degrees, longitude_degrees)
View(site_info)
base <- get_map(location=c(-123.75,48.5), zoom=9, maptype = "terrain-background")

set.seed(20)
study_sites_south <- ggmap(base) +
  theme(panel.border = element_rect(fill = NA, colour= "black", size = 1)) +
  geom_point(aes(y = latitude_degrees, x = longitude_degrees, fill = site_code), data = site_info, size = 4, alpha = 0.9, pch = 21,
             color = "black", stroke = 1.3) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(13)) +
  theme(legend.position = "none") +
  labs(x = "Longitude (degrees)", y = "Latitude (degrees)") +
  geom_text(aes(y = 48.3, x = -124, label = "Strait of Juan de Fuca", angle = 340), size = 8, fontface = "italic") +
  geom_text(aes(y = 48.8, x = -124.1, label = "Vancouver Island"), angle = 340, size = 8, fontface = "italic") +
  geom_text_repel(aes(y = latitude_degrees, x = longitude_degrees, label = site_code), data = site_info, force = 1.5, 
                  direction = "both", box.padding = 0.7, size = 8) +theme(axis.ticks.length=unit(-0.25, "cm")) +
  theme(axis.text.y = element_text(margin = margin(l = 20, r = -55), size = 17)) +
  theme(axis.text.x = element_text(vjust = 12, size = 17))+
  theme(axis.text.x = element_text(size = 17, colour = "gray10")) +
  theme(axis.text.y = element_text(size = 17, colour = "gray10")) +
  ylim(c(48, 49)) +
  theme(axis.ticks.length.x =unit(-0.5, "cm"))
study_sites_south

ggsave(filename = "./outputs/sitemap_south.png", study_sites_south)

base <- get_map(location=c(-125.7,50), zoom=6, maptype = "terrain-background", source = "stamen")

study_sites_wide <- ggmap(base) +
  theme(panel.border = element_rect(fill = NA, colour= "black", size = 1)) +
  geom_point(aes(y = latitude_degrees, x = longitude_degrees, fill = site_code), data = site_info, size = 2, alpha = 0.9, pch = 21,
             color = "black", stroke = 1.2) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(13)) +
  theme(legend.position = "none") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)) + 
  labs(x = "Longitude (degrees)", y = "Latitude (degrees)") +
  geom_text(aes(y = 49.5, x = -130.1, label = "Pacific Ocean"), size = 6, fontface = "italic") +
  geom_text(aes(y = 52, x = -126, label = "BC"), size = 6, fontface = "italic") +
  geom_text(aes(y = 46.8, x = -123, label = "WA"), size = 6, fontface = "italic")

study_sites_wide
ggsave(filename = "./outputs/sitemap_wide.png", study_sites_wide)




base <- get_map(location=c(-128.06,51.57), zoom=10, maptype = "terrain-background", source = "stamen")
study_sites_north <- ggmap(base) +
  theme(panel.border = element_rect(fill = NA, colour= "black", size = 1)) +
  geom_point(aes(y = latitude_degrees, x = longitude_degrees, fill = site_code), data = site_info, size = 4, alpha = 0.9, pch = 21,
             color = "black", stroke = 1.2) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(13)) +
  theme(legend.position = "none") +
  labs(x = "Longitude (degrees)", y = "Latitude (degrees)") +
  geom_text(aes(y = 51.6, x = -128, label = "Calvert Island"), size = 8, fontface = "italic") +
  geom_text_repel(aes(y = latitude_degrees, x = longitude_degrees, label = site_code), data = site_info, force = 1.5, 
                  direction = "both", box.padding = 0.7, size = 8) + 
  theme(axis.ticks.length.y =unit(-0.25, "cm")) +
  theme(axis.ticks.length.x =unit(-0.25, "cm")) +
  theme(axis.text.y = element_text(margin = margin(l = 15, r = -45), size = 17)) +
  theme(axis.text.x = element_text(vjust = 10, size = 15))+
  theme(axis.text.x = element_text(size = 17, colour = "gray10")) +
  theme(axis.text.y = element_text(size = 17, colour = "gray10")) +
  ylim(c(51.4, 51.75)) +
  xlim(c(-128.3, -127.7))
  
study_sites_north
ggsave(filename = "./outputs/sitemap_north.png", study_sites_north)

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

