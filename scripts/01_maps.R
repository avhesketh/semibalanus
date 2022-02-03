## Site maps: Figure 1 of ms
## Amelia V. Hesketh, January 2022

pkgs <- c("ggspatial", "tidyverse", "RColorBrewer", "ggrepel", "ggmap")
lapply(pkgs, library, character = T)
rm(pkgs)

# input Google key to access map data
register_google(key = "AIzaSyDXb9xA_MSl7GzYF5Vcl9sedNZrawnNX1Q")

# load in site information data to put points on the eventual maps
site_info <- read_csv("./raw_data/SBHW_siteinformation.csv") %>% 
  select(site_code, latitude_degrees, longitude_degrees, mortality, infauna, angles,
         orientation_degrees, solar_elevation, solar_azimuth) %>% 
  mutate_all(~replace_na(.,0)) %>% 
  mutate(shape = case_when(mortality == 1 & infauna == 0 & angles == 0 ~ "M",
                           mortality == 1 & infauna == 1 & angles == 0 ~ "MI",
                           mortality == 0 & infauna == 1 & angles == 0 ~ "I",
                           mortality == 0 & infauna == 0 & angles == 1 ~ "A",
                           mortality == 1 & infauna == 0 & angles == 1 ~ "MA",
                           mortality == 0 & infauna == 1 & angles == 1 ~ "IA",
                           mortality == 1 & infauna == 1 & angles == 1 ~ "MIA")) %>% 
  rename(lat = latitude_degrees, long = longitude_degrees)
  
site.levels <- site_info$site_code

set.seed(20)
site_info <- site_info %>% mutate(site_code = factor(site_code, levels = site.levels), 
                                  full.pal = colorRampPalette(brewer.pal(8, "Dark2"))(17))

#write_csv(site_info, "./clean_data/SBHW_SiteInformation.csv")

# Whole coastal area map
base <- get_map(location=c(-126.5,50), zoom=6, maptype = "terrain-background", source = "stamen")

study_sites_wide <- ggmap(base) +
  theme(panel.border = element_rect(fill = NA, colour= "black", size = 1)) +
  geom_point(aes(y = lat, x = long, fill = site_code), data = site_info, size = 1.2, alpha = 0.9, pch = 21,
             color = "black", stroke = 0.8) +
  scale_fill_manual(values = site_info$full.pal) +
  theme(legend.position = "none") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)) + 
  labs(x = "Longitude (degrees)", y = "Latitude (degrees)") +
  geom_text(aes(y = 51.3, x = -130.4, label = "Pacific Ocean"), size = 6, fontface = "italic") +
  geom_text(aes(y = 53.4, x = -126.5, label = "British Columbia"), size = 6, fontface = "italic") +
  geom_text(aes(y = 46.8, x = -124, label = "Washington"), size = 6, fontface = "italic") +
  annotation_north_arrow(location = "tl", height = unit(1.5, "cm"), width = unit(1.5, "cm"))
study_sites_wide

ggsave(filename = "./outputs/sitemap_wide.png", study_sites_wide)

# Inset A: SW Vancouver Island sites
base <- get_map(location=c(lon = -124.5, lat = 48.6) ,zoom=9, maptype = "terrain-background")

study_sites_west <- ggmap(base) +
  theme(panel.border = element_rect(fill = NA, colour= "black", size = 1)) +
  geom_point(aes(y = lat, x = long, fill = site_code, 
                 color = site_code, pch = shape), data = site_info, size = 6, stroke = 1.5) +
  scale_fill_manual(values = site_info$full.pal) +
  scale_color_manual(values = site_info$full.pal) +
  scale_shape_manual(values = c(2, 0, 14, 16, 17, 15, 8)) +
  geom_point(aes(y = lat, x = long, pch = shape), data = site_info, size = 6, 
             color = "black", alpha = 0.45, stroke = 0.8) +
  theme(legend.position = "none") +
  labs(x = "Longitude (degrees)", y = "Latitude (degrees)") +
  geom_text(aes(y = 48.37, x = -124.3, label = "Strait of Juan de Fuca", angle = 340), size = 8, fontface = "italic") +
  geom_text(aes(y = 48.8, x = -124.2, label = "Vancouver Island"), angle = 345, size = 8, fontface = "italic") +
  geom_text_repel(aes(y = lat, x = long, label = site_code), data = site_info, 
                  direction = "both", size = 8, nudge_y = 0.05, nudge_x = 0.2) +
  theme(axis.ticks.length=unit(-0.25, "cm")) +
  theme(axis.text.y = element_text(margin = margin(l = 20, r = -50), size = 20, color = "grey10")) +
  theme(axis.text.x = element_text(vjust = 10, size = 20, color = "grey10")) +
  theme(axis.title = element_blank()) 
study_sites_west

ggsave(filename = "./outputs/sitemap_west.png", study_sites_west, dpi = 1200)

# Inset B: Calvert Island inset

base <- get_map(location=c(-128.06,51.57), zoom=10, maptype = "terrain-background", source = "stamen")

study_sites_north <- ggmap(base) +
  theme(panel.border = element_rect(fill = NA, colour= "black", size = 1)) +
  geom_point(aes(y = lat, x = long, fill = site_code,
                 color = site_code, pch = shape), data = site_info, size = 6, stroke = 1.5) +
  scale_shape_manual(values = c(2, 0, 14, 16, 17, 15, 8)) +
  geom_point(aes(y = lat, x = long, pch = shape), data = site_info, size = 6, 
             color = "black", alpha = 0.45, stroke = 0.8) +
  scale_fill_manual(values = site_info$full.pal) +
  scale_color_manual(values = site_info$full.pal) +
  labs(x = "Longitude (degrees)", y = "Latitude (degrees)", pch = "Data collected") +
  geom_text(aes(y = 51.55, x = -128, label = "Calvert Island"), size = 8.5, fontface = "italic") +
  geom_text_repel(aes(y = lat, x = long, label = site_code), data = site_info, force = 1.5, 
                  direction = "both", box.padding = 0.9, size = 8.5) + 
  theme(axis.ticks.length.y =unit(-0.25, "cm")) +
  theme(axis.ticks.length.x =unit(-0.25, "cm")) +
  theme(axis.text.y = element_text(margin = margin(l = 15, r = -50), size = 20, colour = "gray10")) +
  theme(axis.text.x = element_text(vjust = 10, size = 20, colour = "gray10"))+
  theme(axis.title = element_blank()) +
  theme(legend.position = "none") +
  #theme(legend.title = element_text(size = 17), legend.text = element_text(size = 15)) +
  ylim(c(51.4, 51.75)) +
  xlim(c(-128.35, -127.75))

study_sites_north
ggsave(filename = "./outputs/sitemap_north.png", study_sites_north, dpi = 1200)

# Inset C: Salish Sea sites
base <- get_map(location=c(lon = -123.25, lat = 48.6) ,zoom=9, maptype = "terrain-background")

study_sites_south <- ggmap(base) +
  theme(panel.border = element_rect(fill = NA, colour= "black", size = 1)) +
  geom_point(aes(y = lat, x = long, fill = site_code, 
                 color = site_code, pch = shape), data = site_info, size = 6, stroke = 1.5) +
  scale_fill_manual(values = site_info$full.pal) +
  scale_color_manual(values = site_info$full.pal) +
  scale_shape_manual(values = c(2, 0, 14, 16, 17, 15, 8)) +
  geom_point(aes(y = lat, x = long, pch = shape), data = site_info, size = 6, 
                            color = "black", alpha = 0.45, stroke = 0.8) +
  theme(legend.position = "none") +
  labs(x = "Longitude (degrees)", y = "Latitude (degrees)") +
  geom_text(aes(y = 48.95, x = -123.2, label = "Strait of Georgia", angle = 340), size = 7, fontface = "italic") +
  geom_text(aes(y = 48.28, x = -124, label = "Strait of Juan de Fuca", angle = 350), size = 5, fontface = "italic") +
  geom_text(aes(y = 48.8, x = -124.2, label = "Vancouver Island"), angle = 350, size = 6, fontface = "italic") +
  geom_text_repel(aes(y = lat, x = long, label = site_code), data = site_info, 
                  direction = "both", size = 7, box.padding = 0.55, min.segment.length = 0.3) +
  theme(axis.ticks.length=unit(-0.25, "cm")) +
  theme(axis.text.y = element_text(margin = margin(l = 20, r = -45), size = 17, color = "grey10")) +
  theme(axis.text.x = element_text(vjust = 10, size = 17, color = "grey10")) +
  theme(axis.title = element_blank()) +
  xlim(c(-123.8, -122.6)) +
  ylim(c(48.2, 49.1))
study_sites_south

ggsave(filename = "./outputs/sitemap_south.png", study_sites_south, dpi = 1200)
