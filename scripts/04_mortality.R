## Mortality models for Semibalanus cariosus mortality induced by the PNWHD
## Amelia V Hesketh
## January 2021

pkgs <- c("tidyverse", "lubridate", "car", "plotrix", "patchwork", "glmmTMB",
          "DHARMa", "lme4", "wesanderson", "ggeffects")
lapply(pkgs, library, character.only = T)
rm(pkgs)

# Model 1: site-level variables & algal cover

# Need to calculate site-level variables for initial model

# 1: Air temperature data
temp.files <- list.files("./raw_data/air_temp/") # find raw files

# join together all the raw files into one dataframe
for (file in 1: length(temp.files)){
  file.name = paste("./raw_data/air_temp/", temp.files[file], sep = "")
  file.name.len = nchar(file.name)
  site.name = substr(file.name, (file.name.len-5), (file.name.len-4))
  temp.df <- read_csv(file.name, col_select = c(5:8, 10, 14)) %>% 
    filter(`Date/Time` >= "2021-06-01" & `Date/Time` < "2021-09-01") %>% 
    mutate(site_code = site.name)
  if (file == 1){
    temp.df.all <- temp.df
  }
  if (file > 1){
    temp.df.all <- rbind(temp.df.all, temp.df)
  }
}

# now to rename the currently unnamed columns for ease of coding later
temp.df.rename <- temp.df.all %>%  rename("date" = 1, "year" = 2,"month" = 3, 
                                          "day" = 4, "max_temp_C" = 5, "mean_temp_C" = 6) %>% 
  mutate(site_code = if_else(site_code == "BS", "WBS", site_code))

# isolate only the heat wave period (June 25-29 2021)

temp.hw <- temp.df.rename %>% filter(date <= "2021-06-29" & date >= "2021-06-25") %>% 
  group_by(site_code) %>% summarize(mdmax = mean(max_temp_C, na.rm = T)) 

# 2: load in aspect information for each site
survey.info <- read_csv("./clean_data/SBHW_SiteInformation.csv") %>% 
  select(site_code, mortality, orientation_degrees, solar_azimuth)  %>% 
  na.omit() %>% mutate(degrees_from_azimuth = abs(orientation_degrees-solar_azimuth))

# 3: load in tide timing information for each site
tides_summary <- read_csv("./clean_data/SBHW_Tides.csv") %>% 
  group_by(site_code, date) %>% mutate(low_water = min(tide_height_m)) %>%
  ungroup() %>% filter(tide_height_m == low_water) %>% group_by(site_code) %>% 
  summarize(mean_low_tide_time = mean(hour))

explanatory.variables <- survey.info %>% left_join(temp.hw) %>% 
  select(-mortality, -orientation_degrees,-solar_azimuth) %>% left_join(tides_summary) %>% na.omit

## Compute the response data

mortality <- read_csv("./raw_data/mortality/SBHW_MORT_surveys.csv") %>% 
  select(-cover_live, -cover_dead, -algal_cover_per25sq)

mort.model.df <- mortality %>% full_join(explanatory.variables) %>% 
  mutate(prop_dead = number_dead/(number_dead+number_live)) %>% filter(is.nan(prop_dead) == F)

# examining the mortality data distribution
hist(mort.model.df$prop_dead, breaks = 50, xlab = "Proportion dead", 
     main = "Histogram of mortality data")

# try out different error distributions

mort.mod.0 <- glmmTMB(prop_dead ~ mdmax + mean_low_tide_time + 
                        algal_prop_cover + degrees_from_azimuth + (1 | site_code), 
                      data = mort.model.df, family = binomial(link = "logit"))

plot(density(residuals(mort.mod.0, type = "response"))) 
# has a long tail ... data are overdispersed ... need to look at other fixes.

mort.mod.1 <- glmmTMB(prop_dead ~ mdmax + mean_low_tide_time + 
                        algal_prop_cover + degrees_from_azimuth +
                        (1 | site_code/transect), data = mort.model.df, family = tweedie())

plot(simulateResiduals(mort.mod.1)) # this seems to work. 

#try the beta model since this is more correct

mort.mod.2 <- glmmTMB((prop_dead*nrow(mort.model.df)+0.5)/nrow(mort.model.df) ~ 
                        mdmax + mean_low_tide_time + algal_prop_cover + degrees_from_azimuth + 
                        (1 | site_code/transect), data = mort.model.df, family = beta_family())

plot(simulateResiduals(mort.mod.2)) 
# the error distribution family seems wrong with beta. Stick with Tweedie!

summary(mort.mod.1)
plot(residuals(mort.mod.1))
Anova(mort.mod.1)

## Create a plot of mortality

mort.summary <- mort.model.df %>% mutate(site_code = factor(site_code)) %>% 
  group_by(site_code, degrees_from_azimuth, mean_low_tide_time) %>% 
  summarize(average_mort = mean(prop_dead, na.rm = T), 
            average_maxtemp = mean(mdmax)) %>% 
  mutate(site_code = factor(site_code))

pal <- wes_palette("Zissou1", 20, type = "continuous")

mort.plot <- ggplot(data = mort.model.df, aes(col = degrees_from_azimuth, 
                                              y = prop_dead*100, 
                                              x = mdmax)) + 
  geom_jitter(size = 2, width = 0.2, alpha = 0.8) + 
  scale_color_gradientn(colors = pal, trans="reverse") + 
  theme_classic() + 
  labs(y = "Mortality (%)", col = "Aspect (º)", x = "Mean maximum daily air temperature (ºC)") + 
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), legend.title = element_text(size = 14))

#ggsave(filename = "./outputs/Fig3.png", dpi = 1200, mort.plot, height = 2.5, width = 3.5, units = "in", scale = 1.5)

################################################################################

# Model 2: angle of solar incidence specifically

angle.data <- read_csv("./raw_data/mortality/SBHW_MORT_angles.csv")
View(angle.data)
site.info <- read_csv("./clean_data/SBHW_SiteInformation.csv") %>% 
  select(site_code,angles,solar_elevation, solar_azimuth) %>% na.omit() %>% select(-angles)

#join together the dataframes
angle.calculate <- angle.data %>% full_join(site.info)

# the compass orientation needs to be flipped by 180 degrees/pi radians
# the angle needs to be converted from angle from vertical to angle from horizontal
angle.correct <- angle.calculate %>% 
  mutate(orient_rad = (((orientation_raw_degrees/360)*2*pi)-pi), 
         prop_mort = number_dead/(number_live + number_dead), 
         sigma = ((90-angle_raw_degrees)/360)*2*pi, 
         beta = (solar_elevation/360)*2*pi, 
         solar_az_rad = (solar_azimuth/360)*2*pi)

# now for the calculations
solar.angles <- angle.correct %>% mutate(gamma = abs(orient_rad - solar_az_rad)) %>% 
  mutate(theta = acos(sin(beta)*cos(sigma) + cos(gamma)*cos(beta)*sin(sigma))) %>% 
  mutate(angle = (theta/(2*pi)*360)) %>% 
  select(prop_mort, angle, site_code) %>% 
  mutate(site_code = str_replace_all(site_code, "WA","WBS"))

angle.model.0 <- glmer(prop_mort ~ angle + (1+angle|site_code), 
                       family = binomial(link = "logit"), data = solar.angles)

plot(density(resid(angle.model.0, type='deviance')))
qqnorm(residuals(angle.model.0, type = "deviance"))

# looks reasonable 

angle.model.1 <- update(angle.model.0, ~. -(1+angle|site_code) + (1|site_code))
AIC(angle.model.0, angle.model.1) # allowing slope variation for each site is worse

# use angle.model.1 going forward

summary(angle.model.1)
Anova(angle.model.1)

site.levels <- as.data.frame(c("WBN","SP","SA","FC","TE","TS")) %>% rename(site_code = 1) 
col.conversion <- site.levels %>% left_join(site_info) %>% select(full.pal)
col.levels <- col.conversion$full.pal
site.levels <- site.levels$site_code

set.seed(26)
model.pred <- ggpredict(angle.model.1, terms = c("angle", "site_code"), type = "random") %>%
  rename(site_code = group)

model.glm <- glm(prop_mort ~ angle, family = "binomial", data = solar.angles)

model.pred2 <- ggpredict(model.glm, terms = c("angle")) %>% select(-group)

solar.angles$site_code <- factor(solar.angles$site_code, levels = site.levels)

angles.plot <- ggplot(data = solar.angles, aes(x = angle, y = prop_mort, col = site_code)) + 
  geom_point(size = 2) + 
  labs(y = "Proportion mortality", x = "Angle of solar incidence (º)", col = "Site") + 
  theme_classic() + scale_color_manual(values = col.levels) + 
  scale_fill_manual(values = col.levels) + 
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), legend.title = element_text(size = 14)) + 
  geom_line(data = model.pred, aes(x = x, y = predicted)) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), aes(x = angle, y = prop_mort), col = "black")

#ggsave(angles.plot, filename = "./outputs/Fig4.png", dpi = 1200, height = 2.5, width = 3.5, units = "in", scale = 1.5)

