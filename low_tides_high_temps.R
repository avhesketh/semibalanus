## hottest hour - lowest hour relationships
library(lubridate)

## Ruckle Park, SI, BC (2019 data)

ibutton_rock_summer <- read_csv("./raw_data/hot_hour_low_hour/RP_temp_clean.csv") %>% filter(treatment == "rock") %>%
  filter(month(date_time)>5 & month(date_time)< 9 & year(date_time) == 2019) 

### separate thing for semibalanus experiment!

daily_max <- ibutton_rock_summer %>% mutate(date = date(date_time), hour = hour(date_time)) %>% 
  group_by(date) %>% mutate(max_temp = max(temp)) %>% 
  ungroup() %>% filter(temp == max_temp) %>% select(date, hour, max_temp) %>% 
  rename(hottest_hour = hour) %>% group_by(date) %>% mutate(hottest_hour = mean(hottest_hour)) 

RP_tides <- read_csv("./raw_data/hot_hour_low_hour/RP_tides_summ19.csv") %>% unite(c(year,month,day), col = "date", sep = "-") %>%
  mutate(date = ymd(date)) %>% 
  unite(c(date,time), col = "datetime", sep = " ", remove = F) %>% 
  mutate(datetime = ymd_hms(strptime(datetime, format = "%Y-%m-%d %H:%M:%S"))) %>% 
  group_by(date) %>% mutate(low_tide = min(height_m)) %>% ungroup() %>% 
  filter(height_m == low_tide) %>% mutate(date = ymd(date)) %>% select(-time) %>% 
  mutate(hour = hour(datetime), minutes = minute(datetime)/60) %>% 
  mutate(low_time = hour+minutes) %>% 
  select(date, low_time, low_tide) 

hot_low_RP <- daily_max %>% full_join(RP_tides) %>% unique() %>% 
  filter(date >= "2019-06-13" & date <= "2019-07-07") %>% select(date, low_time, hottest_hour) %>% 
  mutate(site = "ȾESNO¸EṈ¸", eqn = "y = 0.3017x + 9.8328", r2 = 0.2111)

## Chinese Cemetery, Vic, BC (2021 data)

ibutton_CC_2021 <- read_csv("./clean_data/SBHW_temp_clean.csv") %>% 
  filter(month(date_time) > 5 & month(date_time) < 9) %>% 
  filter(treatment == "UI" | treatment == "UR") %>% 
  mutate(date = date(date_time), hour = hour(date_time)) %>% 
  group_by(date) %>% mutate(max_temp = max(temperature_C)) %>% ungroup() %>% 
  filter(temperature_C == max_temp) %>% select(date, hour, max_temp) %>% 
  rename(hottest_hour = hour) %>% group_by(date, max_temp) %>% mutate(hottest_hour = mean(hottest_hour))

CC_tides <- read_delim("./raw_data/hot_hour_low_hour/CC_tides_summ21.csv",
                     col_names = c("date", "time", "na", "height_m","type"),
                     delim = "  ") %>% 
  filter(type == "Low Tide")  %>%  select(-type, -na) %>% 
  mutate(time = str_remove_all(time, " PDT"), height_m = as.numeric(str_remove_all(height_m, " meters"))) %>% 
  unite(c(date, time), col = "datetime", sep = " ") %>% mutate(datetime = ymd_hm(datetime)) %>% 
  mutate(hour = hour(datetime), minutes = minute(datetime)/60, date = date(datetime)) %>% 
  mutate(low_time = hour+minutes) %>% select(date, low_time, height_m) %>% rename(low_tide = height_m) %>% 
  group_by(date) %>% mutate(lowest_tide = min(low_tide)) %>% ungroup() %>% filter(low_tide == lowest_tide) 

hot_low_CC <- ibutton_CC_2021 %>% full_join(CC_tides) %>% unique() %>% na.omit() %>% 
  filter(date >= "2021-06-13" & date <= "2021-07-07") %>% select(date, low_time, hottest_hour) %>% 
  mutate(site = "Sahsima", eqn = "y = 0.1874x + 11.3506", r2 = 0.1922)

## Chris Bamfield data

hot_low_BP <- read_csv("./raw_data/hot_hour_low_hour/BP_tides_summ06.csv") %>% 
  mutate(date = as.Date(date, format = "%d-%b-%y"), site = "Bluestone Point",
         eqn = "y = 0.6063*x + 6.7109", r2 =0.9045)

## find formulae for time relationships

# 1: BP

mod.bp <- lm(hottest_hour ~ low_time, data = hot_low_BP)
summary(mod.bp)

# hottest_hour = 0.6063*low_tide + 6.7109

# 2: CC

mod.cc <- lm(hottest_hour ~ low_time, data = hot_low_CC)
summary(mod.cc)

# hottest_hour = 0.1874*low_tide + 11.3506

# 3: RP

mod.rp <- lm(hottest_hour ~ low_time, data = hot_low_RP)
summary(mod.rp)

# hottest_hour = 0.3017*low_tide + 9.8328

sites <- c("BP","CC","RP")
slopes <- c(0.6063, 0.1874, 0.3017)
intercepts <- c(6.7109, 11.3506, 9.8328)
eq.data <- as.data.frame(cbind(cbind(sites, slopes),intercepts))

## plot of regressions with annotated equations

all.regressions <- hot_low_RP %>% full_join(hot_low_BP) %>% full_join(hot_low_CC) 

low_tide_hot_hour <- ggplot(aes(x = low_time, y = hottest_hour, col = site), data = all.regressions) +
  geom_point()+
  facet_wrap(~ site) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14),
        axis.text = element_text(size = 12), strip.text = element_text(size = 14)) +
  geom_text(aes(x = 14.5, y = 17.8, label = eqn), col = "black", size = 5) +
  geom_text(aes(x = 16, y = 10.05), label = expression(R^2~"="), col = "black", size = 5) +
  geom_text(aes(x = 20.5, y = 10, label = r2), col = "black", size = 5) +
  scale_color_manual(values = c("steelblue","mediumorchid3", "tomato3")) +
  labs(y = "Hour of maximum on-shore temperature", x = "Hour of lowest low tide")
low_tide_hot_hour

ggsave(filename = "./outputs/S1Fig2.png", dpi = 1200, low_tide_hot_hour)

## tide data

tides <- read_csv("./clean_data/SBHW_tides_clean.csv") %>% group_by(date, site_code) %>% 
  mutate(lowest_tide = min(tide_height_m)) %>% ungroup() %>% 
  filter(tide_height_m == lowest_tide) %>% group_by(site_code) %>% 
  summarize(low_time = mean(hour)) %>% 
  mutate(site_code = str_replace_all(site_code, c("CP" = "CC"))) %>% 
  filter(site_code %in% c("WBN","SP","FC","CC","TS","RP"))

hot_hour_estimate <- tides %>% 
  mutate(hot_hour = case_when(site_code %in% c("WBN") ~ (low_time*0.6063+6.7109),
                              site_code %in% c("SP","FC","CC") ~ (low_time*0.1874 + 11.3506),
                              site_code %in% c("TS","RP") ~ (low_time*0.3017 + 9.8328))) %>% 
  mutate(minutes = as.numeric(substr(as.character(hot_hour), 3,8))*60)

# use these values to derive solar elevation


