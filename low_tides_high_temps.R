## hottest hour - lowest hour relationships
library(lubridate)

## Sahsima, Vic, BC (2021 data)

ibutton_SA_2021 <- read_csv("./clean_data/SBHW_temp_clean.csv") %>% 
  filter(month(date_time) > 5 & month(date_time) < 9) %>% 
  filter(treatment == "UI" | treatment == "UR") %>% 
  mutate(date = date(date_time), hour = hour(date_time)) %>% 
  group_by(date) %>% mutate(max_temp = max(temperature_C)) %>% ungroup() %>% 
  filter(temperature_C == max_temp) %>% select(date, hour, max_temp) %>% 
  rename(hottest_hour = hour) %>% group_by(date, max_temp) %>% mutate(hottest_hour = mean(hottest_hour))

SA_tides <- read_delim("./raw_data/hot_hour_low_hour/SA_tides_summ21.csv",
                     col_names = c("date", "time", "na", "height_m","type"),
                     delim = "  ") %>% 
  filter(type == "Low Tide")  %>%  select(-type, -na) %>% 
  mutate(time = str_remove_all(time, " PDT"), height_m = as.numeric(str_remove_all(height_m, " meters"))) %>% 
  unite(c(date, time), col = "datetime", sep = " ") %>% mutate(datetime = ymd_hm(datetime)) %>% 
  mutate(hour = hour(datetime), minutes = minute(datetime)/60, date = date(datetime)) %>% 
  mutate(low_time = hour+minutes) %>% select(date, low_time, height_m) %>% rename(low_tide = height_m) %>% 
  group_by(date) %>% mutate(lowest_tide = min(low_tide)) %>% ungroup() %>% filter(low_tide == lowest_tide) 

hot_low_SA <- ibutton_SA_2021 %>% full_join(SA_tides) %>% unique() %>% na.omit() %>% 
  filter(date >= "2021-06-01" & date <= "2021-08-01") %>% select(date, low_time, hottest_hour) %>% 
  mutate(site = "Sahsima", eqn = "y =  0.5246x + 7.5300", r2 = 0.6549, lag = abs((low_time-hottest_hour)*60)) %>% 
  filter(lag < 380) # filter out where lags super long (based on prior data from Saddlebag Island)

## 2006 Bamfield data

hot_low_BP <- read_csv("./raw_data/hot_hour_low_hour/BP_tides_summ06.csv") %>% 
  mutate(date = as.Date(date, format = "%d-%b-%y"), site = "Bluestone Point",
         eqn = "y = 0.6063x + 6.7109", r2 =0.9045)

## 2006 Saddlebag data

hot_low_SI <- read_csv("./raw_data/hot_hour_low_hour/SI_tides_summ06.csv") %>% 
  separate(low_time, into = c("hour","minutes"), sep = ":") %>% 
  mutate(date = as.Date(date, format = "%d-%b-%y"), site = "Saddlebag Island",
         hottest_hour = hour(hottest_hour),
         hour = as.numeric(hour),
         minutes = as.numeric(minutes),
         low_time = hour + (minutes/60)) %>% select(-hour, -minutes) %>% 
  mutate(eqn = "y = 0.1970x + 12.1887", r2 =0.4358)

## find formulae for time relationships

# 1: BP

mod.bp <- lm(hottest_hour ~ low_time, data = hot_low_BP)
summary(mod.bp)

# hottest_hour = 0.6063*low_tide + 6.7109

# 2: SA

mod.sa <- lm(hottest_hour ~ low_time, data = hot_low_SA)
summary(mod.sa)

# hottest_hour = 0.52458*low_tide + 7.52997

# 3: TE

mod.si <- lm(hottest_hour ~ low_time, data = hot_low_SI)
summary(mod.si)

# hottest_hour = 0.19695*low_tide + 12.18867

sites <- c("BP","SA","SI")
slopes <- c(0.6063, 0.5246, 0.1970)
intercepts <- c(6.7109, 7.5300, 12.1887)
eq.data <- as.data.frame(cbind(cbind(sites, slopes),intercepts))

## plot of regressions with annotated equations

all.regressions <- hot_low_SI %>% full_join(hot_low_BP) %>% full_join(hot_low_SA) %>% 
  mutate(site = factor(site, levels = c("Bluestone Point","Sahsima","Saddlebag Island")))

low_tide_hot_hour <- ggplot(aes(x = low_time, y = hottest_hour, col = site), data = all.regressions) +
  geom_point()+
  facet_wrap(~ site) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14),
        axis.text = element_text(size = 12), strip.text = element_text(size = 14)) +
  geom_text(aes(x = 11.25, y = 17.8, label = eqn), col = "black", size = 5) +
  geom_text(aes(x = 11.5, y = 10.05), label = expression(R^2~"="), col = "black", size = 5) +
  geom_text(aes(x = 15, y = 10, label = r2), col = "black", size = 5) +
  scale_color_manual(values = c("steelblue","#CEAA07","#6E8243")) +
  labs(y = "Hour of maximum on-shore temperature", x = "Hour of lowest low tide")
low_tide_hot_hour

ggsave(filename = "./outputs/S1Fig3.png", dpi = 1200, low_tide_hot_hour, scale = 1)

## tide data

tides <- read_csv("./clean_data/SBHW_tides_clean.csv") %>% group_by(date, site_code) %>% 
  mutate(lowest_tide = min(tide_height_m)) %>% 
  filter(date == "2021-06-28" & lowest_tide == tide_height_m) %>% 
  group_by(site_code) %>% 
  summarize(low_time = mean(hour)) %>% 
  mutate(site_code = str_replace_all(site_code, c("CP" = "SA")))

hot_hour_estimate <- tides %>% 
  mutate(hot_hour = case_when(site_code %in% c("WBS","WBN","BO","PP", "KE","PL") ~ (low_time*0.6063+6.7109),
                              site_code %in% c("SP","FC","SA","TP","SN", "BB") ~ (low_time*0.52458 + 7.52997),
                              site_code %in% c("TS","TE","TK","CE") ~ (low_time*0.1970 + 12.1887))) %>% 
  mutate(minutes = as.numeric(substr(as.character(hot_hour), 3,8))*60)
# use these values to derive solar elevation


