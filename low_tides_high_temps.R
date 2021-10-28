## hottest hour - lowest hour relationships

## Ruckle Park, SI, BC (2019 data)

ibutton_rock_summer <- read_csv("./raw_data/hot_hour_low_hour/RP_temp_clean.csv") %>% filter(treatment == "rock") %>% 
  filter(month(date_time)>5 & month(date_time)< 9 & year(date_time) == 2019) 

### separate thing for semibalanus experiment!

daily_max <- ibutton_rock_summer %>% mutate(date = date(date_time), hour = hour(date_time)) %>% 
  group_by(date) %>% mutate(max_temp = max(temp)) %>% 
  ungroup() %>% filter(temp == max_temp) %>% select(date, hour, max_temp) %>% 
  rename(hottest_hour = hour) %>% group_by(date) %>% mutate(hottest_hour = mean(hottest_hour)) 

RP_tides <- read_csv("./raw_data/hot_hour_low_hour/RP_tides_summ19.csv") %>% unite(c(year,month,day), col = "date", sep = "-") %>% 
  unite(c(date,time), col = "datetime", sep = " ", remove = F) %>% 
  mutate(datetime = ymd_hms(strptime(datetime, format = "%Y-%m-%d %I:%M %p"))) %>% 
  group_by(date) %>% mutate(low_tide = min(height_m)) %>% ungroup() %>% 
  filter(height_m == low_tide) %>% mutate(date = ymd(date)) %>% select(-time) %>% 
  mutate(hour = hour(datetime), minutes = minute(datetime)/60) %>% 
  mutate(low_time = hour+minutes) %>% 
  select(date, low_time, low_tide) 

hot_low_RP <- daily_max %>% full_join(RP_tides) %>% unique() %>% 
  filter(date >= "2019-06-13" & date <= "2019-07-07")

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
  filter(date >= "2021-06-13" & date <= "2021-07-07")

## Chris Bamfield data

hot_low_BP <- read_csv("./raw_data/hot_hour_low_hour/BP_tides_summ06.csv") %>% 
  mutate(date = as.Date(date, format = "%d-%b-%y"))

## find formulae for time relationships

# 1: BP

lm(hottest_hour ~ low_time, data = hot_low_BP)

# hottest_hour = 0.6063*low_tide + 6.7109

# 2: CC

lm(hottest_hour ~ low_time, data = hot_low_CC)


# hottest_hour = 0.1874*low_tide + 11.3506

# 3: RP

lm(hottest_hour ~ low_time, data = hot_low_RP)

# hottest_hour = 0.3017*low_tide + 9.8328

sites <- c("BP","CC","RP")
slopes <- c(0.6063, 0.1874, 0.3017)
intercepts <- c(6.7109, 11.3506, 9.8328)
eq.data <- as.data.frame(cbind(cbind(sites, slopes),intercepts))

## tide data

tides <- read_csv("./clean_data/SBHW_tides_clean.csv") %>% group_by(date, site_code) %>% 
  mutate(lowest_tide = min(tide_height_m)) %>% ungroup() %>% 
  filter(tide_height_m == lowest_tide) %>% group_by(site_code) %>% 
  summarize(low_time = mean(hour)) %>% 
  mutate(site_code = str_replace_all(site_code, c("CP" = "CC"))) %>% 
  filter(site_code %in% c("WB","SP","FC","CC","TS","RP"))

hot_hour_estimate <- tides %>% 
  mutate(hot_hour = case_when(site_code %in% c("WB") ~ (low_time*0.6063+6.7109),
                              site_code %in% c("SP","FC","CC") ~ (low_time*0.1874 + 11.3506),
                              site_code %in% c("TS","RP") ~ (low_time*0.3017 + 9.8328))) %>% 
  mutate(minutes = as.numeric(substr(as.character(hot_hour), 3,8))*60)

# use these values to derive solar elevation


