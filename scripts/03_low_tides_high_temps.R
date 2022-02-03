## Determining the hottest hour to use in pulling solar elevation & azimuth for 
## angle of solar incidence and general site-level mortality model
## Amelia V Hesketh
## January 2021

# Load all relevant packages into R.env
pkgs <- c("tidyverse", "lubridate", "patchwork")
invisible(lapply(pkgs, library, character = T))
rm(pkgs)


## 2006 Bluestone Point, Bamfield data courtesy of CDG Harley

hot_low_BP <- read_csv("./raw_data/hot_hour_low_hour/BP_tides_summ06.csv") %>% 
  mutate(date = as.Date(date, format = "%d-%b-%y"), site = "Bluestone Point")

## Load Sahsima, Vic, BC (2021 data) 
# need to first do some cleaning of these data so they are useable for this analysis

temp.files <- list.files("./raw_data/ibutton_temp/") # list all temperature files

# create blank dataframe into which I can read August temperature data
temp.df.all.aug <- data.frame(matrix(ncol = 4))
colnames(temp.df.all.aug) <- c("Date/Time", "Unit", "Value", "file_name") # with appropriate column names

for (file in 1: length(temp.files)){
  file.name = paste("./raw_data/ibutton_temp/", temp.files[file], sep = "")
  temp.df <- read_csv(file.name, skip = 14) %>% mutate(file_name = temp.files[file]) # load file
  if (str_detect(file.name, "07")==TRUE){ # if collected in July
    if (file == 1){
      temp.df.all.july <- temp.df
    }
    if (file > 1){
      temp.df.all.july <- rbind(temp.df.all.july, temp.df)
    }
  }
  else { # if August data (slightly different format)
    temp.df.all.aug <- rbind(temp.df.all.aug, temp.df)
  }
}

# clean the July data first
temp.df.clean.july <- temp.df.all.july %>% 
  mutate(Value = str_remove_all(Value, "C,")) %>% 
  separate(file_name, sep = "_", into = c("block","treatment","extra")) %>% 
  select(-extra) %>% rename(date = 1, time = 2, temperature_C = 3) %>%
  mutate(time = toupper(str_remove_all(time, fixed(".")))) %>% 
  mutate(time = parse_time(time, "%I:%M:%S %p")) %>% 
  unite(date_time, c(date, time), sep = " ") %>% 
  mutate(date_time = as_datetime(date_time), temperature_C = as.numeric(temperature_C))

# clean August data
temp.df.clean.aug <- temp.df.all.aug %>% na.omit() %>% select(-Unit) %>% 
  separate(file_name, sep = "_", into = c("block","treatment","extra")) %>% 
  select(-extra) %>% rename(date_time = 1, temperature_C = 2) %>% 
  mutate(date_time = parse_date_time(date_time, "%d/%m/%y %I:%M:%S %p"))

# join data into one df
temp.clean <- temp.df.clean.aug %>%  full_join(temp.df.clean.july) %>% 
  mutate(treatment = case_when(treatment == 1 ~ "UI", treatment == 2 ~ "SI", 
                               treatment == 3 ~ "UR", treatment == 4 ~ "SR")) %>% 
  filter(date_time < "2021-08-21 6:00:00" & date_time > "2021-04-17 15:00:00") %>% unique()

#write_csv(temp.clean, "./clean_data/SBHW_TempSahsima.csv")

ibutton_SA_2021 <- read_csv("./clean_data/SBHW_TempSahsima.csv") %>% 
  filter(month(date_time) > 5 & month(date_time) < 9) %>% 
  filter(treatment == "UI" | treatment == "UR") %>% 
  mutate(date = date(date_time), hour = hour(date_time)) %>% 
  group_by(date) %>% mutate(max_temp = max(temperature_C)) %>% ungroup() %>% 
  filter(temperature_C == max_temp) %>% select(date, hour, max_temp) %>% 
  rename(hottest_hour = hour) %>% group_by(date, max_temp) %>% 
  mutate(hottest_hour = mean(hottest_hour))

SA_tides <- read_delim("./raw_data/hot_hour_low_hour/SA_tides_summ21.csv",
                       col_names = c("date", "time", "na", "height_m","type"),
                       delim = "  ") %>% 
  filter(type == "Low Tide")  %>%  select(-type, -na) %>% 
  mutate(time = str_remove_all(time, " PDT"), height_m = as.numeric(str_remove_all(height_m, " meters"))) %>% 
  unite(c(date, time), col = "datetime", sep = " ") %>% mutate(datetime = ymd_hm(datetime)) %>% 
  mutate(hour = hour(datetime), minutes = minute(datetime)/60, date = date(datetime)) %>% 
  mutate(low_time = hour+minutes) %>% select(date, low_time, height_m) %>% 
  rename(low_tide = height_m) %>% 
  group_by(date) %>% mutate(lowest_tide = min(low_tide)) %>% ungroup() %>% 
  filter(low_tide == lowest_tide) 

hot_low_SA <- ibutton_SA_2021 %>% full_join(SA_tides) %>% unique() %>% na.omit() %>% 
  filter(date >= "2021-06-01" & date <= "2021-08-01") %>% 
  select(date, low_time, hottest_hour) %>% 
  mutate(site = "Sahsima", lag = abs((low_time-hottest_hour)*60)) %>% 
  filter(lag < 380) # filter out where lags are super long (based on other data from Saddlebag & Bluestone)


## 2006 Saddlebag Island data courtesy of CDG Harley

hot_low_SI <- read_csv("./raw_data/hot_hour_low_hour/SI_tides_summ06.csv") %>% 
  separate(low_time, into = c("hour","minutes"), sep = ":") %>% 
  mutate(date = as.Date(date, format = "%d-%b-%y"), site = "Saddlebag Island",
         hottest_hour = hour(hottest_hour),
         hour = as.numeric(hour),
         minutes = as.numeric(minutes),
         low_time = hour + (minutes/60)) %>% select(-hour, -minutes)


## find formulae for time relationships

# 1: BP

mod.bp <- lm(hottest_hour ~ low_time, data = hot_low_BP)
summary(mod.bp)

# hottest_hour = 0.6063*low_tide + 6.7109, R2 = 0.9045

# 2: SA

mod.sa <- lm(hottest_hour ~ low_time, data = hot_low_SA)
summary(mod.sa)

# hottest_hour = 0.52458*low_tide + 7.52997, R2 = 0.6549

# 3: TE

mod.si <- lm(hottest_hour ~ low_time, data = hot_low_SI)
summary(mod.si)

# hottest_hour = 0.19695*low_tide + 12.18867, R2 = 0.4358


## plot of regressions with annotated equations

all.regressions <- hot_low_SI %>% full_join(hot_low_BP) %>% full_join(hot_low_SA) %>% 
  mutate(site = factor(site, levels = c("Bluestone Point","Sahsima","Saddlebag Island")),
         eqn = case_when(site == "Bluestone Point" ~ "y = 0.6063x + 6.7109",
                         site == "Sahsima" ~ "y =  0.5246x + 7.5300",
                         site == "Saddlebag Island"~ "y = 0.1970x + 12.1887"),
         r2 = case_when(site == "Bluestone Point" ~ 0.9045,
                        site == "Sahsima" ~ 0.6549,
                        site == "Saddlebag Island"~ 0.4358))
                         

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

#ggsave(filename = "../outputs/S1Fig3.png", dpi = 1200, low_tide_hot_hour, scale = 1)

################################################################################

# Clean tide data to find the time of the low tide

# tide data from different sites are formatted slightly differently

# Clover Point
tides_cp <- read_delim("./raw_data/tides/tides_CP.csv", 
                       col_names = c("date","junk","time","tz","tide_height_m")) %>%
  mutate(hour = hour(time), site_code = "CP") %>% select(-junk, -tz, -time)

# Tower Point
tides_tp <- read_delim("./raw_data/tides/tides_TP.csv", 
                       col_names = c("date","junk","time","tz","tide_height_m")) %>%
  mutate(hour = hour(time), site_code = "TP") %>% select(-junk, -tz, -time) 

# Rest of sites all in one datasheet
tides_rest <- read_csv("./raw_data/tides/tides_other_sites.csv") %>% 
  pivot_longer(names_to = "hour", values_to = "tide_height_m", cols = 3:length(.)) %>%
  mutate(hour = as.integer(hour))

tides_full <- rbind(rbind(tides_cp, tides_tp),tides_rest)

#write_csv(tides_full, "./clean_data/SBHW_Tides.csv")

## Now need to find the time of low tide on 28 June 2021 for each site.

tides <- read_csv("./clean_data/SBHW_Tides.csv") %>% group_by(date, site_code) %>% 
  mutate(lowest_tide = min(tide_height_m)) %>% 
  filter(date == "2021-06-28" & lowest_tide == tide_height_m) %>% 
  group_by(site_code) %>% 
  summarize(low_time = mean(hour)) %>% 
  mutate(site_code = str_replace_all(site_code, c("CP" = "SA")))

hot_hour_estimate <- tides %>% # use closest reference site's regression to compute hottest hour @ each site surveyed
  mutate(hot_hour = case_when(site_code %in% c("WBS","WBN","BO","PP", "KE","PL") ~ (low_time*0.6063+6.7109),
                              site_code %in% c("SP","FC","SA","TP","SN", "BB") ~ (low_time*0.52458 + 7.52997),
                              site_code %in% c("TS","TE","TK","CE") ~ (low_time*0.1970 + 12.1887))) %>% 
  mutate(minutes = as.numeric(substr(as.character(hot_hour), 3,8))*60)

# use these time values + lat/long to derive solar elevation 
#(done using NOAA solar calculator, values included in 'SBHW_SiteInformation.csv')

################################################################################

# Supplemental figure to show magnitude of heat wave

hourly.temp.yyj <- read_csv("../raw_data/hourly_temps_figure/hourly_temp_vicairport.csv", 
                            col_select = c(5:8, 10, 14)) %>% 
  filter(Day <= 30 & Day >=23) %>% rename(date_time = 1, temp = 5, humidity = 6) %>% 
  mutate(date_time = ymd_hms(date_time))

yyj.panel <- ggplot(data = hourly.temp.yyj, aes(x = date_time)) + 
  geom_line(aes(y = temp), col = "mediumorchid4", lwd = 1) + 
  annotate(geom = "rect", xmin = ymd_hms("2021-06-23 00:00:00"), 
           xmax = ymd_hms("2021-06-30 23:00:00"), ymin = 9.8, ymax = 19.9, alpha = 0.3) + 
  geom_line(aes(y = 14.9), lty = "dotted") + 
  labs(x = "Date", y = "Temperature (ºC)") + 
  coord_cartesian(xlim = c(ymd_hms("2021-06-23 00:00:00"), 
                           ymd_hms("2021-06-30 23:00:00")), expand = F, ylim = c(5,40)) + 
  theme_classic() + theme(axis.title = element_text(size = 14), 
                          axis.text = element_text(size = 12))

hourly.temp.bellabella <-read_csv("../raw_data/hourly_temps_figure/temp_bellabella_june.csv", 
                                  col_select = c(5:8, 10, 14)) %>% filter(Day <= 30 & Day >=23) %>% 
  rename(date_time = 1, temp = 5, humidity = 6) %>% mutate(date_time = ymd_hms(date_time))

bb.panel <- ggplot(data = hourly.temp.bellabella, aes(x = date_time)) + 
  geom_line(aes(y = temp), col = "olivedrab4", lwd = 1) + 
  annotate(geom = "rect", xmin = ymd_hms("2021-06-23 00:00:00"), 
           xmax = ymd_hms("2021-06-30 23:00:00"), ymin = 9.9, ymax = 19.0, alpha = 0.3) + 
  geom_line(aes(y = 13.5), lty = "dotted") + labs(x = "Date", y = "Temperature (ºC)") + 
  coord_cartesian(xlim = c(ymd_hms("2021-06-23 00:00:00"), ymd_hms("2021-06-30 23:00:00")), 
                  expand = F, ylim = c(5,40)) + 
  theme_classic() + 
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))

S1Fig1 <- bb.panel / yyj.panel + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(face = "bold"))

#ggsave(S1Fig1, filename = "../outputs/S1Fig1.png", dpi = 1200, width = 7, height = 5, units = "in" )
