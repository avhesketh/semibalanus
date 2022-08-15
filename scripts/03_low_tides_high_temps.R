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

# Look only at temperatures within bare plots
ibutton_SA_2021 <- read_csv("./clean_data/SBHW_TempSahsima.csv") %>% 
  filter(month(date_time) > 5 & month(date_time) < 9) %>% # retain summer tides
  filter(treatment == "UI" | treatment == "UR") %>% # retain bare shores
  mutate(date = date(date_time), hour = hour(date_time)) %>% 
  group_by(date) %>% mutate(max_temp = max(temperature_C)) %>%  # calculate when shore temp = maximum
  ungroup() %>% 
  filter(temperature_C == max_temp) %>% select(date, hour, max_temp) %>%
  # retain only times when temp = max (sometimes multiple measurements)
  rename(hottest_hour = hour) %>% group_by(date, max_temp) %>% 
  mutate(hottest_hour = mean(hottest_hour)) # find the average hottest hour

# read in tide data for the site and clean up
# find the lowest tide of each day
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

# join tide and temp data together
hot_low_SA <- ibutton_SA_2021 %>% full_join(SA_tides) %>% unique() %>% na.omit() %>% 
  filter(date >= "2021-06-01" & date <= "2021-08-01") %>% 
  select(date, low_time, hottest_hour) %>% 
  mutate(site = "Sahsima", lag = abs((low_time-hottest_hour)*60)) %>% 
  filter(lag < 380)
# filter out where lags are super long (based on other data from Saddlebag & Bluestone)
# since these data are anomalous


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

mod.bp <- lm(hottest_hour ~ poly(low_time,2, raw =T), data = hot_low_BP)
summary(mod.bp)
plot(mod.bp)

# hottest_hour = 8.7439*low_tide - 1.219*low_tide^2 + 13.56, R2 = 0.9194

# 2: SA

mod.sa <- lm(hottest_hour ~ poly(low_time,2, raw=T), data = hot_low_SA)
summary(mod.sa)
plot(mod.sa)

# hottest_hour = 11.3419*low_tide - 3.7188*low_tide^2 + 13.3726, R2 = 0.722

# 3: TE

mod.si <- lm(hottest_hour ~ poly(low_time,2, raw = T), data = hot_low_SI)
summary(mod.si)
plot(mod.si)

# hottest_hour = 3.1902*low_tide - 1.0388*low_tide^2 + 14.56, R2 = 0.4632


## plot of regressions with annotated equations

all.regressions <- hot_low_SI %>% full_join(hot_low_BP) %>% full_join(hot_low_SA) %>% 
  mutate(site = factor(site, levels = c("Bluestone Point","Sahsima","Saddlebag Island")),
         eqn = case_when(site == "Bluestone Point" ~ "y = 1.239x -0.028x^2 + 3.375",
                         site == "Sahsima" ~ "y = 1.843x - 0.060x^2 + 0.786",
                         site == "Saddlebag Island"~ "y = 0.815x - 0.026x^2 + 8.847"),
         r2 = case_when(site == "Bluestone Point" ~ 0.9194,
                        site == "Sahsima" ~ 0.7220,
                        site == "Saddlebag Island"~ 0.4632))
                         

low_tide_hot_hour <- ggplot(aes(x = low_time, y = hottest_hour, col = site), data = all.regressions) +
  geom_point()+
  facet_wrap(~ site) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14),
        axis.text = element_text(size = 12), strip.text = element_text(size = 14)) +
  geom_text(aes(x = 11.25, y = 17.8, label = eqn), col = "black", size = 4) +
  geom_text(aes(x = 11.5, y = 10.05), label = expression(R^2~"="), col = "black", size = 5) +
  geom_text(aes(x = 15, y = 10, label = r2), col = "black", size = 5) +
  scale_color_manual(values = c("steelblue","#CEAA07","#6E8243")) +
  labs(y = "Hour of maximum on-shore temperature", x = "Hour of lowest low tide")
low_tide_hot_hour

#ggsave(filename = "./outputs/S1Fig4.png", dpi = 1200, low_tide_hot_hour, scale = 1)

################################################################################

## Now need to find the time of low tide on 28 June 2021 for each site.

tides <- read_csv("./raw_data/hot_hour_low_hour/low_tide_jun28.csv") %>%
  separate(low_tide, into = c("hours","minutes", "seconds"), sep = ":", remove = F) %>%
  mutate(low_tide = as.numeric(hours) + as.numeric(minutes)/60) %>% select(-hours,-minutes)

hot_hour_estimate <- tides %>% # use closest reference site's regression to compute hottest hour @ each site surveyed
  mutate(hot_hour = case_when(site_code %in% c("WBS","WBN","BO","PP", "KE","PL") ~ (1.23916*low_tide - 0.02806*(low_tide^2) + 3.37509),
                              site_code %in% c("SP","FC","SA","TP","SN", "BB") ~ (1.84323*low_tide - 0.05978*(low_tide^2) + 0.78625),
                              site_code %in% c("TS","TE","TK","CE") ~ (0.81526*low_tide - 0.02639*(low_tide^2) + 8.84688))) %>% 
  mutate(minutes = as.numeric(substr(as.character(hot_hour), 3,8))*60)

# use these time values + lat/long to derive solar elevation 
#(done using NOAA solar calculator, values included in 'SBHW_SiteInformation.csv')

################################################################################

# Supplemental figure to show magnitude of heat wave

hourly.temp.yyj <- read_csv("./raw_data/hourly_temps_figure/hourly_temp_vicairport.csv", 
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

hourly.temp.calvert <-read_csv("./raw_data/hourly_temps_figure/hourly_temp_pruth.csv") %>% 
  filter(year == 2021 & month == "Jun") %>% 
  mutate(date_time = ymd_hm(measurementTime)) %>% 
  select(date_time, AirTemp_Avg) %>% 
  rename(temp = AirTemp_Avg)

calvert.panel <- ggplot(data = hourly.temp.calvert, aes(x = date_time)) + 
  geom_line(aes(y = temp), col = "olivedrab4", lwd = 1) + 
  annotate(geom = "rect", xmin = ymd_hms("2021-06-23 00:00:00"), 
           xmax = ymd_hms("2021-06-30 23:00:00"), ymin = 9.9, ymax = 19.0, alpha = 0.3) + 
  geom_line(aes(y = 13.5), lty = "dotted") + labs(x = "Date", y = "Temperature (ºC)") + 
  coord_cartesian(xlim = c(ymd_hms("2021-06-23 00:00:00"), ymd_hms("2021-06-30 23:00:00")), 
                  expand = F, ylim = c(5,40)) + 
  theme_classic() + 
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))

S1Fig1 <- calvert.panel / yyj.panel + plot_annotation(tag_levels = "a") & theme(plot.tag = element_text(face = "bold"))
#ggsave(yyj.panel, filename = "./outputs/S1Fig1_simple.png", dpi = 1200, width = 4, height = 1.5, units = "in",scale =1.5)
ggsave(S1Fig1, filename = "./outputs/S1Fig1.png", dpi = 1200, width = 7, height = 5, units = "in", scale = 1.4)
