## April 2020: getting temperature files into one csv

library(tidyverse)
library(lubridate)

setwd("./temperatures")

file_names <- list.files()

ibuttons <- data.frame()

for (i in 1:length(file_names)){
  temp_data <- read_csv(file_names[i], skip = 14)
  temp_data$info <- file_names[i]
  ibuttons <- rbind(ibuttons, temp_data)
}

## split date and time column

ibuttons$dt <- as_datetime(ibuttons$`Date/Time`, format = "%d/%m/%y %I:%M:%S %p")

ibuttons$dt <- round_date(ibuttons$dt, unit = "hour")

dt <- ibuttons$dt

ibutton_sites <- ibuttons %>% 
  select(Value, info) %>% 
  separate(info, into = c("site","collect_date","rest"), sep = "_") %>% 
  dplyr::select(-collect_date, -rest) %>% 
  rename(temp = Value)

temperature <- as.data.frame(cbind(ibutton_sites, dt))

temperature <- temperature %>% 
  filter(site == "SP" & dt <= "2020-04-04 19:01"| (site == "CC" & dt >= "2020-01-24 21:00" & dt <= "2020-04-04 21:00"))

### plotting and summarizing it

site_summary <- ibutton_clean %>% 
  group_by(site) %>% 
  summarize(mean_temp = mean(temp), up_quant = quantile(temp, 0.9))

temp_plot <- temperature %>% 
  group_by(site, dt) %>% 
  summarize(mean_temp = mean(temp))

temperature_plot <- ggplot(aes(x = dt, y = mean_temp, colour = site), data = temp_plot) +
  geom_line() +
  theme_classic() +
  ylab("Temperature (˚C)") +
  xlab("Date")

temperature_plot

# SP is hotter than CC in terms of mean and also the maxima we see on shore...
