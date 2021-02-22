library(tidyverse)
library(R.utils)
library(glue)
library(magrittr)
library(weathermetrics)
library(padr)
library(ggExtra)
library(lubridate)
library(scales)
library(hrbrthemes)

# create url vector
urls <- 'ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-lite/{yr}/{usaf}-{wban}-{yr}.gz' %>%
  glue(yr = 1973:2017, usaf = 725030, wban = 14732)

# download function
fetch <- safely(function(url, .pb = NULL) {
  if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
  
  url %>%
    downloadFile() %>%
    gunzip(tempfile()) %>%
    read_fwf(
      col_positions = fwf_cols(year = c(1, 4), month = c(6, 7), day = c(9, 11), hour = c(12, 13), temp = c(14, 19)), 
      col_types = cols(year = "i", month = "i", day = "i", hour = "i", temp = "d")
    )
})

progress_bar <- progress_estimated(length(urls))
weather_data <- map(urls, fetch, .pb = progress_bar)

data <- weather_data %>% 
  map_df(~.x$result) %>%
  #remove missing temps that are encoded as -9999 (ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-lite/isd-lite-format.pdf)
  filter(temp != -9999) %>% 
  # temp is recorded in celsius w/ a scaling factor of 10
  mutate(temp = celsius.to.fahrenheit(temp/10)) %>% 
  mutate(timestamp = make_datetime(year, month, day, hour)) %>%
  pad() %>%
  mutate(
    year = year(timestamp),
    month = month(timestamp),
    day = mday(timestamp),
    hour = hour(timestamp)
  ) %>%
  fill(temp) 

data %>%
  mutate(idx = (yday(timestamp)-1)*24 + hour) %>% # create hourly index
  # removes leap date and adjust hourly index of leap years
  mutate(
    idx = if_else(
      leap_year(year) & idx > 1416,
      idx - 24,
      idx
    )
  ) %>%
  filter(!(month == 2 & day == 29)) %>%  
  ggplot(aes(x = idx, y = year, fill = temp)) + 
  geom_tile(height = .9) + # creat a white line between years
  scale_y_reverse(
    expand = expand_scale(add = c(1, 0)),
    breaks = 1973:2017
  ) +
  scale_x_continuous(
    expand = c(0,0),
    breaks = c(0, 2196, 4392, 6588, 8759),
    labels = c(
      "Jan 1, 12:00 AM", 
      "Apr 2, 12:00 PM", 
      "Jul 3, 12:00 AM", 
      "Oct 2, 12:00 PM",
      "Dec 31, 11:00 PM"
    )
  ) +
  scale_fill_viridis_c(
    labels = c("0℉", "25℉", "50℉", "75℉", "100℉"),
    option = "C"
  ) +
  theme_ipsum() +
  removeGrid() +
  labs(
    x = "Hour of the year",
    y = "Year",
    fill = "Temp",
    title = "Hourly Temperature in NY", 
    subtitle = "1973-2017",
    caption = "Source: NCEI/NOAA, www.ncdc.noaa.gov"
  )

  
ggsave("weather.png", scale = 3)
