## ----message=FALSE, warning=FALSE----------------------------------------
library(readr)
library(dplyr)
library(lubridate)
library(trackdf)

raw <- read_csv(system.file("extdata/video/01.csv", package = "trackdf")) %>%
  filter(., !ignore)
raw

## ------------------------------------------------------------------------
tracks <- track_df(x = raw$x, y = raw$y, t = raw$frame, id = raw$track, 
                   origin = "2019-03-24 12:55:23",
                   period = period("1 second") / 30, 
                   tz = "America/New_York")
tracks

## ----message=FALSE, warning=FALSE----------------------------------------
library(readr)
library(dplyr)
library(lubridate)
library(trackdf)

raw <- read_csv(system.file("extdata/gps/01.csv", package = "trackdf")) %>%
  na.omit()
raw

## ------------------------------------------------------------------------
tracks <- track_df(x = raw$lon, y = raw$lat, t = paste(raw$date, raw$time), id = 1,  
                  proj = "+proj=longlat", tz = "Africa/Windhoek")
tracks

