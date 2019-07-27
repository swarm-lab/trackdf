## ----message=FALSE, warning=FALSE----------------------------------------
library(readr)
library(dplyr)
library(lubridate)
library(trackdf)

raw <- read_csv(system.file("extdata/video/01.csv", package = "trackdf")) %>%
  filter(., !ignore)
raw

## ----paged.print=FALSE---------------------------------------------------
vid_df <- track_df(x = raw$x, y = raw$y, t = raw$frame, id = raw$track, 
                   origin = "2019-03-24 12:55:23",
                   period = period("1 second") / 30, 
                   tz = "America/New_York")
head(vid_df)

## ----paged.print=FALSE---------------------------------------------------
vid_tbl <- track_tbl(x = raw$x, y = raw$y, t = raw$frame, id = raw$track, 
                     origin = "2019-03-24 12:55:23",
                     period = period("1 second") / 30, 
                     tz = "America/New_York")
print(vid_tbl)

## ----paged.print=FALSE---------------------------------------------------
vid_dt <- track_dt(x = raw$x, y = raw$y, t = raw$frame, id = raw$track, 
                   origin = "2019-03-24 12:55:23",
                   period = period("1 second") / 30, 
                   tz = "America/New_York")
vid_dt

## ----message=FALSE, warning=FALSE----------------------------------------
raw <- read_csv(system.file("extdata/gps/01.csv", package = "trackdf")) %>%
  na.omit()
raw

## ----paged.print=FALSE---------------------------------------------------
gps_df <- track_df(x = raw$lon, y = raw$lat, t = paste(raw$date, raw$time), id = 1,  
                   proj = "+proj=longlat", tz = "Africa/Windhoek")
head(gps_df)

## ----paged.print=FALSE---------------------------------------------------
gps_tbl <- track_tbl(x = raw$lon, y = raw$lat, t = paste(raw$date, raw$time), id = 1,  
                     proj = "+proj=longlat", tz = "Africa/Windhoek")
gps_tbl

## ----paged.print=FALSE---------------------------------------------------
gps_dt <- track_dt(x = raw$lon, y = raw$lat, t = paste(raw$date, raw$time), id = 1,  
                   proj = "+proj=longlat", tz = "Africa/Windhoek")
gps_dt

