## ----message=FALSE, warning=FALSE----------------------------------------
library(trackdf)
library(lubridate)

if (requireNamespace("readr", quietly = TRUE) & requireNamespace("dplyr", quietly = TRUE)) {
  library(readr)
  library(dplyr)
  raw <- read_csv(system.file("extdata/video/01.csv", package = "trackdf")) %>%
    filter(., !ignore)
} else {
  raw <- read.csv(system.file("extdata/video/01.csv", package = "trackdf"))
  raw <- raw[raw$ignore, ]
}

raw

## ----paged.print=FALSE---------------------------------------------------
vid_df <- track(x = raw$x, y = raw$y, t = raw$frame, id = raw$track, 
                origin = "2019-03-24 12:55:23", period = period("1 second") / 30, 
                tz = "America/New_York", table = "df")
head(vid_df)

## ----paged.print=FALSE---------------------------------------------------
vid_tbl <- track(x = raw$x, y = raw$y, t = raw$frame, id = raw$track, 
                 origin = "2019-03-24 12:55:23", period = period("1 second") / 30, 
                 tz = "America/New_York", table = "tbl")
print(vid_tbl)

## ----paged.print=FALSE---------------------------------------------------
vid_dt <- track(x = raw$x, y = raw$y, t = raw$frame, id = raw$track, 
                origin = "2019-03-24 12:55:23", period = period("1 second") / 30, 
                tz = "America/New_York", table = "dt")
vid_dt

## ----message=FALSE, warning=FALSE----------------------------------------
if (requireNamespace("readr", quietly = TRUE) & requireNamespace("dplyr", quietly = TRUE)) {
  raw <- read_csv(system.file("extdata/gps/01.csv", package = "trackdf")) %>%
    na.omit()
} else {
  raw <- read.csv(system.file("extdata/gps/01.csv", package = "trackdf"))
  raw <- raw[complete.cases(raw), ]
}

raw

## ----paged.print=FALSE---------------------------------------------------
gps_df <- track(x = raw$lon, y = raw$lat, t = paste(raw$date, raw$time), id = 1,  
                proj = "+proj=longlat", tz = "Africa/Windhoek", table = "df")
head(gps_df)

## ----paged.print=FALSE---------------------------------------------------
gps_tbl <- track(x = raw$lon, y = raw$lat, t = paste(raw$date, raw$time), id = 1,  
                 proj = "+proj=longlat", tz = "Africa/Windhoek", table = "tbl")
gps_tbl

## ----paged.print=FALSE---------------------------------------------------
gps_dt <- track(x = raw$lon, y = raw$lat, t = paste(raw$date, raw$time), id = 1,  
                proj = "+proj=longlat", tz = "Africa/Windhoek", table = "dt")
gps_dt

