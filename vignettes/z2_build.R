## ----message=FALSE, warning=FALSE---------------------------------------------
raw <- read.csv(system.file("extdata/video/01.csv", package = "trackdf"))
print(raw, max = 10 * ncol(raw))

## -----------------------------------------------------------------------------
library(trackdf)

tt <- track(x = raw$x, y = raw$y, t = raw$frame, id = raw$track_fixed)
print(tt, max = 10 * ncol(tt))

## -----------------------------------------------------------------------------
tt <- track(x = raw$x, y = raw$y, t = raw$frame, id = raw$track_fixed, 
            origin = "2019-03-24 12:55:23", 
            period = "0.04S", # 1/25 of a second
            tz = "America/New_York")
print(tt, max = 10 * ncol(tt))

## -----------------------------------------------------------------------------
tt <- track(x = raw$x, y = raw$y, t = raw$frame, id = raw$track_fixed, 
            ignore = raw$ignore,
            origin = "2019-03-24 12:55:23", 
            period = "0.04S", # 1/25 of a second
            tz = "America/New_York")
print(tt, max = 10 * ncol(tt))

## -----------------------------------------------------------------------------
tt <- track(x = raw$x, y = raw$y, t = raw$frame, id = raw$track_fixed, 
            ignore = raw$ignore,
            origin = "2019-03-24 12:55:23", 
            period = "0.04S", # 1/25 of a second
            tz = "America/New_York",
            table = "tbl")
print(tt)

## -----------------------------------------------------------------------------
tt <- track(x = raw$x, y = raw$y, t = raw$frame, id = raw$track_fixed, 
            ignore = raw$ignore,
            origin = "2019-03-24 12:55:23", 
            period = "0.04S", # 1/25 of a second
            tz = "America/New_York",
            table = "dt")
print(tt)

## ----message=FALSE, warning=FALSE---------------------------------------------
raw <- read.csv(system.file("extdata/gps/02.csv", package = "trackdf"))
print(raw, max = 10 * ncol(raw))

## ----paged.print=FALSE--------------------------------------------------------
tt <- track(x = raw$lon, y = raw$lat, t = paste(raw$date, raw$time), id = 1,  
                proj = "+proj=longlat", tz = "Africa/Windhoek")
print(tt, max = 10 * ncol(tt))

