## -----------------------------------------------------------------------------
library(trackdf)
raw <- read.csv(system.file("extdata/gps/01.csv", package = "trackdf"))
tt <- track(x = raw$lon, y = raw$lat, t = paste(raw$date, raw$time), id = 1,  
            proj = "+proj=longlat", tz = "Africa/Windhoek")
print(tt, max = 10 * ncol(tt))

## -----------------------------------------------------------------------------
missing <- missing_data(tt)
missing

## -----------------------------------------------------------------------------
dups <- duplicated_data(tt, type = "t")
dups

## -----------------------------------------------------------------------------
inconsistent <- inconsistent_data(tt, s = 15)
inconsistent

