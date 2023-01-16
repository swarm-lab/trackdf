## -----------------------------------------------------------------------------
library(trackdf)

data("tracks")
print(tracks, max = 10 * ncol(tracks))

## -----------------------------------------------------------------------------
is_track(tracks)

## -----------------------------------------------------------------------------
is_geo(tracks)

## -----------------------------------------------------------------------------
n_tracks(tracks)

## -----------------------------------------------------------------------------
n_dims(tracks)

## -----------------------------------------------------------------------------
print(tracks, max = 10 * ncol(tracks))

## -----------------------------------------------------------------------------
projection(tracks)

## -----------------------------------------------------------------------------
projection(tracks) <- "+proj=somerc +lat_0=46.9524056 +lon_0=7.43958333 +ellps=bessel +x_0=2600000 +y_0=1200000 +towgs84=674.374,15.056,405.346 +units=m +k_0=1 +no_defs"
print(tracks, max = 10 * ncol(tracks))

## -----------------------------------------------------------------------------
projection(tracks) <- "+proj=longlat" 
print(tracks, max = 10 * ncol(tracks))

## -----------------------------------------------------------------------------
tracks_somerc <- project(tracks, "+proj=somerc +lat_0=46.9524056 +lon_0=7.43958333 +ellps=bessel +x_0=2600000 +y_0=1200000 +towgs84=674.374,15.056,405.346 +units=m +k_0=1 +no_defs")
print(tracks_somerc, max = 10 * ncol(tracks))

## -----------------------------------------------------------------------------
raw1 <- read.csv(system.file("extdata/gps/02.csv", package = "trackdf"))
raw2 <- read.csv(system.file("extdata/gps/03.csv", package = "trackdf"))
raw3 <- read.csv(system.file("extdata/video/01.csv", package = "trackdf"))

track1 <- track(x = raw1$lon, y = raw1$lat, t = paste(raw1$date, raw1$time), 
                id = 1, proj = "+proj=longlat", tz = "Africa/Windhoek")
track2 <- track(x = raw2$lon, y = raw2$lat, t = paste(raw2$date, raw2$time), 
                id = 2, proj = "+proj=longlat", tz = "Africa/Windhoek")
track3 <- track(x = raw3$x, y = raw3$y, t = raw3$frame, id = raw3$track_fixed, 
                origin = "2019-03-24 12:55:23", period = "0.04S", 
                tz = "America/New_York")

## ---- error=TRUE--------------------------------------------------------------
bounded_tracks <- bind_tracks(track1, track2, track3)

## -----------------------------------------------------------------------------
bounded_tracks <- rbind(track1, track2, track3)
print(bounded_tracks, max = 10 * ncol(bounded_tracks))

## -----------------------------------------------------------------------------
library(dplyr)

filtered_tracks <- tracks %>%
  filter(., t >= as.POSIXct("2015-09-10 07:01:00", tz = "Africa/Windhoek"),
         t <= as.POSIXct("2015-09-10 07:11:00 CAT", tz = "Africa/Windhoek"))
print(filtered_tracks, max = 10 * ncol(filtered_tracks))

## -----------------------------------------------------------------------------
library(ggplot2)

ggplot(data = tracks) +
  aes(x = x, y = y, color = id) +
  geom_path() +
  coord_map()

