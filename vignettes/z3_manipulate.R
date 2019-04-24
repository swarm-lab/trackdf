## ------------------------------------------------------------------------
library(trackdf)

data("tracks")
tracks

## ------------------------------------------------------------------------
is_track_df(tracks)
is_geo(tracks)

## ------------------------------------------------------------------------
head(tracks$id)
head(tracks[["t"]])

## ------------------------------------------------------------------------
tracks[1]
tracks[1, ]
tracks[1, 1]
head(tracks[, 1])

## ------------------------------------------------------------------------
tracks$id[tracks$id == "1"] <- "0"
tracks

tracks[tracks[, 1] == "0", 1] <- "1"
tracks

## ------------------------------------------------------------------------
projection(tracks)

## ------------------------------------------------------------------------
projection(tracks) <- "+proj=somerc +lat_0=46.9524056 +lon_0=7.43958333 +ellps=bessel +x_0=2600000 +y_0=1200000 +towgs84=674.374,15.056,405.346 +units=m +k_0=1 +no_defs"
tracks

projection(tracks) <- "+proj=longlat" 
tracks

## ------------------------------------------------------------------------
tracks_new_proj <- project(tracks, "+proj=somerc +lat_0=46.9524056 +lon_0=7.43958333 +ellps=bessel +x_0=2600000 +y_0=1200000 +towgs84=674.374,15.056,405.346 +units=m +k_0=1 +no_defs")

tracks_new_proj
tracks

## ------------------------------------------------------------------------
library(dplyr)

tracks %>%
  filter(., t < as.POSIXct("2015-09-10 07:01:00", tz = "Africa/Windhoek"))

tracks %>%
  group_by(., id) %>%
  summarize(., x = mean(x),
            y = mean(y))

## ------------------------------------------------------------------------
track1 <- filter(tracks, id == "1")
track2 <- filter(tracks, id == "2")

bind_track_df(track1, track2)
rbind(track1, track2)

## ------------------------------------------------------------------------
plot(y ~ x , data = track1, type = "l", col = "red", xlim = range(tracks$x), 
     lwd = 2, asp = 1)
lines(y ~ x , data = track2, type = "l", col = "blue",  lwd = 2)

## ------------------------------------------------------------------------
library(ggplot2)

ggplot(data = tracks, mapping = aes(x = x, y = y, color = id)) +
  geom_path() +
  coord_map()

