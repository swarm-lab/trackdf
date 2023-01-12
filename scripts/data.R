library(trackdf)

raw1 <- read.csv(system.file("extdata/gps/01.csv", package = "trackdf"))
df1 <- track(x = raw1$lon, y = raw1$lat, t = paste(raw1$date, raw1$time), id = 1,
             proj = "+proj=longlat", tz = "Africa/Windhoek", table = "df")
df1 <- df1[complete.cases(df1), ]

raw2 <- read.csv(system.file("extdata/gps/02.csv", package = "trackdf"))
df2 <- track(x = raw2$lon, y = raw2$lat, t = paste(raw2$date, raw2$time), id = 2,
             proj = "+proj=longlat", tz = "Africa/Windhoek", table = "df")
df2 <- df2[complete.cases(df2), ]

tracks <- bind_tracks(df1, df2)
short_tracks <- tracks[tracks$t >= "2015-09-10 07:01:00" &
                         tracks$t <= "2015-09-10 07:01:59", ]

save(tracks, file = "data/tracks.rda")
save(short_tracks, file = "data/short_tracks.rda")