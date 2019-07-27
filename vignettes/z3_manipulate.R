## ----paged.print=FALSE---------------------------------------------------
library(trackdf)
data("tracks")

## ----paged.print=FALSE---------------------------------------------------
t_df <- track_df(x = tracks$x, y = tracks$y, t = tracks$t, id = tracks$id,
  proj = "+proj=longlat", tz = "Africa/Windhoek")
head(t_df)

## ----paged.print=FALSE---------------------------------------------------
t_tbl <- track_tbl(x = tracks$x, y = tracks$y, t = tracks$t, id = tracks$id,
  proj = "+proj=longlat", tz = "Africa/Windhoek")
t_tbl 

## ----paged.print=FALSE---------------------------------------------------
t_dt <- track_dt(x = tracks$x, y = tracks$y, t = tracks$t, id = tracks$id,
  proj = "+proj=longlat", tz = "Africa/Windhoek")
t_dt

## ------------------------------------------------------------------------
is_track(t_df)
is_track(t_tbl)
is_track(t_dt)

## ------------------------------------------------------------------------
is_geo(t_df)
is_geo(t_tbl)
is_geo(t_dt)

## ----paged.print=FALSE---------------------------------------------------
head(t_df$id)
head(t_tbl$id)
head(t_dt$id)

## ----paged.print=FALSE---------------------------------------------------
head(t_df[["t"]])
head(t_tbl[["t"]])
head(t_dt[["t"]])

## ----paged.print=FALSE---------------------------------------------------
head(t_df[1])
t_tbl[1]
t_dt[1]

## ----paged.print=FALSE---------------------------------------------------
t_df[1, ]
t_tbl[1, ]
t_dt[1, ]

## ----paged.print=FALSE---------------------------------------------------
t_df[1, 1]
t_tbl[1, 1]
t_dt[1, 1]

## ------------------------------------------------------------------------
t_df$id[t_df$id == "1"] <- "0"
head(t_df)

## ------------------------------------------------------------------------
t_df[t_df[, 1] == "0", 1] <- "1"
head(t_df)

