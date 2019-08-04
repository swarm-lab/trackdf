## ----paged.print=FALSE---------------------------------------------------
library(trackdf)
data("tracks")

## ----paged.print=FALSE---------------------------------------------------
t_df <- track(x = tracks$x, y = tracks$y, t = tracks$t, id = tracks$id,
  proj = "+proj=longlat", tz = "Africa/Windhoek", table = "df")
head(t_df)

## ----paged.print=FALSE---------------------------------------------------
t_tbl <- track(x = tracks$x, y = tracks$y, t = tracks$t, id = tracks$id,
  proj = "+proj=longlat", tz = "Africa/Windhoek", table = "tbl")
t_tbl 

## ----paged.print=FALSE---------------------------------------------------
t_dt <- track(x = tracks$x, y = tracks$y, t = tracks$t, id = tracks$id,
  proj = "+proj=longlat", tz = "Africa/Windhoek", table = "dt")
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

## ------------------------------------------------------------------------
projection(t_df)

## ------------------------------------------------------------------------
projection(t_df) <- "+proj=somerc +lat_0=46.9524056 +lon_0=7.43958333 +ellps=bessel +x_0=2600000 +y_0=1200000 +towgs84=674.374,15.056,405.346 +units=m +k_0=1 +no_defs"
head(t_df)

## ------------------------------------------------------------------------
projection(t_df) <- "+proj=longlat" 
head(t_df)

## ------------------------------------------------------------------------
t_df_new_proj <- project(t_df, "+proj=somerc +lat_0=46.9524056 +lon_0=7.43958333 +ellps=bessel +x_0=2600000 +y_0=1200000 +towgs84=674.374,15.056,405.346 +units=m +k_0=1 +no_defs")

head(t_df_new_proj)

## ------------------------------------------------------------------------
if (requireNamespace("dplyr", quietly = TRUE)) {
  library(dplyr)
  
  t_df %>%
    filter(., t < as.POSIXct("2015-09-10 07:01:00", tz = "Africa/Windhoek")) %>%
    head(.)
}

## ------------------------------------------------------------------------
if (requireNamespace("dplyr", quietly = TRUE)) {
  library(dplyr)
  
  t_df %>%
    group_by(., id) %>%
    summarize(., x = mean(x),
              y = mean(y))
}

## ----eval=FALSE----------------------------------------------------------
#  if (requireNamespace("dplyr", quietly = TRUE)) {
#    t_df1 <- dplyr::filter(t_df, id == "1")
#    t_df2 <- dplyr::filter(t_df, id == "2")
#  } else {
#    t_df1 <- t_df[t_df$id == "1", ]
#    t_df2 <- t_df[t_df$id == "2", ]
#  }
#  
#  head(rbind_track(t_df1, t_df2))

## ----paged.print=FALSE, eval=FALSE---------------------------------------
#  if (requireNamespace("dplyr", quietly = TRUE)) {
#    t_tbl1 <- dplyr::filter(t_tbl, id == "1")
#    t_tbl2 <- dplyr::filter(t_tbl, id == "2")
#  } else {
#    t_tbl1 <- t_tbl[t_tbl$id == "1", ]
#    t_tbl2 <- t_tbl[t_tbl$id == "2", ]
#  }
#  
#  rbind_track(t_tbl1, t_tbl2)

## ----paged.print=FALSE, eval=FALSE---------------------------------------
#  if (requireNamespace("dplyr", quietly = TRUE)) {
#    t_dt1 <- dplyr::filter(t_dt, id == "1")
#    t_dt2 <- dplyr::filter(t_dt, id == "2")
#  } else {
#    t_dt1 <- t_dt[t_dt$id == "1", ]
#    t_dt2 <- t_dt[t_dt$id == "2", ]
#  }
#  
#  rbind_track(t_dt1, t_dt2)

## ------------------------------------------------------------------------
plot(y ~ x, data = t_dt[t_dt$id == "1"], type = "l", col = "red", 
     xlim = range(t_dt$x), lwd = 2, asp = 1)
lines(y ~ x , data = t_dt[t_dt$id == "2"], col = "blue",  lwd = 2)

## ------------------------------------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::ggplot(data = t_dt, mapping = ggplot2::aes(x = x, y = y, color = id)) +
    ggplot2::geom_path() +
    ggplot2::coord_map()
} 

