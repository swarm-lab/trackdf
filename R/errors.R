#' @title Find Missing Data in a Track Table
#'
#' @description This function attempts to automatically detect missing data (for
#'  instance due to writing errors) in \code{\link{track}} tables.
#'
#' @param x A \code{\link{track}} table as produced by the \code{\link{track}}
#'  function.
#'
#' @param begin A full timestamp (date+time) in \code{\link{POSIXct}} format
#'  corresponding to the time from which the missing data should be looked for.
#'  If not set, the first timestamp of the track table will be used.
#'
#' @param end A full timestamp (date+time) in \code{\link{POSIXct}} format
#'  corresponding to the time until which the missing data should be looked for.
#'  If not set, the last timestamp of the track table will be used.
#'
#' @param step A \code{\link{difftime}} object representing the expected time
#'  between two consecutive locations of the trajectory. If not set, it is set
#'  to the most common time difference between successive locations in \code{x}.
#'
#' @return A data frame/table/tibble with the missing timestamps for each
#'  individual track in \code{x}. If no missing data are detected, the function
#'  returns \code{NULL}.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{track}}
#'
#' @examples
#' # Create data set with missing timestamps
#' data(short_tracks)
#' t_df <- track(x = short_tracks$x, y = short_tracks$y, t = short_tracks$t,
#'               id = short_tracks$id, proj = "+proj=longlat",
#'               tz = "Africa/Windhoek", table = "df")
#' t_df <- t_df[-c(10, 100), ]
#'
#' # Find missing data
#' missing <- missing_t(t_df)
#'
#' @export
missing_t <- function(x, begin = NULL, end = NULL, step = NULL) {
  if (!(is_track(x)))
    stop("x is not a track table.")

  out <- by(x, x$id, function(x) {
    if (is.null(step)) {
      d <- diff(x[["t"]])
      u <- units(d)
      step <- as.difftime(.Mode(d)[1], units = u)
    }

    if (is.null(begin)) {
      begin <- min(x[["t"]], na.rm = TRUE)
    }

    if (is.null(end)) {
      end <- max(x[["t"]], na.rm = TRUE)
    }

    full_seq <- seq(begin, end, step)
    my_seq <- x[["t"]]
    my_seq <- my_seq[!(duplicated(my_seq) & !is.na(my_seq))]
    m1 <- match(full_seq, my_seq)
    idx <- which(is.na(m1))

    if (length(idx) > 0) {
      if (inherits(x, "tbl")) {
        tibble::tibble(id = x$id[1], t = full_seq[idx])
      } else if (inherits(x, "data.table")) {
        data.table::data.table(id = x$id[1], t = full_seq[idx])
      } else {
        data.frame(id = x$id[1], t = full_seq[idx])
      }
    } else {
      NULL
    }
  })

  do.call(rbind, out)
}


#' @title Find Missing Locations in a Track Table
#'
#' @description This function attempts to automatically detect missing location
#'  data in \code{\link{track}} tables.
#'
#' @param x A \code{\link{track}} table as produced by the \code{\link{track}}
#'  function.
#'
#' @return A data frame/table/tibble with the timestamps and type of missing
#'  data for each individual track in \code{x}. If no missing data are detected,
#'  the function returns \code{NULL}.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{track}}
#'
#' @examples
#' # Create data set with missing coordinates
#' data(short_tracks)
#' t_df <- track(x = short_tracks$x, y = short_tracks$y, t = short_tracks$t,
#'               id = short_tracks$id, proj = "+proj=longlat",
#'               tz = "Africa/Windhoek", table = "df")
#' t_df$x[c(10, 100)] <- NA
#' t_df$y[c(20, 110)] <- NA
#'
#' # Find missing data
#' missing <- missing_xy(t_df)
#'
#' @export
missing_xy <- function(x) {
  if (!(is_track(x)))
    stop("x is not a track table.")

  out <- by(x, x$id, function(x) {
    na_x <- which(is.na(x$x))
    na_y <- which(is.na(x$y))

    if (length(na_x) > 0 | length(na_y) > 0) {
      if (inherits(x, "tbl")) {
        tibble::tibble(id = x$id[1],
                       t = c(x[["t"]][na_x], x[["t"]][na_y]),
                       missing = c(rep("x", length(na_x)),
                                   rep("y", length(na_y))))
      } else if (inherits(x, "data.table")) {
        data.table::data.table(id = x$id[1],
                               t = c(x[["t"]][na_x], x[["t"]][na_y]),
                               missing = c(rep("x", length(na_x)),
                                           rep("y", length(na_y))))
      } else {
        data.frame(id = x$id[1],
                   t = c(x[["t"]][na_x], x[["t"]][na_y]),
                   missing = c(rep("x", length(na_x)),
                               rep("y", length(na_y))))
      }
    } else {
      NULL
    }
  })

  do.call(rbind, out)
}


#' @title Find Duplicated Timestamps in a Track Table
#'
#' @description This function attempts to automatically detect duplicated
#'  timestamps in \code{\link{track}} tables.
#'
#' @param x \code{\link{track}} table as produced by the \code{\link{track}}
#'  function.
#'
#' @return A data frame/table/tibble with the duplicated timestamps for each
#'  individual track in \code{x}. If no duplicated data are detected, the
#'  function returns \code{NULL}.
#'
#' @note Incomplete data (that is, data containing "NAs") are ignored.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{track}}
#'
#' @examples
#' # Create data set with duplicated data
#' data(short_tracks)
#' t_df <- track(x = short_tracks$x, y = short_tracks$y, t = short_tracks$t,
#'               id = short_tracks$id, proj = "+proj=longlat",
#'               tz = "Africa/Windhoek", table = "df")
#' t_df <- bind_tracks(t_df, t_df[1:10, ], t_df[100:110, ])
#'
#' # Find duplicated timestamps
#' duplicated <- duplicated_t(t_df)
#'
#' @export
duplicated_t <- function(x) {
  if (!(is_track(x)))
    stop("x is not a track table.")

  out <- by(x, x$id, function(x) {
    idx <- which(duplicated(x[["t"]]) & !is.na(x[["t"]]))

    if (length(idx) > 0) {
      if (inherits(x, "tbl")) {
        tibble::tibble(id = x$id[1], t = x[["t"]][idx])
      } else if (inherits(x, "data.table")) {
        data.table::data.table(id = x$id[1], t = x[["t"]][idx])
      } else {
        data.frame(id = x$id[1], t = x[["t"]][idx])
      }
    } else {
      NULL
    }
  })

  do.call(rbind, out)
}


#' @title Find Duplicated Data in a Track Table
#'
#' @description This function attempts to automatically detect fully duplicated
#'  data in \code{\link{track}} tables.
#'
#' @param x \code{\link{track}} table as produced by the \code{\link{track}}
#'  function.
#'
#' @return A data frame/table/tibble with the duplicated timestamps and
#'  coordinates for each individual track in \code{x}. If no duplicated data are
#'  detected, the function returns \code{NULL}.
#'
#' @note Incomplete data (that is, data containing "NAs") are ignored.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{track}}
#'
#' @examples
#' # Create data set with duplicated data
#' data(short_tracks)
#' t_df <- track(x = short_tracks$x, y = short_tracks$y, t = short_tracks$t,
#'               id = short_tracks$id, proj = "+proj=longlat",
#'               tz = "Africa/Windhoek", table = "df")
#' t_df <- bind_tracks(t_df, t_df[1:10, ], t_df[100:110, ])
#'
#' # Find duplicated data
#' duplicated <- duplicated_xyt(t_df)
#'
#' @export
duplicated_xyt <- function(x) {
  if (!(is_track(x)))
    stop("x is not a track table.")

  out <- by(x, x$id, function(x) {
    idx <- which(duplicated(x[["t"]]) & !is.na(x[["t"]]) &
                   duplicated(x[["x"]]) & !is.na(x[["x"]]) &
                   duplicated(x[["y"]]) & !is.na(x[["y"]]))

    if (length(idx) > 0) {
      if (inherits(x, "tbl")) {
        tibble::as_tibble(x[idx, ])
      } else if (inherits(x, "data.table")) {
        data.table::as.data.table(x[idx, ])
      } else {
        as.data.frame(x[idx, ])
      }
    } else {
      NULL
    }
  })

  do.call(rbind, out)
}


#' @title Find Inconsistent Locations in a Track Table
#'
#' @description This function attempts to automatically detect inconsistent
#'  locations (for instance due to a writing error or GPS innacuracies) in
#'  \code{\link{track}} tables.
#'
#' @param x \code{\link{track}} table as produced by the \code{\link{track}}
#'  function.
#'
#' @param s The discrimination threshold of the outlier detection algorithm.
#'  Higher values correspond to less outliers.
#'
#' @return A data frame/table/tibble with the timestamps and coordinates of the
#'  the inconsistent data for each individual track in \code{x}. If no
#'  inconsistencies are detected, the function returns \code{NULL}.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{track}}
#'
#' @examples
#' # Create data set with inconsistent data
#' data(tracks)
#' t_df <- track(x = tracks$x, y = tracks$y, t = tracks$t,
#'               id = tracks$id, proj = "+proj=longlat",
#'               tz = "Africa/Windhoek", table = "df")
#' t_df$x[1000] <- t_df$x[1000] * 1.0001
#' t_df$y[4000] <- t_df$y[4000] * 1.00001
#'
#' # Find inconsistent data
#' inconsitent <- inconsistent_xy(t_df)
#'
#' @export
inconsistent_xy <- function(x, s = 5) {
  if (!(is_track(x)))
    stop("x is not a track table.")

  out <- by(x, x$id, function(x) {
    nas1 <- is.na(x$x) | is.na(x$t)
    m1 <- stats::loess(x ~ as.numeric(t), data = x, span = 0.05, degree = 2)
    r <- rep(NA, nrow(x))
    r[!nas1] <- abs(stats::residuals(m1))
    r[r == 0] <- min(r[r > 0])
    m1 <- stats::loess(x ~ as.numeric(t), data = x, span = 0.05, degree = 2,
                       weights = 1 / r)

    nas2 <- is.na(x$y) | is.na(x$t)
    m2 <- stats::loess(y ~ as.numeric(t), data = x, span = 0.05, degree = 2)
    r <- rep(NA, nrow(x))
    r[!nas2] <- abs(stats::residuals(m2))
    r[r == 0] <- min(r[r > 0])
    m2 <- stats::loess(y ~ as.numeric(t), data = x, span = 0.05, degree = 2,
                       weights = 1 / r)

    r1 <- sqrt(abs(m1$residuals))
    r2 <- sqrt(abs(m2$residuals))
    out1 <- rep(NA, nrow(x))
    out1[!nas1] <- r1 > stats::median(r1) + s * stats::IQR(r1)
    out2 <- rep(NA, nrow(x))
    out2[!nas2] <- r2 > stats::median(r2) + s * stats::IQR(r2)
    idx <- unique(c(which(out1), which(out2)))

    if (length(idx) > 0) {
      if (inherits(x, "tbl")) {
        tibble::as_tibble(x[idx, ])
      } else if (inherits(x, "data.table")) {
        data.table::as.data.table(x[idx, ])
      } else {
        as.data.frame(x[idx, ])
      }
    } else {
      NULL
    }
  })

  do.call(rbind, out)
}
