.missing_t <- function(x, begin = NULL, end = NULL, step = NULL) {
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

    out <- x[0:length(idx), ]

    if (nrow(out) > 0) {
      out[["t"]] <- full_seq[idx]
      out[, !names(out) %in% c("id", "t")] <- NA
    }

    out
  }, simplify = FALSE)

  bind_tracks(lapply(out, function(out) out))
}

.missing_xyz <- function(x, begin = NULL, end = NULL) {
  if (is.null(begin)) {
    begin <- min(x[["t"]], na.rm = TRUE)
  }

  if (is.null(end)) {
    end <- max(x[["t"]], na.rm = TRUE)
  }

  ix <- x[["t"]] >= begin & x[["t"]] <= end
  ix[is.na(ix)] <- FALSE

  out <- by(x[ix, ], x[ix, ][["id"]], function(x) {
    na_x <- which(is.na(x[["x"]]))
    na_y <- which(is.na(x[["y"]]))
    na_z <- which(is.na(x[["z"]]))
    x[unique(c(na_x, na_y, na_z)), ]
  }, simplify = FALSE)

  bind_tracks(lapply(out, function(out) out))
}

#' @title Find Missing Data in a Track Table
#'
#' @description This function attempts to automatically detect missing data (for
#'  instance due to writing errors) in \code{\link{track}} tables.
#'
#' @param x A \code{\link{track}} table as produced by the \code{\link{track}}
#'  function.
#'
#' @param begin A full time stamp (date+time) in \code{\link{POSIXct}} format
#'  corresponding to the time from which the missing data should be looked for.
#'  If not set, the first time stamp of the track table will be used.
#'
#' @param end A full time stamp (date+time) in \code{\link{POSIXct}} format
#'  corresponding to the time until which the missing data should be looked for.
#'  If not set, the last time stamp of the track table will be used.
#'
#' @param step A \code{\link{difftime}} object representing the expected time
#'  between two consecutive locations of the trajectory. If not set, it is set
#'  to the most common time difference between successive locations in \code{x}.
#'
#' @return A track table of all observations with missing data. The missing data
#'  is indicated with \code{\link{NA}}.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{track}}
#'
#' @examples
#' # Create data set with missing time stamps
#' data(short_tracks)
#' t_df <- track(x = short_tracks$x, y = short_tracks$y, t = short_tracks$t,
#'               id = short_tracks$id, proj = "+proj=longlat",
#'               tz = "Africa/Windhoek", table = "df")
#' t_df <- t_df[-c(10, 100), ]
#'
#' # Find missing data
#' missing <- missing_data(t_df)
#'
#' @export
missing_data <- function(x, begin = NULL, end = NULL, step = NULL) {
  if (!(is_track(x)))
    stop("x is not a track table.")

  out <- bind_tracks(.missing_t(x, begin, end, step),
                     .missing_xyz(x, begin, end))
  out[order(out$id, out$t), ]
}


#' @title Find Duplicated Data in a Track Table
#'
#' @description This function attempts to automatically detect duplicated
#'  data in \code{\link{track}} tables.
#'
#' @param x \code{\link{track}} table as produced by the \code{\link{track}}
#'  function.
#'
#' @param type A character string or a vector of character strings indicating
#'  the type of duplications to look for. The strings can be any combination
#'  of \code{"t"} (for time duplications) and \code{"x"}, \code{"y"}, \code{"z"}
#'  (for coordinate duplications). For instance, the string \code{"txy"} will
#'  return data with duplicated time stamps and duplicated x and y coordinates.
#'
#' @return A track table of all observations that are duplicated, as per the
#'  duplication rule defined by \code{type}.
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
#' duplicated <- duplicated_data(t_df)
#'
#' @export
duplicated_data <- function(x, type = "txy") {
  if (!(is_track(x)))
    stop("x is not a track table.")

  out <- by(x, x$id, function(x) {
    dup <- cbind(
      (duplicated(x[["t"]]) | duplicated(x[["t"]], fromLast = TRUE)) & !is.na(x[["t"]]),
      (duplicated(x[["x"]]) | duplicated(x[["x"]], fromLast = TRUE)) & !is.na(x[["x"]]),
      (duplicated(x[["y"]]) | duplicated(x[["y"]], fromLast = TRUE)) & !is.na(x[["y"]]),
      (duplicated(x[["z"]]) | duplicated(x[["z"]], fromLast = TRUE)) & !is.na(x[["z"]]))
    ix <- apply(dup, 1, any)
    out <- x[ix, ]
    out[["duplicate"]] <- apply(
      dup[ix, , drop = FALSE], 1,
      function(bool) {
        paste0(c("t", "x", "y", "z")[which(bool)], collapse = "")
      })
    unique(out)
  }, simplify = FALSE)

  out <- bind_tracks(lapply(out, function(out) out))
  type <- sapply(gsub(" ", "", type), function(x) intToUtf8(sort(utf8ToInt(x))))
  out[grepl(paste(type, collapse = "|"), out[["duplicate"]]), ]
}


#' @title Find Inconsistent Locations in a Track Table
#'
#' @description This function attempts to automatically detect inconsistent
#'  locations (for instance due to a writing error or GPS inaccuracies) in
#'  \code{\link{track}} tables.
#'
#' @param x \code{\link{track}} table as produced by the \code{\link{track}}
#'  function.
#'
#' @param s The discrimination threshold for the outlier detection algorithm.
#'  Higher values correspond to less outliers.
#'
#' @return A track table of all inconsistent data.
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
#' inconsistent <- inconsistent_data(t_df)
#'
#' @export
inconsistent_data <- function(x, s = 15) {
  if (!(is_track(x)))
    stop("x is not a track table.")

  out <- by(x, x$id, function(x) {
    nas1 <- is.na(x$x) | is.na(x$t)
    m1 <- stats::loess(x ~ as.numeric(t), data = x, span = 0.05, degree = 2,
                       na.action = "na.omit")
    r <- rep(NA, nrow(x))
    r[!nas1] <- abs(stats::residuals(m1))
    r[r == 0] <- min(r[r > 0])
    m1 <- stats::loess(x ~ as.numeric(t), data = x, span = 0.05, degree = 2,
                       weights = 1 / r, na.action = "na.omit")

    nas2 <- is.na(x$y) | is.na(x$t)
    m2 <- stats::loess(y ~ as.numeric(t), data = x, span = 0.05, degree = 2,
                       na.action = "na.omit")
    r <- rep(NA, nrow(x))
    r[!nas2] <- abs(stats::residuals(m2))
    r[r == 0] <- min(r[r > 0])
    m2 <- stats::loess(y ~ as.numeric(t), data = x, span = 0.05, degree = 2,
                       weights = 1 / r, na.action = "na.omit")

    r1 <- sqrt(abs(m1$residuals))
    r2 <- sqrt(abs(m2$residuals))
    out1 <- rep(NA, nrow(x))
    out1[!nas1] <- r1 > (stats::median(r1) + s * stats::IQR(r1))
    out2 <- rep(NA, nrow(x))
    out2[!nas2] <- r2 > (stats::median(r2) + s * stats::IQR(r2))
    ix <- unique(c(which(out1), which(out2)))

    x[ix, ]
  })

  bind_tracks(lapply(out, function(out) out))
}
