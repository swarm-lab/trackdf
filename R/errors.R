#' @title Find Missing Data in a Track Table
#'
#' @description This function attempts to automatically detect missing data (for
#'  instance due to writing errors) in \code{\link{track}} tables.
#'
#' @param x A \code{\link{track}} table as produced by the \code{\link{track}}
#'  function. The table should only contain the observations for a single
#'  individual. See the examples below to handle multiple individuals.
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
#' @return A vector of timestamps missing from \code{x}. If no missing data are
#'  detected, the function returns an empty vector.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{track}}, \code{\link{fix_missing_t}}
#'
#' @examples
#' # Create data set with missing timestamps
#' data(short_tracks)
#' t_df <- track(x = short_tracks$x, y = short_tracks$y, t = short_tracks$t,
#'               id = short_tracks$id, proj = "+proj=longlat",
#'               tz = "Africa/Windhoek", table = "df")
#' t_df <- t_df[-c(10, 100), ]
#'
#' # Find missing data for one individual
#' missing_one <- missing_t(t_df[t_df$id == 1, ])
#'
#' # Find missing data for all individuals
#' missing_all <- lapply(split(t_df, t_df$id), missing_t)
#'
#' @export
missing_t <- function(x, begin = NULL, end = NULL, step = NULL) {
  if (!(is_track(x)))
    stop("x is not a track table.")

  if (n_tracks(x) > 1)
    stop("x contains observations for multiple individuals.")

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
  full_seq[idx]
}


#' @title Find Duplicated Timestamps in a Track Table
#'
#' @description This function attempts to automatically detect duplicated
#'  timestamps in \code{\link{track}} tables.
#'
#' @param x \code{\link{track}} table as produced by the \code{\link{track}}
#'  function. The table should only contain the observations for a single
#'  individual. See the examples below to handle multiple individuals.
#'
#' @return A vector of timestamps duplicated in \code{x}. If no duplicated
#'  timestamps are detected, the function returns an empty vector.
#'
#' @note Missing timestamps ("NA") are ignored.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{track}}, \code{\link{fix_t_duplicates}}
#'
#' @examples
#' # Create data set with duplicated data
#' data(short_tracks)
#' t_df <- track(x = short_tracks$x, y = short_tracks$y, t = short_tracks$t,
#'               id = short_tracks$id, proj = "+proj=longlat",
#'               tz = "Africa/Windhoek", table = "df")
#' t_df <- bind_tracks(t_df, t_df[1:10, ], t_df[100:110, ])
#'
#' # Find missing data for one individual
#' duplicated_one <- t_duplicates(t_df[t_df$id == 1, ])
#'
#' # Find missing data for all individuals
#' duplicated_all <- lapply(split(t_df, t_df$id), t_duplicates)
#'
#' @export
t_duplicates <- function(x) {
  if (!(is_track(x)))
    stop("x is not a track table.")

  if (n_tracks(x) > 1)
    stop("x contains observations for multiple individuals.")

  idx <- which(duplicated(x[["t"]]) & !is.na(x[["t"]]))
  x[["t"]][idx]
}


#' @title Find Duplicated Data in a Track Table
#'
#' @description This function attempts to automatically detect fully duplicated
#'  data in \code{\link{track}} tables.
#'
#' @param x \code{\link{track}} table as produced by the \code{\link{track}}
#'  function. The table should only contain the observations for a single
#'  individual. See the examples below to handle multiple individuals.
#'
#' @return A vector of indices for the fully duplicated data in \code{x}. If no
#'  duplicated data are detected, the function returns an empty vector.
#'
#' @note Incomplete data (that is, data containing "NAs") are ignored.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{track}}, \code{\link{fix_duplicates}}
#'
#' @examples
#' # Create data set with duplicated data
#' data(short_tracks)
#' t_df <- track(x = short_tracks$x, y = short_tracks$y, t = short_tracks$t,
#'               id = short_tracks$id, proj = "+proj=longlat",
#'               tz = "Africa/Windhoek", table = "df")
#' t_df <- bind_tracks(t_df, t_df[1:10, ], t_df[100:110, ])
#'
#' # Find missing data for one individual
#' duplicated_one <- duplicates(t_df[t_df$id == 1, ])
#'
#' # Find missing data for all individuals
#' duplicated_all <- lapply(split(t_df, t_df$id), duplicates)
#'
#' @export
duplicates <- function(x) {
  if (!(is_track(x)))
    stop("x is not a track table.")

  if (n_tracks(x) > 1)
    stop("x contains observations for multiple individuals.")

  idx <- which(duplicated(x[["t"]]) & !is.na(x[["t"]]) &
                 duplicated(x[["x"]]) & !is.na(x[["x"]]) &
                 duplicated(x[["y"]]) & !is.na(x[["y"]]))
  idx
}


#' @title Find Inconsistent Locations in a Track Table
#'
#' @description This function attempts to automatically detect inconsistent
#'  locations (for instance due to a writing error or GPS innacuracies) in
#'  \code{\link{track}} tables.
#'
#' @param x \code{\link{track}} table as produced by the \code{\link{track}}
#'  function. The table should only contain the observations for a single
#'  individual. See the examples below to handle multiple individuals.
#'
#' @param s The discrimination threshold of the outlier detection algorithm.
#'  Higher values correspond to less outliers.
#'
#' @return A vector of indices for the inconsistent locations in \code{x}. If no
#'  inconsistencies are detected, the function returns an empty vector.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{track}}, \code{\link{fix_xy_errors}}
#'
#' @examples
#' # TODO
#'
#' @export
xy_errors <- function(x, s = 15, span = 0.05, degree = 2, ...) {
  if (!(is_track(x)))
    stop("x is not a track table.")

  if (n_tracks(x) > 1)
    stop("x contains observations for multiple individuals.")

  nas1 <- is.na(x$x) | is.na(x$t)
  m1 <- stats::loess(x ~ as.numeric(t), data = x, span = span,
                     degree = degree, ...)
  r <- rep(NA, nrow(x))
  r[!nas1] <- abs(stats::residuals(m1))
  r[r == 0] <- min(r[r > 0])
  m1 <- stats::loess(x ~ as.numeric(t), data = x, span = span,
                     degree = degree, weights = 1 / r, ...)

  nas2 <- is.na(x$y) | is.na(x$t)
  m2 <- stats::loess(y ~ as.numeric(t), data = x, span = span,
                     degree = degree, ...)
  r <- rep(NA, nrow(x))
  r[!nas2] <- abs(stats::residuals(m2))
  r[r == 0] <- min(r[r > 0])
  m2 <- stats::loess(y ~ as.numeric(t), data = x, span = span,
                     degree = degree, weights = 1 / r, ...)

  r1 <- sqrt(abs(m1$residuals))
  r2 <- sqrt(abs(m2$residuals))
  out1 <- rep(NA, nrow(x))
  out1[!nas1] <- r1 > stats::median(r1) + s * stats::IQR(r1)
  out2 <- rep(NA, nrow(x))
  out2[!nas2] <- r2 > stats::median(r2) + s * stats::IQR(r2)
  idx <- unique(c(which(out1), which(out2)))
  idx
}
