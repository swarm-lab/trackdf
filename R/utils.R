#' @importFrom sf st_crs
#' @importFrom sf sf_project
#'
#' @title Access/Modify the Projection of a Track Table
#'
#' @description Functions to access or modify the projection of a data table.
#'  Changing the projection will trigger automatically the conversion of the
#'  locations in the new coordinate system.
#'
#' @param x A track table.
#'
#' @param value A character string or a \code{\link[terra:crs]{terra::crs}}
#'  object representing the projection of the coordinates.
#'  \code{"+proj=longlat"} is suitable for the outputs of most GPS trackers.
#'
#' @return A track table.
#'
#' @note It is not possible to modify the projection if missing coordinates
#'  are present.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' data(short_tracks)
#'
#' projection(short_tracks)
#' tracks_projected <- project(short_tracks, "+proj=somerc")
#' projection(tracks_projected)
#' projection(tracks_projected) <- "+proj=longlat"
#' projection(tracks_projected)
#'
#' @export
projection <- function(x) {
  if (!is_track(x))
    stop("This is not a track_df object.")

  attr(x, "proj")
}


#' @rdname projection
#'
#' @export
`projection<-` <- function(x, value = "+proj=longlat") {
  if (!is_track(x))
    stop("This is not a track_df object.")

  if (is.character(value)) {
    value <- sf::st_crs(value)
  } else if (!inherits(value, "crs")) {
    stop("value must be an object of class character or crs")
  }

  if (!is.null(methods::slotNames(attr(x, "proj")))) {
    tmp <- x[, c("x", "y")]

    if (sum(stats::complete.cases(tmp)) < nrow(tmp))
      stop("The projection cannot be modified when missing coordinates are present.")

    tmp <- sf::sf_project(sf::st_crs(attr(x, "proj")), value, tmp)
    x[, c("x", "y")] <- tmp
  }

  attr(x, "proj") <- value
  x
}

#' @rdname projection
#'
#' @export
project <- function(x, value) {
  if (!is_track(x))
    stop("This is not a track_df object.")

  projection(x) <- value
  x
}


#' @title Check if Track Table Uses Geographic Coordinates
#'
#' @description Track tables produced by \code{\link{track_df}} can use a
#'  cartesian (x, y, z) or a geographic (latitude, longitude, altitude)
#'  coordinate system. This function helps determine which is being used in a
#'  particular table.
#'
#' @param x A track data table as produced by \code{\link{track_df}}.
#'
#' @return A logical.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' data(short_tracks)
#'
#' is_geo(short_tracks)
#'
#' @export
is_geo <- function(x) {
  if (!is_track(x)) {
    stop("This is not a track_df object.")
  } else {
    !is.na(attr(x, "proj")$wkt)
  }
}


#' @title Number of Spatial Dimensions of a Track Table
#'
#' @description Track tables produced by \code{\link{track_df}} can have 2 (x,y)
#'  or 3 (x, y, z) spatial dimensions. This function returns the number of
#'  spatial dimensions of a track table.
#'
#' @param x A track data table as produced by \code{\link{track_df}}.
#'
#' @return A numeric value.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' data(short_tracks)
#'
#' n_dims(short_tracks)
#'
#' @export
n_dims <- function(x) {
  if (!is_track(x)) {
    stop("This is not a track_df object.")
  } else {
    sum(c("x", "y", "z") %in% names(x))
  }
}


#' @title Number of Tracks in a Track Table
#'
#' @description Track tables produced by \code{\link{track_df}} can contain
#'  multiple tracks (e.g., from different animals). This function returns the
#'  number of tracks in a track table.
#'
#' @param x A track data table as produced by \code{\link{track_df}}.
#'
#' @return A numeric value.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' data(short_tracks)
#'
#' n_tracks(short_tracks)
#'
#' @export
n_tracks <- function(x) {
  if (!is_track(x)) {
    stop("This is not a track_df object.")
  } else {
    length(unique(x$id))
  }
}


#' @title Compute The Mode(s) Of A Discrete Distribution
#'
#' @description This is an internal utility function to compute the mode(s) of
#'  a discrete distribution.
#'
#' @param x A vector or matrix of discrete values.
#'
#' @param na.rm A logical value indicating whether NA values should be stripped
#'  before the computation proceeds (default: TRUE).
#'
#' @return A vector of values corresponding to the mode(s) of x.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
.Mode <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
