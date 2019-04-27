#' @title Access/Modify the Projection of a Track Table
#'
#' @description Functions to access or modify the projection of a data table.
#'  Changing the projection will trigger automatically the conversion of the
#'  locations in the new coordinate system.
#'
#' @param x A track table.
#'
#' @param value A character string or a \code{\link[sp:CRS]{sp::CRS}} object
#'  representing the projection of the coordinates. \code{"+proj=longlat"} is
#'  suitable for the outputs of most GPS trackers.
#'
#' @return A track table.
#'
#' @note It is not possible to modify the projection if missing coordinates
#'  are present.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' # TODO
#'
#' @export
projection <- function(x) {
  if (!is_track_df(x))
    stop("This is not a track_df object.")

  attr(x, "proj")
}


#' @rdname projection
#'
#' @export
`projection<-` <- function(x, value = "+proj=longlat") {
  if (!is_track_df(x))
    stop("This is not a track_df object.")

  if (is.character(value)) {
    value <- sp::CRS(value)
  } else if (class(value) != "CRS") {
    stop("value must be an object of class character or CRS")
  }

  if (!is.null(methods::slotNames(attr(x, "proj")))) {
    tmp <- x[, c("x", "y")]

    if (sum(stats::complete.cases(tmp)) < nrow(tmp))
      stop("The projection cannot be modified when missing coordinates are present.")

    sp::coordinates(tmp) <- c("x", "y")
    sp::proj4string(tmp) <- attr(x, "proj")
    tmp <- sp::spTransform(tmp, value)

    x[, c("x", "y")] <- tibble::as_tibble(tmp)
  }

  attr(x, "proj") <- value
  x
}

#' @rdname projection
#'
#' @export
project <- function(x, value) {
  if (!is_track_df(x))
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
#' # TODO
#'
#' @export
is_geo <- function(x) {
  if (!is_track_df(x)) {
    stop("This is not a track_df object.")
  } else {
    !is.na(attr(x, "proj")@projargs)
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
#' # TODO
#'
#' @export
n_dims <- function(x) {
  if (!is_track_df(x)) {
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
#' # TODO
#'
#' @export
n_tracks <- function(x) {
  if (!is_track_df(x)) {
    stop("This is not a track_df object.")
  } else {
    length(unique(x$id))
  }
}
