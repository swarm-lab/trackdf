#' @title Maintain Class After Modification
#'
#' @description Copy class and attributes from the original version of an object
#'  to a modified version.
#'
#' @param x The original object, which has a class/attributes to copy
#'
#' @param result The modified object, which is / might be missing the class/attributes.
#'
#' @return \code{result}, now with class/attributes restored.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
.reclass <- function(x, result) {
  UseMethod('.reclass')
}

.reclass.default <- function(x, result) {
  if (inherits(x, "data.table") & !inherits(result, "data.table")) {
    result <- data.table::as.data.table(result)
  }

  class(result) <- unique(c(class(x)[[1]], class(result)))
  attr(result, class(x)[[1]]) <- attr(x, class(x)[[1]])
  attr(result, "proj") <- attr(x, "proj")

  if (is_track(result)) {
    result
  } else {
    class(result) <- class(result)[2:length(class(result))]
    attr(result, "proj") <- NULL
    result
  }
}


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
#' data(tracks)
#'
#' projection(tracks)
#' tracks_projected <- project(tracks, "+proj=somerc")
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
#' data(tracks)
#'
#' is_geo(tracks)
#'
#' @export
is_geo <- function(x) {
  if (!is_track(x)) {
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
#' data(tracks)
#'
#' n_dims(tracks)
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
#' data(tracks)
#'
#' n_tracks(tracks)
#'
#' @export
n_tracks <- function(x) {
  if (!is_track(x)) {
    stop("This is not a track_df object.")
  } else {
    length(unique(x$id))
  }
}
