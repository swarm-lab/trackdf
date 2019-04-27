#' @title Convert a Track Table to/from Other Formats
#'
#' @description The following methods will convert track tables to other
#'  common formats used for processing tracking and spatial data.
#'
#' @name conversions
#'
#' @param x A track table to convert.
#'
#' @param id A character vector representing the identity of the animal to which
#'  each location belong.
#'
#' @param ... Other parameters to be passed to:
#' \itemize{
#'   \item{\code{\link{track_df}} if \code{as_track_df} is used.}
#'   \item{\code{\link[moveVis:df2move]{moveVis::df2move}} if \code{as_move} is
#'     used.}
#'   \item{\code{\link[sp:SpatialPointsDataFrame]{sp::SpatialPointsDataFrame}}
#'     if \code{as_sp} is used.}
#'   \item{\code{\link[adehabitatLT:as.ltraj]{adehabitatLT::as.ltraj}} if
#'     \code{as_ltraj} is used.}
#'   \item{\code{\link[ctmm:as.telemetry]{ctmm::as.telemetry}} if
#'     \code{as_telemetry} is used.}
#' }
#'
#' @return The coordinates converted in the chosen format.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' # TODO
#'
#' @rdname conversions
#'
#' @export
as_track_df <- function(x, ...) {
  UseMethod("as_track_df", x)
}

#' @rdname conversions
#'
#' @export
as_track_df.MoveStack <- function(x, ...) {
  l <- list()

  for (i in 1:move::n.indiv(x)) {
    l[[i]] <- as_track_df(x[[i]], id = i)
  }

  bind_track_df(l)
}

#' @rdname conversions
#'
#' @export
as_track_df.Move <- function(x, id, ...) {
  track_df(x = x$x, y = x$y, t = x$time, id = id, ..., proj = x@proj4string,
           tz = lubridate::tz(x$time))
}


#' @rdname conversions
#'
#' @export
as_move <- function(x, ...) {
  UseMethod("as_move", x)
}

#' @rdname conversions
#'
#' @export
as_move.track_df <- function(x, ...) {
  if (n_dims(x) > 2)
    warning("move objects are 2D only. The 3rd dimension will be stripped away.")

  moveVis::df2move(x, proj = projection(x), x = "x", y = "y", time = "t",
                   track_id = "id", ...)
}


#' @rdname conversions
#'
#' @export
as_sp <- function(x, ...) {
  UseMethod("as_sp", x)
}

#' @rdname conversions
#'
#' @export
as_sp.track_df <- function(x, ...) {
  if (n_dims(x) > 2)
    warning("sp objects are 2D only. The 3rd dimension will be stripped away.")

  sp::SpatialPointsDataFrame(
    coords = x[, c("x", "y")],
    data = x[, c("id", "t")],
    proj4string = attr(x, "proj", ...)
  )
}


#' @rdname conversions
#'
#' @export
as_ltraj <- function(x, ...) {
  UseMethod("as_ltraj", x)
}

#' @rdname conversions
#'
#' @export
as_ltraj.track_df <- function(x, ...) {
  if (n_dims(x) > 2)
    warning("ltraj objects are 2D only. The 3rd dimension will be stripped away.")

  adehabitatLT::as.ltraj(xy = x[, c("x", "y")], date = x$t, id = x$id, typeII = TRUE, ...)
}


#' @rdname conversions
#'
#' @export
as_telemetry <- function(x, ...) {
  UseMethod("as_telemetry", x)
}

#' @rdname conversions
#'
#' @export
as_telemetry.track_df <- function(x, ...) {
  if (!is_geo(x))
    stop("Only geographic track_df objects can be converted to telemetry objects.")

  if (n_dims(x) > 2)
    warning("telemetry objects are 2D only. The 3rd dimension will be stripped away.")

  projection(x) <- "+init=epsg:4326"
  ctmm::as.telemetry(data.frame(lon = x$x, lat = x$y, timestamp = x$t, id = x$id))
}


#' @rdname conversions
#'
#' @export
as_moveHMM  <- function(x, ...) {
  UseMethod("as_moveHMM", x)
}

#' @rdname conversions
#'
#' @export
as_moveHMM.track_df <- function(x, ...) {
  if (!is_geo(x))
    stop("Only geographic track_df objects can be converted to telemetry objects.")

  if (n_dims(x) > 2)
    warning("telemetry objects are 2D only. The 3rd dimension will be stripped away.")

  projection(x) <- "+proj=longlat"
  colnames(x)[colnames(x) == "id"] <- "ID"
  moveHMM::prepData(x, type = "LL")
}
