#' @title Convert a Track Table to Other Formats
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
#'   \item{\code{\link{track_df}} if `as_track_df` is used.}
#'   \item{\code{\link[moveVis:df2move]{moveVis::df2move}} if `as_move` is used.}
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
  if (!is_track_df(x))
    stop("This is not a track_df object.")

  if (sum(c("x", "y", "z") %in% names(x)) > 2)
    warning("move objects are 2D only. The 3rd dimension will be stripped away.")

  moveVis::df2move(x, proj = projection(x), x = "x", y = "y", time = "t",
                   track_id = "id", ...)
}



