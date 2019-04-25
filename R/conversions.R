#' @title Convert a Track Table to Other Formats
#'
#' @description The following methods will convert track tables to other
#'  common formats used for processing tracking and spatial data.
#'
#' @name conversions
#'
#' @return The coordinates converted in the chosen format.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' # TODO
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
