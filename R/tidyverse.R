#' @title Maintain Class After Modification
#'
#' @description Copy class and attributes from the original version of an object
#'  to a modified version.
#'
#' @param x The original object, which has a class/attributes to copy
#'
#' @param result The modified object, which is/might be missing the
#'  class/attributes.
#'
#' @return \code{result}, now with class/attributes restored.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
.reclass <- function(x, result) {
  UseMethod('.reclass')
}

.reclass.default <- function(x, result) {
  if (!inherits(result, "grouped_df")) {
    if (attr(x, "type") == "data.table") {
      if (!inherits(result, "data.table"))
        result <- data.table::as.data.table(result)
    } else if (attr(x, "type") == "data.frame") {
      if (inherits(result, "tbl"))
        result <- as.data.frame(result)
    }
  }

  class(result) <- unique(c(class(x)[[1]], class(result)))
  attr(result, class(x)[[1]]) <- attr(x, class(x)[[1]])
  attr(result, "proj") <- attr(x, "proj")
  attr(result, "type") <- attr(x, "type")

  if (is_track(result)) {
    result
  } else {
    ix <- which(class(result) == "track")
    if (length(ix) > 0)
      class(result) <- class(result)[-ix]

    attr(result, "proj") <- NULL
    result
  }
}

#' @title Dplyr Methods For Track Tables
#'
#' @name dplyr_track
#'
#' @description \code{\link[dplyr]{dplyr}} methods for track tables objects.
#'
#' @param .data A track table.
#'
#' @param ... Additional arguments to be passed to the corresponding
#'  \code{\link[dplyr]{dplyr}} method.
#'
#' @seealso \code{\link[dplyr]{dplyr}}

#' @importFrom dplyr select
#' @rdname dplyr_track
#'
#' @export
select.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @importFrom dplyr rename
#' @rdname dplyr_track
#'
#' @export
rename.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track
#'
#' @export
filter.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @title Subset rows using column values
#'
#' @description This is a re-export of the \code{\link[dplyr]{filter}} function
#'  in the \code{\link[dplyr]{dplyr}} package. See the help
#'
#' @param .data See \code{\link[dplyr]{filter}} for an explanation.
#' @param ... Other args
#' @name filter
#' @importFrom dplyr filter
#' @export filter
NULL

#' @importFrom dplyr arrange
#' @rdname dplyr_track
#'
#' @export
arrange.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @importFrom dplyr mutate
#' @rdname dplyr_track
#'
#' @export
mutate.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @importFrom dplyr transmute
#' @rdname dplyr_track
#'
#' @export
transmute.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @importFrom dplyr summarise
#' @rdname dplyr_track
#'
#' @export
summarise.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @importFrom dplyr summarize
#' @rdname dplyr_track
#'
#' @export
summarize.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @importFrom dplyr group_by
#' @rdname dplyr_track
#'
#' @export
group_by.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @importFrom dplyr ungroup
#' @rdname dplyr_track
#'
#' @export
ungroup.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @importFrom dplyr sample_n
#' @rdname dplyr_track
#'
#' @export
sample_n.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @importFrom dplyr sample_frac
#' @rdname dplyr_track
#'
#' @export
sample_frac.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @importFrom dplyr do
#' @rdname dplyr_track
#'
#' @export
do.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @importFrom dplyr slice
#' @rdname dplyr_track
#'
#' @export
slice.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @importFrom dplyr semi_join
#' @rdname dplyr_track
#'
#' @export
semi_join.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @importFrom dplyr anti_join
#' @rdname dplyr_track
#'
#' @export
anti_join.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @importFrom dplyr inner_join
#' @rdname dplyr_track
#'
#' @export
inner_join.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @importFrom dplyr left_join
#' @rdname dplyr_track
#'
#' @export
left_join.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @importFrom dplyr right_join
#' @rdname dplyr_track
#'
#' @export
right_join.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @importFrom dplyr full_join
#' @rdname dplyr_track
#'
#' @export
full_join.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @importFrom dplyr nest_join
#' @rdname dplyr_track
#'
#' @export
nest_join.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @importFrom dplyr distinct
#' @rdname dplyr_track
#'
#' @export
distinct.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @importFrom dplyr rowwise
#' @rdname dplyr_track
#'
#' @export
rowwise.track <- function(.data, ...) .reclass(.data, NextMethod())
