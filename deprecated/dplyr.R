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
#' @seealso \code{\link[dplyr]{select}}, \code{\link[dplyr]{rename}},
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{arrange}},
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{transmute}},
#'  \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{summarize}},
#'  \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{ungroup}},
#'  \code{\link[dplyr]{sample_n}}, \code{\link[dplyr]{sample_frac}},
#'  \code{\link[dplyr]{do}}

#' @rdname dplyr_track
#'
#' @export
select.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track
#'
#' @export
rename.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track
#'
#' @export
filter.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track
#'
#' @export
arrange.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track
#'
#' @export
mutate.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track
#'
#' @export
transmute.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track
#'
#' @export
summarise.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track
#'
#' @export
summarize.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track
#'
#' @export
group_by.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track
#'
#' @export
ungroup.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track
#'
#' @export
sample_n.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track
#'
#' @export
sample_frac.track <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track
#'
#' @export
do.track <- function(.data, ...) .reclass(.data, NextMethod())

#if_else
