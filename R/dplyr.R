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
