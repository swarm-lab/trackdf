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
#'
#' @examples
#' # TODO
.reclass <- function(x, result) {
  UseMethod('.reclass')
}

.reclass.default <- function(x, result) {
  class(result) <- unique(c(class(x)[[1]], class(result)))
  attr(result, class(x)[[1]]) <- attr(x, class(x)[[1]])
  attr(result, "proj") <- attr(x, "proj")

  if (is_track_df(result)) {
    result
  } else {
    class(result) <- class(result)[2:length(class(result))]
    attr(result, "proj") <- NULL
    result
  }
}


#' @title Dplyr Methods For Track Tables
#'
#' @name dplyr_track_df
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

#' @rdname dplyr_track_df
#'
#' @export
select.track_df <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track_df
#'
#' @export
rename.track_df <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track_df
#'
#' @export
filter.track_df <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track_df
#'
#' @export
arrange.track_df <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track_df
#'
#' @export
mutate.track_df <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track_df
#'
#' @export
transmute.track_df <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track_df
#'
#' @export
summarise.track_df <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track_df
#'
#' @export
summarize.track_df <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track_df
#'
#' @export
group_by.track_df <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track_df
#'
#' @export
ungroup.track_df <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track_df
#'
#' @export
sample_n.track_df <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track_df
#'
#' @export
sample_frac.track_df <- function(.data, ...) .reclass(.data, NextMethod())

#' @rdname dplyr_track_df
#'
#' @export
do.track_df <- function(.data, ...) .reclass(.data, NextMethod())


#' @title Bind Multiple Track Tables by Row
#'
#' @description \code{bind_track_df} and \code{rbind.track_df} are modified
#'  versions of \code{\link[dplyr:bind_rows]{dplyr::bind_rows}} and
#'  \code{\link[base:rbind]{base::rbind}} respectively, that work with track
#'  tables. In particular, they make sure that you cannot bind together two
#'  tables with different projections and, in the case of \code{rbind.track_df},
#'  that the projection attribute is inherited by the resulting track table
#'  (\code{\link[dplyr:bind_rows]{dplyr::bind_rows}} strips it otherwise).
#'
#' @param ... Track tables to combine.
#'
#'  Each argument can either be a track table, a list that could be track table,
#'  or a list of track table.
#'  Columns are matched by name, and any missing columns will be filled with NA.
#'
#' @param .id	Track table identifier.
#'
#'  When .id is supplied, a new column of identifiers is created to link each
#'  row to its original data frame. The labels are taken from the named
#'  arguments to bind_rows(). When a list of data frames is supplied, the labels
#'  are taken from the names of the list. If no names are found a numeric
#'  sequence is used instead.
#'
#' @note \code{\link[dplyr:bind_rows]{dplyr::bind_rows}} cannot be converted to
#'  a S3 generic and therefore cannot implement convenient dispatching based on
#'  the class of the tables it is provided with. As a consequence, it is not
#'  possible at the moment to provide specific implementations of \code{bind_row}
#'  for track tables.
#'
#' @return A track table.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' # TODO
#'
#' @export
bind_track_df <- function(..., .id = NULL) {
  df <- list(...)
  proj <- unique(lapply(df, attr, "proj"))
  cl <- unique(lapply(df, class))

  if (length(proj) > 1 | length(cl) > 1)
    stop("All elements should be track tables with the same projection.")

  out <- dplyr::bind_rows(..., .id = .id)
  attr(out, "proj") <- proj[[1]]
  out
}

#' @rdname bind_track_df
#'
#' @export
rbind.track_df <- function(...) {
  df <- list(...)
  proj <- unique(lapply(df, attr, "proj"))
  cl <- unique(lapply(df, class))

  if (length(proj) > 1 | length(cl) > 1)
    stop("All elements should be track tables with the same projection.")

  rbind.data.frame(...)
}
