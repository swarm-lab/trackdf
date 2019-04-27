#' @title Build a Track Table
#'
#' @description \code{track_df} constructs a track table. It is used like
#'  \code{\link[tibble]{tibble}}, but with a notable difference: it has an extra
#'  attributes to store the projection of the track coordinates, and modifying
#'  the projection will automatically trigger the appropriate conversion of the
#'  coordinates.
#'
#' @param x,y,z Numaric vectors representing the coordinates of the locations.
#'  \code{x} and \code{y} are required. \code{z} can be ignored if the
#'  trajectories are 2-dimensional. Note: if the vectors are not of the same
#'  length, the shorter ones will be recycled to match the length of the longer
#'  one.
#'
#' @param t A numeric vector or a vector of objects that can be coerced to
#'  date-time objects by \code{link[lubridate]{as_datetime}} representing the
#'  times (or frames) at which each location was recorded. If numeric, the
#'  origin and period of the time points can be set using \code{origin} and
#'  \code{period} below.
#'
#' @param id A vector that can be coerced to a character vector by
#'  \code{\link{as.character}} representing the identity of the animal to which
#'  each location belong.
#'
#' @param ... A set of name-value pairs. Arguments are evaluated sequentially,
#'  so you can refer to previously created elements. These arguments are
#'  processed with \code{\link[rlang:quotation]{rlang::quos()}} and support
#'  unquote via \code{!!} and unquote-splice via \code{!!!}. Use \code{:=} to
#'  create columns that start with a dot.
#'
#' @param proj A character string or a \code{\link[sp:CRS]{sp::CRS}} object
#'  representing the projection of the coordinates. Leave empty if the
#'  coordinates are not projected (e.g., output of video tracking).
#'  \code{"+proj=longlat"} is suitable for the outputs of most GPS trackers.
#'
#' @param origin Something that can be coerced to a date-time object by
#'  \code{link[lubridate]{as_datetime}} representing the start date and time of
#'  the observations when \code{t} is a numeric vector.
#'
#' @param period A character vector in a shorthand format (e.g. "1 second") or
#'  ISO 8601 specification. This is used when \code{t} is a numeric vector to
#'  represent time unit of the observations. All unambiguous name units and
#'  abbreviations are supported, "m" stands for months, "M" for minutes unless
#'  ISO 8601 "P" modifier is present (see examples). Fractional units are
#'  supported but the fractional part is always converted to seconds. See
#'  \code{\link[lubridate]{period}} for more details.
#'
#' @param tz A time zone name. See \code{\link{OlsonNames}}.
#'
#' @return A track table, which is a colloquial term for an object of class
#'  \code{track_df}. A \code{track_df} object is also a tibble and a data frame,
#'  i.e. it has class \code{\link[tibble]{tbl_df}} and \code{\link{data.frame}}.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' # TODO
#'
#' @export
track_df <- function(x, y, z, t, id, ..., proj, origin, period, tz) {
  if (!is.numeric(x))
    stop("x must be an object of class numeric.")

  if (!is.numeric(y))
    stop("y must be an object of class numeric.")

  if (missing(tz)) {
    message("No timezone provided. Defaulting to UTC.")
    tz <- "UTC"
  }

  if (!lubridate::is.POSIXct(t)) {
    if (is.numeric(t)) {
      if (!missing(origin)) {
        origin <- tryCatch(lubridate::as_datetime(origin, tz = tz), error = function(e) {
          e$call <- "track_df(..., origin)"
          e$message <- "Invalid origin format."
          stop(e)
        })
      } else {
        message("No origin provided. Defaulting to Sys.time().")
        origin <- Sys.time()
      }

      if (!missing(period)) {
        period <- tryCatch(lubridate::period(period), error = function(e) {
          e$call <- "track_df(..., period)"
          e$message <- "Invalid period specification."
          stop(e)
        })
      } else {
        message("No period provided. Defaulting to 1 second.")
        period <- lubridate::period("1 second")
      }

      t <- lubridate::as_datetime(t * period, origin = origin, tz = tz)
    } else {
      t <- lubridate::as_datetime(t, tz = tz)
    }
  } else {
    t <- lubridate::as_datetime(t, tz = tz)
  }

  if (!missing(id)) {
    id <- tryCatch(as.character(id), error = function(e) {
      e$call <- "track_df(..., id)"
      e$message <- "id cannot be converted to a character string."
      stop(e)
    })
  } else {
    message("No id provided. Defaulting to 0.")
    id <- "0"
  }

  if (!missing(proj)) {
    if (is.character(proj)) {
      proj <- sp::CRS(proj)
    } else if (class(proj) != "CRS") {
      stop("proj must be an object of class character or CRS")
    }
  } else {
    proj <- sp::CRS()
  }

  if (!missing(z)) {
    if (!is.numeric(z)) {
      stop("z must be an object of class numeric.")
    } else {
      out <- tibble::tibble(id = id, t = t, x = x, y = y, z = z)
    }
  } else {
    out <- tibble::tibble(id = id, t = t, x = x, y = y)
  }

  args <- list(...)
  if (length(args) > 0) {
    var <- names(args)
    for (i in length(var)) {
      out[[var[[i]]]] <- args[[var[[i]]]]
    }
  }

  attr(out, "proj") <- proj
  class(out) <- c("track_df", class(out))
  out
}


#' @title Check Validity of Track Table
#'
#' @description Test whether a variable contains a track table as produced
#'  by \code{\link{track_df}}.
#'
#' @param x An object to test.
#'
#' @return A logical indicating whether the variable contains a track table
#'  (\code{TRUE}) or not (\code{FALSE}).
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{track_df}}
#'
#' @examples
#' # TODO
#'
#' @export
is_track_df <- function(x) {
  any(class(x) == "track_df") &
    all(tibble::has_name(x, c("id", "t", "x", "y"))) &
    !is.null(attr(x, "proj"))
}


#' @export
print.track_df <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  if (!is_track_df(x)) {
    warning("This is a malformed track_df object. Printing as is.")
    print.AsIs(x)
  } else {
    n_obs <- nrow(x)
    n_tracks <- length(unique(x$id))
    geo <- is_geo(x)
    n_dims <- paste0(sum(c("x", "y", "z") %in% names(x)), "D")

    cat("Track table [", n_obs, " observations]\n", sep = "")
    cat("Number of tracks: ", n_tracks, "\n")
    cat("Dimensions: ", n_dims, "\n")
    cat("Geographic: ", geo, "\n")
    if (geo)
      cat("Projection: ", attr(x, "proj")@projargs, "\n")
    cat("\n")

    df_print <- paste0(format(x, ..., n = n, width = width, n_extra = n_extra), "\n")
    cat(df_print, sep = "")
    invisible(x)
  }
}


#' @title Extract or Replace Parts of a Track Table
#'
#' @method [ track_df
#'
#' @description Accessing columns, rows, or cells via $, [[, or [ is mostly
#'  similar to regular \code{\link[base:Extract.data.frame]{data frames}}.
#'  However, the behavior is different for track tables and data frames in some
#'  cases, because track tables inherit from \code{\link[tibble]{tibble}}:
#'  \itemize{
#'   \item \code{[} always returns a track table by default, even if only one
#'    column is accessed.
#'   \item Partial matching of column names with $ and [[ is not supported, a
#'    warning is given and NULL is returned.
#'  }
#'  For more info, refer to \link[tibble:subsetting]{tibble's subsetting
#'  documentation}
#'
#' @param x A track table.
#'
#' @param i,j Row and column indices. If \code{j} is omitted, \code{i} is used as
#'  column index.
#'
#' @param drop Coerce to a vector if fetching one column via \code{track_df[, j]}.
#'  Default \code{FALSE}, ignored when accessing a column via \code{track_df[j]}.
#'
#' @param value A value to be assigned to x.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{track_df}}
#'
#' @examples
#' # TODO
#'
#' @export
`[.track_df` <- function(x, i, j, drop = TRUE) {
  class(x) <- class(x)[class(x) != "track_df"]
  x <- x[i, j, drop = drop]

  if (all(tibble::has_name(x, c("id", "t", "x", "y"))))
    class(x) <- c("track_df", class(x))

  x
}

#' @rdname sub-.track_df
#'
#' @method [<- track_df
#'
#' @export
`[<-.track_df` <- function(x, i, j, value) {
  class(x) <- class(x)[class(x) != "track_df"]
  x[i, j] <- value
  class(x) <- c("track_df", class(x))
  x
}