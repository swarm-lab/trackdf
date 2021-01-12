.track <- function(x, y, z, t, id, proj, origin, period, tz, format) {
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
        origin <- tryCatch(
          if (missing(format))
            lubridate::as_datetime(origin, tz = tz)
          else
            lubridate::as_datetime(origin, tz = tz, format = format),
          error = function(e) {
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

      t <- if (missing(format))
        lubridate::as_datetime(t * period, origin = origin, tz = tz)
      else
        lubridate::as_datetime(t * period, origin = origin, tz = tz, format = format)
    } else {
      t <- if (missing(format))
        lubridate::as_datetime(t, tz = tz)
      else
        lubridate::as_datetime(t, tz = tz, format = format)
    }
  } else {
    t <- if (missing(format))
      lubridate::as_datetime(t, tz = tz)
    else
      lubridate::as_datetime(t, tz = tz, format = format)
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
      out <- list(id = as.character(id), t = t, x = x, y = y, z = z, proj = proj)
    }
  } else {
    out <- list(id = as.character(id), t = t, x = x, y = y, proj = proj)
  }

  out
}

#' @name track_
#' @aliases track_tbl
#' @aliases track_dt
#'
#'
#' @title Build a Track Table
#'
#' @description \code{track} constructs track tables based on \code{\link{data.frame}}
#'  (the default), \code{\link[tibble]{tibble}}, or \code{\link[data.table]{data.table}}.
#'  \code{track} is a convenience function that executes \code{track_df},
#'  \code{track_tbl}, or \code{track_dt} based on the value of the `table`
#'  parameter. Track tables can be used like the data structure they are build
#'  upon but with a notable difference: they have an extra attribute to store
#'  the projection of the track coordinates, and modifying the projection will
#'  automatically trigger the appropriate conversion of the coordinates.
#'
#' @param x,y,z Numeric vectors representing the coordinates of the locations.
#'  \code{x} and \code{y} are required. \code{z} can be ignored if the
#'  trajectories are 2-dimensional. Note: if the vectors are not of the same
#'  length, the shorter ones will be recycled to match the length of the longer
#'  one.
#'
#' @param t A numeric vector or a vector of objects that can be coerced to
#'  date-time objects by \code{\link[lubridate]{as_datetime}} representing the
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
#'  \code{"+proj=longlat"} is suitable for the output of most GPS trackers.
#'
#' @param origin Something that can be coerced to a date-time object by
#'  \code{\link[lubridate]{as_datetime}} representing the start date and time of
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
#' @param format A character string indicating the formatting of `t`. See
#'  \code{\link{strptime}} for how to specify this parameter.
#'
#' When supplied parsing is performed by strptime(). For this reason consider using specialized parsing functions in lubridate.
#'
#' @param table A string indicating the class of the table on which the track
#'  table should be built. It can be a \code{\link{data.frame}} ("df", the default),
#'  a \code{\link[tibble]{tibble}} ("tbl"), or a \code{\link[data.table]{data.table}}
#'  ("dt").
#'
#' @return A track table, which is a colloquial term for an object of class
#'  \code{track}.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' data(short_tracks)
#'
#' t_df <- track(x = short_tracks$x, y = short_tracks$y, t = short_tracks$t,
#'   id = short_tracks$id, proj = "+proj=longlat", tz = "Africa/Windhoek", table = "df")
#'
#' t_df <- track_df(x = short_tracks$x, y = short_tracks$y, t = short_tracks$t,
#'   id = short_tracks$id, proj = "+proj=longlat", tz = "Africa/Windhoek")
#'
#' t_tbl <- track_tbl(x = short_tracks$x, y = short_tracks$y, t = short_tracks$t,
#' id = short_tracks$id, proj = "+proj=longlat", tz = "Africa/Windhoek")
#'
#' t_dt <- track_dt(x = short_tracks$x, y = short_tracks$y, t = short_tracks$t,
#'   id = short_tracks$id, proj = "+proj=longlat", tz = "Africa/Windhoek")
#'
#' @rdname track_
#'
#' @export
track <- function(x, y, z, t, id, ..., proj, origin, period, tz, format, table = "df") {
  switch(table,
         "df" = track_df(x = x, y = y, z = z, t = t, id = id, ..., proj = proj,
                         origin = origin, period = period, tz = tz, format = format),
         "tbl" = track_tbl(x = x, y = y, z = z, t = t, id = id, ..., proj = proj,
                           origin = origin, period = period, tz = tz, format = format),
         "dt" = track_dt(x = x, y = y, z = z, t = t, id = id, ..., proj = proj,
                         origin = origin, period = period, tz = tz, format = format),
         stop("Unknown table type.")
  )
}

#' @rdname track_
#'
#' @export
track_df <- function(x, y, z, t, id, ..., proj, origin, period, tz, format) {
  l <- .track(x, y, z, t, id, proj, origin, period, tz, format)
  out <- as.data.frame(l[names(l) != "proj"], stringsAsFactors = FALSE)

  args <- list(...)
  if (length(args) > 0) {
    var <- names(args)
    for (i in 1:length(var)) {
      out[[var[[i]]]] <- args[[var[[i]]]]
    }
  }

  attr(out, "proj") <- l$proj
  class(out) <- c("track", class(out))
  out
}

#' @rdname track_
#'
#' @export
track_tbl <- function(x, y, z, t, id, ..., proj, origin, period, tz, format) {
  l <- .track(x, y, z, t, id, proj, origin, period, tz, format)
  out <- tibble::as_tibble(l[names(l) != "proj"])

  args <- list(...)
  if (length(args) > 0) {
    var <- names(args)
    for (i in 1:length(var)) {
      out[[var[[i]]]] <- args[[var[[i]]]]
    }
  }

  attr(out, "proj") <- l$proj
  class(out) <- c("track", class(out))
  out
}

#' @rdname track_
#'
#' @export
track_dt <- function(x, y, z, t, id, ..., proj, origin, period, tz, format) {
  l <- .track(x, y, z, t, id, proj, origin, period, tz, format)
  out <- data.table::as.data.table(l[names(l) != "proj"])

  args <- list(...)
  if (length(args) > 0) {
    var <- names(args)
    for (i in 1:length(var)) {
      out[[var[[i]]]] <- args[[var[[i]]]]
    }
  }

  attr(out, "proj") <- l$proj
  class(out) <- c("track", class(out))
  out
}


#' @title Check Validity of Track Table
#'
#' @description Test whether a variable contains a track table as produced
#'  by \code{\link{track_df}}, \code{\link{track_tbl}}, or \code{\link{track_dt}}.
#'
#' @param x An object to test.
#'
#' @return A logical indicating whether the variable contains a track table
#'  (\code{TRUE}) or not (\code{FALSE}).
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{track_df}}, \code{\link{track_tbl}}, \code{\link{track_dt}}
#'
#' @examples
#' data(short_tracks)
#'
#' is_track(short_tracks)
#'
#' @export
is_track <- function(x) {
  any(class(x) == "track") &
    all(c("id", "t", "x", "y") %in% names(x)) &
    !is.null(attr(x, "proj"))
}


#' @export
print.track <- function(x, ...) {
  if (!is_track(x)) {
    warning("This is a malformed track object. Printing as is.")
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
    cat("Table class: ", ifelse("data.table" %in% class(x), "data table",
                                ifelse("tbl" %in% class(x), "tibble", "data frame")))
    cat("\n")

    if (any(c("tbl", "data.table") %in% class(x))) {
      class(x) <- class(x)[class(x) != "track"]
      print(x, ...)
    } else {
      class(x) <- class(x)[class(x) != "track"]
      print.data.frame(x, ...)
    }

    invisible(x)
  }
}


#' @title Extract or Replace Parts of a Track Table
#'
#' @description Accessing columns, rows, or cells via $, [[, or [ is mostly
#'  similar to regular \code{\link[base:Extract.data.frame]{data frames}}.
#'  However, the behavior is sometimes different for track tables based on
#'  \code{\link[tibble]{tibble}} and \code{\link[data.table]{data.table}}. For
#'  more info, refer to \link[tibble:subsetting]{tibble}'s and
#'  \code{\link[data.table]{data.table}}'s subsetting documentation.
#'
#' @param x A track table.
#'
#' @param ... Other parameters to be passed to the extracting/subsetting
#'  functions of \code{\link{data.frame}}, \code{\link[tibble]{tibble}}, and
#'  \code{\link[data.table]{data.table}}.
#'
#' @param value A suitable replacement value: it will be repeated a whole number
#'  of times if necessary and it may be coerced: see the `Coercion` section in
#'  \code{\link{data.frame}}. If `NULL`, deletes the column if a single column
#'  is selected.
#'
#' @return A subset of the track table is \code{[} is called, or a modified version
#'  of the track table if \code{[<-} is called.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{track_df}}, \code{\link{track_tbl}}, \code{\link{track_dt}}
#'
#' @examples
#' data(short_tracks)
#'
#' short_tracks[1]
#' short_tracks[1, ]
#' short_tracks[1, 1]
#' short_tracks$id[short_tracks$id == "1"] <- "0"
#' short_tracks[short_tracks[, 1] == "0", 1] <- "1"
#'
#' @export
`[.track` <- function(x, ...) {
  out <- NextMethod()

  if (is_track(out)) {
    out
  } else {
    class(out) <- class(out)[class(out) != "track"]
    attr(out, "proj") <- NULL
    out
  }
}

#' @rdname sub-.track
#'
#' @export
`[<-.track` <- function(x, ..., value) {
  out <- NextMethod()

  if (is_track(out)) {
    out
  } else {
    class(out) <- class(out)[class(out) != "track"]
    attr(out, "proj") <- NULL
    out
  }
}


#' @title Bind Multiple Track Tables by Row
#'
#' @description {rbind_track} uses \code{\link[data.table:rbindlist]{data.table::rbindlist}}
#'  to combine track tables by rows, but makes sure that you cannot bind together
#'  two tables with different projections, that the projection attribute is
#'  inherited by the resulting track table, and that track tables based on
#'  different table classes are coerced to the same table class.
#'
#' @param ... Track tables to combine. Each argument can either be a track table
#'  or a list of track tables. The track tables must have the same projection.
#'
#' @return A track table.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' data(short_tracks)
#'
#' rbind_track(short_tracks, short_tracks)
#' rbind_track(list(short_tracks, short_tracks))
#'
#' @export
rbind_track <- function(...) {
  df_list <- list(...)

  if (length(df_list) == 1 & inherits(df_list[[1]], "list"))
    df_list <- unlist(df_list, recursive = FALSE)

  if (!all(sapply(df_list, inherits, what = "track")))
    stop("All elements should be track tables.")

  table_cl <- sapply(df_list, inherits, what = "tbl") +
    2 * sapply(df_list, inherits, what = "data.table")

  if (!all(table_cl == table_cl[1]))
    warning("The elements are of different table classes. They were converted
            to the table class of the first element.")

  proj <- lapply(df_list, attr, "proj")

  if (length(unique(proj)) > 1)
    stop("All elements should have the same projection.")

  df_list <- lapply(df_list, function(x) {
    class(x) <- class(x)[class(x) != "track"]
    x
  })

  out <- data.table::rbindlist(df_list)

  if (table_cl[1] == 0) {
    out <- as.data.frame(out)
  } else if (table_cl[1] == 1) {
    out <- dplyr::as_tibble(out)
  }

  class(out) <- c("track", class(out))
  attr(out, "proj") <- proj[[1]]
  out
}
