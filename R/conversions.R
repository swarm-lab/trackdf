#' @importFrom methods as
#'
#' @title Convert a Track Table to/from Other Formats
#'
#' @description The following methods will convert track tables to and from other
#'  common formats used for processing tracking and spatial data.
#'
#' @name conversions
#'
#' @param x An object to convert.
#'
#' @param table A string indicating the class of the table on which the track
#'  table should be built. It can be a \code{\link{data.frame}} ("df", the default),
#'  a \code{\link[tibble]{tibble}} ("tbl"), or a \code{\link[data.table]{data.table}}
#'  ("dt").
#'
#' @param type For converting \code{\link[moveHMM:moveData]{moveHMM::moveData}}
#'  to track table only, a character string indicating the type of coordinates
#'  stored in the \code{\link[moveHMM:moveData]{moveHMM::moveData}} object: "LL"
#'  if longitude/latitude (default), "UTM" if easting/northing.
#'
#' @param ... Other parameters to be passed to:
#' \itemize{
#'   \item{\code{\link{track_df}}, \code{\link{track_tbl}} or \code{\link{track_dt}}
#'     if \code{as_track} is used.}
#'   \item{\code{\link[moveVis:df2move]{moveVis::df2move}} if \code{as_move} is
#'     used.}
#'   \item{\code{\link[sp:SpatialPointsDataFrame]{sp::SpatialPointsDataFrame}}
#'     if \code{as_sp} is used.}
#'   \item{\code{\link[adehabitatLT:as.ltraj]{adehabitatLT::as.ltraj}} if
#'     \code{as_ltraj} is used.}
#'   \item{\code{\link[ctmm:as.telemetry]{ctmm::as.telemetry}} if
#'     \code{as_telemetry} is used.}
#'   \item{\code{\link[moveHMM:prepData]{moveHMM::prepData}} if
#'     \code{as_moveHMM} is used.}
#' }
#'
#' @return The coordinates converted in the chosen format.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{track_df}}, \code{\link{track_tbl}}, \code{\link{track_dt}}
#'
#' @examples
#' data(short_tracks)
#'
#' mv <- as_move(short_tracks)
#' sp <- as_sp(short_tracks)
#' lt <- as_ltraj(short_tracks)
#' tl <- as_telemetry(short_tracks)
#' hhm <- as_moveHMM(short_tracks, type = "LL")
#'
#' as_track(mv)
#' as_track(sp)
#' as_track(lt)
#' as_track(tl)
#' as_track(hhm)
#'
#' @rdname conversions
#'
#' @export
as_track <- function(x, table = "df", ...) {
  UseMethod("as_track", x)
}


### MOVE + MOVESTACK

#' @rdname conversions
#'
#' @export
as_track.MoveStack <- function(x, table = "df", ...) {
  x_df <- as(x, "data.frame")

  if (table == "df") {
    track_df(x = x_df$x, y = x_df$y, t = x_df$time, id = x_df$trackId, ...,
             proj = x@proj4string, tz = lubridate::tz(x_df$time))
  } else if (table == "tbl") {
    track_tbl(x = x_df$x, y = x_df$y, t = x_df$time, id = x_df$trackId, ...,
              proj = x@proj4string, tz = lubridate::tz(x_df$time))
  } else if (table == "dt") {
    track_dt(x = x_df$x, y = x_df$y, t = x_df$time, id = x_df$trackId, ...,
             proj = x@proj4string, tz = lubridate::tz(x_df$time))
  } else {
    stop("Invalid table type.")
  }
}

#' @rdname conversions
#'
#' @export
as_track.Move <- function(x, table = "df", ...) {
  x_df <- as(x, "data.frame")

  if (table == "df") {
    track_df(x = x_df$x, y = x_df$y, t = x_df$time, ...,
             proj = x@proj4string, tz = lubridate::tz(x_df$time))
  } else if (table == "tbl") {
    track_tbl(x = x_df$x, y = x_df$y, t = x_df$time, ...,
              proj = x@proj4string, tz = lubridate::tz(x_df$time))
  } else if (table == "dt") {
    track_dt(x = x_df$x, y = x_df$y, t = x_df$time, ...,
             proj = x@proj4string, tz = lubridate::tz(x_df$time))
  } else {
    stop("Invalid table type.")
  }
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
as_move.track <- function(x, ...) {
  if (n_dims(x) > 2)
    warning("move objects are 2D only. The 3rd dimension will be stripped away.")

  moveVis::df2move(x, proj = projection(x), x = "x", y = "y", time = "t",
                   track_id = "id", ...)
}


### SPATIALPOINTSDATAFRAME

#' @rdname conversions
#'
#' @export
as_track.SpatialPointsDataFrame <- function(x, table = "df", ...) {
  x_df <- as(x, "data.frame")

  if (table == "df") {
    track_df(x = x_df$x, y = x_df$y, t = x_df$t, id = x_df$id, ...,
             proj = x@proj4string, tz = lubridate::tz(x_df$t))
  } else if (table == "tbl") {
    track_tbl(x = x_df$x, y = x_df$y, t = x_df$t, id = x_df$id, ...,
              proj = x@proj4string, tz = lubridate::tz(x_df$t))
  } else if (table == "dt") {
    track_dt(x = x_df$x, y = x_df$y, t = x_df$t, id = x_df$id, ...,
             proj = x@proj4string, tz = lubridate::tz(x_df$t))
  } else {
    stop("Invalid table type.")
  }
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
as_sp.track <- function(x, ...) {
  if (n_dims(x) > 2)
    warning("sp objects are 2D only. The 3rd dimension will be stripped away.")

  sp::SpatialPointsDataFrame(coords = x[, c("x", "y")], data = x[, c("id", "t")],
                             proj4string = attr(x, "proj"), ...)
}


### LTRAJ

#' @rdname conversions
#'
#' @export
as_track.ltraj <- function(x, table = "df", ...) {
  names(x) <- adehabitatLT::burst(x)
  x_df <- data.table::rbindlist(x, idcol = "id")

  if (table == "df") {
    track_df(x = x_df$x, y = x_df$y, t = x_df$date, id = x_df$id, ...,
             proj = attr(x, "proj4string"), tz = lubridate::tz(x_df$date))
  } else if (table == "tbl") {
    track_tbl(x = x_df$x, y = x_df$y, t = x_df$date, id = x_df$id, ...,
              proj = attr(x, "proj4string"), tz = lubridate::tz(x_df$date))
  } else if (table == "dt") {
    track_dt(x = x_df$x, y = x_df$y, t = x_df$date, id = x_df$id, ...,
             proj = attr(x, "proj4string"), tz = lubridate::tz(x_df$date))
  } else {
    stop("Invalid table type.")
  }
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
as_ltraj.track <- function(x, ...) {
  if (n_dims(x) > 2)
    warning("ltraj objects are 2D only. The 3rd dimension will be stripped away.")

  adehabitatLT::as.ltraj(xy = x[, c("x", "y")], date = x$t, id = x$id, typeII = TRUE,
                         proj4string = projection(x), ...)
}


### TELEMETRY

#' @rdname conversions
#'
#' @export
as_track.telemetry <- function(x, table = "df", ...) {
  if (table == "df") {
    track_df(x = x$longitude, y = x$latitude, t = x$timestamp, id = x@info$identity,
             ..., proj = x@info$projection, tz = x@info$timezone)
  } else if (table == "tbl") {
    track_tbl(x = x$longitude, y = x$latitude, t = x$timestamp, id = x@info$identity,
              ..., proj = x@info$projection, tz = x@info$timezone)
  } else if (table == "dt") {
    track_dt(x = x$longitude, y = x$latitude, t = x$timestamp, id = x@info$identity,
             ..., proj = x@info$projection, tz = x@info$timezone)
  } else {
    stop("Invalid table type.")
  }
}

#' @rdname conversions
#'
#' @export
as_track.list <- function(x, table = "df", ...) {
  if (!all(sapply(x, inherits, what = "telemetry")))
    stop("No applicable method for 'as_track' applied to an object of class 'list'.")

  rbind_track(lapply(x, as_track, table = table, ...))
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
as_telemetry.track <- function(x, ...) {
  if (!is_geo(x))
    stop("Only geographic track objects can be converted to telemetry objects.")

  if (n_dims(x) > 2)
    warning("telemetry objects are 2D only. The 3rd dimension will be stripped away.")

  proj <- projection(x)@projargs

  if (grepl("longlat", proj, fixed = TRUE) || grepl("latlong", proj, fixed = TRUE)) {
    proj <- NULL
  }

  ctmm::as.telemetry(data.frame(lon = x$x, lat = x$y, timestamp = x$t, id = x$id),
                     timezone = lubridate::tz(x$t), projection = proj, ...)
}


### MOVEHMM

#' @rdname conversions
#'
#' @export
as_track.moveData <- function(x, table = "df", type = c("LL", "UTM"), ...) {
  if (type[1] == "LL") {
    if (table == "df") {
      track_df(x = x$x, y = x$y, t = x$t, id = x$ID, ..., proj = "+proj=longlat",
               tz = lubridate::tz(x$t))
    } else if (table == "tbl") {
      track_tbl(x = x$x, y = x$y, t = x$t, id = x$ID, ..., proj = "+proj=longlat",
                tz = lubridate::tz(x$t))
    } else if (table == "dt") {
      track_dt(x = x$x, y = x$y, t = x$t, id = x$ID, ..., proj = "+proj=longlat",
               tz = lubridate::tz(x$t))
    } else {
      stop("Invalid table type.")
    }
  } else if (type[1] == "UTM") {
    if (table == "df") {
      track_df(x = x$x, y = x$y, t = x$t, id = x$ID, ..., tz = lubridate::tz(x$t))
    } else if (table == "tbl") {
      track_tbl(x = x$x, y = x$y, t = x$t, id = x$ID, ..., tz = lubridate::tz(x$t))
    } else if (table == "dt") {
      track_dt(x = x$x, y = x$y, t = x$t, id = x$ID, ..., tz = lubridate::tz(x$t))
    } else {
      stop("Invalid table type.")
    }
  } else {
    stop("Invalid type.")
  }
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
as_moveHMM.track <- function(x, ...) {
  if (n_dims(x) > 2)
    warning("telemetry objects are 2D only. The 3rd dimension will be stripped away.")

  if (is.na(projection(x)@projargs)) {
    colnames(x)[colnames(x) == "id"] <- "ID"
    moveHMM::prepData(as.data.frame(x), type = "UTM", LLangle = FALSE)
  } else {
    projection(x) <- "+proj=longlat"
    colnames(x)[colnames(x) == "id"] <- "ID"
    moveHMM::prepData(as.data.frame(x), type = "LL", LLangle = TRUE)
  }
}
