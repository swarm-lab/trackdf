% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{is_geo}
\alias{is_geo}
\title{Check if Track Table Uses Geographic Coordinates}
\usage{
is_geo(x)
}
\arguments{
\item{x}{A track data table as produced by \code{\link{track_df}}.}
}
\value{
A logical.
}
\description{
Track tables produced by \code{\link{track_df}} can use a
 cartesian (x, y, z) or a geographic (latitude, longitude, altitude)
 coordinate system. This function helps determine which is being used in a
 particular table.
}
\examples{
data(short_tracks)

is_geo(short_tracks)

}
\author{
Simon Garnier, \email{garnier@njit.edu}
}
