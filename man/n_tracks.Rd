% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{n_tracks}
\alias{n_tracks}
\title{Number of Tracks in a Track Table}
\usage{
n_tracks(x)
}
\arguments{
\item{x}{A track data table as produced by \code{\link{track_df}}.}
}
\value{
A numeric value.
}
\description{
Track tables produced by \code{\link{track_df}} can contain
 multiple tracks (e.g., from different animals). This function returns the
 number of tracks in a track table.
}
\examples{
data(tracks)

n_tracks(tracks)

}
\author{
Simon Garnier, \email{garnier@njit.edu}
}
