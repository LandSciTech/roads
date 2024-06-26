% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dem_example.R
\docType{data}
\name{dem_example}
\alias{dem_example}
\title{Grade penalty example data}
\format{
A named list with components:
\itemize{
\item ex_elev: a \code{PackedSpatRaster} of elevation.
\item ex_wat: a \code{PackedSpatRaster} of proportion water.
}
}
\usage{
data(dem_example)
}
\description{
A list containing two rasters covering an area near Revelstoke, British
Columbia, Canada. \code{ex_elev} is elevation data and \code{ex_wat} is the proportion
of the cell that contains water. Both are subsets of data downloaded with the
geodata package at 30 arc seconds resolution.\code{SpatRaster} files created with
the terra package must be saved with \code{\link[terra:wrap]{terra::wrap()}} and need to be unwrapped
before they are used. \code{\link[=prepExData]{prepExData()}} does this.
}
\details{
Elevation data are primarily from Shuttle Radar Topography Mission (SRTM),
specifically the hole-filled CGIAR-SRTM (90 m resolution) from
https://srtm.csi.cgiar.org/.

Water data are derived from the ESA WorldCover data set at 0.3-seconds
resolution. (License CC BY 4.0). See https://esa-worldcover.org/en for more
information.
}
\examples{
dem_example
prepExData(dem_example)
}
\references{
Zanaga, D., Van De Kerchove, R., De Keersmaecker, W., Souverijns,
N., Brockmann, C., Quast, R., Wevers, J., Grosu, A., Paccini, A., Vergnaud,
S., Cartus, O., Santoro, M., Fritz, S., Georgieva, I., Lesiv, M., Carter,
S., Herold, M., Li, Linlin, Tsendbazar, N.E., Ramoino, F., Arino, O., 2021.
ESA WorldCover 10 m 2020 v100. doi:10.5281/zenodo.5571936.
}
\keyword{datasets}
