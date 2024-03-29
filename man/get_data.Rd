% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_data.R
\name{get_data}
\alias{get_data}
\title{Obtener la última actualización de un conjunto de datos de Datafaro}
\usage{
get_data(.dataset, .token = Sys.getenv("DATAFARO_TOKEN"))
}
\arguments{
\item{.dataset}{\link{character} Código del conjunto de datos}

\item{.token}{\link{character} Token de acceso a la API. Por defecto se utiliza el
token guardado en la variable de entorno \code{DATAFARO_TOKEN}. Si no se ha
guardado un token, se debe especificar el token manualmente.}
}
\value{
Un data.frame con la última actualización del conjunto de datos.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
\examples{
\dontrun{
  get_data("imae")
}
}
