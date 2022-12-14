% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_tbClicked.R
\name{tbClickedServer}
\alias{tbClickedServer}
\title{Server Module for reactable of clicked node}
\usage{
tbClickedServer(
  id,
  data,
  height,
  dict.combine,
  name_weight = "cosine similarity"
)
}
\arguments{
\item{id}{string. Namespace of the module.}

\item{data}{dataframe. the collected data for the clicked node.}

\item{height}{string. A valid CSS unit \(like "100\%", "400px", "auto"\).}

\item{dict.combine}{dataframe. "id", "term", "category"}

\item{name_weight}{string. Default "cosine similarity". Name for the column of weight in "data".}
}
\value{
server part for tbClicked UI.
}
\description{
Server Module for reactable of clicked node
}
\examples{
\dontrun{
tbClickedServer('tb_clicked')
}
}
