% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proposicoes.R
\name{fetch_relacionadas}
\alias{fetch_relacionadas}
\title{Fetches all propositions related to a proposition}
\usage{
fetch_relacionadas(id_prop)
}
\arguments{
\item{id_prop}{Proposition's ID}
}
\value{
Dataframe containing all the related propositions.
}
\description{
Returns all propositions related to a proposition by its id.
}
\examples{
relacionadas_pec241 <- fetch_relacionadas(2088351)
}
\seealso{
\code{\link[rcongresso]{fetch_id_proposicao_camara}}
}
