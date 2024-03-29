% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bdc.R
\name{get.consents}
\alias{get.consents}
\title{Get a data frame of consents.}
\usage{
get.consents(resource, verbose = FALSE)
}
\arguments{
\item{resource}{A PIC-SURE resource object.}

\item{verbose}{Flag to display additional runtime information.}
}
\value{
A list of HPDS paths.
}
\description{
Get a data frame of consents.
}
\examples{

# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
# get.consents(myres)

}
