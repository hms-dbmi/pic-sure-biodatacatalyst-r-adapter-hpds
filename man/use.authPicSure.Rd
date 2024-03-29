% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bdc.R
\name{use.authPicSure}
\alias{use.authPicSure}
\title{Get a new reference to the auth PIC-SURE HPDS-based resource.}
\usage{
use.authPicSure(connection, verbose = FALSE)
}
\arguments{
\item{connection}{A PIC-SURE connection object.}

\item{verbose}{Flag to display additional runtime information.}
}
\value{
An object which provides access to the auth PIC-SURE resource.
}
\description{
Get a new reference to the auth PIC-SURE HPDS-based resource.
}
\examples{

# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
# myres <- hpds::use.authPicSure(connection=myconn)

}
