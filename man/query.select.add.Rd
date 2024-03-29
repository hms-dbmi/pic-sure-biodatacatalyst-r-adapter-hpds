% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bdc.R
\name{query.select.add}
\alias{query.select.add}
\title{Add a "select" restriction to a query instance.}
\usage{
query.select.add(query, keys, verbose = FALSE)
}
\arguments{
\item{query}{A query instance object.}

\item{keys}{One or more keys to add to the given query object's select list.}

\item{verbose}{Flag to display additional runtime information.}
}
\description{
Add a "select" restriction to a query instance.
}
\examples{

# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
# myquery <- hpds::new.query(resource=myres)
# hpds::query.select.add(query=myquery, keys="\\demographics\\SEX\\")

}
