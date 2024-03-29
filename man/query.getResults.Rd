% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bdc.R
\name{query.getResults}
\alias{query.getResults}
\title{Retreve results from a previously run query.}
\usage{
query.getResults(resource, query_uuid)
}
\arguments{
\item{resource}{A resource connection instance object.}

\item{query_uuid}{The UUID identifier for the query to get results for.}
}
\description{
Retreve results from a previously run query.
}
\examples{

# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
# myresults <- hpds::query.getResults(resource=myres, query_uuid="YOUR-QUERY-UUID-0000")

}
