% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hpds.R
\name{query.run}
\alias{query.run}
\title{Run a query instance using any restrictions that have been added to it.}
\usage{
query.run(query, result.type = "dataframe", verbose = FALSE)
}
\arguments{
\item{query}{A query instance object.}

\item{result.type}{A string specifying what type of results to return. Possible values: "count", "results", "dataframe" and "crosscount".}

\item{verbose}{Flag to display additional runtime information.}
}
\description{
Run a query instance using any restrictions that have been added to it.
}
\examples{

# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
# myquery <- hpds::new.query(resource=myres)

## ...modify the query by adding search criteria... ##

# results <- hpds::query.run(query=myquery)

}
