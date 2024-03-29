% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bdc.R
\name{query.anyof.delete}
\alias{query.anyof.delete}
\title{Delete an "AnyOf" restriction from a query instance.}
\usage{
query.anyof.delete(query, keys, verbose = FALSE)
}
\arguments{
\item{query}{A query instance object.}

\item{keys}{One or more keys to delete from the given query object's AnyOf list.}

\item{verbose}{Flag to display additional runtime information.}
}
\description{
Delete an "AnyOf" restriction from a query instance.
}
\examples{

# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
# myquery <- hpds::new.query(resource=myres)
# hpds::query.anyof.add(query=myquery, keys="\\demographics\\SEX\\")
## ...opps, added wrong term, lets delete it...
# hpds::query.anyof.delete(query=myquery, keys="\\demographics\\SEX\\")

}
