% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R6-classes.R
\docType{class}
\name{PicSureHpdsDictionary}
\alias{PicSureHpdsDictionary}
\title{R6 class that runs searches against a HPDS resource's data dictionary - DO NOT CREATE THIS OBJECT DIRECTLY!}
\format{\code{\link{PicSureHpdsDictionary}} object.}
\usage{
PicSureHpdsDictionary
}
\value{
Object of \code{\link{R6Class}} used to access a HPDS-hosted resource's data dictionary.
}
\description{
R6 class that runs searches against a HPDS resource's data dictionary - DO NOT CREATE THIS OBJECT DIRECTLY!
}
\section{Methods}{

\describe{
  \item{Documentation}{For full documentation of each method go to https://github.com/hms-dbmi/pic-sure-r-adapter-hpds}
  \item{\code{new(refHpdsResourceConnection)}}{This method is used to create new object of this class. DO NOT CREATE THIS OBJECT DIRECTLY!}

  \item{\code{find(term=FALSE)}}{This method returns a \code{PicSureHpdsDictionaryResult} object containing the results of the search on the HPDS resource's data dictionary.}}
}

\keyword{data}
