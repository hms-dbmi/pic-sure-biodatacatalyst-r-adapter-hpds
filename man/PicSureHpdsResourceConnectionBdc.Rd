% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R6-classes.R
\docType{class}
\name{PicSureHpdsResourceConnectionBdc}
\alias{PicSureHpdsResourceConnectionBdc}
\title{PicSureHpdsResourceConnectionBdc}
\value{
Object of \code{\link{R6Class}} used to access the dictionary and query services via the objects it returns.
}
\description{
R6 class that allows access to the data dictionary and query services of a selected HPDS-hosted resources on a PIC-SURE network.

For full documentation of each method go to https://github.com/hms-dbmi/pic-sure-r-adapter-hpds
}
\keyword{data}
\section{Super class}{
\code{\link[hpds:PicSureHpdsResourceConnection]{hpds::PicSureHpdsResourceConnection}} -> \code{PicSureHpdsResourceConnectionBdc}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PicSureHpdsResourceConnectionBdc-new}{\code{PicSureHpdsResourceConnectionBdc$new()}}
\item \href{#method-PicSureHpdsResourceConnectionBdc-consents}{\code{PicSureHpdsResourceConnectionBdc$consents()}}
\item \href{#method-PicSureHpdsResourceConnectionBdc-query}{\code{PicSureHpdsResourceConnectionBdc$query()}}
\item \href{#method-PicSureHpdsResourceConnectionBdc-clone}{\code{PicSureHpdsResourceConnectionBdc$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="hpds" data-topic="PicSureHpdsResourceConnection" data-id="dictionary"><a href='../../hpds/html/PicSureHpdsResourceConnection.html#method-PicSureHpdsResourceConnection-dictionary'><code>hpds::PicSureHpdsResourceConnection$dictionary()</code></a></li>
<li><span class="pkg-link" data-pkg="hpds" data-topic="PicSureHpdsResourceConnection" data-id="getQueryByUUID"><a href='../../hpds/html/PicSureHpdsResourceConnection.html#method-PicSureHpdsResourceConnection-getQueryByUUID'><code>hpds::PicSureHpdsResourceConnection$getQueryByUUID()</code></a></li>
<li><span class="pkg-link" data-pkg="hpds" data-topic="PicSureHpdsResourceConnection" data-id="retrieveQueryResults"><a href='../../hpds/html/PicSureHpdsResourceConnection.html#method-PicSureHpdsResourceConnection-retrieveQueryResults'><code>hpds::PicSureHpdsResourceConnection$retrieveQueryResults()</code></a></li>
<li><span class="pkg-link" data-pkg="hpds" data-topic="PicSureHpdsResourceConnection" data-id="version"><a href='../../hpds/html/PicSureHpdsResourceConnection.html#method-PicSureHpdsResourceConnection-version'><code>hpds::PicSureHpdsResourceConnection$version()</code></a></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PicSureHpdsResourceConnectionBdc-new"></a>}}
\if{latex}{\out{\hypertarget{method-PicSureHpdsResourceConnectionBdc-new}{}}}
\subsection{Method \code{new()}}{
This method is used to create new object of this class which uses the passed PicSureConnection object for communication with the PIC-SURE network along with a UUID to identify a HPDS-hosted resource.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PicSureHpdsResourceConnectionBdc$new(connection, resource_uuid)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{connection}}{A picsure::PicSureConnection PIC-SURE connection object.}

\item{\code{resource_uuid}}{The UUID identity of a Resource hosted via the PIC-SURE connection.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PicSureHpdsResourceConnectionBdc-consents"></a>}}
\if{latex}{\out{\hypertarget{method-PicSureHpdsResourceConnectionBdc-consents}{}}}
\subsection{Method \code{consents()}}{
This method returns a data frame of consents and their status as harmonized or topmed.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PicSureHpdsResourceConnectionBdc$consents()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A \code{\link{data.frame}} object containing consents and if they are topmed or harmonized.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PicSureHpdsResourceConnectionBdc-query"></a>}}
\if{latex}{\out{\hypertarget{method-PicSureHpdsResourceConnectionBdc-query}{}}}
\subsection{Method \code{query()}}{
This method returns a new bdc::PicSureHpdsQueryBdc object configured to run all commands against the previously specified HPDS-hosted resource.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PicSureHpdsResourceConnectionBdc$query()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A bdc::PicSureHpdsQueryBdc object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PicSureHpdsResourceConnectionBdc-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PicSureHpdsResourceConnectionBdc-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PicSureHpdsResourceConnectionBdc$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
