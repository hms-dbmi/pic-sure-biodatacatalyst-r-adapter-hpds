#' bdc: An Adapter Library for HPDS Services on the BioData Catalyst Network.
#'
#' This package is used to interact with HPDS Services and data hosted
#' on a PIC-SURE network.  This library requires the 'picsure' package
#' to handle all connectivity and authentication with the PIC-SURE
#' Network itself.
#'
#' @docType package
#' @name bdc
#'
NULL



#' Get a new reference to a HPDS-based PIC-SURE resource.
#'
#' @param connection A PIC-SURE connection object.
#' @param resourceUUID The UUID identity of a Resource hosted via the PIC-SURE connection.
#' @param verbose Flag to display additional runtime information.
#' @return An object which provides access to the requested Resource.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'
#' @export
get.resource <- function(connection, resourceUUID, verbose=FALSE) {
  if (class(connection) == "PicSure_Connection") {
    result <- PicSureHpdsResourceConnectionBdc$new(connection, resourceUUID)
    class(result) <- "Hpds_Resource"
    return(result)
  } else {
    message("Invalid connection was passed to get.resource() function")
    stop()
  }
}

#' Get a new reference to the dictionary HPDS-based PIC-SURE resource.
#'
#' @param connection A PIC-SURE connection object.
#' @param verbose Flag to display additional runtime information.
#' @return An object which provides access to the dictionary resource.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::use.dictionary(connection=myconn)
#'
#' @export
use.dictionary <- function(connection, verbose=FALSE) {
  return(get.resource(connection, '36363664-6231-6134-2d38-6538652d3131', verbose))
}

#' Get a new reference to the open PIC-SURE HPDS-based resource.
#'
#' @param connection A PIC-SURE connection object.
#' @param verbose Flag to display additional runtime information.
#' @return An object which provides access to the open PIC-SURE resource.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::use.openPicSure(connection=myconn)
#'
#' @export
use.openPicSure <- function(connection, verbose=FALSE) {
  return(get.resource(connection, '70c837be-5ffc-11eb-ae93-0242ac130002', verbose))
}

#' Get a new reference to the auth PIC-SURE HPDS-based resource.
#'
#' @param connection A PIC-SURE connection object.
#' @param verbose Flag to display additional runtime information.
#' @return An object which provides access to the auth PIC-SURE resource.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::use.authPicSure(connection=myconn)
#'
#' @export
use.authPicSure <- function (connection, verbose=FALSE) {
  return(get.resource(connection, '02e23f52-f354-4e8b-992c-d37c8b9ba140', verbose))
}

# ===== data dictionary functions =====


#' Search the data dictionary of a PIC-SURE resource.
#'
#' @param resource A PIC-SURE resource object.
#' @param term A string to search for in the resource's data dictionary. By default, if no term is provided, the whole dictionary will be returned.
#' @param limit A limit of return results.
#' @param offset An offset to start retrning values from.
#' @param verbose Flag to display additional runtime information.
#' @return An object representing the search results.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# asthma.terms <- hpds::find.in.dictionary(resource=myres, term="asthma")
#'
#' @export
find.in.dictionary <- function(resource, term="", limit=0, offset=0, verbose=FALSE){
  if (class(resource) == "Hpds_Resource") {
    dictionaryObj <- resource$dictionary()
    class(dictionaryObj) <- "Hpds_Dictionary"
    result <- dictionaryObj$find(term, limit, offset)
    class(result) <- "Hpds_DictionaryResults"
    return(result)
  } else {
    message("Invalid resource was passed to find.in.dictionary() function")
    stop()
  }
}


#' Get the number of results returned in the given data dictionary lookup.
#'
#' @param dictionary.results A data dictionary search results object.
#' @param verbose Flag to display additional runtime information.
#' @return An integer of how many data dictionary entries were found in
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# asthma.terms <- hpds::find.in.dictionary(resource=myres, term="asthma")
#'# get.count(asthma.terms)
#'
#' @export
get.count <- function(dictionary.results, verbose=FALSE) {
  if (class(dictionary.results) == "Hpds_DictionaryResults") {
    result <- dictionary.results$count()
    return(result)
  } else {
    message("Invalid dictionary results was passed to get.count() function")
    stop()
  }
}

#' Get variable info for a given path.
#'
#' @param dictionary.results A data dictionary search results object.
#' @param path A character object containing an HPDS path.
#' @param verbose Flag to display additional runtime information.
#' @return A data frame object containing variable info for an HPDS path.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# asthma.terms <- hpds::find.in.dictionary(resource=myres, term="asthma")
#'# get.varInfo(asthma.terms, '\\phs001211\\...')
#'
#' @export
get.varInfo <- function(dictionary.results, path, verbose=FALSE) {
  if (class(dictionary.results) == "Hpds_DictionaryResults") {
    result <- dictionary.results$varInfo(path)
    return(result)
  } else {
    message("Invalid dictionary results was passed to get.varInfo() function")
    stop()
  }
}

#' Get a list of HPDS paths.
#'
#' @param dictionary.results A data dictionary search results object.
#' @param verbose Flag to display additional runtime information.
#' @return A list of HPDS paths.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# asthma.terms <- hpds::find.in.dictionary(resource=myres, term="asthma")
#'# get.paths(asthma.terms)
#'
#' @export
get.paths <- function(dictionary.results, verbose=FALSE) {
  if (class(dictionary.results) == "Hpds_DictionaryResults") {
    result <- dictionary.results$paths()
    return(result)
  } else {
    message("Invalid dictionary results was passed to get.paths() function")
    stop()
  }
}

#' Get a data frame of consents.
#'
#' @param resource A PIC-SURE resource object.
#' @param verbose Flag to display additional runtime information.
#' @return A list of HPDS paths.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# get.consents(myres)
#'
#' @export
get.consents <- function(resource, verbose=FALSE) {
  if (class(resource) == "Hpds_Resource") {
    result <- resource$consents()
    return(result)
  } else {
    message("Invalid resource was passed to get.consents() function")
    stop()
  }
}

#' Get a list of genomic annotations as a data frame.
#'
#' @param resource A PIC-SURE resource object.
#' @param verbose Flag to display additional runtime information.
#' @return Results in a dataframe object.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# get.genotypeAnnotations(myres)
#'
#' @export
get.genotypeAnnotations <- function(resource, verbose=FALSE) {
  if (class(resource) == "Hpds_Resource") {
    result <- resource$dictionary()$genotypeAnnotations()
    return(result)
  } else {
    message("Invalid resource was passed to get.genotypeAnnotations() function")
    stop()
  }
}


#' Extract the entries of all the results in a given data dictionary lookup.
#'
#' @param dictionary.results A dictionary results object.
#' @param verbose Flag to display additional runtime information.
#' @return A collection of result entries.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# asthma.terms <- hpds::find.in.dictionary(resource=myres, term="asthma")
#'# extract.entries(asthma.terms)
#'
#' @export
extract.entries <- function(dictionary.results, verbose=FALSE) {
  if (class(dictionary.results) == "Hpds_DictionaryResults") {
    result <- dictionary.results$entries()
    return(result)
  } else {
    message("Invalid dictionary results was passed to extract.entries() function")
    stop()
  }
}


#' Extract all the results of a given data dictionary lookup in a data frame format.
#'
#' @param dictionary.results A dictionary results object.
#' @param verbose Flag to display additional runtime information.
#' @return Results in a dataframe format.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# asthma.terms <- hpds::find.in.dictionary(resource=myres, term="asthma")
#'# extract.dataframe(asthma.terms)
#'
#' @export
extract.dataframe <- function(dictionary.results, verbose=FALSE) {
  if (class(dictionary.results) == "Hpds_DictionaryResults") {
    result <- dictionary.results$dataframe()
    return(result)
  } else {
    message("Invalid dictionary result was passed to extract.entries() function")
    stop()
  }
}


# ===== query functions =====


#' Create a new query instance with no restrictions.
#'
#' @param resource A resource object that the returned query object will execute against.
#' @param verbose Flag to display additional runtime information.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# myquery <- hpds::new.query(resource=myres)
#'
#' @export
new.query <- function(resource, verbose=FALSE) {
  if (class(resource) == "Hpds_Resource") {
    result <- resource$query()
    class(result) <- "Hpds_Query"
    return(result)
  } else {
    message("The resource given to new.query() is not a Hpds_Query typed object")
    stop()
  }
}


#' Run a query instance using any restrictions that have been added to it.
#'
#' @param query A query instance object.
#' @param result.type A string specifying what type of results to return. Possible values: "count", "dataframe", "variantsApproximateCount" and "variantsDataFrame".
#' @param verbose Flag to display additional runtime information.
#' @details Description of result.type values
##' \itemize{
##'  \item{"count": }{Single count indicating the number of matching records}
##'  \item{"dataframe": }{DataFrame containing the matching records}
##'  \item{"variantsApproximateCount": }{Single estimated count of the number of matching variants}
##'  \item{"variantsDataFrame": }{DataFrame of the matching variants}
##' }#' @param verbose Flag to display additional runtime information.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# myquery <- hpds::new.query(resource=myres)
#'
#'## ...modify the query by adding search criteria... ##
#'
#'# results <- hpds::query.run(query=myquery)
#'
#' @export
query.run <- function(query, result.type="results", verbose=FALSE) {
  if (class(query) == "Hpds_Query") {
    result <- switch(result.type,
      "count" = query$getCount(),
      "results" = query$getResults(),
      "dataframe" = query$getResultsDataFrame(),
      "crosscount" = query$getResultsCrossCounts()
    )
    return(result)
  } else {
    message("The resource given to query.run() is not a Hpds_Query typed object")
    stop()
  }
}

#' Load a query from a JSON-formated string.
#'
#' @param query A JSON string that defines the query instance object.
#' @param query.def A query definition to load from.
#' @param verbose Flag to display additional runtime information.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# myquery <- hpds::new.query(resource=myres)
#'
#'## ...modify the query by adding search criteria... ##
#'
#'#  myquery <- hpds::query.load(query=myquerydef)
#'
#' @export
query.load<- function(query, query.def="", verbose=FALSE) {
  if (class(query) == "Hpds_Query") {
    query$load(query.def)
    return(query)
  } else {
    message("The resource given to query.load() is not a Hpds_Query typed object")
    stop()
  }
}

#' Retrieve an existing query by its unique identifier
#'
#' @param resource A resource object that the returned query object will execute against.
#' @param queryUUID the valid UUID of a query to retrieve
#' @param verbose Flag to display additional runtime information.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# myquery <- hpds::query.from.uuid(resource=myres, queryUUID="UUID" )
#'
#' @export
query.from.uuid <- function(resource, queryUUID, verbose=FALSE) {
  if (class(resource) == "Hpds_Resource") {
    result <- resource$getQueryByUUID(queryUUID)
    class(result) <- "Hpds_Query"
    return(result)
  } else {
    message("The resource given to new.query() is not a Hpds_Query typed object")
    stop()
  }
}

#' Save a query in JSON-format with any restrictions that have been added to it.
#'
#' @param query A query instance object.
#' @param result.type The type of result to save.
#' @param verbose Flag to display additional runtime information.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# myquery <- hpds::new.query(resource=myres)
#'
#'## ...modify the query by adding search criteria... ##
#'
#'# querydef <- hpds::query.save(query=myquery)
#'
#' @export
query.save<- function(query, result.type="dataframe", verbose=FALSE) {
  if (class(query) == "Hpds_Query") {
    result = query$save(result.type)
    return(result)
  } else {
    message("The resource given to query.save() is not a Hpds_Query typed object")
    stop()
  }
}


#' Add a "select" restriction to a query instance.
#'
#' @param query A query instance object.
#' @param keys One or more keys to add to the given query object's select list.
#' @param verbose Flag to display additional runtime information.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# myquery <- hpds::new.query(resource=myres)
#'# hpds::query.select.add(query=myquery, keys="\\demographics\\SEX\\")
#'
#' @export
query.select.add <- function(query, keys, verbose=FALSE) {
  if (class(query) == "Hpds_Query") {
    query$select()$add(keys)
  } else {
    message("The query given to query.select.add() is not a Hpds_Query typed object")
    stop()
  }
}


#' Delete a "select" restriction from a query instance.
#'
#' @param query A query instance object.
#' @param keys One or more keys to delete from the given query object's select list.
#' @param verbose Flag to display additional runtime information.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# myquery <- hpds::new.query(resource=myres)
#'# hpds::query.select.add(query=myquery, keys="\\demographics\\SEX\\")
#'## ...opps, added wrong term, lets delete it...
#'# hpds::query.select.delete(query=myquery, keys="\\demographics\\SEX\\")
#'
#' @export
query.select.delete <- function(query, keys, verbose=FALSE) {
  if (class(query) == "Hpds_Query") {
    query$select()$delete(keys)
  } else {
    message("The query given to query.select.delete() is not a Hpds_Query typed object")
    stop()
  }
}


#' Add a "cross-counts" restriction to a query instance.
#'
#' @param query A query instance object.
#' @param keys One or more keys to add to the given query object's cross-count list.
#' @param verbose Flag to display additional runtime information.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# myquery <- hpds::new.query(resource=myres)
#'# hpds::query.crosscounts.add(query=myquery, keys="\\demographics\\SEX\\")
#'
#' @export
query.crosscounts.add <- function(query, keys, verbose=FALSE) {
  if (class(query) == "Hpds_Query") {
    query$crosscounts()$add(keys)
  } else {
    message("The query given to query.crosscounts.add() is not a Hpds_Query typed object")
    stop()
  }
}


#' Delete a "cross-counts" restriction from a query instance.
#'
#' @param query A query instance object.
#' @param keys One or more keys to delete from the given query object's cross-count list.
#' @param verbose Flag to display additional runtime information.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# myquery <- hpds::new.query(resource=myres)
#'# hpds::query.crosscounts.add(query=myquery, keys="\\demographics\\SEX\\")
#'## ...opps, added wrong term, lets delete it...
#'# hpds::query.crosscounts.delete(query=myquery, keys="\\demographics\\SEX\\")
#'
#' @export
query.crosscounts.delete <- function(query, keys, verbose=FALSE) {
  if (class(query) == "Hpds_Query") {
    query$crosscounts()$delete(keys)
  } else {
    message("The query given to query.crosscounts.delete() is not a Hpds_Query typed object")
    stop()
  }
}


#' Add a "require" restriction to a query instance.
#'
#' @param query A query instance object.
#' @param keys One or more keys to add to the given query object's require list.
#' @param verbose Flag to display additional runtime information.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# myquery <- hpds::new.query(resource=myres)
#'# hpds::query.require.add(query=myquery, keys="\\demographics\\SEX\\")
#'
#' @export
query.require.add <- function(query, keys, verbose=FALSE) {
  if (class(query) == "Hpds_Query") {
    query$require()$add(keys)
  } else {
    message("The query given to query.require.add() is not a Hpds_Query typed object")
    stop()
  }
}


#' Delete a "require" restriction from a query instance.
#'
#' @param query A query instance object.
#' @param keys One or more keys to delete from the given query object's require list.
#' @param verbose Flag to display additional runtime information.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# myquery <- hpds::new.query(resource=myres)
#'# hpds::query.require.add(query=myquery, keys="\\demographics\\SEX\\")
#'## ...opps, added wrong term, lets delete it...
#'# hpds::query.require.delete(query=myquery, keys="\\demographics\\SEX\\")
#'
#' @export
query.require.delete <- function(query, keys, verbose=FALSE) {
  if (class(query) == "Hpds_Query") {
    query$require()$delete(keys)
  } else {
    message("The query given to query.require.delete() is not a Hpds_Query typed object")
    stop()
  }
}


#' Add an "AnyOf" restriction from a query instance.
#'
#' @param query A query instance object.
#' @param keys One or more keys to add to the given query object's AnyOf list.
#' @param verbose Flag to display additional runtime information.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# myquery <- hpds::new.query(resource=myres)
#'# hpds::query.anyof.add(query=myquery, keys="\\demographics\\SEX\\")
#'
#' @export
query.anyof.add <- function(query, keys, verbose=FALSE) {
  if (class(query) == "Hpds_Query") {
    query$anyof()$add(keys)
  } else {
    message("The query given to query.anyof.add() is not a Hpds_Query typed object")
    stop()
  }
}


#' Delete an "AnyOf" restriction from a query instance.
#'
#' @param query A query instance object.
#' @param keys One or more keys to delete from the given query object's AnyOf list.
#' @param verbose Flag to display additional runtime information.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# myquery <- hpds::new.query(resource=myres)
#'# hpds::query.anyof.add(query=myquery, keys="\\demographics\\SEX\\")
#'## ...opps, added wrong term, lets delete it...
#'# hpds::query.anyof.delete(query=myquery, keys="\\demographics\\SEX\\")
#'
#' @export
query.anyof.delete <- function(query, keys, verbose=FALSE) {
  if (class(query) == "Hpds_Query") {
    query$anyof()$delete(keys)
  } else {
    message("The query given to query.anyof.delete() is not a Hpds_Query typed object")
    stop()
  }
}


#' Add a "Filter" restriction to a query instance.
#'
#' @param query A query instance object.
#' @param keys One or more keys to add to the given query object's filter list.
#' @param verbose Flag to display additional runtime information.
#' @param ... Extra entries such as "min", "max" and "value"
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# myquery <- hpds::new.query(resource=myres)
#'# hpds::query.select.add(query=myquery, keys="\\demographics\\SEX\\", values="Male")
#'# hpds::query.select.add(query=myquery, keys="\\demographics\\AGE\\", min=1, max=5)
#'
#' @export
query.filter.add <- function(query, keys, ..., verbose=FALSE) {
  if (class(query) != "Hpds_Query") {
    message("The query given to query.filter.add() is not a Hpds_Query typed object")
    stop()
  }
  args = list(...)
  minmaxError = function(errorText) {
    # load the range list first, so the message text prints after we download the metadata
    rangeList = Reduce(
      function(acc, key) {
        info = query$dictionary$getKeyInfo(key)
        return(paste(acc, paste('-', key, "min:", info$min, "max:", info$max), sep="\n"))
      },
      as.list(keys),
      init="Min and max ranges for keys provided:"
    )
    message(paste0(errorText, "\n\n", rangeList))
  }
  
  if(is.null(args$min) && !is.null(args$max)){
    return(minmaxError('Please specify a minimum value.'))
  } else if(is.null(args$max) && !is.null(args$min)){
    return(minmaxError('Please specify a maximum value.'))
  }
  
  query$filter()$add(keys, ...)
}


#' Delete a "Filter" restriction from a query instance.
#'
#' @param query A query instance object.
#' @param keys One or more keys to delete from the given query object's filter list.
#' @param verbose Flag to display additional runtime information.
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# myquery <- hpds::new.query(resource=myres)
#'# hpds::query.filter.add(query=myquery, keys="\\demographics\\SEX\\", values="Male")
#'## ...opps, added wrong term, lets delete it...
#'# hpds::query.filter.delete(query=myquery, keys="\\demographics\\SEX\\")
#'
#' @export
query.filter.delete <- function(query, keys, verbose=FALSE) {
  if (class(query) == "Hpds_Query") {
    query$filter()$delete(keys)
  } else {
    message("The query given to query.filter.delete() is not a Hpds_Query typed object")
    stop()
  }
}


#' Prints to screen the contents of a query instance's restriction parameters.
#'
#' @param query A query instance object.
#' @param verbose Flag to display additional runtime information.
#' @export
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# myquery <- hpds::new.query(resource=myres)
#'# hpds::query.select.add(query=myquery, keys="\\demographics\\SEX\\")
#'# hpds::query.select.add(query=myquery, keys="\\demographics\\AGE\\")
#'# hpds::query.filter.add(query=myquery, keys="\\demographics\\SEX\\", values="Female")
#'# hpds::query.filter.add(query=myquery, keys="\\demographics\\AGE\\", min="10", max="65")
#'# hpds::query.show(query=myquery)
#'
#' @export
query.show <- function(query, verbose=FALSE) {
  if (class(query) == "Hpds_Query") {
    query$show()
  } else {
    message("The query given to query.show() is not a Hpds_Query typed object")
    stop()
  }
}


#' Retreve results from a previously run query.
#'
#' @param resource    A resource connection instance object.
#' @param query_uuid  The UUID identifier for the query to get results for.
#' @export
#' @examples
#'
#'# myconn <- picsure::connect(url="http://your.server/PIC-SURE/", token="your-security-token")
#'# myres <- hpds::get.resource(connection=myconn, resourceUUID="YOUR-UUID-0000")
#'# myresults <- hpds::query.getResults(resource=myres, query_uuid="YOUR-QUERY-UUID-0000")
#'
#' @export
query.getResults <- function(resource, query_uuid) {
  if (class(resource) == "Hpds_Resource") {
    result <- resource$retrieveQueryResults(query_uuid = query_uuid)
    return(result)
  } else {
    message("The resource given to query.getResults() is not a Hpds_Resource typed object")
    stop()
  }
}
