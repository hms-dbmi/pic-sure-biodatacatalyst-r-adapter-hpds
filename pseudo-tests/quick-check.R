# Quick and dirty script file to test functionality (run manually)

library(picsure)
library(bdc)

runtest = function(url, token) {
    ## Initialize connection and resources
    study_filter = function(study) function(matches, consent) c(matches, str_detect(consent, paste0("^(", study, ")")))
    connection = picsure::connect(url=url, token=token)
    authPicSure = bdc::use.authPicSure(connection)
    dictionary = bdc::use.dictionary(connection)

    consents = bdc::get.consents(authPicSure)
    head(consents[1:5,])

    annotations = bdc::get.genotypeAnnotations(authPicSure)
    annotations[,c('genomic_annotation', 'description', 'continuous')] # excluding values list because it's huge...

    ## Compliant Example (ARIC)
    compliant = bdc::find.in.dictionary(dictionary, "phs001211")
    print(bdc::get.count(compliant))

    paths = bdc::get.paths(compliant)
    print(as.character(paths[1:5]))

    compdf = bdc::extract.dataframe(compliant)
    subset = compdf[compdf["data_type"] == "categorical",]
    print(head(subset[1,]))

    print(bdc::get.varInfo(compliant, subset[6, 'HPDS_PATH']))

    # Test search for invalid path
    bdc::get.varInfo(compliant, "what")
    consent_filter = consents[Reduce(study_filter('phs001211'), consents[['consent']], init=c()), 'consent']
    print(consent_filter)
    
    query = bdc::new.query(authPicSure)
    bdc::query.filter.add(query, "\\phs001211\\pht005757\\phv00397013\\TOPMed_Phase\\", min=1.0, max=1.4)
    bdc::query.filter.delete(query, "\\_consents\\")
    bdc::query.filter.add(query, "\\_consents\\", as.list(consent_filter))
    bdc::query.show(query)
    results = bdc::query.run(query)
    print(results[1:2,])

    ## Non-Compliant (BABYHUG)
    noncompliant = bdc::find.in.dictionary(dictionary, "phs002415")
    print(bdc::get.count(noncompliant))

    paths = bdc::get.paths(noncompliant)
    print(as.character(paths[1:5]))

    noncompdf = bdc::extract.dataframe(noncompliant)
    subset = noncompdf[noncompdf["data_type"] != "categorical",]
    print(subset[1,])

    print(bdc::get.varInfo(noncompliant, subset[1,"HPDS_PATH"]))

    consent_filter = consents[Reduce(study_filter('phs002415'), consents[['consent']], init=c()), 'consent']
    print(consent_filter)

    query = bdc::new.query(authPicSure)
    invisible(lapply(c(subset[1:50,'HPDS_PATH']), function(path) bdc::query.select.add(query, path)))
    bdc::query.filter.delete(query, "\\_consents\\")
    bdc::query.filter.add(query, "\\_consents\\", as.list(consent_filter))
    bdc::query.show(query)

    results = bdc::query.run(query)
    print(results[1:2,])

    ## Harmonized
    harmonized = bdc::find.in.dictionary(dictionary, "DCC Harmonized data set")
    print(bdc::get.count(harmonized))

    paths = bdc::get.paths(harmonized)
    print(as.character(paths[1:5]))

    harmonizeddf = bdc::extract.dataframe(harmonized)
    subset = harmonizeddf[(
        harmonizeddf["data_type"] == "categorical" 
        & apply(harmonizeddf["var_name"], 1, function(x) str_detect(x, "carotid"))
    ),]
    print(subset[1,])

    query = bdc::new.query(authPicSure)
    invisible(lapply(c(subset[,'HPDS_PATH']), function(path) bdc::query.select.add(query, path)))
    results = bdc::query.run(query)
    print(results[1:2,])

    ## Query UUID
    query_id = "71071afc-41d0-4d1c-b7f9-cc54868e9628"
    results <- bdc::query.getResults(authPicSure, query_id)
    results <- read.table(textConnection(results), sep = ",")
    print(results[1:2,])

    ## Add and delete query elements
    keyA = "\\_studies_consents\\phs000007\\HMB-IRB-NPU-MDS\\"
    keyB = "\\_studies_consents\\phs000209\\HMB\\"
    query = bdc::new.query(authPicSure)
    bdc::query.filter.delete(query, "\\_consents\\")
    bdc::query.select.add(query, keyA)
    bdc::query.require.add(query, keyA)
    bdc::query.anyof.add(query, keyA)
    bdc::query.anyof.add(query, keyB)
    bdc::query.crosscounts.add(query, keyA)
    bdc::query.show(query)
    bdc::query.select.delete(query, keyA)
    bdc::query.require.delete(query, keyA)
    bdc::query.anyof.delete(query, keyA)
    bdc::query.crosscounts.delete(query, keyA)
    bdc::query.show(query)

    # Variant query
    query = bdc::new.query(authPicSure)
    bdc::query.filter.add(query, "Gene_with_variant", values = "CHD8")
    bdc::query.show(query)
}
