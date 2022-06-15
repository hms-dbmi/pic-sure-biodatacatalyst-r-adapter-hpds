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
    head(consents)

    annotations = bdc::get.genotypeAnnotations(authPicSure)
    annotations[,c('genomic_annotation', 'description', 'continuous')] # excluding values list because it's huge...

    # Test search for invalid path
    bdc::get.varInfo(compliant, "what")
    consent_filter = consents[Reduce(study_filter('phs001211'), consents[['consent']], init=c()), 'consent']
    consent_filter

    ## Compliant Example (ARIC)
    compliant = bdc::find.in.dictionary(dictionary, "phs001211")
    bdc::get.count(compliant)
    as.character(bdc::get.paths(compliant))

    compdf = bdc::extract.dataframe(compliant)
    subset = compdf[compdf["data_type"] == "categorical",]
    head(subset)

    bdc::get.varInfo(compliant, subset[6, 'HPDS_PATH'])
    
    query = bdc::new.query(authPicSure)
    bdc::query.filter.add(query, "\\phs001211\\pht005757\\phv00397013\\TOPMed_Phase\\", min=1.0, max=1.4)
    bdc::query.filter.delete(query, "\\_consents\\")
    bdc::query.filter.add(query, "\\_consents\\", as.list(consent_filter))
    bdc::query.show(query)
    results = bdc::query.run(query)
    head(results)

    ## Non-Compliant (BABYHUG)
    noncompliant = bdc::find.in.dictionary(dictionary, "phs002415")
    bdc::get.count(noncompliant)

    head(as.character(bdc::get.paths(noncompliant)))

    noncompdf = bdc::extract.dataframe(noncompliant)
    subset = noncompdf[noncompdf["data_type"] != "categorical",]
    head(subset)

    bdc::get.varInfo(noncompliant, subset[1,"HPDS_PATH"])

    consent_filter = consents[Reduce(study_filter('phs002415'), consents[['consent']], init=c()), 'consent']
    consent_filter

    query = bdc::new.query(authPicSure)
    invisible(lapply(c(subset[1:50,'HPDS_PATH']), function(path) bdc::query.select.add(query, path)))
    bdc::query.filter.delete(query, "\\_consents\\")
    bdc::query.filter.add(query, "\\_consents\\", as.list(consent_filter))
    bdc::query.show(query)

    results = bdc::query.run(query)
    head(results)

    ## Harmonized
    harmonized = bdc::find.in.dictionary(dictionary, "DCC Harmonized data set")
    bdc::get.count(harmonized)

    head(as.character(bdc::get.paths(harmonized)))

    harmonizeddf = bdc::extract.dataframe(harmonized)
    subset = harmonizeddf[(
        harmonizeddf["data_type"] == "categorical" 
        & apply(harmonizeddf["var_name"], 1, function(x) str_detect(x, "carotid"))
    ),]
    subset

    query = bdc::new.query(authPicSure)
    invisible(lapply(c(subset[,'HPDS_PATH']), function(path) bdc::query.select.add(query, path)))
    results = bdc::query.run(query)
    head(results)

    ## Query UUID
    query_id = "71071afc-41d0-4d1c-b7f9-cc54868e9628"
    results <- bdc::query.getResults(authPicSure, query_id)
    results <- read.table(textConnection(results), sep = ",")
    head(results)

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
