request_eurovoc_sparql <- function(){
    endpoint <- "http://publications.europa.eu/webapi/rdf/sparql"
    query <- "
        PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
        PREFIX ns3: <http://purl.org/dc/terms/>

          SELECT DISTINCT
          ?eurovoc_id
          ?eurovoc_label
          ?mt_id
          ?mt_label_id
          ?mt_label
          ?domain_id
          ?domain_label_id
          ?domain_label
          from <http://eurovoc.europa.eu/100141>
          where{
          ?eurovoc_id a skos:Concept .
          ?eurovoc_id skos:prefLabel ?eurovoc_label.
          ?eurovoc_id skos:inScheme ?mt_id .
          ?mt_id skos:prefLabel ?mt_label.
          ?mt_id ns3:identifier ?mt_label_id.
          ?mt_id ns3:subject ?domain_id.
          ?domain_id skos:prefLabel ?domain_label.
          ?domain_id ns3:identifier ?domain_label_id.
          filter (str(?mt_id) != 'http://eurovoc.europa.eu/100141')
          filter (lang(?eurovoc_label) = 'en')
          filter (lang(?mt_label) = 'en')
          filter (lang(?domain_label) = 'en')

          }"
    eurovoc_sparql_df <- SPARQL::SPARQL(endpoint,query,format="xml")$results
    return(eurovoc_sparql_df)
}

clean_eurovoc_sparql_df <- function(eurovoc_sparql_df){
    # clean ids
    eurovoc_sparql_df$eurovoc_id <- gsub(".*/|>.*","",eurovoc_sparql_df$eurovoc_id)
    eurovoc_sparql_df$mt_id <- gsub(".*/|>.*","",eurovoc_sparql_df$mt_id)
    eurovoc_sparql_df$domain_id <- gsub(".*/|>.*","",eurovoc_sparql_df$domain_id)

    # clean labels
    eurovoc_sparql_df$eurovoc_label <- gsub("\"","",eurovoc_sparql_df$eurovoc_label)
    eurovoc_sparql_df$eurovoc_label <- gsub("@en","",eurovoc_sparql_df$eurovoc_label)

    eurovoc_sparql_df$mt_label <- gsub("\"","",eurovoc_sparql_df$mt_label)
    eurovoc_sparql_df$mt_label <- gsub("@en","",eurovoc_sparql_df$mt_label)

    eurovoc_sparql_df$domain_label <- gsub("\"","",eurovoc_sparql_df$domain_label)
    eurovoc_sparql_df$domain_label <- gsub("@en","",eurovoc_sparql_df$domain_label)
    return(eurovoc_sparql_df)
}

#' @export
get_eurovoc_data <- function(request = FALSE){
    if (request){
        eurovoc_sparql_df <- request_eurovoc_sparql()
        eurovoc_sparql_df <- clean_eurovoc_sparql_df(eurovoc_sparql_df)
        return(eurovoc_sparql_df)
    }else{
        # get from stored ata
        return(eurovoc_sparql)
    }

}

#' @export
create_eurovoc_domain_dummy_variables <- function(df, request = FALSE, rm_raw = FALSE){
    eurovoc_sparql <- get_eurovoc_data(request=request)
    eurovoc_raw_varnames <- names(df)[grep("eurovoc_[0-9]+$", names(df))]

    # subset df to eurovoc variables only
    df_eurovoc <- df[,c("procedure_id", eurovoc_raw_varnames)]

    # Convert eurovoc df to long to remove NAs
    df_eurovoc <- reshape2::melt(df_eurovoc, id.vars="procedure_id")
    df_eurovoc <- df_eurovoc[!is.na(df_eurovoc$value),]
    df_eurovoc$variable <- NULL


    # Merge Eurovoc data from SPARQL request
    eurovoc_merged1 <- merge(df_eurovoc, eurovoc_sparql, by.x = "value", by.y="eurovoc_id")
    eurovoc2 <- subset(df_eurovoc, !(value %in% eurovoc_merged1$value))
    eurovoc_merged2 <- merge(eurovoc2, unique(eurovoc_sparql[-c(1:2)]), by.x = "value", by.y="mt_id")
    eurovoc3 <- subset(eurovoc2, !(value %in% eurovoc_merged2$value))
    eurovoc_merged3 <- merge(eurovoc3, unique(eurovoc_sparql[-c(1:5)]), by.x = "value", by.y="domain_id")
    eurovoc_merged <- dplyr::bind_rows(eurovoc_merged1, eurovoc_merged2, eurovoc_merged3)

    # Convert to wide and set TRUE/FALSE dummies if more than 0 matches of domain
    eurovoc_w <- reshape2::dcast(eurovoc_merged,procedure_id~domain_label, fun.aggregate = function(x) ifelse(length(x)>0,TRUE,FALSE))

    # rename vars
    names(eurovoc_w) <- sapply(names(eurovoc_w), function(x){if(grepl("^[0-9]", x)) paste0("eurovoc_", gsub("[ ,]+", "_", tolower(x))) else x}) # lowercase, all space etc to _
    names(eurovoc_w) <- sapply(names(eurovoc_w), function(x){if(grepl("^eurovoc", x)) paste0(gsub("-", "", tolower(x), perl=TRUE)) else x}) # remove -

    names(eurovoc_w) <- sapply(names(eurovoc_w), function(x){if(grepl("^eurovoc", x)) paste0(gsub("(?<=[a-z])_[_a-z]+", "", tolower(x), perl=TRUE)) else x}) # shorten names

    if(rm_raw){
        df <- df[, !(names(df)%in% eurovoc_raw_varnames)]
    }

    # Merge Eurovoc to df and return
    merge(df, eurovoc_w, by ="procedure_id", all.x=TRUE)

}

