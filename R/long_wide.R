#' @export
is_long <- function(df){
    if(NROW(df$procedure_id)>NROW(unique(df$procedure_id))){
        return(TRUE)
    } else{
        return(FALSE)
    }
}

#' @export
long <- function(df, event_names = list(), doc_names = list()){

    # @TODO: event name should be named list of "wide-eventvarname": long name
    # @TODO: doc name should be named list of "wide-eventvarname": wide-docvarname (empty if main)

    ## convert all variables to character to later wide to long transformation
    df[] <- lapply(df, as.character)

    df_docs <- df[, c("procedure_id", grep("^doc_", names(df), value = TRUE))]
    df_events <- df[, c("procedure_id", grep("^e_", names(df), value = TRUE))]
    df_procs <- df[,c("procedure_id", names(df)[!names(df) %in% c(names(df_docs), names(df_events))])]

    # 1. doc to long, events to long

    ## DOCS ----

    ## transform from wide to long
    df_docs_long <- tidyr::pivot_longer(df_docs,
        cols = tidyselect::starts_with("doc"),
        names_to = c("doc", ".value"),
        names_pattern = paste0("doc_[_]*((?:[a-zA-Z_0-9]+?[A-Z]+?[a-zA-Z_0-9]+?)(?=__)|(?:[a-z]*?)(?=_))_(?:_(?!bad_formatting|euplexcy)){0,1}(.*)"), # old: doc_[_]*((?:[a-zA-Z_0-9]*)(?=__)|(?:[a-z]*)(?=_))_[_]{0,1}(.*)
        values_drop_na = TRUE)

    ## set prefix for correct variable names
    names(df_docs_long)[-c(1,2)] <- sapply(names(df_docs_long)[-c(1,2)], function(x) paste0("doc_", x))

    ## EVENTS ----

    ## transform from wide to long
    df_events_long <- tidyr::pivot_longer(df_events,
                                        cols = tidyselect::starts_with("e_"),
                                        names_to = c("event", ".value"),
                                        names_pattern = paste0("e_[_]*((?:[a-zA-Z_0-9]+?[A-Z]+?[a-zA-Z_0-9]+?)(?=__)|(?:[a-z]*)(?=_))_[_]{0,1}(.*)"), #old: e_[_]*((?:[a-zA-Z_0-9]*)(?=__)|(?:[a-z]*)(?=_))_[_]{0,1}(.*)
                                        values_drop_na = TRUE)

    ## rename, set prefix
    names(df_events_long)[-c(1,2)] <- sapply(names(df_events_long)[-c(1,2)], function(x) paste0("e_", x))

    if (any(grep("_[0-9]+$", names(df_procs)))){

        # proc to long
        df_procs_long <- tidyr::pivot_longer(df_procs,
                                            cols = tidyselect::matches("_[0-9]+$"),
                                            names_to = c(".value"),
                                            names_pattern = "(.*)_[0-9]+$",
                                            values_drop_na = TRUE,
                                            names_repair="unique")
    }else {
        df_procs_long <- df_procs
    }

    rm(df_docs, df_events, df_procs)

    # 2. merge doc to event
    df_eventsdocs_long <- merge(df_events_long, df_docs_long, all = TRUE)

    # 3. merge docsevents to proc_long
    df_procseventsdocs_long <- merge(df_procs_long, df_eventsdocs_long, all.x=TRUE)

    df_long <- df_procseventsdocs_long

    rm(df_procs_long, df_eventsdocs_long, df_procseventsdocs_long)


    # 5. reformat variables
    df_long <- utils::type.convert(df_long, as.is = TRUE)
    df_long <- reformat_date_variables(df_long)

    df_long
}


#wide <- function(df_long){
#
#    df_wide <- tidyr::pivot_wider(df_long, tidyr::matches(""))
#
#}
