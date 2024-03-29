library(dplyr)

complexity_varnames_noncore <-
    sort(
        c(
            "flesch_kincaid_grade_level",
            "lix",
            "dale_chall",
            "flesch_kincaid_reading_ease",
            "smog",
            "coleman_liau_index",
            "forcast",
            'avg_article_depth',
            'flesch'
        )
    )

complexity_varnames_core <-
    sort(
        c(
            "lix",
            "ref_",
            "structural_size",
            "word_entropy",
            "citation_count",
            "citations",
            "word_count",
            "words",
            "word_entropy_l",
            "article_count",
            "articles",
            "recital_count",
            "recitals",
            "structural_size",
            "struct_size",
            "word_count_noannex",
            "words_noannex",
            "avg_depth"
        )
    )

complexity_varnames <-
    sort(unique(c(
        complexity_varnames_noncore, complexity_varnames_core
    )))

df_complexity_varnames <-
    function(df,
             complexity_vars = "all",
             doc = "all") {
        if (complexity_vars == "all") {
            complexity_varnames <- complexity_varnames
        } else if (complexity_vars == "core") {
            complexity_varnames <- complexity_varnames_core
        } else if (nchar(complexity_vars) > 0) {
            complexity_varnames <-
                grep(complexity_vars, complexity_varnames, value = TRUE)
        }

        complexity_varnames_df <-
            unique(unlist(sapply(complexity_varnames, function(x)
                grep(paste0("doc_", ".*", x), names(df), value = TRUE))))

        if (doc == "all") {
            return(complexity_varnames_df)
        } else{
            return(grep(
                paste0("^doc_[_]*", doc),
                complexity_varnames_df,
                value = TRUE
            ))
        }
    }

df_doc_types <-
    function(df,
             add_underscore_to_raw = FALSE,
             underscore = "_") {
        type_matches <-
            stringr::str_match(names(df),
                               "doc_[_]*((?:[a-zA-Z_0-9]*)(?=__)|(?:[a-z]*)(?=_))_[_]*(.*)")[, 2]
        type_matches <- unique(type_matches[!is.na(type_matches)])
        types <- gsub("_$", "", type_matches)
        if (add_underscore_to_raw) {
            for (i in 1:NROW(types)) {
                type <- types[i]
                if (grepl("_", type)) {
                    types[i] <- paste0(underscore, type, underscore)
                }
            }
        }
        types
    }

create_complexity_variables <- function(df) {
    # Relative ref vars
    if (!is_long(df)) {
        doc_types <-
            df_doc_types(df,
                         add_underscore_to_raw = TRUE,
                         underscore = "__")
        for (i in 1:NROW(doc_types)) {
            doc_type <- doc_types[i]
            df[, paste0("doc_", doc_type , "_ref_ext_enacting_rel")] <-
                df[, grep(paste0(doc_type, ".*", "ref_ext_enacting$"),
                          names(df))] / df[, grep(paste0(doc_type, ".*", "(article_count|articles)$"),
                                                  names(df))]
            df[, paste0("doc_", doc_type , "_ref_int_enacting_rel")] <-
                df[, grep(paste0(doc_type, ".*", "ref_int_enacting$"),
                          names(df))] / df[, grep(paste0(doc_type, ".*", "(article_count|articles)$"),
                                                  names(df))]
        }
    } else{
        df$doc_ref_ext_enacting_rel <-
            df[, grep("ref_ext_enacting$", names(df))] / df[, grep("(article_count|articles)$", names(df))]
        df$doc_ref_int_enacting_rel <-
            df[, grep("ref_int_enacting$", names(df))] / df[, grep("(article_count|articles)$", names(df))]

    }
    df
}

# Functions to wrangle raw euplexdb datasets

#' @export
reformat_missing_data <- function(df) {
    df[df == ""] <- NA

    df
}


#' @export
reformat_date_variables <- function(df) {

    date_vars <- grep("_date$", names(df), value=TRUE)

    if (NROW(date_vars)==1){
        df[, date_vars] <- lubridate::as_date(df[, date_vars])
    }
    else {
        for (i in 1:NROW(date_vars)){
            var <- date_vars[i]

            df[, var] <-  lubridate::as_date(df[, var])

        }
    }

    df
}

#' @export
reformat_logical_variables <- function(df) {
    for (col in 1:NCOL(df)) {
        if (!grepl("_date$", names(df)[col])) {
            if (all((is.character(df[, col])) &
                    (
                        is.na(df[, col]) |
                        tolower(df[, col]) == "true" |
                        tolower(df[, col]) == "false" | df[, col] == ""
                    )
            )) {
                df[, col] <- as.logical(df[, col])
            }
        }
    }

    df
}


#' @export
create_named_procedure_event_variables <- function(df, event_codes = c(), event_name  = c()) {
        # @TODO
        # merge variables other than legal_date for multi-event-code events (e.g. final)

        # Codes must be supplied in the reverse order they appear in the legislative process (last possible first)!

        if ('procedure_id' %in% names(df)) {
            merge_by = "procedure_id"
        } else{
            merge_by = "procedure_reference_0"
        }

        # create a dataframe containing only the variables of interest
        df_events <-
            df[, c(merge_by, unlist(sapply(event_codes, function(x)
                grep(x, names(df), value = TRUE), simplify = "array")))]


        # some last corrections to allow for errorless bind_rows below
        df_events[, grep("legal_basis", names(df_events), value = TRUE)] <-
            apply(df_events[, grep("legal_basis", names(df_events), value = TRUE)], 2, as.character)

        rows <- list()
        j <- 1

        # for every row
        for (i in 1:NROW(df_events)) {
            df_row <- df_events[i, ]
            for (code in event_codes) {
                legal_date <- df_events[i, paste0("e_", code, "_legal_date")]
                # check if date/document ref available
                if (NROW(legal_date) == 1 & !gtools::invalid(legal_date) & !is.na(legal_date)) {
                    # convert all vars with the resp. code for that row
                    names(df_row) <-
                        gsub(code, event_name, names(df_row))
                    df_row$e_code <- code
                    # add _vars_ from df_row to df_final
                    rows[[j]] <-
                        df_row[, c(merge_by,
                                   grep(event_name, names(df_row), value = TRUE))]
                    j <- j + 1
                    # break event code for-loop to continue with next row
                    break
                }
            }
        }

        df_rows <- dplyr::bind_rows(rows)

        # return warning if merge variable is not unique
        if (any(duplicated(df[[merge_by]]))) {
            warning(
                paste(
                    "Named event results are not reliable: variable",
                    merge_by,
                    "not unique across rows! Please remove duplicates or use a newer dataset version."
                )
            )
        }

        merge(df, df_rows, all.x = TRUE, by = merge_by)
}

#' @export
remove_na_variables <- function(df) {
    df[, colSums(is.na(df)) != nrow(df)]
}

#' @export
remove_raw_variables <- function(df) {
    df[, !grepl("^doc__|^e__)", names(df))]
}

#' @export
remove_variables <- function(df, varnames_regex = NA){
    # varnames to be removed
    varnames_rm <- c(grep(varnames_regex, names(df), value=TRUE))
    df <- df[, !(names(df) %in% varnames_rm)]

    df

}

#' @export
remove_extra_variables <- function(df, varnames_regex = NA) {

    df <- df %>%
        remove_na_variables() %>%
        remove_raw_variables() %>%
        remove_variables(varnames_regex = varnames_regex)

    df
}


#' @export
order_variables <- function(df) {
    # 1. procedure id
    # 2. procedure reference
    # 3. everything else
    # 4. event
    # 5. doc-event

    df
}

#' @export
apply_correction_data <- function(df) {
    df[which(df$doc_proposal_uri_celex == "52008PC0458"), ]$doc_proposal_procedure_subtype <-
        "Recast"
    df[which(df$doc_proposal_uri_celex == "51997PC0368"), ]$doc_proposal__bad_formatting <-
        TRUE

    df

}

#' @export
set_bad_formatting_observations_na <- function(df, newline=FALSE, newline_ratio_cutoff=0.003, doc='all') {

        # long format handling (no doc in varname)
        doc_complexity_vars_all <- df_complexity_varnames(df, complexity_vars = "all")

        bad_formatting_varnames <-
            grep("bad_formatting$", names(df), value = TRUE)

        if(!doc=="all" & !is_long(df)){
            bad_formatting_varnames <- bad_formatting_varnames[grepl(doc, bad_formatting_varnames)]
        }

            for (i in 1:NROW(bad_formatting_varnames)) {

                bad_formatting_var <- bad_formatting_varnames[i]
                bad_formatting_reason_var <- paste0(bad_formatting_varnames[i], '_reason')

                if(!is_long(df)){
                    var_doc <- gsub('(doc_)(.+?)(_.*)', '\\2', bad_formatting_var, perl=TRUE)
                    doc_complexity_vars <- doc_complexity_vars_all[grepl(var_doc, doc_complexity_vars_all)]
                } else{
                    doc_complexity_vars <- doc_complexity_vars_all
                }


                if (newline){

                    df[which(df[, bad_formatting_var]), doc_complexity_vars] <-
                        NA
                } else {

                    df[which(df[, bad_formatting_var] & df[, bad_formatting_reason_var]!='newline;'), doc_complexity_vars] <-
                        NA
                }
            }


        # Newline cutoff

        newline_ratio_varnames <- grep("newline_ratio$", names(df), value = TRUE)

        if(!doc=="all" & !is_long(df)){
            newline_ratio_varnames <- newline_ratio_varnames[grepl(doc, newline_ratio_varnames)]
        }

        if (NROW(newline_ratio_varnames)>0){
            for (k in 1:NROW(newline_ratio_varnames)){

                if(!is_long(df)){
                    var_doc <- gsub('(doc_)(.+?)(_.*)', '\\2', newline_ratio_varnames[k], perl=TRUE)
                    doc_complexity_vars <- doc_complexity_vars_all[grepl(var_doc, doc_complexity_vars_all)]
                } else{
                    doc_complexity_vars <- doc_complexity_vars_all
                }

                df[which(df[, newline_ratio_varnames[k]]<newline_ratio_cutoff), doc_complexity_vars] <-
                NA

            }
        }


    df

}

#' @export
set_recast_observations_na <- function(df, doc='proposal') {

    leg_proc_subtype_varnames <-
        grep("procedure_subtype$", names(df), value = TRUE)

    if(!doc=='all'){
        leg_proc_subtype_varnames <- leg_proc_subtype_varnames[grepl(doc, leg_proc_subtype_varnames)]
    }


    for (i in 1:NROW(leg_proc_subtype_varnames)) {
        doc_complexity_vars <-
            df_complexity_varnames(df, complexity_vars = "all")

        if(!doc=='all'){
            doc_complexity_vars <- doc_complexity_vars[grepl(doc, doc_complexity_vars)]
        }

        df[which(df[, leg_proc_subtype_varnames[i]] == "Recast"), doc_complexity_vars] <-
            NA
    }

    df

}

#' @export
keep_only <- function(df, keep_events = c("proposal", "final"), keep_docs = c("proposal"), long = FALSE) {
        if (!long) {
            event_varnames <- grep("^e_", names(df), value = TRUE)
            keep_event_varnames <- c()
            for (i in 1:NROW(keep_events)) {
                keep_event_varnames <-
                    c(keep_event_varnames,
                      grep(
                          paste0("^e_", keep_events[i]),
                          event_varnames,
                          value = TRUE
                      ))
            }
            drop_event_varnames <-
                event_varnames[!event_varnames %in% keep_event_varnames]

            doc_varnames <- grep("^doc_", names(df), value = TRUE)
            keep_doc_varnames <- c()
            for (i in 1:NROW(keep_docs)) {
                keep_doc_varnames <-
                    c(keep_doc_varnames,
                      grep(paste0("doc_", keep_docs[i]), doc_varnames, value = TRUE))
            }
            drop_doc_varnames <-
                doc_varnames[!doc_varnames %in% keep_doc_varnames]

            df <-
                df[, !names(df) %in% c(drop_event_varnames, drop_doc_varnames)]

        }

        df
    }



#' @export
create_complete_cases_variable <-
    function(df, vars = "complexity_core", doc = "all") {
        if (is_long(df)) {
            warning("This function currently only detects complete cases for wide-format data!")
        }

        if (vars == "all") {
            df$complete <- stats::complete.cases(df)

        }

        if (vars == "complexity_core") {
            if (doc == "all") {
                vars_check_list <-
                    df_complexity_varnames(df, complexity_vars = "core", doc = doc)
                df$complete_complexity <-
                    stats::complete.cases(df[, vars_check_list])

            } else {
                vars_check_list <-
                    df_complexity_varnames(df, complexity_vars = "core", doc = doc)
                df[, paste0("doc_", doc, "_complete_complexity")] <-
                    stats::complete.cases(df[, vars_check_list])
            }


        }

        df
    }


#' @export
create_title_variable <-
    function(df,
             lang = "en",
             type = "",
             guess_title_lang = FALSE,
             rm_raw = FALSE) {
        title_varnames <-
            grep(paste0("^", type, "title"), names(df), value = TRUE)

        title_nolang_varnames <-
            grep(paste0("^", type, "title_[0-9]+$"), title_varnames, value = TRUE)
        title_lang_varname <-
            grep(paste0("_", lang), title_varnames, value = TRUE)

        if (NROW(title_lang_varname) != 1) {
            if (NROW(title_lang_varname) > 1) {
                stop("More than one language variable found for selected language .")
            } else if (NROW(title_lang_varname) > 1) {
                stop("No language variable found.")
            }
        }

        # select title language var
        df[, paste0(type, "title")] <- apply(df, 1, function(x) {
            if (nchar(x[title_lang_varname]) < 2 |
                is.na(x[title_lang_varname])) {
                row_titles_nolang <- sapply(title_nolang_varnames, function(y) {
                    if (!is.na(x[y]) & nchar(x[y])> 2 & !is.null(x[y])) {
                        return(x[y])
                    } else{
                        return(NA)
                    }
                })

                row_titles_nolang <-
                    row_titles_nolang[!is.na(row_titles_nolang)]

                # if there are title vars with no lang specifier
                if (!NROW(row_titles_nolang) == 0) {
                    # sort by length and select longest
                    row_titles_nolang<- row_titles_nolang[order(nchar(row_titles_nolang))]
                    if (guess_title_lang) {
                        # @TODO guess lang
                        ## convert to dataframe, add lang var and fill, then select matching lang
                        warning("Language guessing for non-language specified title variables is not implemented yet!")

                        return(row_titles_nolang[1])
                    } else{
                        return(row_titles_nolang[1])
                    }
                } else{
                    return(NA)
                }

            } else{
                return(x[title_lang_varname])
            }
        })

        # remove raw variables
        if (rm_raw) {
            df <- df[, !(names(df) %in% title_varnames)]
        }

        df

    }

#' @export
create_unified_title_variable <-
    function(df,
             lang = "en",
             guess_title_lang = FALSE, guess_title_lang_conflict = TRUE,
             rm_raw = FALSE) {

        # create title and dossier title vars
        df <- df %>%
            create_title_variable(
                lang = lang,
                type = "",
                guess_title_lang = FALSE,
                rm_raw = rm_raw) %>%
            create_title_variable(
                lang = lang,
                type = "dossier_",
                guess_title_lang = FALSE,
                rm_raw = rm_raw)

        # check if there are differences and if so why
        df[, "title"] <- apply(df, 1, function(x) {
            if (all(c(is.na(x["title"]), is.na(x["dossier_title"]))) | x["title"] == x["dossier_title"]) {
                return(x["title"])
            } else if(any(c(is.na(x["title"]), is.na(x["dossier_title"])))){
                if(is.na(x["title"]) & !is.na(x["dossier_title"])){
                    return(x['dossier_title'])
                } else if(!is.na(x["title"]) & is.na(x["dossier_title"])) {
                    return(x['title'])
                } else {
                    return (NA)
                }
            }
            else {
                if (guess_title_lang_conflict) {
                    # try to detect language
                    title_lang_guess = cld3::detect_language(c(x[["title"]]))
                    dossier_title_lang_guess = cld3::detect_language(c(x[["dossier_title"]]))

                    # if title is en, choose title
                    if (!is.na(title_lang_guess) &
                        title_lang_guess[1] == "en") {
                        return(x['title'])
                    } else if (!is.na(dossier_title_lang_guess) &
                               dossier_title_lang_guess[1] == "en") {
                        return(x['dossier_title'])
                    } else {
                        # if not en, choose title
                        return(x['title'])

                    }

                } else{
                    return(x['title'])
                }


            }
        })

        if (rm_raw) {
            df['dossier_title'] <- NULL
        }

        df

}


#' @export
rename_variables <- function(df, rm_lone_num_suffix = TRUE) {
    df <- df %>%
        rename_count_variables() %>%
        rename_dot_variables() %>%
        shorten_varnames() %>%
        {
            if (rm_lone_num_suffix)
                rename_num_suffix_variables(.)
            else
                .
        }
    df
}

#' @export
rename_num_suffix_variables <- function(df) {
    num_suffix_names <- grep("_[0-9]+$", names(df), value = TRUE)
    num_suffix_names <- gsub("_[0-9]+$", "", num_suffix_names)
    num_suffix_names_tab <- table(num_suffix_names)
    lone_num_suffix_names <-
        sapply(names(num_suffix_names_tab), function(x) {
            if (num_suffix_names_tab[x] == 1)
                x
            else
                NA
        })
    lone_num_suffix_names <-
        lone_num_suffix_names[!is.na(lone_num_suffix_names)]

    if (NROW(lone_num_suffix_names) > 0) {
        for (i in 1:NROW(lone_num_suffix_names)) {
            varname <- lone_num_suffix_names[i]
            names(df)[(grepl(paste0(varname, "_[0-9]+$"), names(df)))] <-
                varname
        }
    }

    df
}

#' @export
rename_count_variables <- function(df) {
    names(df) <- gsub("_count", "s", names(df))
    df
}

#' @export
rename_dot_variables <- function(df) {
    names(df) <- gsub("\\.", "_", names(df), perl = TRUE)
    df
}

#' @export
shorten_varnames <- function(df) {
    # structural to struct
    names(df) <- gsub("structural_", "struct_", names(df))

    # proposal to prop
    names(df) <-
        gsub("legislative_instrument", "leg_instr", names(df))

    # remove kincaid form flesch_kincaid
    names(df) <- gsub("kincaid_", "", names(df))

    names(df) <- gsub("responsible", "resp", names(df))
    names(df) <- gsub("institution", "inst", names(df))
    names(df) <- gsub("corporate", "corp", names(df))

    df
}

#' @export
subset_by_date <-
    function(df, proposal_dates, orders = c("ymd", "dmy")) {
        proposal_min_date <- NA
        proposal_max_date <- NA

        if (is_long(df)) {
            stop("Subsetting currently only works for wide-format data!")
        }

        if (!missing(proposal_dates)) {
            if (is.vector(proposal_dates)) {
                if (NROW(proposal_dates) == 1) {
                    proposal_min_date <-
                        lubridate::parse_date_time(proposal_dates[1], orders = orders)
                } else{
                    proposal_date <-
                        sapply(proposal_dates, function(x)
                            ifelse((nchar(x) < 2 |
                                        is.na(x)),
                                   lubridate::today(),
                                   lubridate::parse_date_time(x, orders = orders)
                            ))
                    proposal_min_date <-
                        lubridate::parse_date_time(proposal_dates[1], orders = orders)
                    proposal_max_date <-
                        lubridate::parse_date_time(proposal_dates[2], orders = orders)
                }
            } else{
                proposal_min_date = lubridate::parse_date_time(proposal_dates, orders =
                                                                   orders)
            }

            if (is.na(proposal_max_date)) {
                proposal_max_date <- lubridate::today(tzone = "UTC")
            }

            # get varname of proposal legal date var
            proposal_date_varnames <-
                grep("^e(?:(_proposal_)|(__ADP_byCOM__)|(_))legal_date$",
                     names(df),
                     value = TRUE)

            # if multiple select the one with "proposal" in it
            if (NROW(proposal_date_varnames) > 1) {
                for (i in 1:NROW(proposal_date_varnames)) {
                    proposal_date_varname <- proposal_date_varnames[i]
                    if ("proposal" %in% proposal_date_varname) {
                        break
                    }
                }
            }

            # subset by proposal date
            df <- subset(df, (df[, proposal_date_varname] >= proposal_min_date & df[, proposal_date_varname] <= proposal_max_date))
        }

        # @TODO: final dates

        df
    }


#' @export
group_vars_to_list_var <-
    function(df, varname_regex, new_varname, separator="###", rm_old = TRUE){

        varnames_old <- grep(varname_regex, names(df), value=TRUE, perl=TRUE)

        df[, new_varname] <- apply(df, 1, function(x){
            paste(x[varnames_old][nzchar(x[varnames_old]) & !is.na(x[varnames_old])], collapse = separator)
        })

        df[, new_varname][nchar(df[, new_varname])==0] <- NA

        if(rm_old){
            df <- remove_variables(df, varnames_regex = varname_regex)
        }

        df
    }



#' @export
create_eurlex_search_variable <-
    function(df, api_base_url = "https://eurlexapi.mxhg.org/procedure/", max_retries = 3){

        # call api and get cases that show up in eurlex search (wrap in try catch)
        tryCatch({
            eurlex_search <- get_eurlex_search_result_from_api(api_base_url, endpoint = "search", max_retries = max_retries)

            eurlex_search_references <- data.frame(`procedure_id` = eurlex_search[, 'reference'])
            eurlex_search_references$eurlex_search <- TRUE

            df <- merge(df, eurlex_search_references, by.x = 'procedure_id', by.y = 'procedure_id', all.x=TRUE)

            df$eurlex_search <- ifelse(is.na(df$eurlex_search), FALSE, TRUE)

            return(df)

        },
            error = function(e){
                warning('Could not create EURLEX search variable, proceeding without!')

                return(df)
            }
        )


    }


get_eurlex_search_result_from_api <-
    function(api_base_url = "https://eurlexapi.mxhg.org/procedure/", endpoint = "search", max_retries = 3){

        # request API
        res <- httr::RETRY(
            "GET",
            url = paste0(api_base_url, endpoint),
            times = max_retries
        )

        # check the response status, throw error if failed
        httr::stop_for_status(res)

        # convert res to json
        eurlex_search <- jsonlite::fromJSON(rawToChar(res$content))

        eurlex_search

    }
