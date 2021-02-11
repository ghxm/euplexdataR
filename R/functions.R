library(dplyr)

complexity_varnames <- sort(c("flesch_kincaid_grade_level", "lix", "ref_ext_enacting", "structural_size_enacting", "word_entropy", "citation_count", "dale_chall", "flesch_kincaid_reading_ease", "ref_int_enacting", "smog", "word_count", "word_entropy_l", "article_count", "coleman_liau_index", "forcast", "recital_count", "structural_size", "word_count_noannex", "avg_depth"))

complexity_varnames_core <- sort(c("lix", "ref_", "structural_size", "word_entropy", "citation_count", "word_count", "word_entropy_l", "article_count", "recital_count", "structural_size", "word_count_noannex", "avg_depth"))

# Functions to wrangle raw euplexdb datasets

#' @export
reformat_missing_data <- function(df){
    df[df==""] <- NA

    df
}


#' @export
reformat_date_variables <- function(df){
    df[,grep("_date$", names(df))] <- lapply(df[,grep("_date$", names(df))], function(x) lubridate::as_date(x))

    df
}

#' @export
reformat_logical_variables <- function(df){

    for(col in 1:NCOL(df)){
        if (!grepl("_date$", names(df)[col])){
            if (all((is.character(df[,col]))&(is.na(df[,col]) | tolower(df[,col])=="true" | tolower(df[,col])=="false" | df[,col]==""))){
                df[,col] <- as.logical(df[,col])
            }
        }
    }

    df
}

#' @export
create_procedure_type_dummies <- function(df){
    # create procedure type dummies
    df$cod <- ifelse(df$procedure_type_0=="COD", 1, 0)
    df$cns  <- ifelse(df$procedure_type_0=="CNS", 1, 0)

    df
}

#' @export
create_procedure_type_dummies <- function(df){
    # create procedure_type dummies
    df$regulation <- ifelse(df$doc__ADP_byCOM__legislative_instrument=="Regulation", 1, 0)
    df$directive <- ifelse(df$doc__ADP_byCOM__legislative_instrument=="Directive", 1, 0)
    df$decision <- ifelse(df$doc__ADP_byCOM__legislative_instrument== "Decision", 1, 0)

    df
}

#' @export
create_named_procedure_event_variables <- function(df, event_codes = c(), event_name  = c()){

    # @TODO
    # merge variables other dan legal_date for multi-event-code events (e.g. final)

    # Codes must be supplied in the reverse order they appear in the legislative process (last possible first)!

    if ('procedure_id' %in% names(df)){
        merge_by = "procedure_id"
    }else{
        merge_by = "procedure_reference_0"
    }

    # create a dataframe containing only the variables of interest
    df_events <- df[, c(merge_by, unlist(sapply(event_codes, function(x) grep(x,names(df), value=TRUE), simplify="array")))]


    # some last corrections to allow for errorless bind_rows below
    df_events[, grep("legal_basis", names(df_events), value = TRUE)] <- apply(df_events[, grep("legal_basis", names(df_events), value = TRUE)], 2, as.character)

    rows <- list()
    j <- 1


    # for every row
    for(i in 1:NROW(df_events)){
        df_row <- df_events[i,]
        for(code in event_codes){
            legal_date <- df_events[i, paste0("e_", code, "_legal_date")]
            # check if date/document ref available
            if (!gtools::invalid(legal_date)){
                # convert all vars with the resp. code for that row
                names(df_row) <- gsub(code, event_name, names(df_row))
                df_row$e_code <- code
                # add _vars_ from df_row to df_final
                rows[[j]] <- df_row[,c(merge_by, grep(event_name, names(df_row), value = TRUE))]
                j <- j + 1
                # break event code for-loop to continue with next row
                break
            }
        }
    }

    df_rows <- dplyr::bind_rows(rows)

    # return warning if merge variable is not unique
    if(any(duplicated(df[[merge_by]]))){
        warning(paste("Named event results are not reliable: variable", merge_by, "not unique across rows! Please remove duplicates or use a newer dataset version."))
    }

    merge(df, df_rows, all.x=TRUE, by = merge_by)
}

#' @export
remove_na_variables <- function(df){
    df[, colSums(is.na(df)) != nrow(df)]
}

#' @export
remove_raw_variables <- function(df){
    df[,!grepl("^doc__|e__", names(df))]
}

#' @export
remove_extra_variables <- function(df){
    df <- df %>%
        remove_na_variables() %>%
        remove_raw_variables()

    df
}


#' @export
order_variables <- function(df){

    # 1. procedure id
    # 2. procedure reference
    # 3. everything else
    # 4. event
    # 5. doc-event

    df
}

#' @export
apply_correction_data <- function(df){

    df[which(df$doc_proposal_uri_celex == "52008PC0458"),]$doc_proposal_procedure_subtype <- "Recast"
    df[which(df$doc_proposal_uri_celex == "51997PC0368"),]$doc_proposal__bad_formatting <- TRUE

    df


}

#' @export
set_bad_formatting_observations_na <- function(df){

    bad_formatting_varnames <- grep("bad_formatting$", names(df), value=TRUE)

    for(i in 1:NROW(bad_formatting_varnames)){
        varname_prefix <- strsplit(bad_formatting_varnames[i], "_bad_formatting")[[1]]
        doc_complexity_vars <- paste0(varname_prefix, complexity_varnames)
        df[which(df[, bad_formatting_varnames[i]]), doc_complexity_vars] <- NA
    }

    df

}

#' @export
set_recast_observations_na <- function(df){

    leg_proc_subtype_varnames <- grep("procedure_subtype$", names(df), value=TRUE)


    for(i in 1:NROW(leg_proc_subtype_varnames)){
        varname_prefix <- strsplit(leg_proc_subtype_varnames[i], "procedure_subtype")[[1]]
        doc_complexity_vars <- paste0(varname_prefix, complexity_varnames)
        df[which(df[, leg_proc_subtype_varnames[i]]=="Recast"), doc_complexity_vars] <- NA
    }

    df

}

#' @export
keep_only<- function(df, keep_events = c("proposal", "final"),  keep_docs = c("proposal"), long = FALSE){

    if(!long){
        event_varnames <- grep("^e_", names(df), value = TRUE)
        keep_event_varnames <- c()
        for (i in 1:NROW(keep_events)){
            keep_event_varnames <- c(keep_event_varnames, grep(paste0("^e_", keep_events[i]), event_varnames, value = TRUE))
        }
        drop_event_varnames <- event_varnames[!event_varnames %in% keep_event_varnames]

        doc_varnames <- grep("^doc_", names(df), value = TRUE)
        keep_doc_varnames <- c()
        for (i in 1:NROW(keep_docs)){
            keep_doc_varnames <- c(keep_doc_varnames, grep(paste0("doc_", keep_docs[i]), doc_varnames, value = TRUE))
        }
        drop_doc_varnames <- doc_varnames[!doc_varnames %in% keep_doc_varnames]

        df <- df[,!names(df) %in% c(drop_event_varnames, drop_doc_varnames)]

    }

    df
}



#' @export
create_complete_cases_variable <- function(df, vars = "complexity", doc = "all"){

    if(is_long(df)){
        warning("This function currently only detects complete cases for wide-format data!")
    }

    if(vars=="all"){
        df$complete <- complete.cases(df)

    }

    if(vars=="complexity"){
        if(doc=="all"){
        vars_check_list <- unique(unlist(sapply(complexity_varnames, function(x) grep(x, names(df), value=TRUE))))
        df$complete_complexity <- complete.cases(df[, vars_check_list])

        }else{
            vars_check_list <- unique(unlist(sapply(complexity_varnames, function(x) grep(paste0(doc, ".*", x), names(df), value=TRUE))))
            df[,paste0("doc_", doc, "_complete_complexity")] <- complete.cases(df[, vars_check_list])
        }


    }

    df
}
