print_header <- function(text, simple = FALSE){
    cat("\n")
    if(!simple){
        cat("==========================")
        cat("\n")
    }
    cat(text)
    cat("\n")
    cat("------------------")
    cat("\n")
}

#' @export
inspect_complexity_data <- function(df, probs = c(0.999,1), head = NA, cutoffs = c(NA,NA), decreasing = TRUE, docs = c("proposal"), complexity_vars = "core", display = c(), amending = "exclude", complete_only = TRUE){

    vars_inspect <- df_complexity_varnames(df,complexity_vars = complexity_vars)
    doc_types <- df_doc_types(df)

    if(!is.vector(docs)){
        docs = c(docs)
    }


    doc_types <- doc_types[doc_types %in% docs]

    for(i in 1:NROW(docs)){
        doc <- doc_types[i]
        df_inspect <- df
        # subset complete only
        if(complete_only){
            df_inspect <- df[which(df[,paste0("doc_", doc, "_complete_complexity")]),]
        }

        # subset amending
        if(amending=="exclude"){
            df_inspect <- df_inspect[which(!df_inspect[,paste0("doc_", doc, "_amending")]),]
        }

        for(j in 1:NROW(vars_inspect)){
            var <- vars_inspect[j]

            print_header(var, simple = TRUE)

            # sort by var
            df_inspect <- df_inspect[order(df_inspect[,var], decreasing = decreasing, na.last = TRUE),]

            if(is.na(head)){
                if(any(is.na(cutoffs))){
                    # calculate quantiles
                    cut <- quantile(df_inspect[, var], probs = probs, na.rm =TRUE)
                } else {
                    cut <- cutoffs
                }
            df_inspect_cut <- df_inspect[which(df_inspect[,var]>=cut[1] & df_inspect[,var]<=cut[2]),]
            }else {
                df_inspect_cut <- head(df_inspect, n = head)
            }

            print(observations(df_inspect_cut, display = c(display, var), docs = c(doc), events = c(doc)))

        }
    }

    # loop through complexity vars

    # display data between cutoff sorted



}


#' @export
observations <- function(df, display = c(), events = c("proposal"), docs = c("proposal")){

    always_display_events_varnames <- c("legal_date")
    always_display_events <- sapply(events, function(x) paste0("e_", x, "_", always_display_events_varnames))

    always_display_docs_varnames <- c("uri_celex", "procedure_subtype")
    always_display_docs <- sapply(docs, function(x) paste0("doc_", x, "_", always_display_docs_varnames))

    always_display <- c("procedure_id", always_display_docs, always_display_events)

    if(NROW(display)>0){
        vars <- c(always_display, display)
    }else{
        vars <- always_display
    }

    df[, vars]

}

#' @export
data_summary_report <- function(df){

    print_header("euplexdb dataset")

    print_header("SUMMARY", simple = TRUE)


    cat(paste("Number of procedures:", NROW(unique(df$procedure_id))))
    cat("\n")

    cat(paste("Proposal dates:", min(df$e_proposal_legal_date, na.rm =TRUE), "--", max(df$e_proposal_legal_date, na.rm =TRUE)))
    cat("\n")

    cat("Procedure types")
    cat("\n")
    print(table(df[,grep("procedure_type", names(df), value=TRUE)], useNA = "always"))
    cat("\n")
    cat("\n")


    doc_types <- df_doc_types(df)
    cat("Document types:\t")
    cat(paste({if(is_long(df)) unique(df$doc) else doc_types}, sep = ""))

    for(i in 1:NROW(doc_types)){
        doc <- doc_types[i]
        cat("\n")
        cat("\n")
        cat(toupper(doc))
        cat("\n")
        cat("---")
        cat("\n")
        cat(paste("in data:", NROW(df[which(!is.na(df[,paste0("doc_",doc,"_words_noannex")])),])))
        cat("\n")
        data_doc_coverage_report(df, doc = doc)
        cat("\n")
        cat(paste("complexity complete:", NROW(df[which(df[,paste0("doc_",doc,"_complete_complexity")]),])))
        cat("\n")
        cat("\n")
        if(doc=="proposal"|doc=="final"){
            cat(paste("... amending:", NROW(df[which(df[,paste0("doc_",doc,"_amending")] & df[,paste0("doc_",doc,"_complete_complexity")]),])))
            cat("\n")

            cat(paste("... and amending:", NROW(df[which(df[,paste0("doc_",doc,"_and_amending")] & df[,paste0("doc_",doc,"_complete_complexity")]),])))
            cat("\n")

            cat(paste("... adapting:", NROW(df[which(df[,paste0("doc_",doc,"_adapting")] & df[,paste0("doc_",doc,"_complete_complexity")]),])))
            cat("\n")

            cat(paste("... repealing:", NROW(df[which(df[,paste0("doc_",doc,"_repealing")] & df[,paste0("doc_",doc,"_complete_complexity")]),])))
            cat("\n")

            cat(paste("... and repealing:", NROW(df[which(df[,paste0("doc_",doc,"_and_repealing")] & df[,paste0("doc_",doc,"_complete_complexity")]),])))
            cat("\n")
            cat("\n")
            cat(paste("... (possibly already removed due to) bad formatting:", NROW(df[which(df[,grep(paste0(doc,".*bad_formatting$"), names(df), value = TRUE)]),])))
            cat("\n")
            cat("\n")
            cat("\n")

            cat(paste("... legislative instruments:"))
            print(table(df[,paste0("doc_",doc,{if(any(grepl("_legislative_instrument", names(df)))) "_legislative_instrument" else "_leg_instr"})], useNA = "always"))

            cat("\n")

            data_complexity_variables_report(df, doc = doc, vars= "core")

        }

    }



}

#' @export
data_doc_coverage_report <- function(df, doc = "proposal", time = "year"){

    print_header(paste("Availability (completeness) of", doc, " complexity variables"), simple = TRUE)

    print(table(df[,paste0("doc_", doc, "_complete_complexity")],
          {if(time == "quarter")
              lubridate::quarter(df[,paste0("e_", doc, "_legal_date")], with_year = TRUE)
            else if (time == "year")
                lubridate::year(df[,paste0("e_", doc, "_legal_date")]) }, dnn = c(paste(doc), "year")))

}


#' @export
data_complexity_variables_report <- function(df, doc = "proposal", vars = "core"){

    print_header(paste("Variable summary for", vars, doc, "complexity variables"), simple = TRUE)

    vars_check_list <- df_complexity_varnames(df, complexity_vars = vars)

    # ranges
    stargazer::stargazer(df[,vars_check_list], type = "text", digits = 1)

}
