#' @export
data_report <- function(df, report = "all"){

}


#' @export
observations <- function(df, display = c()){

    always_display <- c("procedure_id","doc_proposal_uri_celex", "e_proposal_legal_date", "doc_proposal_procedure_subtype")

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

    cat(paste("Number of procedures:", NROW(unique(df$procedure_id))))
    cat("\n")

    cat(paste("Proposal dates:", min(df$e_proposal_legal_date, na.rm =TRUE), "--", max(df$e_proposal_legal_date, na.rm =TRUE)))
    cat("\n")

    cat("Procedure types")
    cat("\n")
    print(table(df$procedure_type_0, useNA = "always"))
    cat("\n")
    cat("\n")


    doc_types <- unique(na.omit(stringr::str_extract(names(df), "(?<=doc_).*?(?=_)")))
    cat(paste("Document types:\t", {if(is_long(df)) unique(df$doc) else doc_types}, sep = ""))

    for(i in 1:NROW(doc_types)){
        doc <- doc_types[i]
        cat("\n")
        cat("\n")
        cat(doc)
        cat("\n")
        cat("---")
        cat("\n")
        cat(paste("in data:", NROW(df[which(!is.na(df[,paste0("doc_",doc,"_word_count")])),])))
        cat("\n")
        cat(paste("complexity complete:", NROW(df[which(df[,paste0("doc_",doc,"_complete_complexity")]),])))
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
            print(table(df[,paste0("doc_",doc,"_legislative_instrument")], useNA = "always"))

            cat("\n")

        }

    }



}

#' @export
data_doc_coverage_report <- function(df, doc = "proposal", time = "year"){

    print_header(paste("Availability (completeness) of", doc, " complexity variables"))

    print(table(df[,paste0("doc_", doc, "_complete_complexity")],
          {if(time == "quarter")
              lubridate::quarter(df[,paste0("e_", doc, "_legal_date")], with_year = TRUE)
            else if (time == "year")
                lubridate::year(df[,paste0("e_", doc, "_legal_date")]) }, dnn = c(paste(doc), "year")))

}

print_header <- function(text){
    cat("\n")
    cat("==========================")
    cat("\n")
    cat(text)
    cat("\n")
    cat("------------------")
    cat("\n")
}

#' @export
data_complexity_variables_report <- function(df, doc = "proposal", vars = "core"){

    print_header(paste("Variable summary for", vars, doc, "complexity variables"))

    if(vars == "all"){
    vars_check_list <- unique(unlist(sapply(complexity_varnames, function(x) grep(paste0(doc, ".*", x), names(df), value=TRUE))))
    } else if(vars == "core"){
    vars_check_list <- unique(unlist(sapply(complexity_varnames_core, function(x) grep(paste0(doc, ".*", x), names(df), value=TRUE))))
    }

    # ranges
    stargazer::stargazer(df[,vars_check_list], type = "text", digits = 1)

}
