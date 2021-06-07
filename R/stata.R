#' @export
stata <- function(df, out="", version = 14){

    df <- df %>%
        stata_convert_dates() %>%
        stata_shorten_strings %>%
        stata_shorten_varnames()

    if(NROW(stata_too_long_varnames(df)) > 0){
        warning("The following variables will be removed due to the Stata variable name size limit (32 chars):\n", paste(stata_too_long_varnames(df), "\n"))
        df <- df[,nchar(names(df)) < 32]
    }


    if(nchar(out) > 0){
        haven::write_dta(df, out, version = version, label = attr(utils::data, "euplexdb version 1.0"))
    }else {
        df
    }


}

#' @export
stata_too_long_varnames <- function(df){

    # shortening strings to 128 characters for stata
    names(df)[(nchar(names(df)) > 32)]

}

#' @export
stata_shorten_varnames <- function(df, rename_doc=FALSE){

    # doc_ to d_
    if(rename_doc){
        names(df) <- gsub("doc_", "d_", names(df))
    }

    # proposal to prop
    names(df) <- gsub("proposal_", "prop_", names(df))
    names(df) <- gsub("final_", "fin_", names(df))

    df
}

#' @export
stata_shorten_strings <- function(df){

    # shortening strings to 128 characters for stata
    df[, unlist(lapply(df, is.character))] <- apply(df[, which(sapply(df, is.character))], 2, function(y) sapply(y, function(x) ifelse((nchar(x, keepNA = FALSE)) > 128, strtrim(x, 128), x)))

    df
}


#' @export
stata_convert_dates <- function(df){

    df[,grep("_date$", names(df))] <- apply(as.data.frame(df[,grep("_date$", names(df))]),2, function(x) stataXml::asStataTime(as.Date(x, optional = TRUE)))

    df
}

