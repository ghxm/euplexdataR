
# Functions to wrangle raw euplexdb datasets

#' @export
reformat_date_variables <- function(df){
    df[,grep("_date$", names(df))] <- apply(df[,grep("_date$", names(df))], 2, function(x) lubridate::as_date(x))

    df
}

#' @export
reformat_logical_variables <- function(df){
    df[df==""] <- NA

    for(col in 1:NCOL(df)){
        if (all(df[,col]=="TRUE" | df[,col]=="FALSE" | is.na(df[,col]))){
            df[,col] <- as.logical(df[,col])
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

    # Codes must be supplied in the reverse order they appear in the legislative process (last possible first)!

    # create a dataframe containing only the variables of interest
    df_events <- df[, c("procedure_reference_0", unlist(sapply(event_codes, function(x) grep(x,names(df), value=TRUE), simplify="array")))]


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
                rows[[j]] <- df_row[,c("procedure_reference_0", grep(event_name, names(df_row), value = TRUE))]
                j <- j + 1
                # break event code for-loop to continue with next row
                break
            }
        }
    }

    df_rows <- dplyr::bind_rows(rows)


    if ('procedure_id' %in% names(df)){
        merge_by = "procedure_id"
    }else{
        merge_by = "procedure_reference_0"
    }

    # return warning if merge variable is not unique
    if(any(duplicated(df[[merge_by]]))){
        warning(paste("Named event results are not reliable: variable", merge_by, "not unique across rows! Please remove duplicates or use a newer dataset version."))
    }

    merge(df, df_rows, all.x=TRUE, by = merge_by)
}
