generate_hand_coding_samples <- function(df, out = NULL, n_coders = NULL, coder_names = c(), sample_size = 100, overlap = 3, doc = c("proposal"), hand_coding_vars = c("citations", "recitals", "articles", "ref_int_enacting", "ref_ext_enacting", '_bad_formatting', "comments")){

    if(is_long(df)){
        stop("This function currently only works for wide-format data! Please convert first.")
    }

    if(NROW(coder_names)==0){
        coder_names <- 1:n_coders
    } else if (!is.null (n_coders) && NROW(coder_names) != n_coders) {
        stop("Number of coders argument (n_coders) and length of coder name list do not match! Either match the number of coders and the name list or supply just one of both.")
    } else if(!is.null (n_coders)){
        n_coders <- NROW(coder_names)
    }


    coding_samples <- list()

    # draw samples, assign IDs
    df_samples <- dplyr::sample_n(df, size = sample_size, replace = FALSE) %>%
        dplyr::select("procedure_id", paste0("doc_", doc, "_uri_celex")) %>%
        dplyr::mutate(ID = dplyr::row_number())

    df_samples[, paste0("doc_", doc, "_", hand_coding_vars)] <- NA

    samples_per_coder <- as.integer(sample_size/n_coders)

    # select overlap samples

    df_samples_overlap <- df_samples[1:overlap,]
    df_samples <- df_samples[(overlap+1):NROW(df_samples),]

    # assign samples minus overlap to coders
    df_samples$coder <- as.character(rep(coder_names, length.out = NROW(df_samples),each = as.integer(NROW(df_samples) / n_coders)))

    # add urls
    if(any(is.na(df_samples$doc_proposal_uri_celex))){
        warning("Some CELEX IDs are NA. This function currently only works with CELEX IDs. Please check your input data!")
    }

    df_samples$url <- paste0("https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=" , df_samples$doc_proposal_uri_celex, "&from=EN")


    # order variables, remove unnecessary
    df_samples_sparse <- df_samples[, c("ID", "url", unlist(sapply(hand_coding_vars, function(x) grep(x, names(df_samples), value = TRUE))))]

    coding_samples <- split(df_samples_sparse, f=df_samples$coder)


    # add overlap samples

    coding_samples <- lapply(coding_samples, function(x) dplyr::bind_rows(x[,!names(x) %in% c("coder")], df_samples_overlap))


    # save ID - CELEX - procedure_id table
    id_table <- dplyr::bind_rows(df_samples, df_samples_overlap)
    id_table[is.na(id_table$coder),]$coder <- "overlap"
    id_table <- data.frame(id_table[ , ! colnames(id_table) %in% id_table$coder], stats::model.matrix( ~ coder - 1, id_table))

    coding_samples[['id_table']] <- id_table


    if(is.null(out)){
        coding_samples
    }else{
        if(!endsWith(out, "/")){
            out <- paste0(out, "/")
        }
        for(k in 1:NROW(coding_samples)){
            table_name <- names(coding_samples)[k]
            openxlsx::write.xlsx(coding_samples[table_name], file=paste0(out, "euplexdb_hand_coding_", table_name, ".xlsx"))
        }
    }
}
