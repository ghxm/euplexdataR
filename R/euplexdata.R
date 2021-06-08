# Main euplexdata function

#' @export
euplexdata <- function (df, remove_extra_vars=TRUE, rename_vars = TRUE, request_eurovoc = FALSE, rm_raw = TRUE, events_docs=c('proposal', 'final')){

    df <- df %>%
        reformat_missing_data() %>%
        reformat_date_variables() %>%
        reformat_logical_variables() %>%
        {if ('final' %in% docs_events) create_named_procedure_event_variables(event_name = "final", event_codes = c("_PUB_OJ_", "_SIGN_byEP_CONSIL_", "_ADP_FRM_byCONSIL_")) else .}%>%
        create_named_procedure_event_variables(event_name = "proposal", event_codes = c("_ADP_byCOM_")) %>%
        apply_correction_data() %>%
        {if(remove_extra_vars) remove_extra_variables(.) else .} %>%
        {if(rename_vars) rename_variables(.) else .} %>%
        create_complexity_variables() %>%
        create_eurovoc_domain_dummy_variables(request = request_eurovoc, rm_raw = rm_raw)

    df$X <- NULL

    df
}


#' @export
public_dataset <- function(df, events = c("proposal"), docs =  c("proposal"), wide = FALSE, proposal_dates = NULL, rm_raw = TRUE, var_rm_regex = "legal_basis|decision_mode__fd"){

    df <- df %>%
        set_bad_formatting_observations_na(newline = FALSE, newline_ratio_cutoff = 0.003) %>%
        set_recast_observations_na(doc="proposal") %>%
        {
            if(!is.null(proposal_dates)){
                subset_by_date(., proposal_dates = proposal_dates)}
            else{.}
        } %>%
        keep_only(keep_events = events, keep_docs = docs) %>%
        create_complete_cases_variable(vars="complexity_core",doc="proposal") %>%
        {if('final' %in% docs) create_complete_cases_variable(vars="complexity_core",doc="final") else .} %>%
        create_unified_title_variable(lang="en", guess_title_lang = FALSE, guess_title_lang_conflict = TRUE, rm_raw = rm_raw) %>%
        remove_extra_variables(varnames_regex = var_rm_regex) %>%
        {if(wide) . else long(.)} %>%
        order_variables()

        # @ TODO create eurovoc indicator if not already in dataset

    df

}
