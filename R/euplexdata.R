# Main euplexdata function

#' @export
euplexdata <- function (df, remove_extra_vars=TRUE, rename_vars = TRUE, request_eurovoc = FALSE){

    df <- df %>%
        reformat_missing_data() %>%
        reformat_date_variables() %>%
        reformat_logical_variables() %>%
        create_named_procedure_event_variables(event_name = "final", event_codes = c("_PUB_OJ_", "_SIGN_byEP_CONSIL_", "_ADP_FRM_byCONSIL_")) %>%
        create_named_procedure_event_variables(event_name = "proposal", event_codes = c("_ADP_byCOM_")) %>%
        apply_correction_data() %>%
        {if(remove_extra_vars) remove_extra_variables(.) else .} %>%
        {if(rename_vars) rename_variables(.) else .} %>%
        create_complexity_variables() %>%
        create_eurovoc_domain_dummy_variables(request = request_eurovoc)

    df$X <- NULL

    df
}


#' @export
public_dataset <- function(df, events = c("proposal", "final"), docs =  c("proposal")){

    df <- df %>%
        set_bad_formatting_observations_na() %>%
        set_recast_observations_na() %>%
        keep_only(keep_events = events, keep_docs = docs) %>%
        create_complete_cases_variable(vars="all") %>%
        create_complete_cases_variable(vars="complexity_core", doc = "proposal") %>%
        remove_extra_variables %>%
        order_variables()

        # @ TODO create eurovoc indicator if not already in dataset

    df

}
