# Main euplexdata function

#' @export
euplexdata <- function (df, remove_extra_vars=TRUE){

    df %>%
        reformat_missing_data() %>%
        reformat_date_variables() %>%
        create_procedure_type_dummies() %>%
        reformat_logical_variables() %>%
        create_named_procedure_event_variables(event_name = "final", event_codes = c("_PUB_OJ_", "_SIGN_byEP_CONSIL_", "_ADP_FRM_byCONSIL_")) %>%
        create_named_procedure_event_variables(event_name = "proposal", event_codes = c("_ADP_byCOM_")) %>%
        apply_correction_data() %>%
        {if(remove_extra_vars) remove_extra_variables(.) else .}

}
