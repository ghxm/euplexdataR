

euplexdata <- function (df){

    # @TODO: make into proper class?
    # @TODO: add special option to determine last event codes etc

    df %>%
        reformat_date_variables() %>%
        create_procedure_type_dummies() %>%
        reformat_logical_variables() %>%
        create_named_procedure_event_variables(event_name = "final", event_codes = c("_PUB_OJ_", "_SIGN_byEP_CONSIL_", "_ADP_FRM_byCONSIL_")) %>%
        create_named_procedure_event_variables(event_name = "proposal", event_codes = c("_ADP_byCOM_"))

}
