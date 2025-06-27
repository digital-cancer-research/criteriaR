#' Convert trial data to json format and lookup cancer types and alterations using dictionaries
#'
#' @description
#' As a first step towards annotating trial eligibility, convert relevant data to json format, 1 json string per trial.\cr
#' Relevant data include condition names, design group titles and descriptions, and eligibility criteria.\cr
#' @param trial_data A list of dataframes containing raw clinical trial data.\cr
#' @returns A dataframe, 1 row per trial including JSON-structured data\cr
#' @export
compose_trial_json = function(trial_data) {
  eligibilities = trial_data$eligibilities
  conditions = trial_data$conditions
  design_groups = trial_data$design_groups

  # Summarize conditions: one row per nct_id with list of conditions
  conditions_summary <- conditions |>
    dplyr::group_by(nct_id) |>
    dplyr::summarise(conditions = list(name), .groups = 'drop')

  # Summarize design groups: one row per nct_id with list of design group objects
  design_groups_summary <- design_groups |>
    dplyr::select(nct_id, dg_title = title, dg_description = description) |>
    dplyr::group_by(nct_id) |>
    dplyr::summarise(
      design_groups = list(tibble::tibble(title = dg_title, description = dg_description)),
      .groups = 'drop'
    )

  # Combine everything with eligibilities
  merged_data <- eligibilities |>
    dplyr::select(nct_id, criteria) |>
    dplyr::left_join(conditions_summary, by = "nct_id") |>
    dplyr::left_join(design_groups_summary, by = "nct_id")

  # Convert to JSON
  json_data <- merged_data |>
    dplyr::mutate(
      json = purrr::pmap_chr(
        list(nct_id, conditions, criteria, design_groups),
        function(nct_id, conditions, criteria, design_groups) {
          jsonlite::toJSON(
            list(
              nct_id = nct_id,
              conditions = conditions,
              criteria = criteria,
              design_groups = design_groups
            ),
            auto_unbox = TRUE
          )
        }
      )
    )

  return(json_data)

}
