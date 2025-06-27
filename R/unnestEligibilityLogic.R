#' Unnest and Transform Eligibility Logic from JSON to Data Frame Format
#'
#' @description
#' Converts the LLM output from prompt2 from json format into dataframe and unnests (empty rows are retained).
#' @param eligibility_data A dataframe including `prompt_2_output` column storing JSON-formatted eligibility logic for each trial.
#' @param raw_trial_data A dataframe containing raw trial data, including design groups information to be linked to the eligibility logic.
#'
#' @return A dataframe similar to the input `eligibility_data` but with the JSON-formatted `prompt_2_output` converted and unnested into separate columns.
#' Additional columns for design group titles (`dg_title`), statuses (`dg_status`), and IDs (`dg_id`) are also included
#' @details
#' Note that design group IDs are not static - i.e. the same design group could have different IDs over time - this is a feature of the AACT database.
#' @export
unnestEligibilityLogic = function(eligibility_data, raw_trial_data ) {
  ## get design groups table as we will need dg titles later
  design_groups = raw_trial_data$design_groups


  # convert prompt_2_output from json to dataframe}
  eligibility_data$logic_df = vector("list", nrow(eligibility_data))

  for(i in 1:nrow(eligibility_data)) {
    json = eligibility_data$prompt_2_output[i]

    if(is.na(json)) next

    tryCatch({
      eligibility_data$logic_df[[i]] <- json |>
        jsonlite::fromJSON()  # Use double brackets for list assignment
    }, error = function(e) {
      message("An error occurred: ", e$message)
    })

  }

  # Unnest the logic_df column while keeping empty rows
  eligibility_data <- eligibility_data |>
    tidyr::unnest(cols = 'logic_df', keep_empty = T, names_sep = '_') |>
    as.data.frame() |>
    dplyr::rename(logic = logic_df_logic_string) |>
    dplyr::rename(dg_title = logic_df_dg_title) |>
    # dplyr::rename(dg_id = logic_df_dg_id) |>
    dplyr::rename(dg_status = logic_df_dg_status) |>
    merge(by.x = c('nct_id', 'dg_title'), y = dplyr::select(design_groups, nct_id, dg_id = id, dg_title = title), by.y = c('nct_id', 'dg_title')) |>
    # dplyr::mutate(conditions = purrr::map_chr(conditions, ~ paste(.x$condition_name, collapse = ", "))) |>
    # dplyr::select(nct_id, trial_json = json, conditions, criteria, dg_id, dg_title, dg_status, logic) |>
    unique()

  return(eligibility_data)
}
