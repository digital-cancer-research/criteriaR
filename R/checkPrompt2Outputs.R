#' Check whether second prompts have already been used
#' @description
#' If the pipeline has been run before, check for each trial whether an identical prompt2 has been sent to LLM before.\cr
#' If an identical prompt has been sent previously, use the output from that call in order to reduce the number of calls that will go the the LLM during the next step of the pipeline.\cr
#' @param latest_prompts A dataframe that includes prompt2 values formed during the previous step.
#' @param db_path Path to a SQLite database - if the database includes an `eligibility` table, this function will check if a prompt has been used before
#' @returns A dataframe with an additional column `prompt_2_output`. This column is filled with NA by default, but includes previous responses if prompt2 has already been sent previously.
#' @export
checkPrompt2Outputs <- function(latest_prompts, db_path) {

  ## add a column that will store outputs from LLM
  latest_prompts$prompt_2_output <- NA


  ## if local database doesn't exist (i.e. pipeline has not yet been run) just return the table
  if(!(file.exists(db_path))) {
    return(latest_prompts)
  }

  sqlite_con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  ## if eligibility table does not exist in DB, just return the table
  table_names = DBI::dbListTables(sqlite_con)
  if(!('eligibility' %in% table_names)) {
    return(latest_prompts)
  }


  for(i in 1:nrow(latest_prompts)) {
    study_id <- latest_prompts$nct_id[i]
    latest_prompt_2 = latest_prompts$prompt_2[i]

    stored_prompt_2 <- DBI::dbGetQuery(sqlite_con, paste0('SELECT DISTINCT prompt_2 FROM eligibility e WHERE e.nct_id = "', study_id, '"')) |> dplyr::pull(prompt_2)

    if(length(stored_prompt_2) == 0) next


    if(stored_prompt_2 == latest_prompt_2) {
      stored_prompt_2_output <- DBI::dbGetQuery(sqlite_con, paste0('SELECT DISTINCT prompt_2_output FROM eligibility e WHERE e.nct_id = "', study_id, '"')) |>
        dplyr::pull(prompt_2_output)

      if(length(stored_prompt_2_output) == 0) next

      latest_prompts$prompt_2_output[i] <- stored_prompt_2_output      ## use the stored output from last time this prompt was called
    }
  }

  return(latest_prompts)
}
