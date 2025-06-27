#' Use LLM to generate R expressions that evaluate trial eligibility
#'
#' @description
#' Based on the prompts formed in the previous step, generate R expressions (1 per treatment arm) that evaluate a patient object and return either `TRUE` (eligibile) or `FALSE` (ineligible)
#' @param eligibility_data A dataframe including columns `prompt_2` and `prompt_2_output`
#' @returns The input dataframe with the `prompt_2_output` column updated with the generated logic from the LLM. For treatment arms that already have an output (or where no relevant terms are mentioned (because an identical prompt2 has been used before) the original response is preserved.
#' @details
#' Calls the function `callLLM' for which API endpoint and key are required. These are specified as environment variables
#' @seealso \code{\link{callLLM}}
#' @export
createEligibilityLogic = function(eligibility_data) {
  eligibility_criteria <- eligibility_data
  for(i in 1:nrow(eligibility_criteria)) {
    if(!is.na(eligibility_criteria$prompt_2_output[i])) {
      print(i)
      print('already used this prompt, using stored output')
      next
    }
    cat(paste0('Second call to LLM, i = ', i, '\n\n'))
    prompt_2 = eligibility_criteria$prompt_2[i]
    if(is.na(prompt_2)) {
      print('no relevant terms mentioned in criteria...')
      next
    }

    logic_json = prompt_2 |>
      callLLM() |>
      gsub(pattern = '```json', replacement = '') |>
      gsub(pattern = '```', replacement = '')

    while(is.na(logic_json)) {
      print('Waiting (second call to LLM) .....')

      # Introduce a delay to avoid hitting the rate limit (e.g., 1 second)
      Sys.sleep(20)  # Adjust the duration as needed based on the rate limit

      logic_json = callLLM(prompt_2) |>
        gsub(pattern = '```json', replacement = '') |>
        gsub(pattern = '```', replacement = '')
    }

    eligibility_criteria$prompt_2_output[i] <- logic_json
  }
  return(eligibility_criteria)
}
