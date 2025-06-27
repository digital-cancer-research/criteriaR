#' Form first prompts to go to Language Model
#'
#' @description
#' In addition to dictionary based lookups of cancer types and alterations in trial data, form prompts (1 per trial) that will send the trial data to language model in order to identify additional matches.\cr
#' @param eligibility_data A dataframe containing trial information in JSON format.
#' @returns The input dataframe with an additional column, `prompt_1`, which contains the fully formed prompts for the LLM.
#' @details
#' - Uses few-shot prompting with chain of thought reasoning.\cr
#' - Prompts are formed by concatenating instructions, examples, and trial data.\cr
#' - Instructions and examples are stored at the following relative paths:\cr
#'    - `inst/prompts/prompt_1_instructions.txt`
#'    - `dinst/prompts/prompt_1_example_1.txt`
#'    - `inst/prompts/prompt_1_example_2.txt`
#'    - `inst/prompts/prompt_1_example_3.txt`
#' @export
formPrompt1 = function(eligibility_data) {
  eligibility_lookups = eligibility_data
  prompt_1_instructions <- readr::read_file(system.file("prompts", "prompt_1_instructions.txt", package = "criteriaR"))
  prompt_1_example_1 <- readr::read_file(system.file("prompts", "prompt_1_example_1.txt", package = "criteriaR"))
  prompt_1_example_2 <- readr::read_file(system.file("prompts", "prompt_1_example_2.txt", package = "criteriaR"))
  prompt_1_example_3 <- readr::read_file(system.file("prompts", "prompt_1_example_3.txt", package = "criteriaR"))


  filepath <-



  # form first prompts to LLM}
  eligibility_lookups$prompt_1 <- paste(
    prompt_1_instructions,
    prompt_1_example_1,
    prompt_1_example_2,
    prompt_1_example_3,
    'No more examples - the trial infomration to be analysed for this task are as follows....',
    eligibility_lookups$json,
    sep = '\n'
  )


  return(eligibility_lookups)
}
