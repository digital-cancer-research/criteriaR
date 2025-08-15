#' Form second prompts to go to language model
#'
#' @description
#'
#' For each trial, form a second prompt that will be sent to the language model.\cr
#' The second prompt will instruct the language model to generate an R expression (1 per treatment arm) that evaluates a patient object and returns TRUE (i.e. eligible) or FALSE (i.e. ineligible)\cr
#' Each prompt will include:\cr
#' - Instructions\cr
#' - Examples (with chain of thought reasoning)\cr
#' - A set of cancer types (and their NCI thesaurus codes) that are potentially relevant to eligibility\cr
#' - A set of alterations (and their NCI thesaurus codes) that are potentially relevant to eligibilty\cr
#' - The trial information (json format)\cr
#' @param eligibility_data A dataframe containing the initial eligibility criteria data
#' and the results from both LLM processing and dictionary lookups for cancer types and
#' molecular dysfunctions.
#' @param thesaurus A list containing the pruned thesaurus data used for obtaining the Preferred Terms (PTs) corresponding to the identified codes from the lookups.
#' @param prompt_2_instructions (Optional) Instructions to be included at start of prompt. If null, default text from package will be used.
#' @param prompt_2_example_1 (Optional) An example of input, reasoning and expected output. If null, default text from package will be used.
#' @param prompt_2_example_2 (Optional) An example of input, reasoning and expected output. If null, default text from package will be used.
#'
#' @returns Returns the input dataframe augmented with additional columns:
#' - `cancer_lookups_combined`: A combined list of unique cancer type codes identified from both LLM outputs and dictionary lookups.
#' - `moldysf_lookups_combined`: A combined list of unique molecular dysfunction codes identified from both LLM outputs and dictionary lookups.
#' - `cancer_codes_json`: JSON-formatted strings containing the PTs for the combined cancer type codes.
#' - `moldysf_codes_json`: JSON-formatted strings containing the PTs for the combined molecular dysfunction codes.
#' - `prompt_2`: The fully formed second prompts, incorporating instructions, examples, and the trial information to be analyzed, along with the potential eligible conditions and alterations in JSON format.
#'
#' @details
#' - Uses few-shot prompting with chain of thought reasoning.\cr
#' - Prompts are formed by concatenating instructions, examples, and trial data.\cr
#' - Instructions and examples are configurable, and should be stored as text files in the following relative paths:\cr
#'    - `inst/prompts/prompt_2_instructions.txt`
#'    - `inst/prompts/prompt_2_example_1.txt`
#'    - `inst/prompts/prompt_2_example_2.txt`
#'
#' @export
formPrompt2 = function(eligibility_data, thesaurus,
                       prompt_2_instructions = NULL,
                       prompt_2_example_1 = NULL,
                       prompt_2_example_2 = NULL ) {

  # prompt_2_instructions <- readr::read_file(system.file("prompts", "prompt_2_instructions.txt", package = "criteriaR"))
  # prompt_2_example_1 <- readr::read_file(system.file("prompts", "prompt_2_example_1.txt", package = "criteriaR"))
  # prompt_2_example_2 <- readr::read_file(system.file("prompts", "prompt_2_example_2.txt", package = "criteriaR"))

  if (is.null(prompt_2_instructions)) {
    prompt_2_instructions <- readr::read_file(
      system.file("prompts", "prompt_2_instructions.txt", package = "criteriaR")
    )
  }

  if (is.null(prompt_2_example_1)) {
    prompt_2_example_1 <- readr::read_file(
      system.file("prompts", "prompt_2_example_1.txt", package = "criteriaR")
    )
  }

  if (is.null(prompt_2_example_2)) {
    prompt_2_example_2 <- readr::read_file(
      system.file("prompts", "prompt_2_example_2.txt", package = "criteriaR")
    )
  }



  ## combine lookup values from LLM and dictionaries into single lists per study
  eligibility_criteria <- eligibility_data |>
    dplyr::mutate(cancer_lookups_combined = purrr::map2(cancer_lookup, cancer_lookup_llm, ~ unique(c(.x, .y)))) |>
    dplyr::mutate(moldysf_lookups_combined = purrr::map2(moldysf_lookup, moldysf_lookup_llm, ~ unique(c(.x, .y)))) |>
    # dplyr::select(nct_id, conditions, criteria, design_groups, json, cancer_lookups_combined, moldysf_lookups_combined) |>
    unique()

  ## for each unique set of codes, get the PTs and save as a json string
  eligibility_criteria$cancer_codes_json <- NA
  eligibility_criteria$moldysf_codes_json <- NA

  for(i in 1:nrow(eligibility_criteria)) {
    cancer_codes = eligibility_criteria$cancer_lookups_combined[[i]]
    moldysf_codes = eligibility_criteria$moldysf_lookups_combined[[i]]

    cancer_json <-  thesaurus$thesaurus |>
      dplyr::filter(code %in% cancer_codes) |>
      dplyr::select(code, PT) |>
      unique() |>
      jsonlite::toJSON()

    moldysf_json <-  thesaurus$thesaurus |>
      dplyr::filter(code %in% moldysf_codes) |>
      dplyr::select(code, PT) |>
      unique() |>
      jsonlite::toJSON()

    eligibility_criteria$cancer_codes_json[i] <- cancer_json
    eligibility_criteria$moldysf_codes_json[i] <- moldysf_json
  }

  ## form second prompts to LLM


  eligibility_criteria$prompt_2 <- paste(
    prompt_2_instructions,
    prompt_2_example_1,
    prompt_2_example_2,
    'No more examples - the trial infomration to be analysed for this task are as follows....',
    'POTENTIAL ELIGIBLE CONDITION VALUES:',
    eligibility_criteria$cancer_codes_json,
    'POTENTIAL ELIGIBLE ALTERATION VALUES:',
    eligibility_criteria$moldysf_codes_json,
    '\n',
    'TRIAL INFORMATION: ',
    eligibility_criteria$json,
    sep = '\n'
  )

  return(eligibility_criteria)
}
