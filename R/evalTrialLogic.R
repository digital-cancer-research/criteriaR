#' Evaluate Trial Eligibility Logic for a Patient
#'
#' Parses and evaluates a trial eligibility logic string in the context of a given patient's clinical data.
#'
#' @details
#' This function takes a `patient` object (typically a list or data frame row)
#' containing at least `condition` and `alteration` fields, and a string representing
#' trial eligibility logic.\cr
#' The `trial_logic_string` is parsed and evaluated dynamically using `eval()` and `parse()`,
#' enabling flexible trial matching based on complex logical criteria.\cr
#'
#' **Note:** As `eval(parse())` executes arbitrary R code, ensure that `trial_logic_string` is **fully validated and controlled to avoid security risks.**
#'
#' @param patient A list or data frame row representing a patient. Must contain fields `condition` and `alteration`.
#' @param trial_logic_string A character string representing R code that defines eligibility logic,
#' typically involving `condition` and `alteration`.
#'
#' @return Logical (`TRUE` or `FALSE`) indicating whether the patient satisfies the trial logic.
#'
#' @seealso [base::eval()], [base::parse()]
#'
#' @examples
#' # Example patient data
#' patient <- list(
#'   condition = "Lung Cancer",
#'   alteration = "EGFR Mutation"
#' )
#'
#' # Example trial logic string
#' trial_logic_string <- "condition == 'Lung Cancer' && alteration == 'EGFR Mutation'"
#'
#' # Evaluate trial eligibility
#' is_eligible <- evalTrialLogic(patient, trial_logic_string)
#'
#' print(is_eligible)
#' #> TRUE
#'
#' @export
evalTrialLogic <- function(patient, trial_logic_string) {
  condition = patient$condition
  alteration = patient$alteration

  result = eval(parse(text = trial_logic_string))

  return(result)

}
