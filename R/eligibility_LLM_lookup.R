#' Additional lookups of cancer types and alterations using LLM
#'
#' @description
#' Prompt language model with trial data to get cancer types and alterations mentioned in trial record.\cr
#' This is just a lookup, there is no determination of how these mentions relate to eligibility.\cr
#' For each trial, if an identical prompt has already been sent previously and the results stored in a local database, the result from that call will be used, and the language model will not be called again.
#' The outputs from the LLM (whether obtained previously or through this function) will be used as the basis for dictionary lookups in order to map to NCI thesaurus codes.
#' @param eligibility_data A dataframe containing trial information, including columns `prompt_1` and `prompt_1_output` for storing the LLM-processed output.
#' @param dictionaries A list containing two specific dictionaries: `cancer_dict` for cancer types and `alteration_dict` for molecular dysfunctions.
#' @param llm_endpoint The API endpoint URL.
#' @param llm_api_key The API key used for authentication.
#' @returns Returns the input dataframe with additional columns:
#' - `cancer_lookup_llm`: for each trial, a list of NCI thesaurus codes related to cancer types.
#' - `moldysf_lookup_llm`: for each trial, a list of NCI thesaurus codes related to molecular dysfunctions.
#' @details
#' Calls the function `callLLM' for which API endpoint and key are required. These are specified as environment variables
#' @seealso \code{\link{callLLM}}
#' @export
eligibility_LLM_lookup = function(eligibility_data, dictionaries, llm_endpoint,llm_api_key) {
  eligibility_lookups = eligibility_data

  cancertype_dictionary <- dictionaries$cancer_dict
  moldysf_dictionary <- dictionaries$alteration_dict

  for(i in 1:nrow(eligibility_lookups)) {

    if(!is.na(eligibility_lookups$prompt_1_output[i])) {
      print(i)
      print('already used this prompt, using stored output')
      next
    }

    print(paste0('First call to LLM: ', i))
    prompt_1 = eligibility_lookups$prompt_1[i]

    terms = criteriaR::callLLM(prompt = prompt_1, endpoint = llm_endpoint, api_key = llm_api_key)

    ## NB THE CALL TO LLM MAY LEGITIMATELY RETURN NA IF NOT RELEVANT, SO NEED TO DISTINGUISH THIS FROM FAILURE TO RESPOND....
    ## e.g. instruct prompt to return 'not relevant' or something like that...
    ## OTHERWISE COULD GET STUCK IN A PERMANENT LOOP HERE....

    while(is.na(terms)) {
      print('Waiting (first call to LLM) .....')
      # Introduce a delay to avoid hitting the rate limit (e.g., 1 second)
      Sys.sleep(20)  # Adjust the duration as needed based on the rate limit

      terms = criteriaR::callLLM(prompt = prompt_1)
    }

    print(terms)

    eligibility_lookups$prompt_1_output[i] <- terms

  }

  # map prompt_1 outputs to cancer types in dictionary}
  ## this will constrain the logic to evaluate entities of the correct semantic type(s)
  ## will also disambiguate e.g. mismatched acronyms such as CRC which is a synonym of both Colorectal Carcinoma and Cytogenetic Complete Response in NCIt

  eligibility_lookups <- eligibility_lookups |>
    dplyr::mutate(cancer_lookup_llm = as.list(quanteda::tokens_lookup(x=quanteda::tokens(prompt_1_output), dictionary = cancertype_dictionary, nested_scope = 'dictionary'))) |>
    dplyr::mutate(moldysf_lookup_llm = as.list(quanteda::tokens_lookup(x=quanteda::tokens(prompt_1_output), dictionary = moldysf_dictionary, nested_scope = 'dictionary')))

  return(eligibility_lookups)

}
