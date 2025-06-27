#' Convert trial data to json format and lookup cancer types and alterations using dictionaries
#'
#' @description
#' As a first step towards annotating trial eligibility, convert relevant data to json format, 1 json string per trial.\cr
#' Relevant data include condition names, design group titles and descriptions, and eligibility criteria.\cr
#' Lookup matches against dictionaries of cancer types and molecular alterations in the json strings.\cr
#' @param json_data A dataframe that includes trial data in column `json`, in json format.\cr
#' @param dictionaries A list of dictionaries including `cancer_dict` and `alteration_dict`.\cr
#' @returns A dataframe, 1 row per trial including new columns with results of lookups:\cr
#' - `cancer_lookup`
#' - `moldysf_lookup`
#' @export
eligibilityLookup = function(json_data, dictionaries) {

  cancertype_dictionary <- dictionaries$cancer_dict
  moldysf_dictionary <- dictionaries$alteration_dict

  ## lookup cancertypes
  json_data <- json_data |>
    dplyr::mutate(cancer_lookup = json |>
                    quanteda::tokens() |>
                    quanteda::tokens_lookup(dictionary = cancertype_dictionary, nested_scope = 'dictionary') |>
                    as.list()
    )

  ## lookup alterations
  json_data <- json_data |>
    dplyr::mutate(moldysf_lookup = json |>
                    quanteda::tokens() |>
                    quanteda::tokens_lookup(dictionary = moldysf_dictionary, nested_scope = 'dictionary') |>
                    as.list()
    )

  return(json_data)

}
