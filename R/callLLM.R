#' Call Large Language Model
#' @description
#' Call a language model via API
#'
#' @details
#' This function requires two environment variables to be set:\cr
#' - `LLM_API_KEY`: The API key used for authentication.\cr
#' - `LLM_ENDPOINT`: The full URL endpoint for the API.\cr
#'
#' Set these environment variables either in your R session using `Sys.setenv()`, or permanently in a `.Renviron` file.\cr
#'
#' If either `LLM_API_KEY` or `LLM_ENDPOINT` is not set, the function will throw an error.\cr
#' @param prompt (string) The prompt to be passed to the language model .
#' @returns The response from the model
#' @export

callLLM <- function(prompt) {
  # Json payload
  json_data <- list(
    messages = list(
      list(role = "user", content = prompt)  # Required structure for chat messages
    ),
    temperature = 0,
    max_tokens = 1000
  )
  # Including the API key in HTTP headers
  headers <- httr::add_headers(
    'api-key' = Sys.getenv('LLM_API_KEY'),
    'Content-Type' = 'application/json'
  )

  response <- httr::POST(url = Sys.getenv('LLM_ENDPOINT'),
                         body = json_data,
                         encode = "json",
                         config = headers)

  if (httr::status_code(response) == 200) {
    result <- httr::content(response, as = "parsed")
    message_content <- result$choices[[1]]$message$content
    return(message_content)
  } else return(NA)


}
