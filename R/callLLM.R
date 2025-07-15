#' Call Large Language Model
#' @description
#' Call a language model via API
#' @details
#' This function sends a prompt to a language model API (e.g., OpenAI-compatible endpoint) and returns the generated response.
#' By default, it reads the API endpoint and key from environment variables, but these can be overridden via function arguments.
#' Set these environment variables either in your R session using `Sys.setenv()`, or permanently in a `.Renviron` file.
#' @param prompt The prompt to be passed to the language model
#' @param endpoint The API endpoint URL. Defaults to the value of `Sys.getenv("LLM_ENDPOINT")`.
#' @param api_key The API key used for authentication. Defaults to `Sys.getenv("LLM_API_KEY")`.
#' @param temperature Sampling temperature. Higher values (e.g., 0.7) produce more random output. Defaults to 0.
#' @param max_tokens The maximum number of tokens to generate in the response. Defaults to 1000
#' @return The response from the model
#' @export
callLLM <- function(prompt,endpoint = Sys.getenv("LLM_ENDPOINT"),api_key = Sys.getenv("LLM_API_KEY"), temperature = 0,max_tokens = 1000) {
  # Json payload
  json_data <- list(
    messages = list(
      list(role = "user", content = prompt)  # Required structure for chat messages
    ),
    temperature = temperature,
    max_tokens = max_tokens
  )

  # Including the API key in HTTP headers
  headers <- httr::add_headers(
    'api-key' = api_key,
    'Content-Type' = 'application/json'
  )

  response <- httr::POST(url = endpoint,
                         body = json_data,
                         encode = "json",
                         config = headers)


  if (httr::status_code(response) == 200) {
    result <- httr::content(response, as = "parsed")
    message_content <- result$choices[[1]]$message$content
    return(message_content)
  } else return(NA)

}
