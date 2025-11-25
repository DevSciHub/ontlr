#' Função interna para fazer requisições HTTP
#' 
#' @param url URL para requisição
#' @param params Parâmetros da query string
#' @param timeout Timeout em segundos
#' @return Lista com resposta JSON parseada
#' @keywords internal
.make_request <- function(url, params = NULL, timeout = 60) {
  tryCatch({
    response <- httr::GET(
      url, 
      query = params, 
      httr::timeout(timeout),
      httr::add_headers(
        "User-Agent" = "ontlpyR/1.0.0",
        "Accept" = "application/json"
      )
    )
    
    # Verifica status HTTP
    if (httr::http_error(response)) {
      stop(paste("Erro HTTP:", httr::status_code(response)))
    }
    
    # Parse do JSON
    content <- httr::content(response, "text", encoding = "UTF-8")
    jsonlite::fromJSON(content, simplifyDataFrame = FALSE)
    
  }, error = function(e) {
    stop(paste("Erro na requisição para", url, ":", e$message))
  })
}
