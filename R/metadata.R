#' Get complete structural metadata for the specified dataset
#'
#' Provides detailed information about dataset structure, columns, data types
#' and technical metadata from the ONTL API.
#'
#' @param dataset Name of the ONTL dataset (default: "antt_pracaspedagio")
#' @return List containing complete metadata
#' @export
#'
#' @examples
#' \dontrun{
#' metadata <- get_metadados()
#' names(metadata)
#' }
get_metadados <- function(dataset = "antt_pracaspedagio") {
  metadados_url <- paste0(
    "https://ontl-apim.infrasa.gov.br/api/public/datasets/", 
    dataset, 
    "/meta"
  )
  
  tryCatch({
    .make_request(metadados_url)
  }, error = function(e) {
    warning(paste("Erro ao buscar metadados:", e$message))
    return(list())
  })
}

#' Retrieve list of filterable columns from dataset metadata
#'
#' Generates a data frame with columns that can be used to filter
#' data in the API, facilitating specific query construction.
#'
#' @param dataset Name of the ONTL dataset (default: "antt_pracaspedagio")
#' @return Tibble with two columns: dataset and column_name
#' @export
#'
#' @examples
#' \dontrun{
#' columns <- get_metadados_filterable_columns()
#' print(columns)
#' }
get_metadados_filterable_columns <- function(dataset = "antt_pracaspedagio") {
  tryCatch({
    meta <- get_metadados(dataset)
    cols <- meta$filterableColumns
    
    if (is.null(cols)) {
      cols <- character(0)
    }
    
    table <- if (!is.null(meta$table)) meta$table else dataset
    
    tibble::tibble(
      dataset = rep(table, length(cols)),
      column_name = cols
    )
    
  }, error = function(e) {
    warning(paste("Erro ao recuperar colunas filtráveis:", e$message))
    return(tibble::tibble(dataset = character(), column_name = character()))
  })
}
