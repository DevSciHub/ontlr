#' Fetch paginated historical toll data from ANTT
#'
#' This function performs multiple requests to the ONTL public API to retrieve
#' all records for a given year, automatically handling result pagination.
#' Ideal for historical toll data analysis and transportation research projects.
#'
#' @param year Reference year for data filtering (default: 2000)
#' @param dataset Name of the dataset in the ONTL API 
#'                (default: "antt_pracaspedagio")
#' @param column_order_by Column name used for result ordering
#'                        (default: "int_idapracaspedagio")
#' @param column_filter_year Column name containing the year for filtering
#'                          (default: "int_ano")
#'
#' @return A tibble containing all records found for the specified year
#' @export
#'
#' @examples
#' \dontrun{
#' # Fetch toll data for 2023
#' df_2023 <- get_dataset(year = 2023)
#' 
#' # Fetch data with custom parameters
#' df_custom <- get_dataset(
#'   year = 2022,
#'   dataset = "antt_pracaspedagio",
#'   column_order_by = "int_idapracaspedagio"
#' )
#' }
#'
#' @seealso 
#' \code{\link{get_metadados}} For dataset structure information
#' \code{\link{get_metadados_filterable_columns}} For available filterable columns
#'
#' @note
#' - The function automatically handles pagination (100 records per page)
#' - Process stops when an empty page is encountered  
#' - Consider error handling for potential network failures
#' - Very old data (before 2000) may not be available
#'
#' @section Warning:
#' - Queries for years with large datasets may take several minutes
#' - The function does not implement caching between executions
#' - Ensure stable internet connection
get_dataset <- function(year = 2000, 
                       dataset = "antt_pracaspedagio", 
                       column_order_by = "int_idapracaspedagio", 
                       column_filter_year = "int_ano") {
  
  base_url <- paste0("https://ontl-apim.infrasa.gov.br/api/public/datasets/", dataset)
  all_records <- list()
  page <- 1
  per_page <- 100
  
  cat("Buscando dados para o ano", year, "...\n")
  
  while (TRUE) {
    params <- list(
      per_page = per_page,
      page = page,
      order_by = column_order_by
    )
    params[[column_filter_year]] <- year
    
    tryCatch({
      response <- .make_request(base_url, params = params)
      data <- response$data
      
      if (length(data) == 0 || is.null(data)) {
        break
      }
      
      all_records <- c(all_records, data)
      cat("Página", page, "-", length(data), "registros\n")
      page <- page + 1
      
      # Pequena pausa para não sobrecarregar a API
      Sys.sleep(0.1)
      
    }, error = function(e) {
      warning(paste("Erro ao buscar dados para o ano", year, ":", e$message))
      break
    })
  }
  
  cat("Total de registros encontrados:", length(all_records), "\n")
  
  # Converte para tibble
  if (length(all_records) > 0) {
    df <- dplyr::bind_rows(lapply(all_records, tibble::as_tibble))
    return(df)
  } else {
    return(tibble::tibble())
  }
}
