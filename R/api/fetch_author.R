#' Fetch publications for a given author from OpenAlex
#'
#' Searches for an author by name via the OpenAlex API and retrieves
#' their associated publications as a data.frame.
#'
#' @param last_name  character. Author's last name.
#' @param first_name character. Author's first name.
#' @param max_results integer. Maximum number of works to retrieve (default: 200).
#' @param verbose    logical. If TRUE, display a message with the author query (default: FALSE).
#'
#' @return A data.frame of publications (empty skeleton for now).
#'   If multiple author candidates are found, returns a data.frame of
#'   candidates with attribute \code{attr(result, "multiple_candidates") = TRUE}.
#'
#' @importFrom dplyr tibble select
#' @export
#'
#' @examples
#' \dontrun{
#'   works <- fetch_author("Gorwood", "Philip")
#'   works <- fetch_author("Gorwood", "Philip", verbose = TRUE)
#' }
fetch_author <- function(last_name, first_name, max_results = 200, verbose = FALSE) {
  
  # --- Input validation -------------------------------------------------------
  stopifnot(
    is.character(last_name),  length(last_name)  == 1, nchar(last_name)  > 0,
    is.character(first_name), length(first_name) == 1, nchar(first_name) > 0,
    is.numeric(max_results),  max_results >= 1
  )
  
  author_query <- paste(first_name, last_name)
  
  # --- Placeholder : empty data.frame -----------------------------------------
  # TODO S1.1 : replace with actual oa_fetch() call (openalexR)
  
  empty_works <- dplyr::tibble(
    work_id        = character(),
    title          = character(),
    year           = integer(),
    journal        = character(),
    doi            = character(),
    cited_by_count = integer(),
    authorships    = list()    # list-column : unnested downstream
  )
  
  if (verbose) {
    message("Searching for author: ", author_query)
  }
  
  message(
    "fetch_author() called for: ", author_query, "\n",
    "-> API call not yet implemented (Sprint S1.1)\n",
    "-> Returning empty data.frame."
  )
  
  return(empty_works)
}
