# =============================================================================
# R/api/parse_records.R
# Aplatissement des list-columns retournées par fetch_author()
# =============================================================================

#' Unnest authorships into a flat co-authorship data.frame
#'
#' Takes the `authorships` list-column from fetch_author() output and
#' returns one row per (article x author), suitable for network construction.
#'
#' @param works tibble. Output of fetch_author().
#'
#' @return A tibble with columns:
#'   work_id, year, author_id, author_name, author_position, is_corresponding
#'
#' @importFrom dplyr select mutate
#' @importFrom purrr map2_dfr
#' @export
unnest_authorships <- function(works) {
  
  stopifnot(
    is.data.frame(works),
    "authorships" %in% names(works),
    "work_id"     %in% names(works)
  )
  
  purrr::map2_dfr(
    works$work_id,
    works$authorships,
    function(wid, auth_df) {
      if (is.null(auth_df) || nrow(auth_df) == 0) return(NULL)
      dplyr::tibble(
        work_id          = wid,
        author_id        = auth_df$id,
        author_name      = auth_df$display_name,
        author_position  = auth_df$author_position,
        is_corresponding = auth_df$is_corresponding
      )
    }
  ) |>
    # Joindre l'année depuis works (utile pour filtrer par période)
    dplyr::left_join(
      dplyr::select(works, work_id, year),
      by = "work_id"
    )
}


#' Unnest affiliations into a flat institution data.frame
#'
#' Takes the `authorships` list-column and extracts institution-level data,
#' one row per (article x author x institution).
#'
#' @param works tibble. Output of fetch_author().
#'
#' @return A tibble with columns:
#'   work_id, year, author_id, author_name,
#'   institution_id, institution_name, country_code, institution_type
#'
#' @importFrom dplyr select tibble left_join
#' @importFrom purrr map2_dfr
#' @export
unnest_affiliations <- function(works) {
  
  stopifnot(
    is.data.frame(works),
    "authorships" %in% names(works)
  )
  
  purrr::map2_dfr(
    works$work_id,
    works$authorships,
    function(wid, auth_df) {
      if (is.null(auth_df) || nrow(auth_df) == 0) return(NULL)
      
      purrr::map2_dfr(
        auth_df$id,
        auth_df$affiliations,
        function(aid, aff_df) {
          if (is.null(aff_df) || nrow(aff_df) == 0) return(NULL)
          dplyr::tibble(
            work_id          = wid,
            author_id        = aid,
            institution_id   = aff_df$id,
            institution_name = aff_df$display_name,
            country_code     = aff_df$country_code,
            institution_type = aff_df$type
          )
        }
      )
    }
  ) |>
    dplyr::left_join(
      dplyr::select(works, work_id, year),
      by = "work_id"
    )
}