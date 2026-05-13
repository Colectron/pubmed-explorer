# =============================================================================
# R/api/fetch_author.R
# Requêtage OpenAlex : recherche auteur + récupération de ses publications
# =============================================================================

#' Fetch publications for a given author from OpenAlex
#'
#' Searches for an author by name via the OpenAlex API (openalexR),
#' disambiguates candidates when needed, and retrieves publications.
#'
#' @param last_name    character. Author's last name.
#' @param first_name   character. Author's first name.
#' @param max_results  integer. Maximum number of works to retrieve (default: 200).
#' @param verbose      logical. If TRUE, print progress messages (default: FALSE).
#'
#' @return A tibble of publications with columns:
#'   work_id, title, year, journal, doi, cited_by_count, authorships (list-col).
#'   If disambiguation is ambiguous, returns a candidate tibble with
#'   \code{attr(result, "multiple_candidates") = TRUE}.
#'   Returns an empty tibble if no author is found.
#'
#' @importFrom openalexR oa_fetch
#' @importFrom dplyr tibble arrange desc select any_of
#' @export
fetch_author <- function(last_name, first_name, max_results = 200, verbose = FALSE) {
  
  stopifnot(
    is.character(last_name),  length(last_name)  == 1, nchar(last_name)  > 0,
    is.character(first_name), length(first_name) == 1, nchar(first_name) > 0,
    is.numeric(max_results),  max_results >= 1,
    is.logical(verbose)
  )
  
  author_query <- paste(first_name, last_name)
  if (verbose) message("Searching OpenAlex for: ", author_query)
  
  # --- 1. Recherche des candidats -------------------------------------------
  candidates <- tryCatch(
    openalexR::oa_fetch(
      entity              = "authors",
      display_name.search = author_query,
      verbose             = FALSE
    ),
    error = function(e) {
      warning("OpenAlex author search failed: ", conditionMessage(e))
      NULL
    }
  )
  
  if (is.null(candidates) || nrow(candidates) == 0) {
    if (verbose) message("No author found for: ", author_query)
    return(.empty_works_df())
  }
  
  # --- 2. Désambiguïsation --------------------------------------------------
  # disambiguate_author() centralise toute la logique de résolution :
  # ORCID > dominance > overlap d'institutions > ambiguous
  disambiguation <- disambiguate_author(candidates, verbose = verbose)
  
  # Cas ambiguous : on ne peut pas choisir automatiquement
  # -> retourner la liste des candidats pour que l'UI propose un choix
  if (disambiguation$method == "ambiguous") {
    out <- disambiguation$resolved |>
      dplyr::select(dplyr::any_of(c(
        "id", "display_name", "orcid",
        "works_count", "cited_by_count", "h_index"
      )))
    attr(out, "multiple_candidates") <- TRUE
    return(out)
  }
  
  # Candidat résolu (single / orcid / dominant)
  resolved <- disambiguation$resolved
  
  # --- 3. Récupération des publications -------------------------------------
  # Si des IDs fragments ont été identifiés (merged_ids), on les agrège
  # pour ne rater aucune publication de cet auteur
  author_ids <- c(resolved$id[1], disambiguation$merged_ids)
  
  if (verbose) message(
    "Fetching works for: ", resolved$display_name[1],
    " [method: ", disambiguation$method, "]",
    if (length(disambiguation$merged_ids) > 0)
      paste0(" + ", length(disambiguation$merged_ids), " merged fragment(s)")
  )
  
  works_list <- lapply(author_ids, function(aid) {
    tryCatch(
      openalexR::oa_fetch(
        entity     = "works",
        author.id  = aid,
        count_only = FALSE,
        verbose    = FALSE
      ),
      error = function(e) {
        warning("Works fetch failed for ", aid, ": ", conditionMessage(e))
        NULL
      }
    )
  })
  
  works <- dplyr::bind_rows(Filter(Negate(is.null), works_list))
  
  if (nrow(works) == 0) {
    if (verbose) message("No works found.")
    return(.empty_works_df())
  }
  
  # Dédoublonnage (un article peut apparaître dans plusieurs fragments)
  works <- works[!duplicated(works$id), ]
  
  if (nrow(works) > max_results) {
    if (verbose) message("Truncating to ", max_results, " / ", nrow(works), " works.")
    works <- works[seq_len(max_results), ]
  }
  
  # --- 4. Mise en forme -----------------------------------------------------
  out <- dplyr::tibble(
    work_id        = works$id,
    title          = works$display_name,
    year           = works$publication_year,
    journal        = works$so,
    doi            = works$doi,
    cited_by_count = works$cited_by_count,
    authorships    = works$authorships  # list-col, traitée dans parse_records.R
  )
  
  if (verbose) message("Done. ", nrow(out), " works returned.")
  return(out)
}


# =============================================================================
# disambiguate_author() : résolution d'entité auteur
# =============================================================================
# Stratégie par priorité décroissante :
#   1. ORCID partagé          -> certitude absolue
#   2. Dominance works_count  -> heuristique forte (ratio >= 10x)
#   3. Ambiguous              -> choix manuel requis (renvoyé à l'UI)
#
# Les IDs des fragments absorbés sont retournés dans merged_ids pour
# permettre l'agrégation de leurs publications dans fetch_author().
# =============================================================================

#' @keywords internal
disambiguate_author <- function(candidates,
                                dominant_ratio = 10,
                                min_works      = 10,
                                verbose        = FALSE) {
  
  stopifnot(is.data.frame(candidates), nrow(candidates) >= 1)
  
  if (nrow(candidates) == 1)
    return(list(resolved = candidates, method = "single", merged_ids = character(0)))
  
  candidates <- dplyr::arrange(candidates, dplyr::desc(works_count))
  
  # Signal 1 : ORCID -----------------------------------------------------------
  if ("orcid" %in% names(candidates)) {
    orcid_top <- candidates$orcid[1]
    if (!is.na(orcid_top) && nchar(orcid_top) > 0) {
      same_orcid <- !is.na(candidates$orcid) & candidates$orcid == orcid_top
      merged <- candidates$id[same_orcid & candidates$id != candidates$id[1]]
      if (verbose && length(merged) > 0)
        message("ORCID: merging ", length(merged), " duplicate(s).")
      return(list(resolved = candidates[1, ], method = "orcid", merged_ids = merged))
    }
  }
  
  # Signal 2 : dominance -------------------------------------------------------
  top    <- candidates$works_count[1]
  second <- candidates$works_count[2]
  
  if (top >= min_works && (second == 0 || top / second >= dominant_ratio)) {
    merged <- .find_institution_overlap(candidates, verbose)
    if (verbose) message(
      "Dominant: ", top, " works vs ", second, ". ",
      length(merged), " fragment(s) with institution overlap."
    )
    return(list(resolved = candidates[1, ], method = "dominant", merged_ids = merged))
  }
  
  # Signal 3 : ambiguous -------------------------------------------------------
  if (verbose) message("Ambiguous: ", nrow(candidates), " candidates, manual selection needed.")
  return(list(resolved = candidates, method = "ambiguous", merged_ids = character(0)))
}


# =============================================================================
# Helpers privés
# =============================================================================

#' Trouve les fragments partageant ≥1 institution avec le candidat dominant
#' @keywords internal
.find_institution_overlap <- function(candidates, verbose = FALSE) {
  inst_top <- .extract_institution_names(candidates$last_known_institutions[[1]])
  if (length(inst_top) == 0) return(character(0))
  
  merged <- character(0)
  for (i in seq_len(nrow(candidates))[-1]) {
    inst_i <- .extract_institution_names(candidates$last_known_institutions[[i]])
    if (length(intersect(inst_top, inst_i)) > 0) {
      if (verbose) message(
        "  overlap: '", candidates$display_name[i], "' shares institution."
      )
      merged <- c(merged, candidates$id[i])
    }
  }
  merged
}

#' Extrait les noms d'institutions depuis la list-col last_known_institutions
#' @keywords internal
.extract_institution_names <- function(inst_col) {
  if (is.null(inst_col) || (length(inst_col) == 1 && is.na(inst_col)))
    return(character(0))
  if (is.data.frame(inst_col) && "display_name" %in% names(inst_col))
    return(inst_col$display_name)
  if (is.list(inst_col)) {
    out <- unlist(lapply(inst_col, function(x)
      if (is.data.frame(x) && "display_name" %in% names(x)) x$display_name
    ))
    return(if (length(out) > 0) out else character(0))
  }
  character(0)
}

#' @keywords internal
.empty_works_df <- function() {
  dplyr::tibble(
    work_id        = character(),
    title          = character(),
    year           = integer(),
    journal        = character(),
    doi            = character(),
    cited_by_count = integer(),
    authorships    = list()
  )
}