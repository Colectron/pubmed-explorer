# =============================================================================
# R/analysis/filter_network.R
# Filtrage du graphe ego avant visualisation
# =============================================================================
# Opérateurs appliqués dans l'ordre suivant :
#   1. Filtre sur le poids des arêtes  (nb articles co-signés)
#   2. Filtre sur la position de l'auteur cible dans les articles
#   3. Vue top-N co-auteurs  (garde les N arêtes de poids maximum)
#   4. Suppression des nœuds isolés
#
# Chaque opérateur est indépendant et composable. L'ordre est délibéré :
# top_n s'applique après les filtres qualitatifs pour que le "top" reflète
# déjà les critères de sélection.
# =============================================================================

#' Filter a co-authorship ego network
#'
#' Applies a cascade of optional filters to the igraph object returned by
#' build_ego_network().  All parameters default to "no filter" (NULL / FALSE)
#' so the function is always safe to call with partial arguments.
#'
#' @param g              igraph. Ego network (output of build_ego_network()).
#' @param min_weight     integer or NULL. Keep edges with weight >= min_weight.
#'                       NULL disables the filter.
#' @param author_positions character vector or NULL.  Keep only edges whose
#'                       ego-authored articles include ≥1 article where ego
#'                       held one of the specified positions.
#'                       Accepted values: "first", "middle", "last", "corresponding".
#'                       Requires the `coauthors` argument (see below).
#'                       NULL disables the filter.
#' @param coauthors      tibble or NULL. Output of unnest_authorships(), needed
#'                       only when `author_positions` is not NULL.
#' @param ego_id         character or NULL. OpenAlex ID of the ego node, needed
#'                       only when `author_positions` is not NULL.
#' @param top_n          integer or NULL. Retain the top_n co-authors by edge
#'                       weight (ego node always kept). NULL disables the filter.
#' @param remove_isolates logical. If TRUE, remove nodes with degree 0 after
#'                       all other filters. Default FALSE.
#'
#' @return An igraph object, a (possibly strict) subgraph of `g`.
#'
#' @importFrom igraph delete_edges E V delete_vertices degree subgraph.edges
#' @importFrom dplyr filter pull distinct
#' @export
filter_network <- function(g,
                           min_weight       = NULL,
                           author_positions = NULL,
                           coauthors        = NULL,
                           ego_id           = NULL,
                           top_n            = NULL,
                           remove_isolates  = FALSE) {
  
  stopifnot(igraph::is_igraph(g))
  
  # --- 1. Filtre poids --------------------------------------------------------
  # On filtre sur un seul sens (ego_to_co) puis on synchronise co_to_ego,
  # pour garantir que les deux directions d'une paire restent toujours ensemble.
  if (!is.null(min_weight)) {
    stopifnot(is.numeric(min_weight), min_weight >= 1)
    keep_pairs <- which(
      igraph::E(g)$weight  >= min_weight &
        igraph::E(g)$direction == "ego_to_co"
    )
    co_ids_kept <- igraph::tail_of(g, keep_pairs)$name
    edges_to_drop <- which(
      igraph::E(g)$weight < min_weight |
        (igraph::E(g)$direction == "co_to_ego" &
           !(igraph::head_of(g, igraph::E(g))$name %in% co_ids_kept))
    )
    if (length(edges_to_drop) > 0)
      g <- igraph::delete_edges(g, edges_to_drop)
  }
  
  # --- 2. Filtre position -----------------------------------------------------
  # Pour chaque co-auteur, on vérifie si l'ego occupait l'une des positions
  # demandées dans ≥1 article commun.
  # La position est portée par `author_position` dans la table `coauthors`
  # (colonne de l'EGO, pas du co-auteur).
  if (!is.null(author_positions)) {
    stopifnot(
      is.character(author_positions),
      !is.null(coauthors), is.data.frame(coauthors),
      !is.null(ego_id),    is.character(ego_id)
    )
    valid_pos <- c("first", "middle", "last", "corresponding")
    unknown   <- setdiff(author_positions, valid_pos)
    if (length(unknown) > 0)
      warning("Unknown position(s) ignored: ", paste(unknown, collapse = ", "))
    
    # Articles où l'ego a l'une des positions requises
    eligible_works <- coauthors |>
      dplyr::filter(
        author_id        == ego_id,
        author_position  %in% author_positions
      ) |>
      dplyr::pull(work_id) |>
      unique()
    
    if (length(eligible_works) == 0) {
      message("No works match the requested author positions — graph will be empty.")
      return(.empty_graph())
    }
    
    # Co-auteurs présents dans ces articles
    eligible_coauthors <- coauthors |>
      dplyr::filter(work_id %in% eligible_works, author_id != ego_id) |>
      dplyr::pull(author_id) |>
      unique()
    
    # Supprimer les arêtes dont la cible n'est pas un co-auteur éligible
    edge_df  <- igraph::as_data_frame(g, what = "edges")
    drop_idx <- which(!(edge_df$to %in% eligible_coauthors) &
                        !(edge_df$from %in% eligible_coauthors))
    if (length(drop_idx) > 0)
      g <- igraph::delete_edges(g, drop_idx)
  }
  
  # --- 3. Top-N co-auteurs ---------------------------------------------------
  # Sélection sur ego_to_co uniquement, puis synchronisation co_to_ego.
  if (!is.null(top_n)) {
    stopifnot(is.numeric(top_n), top_n >= 1)
    edge_df <- igraph::as_data_frame(g, what = "edges")
    ego_edges <- edge_df |> dplyr::filter(direction == "ego_to_co")
    if (nrow(ego_edges) > top_n) {
      keep_to <- ego_edges |>
        dplyr::arrange(dplyr::desc(weight)) |>
        dplyr::slice_head(n = top_n) |>
        dplyr::pull(to)
      drop_idx <- which(
        (edge_df$direction == "ego_to_co" & !(edge_df$to   %in% keep_to)) |
          (edge_df$direction == "co_to_ego" & !(edge_df$from %in% keep_to))
      )
      if (length(drop_idx) > 0)
        g <- igraph::delete_edges(g, drop_idx)
    }
  }
  
  # --- 4. Suppression des isolés ---------------------------------------------
  # Pour un graphe dirigé, un nœud isolé a in-degree + out-degree = 0
  if (isTRUE(remove_isolates)) {
    iso <- which(igraph::degree(g, mode = "all") == 0 & !igraph::V(g)$is_ego)
    if (length(iso) > 0)
      g <- igraph::delete_vertices(g, iso)
  }
  
  return(g)
}


# =============================================================================
# Helpers privés
# =============================================================================

#' @keywords internal
.empty_graph <- function() {
  igraph::make_empty_graph(n = 0, directed = FALSE)
}