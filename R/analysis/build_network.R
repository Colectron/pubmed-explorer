# =============================================================================
# R/analysis/build_network.R
# Construction du graphe de co-auteurs (ego network rang 0)
# =============================================================================
# Rang 0 : nœud central (auteur cible) + tous ses co-auteurs directs.
#
# Le graphe est DIRIGÉ : deux arêtes par paire (ego, v) —
#   ego → v  : représente le rôle de l'ego dans les articles communs
#   v → ego  : représente le rôle de v dans les articles communs
#
# Le poids de chaque arête est identique (nb d'articles co-signés) ;
# l'encodage des rôles est délégué à build_visnetwork.R qui lit
# author_position dans la table coauthors.
# =============================================================================

#' Build ego co-authorship network (rank 0, directed)
#'
#' From a flat co-authorship data.frame (output of unnest_authorships()),
#' builds a weighted **directed** graph where:
#'   - nodes  = target author + all co-authors
#'   - edges  = two directed edges per pair: ego→coauthor and coauthor→ego
#'   - weight = number of co-authored articles (same on both directions)
#'
#' Having two directed edges per pair allows build_visnetwork_tables() to
#' encode the ego's position on the outgoing edge and the coauthor's position
#' on the incoming edge, without any manual edge reversal.
#'
#' @param coauthors  tibble. Output of unnest_authorships().
#' @param ego_id     character. OpenAlex ID of the target author.
#' @param ego_name   character. Display name of the target author.
#'
#' @return A **directed** igraph object with node attributes:
#'   name (OpenAlex ID), label (display name), is_ego (logical),
#'   n_works (number of co-authored articles with ego)
#'   and edge attribute: weight (co-authored article count).
#'
#' @importFrom dplyr filter select distinct group_by summarise n left_join
#'   bind_rows tibble mutate rename
#' @importFrom igraph graph_from_data_frame V
#' @export
build_ego_network <- function(coauthors, ego_id, ego_name) {
  
  stopifnot(
    is.data.frame(coauthors),
    is.character(ego_id),   length(ego_id)   == 1,
    is.character(ego_name), length(ego_name) == 1,
    all(c("work_id", "author_id", "author_name") %in% names(coauthors))
  )
  
  # --- 1. Articles où l'ego apparaît -----------------------------------------
  ego_works <- coauthors |>
    dplyr::filter(author_id == ego_id) |>
    dplyr::select(work_id) |>
    dplyr::distinct()
  
  # --- 2. Co-auteurs sur ces articles (ego exclu) ----------------------------
  coauth_filtered <- coauthors |>
    dplyr::filter(
      work_id   %in% ego_works$work_id,
      author_id != ego_id
    )
  
  # --- 3. Poids des paires : nb articles co-signés ---------------------------
  pair_weights <- coauth_filtered |>
    dplyr::group_by(author_id, author_name) |>
    dplyr::summarise(weight = dplyr::n(), .groups = "drop")
  
  # --- 4. Arêtes dirigées : ego → coauteur ET coauteur → ego ----------------
  edges_ego_to_co <- pair_weights |>
    dplyr::mutate(from = ego_id, to = author_id) |>
    dplyr::select(from, to, weight)
  
  edges_co_to_ego <- pair_weights |>
    dplyr::mutate(from = author_id, to = ego_id) |>
    dplyr::select(from, to, weight)
  
  edges <- dplyr::bind_rows(edges_ego_to_co, edges_co_to_ego)
  
  # --- 5. Nœuds avec attributs -----------------------------------------------
  nodes_coauth <- coauth_filtered |>
    dplyr::group_by(author_id) |>
    dplyr::summarise(
      author_name = names(sort(table(author_name), decreasing = TRUE))[1],
      .groups = "drop"
    ) |>
    dplyr::rename(name = author_id, label = author_name) |>
    dplyr::mutate(is_ego = FALSE)
  
  node_ego <- dplyr::tibble(
    name   = ego_id,
    label  = ego_name,
    is_ego = TRUE
  )
  
  nodes <- dplyr::bind_rows(node_ego, nodes_coauth)
  
  # n_works : nb d'articles communs avec l'ego (symétrique, lu sur un seul sens)
  n_works_per_node <- edges_ego_to_co |>
    dplyr::group_by(name = to) |>
    dplyr::summarise(n_works = sum(weight), .groups = "drop")
  
  nodes <- nodes |>
    dplyr::left_join(n_works_per_node, by = "name", relationship = "one-to-one") |>
    dplyr::mutate(n_works = ifelse(is_ego, nrow(ego_works), n_works))
  
  # --- 6. Construction igraph (dirigé) ---------------------------------------
  g <- igraph::graph_from_data_frame(
    d        = edges,
    directed = TRUE,          # ← changement clé vs version précédente
    vertices = nodes
  )
  
  return(g)
}