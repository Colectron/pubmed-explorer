# =============================================================================
# R/analysis/build_network.R
# Construction du graphe de co-auteurs (ego network rang 0)
# =============================================================================
# Rang 0 : nœud central (auteur cible) + tous ses co-auteurs directs
#          arête = co-signature d'au moins 1 article
#          poids = nombre d'articles co-signés
# =============================================================================

#' Build ego co-authorship network (rank 0)
#'
#' From a flat co-authorship data.frame (output of unnest_authorships()),
#' builds a weighted undirected graph where:
#'   - nodes  = target author + all co-authors
#'   - edges  = co-authorship on ≥1 article
#'   - weight = number of co-authored articles
#'
#' @param coauthors  tibble. Output of unnest_authorships().
#' @param ego_id     character. OpenAlex ID of the target author.
#' @param ego_name   character. Display name of the target author.
#'
#' @return An igraph object with node attributes:
#'   name (OpenAlex ID), label (display name), is_ego (logical),
#'   n_works (number of co-authored articles with ego)
#'   and edge attribute: weight (co-authored article count)
#'
#' @importFrom dplyr filter select distinct group_by summarise n left_join
#' @importFrom igraph graph_from_data_frame V
#' @export
build_ego_network <- function(coauthors, ego_id, ego_name) {
  
  stopifnot(
    is.data.frame(coauthors),
    is.character(ego_id), length(ego_id) == 1,
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
  
  # --- 3. Arêtes : ego <-> chaque co-auteur, poids = nb articles communs -----
  edges <- coauth_filtered |>
    dplyr::group_by(author_id, author_name) |>
    dplyr::summarise(weight = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(
      from = ego_id,
      to   = author_id
    ) |>
    dplyr::select(from, to, weight)
  
  # --- 4. Nœuds avec attributs -----------------------------------------------
  # Dédoublonnage par author_id (clé stable) : garder le nom le plus fréquent
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
  
  # n_works : dédoublonnage explicite par author_id (to)
  n_works_per_node <- edges |>
    dplyr::group_by(name = to) |>
    dplyr::summarise(n_works = sum(weight), .groups = "drop")
  
  nodes <- nodes |>
    dplyr::left_join(n_works_per_node, by = "name", relationship = "one-to-one") |>
    dplyr::mutate(n_works = ifelse(is_ego, nrow(ego_works), n_works))
  
  # --- 5. Construction igraph ------------------------------------------------
  g <- igraph::graph_from_data_frame(
    d        = edges,
    directed = FALSE,
    vertices = nodes
  )
  
  return(g)
}