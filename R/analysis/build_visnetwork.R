# =============================================================================
# R/analysis/build_visnetwork.R
# Conversion igraph → tables visNetwork avec encodage visuel
# =============================================================================
# Deux graphes distincts exposés :
#   - ego → coauteurs  : couleur arête = position de l'EGO
#   - coauteurs → ego  : couleur arête = position du COAUTEUR
#
# Forme des nœuds (statut du CO-AUTEUR dans les articles partagés) :
#   star     : majorité 1er auteur  |  ou égalité milieu/1er
#   diamond  : égalité 1er/dernier  (± milieu)
#   circle   : majorité dernier     |  ou égalité milieu/dernier
#   square   : majorité milieu      |  cas résiduels
#
# Encodage couleur arête — simplexe Δ² avec shrinkage entropique :
#   first=rouge (1,0,0)  last=bleu (0,0,1)  middle=jaune (1,1,0)
#   couleur = (1 − H(p)^γ) · Σ pᵢ cᵢ    γ=3 par défaut
# =============================================================================


# =============================================================================
# Helpers internes
# =============================================================================

#' Entropie de Shannon normalisée en base 3 (H ∈ [0,1])
#' @keywords internal
.entropy3 <- function(p) {
  -sum(ifelse(p > 0, p * log(p) / log(3), 0))
}

#' Mélange colorimétrique avec shrinkage entropique
#' @keywords internal
.blend_position_color <- function(n_first, n_last, n_middle, gamma = 3) {
  total <- n_first + n_last + n_middle
  if (total == 0L) return("#AAAAAA")
  p        <- c(n_first, n_last, n_middle) / total
  H_shrunk <- .entropy3(p) ^ gamma
  c_first  <- c(1, 0, 0)
  c_last   <- c(0, 0, 1)
  c_middle <- c(1, 1, 0)
  rgb_out  <- (1 - H_shrunk) * (p[1] * c_first + p[2] * c_last + p[3] * c_middle)
  grDevices::rgb(rgb_out[1], rgb_out[2], rgb_out[3])
}

#' Forme du nœud selon la position dominante du CO-AUTEUR
#'
#' Règles (sur les comptages first/last/middle du co-auteur) :
#'   star    : n_f > n_l  & n_f > n_m   (majorité 1er)
#'             n_f = n_m  & n_f > n_l   (égalité milieu/1er)
#'   diamond : n_f = n_l  & n_f >= n_m  (égalité 1er/dernier)
#'   circle  : n_l > n_f  & n_l > n_m   (majorité dernier)
#'             n_l = n_m  & n_l > n_f   (égalité milieu/dernier)
#'   square  : tous les autres cas (majorité milieu, triplets égaux…)
#'
#' @keywords internal
.coauthor_shape <- function(n_first, n_last, n_middle) {
  dplyr::case_when(
    # star : 1er dominant ou égalité milieu/1er
    (n_first > n_last  & n_first > n_middle) |
      (n_first == n_middle & n_first > n_last)  ~ "star",
    # diamond : égalité 1er/dernier
    n_first == n_last & n_first >= n_middle      ~ "diamond",
    # circle : dernier dominant ou égalité milieu/dernier
    (n_last > n_first  & n_last > n_middle) |
      (n_last == n_middle & n_last > n_first)   ~ "circle",
    # square : majorité milieu + cas résiduels
    TRUE                                         ~ "square"
  )
}

#' Compter les positions de `focal_id` dans ses articles communs avec chaque partenaire
#'
#' @param coauthors tibble. Sortie de unnest_authorships().
#' @param focal_id  character. ID OpenAlex de l'auteur dont on compte les positions.
#' @param gamma     numeric. Exposant de shrinkage pour la couleur.
#'
#' @return Tibble : partner_id, n_first, n_last, n_middle, color.
#' @keywords internal
.position_color_per_partner <- function(coauthors, focal_id, gamma = 3) {
  focal_works <- coauthors |>
    dplyr::filter(author_id == focal_id) |>
    dplyr::select(work_id, focal_position = author_position)
  
  partners <- coauthors |>
    dplyr::filter(author_id != focal_id,
                  work_id %in% focal_works$work_id) |>
    dplyr::select(work_id, partner_id = author_id)
  
  dplyr::left_join(partners, focal_works, by = "work_id") |>
    dplyr::filter(!is.na(focal_position)) |>
    dplyr::group_by(partner_id) |>
    dplyr::summarise(
      n_first  = sum(focal_position == "first"),
      n_last   = sum(focal_position == "last"),
      n_middle = sum(focal_position == "middle"),
      .groups  = "drop"
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      color = .blend_position_color(n_first, n_last, n_middle, gamma = gamma)
    ) |>
    dplyr::ungroup()
}


# =============================================================================
# Fonction principale : tables nœuds + arêtes
# =============================================================================

#' Build visNetwork tables for one edge direction
#'
#' Called twice by the rendering functions — once per direction.
#' Edge color encodes the position of `color_focal_id` in shared articles.
#' Node shape encodes the coauthor's own position in shared articles.
#'
#' @param g             igraph. Directed ego network.
#' @param coauthors     tibble or NULL. unnest_authorships() output.
#' @param ego_id        character or NULL.
#' @param direction     "ego_to_co" or "co_to_ego".
#' @param gamma         numeric. Shrinkage exponent (default 3).
#' @param size_range    numeric(2).
#' @param width_range   numeric(2).
#'
#' @return list(nodes, edges).
#' @keywords internal
.build_tables_one_direction <- function(g,
                                        coauthors,
                                        ego_id,
                                        direction    = c("ego_to_co", "co_to_ego"),
                                        gamma        = 3,
                                        size_range   = c(8, 40),
                                        width_range  = c(0.5, 8)) {
  
  direction <- match.arg(direction)
  verts     <- igraph::as_data_frame(g, what = "vertices")
  all_edges <- igraph::as_data_frame(g, what = "edges")
  
  # Filtrer les arêtes selon l'attribut `direction` posé dans build_ego_network()
  edges <- if (direction == "ego_to_co") {
    all_edges |> dplyr::filter(direction == "ego_to_co")
  } else {
    all_edges |> dplyr::filter(direction == "co_to_ego")
  }
  
  # --- Nœuds -----------------------------------------------------------------
  # Forme = statut du co-auteur dans ses articles communs avec ego
  coauth_pos <- if (!is.null(coauthors) && !is.null(ego_id)) {
    # On récupère les positions de chaque co-auteur (focal = co-auteur)
    coauthor_ids <- unique(c(edges$from, edges$to))
    coauthor_ids <- coauthor_ids[coauthor_ids != ego_id]
    
    purrr::map_dfr(coauthor_ids, function(cid) {
      res <- .position_color_per_partner(coauthors, cid, gamma)
      res |>
        dplyr::filter(partner_id == ego_id) |>
        dplyr::mutate(coauthor_id = cid)
    }) |>
      dplyr::select(coauthor_id, n_first, n_last, n_middle)
  } else {
    NULL
  }
  
  nodes <- dplyr::tibble(
    id        = verts$name,
    label     = ifelse(
      verts$is_ego | (!is.na(verts$n_works) & verts$n_works >= 5),
      verts$label, ""
    ),
    title     = paste0(
      "<b>", verts$label, "</b><br>",
      "Co-publications avec ego : ", verts$n_works
    ),
    value     = ifelse(
      verts$is_ego,
      max(verts$n_works, na.rm = TRUE) * 2,
      verts$n_works
    ),
    color     = ifelse(
      verts$is_ego, "#E63946",
      ifelse(verts$n_works >= 10, "#1D3557",
             ifelse(verts$n_works >= 5, "#457B9D", "#A8DADC"))
    ),
    font.size = ifelse(verts$is_ego, 20L, 12L),
    group     = ifelse(verts$is_ego, "ego", "coauthor")
  ) |>
    dplyr::mutate(value = scales::rescale(value, to = size_range))
  
  # Ajout forme selon position co-auteur
  shape_supplement <- if (!is.null(coauth_pos)) {
    coauth_pos |>
      dplyr::rowwise() |>
      dplyr::mutate(
        shape     = .coauthor_shape(n_first, n_last, n_middle),
        title_pos = paste0(
          "<br>R\u00f4le du co-auteur \u2014 ",
          "1er : ", n_first,
          " \u00b7 Dernier : ", n_last,
          " \u00b7 Milieu : ", n_middle
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::select(id = coauthor_id, shape, title_pos)
  } else {
    NULL
  }
  
  if (!is.null(shape_supplement)) {
    nodes <- nodes |>
      dplyr::left_join(shape_supplement, by = "id") |>
      dplyr::mutate(
        shape = dplyr::case_when(
          is_ego              ~ "dot",
          !is.na(shape)       ~ shape,
          TRUE                ~ "square"
        ),
        title = paste0(title, dplyr::coalesce(title_pos, ""))
      ) |>
      dplyr::select(-title_pos)
  } else {
    nodes <- nodes |>
      dplyr::mutate(shape = ifelse(is_ego, "dot", "square"))
  }
  
  # --- Arêtes colorées -------------------------------------------------------
  # Couleur = position du focal (ego si ego_to_co, co-auteur si co_to_ego)
  if (!is.null(coauthors) && !is.null(ego_id)) {
    
    if (direction == "ego_to_co") {
      color_focal_id <- ego_id
      join_key       <- "to"         # partenaire = co-auteur (colonne `to`)
      direction_label <- "Rôle de <i>l'ego</i>"
    } else {
      # Pour co_to_ego, la couleur encode chaque co-auteur individuellement
      # → on calcule via .position_color_per_partner pour chaque co-auteur
      join_key        <- "from"
      direction_label <- "Rôle du <i>co-auteur</i>"
    }
    
    if (direction == "ego_to_co") {
      ego_colors <- .position_color_per_partner(coauthors, ego_id, gamma)
      edges <- edges |>
        dplyr::left_join(
          ego_colors |> dplyr::select(to = partner_id, n_first, n_last, n_middle, color),
          by = "to"
        )
    } else {
      coauth_color_rows <- purrr::map_dfr(unique(edges$from), function(cid) {
        res <- .position_color_per_partner(coauthors, cid, gamma)
        res |>
          dplyr::filter(partner_id == ego_id) |>
          dplyr::mutate(from = cid) |>
          dplyr::select(from, n_first, n_last, n_middle, color)
      })
      edges <- edges |>
        dplyr::left_join(coauth_color_rows, by = "from")
    }
    
    edges <- edges |>
      dplyr::mutate(
        color  = dplyr::coalesce(color, "#AAAAAA"),
        n_first  = dplyr::coalesce(n_first,  0L),
        n_last   = dplyr::coalesce(n_last,   0L),
        n_middle = dplyr::coalesce(n_middle, 0L),
        arrows = "to",
        title  = paste0(
          "<b>", direction_label, "</b><br>",
          weight, " article(s) commun(s)<br>",
          "1er : ", n_first,
          " · Dernier : ", n_last,
          " · Milieu : ", n_middle
        )
      )
  } else {
    edges <- edges |>
      dplyr::mutate(
        color  = "#AAAAAA",
        arrows = "to",
        title  = paste0(weight, " article(s) commun(s)")
      )
  }
  
  edges <- edges |>
    dplyr::mutate(width = scales::rescale(weight, to = width_range))
  
  list(nodes = nodes, edges = edges)
}


# =============================================================================
# API publique
# =============================================================================

#' Build visNetwork tables — ego → coauthors direction
#'
#' Edge color encodes the ego's position in shared articles.
#' Node shape encodes each coauthor's own dominant position.
#'
#' @inheritParams .build_tables_one_direction
#' @export
build_visnetwork_ego_to_co <- function(g, coauthors = NULL, ego_id = NULL,
                                       gamma = 3,
                                       size_range  = c(8, 40),
                                       width_range = c(0.5, 8)) {
  stopifnot(igraph::is_igraph(g))
  .build_tables_one_direction(g, coauthors, ego_id,
                              direction = "ego_to_co",
                              gamma = gamma,
                              size_range = size_range,
                              width_range = width_range)
}

#' Build visNetwork tables — coauthors → ego direction
#'
#' Edge color encodes each coauthor's position in shared articles.
#' Node shape encodes each coauthor's own dominant position.
#'
#' @inheritParams .build_tables_one_direction
#' @export
build_visnetwork_co_to_ego <- function(g, coauthors = NULL, ego_id = NULL,
                                       gamma = 3,
                                       size_range  = c(8, 40),
                                       width_range = c(0.5, 8)) {
  stopifnot(igraph::is_igraph(g))
  .build_tables_one_direction(g, coauthors, ego_id,
                              direction = "co_to_ego",
                              gamma = gamma,
                              size_range = size_range,
                              width_range = width_range)
}


#' Render a visNetwork from pre-built node/edge tables
#'
#' @param tables   list(nodes, edges). Output of build_visnetwork_ego_to_co()
#'                 or build_visnetwork_co_to_ego().
#' @param title    character. Short title displayed above the network.
#' @param height   character. CSS height.
#' @param seed     integer. Layout seed.
#'
#' @return A visNetwork htmlwidget.
#' @export
render_visnetwork <- function(tables,
                              title  = "",
                              height = "700px",
                              seed   = 42L) {
  
  stopifnot(is.list(tables), all(c("nodes", "edges") %in% names(tables)))
  
  legend_nodes <- data.frame(
    label = c("Ego", "\u226510 articles", "5\u20139 articles", "<5 articles"),
    color = c("#E63946", "#1D3557", "#457B9D", "#A8DADC"),
    shape = "dot", size = c(24, 16, 14, 12),
    stringsAsFactors = FALSE
  )
  
  legend_shapes <- data.frame(
    label = c("Majorit\u00e9 1er auteur", "\u00c9galit\u00e9 1er/dernier",
              "Majorit\u00e9 dernier", "Majorit\u00e9 milieu"),
    color = "#888888",
    shape = c("star", "diamond", "circle", "square"),
    size  = 14,
    stringsAsFactors = FALSE
  )
  
  legend_edges <- data.frame(
    label = c("1er auteur (rouge)", "Dernier auteur (bleu)",
              "Milieu (jaune)", "Mix \u00e9quilibr\u00e9 (noir)"),
    color = c("#FF0000", "#0000FF", "#FFFF00", "#111111"),
    width = 3, arrows = "to",
    stringsAsFactors = FALSE
  )
  
  visNetwork::visNetwork(
    nodes  = tables$nodes,
    edges  = tables$edges,
    width  = "100%",
    height = height,
    main   = title
  ) |>
    visNetwork::visNodes(borderWidth = 1.5) |>
    visNetwork::visEdges(
      arrows = "to",
      smooth = list(enabled = TRUE, type = "curvedCW", roundness = 0.15)
    ) |>
    visNetwork::visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
      nodesIdSelection = TRUE
    ) |>
    visNetwork::visPhysics(
      solver           = "forceAtlas2Based",
      forceAtlas2Based = list(gravitationalConstant = -80)
    ) |>
    visNetwork::visInteraction(navigationButtons = TRUE, tooltipDelay = 100) |>
    visNetwork::visLayout(randomSeed = seed) |>
    visNetwork::visLegend(
      addNodes  = dplyr::bind_rows(legend_nodes, legend_shapes),
      addEdges  = legend_edges,
      useGroups = FALSE,
      width     = 0.18,
      position  = "right"
    )
}