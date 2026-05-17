# =============================================================================
# app/app.R
# PubMed Explorer — interface Shiny S1.2
# =============================================================================

library(shiny)
library(bslib)
library(openalexR)
library(dplyr)
library(igraph)
library(visNetwork)
library(scales)

options(openalexR.mailto = "sta.aalexandre@gmail.com")

source("R/api/fetch_author.R")
source("R/api/parse_records.R")
source("R/analysis/build_network.R")
source("R/analysis/filter_network.R")
source("R/analysis/build_visnetwork.R")

# =============================================================================
# UI
# =============================================================================

ui <- page_sidebar(
  title  = "Co-authorship Explorer",
  theme  = bs_theme(version = 5, bootswatch = "flatly"),
  lang   = "fr",
  
  # --- Panneau latéral -------------------------------------------------------
  sidebar = sidebar(
    width = 320,
    
    # 1. Auteur cible
    card(
      card_header("Auteur cible"),
      textInput("first_name", "Prénom",    placeholder = "Philip"),
      textInput("last_name",  "Nom",       placeholder = "Gorwood"),
      numericInput("max_results", "Nb max d'articles", value = 200, min = 10, max = 5000, step = 50),
      actionButton("fetch_btn", "Charger", class = "btn-primary w-100"),
      uiOutput("disambiguation_ui")  # affiché si candidats multiples
    ),
    
    # 2. Filtres réseau (désactivés jusqu'au chargement)
    card(
      card_header("Filtres réseau"),
      uiOutput("filter_weight_ui"),
      checkboxGroupInput(
        "positions",
        "Position de l'ego dans les articles communs",
        choices  = c(
          "Premier auteur"   = "first",
          "Dernier auteur"   = "last",
          "Correspondant"    = "corresponding",
          "Auteur milieu"    = "middle"
        ),
        selected = c("first", "last", "corresponding", "middle")
      ),
      uiOutput("filter_topn_ui"),
      checkboxInput("remove_isolates", "Supprimer les nœuds isolés", value = TRUE),
      actionButton("apply_btn", "Appliquer les filtres", class = "btn-secondary w-100 mt-2")
    )
  ),
  
  # --- Corps principal -------------------------------------------------------
  layout_columns(
    col_widths = c(12),
    
    # Résumé statistiques
    uiOutput("stats_ui"),
    
    # Deux réseaux côte à côte
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(
          "Ego \u2192 co-auteurs",
          tooltip(
            bs_icon("info-circle"),
            "Couleur des liens : r\u00f4le de l'ego dans les articles communs"
          )
        ),
        visNetworkOutput("network_ego_to_co", height = "600px"),
        min_height = 650
      ),
      card(
        card_header(
          "Co-auteurs \u2192 ego",
          tooltip(
            bs_icon("info-circle"),
            "Couleur des liens : r\u00f4le du co-auteur dans les articles communs"
          )
        ),
        visNetworkOutput("network_co_to_ego", height = "600px"),
        min_height = 650
      )
    ),
    
    # Top co-auteurs (tableau)
    card(
      card_header("Top co-auteurs"),
      tableOutput("top_coauth_table"),
      style = "max-height: 400px; overflow-y: auto;"
    )
  )
)

# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {
  
  # --- Données réactives -------------------------------------------------------
  rv <- reactiveValues(
    works      = NULL,
    coauthors  = NULL,
    ego_id     = NULL,
    ego_name   = NULL,
    g_full     = NULL,   # igraph complet
    g_filt     = NULL,   # igraph après filtrage
    candidates = NULL    # tibble si ambiguïté de désambiguïsation
  )
  
  # --- Chargement des données -------------------------------------------------
  observeEvent(input$fetch_btn, {
    req(nchar(input$first_name) > 0, nchar(input$last_name) > 0)
    
    withProgress(message = "Requête OpenAlex…", value = 0, {
      
      setProgress(0.2, detail = "Recherche auteur")
      works <- tryCatch(
        fetch_author(
          last_name   = input$last_name,
          first_name  = input$first_name,
          max_results = input$max_results,
          verbose     = TRUE
        ),
        error = function(e) {
          showNotification(paste("Erreur :", conditionMessage(e)), type = "error")
          NULL
        }
      )
      
      if (is.null(works)) return()
      
      # Cas ambiguïté
      if (isTRUE(attr(works, "multiple_candidates"))) {
        rv$candidates <- works
        showNotification(
          "Plusieurs candidats trouvés — sélectionnez l'auteur dans le panneau.",
          type = "warning", duration = 8
        )
        return()
      }
      
      rv$candidates <- NULL
      rv$works      <- works
      
      setProgress(0.5, detail = "Parsing des co-auteurs")
      coauthors <- unnest_authorships(works)
      rv$coauthors <- coauthors
      
      # Identification ego
      ego_row <- coauthors |>
        filter(grepl(input$last_name, author_name, ignore.case = TRUE)) |>
        count(author_id, author_name, sort = TRUE) |>
        slice(1)
      
      if (nrow(ego_row) == 0) {
        showNotification("Impossible d'identifier l'ego dans les co-auteurs.", type = "error")
        return()
      }
      
      rv$ego_id   <- ego_row$author_id
      rv$ego_name <- paste(input$first_name, input$last_name)
      
      setProgress(0.8, detail = "Construction du graphe")
      rv$g_full <- build_ego_network(coauthors, rv$ego_id, rv$ego_name)
      rv$g_filt <- rv$g_full
      
      setProgress(1.0, detail = "Terminé")
    })
  })
  
  # --- UI de désambiguïsation -------------------------------------------------
  output$disambiguation_ui <- renderUI({
    req(rv$candidates)
    choices <- setNames(rv$candidates$id, paste0(
      rv$candidates$display_name,
      " (", rv$candidates$works_count, " articles)"
    ))
    tagList(
      hr(),
      selectInput("selected_candidate", "Choisir l'auteur :", choices = choices),
      actionButton("confirm_candidate", "Confirmer", class = "btn-warning w-100")
    )
  })
  
  # Confirmation du candidat choisi manuellement
  observeEvent(input$confirm_candidate, {
    req(rv$candidates, input$selected_candidate)
    # Relancer fetch avec l'ID forcé n'est pas prévu dans l'API actuelle :
    # on informe et on recharge avec le nom exact du candidat sélectionné.
    chosen <- rv$candidates |> filter(id == input$selected_candidate)
    showNotification(
      paste("Candidat sélectionné :", chosen$display_name),
      type = "message"
    )
    # TODO S1.3 : ajouter un paramètre `force_id` dans fetch_author()
  })
  
  # --- Sliders dynamiques (dépendent des données chargées) -------------------
  output$filter_weight_ui <- renderUI({
    req(rv$g_full)
    w <- igraph::E(rv$g_full)$weight
    sliderInput(
      "min_weight", "Nb minimum d'articles communs",
      min = 1, max = max(w), value = 1, step = 1
    )
  })
  
  output$filter_topn_ui <- renderUI({
    req(rv$g_full)
    n <- igraph::ecount(rv$g_full)
    sliderInput(
      "top_n", "Top-N co-auteurs (0 = tous)",
      min = 0, max = n, value = 0, step = 5
    )
  })
  
  # --- Application des filtres -----------------------------------------------
  observeEvent(input$apply_btn, {
    req(rv$g_full)
    pos <- if (length(input$positions) == 4) NULL else input$positions
    tn  <- if (input$top_n == 0) NULL else input$top_n
    
    rv$g_filt <- filter_network(
      g                = rv$g_full,
      min_weight       = input$min_weight,
      author_positions = pos,
      coauthors        = if (!is.null(pos)) rv$coauthors else NULL,
      ego_id           = if (!is.null(pos)) rv$ego_id    else NULL,
      top_n            = tn,
      remove_isolates  = input$remove_isolates
    )
  })
  
  # --- Statistiques ----------------------------------------------------------
  output$stats_ui <- renderUI({
    req(rv$g_filt)
    g <- rv$g_filt
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Nœuds",   value = igraph::vcount(g), showcase = bsicons::bs_icon("people")),
      value_box(title = "Arêtes",  value = igraph::ecount(g), showcase = bsicons::bs_icon("diagram-2")),
      value_box(title = "Articles", value = nrow(rv$works),   showcase = bsicons::bs_icon("file-text")),
      value_box(
        title = "Densité",
        value = round(igraph::edge_density(g), 3),
        showcase = bsicons::bs_icon("grid")
      )
    )
  })
  
  # --- Réseaux visNetwork ----------------------------------------------------
  output$network_ego_to_co <- renderVisNetwork({
    req(rv$g_filt)
    tables <- build_visnetwork_ego_to_co(
      g         = rv$g_filt,
      coauthors = rv$coauthors,
      ego_id    = rv$ego_id
    )
    render_visnetwork(tables, title = "Ego \u2192 co-auteurs")
  })
  
  output$network_co_to_ego <- renderVisNetwork({
    req(rv$g_filt)
    tables <- build_visnetwork_co_to_ego(
      g         = rv$g_filt,
      coauthors = rv$coauthors,
      ego_id    = rv$ego_id
    )
    render_visnetwork(tables, title = "Co-auteurs \u2192 ego")
  })
  
  # --- Tableau top co-auteurs ------------------------------------------------
  output$top_coauth_table <- renderTable({
    req(rv$g_filt)
    igraph::as_data_frame(rv$g_filt, what = "edges") |>
      arrange(desc(weight)) |>
      left_join(
        igraph::as_data_frame(rv$g_filt, what = "vertices") |>
          select(name, label),
        by = c("to" = "name")
      ) |>
      select("Co-auteur" = label, "Articles communs" = weight) |>
      head(30)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

# =============================================================================
shinyApp(ui, server)