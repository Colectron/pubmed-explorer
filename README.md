# PubMed Explorer

> Application Shiny d'exploration et de visualisation des métadonnées bibliométriques issues de PubMed.

## Objectifs

- Recherche par **auteur** (nom + prénom) ou par **équipe de recherche** (ex. UMR 1234)
- Visualisation du **réseau de collaborateurs** (graphe interactif)
- Métriques bibliométriques : nombre de publications, position dans les auteurs, journaux, affiliations
- Visualisation des **réseaux inter-équipes**
- Extensions prévues : dimension géographique, recherche par sujet MeSH

## Stack technique

| Composant | Outil |
|-----------|-------|
| Langage | R |
| Interface | Shiny + bslib |
| API bibliographique | NCBI E-utilities via `rentrez` |
| Réseau | `igraph` + `visNetwork` |
| Graphiques | `plotly` |
| Tableaux | `DT` |
| Reproductibilité | `renv` |

## Installation

```r
# Cloner le repo puis :
renv::restore()

# Ajouter votre clé API NCBI dans .Renviron (non commité) :
# NCBI_API_KEY=votre_cle_ici
```

Clé API NCBI gratuite : https://www.ncbi.nlm.nih.gov/account/

## Lancer l'application

```r
shiny::runApp()
```

## Structure du projet

```
pubmed-explorer/
├── R/
│   ├── api/            # Fonctions de requêtage PubMed
│   ├── analysis/       # Construction des graphes et métriques
│   └── ui_modules/     # Modules Shiny (1 fichier = 1 panneau)
├── docs/               # Documentation interne, notes de session
├── tests/              # Tests unitaires (testthat)
├── app.R               # Point d'entrée Shiny
├── renv.lock           # Snapshot des dépendances R
└── DESCRIPTION         # Métadonnées du projet
```

## Branches

- `main` : version stable, mise à jour aux jalons de sprint
- `dev` : développement courant

## Statut

🚧 En cours de développement — Sprint 0 (infrastructure)
