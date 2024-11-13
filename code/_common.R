# Common Packages --------------------------------------------------------------

# Utilizza pacman per gestire automaticamente i pacchetti
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  here, # Gestione dei percorsi relativi dei file
  tidyverse, # Raccolta di pacchetti per manipolazione e visualizzazione dei dati
  knitr, # Supporto per la generazione di documenti, con integrazione LaTeX
  markdown, # Conversione di markdown in HTML
  scales, # Funzioni per formattare assi e scale nei grafici
  psych, # Funzioni per analisi psicometriche
  lavaan, # Modelli di equazioni strutturali
  semPlot, # Visualizzazione grafica per SEM
  semTools, # Strumenti aggiuntivi per lavaan
  bayesplot, # Grafici diagnostici e di visualizzazione per modelli bayesiani
  patchwork, # Composizione di grafici ggplot
  gridExtra, # Funzioni per visualizzare layout a griglia
  ggExtra, # Funzionalità extra per ggplot2 (grafici marginali)
  ggpubr, # Funzionalità di pubblicazione per grafici ggplot
  viridis, # Palette di colori per ggplot (ideale per scale percettivamente uniformi)
  MASS # Pacchetto per metodi statistici aggiuntivi
)

# Knitr Chunk Options ----------------------------------------------------------

knitr::opts_chunk$set(
  comment = "#>", # Commento per output
  collapse = TRUE, # Raggruppa input e output in blocchi
  message = FALSE, # Nasconde i messaggi
  warning = FALSE, # Nasconde i warning
  error = FALSE, # Nasconde gli errori
  tidy = "styler", # Formattazione automatica del codice con {styler}
  width.cutoff = 76, # Limita larghezza dell'output
  fig.retina = 2, # Migliora la risoluzione delle immagini
  fig.width = 10, # Larghezza di default delle figure
  fig.asp = 2 / 3, # Altezza delle figure in proporzione alla larghezza
  fig.show = "hold", # Mantiene insieme le figure multiple
  out.width = if (knitr::is_html_output()) "75%" else NULL # Figure width at 75% for HTML
)

# General Options --------------------------------------------------------------

options(
  repr.plot.width = 10, # Larghezza di default delle figure
  repr.plot.height = 10 / 1.618033988749895, # Altezza in rapporto aureo
  # dplyr.print_min = 6, # Numero minimo di righe da visualizzare con dplyr
  # dplyr.print_max = 6, # Numero massimo di righe da visualizzare con dplyr
  # pillar.max_footer_lines = 2, # Massimo righe footer per pillar
  pillar.min_chars = 15, # Larghezza minima delle colonne con pillar
  # stringr.view_n = 6, # Numero di valori da visualizzare per stringr
  # cli.num_colors = 0, # Disattiva colori per output CLI
  # cli.hyperlink = FALSE, # Disattiva hyperlink per CLI
  pillar.bold = TRUE, # Usa grassetto nei titoli delle colonne
  width = 76, # Larghezza massima per output (aggiustata per commenti #>)
  digits = 3
)

# ggplot2 theme and colors -----------------------------------------------------

if (knitr::is_html_output()) {
  ggplot2::theme_set(ggplot2::theme_minimal(base_size = 13))
} else if (knitr::is_latex_output()) {
  ggplot2::theme_set(ggplot2::theme_minimal(base_size = 12))
}

ggplot2::update_geom_defaults("point", list(
  color = openintro::IMSCOL["blue", "full"],
  fill = openintro::IMSCOL["blue", "full"]
))
ggplot2::update_geom_defaults("bar", list(
  fill = openintro::IMSCOL["blue", "full"],
  color = "#FFFFFF"
))
ggplot2::update_geom_defaults("col", list(
  fill = openintro::IMSCOL["blue", "full"],
  color = "#FFFFFF"
))
ggplot2::update_geom_defaults("boxplot", list(color = openintro::IMSCOL["blue", "full"]))
ggplot2::update_geom_defaults("density", list(color = openintro::IMSCOL["blue", "full"]))
ggplot2::update_geom_defaults("line", list(color = openintro::IMSCOL["gray", "full"]))
ggplot2::update_geom_defaults("smooth", list(color = openintro::IMSCOL["gray", "full"]))
ggplot2::update_geom_defaults("dotplot", list(
  color = openintro::IMSCOL["blue", "full"],
  fill = openintro::IMSCOL["blue", "full"]
))

# Theme Settings ---------------------------------------------------------------

theme_set(bayesplot::theme_default(base_size = 18, base_family = "sans"))

# Seed -------------------------------------------------------------------------

set.seed(42) # Fissa il seme per la riproducibilità
