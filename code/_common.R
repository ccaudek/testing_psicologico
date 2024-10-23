# Common packages --------------------------------------------------------------

# Usa pacman per gestire pacchetti automaticamente
if (!requireNamespace("pacman")) install.packages("pacman")

pacman::p_load(
  here,          # Gestione dei percorsi di file
  tidyverse,     # Raccolta di pacchetti per manipolazione e visualizzazione dei dati
  knitr,         # Supporto per la generazione di documenti (LaTeX integrato)
  markdown,      # Conversione di markdown in HTML
  scales,        # Funzioni per formattazione di assi e scale nei grafici
  psych,         # Funzioni per analisi psicometriche
  lavaan,        # Modelli di equazioni strutturali
  semPlot,       # Grafici per SEM
  semTools,      # Strumenti aggiuntivi per lavaan
  bayesplot,     # Grafici diagnostici e di visualizzazione per modelli bayesiani
  patchwork,     # Combinazione di grafici ggplot
  gridExtra,     # Funzioni per visualizzazione di griglie
  ggExtra,       # Funzioni extra per ggplot2 (marginal plots)
  ggpubr,        # Pubblicazione di figure basate su ggplot
  viridis,       # Palette di colori per ggplot
  ggokabeito,    # Palette di colori daltonismo-friendly
  MASS,          # Supporto per funzioni statistiche aggiuntive
  nortest        # Test di normalità
)

# Theme settings ---------------------------------------------------------------

theme_set(bayesplot::theme_default(base_size = 18, base_family = "sans"))

# knitr chunk options ----------------------------------------------------------

knitr::opts_chunk$set(
  comment = "#>",          # Formato di commento per l'output
  collapse = TRUE,         # Unisce input e output in un blocco
  message = FALSE,         # Nasconde i messaggi
  warning = FALSE,         # Nasconde i warning
  error = FALSE,           # Nasconde gli errori
  tidy = "styler",         # Formattazione automatica del codice con {styler}
  width.cutoff = 70        # Limita la larghezza dell'output a 70 caratteri
)

# General options --------------------------------------------------------------

options(
  repr.plot.width = 8,     # Larghezza delle figure di default
  repr.plot.height = 8 / 1.618033988749895, # Altezza delle figure (rapporto aureo)
  width = 70               # Limita l'output di R a 70 caratteri per riga
)

set.seed(42)               # Imposta il seme per la riproducibilità
