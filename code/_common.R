# -----------------------------------------------------------------------------
# Common Packages and Settings
# -----------------------------------------------------------------------------

# Use pacman to manage and load required packages automatically
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
  MASS, # Pacchetto per metodi statistici aggiuntivi
  see, # Provides palette_okabeito
  tidyr, # Funzionalità per creare tidy data
  ggokabeito # Qualitative Okabe-Ito Scales for ggplot2 
)



# Suppress unnecessary messages from tidyverse packages
options(tidyverse.quiet = TRUE)

# -----------------------------------------------------------------------------
# Configurazione colori globale
# -----------------------------------------------------------------------------

# Definizione della palette Okabe-Ito personalizzata
okabe_ito_palette <- c(
  "#E69F00", # Orange
  "#56B4E9", # Light blue
  "#009E73", # Green
  "#F0E442", # Yellow
  "#0072B2", # Dark blue
  "#D55E00", # Red
  "#CC79A7", # Pink
  "#999999"  # Grey
)

# Impostazione dei colori di default per ggplot2
theme_set(
  theme_minimal(
    base_size = 13,  # Font size for plots
    base_family = "sans"  # Font family
  )
)

# Impostazioni per scale di colore predefinite
ggplot2::update_geom_defaults("point", list(colour = okabe_ito_palette[1]))
ggplot2::update_geom_defaults("line", list(colour = okabe_ito_palette[1]))

# Impostazioni per scale discrete e continue
default_discrete_scale <- function(...) scale_fill_manual(..., values = okabe_ito_palette)
default_continuous_scale <- function(...) scale_fill_viridis_c(...)

# Set color scheme for bayesplot diagnostics
color_scheme_set("brightblue")

# -----------------------------------------------------------------------------
# knitr Options for Code Chunks
# -----------------------------------------------------------------------------

knitr::opts_chunk$set(
  comment = "#>",                    # Comment prefix for output
  collapse = TRUE,                   # Collapse code and output into a single block
  message = FALSE,                   # Suppress messages
  warning = FALSE,                   # Suppress warnings
  width = 72,                        # Code width
  out.width = "70%",                 # Default output width for figures
  fig.align = "center",              # Center-align figures
  fig.width = 6,                      # Default figure width
  fig.asp = 0.618,                    # Golden ratio for figure height
  fig.show = "hold",                  # Show figures together
  R.options = list(
    digits = 4,                      # Number of digits for output
    width = 76                        # Console width
  ),
  echo = TRUE,                       # Show code by default
  eval = TRUE,                       # Execute code by default
  error = FALSE                      # Suppress error messages in chunks
)

# -----------------------------------------------------------------------------
# Global Options
# -----------------------------------------------------------------------------

options(
  scipen = 1,                        # Favor standard notation over scientific
  digits = 4,                        # Default number of digits
  ggplot2.discrete.colour = ggokabeito::palette_okabe_ito(),
  ggplot2.discrete.fill = ggokabeito::palette_okabe_ito(),
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis",
  show.signif.stars = FALSE,         # Suppress significance stars in output
  pillar.max_footer_lines = 2,       # Control console output of data frames
  pillar.min_chars = 15,
  pillar.bold = TRUE,
  width = 77                         # Console width (80 chars - 3 for comments)
)

# -----------------------------------------------------------------------------
# Seed for Reproducibility
# -----------------------------------------------------------------------------

set.seed(42) # Ensure reproducibility

