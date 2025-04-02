# Rosenberg self-esteem scale 
# https://www.guidapsicologi.it/articoli/problemi-di-autostima-scoprilo-con-la-scala-di-rosenberg
#
# Positive self- esteem: 1, 2, 4, 6, 7
# Negative self-esteem: 3, 5, 8, 9, 10

library(tidyverse)
library(here)
library(rio)
library(lavaan)
library(semPlot)
library(psych)
library(stringr)
library(careless)


# Import data -------------------------------------------------------------

d <- rio::import(
  here::here("data", "rosenberg", "data.xlsx")
) |> 
  dplyr::select(starts_with("ros"))

names(d) <- names(d) %>% 
  str_replace("ros(\\d+)_1", "ros_\\1")

glimpse(d)
summary(d)


# Careless responding -----------------------------------------------------

# Calcolare il longstring index per ciascun partecipante
longstr <- longstring(d)

# Aggiungere indice al dataframe e identificare risposte careless
d_clean <- d %>%
  mutate(longstr = longstr) %>%
  filter(longstr < 5)  # soglia conservativa, ad esempio 5

# Indice IRV (Intra-individual Response Variability)
irv_scores <- irv(d)

# Aggiungere al dataframe originale
d <- d %>% mutate(irv = irv_scores)

# Filtrare con criterio combinato (esempio)
d_clean <- d %>%
  mutate(longstr = longstr) %>%
  filter(longstr < 5, irv > quantile(irv, .05)) # rimuove anche soggetti con bassa variabilità

# Controlla quanti soggetti sono stati rimossi
cat("Numero soggetti originali:", nrow(d), "\n")
cat("Numero soggetti dopo pulizia:", nrow(d_clean), "\n")


# Recode ------------------------------------------------------------------

# Ricodifica degli item negativi
# La formula 5 - .x inverte le risposte da 1→4, 2→3, 3→2 e 4→1.
d_recoded <- d_clean |> 
  mutate(across(c(ros_3, ros_5, ros_8, ros_9, ros_10), ~ 5 - .x))


# Descriptive stats -------------------------------------------------------

describe(d_recoded)

# Calcola le statistiche descrittive
descr <- describe(d_recoded)

# Crea istogrammi per ciascuna variabile
d_recoded %>%
  dplyr::select(ros_1:ros_10) %>%
  pivot_longer(
    cols = everything(), names_to = "Variabile", values_to = "Valore"
    ) %>%
  ggplot(aes(x = Valore)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  facet_wrap(~Variabile, scales = "free") +
  theme_minimal() +
  labs(title = "Istogrammi degli Item", x = "Valore", y = "Frequenza")

cor(d_recoded) |> round(2)


# Model 1 -----------------------------------------------------------------

mod <- "
  F =~ ros_1 + ros_2 + ros_3 + ros_4 + ros_5 + ros_6 + ros_7 + ros_8 + 
       ros_9 + ros_10 
"

fit <- lavaan:::cfa(mod, data = d_recoded, std.lv = TRUE)

summary(
  fit, 
  fit.measures = TRUE, 
  standardized = TRUE
)

semPaths(
  fit,
  "std",
  posCol = c("black"),
  edge.label.cex = 0.7,
  whatLabels = "std", 
  edge.width = 0.3, # Imposta lo spessore delle linee 
  fade = FALSE # Disabilita il fading
)


# Model 2 -----------------------------------------------------------------

mod2 <- "
  F1 =~ ros_1 + ros_2 + ros_4 + ros_6 + ros_7 
  F2 =~ ros_3 + ros_5 + ros_8 + ros_9 + ros_10 
  F1 ~~ NA*F2
"

fit2 <- lavaan:::cfa(mod2, data = d_recoded, std.lv = TRUE)

summary(
  fit2, 
  fit.measures = TRUE, 
  standardized = TRUE
)

lavTestLRT(fit, fit2)


# Correlazione tra un indicatore e il fattore comune ------------------------

# Punteggi fattoriali stimati
factor_scores <- lavPredict(fit)

# Aggiungere i punteggi fattoriali al dataframe originale
d_recoded <- d_recoded %>% 
  mutate(factor_score = as.vector(factor_scores))

glimpse(d_recoded)

# Correlazioni empiriche tra item e fattore stimato
correlations <- d_recoded %>%
  select(ros_1:ros_10, factor_score) %>%
  cor() 

# Estrai solo le correlazioni con factor_score
est_cor <- correlations_with_factor <- correlations["factor_score", 1:10]
est_cor |> round(2)

# Loadings standardizzati dal modello
std_loadings <- parameterEstimates(fit, standardized = TRUE) %>%
  filter(op == "=~") %>%
  pull(std.all)

# Confronta correlazioni empiriche e saturazioni fattoriali standardizzate
plot(est_cor, std_loadings,
     xlab = "Correlazioni empiriche (item - fattore)",
     ylab = "Loadings standardizzati (lavaan)",
     main = "Confronto empirico correlazioni-loadings",
     pch = 19)
abline(0, 1, col = "red", lty = 2)

# Se il modello è ben adattato ai dati, i punti cadranno quasi perfettamente 
# sulla diagonale, indicando che i loadings standardizzati corrispondono 
# empiricamente alle correlazioni tra item e fattore stimato.


# Espressione fattoriale della varianza -----------------------------------

# 1. Calcola la varianza empirica di ciascun item
var_empiriche <- d_recoded %>%
  select(ros_1:ros_10) %>%
  summarise_all(var) %>%
  pivot_longer(everything(), names_to = "Item", values_to = "Varianza_empirica")

# 2. Estrai i loading non-standardizzati (con std.lv=TRUE)
loadings <- parameterEstimates(fit, standardized = FALSE) %>%
  filter(op == "=~") %>%
  select(Item = rhs, loading = est) %>%
  mutate(varianza_fattore = loading^2)

# 3. Estrai le specificità (residual variance)
residui <- parameterEstimates(fit, standardized = FALSE) %>%
  filter(op == "~~", lhs == rhs, lhs %in% paste0("ros_", 1:10)) %>%
  select(Item = lhs, specificita = est)

# 4. Combina tutto in un unico dataframe
decomposizione <- var_empiriche %>%
  left_join(loadings, by = "Item") %>%
  left_join(residui, by = "Item") %>%
  mutate(varianza_ricostruita = varianza_fattore + specificita)

print(decomposizione)

# 5. Verifica empirica con un grafico
plot(decomposizione$Varianza_empirica, decomposizione$varianza_ricostruita,
     xlab = "Varianza empirica degli item",
     ylab = "Varianza ricostruita (λ² + specificità)",
     main = "Decomposizione empirica delle varianze",
     pch = 19)
abline(0, 1, col = "red", lty = 2)


# Correlazioni osservate e correlazioni riprodotte dal modello ------------

# 1. Correlazioni osservate
cor_osservate <- d_recoded %>%
  select(ros_1:ros_10) %>%
  cor()

cor_osservate |> round(2)

# 2. Correlazioni riprodotte dal modello

# Standardizzazione preliminare dei dati 
d_standardized <- d_recoded %>%
  select(ros_1:ros_10) %>%
  mutate(across(everything(), scale))

# Riadatta il modello CFA ai dati standardizzati 
mod_std <- "
  F =~ ros_1 + ros_2 + ros_3 + ros_4 + ros_5 + ros_6 + ros_7 + ros_8 + 
       ros_9 + ros_10
"

fit_std <- cfa(mod_std, data = d_standardized, std.lv = TRUE)

# Calcolo manuale delle correlazioni riprodotte usando solo i loadings 
# standardizzati:

# Estrai saturazioni fattoriali standardizzate
loadings_std <- parameterEstimates(fit_std, standardized = TRUE) %>%
  filter(op == "=~") %>%
  select(item = rhs, loading_std = std.all)

# Calcola la matrice delle correlazioni riprodotte
cor_riprodotte_std <- outer(loadings_std$loading_std, loadings_std$loading_std)

# assegna nomi alle righe e colonne
rownames(cor_riprodotte_std) <- colnames(cor_riprodotte_std) <- loadings_std$item

# Mostra il risultato
round(cor_riprodotte_std, 3)


# 3. Correlazioni residue
cor_residue <- cor_osservate - cor_riprodotte_std

# Visualizza i risultati (arrotondati per semplicità)

cat("\nCorrelazioni residue:\n")
print(round(cor_residue, 3))

# Imposta la diagonale principale su NA
diag(cor_residue) <- NA

# Calcola la correlazione residua media ignorando gli NA
mean_residua <- mean(abs(cor_residue), na.rm = TRUE)

# Mostra il risultato
cat("Correlazione residua media (assoluta):", round(mean_residua, 3), "\n")


# Chi quadrato ------------------------------------------------------------

# Per calcolare manualmente il Chi-quadrato è necessario utilizzare la matrice 
# delle covarianze

library(lavaan)

# Matrici osservata e riprodotta dal modello (covarianze)
S_obs <- cov(d_recoded %>% select(ros_1:ros_10))
S_mod <- fitted(fit)$cov

# Numero di osservazioni
N <- nrow(d_recoded)

# Numero di variabili
p <- ncol(S_obs)

# Calcolo manuale del chi-quadrato con funzione ML usata da lavaan
chi_sq_manual_ml <- 
  (N - 1) * (log(det(S_mod)) + sum(diag(S_obs %*% solve(S_mod))) - log(det(S_obs)) - p)

cat("Chi-quadrato calcolato manualmente (ML):", round(chi_sq_manual_ml, 3), "\n")

# Confronta con il valore calcolato da lavaan
fitMeasures(fit, "chisq")


# Modello multifattoriale ---------------------------------------------------

efa_mod_1 <- '
  efa("efa")*f1 + 
  efa("efa")*f2 =~ ros_1 + ros_2 + ros_3 + ros_4 + ros_5 + ros_6 + ros_7 + 
                   ros_8 + ros_9 + ros_10
'

fit_efa_1 <- cfa(
  efa_mod_1, 
  data = d_recoded, 
  std.lv = TRUE, 
  rotation = "varimax"
)

summary(
  fit_efa_1, 
  fit.measures = TRUE, 
  standardized = TRUE
)

semPaths(
  fit_efa_1,
  "std",
  posCol = c("black"),
  edge.label.cex = 0.7,
  whatLabels = "std", 
  edge.width = 0.3, # Imposta lo spessore delle linee 
  fade = FALSE # Disabilita il fading
)


fit_efa_oblique <- cfa(
  efa_mod_1, 
  data = d_recoded, 
  std.lv = TRUE, 
  rotation = "oblimin"
)

summary(
  fit_efa_oblique, 
  fit.measures = TRUE, 
  standardized = TRUE
)

semPaths(
  fit_efa_oblique,
  "std",
  posCol = c("black"),
  edge.label.cex = 0.7,
  whatLabels = "std", 
  edge.width = 0.3, # Imposta lo spessore delle linee 
  fade = FALSE # Disabilita il fading
)

# Correlazione tra un indicatore e il fattore comune ------------------------

# Ottenere i punteggi fattoriali per entrambi i fattori
factor_scores <- lavPredict(fit_efa_oblique)

# Aggiungere i punteggi fattoriali al dataframe originale
d_recoded <- d_recoded %>%
  mutate(factor_score_f1 = factor_scores[,1],
         factor_score_f2 = factor_scores[,2])

glimpse(d_recoded)

# Correlazioni empiriche tra item e fattori stimati
correlations <- d_recoded %>%
  select(ros_1:ros_10, factor_score_f1, factor_score_f2) %>%
  cor()

# Estrai solo le correlazioni con i fattori
est_cor_f1 <- correlations["factor_score_f1", 1:10]
est_cor_f2 <- correlations["factor_score_f2", 1:10]

est_cor_f1 |> round(2)
est_cor_f2 |> round(2)

# Loadings standardizzati dal modello
std_loadings <- parameterEstimates(fit_efa_oblique, standardized = TRUE) %>%
  filter(op == "=~")

# Estrarre i loadings per ciascun fattore
std_loadings_f1 <- std_loadings %>%
  filter(lhs == "f1") %>%
  pull(std.all)

std_loadings_f2 <- std_loadings %>%
  filter(lhs == "f2") %>%
  pull(std.all)

# Confronto correlazioni empiriche e saturazioni fattoriali standardizzate per f1

par(mfrow=c(1,2))  # Per creare due grafici affiancati

plot(est_cor_f1, std_loadings_f1,
     xlab = "Correlazioni empiriche (item - fattore 1)",
     ylab = "Loadings standardizzati f1 (lavaan)",
     main = "Confronto empirico per Fattore 1",
     pch = 19)
abline(0, 1, col = "red", lty = 2)

# Confronto correlazioni empiriche e saturazioni fattoriali standardizzate per f2
plot(est_cor_f2, std_loadings_f2,
     xlab = "Correlazioni empiriche (item - fattore 2)",
     ylab = "Loadings standardizzati f2 (lavaan)",
     main = "Confronto empirico per Fattore 2",
     pch = 19)
abline(0, 1, col = "red", lty = 2)

# Ripristinare il layout del grafico
par(mfrow=c(1,1))

# Calcoliamo anche la correlazione tra i fattori
cor_factors <- cor(factor_scores)[1,2]
cat("Correlazione tra i fattori:", round(cor_factors, 3), "\n")


# Correlazioni osservate e correlazioni riprodotte dal modello ------------

# 1. Correlazioni osservate
cor_osservate <- d_recoded %>%
  select(ros_1:ros_10) %>%
  cor()
cor_osservate |> round(2)

# 2. Correlazioni riprodotte dal modello
# Standardizzazione preliminare dei dati 
d_standardized <- d_recoded %>%
  select(ros_1:ros_10) %>%
  mutate(across(everything(), scale))

# Riadatta il modello EFA obliquo ai dati standardizzati 
efa_mod_oblique <- '
  efa("efa")*f1 + 
  efa("efa")*f2 =~ ros_1 + ros_2 + ros_3 + ros_4 + ros_5 + ros_6 + ros_7 + ros_8 + ros_9 + ros_10
'

fit_efa_oblique_std <- cfa(efa_mod_oblique, data = d_standardized, std.lv = TRUE, rotation = "oblimin")

# Estrai saturazioni fattoriali standardizzate
loadings_std <- parameterEstimates(fit_efa_oblique_std, standardized = TRUE) %>%
  filter(op == "=~") %>%
  select(factor = lhs, item = rhs, loading_std = std.all)

# Estrai correlazione tra i fattori
phi <- lavInspect(fit_efa_oblique_std, "cor.lv")
phi_f1f2 <- phi[1, 2]

# Crea una matrice ampia per i loading (per facilitare il calcolo)
loadings_wide <- pivot_wider(loadings_std, 
                             names_from = factor, 
                             values_from = loading_std)

# Calcola la matrice delle correlazioni riprodotte considerando i due fattori correlati
# Formula: cor_riprodotta_ij = λ_i1*λ_j1 + λ_i2*λ_j2 + 2*λ_i1*λ_j2*φ_12

# Estrai le matrici di loading per creare una matrice completa
# Crea una matrice di loading (Lambda)
Lambda <- as.matrix(loadings_wide[, 2:3])
Lambda

# Crea la matrice di correlazione tra i fattori (Phi)
Phi <- matrix(c(1, phi_f1f2, phi_f1f2, 1), nrow = 2, ncol = 2)
rownames(Phi) <- colnames(Phi) <- c("f1", "f2")
Phi

# Calcola la matrice delle correlazioni riprodotte usando l'algebra matriciale
# Formula: Sigma_riprodotta = Lambda %*% Phi %*% t(Lambda)
cor_riprodotte_std <- Lambda %*% Phi %*% t(Lambda)

# Mostra il risultato
round(cor_riprodotte_std, 3)

# 3. Correlazioni residue
cor_residue <- cor_osservate - cor_riprodotte_std

# Visualizza i risultati (arrotondati per semplicità)
cat("\nCorrelazioni residue:\n")
print(round(cor_residue, 3))

# Imposta la diagonale principale su NA
diag(cor_residue) <- NA

# Calcola la correlazione residua media ignorando gli NA
mean_residua <- mean(abs(cor_residue), na.rm = TRUE)

# Mostra il risultato
cat("Correlazione residua media (assoluta):", round(mean_residua, 3), "\n")

# Statistica di adattamento aggiuntiva: RMSR (Root Mean Square Residual)
rmsr <- sqrt(mean(cor_residue^2, na.rm = TRUE))
cat("RMSR (Root Mean Square Residual):", round(rmsr, 3), "\n")

# Visualizza la correlazione tra i fattori
cat("\nCorrelazione tra i fattori (φ):", round(phi_f1f2, 3), "\n")


# Espressione fattoriale della varianza -----------------------------------

# 1. Calcola la varianza empirica di ciascun item
var_empiriche <- d_recoded %>%
  select(ros_1:ros_10) %>%
  summarise_all(var) %>%
  pivot_longer(everything(), names_to = "Item", values_to = "Varianza_empirica")

Lambda
Phi

# Calcola la matrice di varianza-covarianza spiegata dai fattori
# Formula: Lambda %*% Phi %*% t(Lambda)
varianza_fattori_matrix <- Lambda %*% Phi %*% t(Lambda)
diag_varianza_fattori <- diag(varianza_fattori_matrix)
names(diag_varianza_fattori) <- rownames(Lambda)

# Estrai le specificità (varianze residue)
residui <- parameterEstimates(fit_efa_oblique, standardized = TRUE) %>%
  filter(op == "~~", lhs == rhs, lhs %in% paste0("ros_", 1:10)) %>%
  select(Item = lhs, specificita = std.all)

# Combina i risultati in un dataframe
decomposizione <- var_empiriche %>%
  mutate(Item = as.character(Item)) %>%
  left_join(tibble(Item = names(diag_varianza_fattori), 
                   varianza_fattori = diag_varianza_fattori), 
            by = "Item") %>%
  left_join(residui, by = "Item") %>%
  mutate(varianza_ricostruita = varianza_fattori + specificita)


# Per esempio, per l'item "ros_1":

# - La varianza empirica osservata è 0.614
# - Il 56.8% (0.568) di questa varianza viene spiegato dal modello fattoriale (la comunalità)
# - Il 43.2% (0.432) rappresenta la varianza residua non spiegata dal modello (la specificità)

# Detto in altri termini, i due fattori obliqui del modello riescono a spiegare 
# circa il 57% della variabilità dell'item "ros_1", mentre il restante 43% è 
# varianza unica dell'item che non viene catturata dal modello fattoriale 
# (potrebbe essere dovuta a specificità dell'item o errore di misurazione).

# È interessante notare che per alcuni item, come "ros_10", il modello spiega 
# una percentuale molto alta della varianza (88%), mentre per altri, come 
# "ros_8", spiega una percentuale molto più bassa (solo il 30%). Questo 
# potrebbe indicare che alcuni item sono più rappresentativi dei costrutti 
# latenti che stiamo misurando rispetto ad altri.


# eof ---
