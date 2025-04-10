# Dimostrazione dell'utilizzo della formula basata sulle correlazioni 
# per il calcolo dei beta nel modello di regressione.

set.seed(123)

n <- 500

# Simulo predittori correlati
x1 <- rnorm(n)
x2 <- 0.6 * x1 + sqrt(1 - 0.6^2) * rnorm(n)
x3 <- 0.5 * x1 + 0.3 * x2 + sqrt(1 - 0.5^2 - 0.3^2) * rnorm(n)

# Variabile dipendente y come combinazione lineare di x1 x2 x3
y <- 0.7 * x1 + 0.5 * x2 + 0.3 * x3 + rnorm(n, sd=0.5)

# Standardizzo tutto
X <- scale(cbind(x1, x2, x3))
y <- scale(y)


# Correlazione tra i predittori
Rxx <- cor(X)

# Correlazione tra i predittori e y
Rxy <- cor(X, y)

# Calcolo dei beta standardizzati
B_hat <- solve(Rxx) %*% Rxy

B_hat

# Modello di regressione lineare standardizzato
mod <- lm(y ~ X)

# Coefficienti standardizzati (escludo l'intercetta)
coef(mod)[-1]





