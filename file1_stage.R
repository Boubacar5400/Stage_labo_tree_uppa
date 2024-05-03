# Définition de la fonction objectif
objective_function <- function(alpha, d, x, w, p, beta, U_bar) {
  term1 <- alpha * x * log(U_bar) - (alpha * x * (beta * (1 - d) + 1) + w - x) - p * (1 - alpha) * x
  term2 <- x * log(1 - alpha)
  term3 <- x * log(1 - d)
  
  return(term1 + term2 + term3)
}

# Simulation du modèle pour trouver les valeurs optimales d'alpha et d
optimize_model <- function(p, beta, U_bar, w) {
  # Définition de la fonction à maximiser
  objective <- function(vars) {
    alpha <- vars[1]
    d <- vars[2]
    
    return(-objective_function(alpha, d, 0, w, p, beta, U_bar))
  }
  
  # Recherche des valeurs optimales d'alpha et d
  result <- optim(c(0.5, 0.5), objective, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1))
  
  alpha_optimal <- result$par[1]
  d_optimal <- result$par[2]
  
  return(list(alpha = alpha_optimal, d = d_optimal))
}

# Paramètres du modèle
p <- 2.0
beta <- 0.5
U_bar <- 1.0
w <- 0.2

# Simulation du modèle
result <- optimize_model(p, beta, U_bar, w)

# Affichage des résultats
cat("Valeur optimale de alpha:", result$alpha, "\n")
cat("Valeur optimale de d:", result$d, "\n")


#################
library(ggplot2)

# Définition de la fonction objectif
objective_function <- function(alpha, d, x, w, p, beta, U_bar) {
  term1 <- alpha * x * log(U_bar) - (alpha * x * (beta * (1 - d) + 1) + w - x) - p * (1 - alpha) * x
  term2 <- x * log(1 - alpha)
  term3 <- x * log(1 - d)
  
  return(term1 + term2 + term3)
}

# Simulation du modèle pour trouver les valeurs optimales d'alpha et d
optimize_model <- function(p, beta, U_bar, w) {
  # Définition de la fonction à maximiser
  objective <- function(vars) {
    alpha <- vars[1]
    d <- vars[2]
    
    return(-objective_function(alpha, d, 0, w, p, beta, U_bar))
  }
  
  # Recherche des valeurs optimales d'alpha et d
  result <- optim(c(0.5, 0.5), objective, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1))
  
  alpha_optimal <- result$par[1]
  d_optimal <- result$par[2]
  
  return(list(alpha = alpha_optimal, d = d_optimal))
}

# Paramètres du modèle
p <- 2.0
beta <- 0.5
U_bar <- 1.0
w <- 0.2

# Simulation du modèle
result <- optimize_model(p, beta, U_bar, w)

alpha_optimal <- result$alpha
d_optimal <- result$d

# Affichage des résultats
cat("Valeur optimale de alpha:", alpha_optimal, "\n")
cat("Valeur optimale de d:", d_optimal, "\n")

# Création des graphiques
# Variation de alpha en fonction de d
d_values <- seq(0, 1, by = 0.01)
alpha_values <- 1 - (beta * (1 - d_values)) / (p - 1)

df_alpha <- data.frame(d = d_values, alpha = alpha_values)

ggplot(df_alpha, aes(x = d, y = alpha)) +
  geom_line() +
  labs(x = "d", y = "alpha") +
  ggtitle("Variation de alpha en fonction de d") +
  theme_minimal()

# Variation de d en fonction de alpha
alpha_values <- seq(0, 1, by = 0.01)
d_values <- 1 - ((alpha_values + p * (1 - alpha_values)) - 1) / beta

df_d <- data.frame(alpha = alpha_values, d = d_values)

ggplot(df_d, aes(x = alpha, y = d)) +
  geom_line() +
  labs(x = "alpha", y = "d") +
  ggtitle("Variation de d en fonction de alpha") +
  theme_minimal()

