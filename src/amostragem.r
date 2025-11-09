# --- Diretório ---
setwd("~/Documentos/UFSC/ENADE-Estatisticas")

# Carrega os dados filtrados de Santa Catarina
source(file = "src/script_sc.r")

# --- Parâmetros fixos padrão ---
z <- 1.96     # nível de confiança de 95%
d <- 0.05     # erro amostral tolerável (5%)

# --- Variáveis Quantitativas ---

# --- Funções ---
n0 <- function(variancia) {
  return((z^2 * variancia) / (d^2))
}

n <- function(n0, N) {
  return(n0 / (1 + (n0 / N)))
}

# --- 1. Conceito ENADE (Contínuo) ---
variancia_enade <- var(data_CPC_SC$conceito_enade_.continuo., na.rm = TRUE)
N_enade <- nrow(data_CPC_SC)

n0_enade <- n0(variancia_enade)
n_enade <- n(n0_enade, N_enade)

cat("\n--- Conceito ENADE (Contínuo) ---\n")
cat("Variância:", round(variancia_enade, 4), "\n")
cat("N:", N_enade, "\n")
cat("n0:", round(n0_enade, 2), "\n")
cat("n:", ceiling(n_enade), "\n")

# --- 2. IDD (Contínuo) ---
variancia_idd <- var(data_IDD_SC$idd_.continuo., na.rm = TRUE)
N_idd <- nrow(data_IDD_SC)

n0_idd <- n0(variancia_idd)
n_idd <- n(n0_idd, N_idd)

cat("\n--- IDD (Contínuo) ---\n")
cat("Variância:", round(variancia_idd, 4), "\n")
cat("N:", N_idd, "\n")
cat("n0:", round(n0_idd, 2), "\n")
cat("n:", ceiling(n_idd), "\n")

# --- 3. CPC (Contínuo) ---
variancia_cpc <- var(data_CPC_SC$cpc_.continuo., na.rm = TRUE)
N_cpc <- nrow(data_CPC_SC)

n0_cpc <- n0(variancia_cpc)
n_cpc <- n(n0_cpc, N_cpc)

cat("\n--- CPC (Contínuo) ---\n")
cat("Variância:", round(variancia_cpc, 4), "\n")
cat("N:", N_cpc, "\n")
cat("n0:", round(n0_cpc, 2), "\n")
cat("n:", ceiling(n_cpc), "\n")

# --- 4. IGC (Contínuo) ---
variancia_igc <- var(data_IGC_SC$igc_.continuo., na.rm = TRUE)
N_igc <- nrow(data_IGC_SC)

n0_igc <- n0(variancia_igc)
n_igc <- n(n0_igc, N_igc)

cat("\n--- IGC (Contínuo) ---\n")
cat("Variância:", round(variancia_igc, 4), "\n")
cat("N:", N_igc, "\n")
cat("n0:", round(n0_igc, 2), "\n")
cat("n:", ceiling(n_igc), "\n")

# --- Variáveis Qualitativas ---

p <- 0.5

# n0 é comum a todos

n0 <- z^2 * p * (1-p) / d^2

# --- 1. Categoria Administrativa ---
N_cat <- nrow(igc_universidades)
n_cat <- n(n0, N_cat)

cat("\n--- Categoria Administrativa ---\n")
cat("N:", N_cat, "\n")
cat("n:", ceiling(n_cat), "\n")


# --- 2. Modalidade de Ensino ---

N_mod <- nrow(data_CPC_SC)   # total de cursos (cada curso tem uma modalidade)
n_mod <- n(n0, N_mod)

cat("\n--- Modalidade de Ensino ---\n")
cat("N:", N_mod, "\n")
cat("n:", ceiling(n_mod), "\n")