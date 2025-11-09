# --- Pacotes ---
library(dplyr)

# --- Amostra aleatória de instituições ---
set.seed(123)  # garante reprodutibilidade
amostra_n <- 89
amostra_instituicoes <- data_IGC_SC %>%
  sample_n(amostra_n)

# --- Contagem de instituições federais ---
sucesso <- sum(amostra_instituicoes$categoria_administrativa == "Pública Federal")
total <- nrow(amostra_instituicoes)
prop_observada <- sucesso / total

cat("Tamanho da amostra:", total, "\n")
cat("Número de instituições federais na amostra:", sucesso, "\n")
cat("Proporção observada:", round(prop_observada, 3), "\n\n")

# --- Teste de hipótese para proporção única (unilateral) ---
# H0: p = 0.25
# H1: p > 0.25
teste_prop <- prop.test(x = sucesso, n = total, p = 0.25, alternative = "greater", correct = FALSE)

# --- Exibir resultado ---
print(teste_prop)

# --- Interpretação ---
cat("\nInterpretação:\n")
if (teste_prop$p.value < 0.05) {
  cat("Rejeita-se H0: A proporção de instituições federais é significativamente maior que 25%.\n")
} else {
  cat("Não se rejeita H0: Não há evidências de que a proporção de instituições federais seja maior que 25%.\n")
}
