# --- Pacotes ---
library(dplyr)

# --- Filtrando os dados ---
# Mantém apenas cursos de instituições públicas federais
federais_igc <- data_IGC_SC %>%
  filter(categoria_administrativa == "Pública Federal") %>%
  select(igc_.continuo.) %>%
  filter(!is.na(igc_.continuo.))

# --- Criando variável indicadora ---
# 1 se IGC ≥ 4 (curso de alto desempenho), 0 caso contrário
federais_igc <- federais_igc %>%
  mutate(igc_alto = ifelse(igc_.continuo. >= 4, 1, 0))

# --- Contagem de sucessos e total ---
sucesso <- sum(federais_igc$igc_alto)
total <- nrow(federais_igc)
prop_observada <- sucesso / total

cat("Número de cursos federais:", total, "\n")
cat("Cursos com IGC ≥ 4:", sucesso, "\n")
cat("Proporção observada:", round(prop_observada, 3), "\n\n")

# --- Teste de hipótese para proporção única ---
# H0: p = 0.7
# H1: p > 0.7
teste_prop <- prop.test(x = sucesso, n = total, p = 0.7, alternative = "greater", correct = FALSE)

# --- Exibir resultado ---
print(teste_prop)

# --- Interpretação ---
cat("\nInterpretação:\n")
if (teste_prop$p.value < 0.05) {
  cat("Rejeita-se H0: A proporção de cursos com IGC ≥ 4 é significativamente maior que 70%.\n")
} else {
  cat("Não se rejeita H0: Não há evidências de que a proporção de cursos com IGC ≥ 4 seja maior que 70%.\n")
}
