# --- Pacotes ---
library(dplyr)
library(ggplot2)

# --- Filtrando os dados ---
# Mantém apenas cursos com ensino Presencial ou a Distância em Santa Catarina
dados_modalidade <- data_CPC_SC %>%
  filter(modalidade_de_ensino %in% c("Educação Presencial", "Educação a Distância")) %>%
  select(modalidade_de_ensino, cpc_.continuo.) %>%
  filter(!is.na(cpc_.continuo.))

# --- Separando os grupos ---
presencial <- dados_modalidade %>%
  filter(modalidade_de_ensino == "Educação Presencial") %>%
  pull(cpc_.continuo.)

ead <- dados_modalidade %>%
  filter(modalidade_de_ensino == "Educação a Distância") %>%
  pull(cpc_.continuo.)

# --- Teste t de Welch ---
# H0: média (Presencial) = média (EaD)
# H1: média (Presencial) > média (EaD)
teste_t <- t.test(presencial, ead,
                  alternative = "greater",   # teste unilateral à direita
                  var.equal = FALSE)         # Welch (variâncias diferentes)

# --- Exibir resultado ---
print(teste_t)

# --- Boxplot comparativo ---
dev.new()
print(
  ggplot(dados_modalidade, aes(x = modalidade_de_ensino, y = cpc_.continuo., fill = modalidade_de_ensino)) +
    geom_boxplot(alpha = 0.7) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
    labs(title = "Comparação do CPC (SC)",
         x = "Modalidade de Ensino",
         y = "CPC (Contínuo)") +
    theme_minimal() +
    theme(legend.position = "none")
)
