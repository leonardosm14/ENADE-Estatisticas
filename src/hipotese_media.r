# --- Pacotes ---
library(dplyr)
library(ggplot2)

# --- Filtrando os dados ---
dados_modalidade <- data_CPC_SC %>%
  filter(modalidade_de_ensino %in% c("Educação Presencial", "Educação a Distância")) %>%
  select(modalidade_de_ensino, cpc_.continuo.) %>%
  filter(!is.na(cpc_.continuo.))

# --- Criar amostra aleatória de tamanho n = 426, conforme "amostragem.r" ---
set.seed(123)  # para reprodutibilidade
dados_amostra <- dados_modalidade %>%
  sample_n(size = 426, replace = FALSE)

# --- Separando os grupos ---
presencial <- dados_amostra %>%
  filter(modalidade_de_ensino == "Educação Presencial") %>%
  pull(cpc_.continuo.)

ead <- dados_amostra %>%
  filter(modalidade_de_ensino == "Educação a Distância") %>%
  pull(cpc_.continuo.)

# --- Teste t de Welch ---
teste_t <- t.test(presencial, ead,
                  alternative = "greater",   # teste unilateral à direita
                  var.equal = FALSE)         # Welch (variâncias diferentes)

# --- Exibir resultado ---
print(teste_t)

# --- Boxplot comparativo ---
dev.new()
print(
  ggplot(dados_amostra, aes(x = modalidade_de_ensino, y = cpc_.continuo., fill = modalidade_de_ensino)) +
    geom_boxplot(alpha = 0.7) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
    labs(x = "Modalidade de Ensino",
         y = "CPC (Contínuo)") +
    theme_minimal() +
    theme(legend.position = "none")
)
