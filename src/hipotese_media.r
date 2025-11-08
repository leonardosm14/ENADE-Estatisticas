# --- Carregando pacotes úteis ---
library(dplyr)
library(ggplot2)

# --- Filtrando os grupos ---
federais_sc <- data_CPC_SC %>%
  filter(categoria_administrativa == "Pública Federal") %>%
  select(cpc_.continuo.)

privadas_fins_sc <- data_CPC_SC %>%
  filter(categoria_administrativa == "Privada com fins lucrativos") %>%
  select(cpc_.continuo.)

# --- Vetores de CPC ---
cpc_fed <- federais_sc$cpc_.continuo.
cpc_priv <- privadas_fins_sc$cpc_.continuo.

# --- Teste t de Welch (padrão) ---
teste_t <- t.test(cpc_fed, cpc_priv, 
                  alternative = "greater",  # unilateral à direita
                  var.equal = FALSE,
                  conf.level = 0.95)        # Welch (variâncias diferentes)

# --- Exibir resultado ---
print(teste_t)

# Plot

dados_plot <- data_CPC_SC %>%
  filter(categoria_administrativa %in% c("Pública Federal", "Privada com fins lucrativos"),
         !is.na(cpc_.continuo.))

dev.new()
print(ggplot(dados_plot, aes(x = categoria_administrativa, y = cpc_.continuo., fill = categoria_administrativa)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  labs(title = "Comparação do CPC entre Federais e Privadas (SC)",
       x = "Categoria Administrativa",
       y = "CPC (Contínuo)") +
  theme_minimal() +
  theme(legend.position = "none"))
