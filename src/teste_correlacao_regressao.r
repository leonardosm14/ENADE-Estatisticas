# Diretório da base de dados - Talvez precisa alterar, dependendo de onde o repositório estiver clonado.
setwd("C:/Users/ruana/Desktop/trab")

# Dados filtrados para Santa Catarina
data_CPC_SC <- read.csv("data/tabelas/data_CPC_SC.csv")
data_IDD_SC <- read.csv("data/tabelas/data_IDD_SC.csv")
data_IGC_SC <- read.csv("data/tabelas/data_IGC_SC.csv")

# ---- Filtragem das tabelas ----


# CPC (possui também o conceito Enade)
data_CPC_SC_filtrado <- data_CPC_SC[, c(
  "ano",
  "codigo_da_ies",
  "nome_da_ies",
  "codigo_do_curso",
  "area_de_avaliacao",
  "conceito_enade_.continuo.",
  "cpc_.continuo."
)]

# IDD
data_IDD_SC_filtrado <- data_IDD_SC[, c(
  "ano",
  "codigo_da_ies",
  "nome_da_ies",
  "codigo_do_curso",
  "area_de_avaliacao",
  "idd_.continuo."
)]

# IGC
data_IGC_SC_filtrado <- data_IGC_SC[, c(
  "ano",
  "codigo_da_ies",
  "nome_da_ies",
  "igc_.continuo."
)]

# Faz o merge das tabelas pelas colunas-chave
data_merged <- merge(
  x = data_CPC_SC_filtrado,
  y = data_IDD_SC_filtrado,
  by = c("ano", "codigo_da_ies", "codigo_do_curso"),
  suffixes = c("_CPC", "_IDD")
)

# Seleciona as colunas principais para análise
data_corre <- data.frame(
  ano = data_merged$ano,
  codigo_da_ies = data_merged$codigo_da_ies,
  codigo_do_curso = data_merged$codigo_do_curso,
  nome_da_ies = data_merged$nome_da_ies_CPC,
  nome_do_curso = data_merged$area_de_avaliacao_CPC,
  conceito_enade = data_merged$conceito_enade_.continuo.,
  idd = data_merged$idd_.continuo.
)

# Remove linhas com valores faltantes (NA) em Enade ou IDD
data_corre <- subset(data_corre, !is.na(conceito_enade) & !is.na(idd))
rownames(data_corre) <- NULL

# Salva a nova tabela
write.csv(data_corre, "data/tabelas/data_IDD_ENADE_SC.csv", row.names = FALSE)


# ---- Teste de hipótese sobre correlação ----


# Teste de correlação entre ENADE e IDD
cor.test(data_corre$conceito_enade, data_corre$idd, method = "pearson")


# ---- Teste de hipótese sobre regressão ----


# Modelo de regressão linear (ENADE dependente de IDD)
modelo <- lm(conceito_enade ~ idd, data = data_corre)

# Sumário com teste t dos coeficientes
summary(modelo)

# Gráfico da Relação ENADE e IDD contínuos
plot(data_corre$idd, data_corre$conceito_enade,
     xlab = "IDD", ylab = "Conceito ENADE",
     main = "Relação linear entre IDD e ENADE",
     pch = 19, col = "blue")

#Plot da Linha de regressão
abline(modelo, col = "red", lwd = 2)

