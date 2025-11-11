# Diretório da base de dados - Talvez precisa alterar, dependendo de onde o repositório estiver clonado.
setwd("~/Documents/ENADE-Estatisticas")

# ---- Filtragem das tabelas ----

# Retira eventuais entradas nulas
data_CPC_SC_limpo <- na.omit(data_CPC_SC[, c("ano", "codigo_da_ies", "nome_da_ies", "codigo_do_curso", "area_de_avaliacao", "conceito_enade_.continuo.")])
data_IDD_SC_limpo <- na.omit(data_IDD_SC[, c("ano", "codigo_da_ies", "nome_da_ies", "codigo_do_curso", "area_de_avaliacao", "idd_.continuo.")])

# Faz a amostragem de acordo com os valores de n calculados em "amostragem.r"
# Note que as amostras são independentes
set.seed(123)
amostra_CPC <- data_CPC_SC_limpo[sample(1:nrow(data_CPC_SC_limpo), 699, replace = FALSE), ]
rownames(amostra_CPC) <- NULL
amostra_IDD <- data_IDD_SC_limpo[sample(1:nrow(data_IDD_SC_limpo), 707, replace = FALSE), ]
rownames(amostra_IDD) <- NULL

# Faz o merge das tabelas pelas colunas-chave
data_merged <- merge(
  x = amostra_CPC,
  y = amostra_IDD,
  by = c("ano", "codigo_da_ies", "codigo_do_curso"),
  suffixes = c("_CPC", "_IDD")
)

# Seleciona as colunas principais para análise
data_corre_reg <- data.frame(
  ano = data_merged$ano,
  codigo_da_ies = data_merged$codigo_da_ies,
  codigo_do_curso = data_merged$codigo_do_curso,
  nome_da_ies = data_merged$nome_da_ies_CPC,
  nome_do_curso = data_merged$area_de_avaliacao_CPC,
  conceito_enade = data_merged$conceito_enade_.continuo.,
  idd = data_merged$idd_.continuo.
)

# Remove linhas com valores faltantes (NA) em Enade ou IDD
data_corre_reg <- subset(data_corre_reg, !is.na(conceito_enade) & !is.na(idd))
rownames(data_corre_reg) <- NULL


# ---- Teste de hipótese sobre correlação ----


# Teste de correlação entre ENADE e IDD
cor.test(data_corre_reg$conceito_enade, data_corre_reg$idd, method = "pearson")


# ---- Teste de hipótese sobre regressão ----


# Modelo de regressão linear (ENADE dependente de IDD)
modelo <- lm(conceito_enade ~ idd, data = data_corre_reg)

# Sumário com teste t dos coeficientes
summary(modelo)

# Gráfico da Relação ENADE e IDD contínuos
dev.new()
plot(data_corre_reg$idd, data_corre_reg$conceito_enade,
     xlab = "IDD", ylab = "Conceito ENADE",
     main = "Relação linear entre IDD e ENADE",
     pch = 19, col = "blue")

#Plot da Linha de regressão
abline(modelo, col = "red", lwd = 2)