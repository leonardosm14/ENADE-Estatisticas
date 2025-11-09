# Diretório da base de dados - Talvez precisa alterar, dependendo de onde o repositório estiver clonado.
setwd("C:/Users/ruana/Desktop/ENADE-Estatisticas-hipoteses_correlacao_regressao/")

# Iremos utilizar a seguinte convenção:
#   dataA <- ano de 2021 -> então, teremos dataA_CPC, dataA_IDD e dataA_IGC
#   dataB <- ano de 2022 -> então, teremos dataB_CPC, dataB_IDD e dataB_IGC
#   dataC <- ano de 2023 -> então, teremos dataC_CPC, dataC_IDD e dataC_IGC

# Carregando as bases de dados
dataA_CPC <- read.csv("data/csv_data/CPC_2021.csv")
dataA_IDD <- read.csv("data/csv_data/IDD_2021.csv")
dataA_IGC <- read.csv("data/csv_data/IGC_2021.csv")

dataB_CPC <- read.csv("data/csv_data/CPC_2022.csv")
dataB_IDD <- read.csv("data/csv_data/IDD_2022.csv")
dataB_IGC <- read.csv("data/csv_data/IGC_2022.csv")

dataC_CPC <- read.csv("data/csv_data/CPC_2023.csv")
dataC_IDD <- read.csv("data/csv_data/IDD_2023.csv")
dataC_IGC <- read.csv("data/csv_data/IGC_2023.csv")

# Função para padronizar colunas com base em um padrão
padroniza_colunas <- function(df, col_padrao) {
  for(col in col_padrao) {
    if(!col %in% colnames(df)) {
      df[[col]] <- NA  # Cria coluna ausente com NA
    }
  }
  # Ordena as colunas para ficarem na mesma ordem do padrão
  df <- df[, col_padrao, drop = FALSE]
  return(df)
}

# Dados combinados usando 2023 como padrão
cols_CPC <- colnames(dataC_CPC)
cols_IDD <- colnames(dataC_IDD)
cols_IGC <- colnames(dataC_IGC)

dataA_CPC <- padroniza_colunas(dataA_CPC, cols_CPC)
dataB_CPC <- padroniza_colunas(dataB_CPC, cols_CPC)
# dataC_CPC já está no padrão, não precisa alterar

dataA_IDD <- padroniza_colunas(dataA_IDD, cols_IDD)
dataB_IDD <- padroniza_colunas(dataB_IDD, cols_IDD)

dataA_IGC <- padroniza_colunas(dataA_IGC, cols_IGC)
dataB_IGC <- padroniza_colunas(dataB_IGC, cols_IGC)
# dataC_IGC já está no padrão

# Combina os datasets
data_CPC <- rbind(dataA_CPC, dataB_CPC, dataC_CPC)
data_IDD <- rbind(dataA_IDD, dataB_IDD, dataC_IDD)
data_IGC <- rbind(dataA_IGC, dataB_IGC, dataC_IGC)
colnames(data_CPC) <- gsub("^X_", "", colnames(data_CPC))
colnames(data_IDD) <- gsub("^X_", "", colnames(data_IDD))
colnames(data_IGC) <- gsub("^X_", "", colnames(data_IGC))

# Para a análise exploratória, vamos utilizar como variáveis:
#  Qualitativas: Categoria Administrativa e Modalidade de Ensino;
#  Quantitativas: CPC, IDD e IGC -- IDD está presente na base de CPC.

# Tabelas de Frequência - Ignorando variáveis nulas
tab_cat_admin <- table(data_CPC$categoria_administrativa)
tab_cat_admin <- tab_cat_admin[names(tab_cat_admin) != ""]

tab_modalidade <- table(data_CPC$modalidade_de_ensino)
tab_modalidade <- tab_modalidade[names(tab_modalidade) != ""]

write.csv(tab_cat_admin, "src/tabelas/tabela_frequencia_categoria_administrativa.csv", row.names = FALSE)
write.csv(tab_modalidade, "src/tabelas/tabela_frequencia_modalidade_ensino.csv", row.names = FALSE)

# Gráfico de pizza para Categoria Administrativa
png("src/gráficos/pizza_categoria_administrativa.png", width = 800, height = 600)
pie(tab_cat_admin,
    main = "Distribuição por Categoria Administrativa",
    col = rainbow(length(tab_cat_admin)),   # cores diferentes para cada fatia
    labels = paste(names(tab_cat_admin), tab_cat_admin, sep = ": "))  # nomes + valores
dev.off()

# Gráfico de pizza para Modalidade de Ensino
png("src/gráficos/pizza_modalidade_ensino.png", width = 800, height = 600)
pie(tab_modalidade,
    main = "Distribuição por Modalidade de Ensino",
    col = rainbow(length(tab_modalidade)),
    labels = paste(names(tab_modalidade), tab_modalidade, sep = ": "))
dev.off()

