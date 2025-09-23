# Diretório da base de dados - Talvez precisa alterar, dependendo de onde o repositório estiver clonado.
setwd("~/Documents/ENADE-Estatisticas")

# Vamos reutilizar os dados agrupados em script_geral.r
source(file="src/script_geral.r")
source(file="src/utils.r")

# Dados filtrados para Santa Catarina relativos a CPC - Instituições se repetem para cada curso
data_CPC_SC <- subset(data_CPC, sigla_da_uf == "SC")

# Dados filtrados para Santa Catarina relativos a IDD - Instituições se repetem para cada curso
data_IDD_SC <- subset(data_IDD, sigla_da_uf == "SC")

# Dados filtrados para Santa Catarina relativos a IGC - Instituições aparecem uma única vez
data_IGC_SC <- subset(data_IGC, sigla_da_uf == "SC")

# ---------- TABELAS DE FREQUÊNCIA --------------- #

# Modalidade de Ensino por Curso
modalidade_de_ensino_curso <- table(data_CPC_SC$modalidade_de_ensino)
write.csv(modalidade_de_ensino_curso, "src/tabelas/SC/modalidade_ensino_curso.csv", row.names = FALSE)

# Categoria Administrativa por Instituição - Aparições únicas
instituicoes_unicas <- unique(data_IGC_SC[, c("nome_da_ies", "codigo_da_ies", "categoria_administrativa")])
categoria_administrativa <- table(instituicoes_unicas$categoria_administrativa)
write.csv(as.data.frame(categoria_administrativa), "src/tabelas/SC/categoria_administrativa_instituicao.csv", row.names = FALSE)

# Conceito ENADE - Contínuo
enade_continuo <- data_CPC_SC$conceito_enade_.continuo.
# Número de classes pelo método de Sturges
k <- nclass.Sturges(enade_continuo)
classes <- cut(enade_continuo, breaks = k, include.lowest = TRUE, right = FALSE)
freq_enade_continuo <- table(classes)
write.csv(as.data.frame(freq_enade_continuo), "src/tabelas/SC/conceito_enade_continuo.csv", row.names = FALSE)

# Indicador de Diferença entre os Desempenhos Observados e Esperado (IDD) - Contínuo
idd_continuo <- data_IDD_SC$idd_.continuo.
k <- nclass.Sturges(idd_continuo)
classes <- cut(idd_continuo, breaks = k, include.lowest = TRUE, right = FALSE)
freq_idd_continuo <- table(classes)
write.csv(as.data.frame(freq_idd_continuo), "src/tabelas/SC/idd_continuo.csv", row.names = FALSE)

# Conceito Preliminar do Curso - CPC - Contínuo
cpc_continuo <- data_CPC_SC$cpc_.continuo.
k <- nclass.Sturges(cpc_continuo)
classes <- cut(cpc_continuo, breaks = k, include.lowest = TRUE, right = FALSE)
freq_cpc_continuo <- table(classes)
write.csv(as.data.frame(freq_cpc_continuo), "src/tabelas/SC/conceito_preliminar_curso_continuo.csv", row.names = FALSE)

# Índice Geral de Cursos - Contínuo
igc_continuo <- data_IGC_SC$igc_.continuo.
k <- nclass.Sturges(igc_continuo)
classes <- cut(igc_continuo, breaks = k, include.lowest = TRUE, right = FALSE)
freq_igc_continuo <- table(classes)
write.csv(as.data.frame(freq_igc_continuo), "src/tabelas/SC/igc_continuo.csv", row.names = FALSE)


# ------- MEDIDAS DE TENDÊNCIA CENTRAL ----------- #






# ------------- MEDIDAS DE DISPERSÃO ------------- #






# ----------- RELAÇÕES ENTRE VARIÁVEIS ----------- #

# Vetor de nomes formatados para variáveis quantitativas
nomes_quantitativas <- c(
  "conceito_enade_.continuo." = "Conceito ENADE - Contínuo",
  "cpc_.continuo." = "CPC - Contínuo",
  "idd_.continuo." = "IDD - Contínuo",
  "igc_.continuo." = "IGC - Contínuo"
)

# Vetor de nomes formatados para variáveis qualitativas
nomes_qualitativas <- c(
  "categoria_administrativa" = "Categoria Administrativa",
  "modalidade_de_ensino" = "Modalidade de Ensino"
)

# Variáveis qualitativas
qualitativas <- c("categoria_administrativa", "modalidade_de_ensino")

# Variáveis quantitativas de CPC
quant_cpc <- c("conceito_enade_.continuo.", "cpc_.continuo.")

# Variáveis quantitativas de IDD
quant_idd <- c("idd_.continuo.")

# Relações em data_CPC_SC
for (q in qualitativas) {
  for (var in quant_cpc) {
    plotar_relacao(data_CPC_SC, q, var)
  }
}

# Relações em data_IDD_SC
for (q in qualitativas) {
  for (var in quant_idd) {
    plotar_relacao(data_IDD_SC, q, var)
  }
}

# ---TABELA DE CONTINGÊNCIA IGC x CATEGORIA ADMINISTRATIVA--- #

# Criar identificador único: universidade + categoria
data_IGC_SC$uni_cat <- paste(data_IGC_SC$nome_da_ies,data_IGC_SC$categoria_administrativa,sep = "_")

# Calcular média de IGC por (universidade + categoria)
uni_cat_ids <- unique(data_IGC_SC$uni_cat)
igc_universidades <- data.frame(nome_da_ies = character(),categoria_administrativa = character(),igc_medio = numeric(),stringsAsFactors = FALSE)

for (id in uni_cat_ids) {
  linhas <- data_IGC_SC[data_IGC_SC$uni_cat == id, ]
  media_igc <- mean(linhas$igc_.continuo., na.rm = TRUE)
  uni <- linhas$nome_da_ies[1]
  cat_admin <- linhas$categoria_administrativa[1]
  
  igc_universidades <- rbind(igc_universidades,data.frame(nome_da_ies = uni,categoria_administrativa = cat_admin,igc_medio = media_igc,stringsAsFactors = FALSE))
}

# Criar classes com função em utils.r
df_igc_bins <- salvar_frequencia_continuo(valores = igc_universidades$igc_medio,arquivo_saida = "src/tabelas/SC/igc_continuo_universidades_cat.csv")

k <- nrow(df_igc_bins)

igc_bins <- cut(igc_universidades$igc_medio,breaks = k,include.lowest = TRUE, right = FALSE)

# Criar tabela de contingência
tabela_contingencia <- table(igc_universidades$categoria_administrativa,igc_bins)
print(tabela_contingencia)

# Salvar CSV
write.csv(as.data.frame(tabela_contingencia),"src/tabelas/SC/igc_categoria_administrativa_contingencia.csv", row.names = FALSE)