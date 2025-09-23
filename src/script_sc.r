# Diretório da base de dados - Talvez precisa alterar, dependendo de onde o repositório estiver clonado.
setwd("~/Documents/ENADE-Estatisticas")

# Vamos reutilizar os dados agrupados em script_geral.r
source(file="src/script_geral.r")

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




