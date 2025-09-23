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

# Conceito ENADE
freq_enade_continuo <- salvar_frequencia_continuo(
  data_CPC_SC$conceito_enade_.continuo.,
  "src/tabelas/SC/conceito_enade_continuo.csv"
)

# Indicador de Diferença entre Desempenhos Observados e Esperado (IDD)
freq_idd_continuo <- salvar_frequencia_continuo(
  data_IDD_SC$idd_.continuo.,
  "src/tabelas/SC/idd_continuo.csv"
)

# Conceito Preliminar do Curso (CPC)
freq_cpc_continuo <- salvar_frequencia_continuo(
  data_CPC_SC$cpc_.continuo.,
  "src/tabelas/SC/conceito_preliminar_curso_continuo.csv"
)

# Índice Geral de Cursos (IGC)
freq_igc_continuo <- salvar_frequencia_continuo(
  data_IGC_SC$igc_.continuo.,
  "src/tabelas/SC/igc_continuo.csv"
)


# ------- MEDIDAS DE TENDÊNCIA CENTRAL ----------- #






# ------------- MEDIDAS DE DISPERSÃO ------------- #






# ----------- RELAÇÕES ENTRE VARIÁVEIS ----------- #




