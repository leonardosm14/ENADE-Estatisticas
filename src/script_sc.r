# Diretório da base de dados - Talvez precisa alterar, dependendo de onde o repositório estiver clonado.
setwd("~/Documents/ENADE-Estatisticas")

# Vamos reutilizar os dados agrupados em script_geral.r
source(file="src/script_geral.r")

# Dados filtrados para Santa Catarina relativos a CPC e IDD
data_CPC_SC <- subset(data_CPC, sigla_da_uf == "SC")
data_CPC_SC

# Dados filtrados para Santa Catarina relativos a IGC
data_IGC_SC <- subset(data_IGC, sigla_da_uf == "SC")
data_IGC_SC$ano

# ---------- TABELAS DE FREQUÊNCIA --------------- #





# ------- MEDIDAS DE TENDÊNCIA CENTRAL ----------- #






# ------------- MEDIDAS DE DISPERSÃO ------------- #






# ----------- RELAÇÕES ENTRE VARIÁVEIS ----------- #




