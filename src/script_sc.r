# Diretório da base de dados - Talvez precisa alterar, dependendo de onde o repositório estiver clonado.
setwd("~/Documentos/ENADE-Estatisticas") # !!!!!

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

# Indicador de Diferença entre os Desempenhos Observados e Esperado (IDD) - Continuo
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

# ------------------ GRÁFICOS ------------------- #


# Modalidade de Ensino por Curso
png("src/gráficos/pizza_modalidade_de_ensino_SC.png")
pie(modalidade_de_ensino_curso,
    main = "Distribuição por Modalidade de Ensino",
    col = c("#FFFF00", "#6e92f5ff"),   # cores diferentes para cada fatia
    labels = paste(c("Educação a
Distância", "Educação 
Presencial"), modalidade_de_ensino_curso, sep = ": "))
dev.off()

# Categoria Administrativa por Instituição - Completo: difícil visibilidade
png("src/gráficos/pizza_categoria_administrativa_SC_completo.png")
pie(categoria_administrativa, 
    main = "Distribuição por Categoria Administrativa",
    col = c("#f3736fff", "#fda58fff", "#6e92f5ff", "#95c3ffff", "#77e9b0ff", "#62bd85ff", "#0ece8eff"),  # cores diferentes para cada fatia
    radius = 0.65,
    labels = paste(c("Comunitária/
Confessional", "Especial", "Privada com Fins Lucrativos", "Privada sem Fins Lucrativos", "
Pública Estadual", "Pública Federal", "Pública Municipal"), categoria_administrativa, sep = ": "))
dev.off()

# Categoria Administrativa por Instituição - Divisão geral
categorias <- factor(c("Outros", "Outros", "Privada", "Privada", "Pública", "Pública", "Pública"))
lista_categorisada <- split(categoria_administrativa, categorias)
data <- c(sum(lista_categorisada$Outros), sum(lista_categorisada$Privada), sum(lista_categorisada$Pública))
png("src/gráficos/pizza_categoria_administrativa_SC_geral.png")
pie(data, 
    main = "Distribuição por Categoria Administrativa",
    col = c("#fda58fff", "#95c3ffff","#77e9b0ff"),   # cores diferentes para cada fatia
    labels = paste(c("Privada", "Pública", "Outros"), data, sep = ": "))  
dev.off()

# Categoria Administrativa por Instituição - Divisão específica

#Privada
png("src/gráficos/pizza_categoria_administrativa_SC_privada.png")
pie(lista_categorisada$Privada,
    main = "Distribuição por Categoria Administrativa",
    col = c("#6e92f5ff", "#95c3ffff"),   # cores diferentes para cada fatia
    labels = paste(c("Privada com Fins Lucrativos", "Privada sem Fins Lucrativos"), lista_categorisada$Privada, sep = ":"))
dev.off()

#Pública
png("src/gráficos/pizza_categoria_administrativa_SC_publica.png")
pie(lista_categorisada$Pública,
    main = "Distribuição por Categoria Administrativa",
    col = c("#77e9b0ff", "#62bd85ff", "#0ece8eff"),   # cores diferentes para cada fatia
    labels = paste(c("Pública 
Estadual", "Pública Federal", "Pública Municipal"), lista_categorisada$Pública, sep = ":"))
dev.off()

#Outros
png("src/gráficos/pizza_categoria_administrativa_SC_outros.png")
pie(lista_categorisada$Outros, 
    main = "Distribuição por Categoria Administrativa",
    col = c("#f3736fff", "#fda58fff"),   # cores diferentes para cada fatia
    labels = paste(c("Comunitária/
Confessional", "Especial"), lista_categorisada$Pública, sep = ":"))
dev.off()

# Conceito ENADE - Contínuo
png("src/gráficos/barras_conceito_enade_SC.png")
barplot(height = freq_enade_continuo, 
    names = c("" ,"" ,"" ,"" ,"" ,"" ,"" ,"" ,"" ,"" ,"" ,""),
    main = "Distribuição do Conceito ENADE",
    xlab = "Conceito Enade",
    ylab = "Particiantes",
    ylim = c(0, 250),
    cex.axis = 0.7,
    col = "#6e92f5ff")
    axis(1, at=(c(1.3,2.5,3.7,4.9,6.1,7.3,8.5,9.7,10.9,12.1,13.3,14.4)), 
    labels = c(0.414,0.829,1.24,1.66,2.07,2.49,2.9,3.31,3.73,4.14,4.56,4.98), cex.axis = 0.7)
dev.off()

png("src/gráficos/histograma_conceito_enade_SC.png")
hist(enade_continuo , breaks=50 , 
     col="#6e92f5ff" , 
     main="Distribuição do Conceito ENADE",
     include.lowest = TRUE,
     ylim = c(0,70),
     xlab = "Conceito Enade", ylab = "Participantes",
     border = "#6e92f5ff")

# Indicador de Diferença entre os Desempenhos Observados e Esperado (IDD) - Continuo
png("src/gráficos/barras_IDD_SC.png")
barplot(height = freq_idd_continuo,
    names = c("" ,"" ,"" ,"" ,"" ,"" ,"" ,"" ,"" ,"" ,"" ,""),
    main = "Distribuição do IDD",
    xlab = "Indicador de Diferença entre os Desempenhos Observados e Esperado",
    ylab = "Particiantes",
    ylim = c(0, 250),
    cex.axis = 0.7,
    col = "#6e92f5ff")
    axis(1, at=(c(1.3,2.5,3.7,4.9,6.1,7.3,8.5,9.7,10.9,12.1,13.3,14.4)), 
    labels = c(0.417,0.833,1.25,1.67,2.08,2.5,2.92,3.33,3.75,4.17,4.58,5), cex.axis = 0.7)
dev.off()

# Conceito Preliminar do Curso - CPC - Contínuo
png("src/gráficos/barras_CPC_SC.png")
barplot(height = freq_cpc_continuo,
    names = c("" ,"" ,"" ,"" ,"" ,"" ,"" ,"" ,"" ,"" ,"" ,""),
    main = "Distribuição do CPC",
    xlab = "Conceito Preliminar do Curso",
    ylab = "Particiantes",
    ylim = c(0, 250),
    cex.axis = 0.7,
    col = "#6e92f5ff")
    axis(1, at=(c(1.3,2.5,3.7,4.9,6.1,7.3,8.5,9.7,10.9,12.1,13.3,14.4)), 
    labels = c(1.28,1.58,1.88,2.19,2.49,2.79,3.09,3.39,3.7,4,4.3,4.61), cex.axis = 0.7)
dev.off()

# Índice Geral de Cursos - Contínuo
png("src/gráficos/barras_IGC_SC.png")
barplot(height = freq_igc_continuo,
    names = c("" ,"" ,"" ,"" ,"" ,"" ,"" ,"" ,""),
    main = "Distribuição do IGC",
    xlab = "Índice Geral de Cursos",
    ylab = "Instituições",
    ylim = c(0, 80),
    cex.axis = 0.7,
    col = "#6e92f5ff")
    axis(1, at=(c(1.3,2.5,3.7,4.9,6.1,7.3,8.5,9.7,10.9)), 
    labels = c(1.31,1.7,2.09,2.48,2.86,3.25,3.64,4.03,4.42), cex.axis = 0.7)
dev.off()

png("src/gráficos/histograma_IGC_SC.png")
hist(igc_continuo , breaks=30 , 
     col="#6e92f5ff" , 
     main="Distribuição do IGC",
     include.lowest = TRUE,
     ylim = c(0,25),
     xlab = "Índice Geral de Cursos", ylab = "Instituições",
     border = "#6e92f5ff")
  
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
