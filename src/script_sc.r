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
freq_enade_continuo <- salvar_frequencia_continuo(valores = enade_continuo, arquivo_saida = "src/tabelas/SC/conceito_enade_continuo.csv")

# Indicador de Diferença entre os Desempenhos Observados e Esperado (IDD) - Continuo
idd_continuo <- data_IDD_SC$idd_.continuo.
freq_idd_continuo <- salvar_frequencia_continuo(valores = idd_continuo, arquivo_saida = "src/tabelas/SC/idd_continuo.csv")

# Conceito Preliminar do Curso - CPC - Contínuo
cpc_continuo <- data_CPC_SC$cpc_.continuo.
freq_cpc_continuo <- salvar_frequencia_continuo(valores = cpc_continuo, arquivo_saida = "src/tabelas/SC/conceito_preliminar_curso_continuo.csv")

igc_continuo <- data_IGC_SC$igc_.continuo.
freq_igc_continuo <- salvar_frequencia_continuo(valores = igc_continuo, arquivo_saida = "src/tabelas/SC/igc_continuo.csv")

# ------------------ GRÁFICOS ------------------- #


# Modalidade de Ensino por Curso
png("src/gráficos/pizza_modalidade_de_ensino_SC.png")
pie(modalidade_de_ensino_curso,
    main = "Distribuição por Modalidade de Ensino",
    col = c("#FFFF00", "#6e92f5ff"),   # cores diferentes para cada fatia
    labels = paste(c("Educação a
Distância", "Educação 
Presencial"), ":
", round((modalidade_de_ensino_curso/1349)*100, 2), "%"))
dev.off()

# Categoria Administrativa por Instituição - Completo: difícil visibilidade
png("src/gráficos/pizza_categoria_administrativa_SC_completo.png")
pie(categoria_administrativa, 
    main = "",
    col = c("#f3736fff", "#fda58fff", "#6e92f5ff", "#95c3ffff", "#77e9b0ff", "#62bd85ff", "#0ece8eff"),  # cores diferentes para cada fatia
    radius = 0.44,
    labels = paste(c("Comunitária/
Confessional", "Especial", "Privada com Fins
Lucrativos", "
Privada sem Fins 
Lucrativos", "
Pública Estadual", "Pública Federal", "Pública Municipal"),": ",round((categoria_administrativa/116)*100, 2),"% "))
title("Distribuição por Categoria Administrativa", line = -2, cex = 0.5)
dev.off()

# Categoria Administrativa por Instituição - Divisão geral
categorias <- factor(c("Outros", "Outros", "Privada", "Privada", "Pública", "Pública", "Pública"))
lista_categorisada <- split(categoria_administrativa, categorias)
data <- c(sum(lista_categorisada$Outros), sum(lista_categorisada$Privada), sum(lista_categorisada$Pública))
png("src/gráficos/pizza_categoria_administrativa_SC_geral.png")
pie(data, 
    main = "Distribuição por Categoria Administrativa - Agrupada",
    radius = 0.8,
    col = c("#fda58fff", "#95c3ffff","#77e9b0ff"),   # cores diferentes para cada fatia
    labels = paste(c("Outros", "Privada", "Pública"), ": ", round((data/116)*100, 2), "%"))
dev.off()

# Categoria Administrativa por Instituição - Divisão específica

#Privada
png("src/gráficos/pizza_categoria_administrativa_SC_privada.png")
pie(lista_categorisada$Privada,
    main = "Distribuição por Categoria Administrativa - Privadas",
    radius = 0.7,
    col = c("#6e92f5ff", "#95c3ffff"),   # cores diferentes para cada fatia
    labels = paste(c("Privada com Fins 
Lucrativos", "

Privada sem Fins 
Lucrativos"), ": ", round((lista_categorisada$Privada/94)*100, 2), "%
"))
dev.off()

#Pública
png("src/gráficos/pizza_categoria_administrativa_SC_publica.png")
pie(lista_categorisada$Pública,
    main = "",
    radius = 0.6,
    col = c("#77e9b0ff", "#62bd85ff", "#0ece8eff"),   # cores diferentes para cada fatia
    labels = paste(c("Pública 
Estadual", "Pública Federal", "Pública Municipal"), ": ", round((lista_categorisada$Pública/8)*100, 2), "%"))
title("Distribuição por Categoria Administrativa - Públias", line = -2, cex = 0.5)
dev.off()

#Outros
png("src/gráficos/pizza_categoria_administrativa_SC_outros.png")
pie(lista_categorisada$Outros, 
    main = "Distribuição por Categoria Administrativa - Outros",
    radius = 0.9,
    col = c("#f3736fff", "#fda58fff"),   # cores diferentes para cada fatia
    labels = paste(c("Comunitária/
Confessional", "Especial"), ": 
", round((lista_categorisada$Outros/14)*100, 2), "%"))
dev.off()

# Conceito ENADE - Contínuo
png("src/gráficos/barras_conceito_enade_SC.png")
barplot(height = as.vector(freq_enade_continuo$Frequência), 
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
barplot(height = freq_idd_continuo$Frequência,
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
barplot(height = freq_cpc_continuo$Frequência,
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
barplot(height = freq_igc_continuo$Frequência,
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
     xlim = c(0.9,5),
     xlab = "Índice Geral de Cursos", ylab = "Instituições",
     border = "#6e92f5ff")
  
# ------- MEDIDAS DE TENDÊNCIA CENTRAL ----------- #

cat("\n--- Calculando Medidas de Resumo para Santa Catarina ---\n")

# Lista das variáveis quantitativas de SC para analisar
vars_sc_para_analise <- list(
  "Conceito ENADE (Contínuo)" = data_CPC_SC$conceito_enade_.continuo.,
  "IDD (Contínuo)" = data_IDD_SC$idd_.continuo.,
  "CPC (Contínuo)" = data_CPC_SC$cpc_.continuo.,
  "IGC (Contínuo)" = data_IGC_SC$igc_.continuo.
)

# Criando uma tabela única com as principais medidas de resumo
tabela_medidas_resumo_sc <- data.frame(
  Indicador = names(vars_sc_para_analise),
  Media = sapply(vars_sc_para_analise, function(x) mean(x, na.rm = TRUE)),
  Mediana = sapply(vars_sc_para_analise, function(x) median(x, na.rm = TRUE)),
  Desvio_Padrao = sapply(vars_sc_para_analise, function(x) sd(x, na.rm = TRUE)),
  Variancia = sapply(vars_sc_para_analise, function(x) var(x, na.rm = TRUE)),
  CV = sapply(vars_sc_para_analise, function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100),
  Minimo = sapply(vars_sc_para_analise, function(x) min(x, na.rm = TRUE)),
  Maximo = sapply(vars_sc_para_analise, function(x) max(x, na.rm = TRUE))
)

# Adicionando a Amplitude (Max - Min)
tabela_medidas_resumo_sc$Amplitude <- tabela_medidas_resumo_sc$Maximo - tabela_medidas_resumo_sc$Minimo

# Arredondando os valores para 3 casas decimais para melhor visualização
tabela_medidas_resumo_sc[, -1] <- round(tabela_medidas_resumo_sc[, -1], 3)

# Salvando a tabela em um arquivo CSV na pasta correta
write.csv(tabela_medidas_resumo_sc, "src/tabelas/SC/tabela_medidas_resumo_sc.csv", row.names = FALSE)

# ---------- GRÁFICOS DE DISTRIBUIÇÃO NORMAL ----------- #

criar_curva_normal <- function(dados, titulo, nome_arquivo, cor = "#6e92f5ff") {
  png(nome_arquivo, width = 800, height = 600)
  
  h <- hist(dados, plot = FALSE, breaks = 30)
  
  plot(h, freq = FALSE, main = titulo, 
       xlab = titulo, ylab = "Densidade", 
       col = cor, border = "white",
       ylim = c(0, max(h$density) * 1.1))
  
  if(length(dados) > 1 && sd(dados, na.rm = TRUE) > 0) {
    x <- seq(min(dados, na.rm = TRUE), max(dados, na.rm = TRUE), length = 100)
    y <- dnorm(x, mean = mean(dados, na.rm = TRUE), sd = sd(dados, na.rm = TRUE))
    lines(x, y, col = "red", lwd = 2)
    
    legend("topright", legend = "Curva Normal", col = "red", lwd = 2, bty = "n")
  }
  
  dev.off()
}

criar_curva_normal(enade_continuo, "Distribuição Normal - Conceito ENADE", 
                   "src/gráficos/curva_normal_enade_SC.png")

criar_curva_normal(idd_continuo, "Distribuição Normal - IDD", 
                   "src/gráficos/curva_normal_idd_SC.png")

criar_curva_normal(cpc_continuo, "Distribuição Normal - CPC", 
                   "src/gráficos/curva_normal_cpc_SC.png")

criar_curva_normal(igc_continuo, "Distribuição Normal - IGC", 
                   "src/gráficos/curva_normal_igc_SC.png")

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

data_IGC_SC$uni_cat <- paste(data_IGC_SC$nome_da_ies, data_IGC_SC$categoria_administrativa, sep = "_")

# Calcular média de IGC por (universidade + categoria)
uni_cat_ids <- unique(data_IGC_SC$uni_cat)
igc_universidades <- data.frame(nome_da_ies = character(), categoria_administrativa = character(), igc_medio = numeric(), stringsAsFactors = FALSE)

for (id in uni_cat_ids) {
  linhas <- data_IGC_SC[data_IGC_SC$uni_cat == id, ]
  media_igc <- mean(linhas$igc_.continuo., na.rm = TRUE)
  uni <- linhas$nome_da_ies[1]
  cat_admin <- linhas$categoria_administrativa[1]
  
  igc_universidades <- rbind(igc_universidades, data.frame(nome_da_ies = uni, categoria_administrativa = cat_admin, igc_medio = media_igc, stringsAsFactors = FALSE))
}

df_igc_bins <- salvar_frequencia_continuo(valores = igc_universidades$igc_medio, arquivo_saida = "src/tabelas/SC/igc_continuo_universidades_cat.csv")

k <- nrow(df_igc_bins)
igc_bins <- cut(igc_universidades$igc_medio, breaks = k, include.lowest = TRUE, right = FALSE)

tabela_contingencia <- table(igc_universidades$categoria_administrativa, igc_bins)

write.csv(as.data.frame(tabela_contingencia), "src/tabelas/SC/igc_categoria_administrativa_contingencia.csv", row.names = FALSE)

# ---------- GRÁFICOS DE COMPARAÇÃO ENTRE VARIÁVEIS QUALITATIVAS ----------- #

# Tabela de contingência correta entre modalidade e categoria administrativa (por cursos)
tabela_contingencia_modalidade_categoria <- table(data_CPC_SC$modalidade_de_ensino, data_CPC_SC$categoria_administrativa)
write.csv(as.data.frame(tabela_contingencia_modalidade_categoria), "src/tabelas/SC/contingencia_modalidade_categoria.csv", row.names = TRUE)

png("src/gráficos/proporcao_modalidade_por_categoria.png")
par(mar = c(5, 10, 4, 2))  # bottom, left, top, right; mais espaço para nomes longos
prop_table <- prop.table(tabela_contingencia_modalidade_categoria, margin = 2) * 100

# Barras horizontais
barplot(prop_table,
        main = "Proporção de Modalidade por Categoria Administrativa (%)",
        xlab = "Percentual por Curso (%)",
        col = c("#FFFF00", "#6e92f5ff"),
        horiz = TRUE,
        las = 1,         # rótulos horizontais legíveis
        cex.names = 0.9)
legend("topright", legend = rownames(prop_table), fill = c("#FFFF00", "#6e92f5ff"))
dev.off()


