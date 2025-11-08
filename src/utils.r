setwd("~/Documentos/UFSC/ENADE-Estatisticas")

salvar_frequencia_continuo <- function(valores, arquivo_saida, right = FALSE, include.lowest = TRUE) {
  # Número de classes pelo método de Sturges
  k <- nclass.Sturges(valores)
  classes <- cut(valores, breaks = k, include.lowest = include.lowest, right = right)
  freq <- table(classes)
  
  df_freq <- data.frame(
    Limites = names(freq),
    Frequência = as.vector(freq)
  )
  
  # Salvar CSV
  write.csv(df_freq, arquivo_saida, row.names = FALSE)
  return(df_freq)
}

plotar_relacao <- function(dados, qualitativa, quantitativa) {
  # Nome formatado da qualitativa
  nome_ql <- ifelse(qualitativa %in% names(nomes_qualitativas),
                    nomes_qualitativas[[qualitativa]], qualitativa)
  # Nome formatado da quantitativa
  nome_qt <- ifelse(quantitativa %in% names(nomes_quantitativas),
                    nomes_quantitativas[[quantitativa]], quantitativa)
  
  # Diretório onde os gráficos serão salvos
  dir_graficos <- "src/gráficos/"
  # Criar nome do arquivo dinamicamente com base nas variáveis
  arquivo_saida <- paste0(dir_graficos,"Boxplot_de_", gsub(" ", "_", nome_qt),"_por_", gsub(" ", "_", nome_ql),".png")
  png(filename = arquivo_saida, width = 1500, height = 800)
  boxplot(dados[[quantitativa]] ~ dados[[qualitativa]],main = paste(nome_qt, "por", nome_ql),xlab = nome_ql, ylab = nome_qt,col = "lightblue", las = 1)
  dev.off()
}