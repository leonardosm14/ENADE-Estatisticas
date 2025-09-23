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