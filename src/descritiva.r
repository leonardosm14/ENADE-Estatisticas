# --- Pacotes ---
library("dplyr")

# Carrega os dados filtrados de Santa Catarina
source(file = "script_sc.r")

# --- Filtrando os dados ---

# ENADE
dados_enade <- data_CPC_SC %>%
  select(conceito_enade_.continuo.) %>%
  filter(!is.na(conceito_enade_.continuo.))

# CPC
dados_cpc <- data_CPC_SC %>%
  select(cpc_.continuo.) %>%
  filter(!is.na(cpc_.continuo.))

# IDD
dados_idd <- data_IDD_SC %>%
  select(idd_.continuo.) %>%
  filter(!is.na(idd_.continuo.))

# IGC
dados_igc <- data_IGC_SC %>%
  select(igc_.continuo.) %>%
  filter(!is.na(igc_.continuo.))

# Modalidade de Ensino
dados_me <- data_CPC_SC %>%
  select(modalidade_de_ensino) %>%
  filter(!is.na(modalidade_de_ensino))

# Categoria Administrativa
dados_ca <- data_IGC_SC %>%
  select(categoria_administrativa) %>%
  filter(!is.na(categoria_administrativa))

# --- Criar amostra aleatória de tamanho n = 426, conforme "amostragem.r" ---

# ENADE
set.seed(123)  # para reprodutibilidade
amostra_enade <- dados_enade %>%
  sample_n(size = 699, replace = FALSE)

#CPC
set.seed(123)  # para reprodutibilidade
amostra_cpc <- dados_cpc %>%
  sample_n(size = 426, replace = FALSE)

# IDD
set.seed(123)  # para reprodutibilidade
amostra_idd <- dados_idd %>%
  sample_n(size = 707, replace = FALSE)

# IGC
set.seed(123)  # para reprodutibilidade
amostra_igc <- dados_igc %>%
  sample_n(size = 163, replace = FALSE)

# Modalidade de Enino
set.seed(123)  # para reprodutibilidade
amostra_me <- dados_me %>%
  sample_n(size = 300, replace = FALSE)

# Categoria Administrativa
set.seed(123)  # para reprodutibilidade
amostra_ca <- dados_ca %>%
  sample_n(size = 150, replace = FALSE)

# --- Estatísticas Descritivas ---

# ENADE
media_enade <- mean(amostra_enade$conceito_enade_.continuo.)
mediana_enade <- median(amostra_enade$conceito_enade_.continuo.)
dp_enade <- sd(amostra_enade$conceito_enade_.continuo.)
erro_enade = qt((1+0.95)/2, 699-1) * (dp_enade/(sqrt(699)))

cat("Média Conceito Enade:", round(media_enade, 4), "\n")
cat("Mediana Conceito Enade:", round(media_enade, 4), "\n")
cat("Desvio Padrão Conceito Enade:", round(dp_enade, 4), "\n")
cat("Intervalo de Confiança da Média do Conceito Enade:", round(media_enade-erro_enade, 4), "-", round(media_enade+erro_enade, 4), "\n")

# CPC
media_cpc <- mean(amostra_cpc$cpc_.continuo.)
mediana_cpc <- median(amostra_cpc$cpc_.continuo.)
dp_cpc <- sd(amostra_cpc$cpc_.continuo.)
erro_cpc = qt((1+0.95)/2, 426-1) * (dp_cpc/(sqrt(426)))

cat("Média CPC:", round(media_cpc, 4), "\n")
cat("Mediana CPC:", round(mediana_cpc, 4), "\n")
cat("Desvio Padrão CPC:", round(dp_cpc, 4), "\n")
cat("Intervalo de Confiança da Média do CPC:", round(media_cpc-erro_cpc, 4), "-", round(media_cpc+erro_cpc, 4), "\n")

# IDD
media_idd <- mean(amostra_idd$idd_.continuo.)
mediana_idd <- median(amostra_idd$idd_.continuo.)
dp_idd <- sd(amostra_idd$idd_.continuo.)
erro_idd = qt((1+0.95)/2, 707-1) * (dp_idd/(sqrt(707)))

cat("Média IDD:", round(media_idd, 4), "\n")
cat("Mediana IDD:", round(mediana_idd, 4), "\n")
cat("Desvio Padrão IDD:", round(dp_idd, 4), "\n")
cat("Intervalo de Confiança da Média do IDD:", round(media_idd-erro_idd, 4), "-", round(media_idd+erro_idd, 4), "\n")

# IGC
media_igc <- mean(amostra_igc$igc_.continuo.)
mediana_igc <- median(amostra_igc$igc_.continuo.)
dp_igc <- sd(amostra_igc$igc_.continuo.)
erro_igc = qt((1+0.95)/2, 163-1) * (dp_igc/(sqrt(163)))

cat("Média IGC:", round(media_igc, 4), "\n")
cat("Mediana IGC:", round(mediana_igc, 4), "\n")
cat("Desvio Padrão IGC:", round(dp_igc, 4), "\n")
cat("Intervalo de Confiança da Média do IGC:", round(media_igc-erro_igc, 4), "-", round(media_igc+erro_igc, 4), "\n")

# Modalidade de ensino
presencial <- sum(amostra_me$modalidade_de_ensino == "Educação Presencial")
total <- nrow(amostra_me)
prop_me <- (presencial / total)
erro_me <- qnorm((1+0.95)/2)*(sqrt(prop_me*(1-prop_me)/300))

cat("Proporção de Cursos Presenciais:", round(prop_me*100, 4), "\n")
cat("Intervalo de Confiança da Proporção de Cursos Presenciais:", round((prop_me-erro_me)*100, 4), "-", round((prop_me+erro_me)*100, 4), "\n")

# Categoria Adiministrativa
publicas <- sum(amostra_ca$categoria_administrativa == "Pública Federal",amostra_ca$categoria_administrativa == "Pública Estadual",amostra_ca$categoria_administrativa == "Pública Munucipal")
total <- nrow(amostra_ca)
prop_ca <- (publicas / total)
erro_ca <- qnorm((1+0.95)/2)*(sqrt(prop_ca*(1-prop_ca)/150))

cat("Proporção de Instituições Públicas:", round(prop_ca*100, 4), "\n")
cat("Intervalo de Confiança da Proporção de Instituições Públicas:", round((prop_ca-erro_ca)*100, 4), "-", round((prop_ca+erro_ca)*100, 4), "\n")
