setwd("~/Documents/ENADE-Estatisticas")
source(file = "src/script_sc.r")

# --- Pacotes ---
library(dplyr)

# --- Amostra aleatória de instituições ---
set.seed(123)
amostra_n <- 150
amostra_instituicoes <- data_IGC_SC %>%
  sample_n(amostra_n)

# --- Contagem de instituições federais ---
sucesso <- sum(amostra_instituicoes$categoria_administrativa == "Pública Federal")
total <- nrow(amostra_instituicoes)
prop_observada <- sucesso / total

cat("Tamanho da amostra:", total, "\n")
cat("Número de instituições federais na amostra:", sucesso, "\n")
cat("Proporção observada:", round(prop_observada, 3), "\n\n")

# --- Teste de hipótese para proporção única (bilateral) ---
# H0: p = 0.25
# H1: p ≠ 0.25
teste_prop <- prop.test(x = sucesso, n = total, p = 0.25, alternative = "two.sided", correct = FALSE)
print(teste_prop)

# --- Interpretação ---
cat("\nInterpretação:\n")
if (teste_prop$p.value < 0.05) {
  cat("Rejeita-se H0: A proporção de instituições federais é significativamente diferente de 25%.\n")
} else {
  cat("Não se rejeita H0: Não há evidências de que a proporção de instituições federais seja diferente de 25%.\n")
}

# --- Gráfico do teste bilateral e salvamento em PDF ---
p0 <- 0.25
phat <- prop_observada
n <- total
z <- (phat - p0) / sqrt(p0 * (1 - p0) / n)
print(z)
alpha <- 0.05
zcrit <- qnorm(1 - alpha / 2)
print(zcrit)

# Caminho de saída
pdf("teste_bilateral.pdf", width = 10, height = 5)

# Dados da distribuição normal padrão
x <- seq(-6, 6, length = 1000)
y <- dnorm(x)

# Plot principal
plot(x, y, type = "l", lwd = 2, col = "black",
     main = "Teste Bilateral para Proporção",
     xlab = "Estatística Z", ylab = "Densidade de probabilidade")

# Regiões críticas
polygon(c(x[x >= zcrit], rev(x[x >= zcrit])),
        c(y[x >= zcrit], rep(0, sum(x >= zcrit))),
        col = rgb(1, 0, 0, 0.4), border = NA)
polygon(c(x[x <= -zcrit], rev(x[x <= -zcrit])),
        c(y[x <= -zcrit], rep(0, sum(x <= -zcrit))),
        col = rgb(1, 0, 0, 0.4), border = NA)

# Linha do valor observado
abline(v = z, col = "blue", lwd = 2)
abline(v = c(-zcrit, zcrit), col = "red", lty = 2)

# Legenda e texto
legend("topright",
       legend = c("Z observado", "Regioes criticas (alpha = 0.05)"),
       col = c("blue", "red"), lwd = 2, lty = c(1, 2), bty = "n")


text(z, dnorm(z) + 0.02, labels = sprintf("z = %.2f", z),
     col = "blue", pos = ifelse(z < 0, 2, 4))

dev.off()

