#AnaBeatrizDuarteDomingues_RM510302_fase2_cap7
#JuniorRodriguesDaSilva_RM559451_fase2_cap7
install.packages("languageserver")
install.packages("ggplot2")
library(ggplot2)

#Dados da variável quantitativa
volume_colhido <- c(8.5, 9, 8.7, 10.1, 9.5, 8.3, 8.1, 9.8, 9.2, 8.9, 8.6, 8.8, 8.4, 9.4, 8.7, 8.9, 9.7, 8.2, 9.1, 9.6, 8.7, 8.5, 10.2, 10.3, 9, 8.7, 9.9, 8.8, 9.1, 9.3)
codigos <- c("MX001", "BTG02", "MX003", "MX004", "BTG05", "MX006", "BTG07", "MX008", "MX009", "BTG10", "MX011", "BTG12", "MX013", "MX014", "BTG15", "MX016", "BTG17", "MX018", "MX019", "BTG20", "MX021", "BTG22", "MX023", "MX024", "BTG25", "MX026", "BTG27", "MX028", "MX029", "BTG30")

#Dados da Variável Qualitativa
qualid_graos <- c("Bom", "Excelente", "Médio", "Bom", "Ruim", "Bom", "Médio", "Excelente", "Bom", "Médio", "Bom", "Excelente", "Médio", "Bom", "Ruim", "Bom", "Excelente", "Médio", "Bom", "Ruim", "Bom", "Médio", "Excelente", "Bom", "Médio", "Bom", "Excelente", "Ruim", "Bom", "Médio")


# 1. Medidas de Tendência Central
media_volume <- mean(volume_colhido)
mediana_volume <- median(volume_colhido)
moda_volume <- as.numeric(names(sort(table(volume_colhido), decreasing = TRUE)[1]))

# Exibindo as medidas de tendência central
cat("Média des Toneladas colhidas:", media_volume, "\n") 
cat("Mediana das Toneladas colhidas:", mediana_volume, "\n")
cat("Moda das Toneladas colhidas:", moda_volume, "\n")

# 2. Medidas de Dispersão
maximo_volume <- max(volume_colhido)
minimo_volume <- min(volume_colhido)
variancia_volume <- var(volume_colhido)
desvio_padrao_volume <- sd(volume_colhido)
amplitude_volume <- max(volume_colhido) - min(volume_colhido)
cv_volume <- (sd(volume_colhido)/mean(volume_colhido))*100

# Exibindo as medidas de dispersão
cat("Maior colheita:", maximo_volume, "\n")
cat("Menor colheita:", minimo_volume, "\n")
cat("Variância de toneladas colhidas:", variancia_volume, "\n")
cat("Desvio Padrão:", desvio_padrao_volume, "\n")
cat("Amplitude:", amplitude_volume, "\n")
cat("Coeficiente de Variação:", cv_volume, "\n")


# 3. Medidas Separatrizes
quartis <- quantile(volume_colhido, probs =c(0.25, 0.50, 0.75))
decis <- quantile(volume_colhido, probs = seq(0.1, 0.9, by = 0.1))
centis <- quantile(volume_colhido, probs = seq(0.01, 0.99, by = 0.01))

#Exibindo as medidas separatrizes
cat("Quartis:", quartis, "\n")
cat("Decis:", decis, "\n")
cat("Centis:", centis, "\n")


# 4. Análise Gráfica
#Criando um data frame
dados_volume <- data.frame(Código_Semente = codigos, Volume_Colhido = volume_colhido)
#Criando um Gráfico de Barras
ggplot(data = dados_volume, aes(x = Código_Semente, y = Volume_Colhido)) +
  geom_bar(stat = "identity", fill = "pink") +
  geom_text(aes(label = Volume_Colhido), vjust = -0.5) + # Adiciona os valores nas barras
  labs(title = "Volume de Milho Colhido", x = "Códigos das Sementes", y = "Volume (Toneladas)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # Rotaciona os rótulos do eixo X


#Análise Gráfica Variável Qualitativa
# Criar o gráfico de barras diretamente a partir da variável qualid_graos
ggplot() + 
  geom_bar(aes(x = qualid_graos), fill = "blue") + 
  geom_text(stat = 'count', aes(x = qualid_graos, label = after_stat(count)), vjust = -0.5) + 
  labs(title = "Distribuição da Qualidade dos Grãos",
       x = "Qualidade dos Grãos",
       y = "Quantidade de Itens") +
  theme_minimal()