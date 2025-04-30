# Carregando os pacotes necessários
library(tidyverse)
library(GGally)
library(reshape2)
library(ggcorrplot)
library(viridis)
library(RColorBrewer)  # Para paletas de cores adicionais

# Carregando os dados
dados_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
nomes_colunas <- c("Tipo", "Alcohol", "Malic_Acid", "Ash", "Alcalinity_of_Ash", "Magnesium",
                   "Total_Phenols", "Flavanoids", "Nonflavanoid_Phenols", "Proanthocyanins",
                   "Color_Intensity", "Hue", "OD280/OD315", "Proline")
vinhos <- read.csv(dados_url, header = FALSE)
colnames(vinhos) <- nomes_colunas

# Preparando dados no formato longo para algumas visualizações
dados_longos <- vinhos %>%
  gather(key = "variable", value = "value", -Tipo)

# a) Média e Desvio Padrão dos Atributos
vinhos %>%
  select(-Tipo) %>%
  summarise_all(list(media = mean, desvio = sd))

# b) Média e Desvio Padrão Agrupado por Tipo
vinhos %>%
  group_by(Tipo) %>%
  summarise_all(list(media = mean, desvio = sd))

# c) Gráfico de Densidade
ggplot(dados_longos, aes(x = value, fill = factor(Tipo))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~variable, scales = "free") +
  labs(fill = "Tipo de Vinho")
Sys.sleep(3)

# d) Boxplot por Atributo e Tipo
ggplot(dados_longos, aes(x = factor(Tipo), y = value, fill = factor(Tipo))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") +
  labs(x = "Tipo de Vinho", y = "Valor", fill = "Tipo")
Sys.sleep(3)

# e) Gráfico de Dispersão entre Atributos
ggpairs(vinhos, columns = 2:6, aes(color = factor(Tipo)))
Sys.sleep(3)

# f) Matriz de Correlação
cor_mat <- cor(vinhos[,-1])
ggcorrplot(cor_mat, lab = TRUE)
Sys.sleep(3)

# g) Gráfico de Coordenadas Paralelas
GGally::ggparcoord(data = vinhos, columns = 2:14, groupColumn = 1, scale = "std") +
  scale_color_gradientn(colors = viridis(100)) +
  theme_minimal()
Sys.sleep(3)

# h) Gráfico de Orientação por Pixel (versão original)

# Normalizar e ordenar os dados
vinhos_ordenados <- vinhos[order(vinhos$Tipo), ]
dados_matriz <- as.matrix(scale(vinhos_ordenados[, -1]))
colnames(dados_matriz) <- colnames(vinhos)[-1]

# Resetar layout padrão (sem barra lateral)
par(mfrow = c(1, 1))
par(mar = c(12, 4, 6, 2))  # margens para eixo X e título

# Gráfico principal
image(
  x = 1:ncol(dados_matriz), 
  y = 1:nrow(dados_matriz),
  z = t(dados_matriz),
  col = viridis(100),
  axes = FALSE,
  xlab = "", 
  ylab = "",
  main = "Gráfico de Orientação por Pixel dos Atributos de Vinhos"
)


# Eixo X com nomes dos atributos
axis(1, at = 1:ncol(dados_matriz), labels = colnames(dados_matriz), las = 2, cex.axis = 0.7)
mtext("Atributos", side = 1, line = 10)

# Eixo Y com os tipos
contagem_tipos <- table(vinhos_ordenados$Tipo)
tipo_indices <- cumsum(contagem_tipos)
rotulos_y <- paste0("Tipo ", names(contagem_tipos), " (n=", contagem_tipos, ")")
axis(2, at = tipo_indices - contagem_tipos/2, labels = rotulos_y, las = 1, cex.axis = 0.8)

# Linhas brancas entre os grupos
for(i in tipo_indices[-length(tipo_indices)]) {
  abline(h = i + 0.5, col = "white", lwd = 2)
}
