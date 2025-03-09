
library(dplyr)
library(factoextra)
library(ggplot2)
library(naniar)
library(cluster)
library(GGally)

# Carrega o dataset
df <- read.csv("Sismos/sismos_sul.csv", stringsAsFactors = FALSE)

# Seleciona as colunas numéricas relevantes para PCA e clustering
df_numeric <- df %>%
  select(latitude, longitude, depth, mag, nst, gap, rms)

# Visualiza os valores ausentes
gg_miss_var(df_numeric)

# Remove linhas com valores ausentes
df_numeric <- na.omit(df_numeric)

# Padroniza os dados (importante para PCA e clustering)
df_scaled <- scale(df_numeric)

# Realiza a Análise de Componentes Principais (PCA)
pca <- prcomp(df_scaled, center = TRUE, scale. = TRUE)
summary(pca)  # Visualiza a variância explicada por cada componente

# Gráfico de variância explicada (Scree Plot)
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50)) +
  labs(title = "Scree Plot - Variância Explicada pela PCA")

# Determina o número ótimo de clusters utilizando o método do "Elbow"
fviz_nbclust(df_scaled, kmeans, method = "wss") +
  labs(title = "Método Elbow para Definir o Número Ótimo de Clusters")

# Define o número de clusters (exemplo: 3 clusters)
set.seed(123)  # Para reprodutibilidade
k <- 4
kmeans_res <- kmeans(df_scaled, centers = k, nstart = 25)

# Adiciona a informação de cluster ao dataset
df_numeric$cluster <- as.factor(kmeans_res$cluster)

# Cria um data frame com os scores dos 2 primeiros componentes principais
pca_scores <- as.data.frame(pca$x[, 1:2])
pca_scores$cluster <- df_numeric$cluster

# Plota os clusters sobre os 2 primeiros componentes da PCA
ggplot(pca_scores, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "PCA e Clustering dos Dados de Terremotos",
       x = "Componente Principal 1",
       y = "Componente Principal 2")

# Biplot da PCA com clusters (inclui vetores das variáveis e elipses de confiança)
fviz_pca_biplot(pca, 
                label = "var", 
                habillage = df_numeric$cluster, 
                addEllipses = TRUE, 
                ellipse.level = 0.95,
                palette = "jco") +
  labs(title = "Biplot da PCA com Clusters")

# Gráfico das contribuições das variáveis para os componentes principais
fviz_pca_var(pca, 
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) +
  labs(title = "Contribuição das Variáveis na PCA")

# Plot Silhouette para avaliar a qualidade do clustering
sil <- silhouette(kmeans_res$cluster, dist(df_scaled))
fviz_silhouette(sil) +
  labs(title = "Silhouette Plot dos Clusters")

# Pair Plot dos dados padronizados coloridos pelo cluster
ggpairs(as.data.frame(df_scaled), mapping = aes(color = df_numeric$cluster)) +
  labs(title = "Pair Plot dos Dados Padronizados com Clusters")
