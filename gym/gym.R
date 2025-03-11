
library(dplyr)
library(factoextra)
library(ggplot2)
library(naniar)
library(cluster)
library(GGally)
library(ggmap)
library(maps)
library(kernlab)
library(patchwork)


# Carrega o dataset
df <- read.csv("gym/gym.csv", stringsAsFactors = FALSE)
df$Workout_Type <- as.factor(df$Workout_Type)


df_numeric <- df %>% select(where(is.numeric))
gg_miss_var(df_numeric)

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
df$cluster <- as.factor(kmeans_res$cluster)
# Cria um data frame com os scores dos 2 primeiros componentes principais
pca_scores <- as.data.frame(pca$x[, 1:3])
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

fig <- plot_ly(pca_scores, 
               x = ~PC1, y = ~PC2, z = ~PC3, 
               color = ~cluster, 
               colors = c("red", "blue", "green"),  # Ajuste as cores conforme necessário
               type = "scatter3d", 
               mode = "markers",
               marker = list(size = 4, opacity = 0.8))

# Ajustar layout
fig <- fig %>% layout(title = "PCA e Clustering Spectral em 3D",
                      scene = list(xaxis = list(title = "PC1"),
                                   yaxis = list(title = "PC2"),
                                   zaxis = list(title = "PC3")))

# Exibir gráfico interativo
fig




dist_matrix <- dist(df_scaled, method = "euclidean")

hc <- hclust(dist_matrix, method = "ward.D2")
plot(hc, labels = df$Workout_Type, main = "Dendrograma - Cluster Hierárquico")

rect.hclust(hc, k = k, border = 2:4) 

hc_clusters <- cutree(hc, k = 4)

# Adicionar os clusters aos dados
df$hc_Cluster <- as.factor(hc_clusters)
pca_scores$hc_Cluster <- as.factor(hc_clusters)

ggplot(pca_scores, aes(x = PC1, y = PC2, color = hc_Cluster)) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "PCA e Clustering dos Dados de Terremotos",
       x = "Componente Principal 1",
       y = "Componente Principal 2")


cxv # Pair Plot dos dados padronizados coloridos pelo cluster
ggpairs(as.data.frame(df), mapping = aes(color = df$cluster)) +
  labs(title = "Pair Plot dos Dados Padronizados com Clusters")


# Supondo que os dados estejam num data frame chamado df_misto
# e que os clusters estejam armazenados na variável df_misto$cluster


summary_table <- df%>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE))

# Exibir a tabela formatada
print(summary_table)






boxplot(df$Weight..kg. ~ df$cluster, 
        xlab = "Cluster", 
        ylab = "Magnitude",
        main = "Boxplot da Magnitude por Cluster")
boxplot(df$Fat_Percentage ~ df$cluster, 
        xlab = "Cluster", 
        ylab = "Profundidade",
        main = "Boxplot da Magnitude por Cluster")
boxplot(df$Calories_Burned ~ df$cluster, 
        xlab = "Cluster", 
        ylab = "Ano",
        main = "Boxplot da Magnitude por Cluster")


library(mclust)

# Ajustando o modelo de misturas GMM com os dados padronizados
gmm_model <- Mclust(df_scaled, G = 3)  # O Mclust seleciona automaticamente o número de componentes

# Visualizando os resultados
summary(gmm_model)  # Exibe um resumo do modelo ajustado

# Obtendo os clusters preditos
df_numeric$gmm_cluster <- as.factor(gmm_model$classification)
df$gmm_cluster <- as.factor(gmm_model$classification)


# Plotando os clusters no espaço PCA
pca_scores$gmm_cluster <- df_numeric$gmm_cluster

ggplot(pca_scores, aes(x = PC1, y = PC2, color = gmm_cluster)) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "PCA e Clustering com Modelo de Misturas (GMM)",
       x = "Componente Principal 1",
       y = "Componente Principal 2")

# Biplot da PCA com GMM
fviz_pca_biplot(pca, 
                label = "var", 
                habillage = df_numeric$gmm_cluster, 
                addEllipses = TRUE, 
                ellipse.level = 0.95,
                palette = "jco") +
  labs(title = "Biplot da PCA com Clusters de Misturas (GMM)")

# Plot do Silhouette para avaliar a qualidade do GMM
sil_gmm <- silhouette(gmm_model$classification, dist(df_scaled))
fviz_silhouette(sil_gmm) +
  labs(title = "Silhouette Plot dos Clusters - GMM")

#SPECTRAL

k <- 4
modelo <- specc(df_scaled, centers = k)
modelo
df_numeric$spectral_cluster <- as.factor(modelo)
df$spectral_cluster <- as.factor(modelo)
pca_scores$spectral_cluster <- df_numeric$spectral_cluster

ggplot(pca_scores, aes(x = PC1, y = PC2, color = spectral_cluster)) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "PCA e Clustering com Spectral",
       x = "Componente Principal 1",
       y = "Componente Principal 2")

ggplot(pca_scores, aes(x = PC1, y = PC3, color = spectral_cluster)) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "PCA e Clustering com Spectral",
       x = "Componente Principal 1",
       y = "Componente Principal 3")

fig <- plot_ly(pca_scores, 
               x = ~PC1, y = ~PC2, z = ~PC3, 
               color = ~spectral_cluster, 
               colors = c("red", "blue", "green"),  # Ajuste as cores conforme necessário
               type = "scatter3d", 
               mode = "markers",
               marker = list(size = 4, opacity = 0.8))

# Ajustar layout
fig <- fig %>% layout(title = "PCA e Clustering Spectral em 3D",
                      scene = list(xaxis = list(title = "PC1"),
                                   yaxis = list(title = "PC2"),
                                   zaxis = list(title = "PC3")))

# Exibir gráfico interativo
fig

# Biplot da PCA com clusters (inclui vetores das variáveis e elipses de confiança)
fviz_pca_biplot(pca, 
                label = "var", 
                habillage = df_numeric$spectral_cluster, 
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
sil <- silhouette(kmeans_res$spectral_cluster, dist(df_scaled))
fviz_silhouette(sil) +
  labs(title = "Silhouette Plot dos Clusters")


for (var in colnames(df_numeric)) {
  
  # Criar boxplot
  boxplot_plt <- ggplot(df_numeric, aes(x = spectral_cluster, y = .data[[var]], fill = spectral_cluster)) +
    geom_boxplot(alpha = 0.7) +
    theme_minimal() +
    labs(title = paste("Boxplot de", var), x = "Cluster", y = var)
  print(boxplot_plt)
}
ggpairs(as.data.frame(df), mapping = aes(color = df$spectral_cluster)) +
  labs(title = "Pair Plot dos Dados Padronizados com Clusters")

