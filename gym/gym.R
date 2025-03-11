
library(dplyr)
library(factoextra)
library(ggplot2)
library(naniar)
library(cluster)
library(GGally)
library(ggmap)
library(maps)
library(plotly)



# Carrega o dataset
df <- read.csv("gym/gym.csv", stringsAsFactors = FALSE)



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
ggpairs(as.data.frame(df), mapping = aes(color = df$cluster)) +
  labs(title = "Pair Plot dos Dados Padronizados com Clusters")


# Supondo que os dados estejam num data frame chamado df_misto
# e que os clusters estejam armazenados na variável df_misto$cluster


summary_table <- df%>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE))

# Exibir a tabela formatada
print(summary_table)




world_map <- map_data("world")

# Definir limites do mapa com base nos dados
lat_min <- min(df_numeric$latitude) - 1
lat_max <- max(df_numeric$latitude) + 1
lon_min <- min(df_numeric$longitude) - 1
lon_max <- max(df_numeric$longitude) + 1

# Plot do mapa dentro dos limites dos dados
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               fill = "gray80", color = "white") +  # Mapa de fundo cinza
  geom_point(data = df_numeric, aes(x = longitude, y = latitude, color = factor(cluster)), 
             size = 2, alpha = 0.7) +  # Pontos coloridos conforme os clusters
  scale_color_brewer(palette = "Set1") + 
  labs(title = "Clusters de Terremotos", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  coord_cartesian(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max))




# Criar um gráfico 3D
fig <- plot_ly(df_numeric, 
               x = ~longitude, 
               y = ~latitude, 
               z = ~depth,  # Profundidade como terceira dimensão
               color = ~factor(cluster),  # Cor pelo cluster
               colors = "Dark2",  # Paleta de cores
               type = "scatter3d", 
               mode = "markers",
               marker = list(size = 5, opacity = 0.7))

# Personalizar o layout
fig <- fig %>%
  layout(scene = list(
    xaxis = list(title = "Longitude"),
    yaxis = list(title = "Latitude"),
    zaxis = list(title = "Profundidade (Depth)"),
    aspectmode = "manual", 
    aspectratio = list(x = 1, y = 1, z = 0.5)  # Ajuste da proporção dos eixos
  ),
  title = "Clusters de Terremotos em 3D")

# Mostrar o gráfico
fig


boxplot(df$mag ~ df$cluster, 
        xlab = "Cluster", 
        ylab = "Magnitude",
        main = "Boxplot da Magnitude por Cluster")
boxplot(df$depth ~ df$cluster, 
        xlab = "Cluster", 
        ylab = "Profundidade",
        main = "Boxplot da Magnitude por Cluster")
boxplot(df$year ~ df$cluster, 
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

world_map <- map_data("world")

# Definir limites do mapa com base nos dados
lat_min <- min(df_numeric$latitude) - 1
lat_max <- max(df_numeric$latitude) + 1
lon_min <- min(df_numeric$longitude) - 1
lon_max <- max(df_numeric$longitude) + 1

# Plot do mapa dentro dos limites dos dados
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               fill = "gray80", color = "white") +  # Mapa de fundo cinza
  geom_point(data = df_numeric, aes(x = longitude, y = latitude, color = factor(gmm_cluster)), 
             size = 2, alpha = 0.7) +  # Pontos coloridos conforme os clusters
  scale_color_brewer(palette = "Set1") + 
  labs(title = "Clusters de Terremotos", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  coord_cartesian(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max))



