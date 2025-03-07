library(dplyr)
library(GGally)
library(reshape2)
library(ggplot2)
library(plotly)


songs <- read.csv("musicoset/song_features/acoustic_features.csv", sep="")
song_pop <- read.csv("musicoset/musicoset_popularity/song_pop.csv", sep="")


songs$key <- as.factor(songs$key)
songs$mode <- as.factor(songs$mode)
songs$time_signature <- as.factor(songs$time_signature)

df <- songs %>%
  inner_join(song_pop, by = "song_id")

df$is_pop <- as.factor(df$is_pop)

df <- df %>%
  mutate(epoca = case_when(
    year <= 1979 ~ "early",
    year >= 1980 & year <= 1999 ~ "mid",
    year >= 2000 ~ "late"
  )) 

df$epoca <- as.factor(df$epoca)


data <- df %>%
  select(-song_id)

cor_matrix <- cor(data %>% select(where(is.numeric)))

cor_melted <- melt(cor_matrix)

ggplot(cor_melted, aes(Var1, Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() + 
  labs(title = "Correlation Heatmap", x = "", y = "") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

discrete_vars <- c("key", "mode", "time_signature", "is_pop", "decade")

plot_variable_distribution <- function(data, discrete_vars, continuous_vars) {
  for (var in names(data)) {
    if (var %in% discrete_vars) {
      barplot(table(data[[var]]), main = paste("Barplot of", var), xlab = var, col = "lightblue")
    } else  {
      hist(data[[var]], main = paste("Histogram of", var), xlab = var, col = "lightblue")
    }
  }
}

plot_variable_distribution(data, discrete_vars, continuous_vars)

# Select continuous variables
continuous_vars <- select(data, where(is.numeric))

# Scale the data (important for PCA)
scaled_data <- scale(continuous_vars)

# Perform PCA
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

# Summary of PCA result to see the proportion of variance explained
summary(pca_result)

# Visualize the PCA components
pca_data <- data.frame(pca_result$x)  # The principal components

# Plot the first two principal components
ggplot(pca_data, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = df$epoca), alpha = 0.7) + 
  labs(title = "PCA: First vs Second Principal Component", x = "PC1", y = "PC2") +
  theme_minimal()

ggplot(pca_data, aes(x = PC1, y = PC3)) +
  geom_point(aes(color = df$epoca), alpha = 0.7) + 
  labs(title = "PCA: First vs Second Principal Component", x = "PC1", y = "PC3") +
  theme_minimal()

ggplot(pca_data, aes(x = PC2, y = PC3)) +
  geom_point(aes(color = df$epoca), alpha = 0.7) + 
  labs(title = "PCA: First vs Second Principal Component", x = "PC2", y = "PC3") +
  theme_minimal()
# Optional: Visualize the loadings (contribution of each variable to the components)
loadings <- data.frame(pca_result$rotation)

ggplot(loadings, aes(x = PC1, y = PC2, label = rownames(loadings))) +
  geom_text() +
  labs(title = "PCA Loadings") +
  theme_minimal()

# Plot the cumulative explained variance
explained_variance <- summary(pca_result)$importance[2,]  # Cumulative proportion
plot(explained_variance, xlab = "Number of Principal Components", ylab = "Cumulative Explained Variance", type = "b")
# Biplot of PCA
biplot(pca_result, scale = 0)

