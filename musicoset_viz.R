library(dplyr)
library(GGally)
library(reshape2)
library(ggplot2)
library(plotly)


songs <- read.csv("musicoset/song_features/acoustic_features.csv", sep="")
song_pop <- read.csv("musicoset/musicoset_popularity/song_pop.csv", sep="")
songs_meta <- read.table("musicoset/musicoset_metadata/songs.csv", sep = "\t", header = TRUE, fill = TRUE)




# songs$key <- as.factor(songs$key)
songs$mode <- as.factor(songs$mode)
songs$time_signature <- as.factor(songs$time_signature)

df <- songs %>%
  inner_join(song_pop, by = "song_id")

df <- df %>%
  inner_join(songs_meta, by = "song_id")


df$is_pop <- as.factor(df$is_pop)

df <- df %>%
  mutate(epoca = case_when(
    year <= 1989 ~ "early",
    year >= 1990 ~ "late"
  )) 

df$epoca <- as.factor(df$epoca)

df$popularity <- as.numeric(df$popularity)

df <- df %>% 
  filter(song_type %in% c("Collaboration", "Solo"))

df <- df %>% 
  filter(explicit %in% c("True", "False"))

df$explicit <- as.factor(df$explicit)



df$song_type <- as.factor(df$song_type)

df <- na.omit(df)


data <- df %>%
  select(-song_id, -year, -song_name, -billboard, -artists, -year_end_score, -popularity)

cor_matrix <- cor(data %>% select(where(is.numeric)))

cor_melted <- reshape2::melt(cor_matrix)

ggplot(cor_melted, aes(Var1, Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() + 
  labs(title = "Correlation Heatmap", x = "", y = "") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

discrete_vars <- c("key", "mode", "time_signature", "is_pop", "decade", "epoca", "song_type", "explicit")

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

