
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")  # Install Bioconductor manager if not present

BiocManager::install("rhdf5")  # Install rhdf5 package

library(BiocManager)

library(dplyr)
library(rhdf5)

# 2. Define the root directory where your dataset is stored.
#    Update the path below to match your dataset folder location.
root_dir <- "."  # e.g., "/Users/username/Documents/10k_song_dataset"

# 3. List all .h5 files recursively from the root directory.
file_list <- list.files(root_dir, pattern = "\\.h5$", recursive = TRUE, full.names = TRUE)
cat("Total .h5 files found:", length(file_list), "\n")

# 4. Define a function to read the song data from an .h5 file.
read_song_data <- function(file_path) {
  # Use tryCatch to avoid errors if a file doesn't have the expected structure
  artist <- tryCatch({
    h5read(file_path, "metadata/songs/artist_name")
  }, error = function(e) NA)
  
  title <- tryCatch({
    h5read(file_path, "metadata/songs/title")
  }, error = function(e) NA)
  
  # Return the information as a data frame row
  data.frame(file = file_path, artist = artist, title = title, stringsAsFactors = FALSE)
}

# 5. Process each file and combine the results into a single data frame.
song_df <- do.call(rbind, lapply(file_list, read_song_data))

# 6. Save the resulting data frame to a CSV file (optional).
output_csv <- "song_data.csv"
write.csv(song_df, output_csv, row.names = FALSE)
cat("Data has been saved to", output_csv, "\n")

# 7. Preview the first few rows of the data frame.
print(head(song_df))
h5ls(song_df$file[1])$analysis

read_all_song_data <- function(file_path) {
  if (!file.exists(file_path)) {
    return(NULL)  # Skip missing files
  }
  
  # Read compound datasets from metadata, analysis, and musicbrainz
  metadata <- tryCatch(h5read(file_path, "metadata/songs"), error = function(e) NULL)
  analysis <- tryCatch(h5read(file_path, "analysis/songs"), error = function(e) NULL)
  musicbrainz <- tryCatch(h5read(file_path, "musicbrainz/songs"), error = function(e) NULL)
  
  # Combine data from all groups into a single row
  all_data <- c(metadata, analysis, musicbrainz)
  
  # Convert to data frame with one row
  df <- as.data.frame(all_data, stringsAsFactors = FALSE)
  
  # Add the file path for reference
  df$file <- file_path
  
  return(df)
}

# Process each file and combine the results into a single data frame
all_song_data <- do.call(rbind, lapply(file_list, read_all_song_data))
# 