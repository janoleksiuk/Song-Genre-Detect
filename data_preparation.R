# script for data preparation - data regarding spotify songs classified as popular and unpopular 
# Jan Oleksiuk, 2025
# data downloaded from: https://www.kaggle.com/datasets/solomonameh/spotify-music-dataset?resource=download (18.05.2025)

# data preparation parameters

# outliers handling method: "MEAN-S", "TUKEY"
# sigma coefficient for "MEAN-S": k_sigma (preferable 2, 2.5, 3)
outlier_method = "NONE" #now it is working bad, none method is applied
k_sigma = 2.5

norm_type = "Z-SCORE"

# loading data
data_raw <- read.csv("data/spotify_data.csv")

# deleting unnecessary data regarding building a model 
# neglected variables: track_artist, track_href, uri, track_album_name, playlist_name,
#                      analysis_url, track_id, track_name, track_album_release_date,
#                      track_album_id, id, playlist_subgenre, type, playlist_id,
#                      time_signature (leaving it leads to errors)

cols_to_remove <- c("track_artist", "track_href", "uri",
                    "track_album_name", "playlist_name", "analysis_url",
                    "track_id", "track_id", "track_name",
                    "track_album_release_date", "track_album_id", "id",
                    "playlist_subgenre", "type", "playlist_id", "time_signature")

data_processed <- data_raw[ , !(names(data_raw) %in% cols_to_remove)]

# handling missing values
# there is no missing data within the table

# handling outliers
if (outlier_method == "MEAN-S")
{
  num_cols <- sapply(data_processed, is.numeric)
  
  rows_to_keep <- rep(TRUE, nrow(data_processed))
  
  for (col_name in names(data_processed)[num_cols]) {
    x <- data_processed[[col_name]]
    mean_x <- mean(x, na.rm = TRUE)
    sd_x <- sd(x, na.rm = TRUE)
    lower_bound <- mean_x - k_sigma * sd_x
    upper_bound <- mean_x + k_sigma * sd_x
    rows_to_keep <- rows_to_keep & (x >= lower_bound & x <= upper_bound)
  
    data_processed <- data_processed[rows_to_keep, ]
  }
} else if (outlier_method == "TUKEY")
{
  num_cols <- sapply(data_processed, is.numeric)
  
  rows_to_keep <- rep(TRUE, nrow(data_processed))
  
  for (col_name in names(data_processed)[num_cols]) {
    x <- data_processed[[col_name]]
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    rows_to_keep <- rows_to_keep & (x >= lower_bound & x <= upper_bound)
  
    data_processed <- data_processed[rows_to_keep, ]
  }
  
} else 
{
  cat("No outliers handling applied.")
}


# normalization of data
if (norm_type == "Z-SCORE")
{
  num_cols <- sapply(data_processed, is.numeric)
  data_processed[num_cols] <- lapply(data_processed[num_cols], function(x) {
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  })
} else if (norm_type == "MIN-MAX")
{
  num_cols <- sapply(data_processed, is.numeric)
  data_processed[num_cols] <- lapply(data_processed[num_cols], function(x) {
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  })
} else
{
  cat("No normalization applied.")
}









