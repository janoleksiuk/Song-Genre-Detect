# script for data classification
# classifying songs to genre
# Jan Oleksiuk, 2025

# !! source data_preparation.R script firstly !!

# importing caret library
library(caret)

# reproducible model
set.seed(100)

# Spltting data 
train_idx <- createDataPartition(data_processed$playlist_genre, p = 0.7, list = FALSE)
train_data <- data_processed[train_idx,]
remaining_data <- data_processed[-train_idx,]

train_idx <- createDataPartition(data_processed$playlist_genre, p = 0.5, list = FALSE)
train_data <- data_processed[train_idx,]
remaining_data <- data_processed[-train_idx,]

# Model - SVM
model <- train(playlist_genre ~ ., data = train_data,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none"),
               tuneGrid = data.frame(degree=3,scale=1,C=1)
  )

# applying model
model.training <-predict(model, train_data) # Apply model to make prediction on Training set
model.testing <-predict(model, test_data) # Apply model to make prediction on Testing set

# model performance (Displays confusion matrix and statistics)
model.training.confusion <- confusionMatrix(
  data = factor(model.training, levels = sort(unique(c(as.character(model.training), as.character(train_data$playlist_genre))))),
  reference = factor(train_data$playlist_genre, levels = sort(unique(c(as.character(model.training), as.character(train_data$playlist_genre)))))
)

model.testing.confusion <- confusionMatrix(
  data = factor(model.testing, levels = sort(unique(c(as.character(model.testing), as.character(test_data$playlist_genre))))),
  reference = factor(test_data$playlist_genre, levels = sort(unique(c(as.character(model.testing), as.character(test_data$playlist_genre)))))
)

print(model.training.confusion)
print(model.testing.confusion)