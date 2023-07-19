# A system that recommends Anime to users.
# Item Based Collaborative Filter (IBCF) recommendation system

library(data.table)
library(recommenderlab)
library(reshape2)
library(ggplot2)



# Retrieve and display data
setwd("/home/osatapathy/CollegeStuff/Sem_3/R_Project/Our_Project/AnimeDataset") #setting working directory
anime_data <- read.csv("anime.csv", stringsAsFactors = FALSE)
rating_data <- read.csv("rating.csv")
str(anime_data)

# Overview the summary
summary(anime_data)
head(anime_data)
summary(rating_data)
head(rating_data)


# Data pre-processing
# Creating a one-hot encoding to create a matrix that comprises of corresponding genres for each of the anime
anime_genre <- as.data.frame(anime_data$genre, stringsAsFactors = FALSE)
# anime_genre
library(data.table)
anime_genre2 <- as.data.frame(
  tstrsplit(anime_genre[, 1], ", ", #separating the genres
            type.convert = TRUE
  ),
  stringsAsFactors = FALSE
)


colnames(anime_genre2) <- c(1:10)
# head(anime_genre2)


list_genre <- c("Drama", "Action", "Sci-Fi", "Comedy", "Adventure", "Fantasy", 
                "Mystery", "Psychological", "Ecchi", "Josei", "Military", "Romance", 
                "Demons", "Dementia", "Music", "Game", "Cars", "Mecha", "Horror", 
                "School", "Historical", "Kids", "Shounen", "Shoujo", "Magic", 
                "Harem", "Martial Arts", "Sports", "Slice of Life", "Seinen", NA, "Parody", 
                "Police", "Thriller", "Supernatural", "Samurai", "Super Power", "Vampire", 
                "Space", "Hentai", "Yaoi", "Yuri")


genre_mat1 <- matrix(0, 12295, 42) # used for one hot encoding
genre_mat1[1,] <- list_genre
head(genre_mat1)
colnames(genre_mat1) <- list_genre
head(genre_mat1)

for(row in 1 : nrow(anime_genre2) ){# creation of one hot encoded matrix
  for(col in 1 : ncol(anime_genre2)){
    gen_col <- which(genre_mat1[1, ] == (anime_genre2[row, col])) # Genre match found
    genre_mat1[row + 1, gen_col] <- 1 # changing 0 -> 1 in the one-hot encoding
  }
}


genre_mat2 <- as.data.frame(genre_mat1[-1, ], stringsAsFactors = FALSE) # remove first row, which was the genre list
head(genre_mat2)
for (col in 1:ncol(genre_mat2)) {
  genre_mat2[, col] <- as.integer(genre_mat2[, col]) # convert from characters to integers
}
head(genre_mat2)
str(genre_mat2)


# Creating a ‘search matrix’ - searching anime by specifying the genre

SearchMatrix <- cbind(anime_data[, 1:2], genre_mat2[])
head(SearchMatrix)

ratingMatrix <- dcast(rating_data, user_id ~ anime_id, value.var = "rating", na.rm = FALSE)
ratingMatrix <- as.matrix(ratingMatrix[, -1]) # remove userIds


# Convert rating matrix into a recommenderlab sparse matrix
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
ratingMatrix

# Overview some important parameters for building recommendation systems for anime
recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)
lapply(recommendation_model, "[[", "description")
# Implementing a single model in the R project – Item Based Collaborative Filtering (IBCF)
recommendation_model$IBCF_realRatingMatrix$parameters
similarity_mat <- similarity(ratingMatrix[1:6, ],
                             method = "cosine",
                             which = "users"
)
as.matrix(similarity_mat)
image(as.matrix(similarity_mat), main = "User's Similarities")

# Portraying the similarity  between the different animes
anime_similarity <- similarity(ratingMatrix[, 1:6],
                               method =
                                 "cosine", which = "items"
)
as.matrix(anime_similarity)
image(as.matrix(anime_similarity), main = "Anime similarity")


rating_values <- as.vector(ratingMatrix@data)
rating_values # contains a list of all the ratings taken column wise, (vertically down)
unique(rating_values) # extracting unique ratings
Table_of_Ratings <- table(rating_values) # creating a count of anime ratings
Table_of_Ratings # tells us which rating has how many counts



# Most viewed animes visualization
library(ggplot2)
anime_views <- colCounts(ratingMatrix) # count views for each anime from colCounts

table_views <- data.frame(
  animeID = names(anime_views),
  views = anime_views
) # create dataframe of views

table_views <- table_views[order(table_views$views,
                                 decreasing = TRUE
), ] # sort by number of views in decreasing order

table_views$name <- NA #added a third column for names of the anime
dim(table_views)

for (index in 1:8347) {
  table_views[index, 3] <- as.character(subset(
    anime_data,
    anime_data$anime_id == table_views[index, 1]
  )$name)
}


table_views[1:6, ]

#Visualize a bar plot for the total number of views of the top Animes
ggplot(table_views[1:6, ], aes(x = name, y = views)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = views), vjust = -0.3, size = 3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Total Views of the Top Animes")


#Heat-map of Anime Ratings

image(ratingMatrix[1:25, 1:25], axes = FALSE, main = "Heatmap of the first 25 rows and 25 columns")
# the above heat map shows the correlation of userId and animes, like most dark when the user gave that anime 10f stars


#Data Preparation
anime_ratings <- ratingMatrix[
  rowCounts(ratingMatrix) > 50,
  colCounts(ratingMatrix) > 50
]

anime_ratings # this stores only those animes who got more that 50 ratings by those users who have given more than 50 ratings


# describing matrix of relevant users
minimum_anime <- quantile(rowCounts(anime_ratings), 0.98) # gives a threshold value for a user to be considered relevant by calculating the 98% of all the animes
minimum_users <- quantile(colCounts(anime_ratings), 0.98) # gives a threshold value for anime to be considered relevant
image(
  anime_ratings[
    rowCounts(anime_ratings) > minimum_anime, # creates a subset of anime_ratings by only including the relevant anime and users
    colCounts(anime_ratings) > minimum_users
  ],
  main = "Heatmap of the top users and anime"
)
# Visualizing the distribution of the average ratings per user
average_ratings <- rowMeans(anime_ratings)
average_ratings <- round(average_ratings,1)#rounding off the rating upto one decimal places
head(average_ratings)
qplot(average_ratings, fill = I("darkgreen"), col = I("yellow")) + # the infamous bell curve graph
  ggtitle("Distribution of the average rating per user")


# Data Normalization -> basically means to change the range of ratings from 1 to 5 to 0 to 1 (all four inclusive)
normalized_ratings <- normalize(anime_ratings)
sum(rowMeans(normalized_ratings) > 0.00001) # calculates the number of users who have given a rating above 0.000 (irl 1 star)
image(
  normalized_ratings[
    rowCounts(normalized_ratings) > minimum_anime,
    colCounts(normalized_ratings) > minimum_users
  ],
  main = "Normalized Ratings of the Top Users"
)
# Data Binarization
binary_minimum_anime <- quantile(rowCounts(anime_ratings), 0.95)
binary_minimum_users <- quantile(colCounts(anime_ratings), 0.95)
good_rated_animes <- binarize(anime_ratings, minRating = 3) # makes the ratings below 3 to zero and equal or above 3 to 1
image(
  good_rated_animes[
    rowCounts(anime_ratings) > binary_minimum_anime,
    colCounts(anime_ratings) > binary_minimum_users
  ],
  main = "Heatmap of the top users and animes"
)

# Collaborative Filtering System
# Splitting the dataset into 80% training set and 20% test set
sampled_data <- sample(
  x = c(TRUE, FALSE),
  size = nrow(anime_ratings),
  replace = TRUE,
  prob = c(0.8, 0.2)
) # just makes 80% of the rows to be training data and the other 20% to be test data
training_data <- anime_ratings[sampled_data, ]
testing_data <- anime_ratings[!sampled_data, ]
# Building the Recommendation System
recommendation_system <- recommenderRegistry$get_entries(dataType = "realRatingMatrix") # making an object of the recommender system 
recommendation_system$IBCF_realRatingMatrix$parameters # listing all the parameters of IBCF for realRatingMatrix object
recommen_model <- Recommender(
  data = training_data, # sending the training data
  method = "IBCF", # setting the algorithm to IBCF
  parameter = list(k = 30)
) # setting k=30, to limit the no. of neighbours considered during recommendation
recommen_model
class(recommen_model)
# Exploring the data science recommendation system model
model_info <- getModel(recommen_model) # getting info of the model
class(model_info$sim) # checking model's class and similarity matrix created during training
dim(model_info$sim) # checking dimension of the similarity matrix which compares similarity of different users
top_items <- 100
image(model_info$sim[1:top_items, 1:top_items],
      main = "Heatmap of the first rows and columns"
)
# Visualize sum of rows and columns with the similarity of the objects above 0
sum_rows <- rowSums(model_info$sim > 0) # calculates for each row, which other columns have a postivie similarity with it
table(sum_rows)


sum_cols <- colSums(model_info$sim > 0) # does the same for column
qplot(sum_cols, fill = I("darkgreen"), col = I("yellow")) + ggtitle("Distribution of the column count") # the x-axis is for the number of similar people and the y-axis is for the number of people having that no. of similar people
# eg. most people have about 25 similar matches

# the number of items to recommend to each user
top_recommendations <- 10
predicted_recommendations <- predict(
  object = recommen_model,
  newdata = testing_data,
  n = top_recommendations
)
head(predicted_recommendations)

# recommendation for the first user
user1 <- predicted_recommendations@items[[1]] # this probably contains the index number of the anime recommended to user 1
user1
anime_user1 <- predicted_recommendations@itemLabels[user1] # this would contain the animeId recommended to user 1
anime_user1
anime_user2 <- anime_user1
anime_user2
for (index in 1:10) {
  anime_user2[index] <- as.character(subset(
    anime_data,
    anime_data$anime_id == anime_user1[index]
  )$name)
}
(anime_user2)
