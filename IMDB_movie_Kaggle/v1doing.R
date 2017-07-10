### read data and thinking

setwd('c://Kaggle_funny/IMDB_movie_Kaggle')
Sys.setlocale(category = "LC_ALL", locale = "")

## read data
movie_raw_data <- read.csv('data/movie_metadata.csv')
director_class <- unique(movie_raw_data$director_name)

# Total director is 2399.
director_name <- c()
numOfmovie <- c()
meanOfGross <- c()
meanOfimdb <- c()

for(i in 1:length(director_class)){
  director_i <- subset(movie_raw_data, movie_raw_data$director_name==director_class[[i]])
  director_name <- c(director_name, as.character(director_class[[i]]))
  numOfmovie <- c(numOfmovie, length(director_i$movie_title))
  meanOfGross <- c(meanOfGross, mean(director_i$gross))
  meanOfimdb <- c(meanOfimdb, mean(director_i$imdb_score))
}
data_director <- data.frame(director_name, numOfmovie, meanOfGross, meanOfimdb)

hist_director <- plot_ly(data = data_director, x = data_director$meanOfimdb, type = "histogram")

# > 5 movie director
over5movie_Director <- subset(data_director, data_director$numOfmovie >=5)
hist_director_over5movie <- plot_ly(data = over5movie_Director, x = over5movie_Director$meanOfimdb, type = "histogram")


library(plotly)
yearTOimdbscore <- plot_ly(data = movie_raw_data, 
                           x = movie_raw_data$title_year, 
                           y = movie_raw_data$imdb_score, 
                           type = "scatter",
                           mode="markers")
yearTOimdbscore

grossTOimdbscore <- plot_ly(data = movie_raw_data, 
                           x = movie_raw_data$gross, 
                           y = movie_raw_data$imdb_score, 
                           type = "scatter",
                           mode="markers")
grossTOimdbscore

