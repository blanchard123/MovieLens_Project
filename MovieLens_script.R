
### Adam Blanchard
### MovieLens Capstone Project 
### HarvardX: PH125.9x - Capstone Project
### https://github.com/blanchard123/MovieLens_Project

##########################################################
# MovieLens Recommendation Systems Project Code 
#########################################################


##########################################################
# Create edx set and validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

########################################################################
# Completed creating edx set, validation set (final hold-out test set)
########################################################################

# Save edx and validation datasets
save(edx, file="edx.RData")
save(validation, file = "validation.RData")

# Load additional libraries 

if(!require(caretEnsemble)) install.packages("caretEnsemble", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(markdown)) install.packages("markdown", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")

library(caretEnsemble)
library(gam)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(knitr)
library(kableExtra)
library(lubridate)
library(markdown)
library(matrixStats)
library(randomForest)
library(Rborist)
library(scales)
library(stringr)
library(dplyr)


#############################################
########## BASIC DATA EXPLORATION ##########

# basic identification of data and variables 
str(edx, strict.width="cut")
str(validation, strict.width="cut")

as_tibble(edx) %>% slice(1:10) %>% 
  kable(caption = "Examination of the edx Data Structure", align = "c") %>%
  kable_styling(font_size = 10, latex_options = "scale_down")

# number of rows and columns
dim(edx)

# basic descriptive information about the variables 
summary(edx)

# number of different users and movies
edx %>% summarize(n_users = n_distinct(userId),
                  n_movies = n_distinct(movieId)) %>% 
  kable(col.names = c("Users", "Movies"), align = "c", 
        caption = "Distinct Users and Movies") %>%
  kable_styling(font_size = 10, latex_options = "scale_down")

# table of number of each rating in descending order 
edx %>% group_by(rating) %>% 
  summarize(number = n()) %>% 
  arrange(desc(number)) %>%
  kable(align = "c", caption = "Frequency of Most Common Ratings") %>%
  kable_styling(font_size = 10, latex_options = "scale_down")

# plot of frequency of ratings
edx %>%
  ggplot(aes(rating)) +
  geom_bar(color = "Black", fill = "#00abff") +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  xlab("Rating") +
  ylab("Frequency of Ratings") +
  theme_light() +
  ggtitle("Figure 1: Frequency Distribution of Ratings") +
  theme(axis.title.x = element_text(vjust = -3)) +
  theme(axis.title.y = element_text(angle = 90, vjust = 3)) +
  theme(plot.title = element_text(size = 15, vjust = 5, hjust = 0.5)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))



########## Exploration of movie effects ########## 

# movies with the greatest number of ratings
edx %>% group_by(movieId, title) %>% 
  summarise(number = n()) %>%
  arrange(desc(number)) %>%
  head(10) %>%
  kable(caption = "Top 10 Most Rated Movies") %>%
  kable_styling(font_size = 10, latex_options = "scale_down")

# movies with the fewest number of ratings
edx %>% group_by(movieId, title) %>% 
  summarise(number = n()) %>%
  arrange(number) %>%
  head(10) %>%
  kable(caption = "Top 10 Most Rated Movies") %>%
  kable_styling(font_size = 10, latex_options = "scale_down")

# plot of frequency of movie ratings
edx %>%
  group_by(movieId) %>%
  summarize(number = n()) %>% 
  ggplot(aes(number)) +
  geom_histogram(bins = 50, color = "black", fill = "#00abff") +
  scale_x_log10(labels = comma) +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 10)) +
  xlab("Frequency of Ratings (log scale)") +
  ylab("Frequency of Movies") +
  theme_light() +
  ggtitle("Figure 2: Frequency of Movie Ratings (log scale)") +
  theme(axis.title.x = element_text(vjust = -3)) +
  theme(axis.title.y = element_text(angle = 90, vjust = 3)) +
  theme(plot.title = element_text(size = 15, vjust = 5, hjust = 0.5)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

# plot of frequency of average movie rating
edx %>%
  group_by(movieId) %>%
  filter(n()>=100) %>%
  summarize(b_i = mean(rating)) %>% 
  ggplot(aes(b_i)) +
  geom_histogram(bins = 50, color = "Black", fill = "#00abff") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 10)) +
  xlab("Average Rating") +
  ylab("Frequency of Movies") +
  theme_light() +
  ggtitle("Figure 3: Frequency of Average Movie Rating") +
  theme(axis.title.x = element_text(vjust = -3)) +
  theme(axis.title.y = element_text(angle = 90, vjust = 3)) +
  theme(plot.title = element_text(size = 15, vjust = 5, hjust = 0.5)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))



########## Exploration of user effects ########## 

# users with the greatest number of ratings
edx %>% group_by(userId) %>% 
  summarise(number = n()) %>%
  arrange(desc(number)) %>%
  head(10) %>%
  kable(caption = "Top 10 Users with the Most Ratings") %>%
  kable_styling(font_size = 10, latex_options = "scale_down")

# users with the fewest number of ratings
edx %>% group_by(userId) %>% 
  summarise(number = n()) %>%
  arrange(number) %>%
  head(10) %>%
  kable(caption = "Top 10 Users with the Least Ratings") %>%
  kable_styling(font_size = 10, latex_options = "scale_down")

# plot of the frequency of user ratings
edx %>%
  group_by(userId) %>%
  summarize(number = n()) %>% 
  ggplot(aes(number)) +
  geom_histogram(bins = 50, color = "black", fill = "#00abff") +
  scale_x_log10(labels = comma) +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 10)) +
  xlab("Frequency of Ratings (log scale)") +
  ylab("Frequency of Users") +
  theme_light() +
  ggtitle("Figure 4: Frequency of User Ratings (log scale)") +
  theme(axis.title.x = element_text(vjust = -3)) +
  theme(axis.title.y = element_text(angle = 90, vjust = 3)) +
  theme(plot.title = element_text(size = 15, vjust = 5, hjust = 0.5)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

# plot of the frequency of average user rating
edx %>%
  group_by(userId) %>%
  filter(n()>=100) %>%
  summarize(b_u = mean(rating)) %>% 
  ggplot(aes(b_u)) +
  geom_histogram(bins = 50, color = "Black", fill = "#00abff") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 10)) +
  xlab("Average Rating") +
  ylab("Frequency of Users") +
  theme_light() +
  ggtitle("Figure 5: Frequency of Average User Rating") +
  theme(axis.title.x = element_text(vjust = -3)) +
  theme(axis.title.y = element_text(angle = 90, vjust = 3)) +
  theme(plot.title = element_text(size = 15, vjust = 5, hjust = 0.5)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))



########## Exploration of time effects ########## 

# transform timestamp to date of rating 
edx_dates <- edx %>% mutate(date_rated = as_datetime(timestamp))

# plot of average rating by week of rating
plot1<- edx_dates %>% mutate(week_rated = round_date(date_rated, unit = "week")) %>%
  group_by(week_rated) %>%
  summarize(average = mean(rating)) %>%
  ggplot(aes(x = week_rated, y = average)) +
  geom_point(alpha = 0.75) +
  geom_smooth(method = "loess") +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  xlab("Date of Rating Rounded to Week") +
  ylab("Average Rating") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -3)) +
  theme(axis.title.y = element_text(angle = 90, vjust = 3)) +
  theme(plot.title = element_text(size = 15, vjust = 5, hjust = 0.5)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

# plot of average rating by month of rating
plot2 <- edx_dates %>% mutate(month_rated = round_date(date_rated, unit = "month")) %>%
  group_by(month_rated) %>%
  summarize(average = mean(rating)) %>%
  ggplot(aes(x = month_rated, y = average)) +
  geom_point(alpha = 0.75) +
  geom_smooth(method = "loess") +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  xlab("Date of Rating Rounded to Month") +
  ylab("Average Rating") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -3)) +
  theme(axis.title.y = element_text(angle = 90, vjust = 3)) +
  theme(plot.title = element_text(size = 15, vjust = 5, hjust = 0.5)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

# plot of average rating by year of rating
plot3 <- edx_dates %>% mutate(year_rated = round_date(date_rated, unit = "year")) %>%
  group_by(year_rated) %>%
  summarize(average = mean(rating)) %>%
  ggplot(aes(x = year_rated, y = average)) +
  geom_point(alpha = 0.75) +
  geom_smooth(method = "loess") +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  xlab("Date of Rating Rounded to Year") +
  ylab("Average Rating") +
  theme_light() +
  theme(axis.title.x = element_text(vjust = -3)) +
  theme(axis.title.y = element_text(angle = 90, vjust = 3)) +
  theme(plot.title = element_text(size = 15, vjust = 5, hjust = 0.5)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

# arrange plot1, plot2, and plot3 to be presented together 
grid.arrange(plot1, plot2, plot3, top="Figure 6: Average Rating Across Time")

# plot of frequency of ratings by year of rating
edx_dates %>%
  group_by(movieId) %>%
  summarize(number = n(), year = as.character(first(year_rated))) %>% 
  ggplot(aes(x = year, y = number)) +
  geom_boxplot(color = "black", fill = "#00abff") +
  scale_y_sqrt(labels = comma, breaks = pretty_breaks(n = 10)) +
  scale_x_discrete(breaks = pretty_breaks(n = 10)) +
  xlab("Year of Rating") +
  ylab("Frequency of Ratings for Each Movie") +
  theme_light() +
  ggtitle("Figure 7: Frequency of Ratings per Movie by Year of Rating") +
  theme(axis.title.x = element_text(vjust = -3)) +
  theme(axis.title.y = element_text(angle = 90, vjust = 3)) +
  theme(plot.title = element_text(size = 15, vjust = 5, hjust = 0.5)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# extracting the release date of each movie
edx_dates <- edx_dates %>% mutate(date_released = str_extract(edx$title, "\\((\\d{4})\\)"))
edx_dates <- edx_dates %>% mutate(date_released = str_extract(edx_dates$date_released, "(\\d{4})"))
edx_dates <- edx_dates %>% mutate(year_released = as.numeric(date_released))

# calculating the difference between release date and rating date 
edx_dates <- edx_dates %>% mutate(year_rated = year(date_rated)) %>%
  mutate(relative_rating_age = year_rated - as.numeric(year_released))

save(edx_dates, file = "edx_dates.RData")

# examine for errors in extracting the date of release 
edx_dates %>% filter(date_released >= 2011) %>% 
  group_by(movieId, title, date_released) %>% 
  summarize(n = n())

edx_dates %>% filter(date_released <= 1900) %>% 
  group_by(movieId, title, date_released) %>% 
  summarize(n = n())

# plot of frequency of ratings by date of release
edx_dates %>%
  group_by(movieId) %>%
  summarize(number = n(), year = as.character(first(year_released))) %>% 
  ggplot(aes(x = year, y = number)) +
  geom_boxplot(color = "black", fill = "#00abff") +
  scale_y_sqrt(labels = comma, breaks = pretty_breaks(n = 10)) +
  scale_x_discrete(breaks = pretty_breaks(n = 10)) +
  xlab("Release Year") +
  ylab("Frequency of Ratings") +
  theme_light() +
  ggtitle("Figure 8: Frequency of Ratings by Release Year") +
  theme(axis.title.x = element_text(vjust = -3)) +
  theme(axis.title.y = element_text(angle = 90, vjust = 3)) +
  theme(plot.title = element_text(size = 15, vjust = 5, hjust = 0.5)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# plot of average ratings by date of release
edx_dates %>%
  group_by(year_released) %>%
  summarize(average = mean(rating)) %>% 
  ggplot(aes(x = year_released, y = average)) +
  geom_point(alpha = 0.75) +
  geom_smooth(method = "loess") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  xlab("Release Year") +
  ylab("Average Rating") +
  theme_light() +
  ggtitle("Figure 9: Average Rating by Release Year") +
  theme(axis.title.x = element_text(vjust = -3)) +
  theme(axis.title.y = element_text(angle = 90, vjust = 3)) +
  theme(plot.title = element_text(size = 15, vjust = 5, hjust = 0.5)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# plot of average rating by rating per year 
edx_dates %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2010 - first(year_released),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(x = rate, y = rating)) +
  geom_point(alpha = 0.75) +
  geom_smooth(method = "loess") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  xlab("Ratings per Year") +
  ylab("Average Rating") +
  theme_light() +
  ggtitle("Figure 10: Average Rating by Ratings per Year") +
  theme(axis.title.x = element_text(vjust = -3)) +
  theme(axis.title.y = element_text(angle = 90, vjust = 3)) +
  theme(plot.title = element_text(size = 15, vjust = 5, hjust = 0.5)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# table of top movies by ratings per year 
edx_dates %>% 
  group_by(movieId) %>%
  summarize(n = n(), years = 2010 - first(year_released),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(10, rate) %>%
  arrange(desc(rate)) %>%
  kable(caption = "Movies with the Most ratings per Year") %>%
  kable_styling(font_size = 10)

# table of least movies by ratings per year 
edx_dates %>% 
  group_by(movieId) %>%
  summarize(n = n(), years = 2010 - first(year_released),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(-10, rate) %>%
  arrange(rate) %>%
  kable(caption = "Movies with the Least Ratings per Year") %>%
  kable_styling(font_size = 10)

# plot of frequency of ratings by relative age of rating
edx_dates %>%
  group_by(movieId) %>% 
  summarize(number = n(), average_year = mean(relative_rating_age)) %>% 
  ggplot(aes(x = average_year, y = number)) +
  geom_point(stat = "identity", color = "black", fill = "#00abff") +
  scale_y_sqrt(labels = comma, breaks = pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  xlab("Average Relative Age of Rating") +
  ylab("Frequency of Ratings") +
  theme_light() +
  ggtitle("Figure 11: Frequency of Ratings by Avergae Relative Age of Rating") +
  theme(axis.title.x = element_text(vjust = -3)) +
  theme(axis.title.y = element_text(angle = 90, vjust = 3)) +
  theme(plot.title = element_text(size = 15, vjust = 5, hjust = 0.5)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# plot of average of ratings by relative age of rating
edx_dates %>%
  group_by(movieId) %>%
  summarize(average = mean(rating), average_year = mean(relative_rating_age)) %>% 
  ggplot(aes(x = average_year, y = average)) +
  geom_point(color = "black", fill = "#00abff") +
  geom_smooth(method = "loess") +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  xlab("Average Relative Age of Rating") +
  ylab("Average Rating") +
  theme_light() +
  ggtitle("Figure 12: Average Rating by Average Relative Age of Rating") +
  theme(axis.title.x = element_text(vjust = -3)) +
  theme(axis.title.y = element_text(angle = 90, vjust = 3)) +
  theme(plot.title = element_text(size = 15, vjust = 5, hjust = 0.5)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



########## Exploration of genre effects ########## 

# table of number of ratings and average rating by overall genre combination
edx %>% group_by(genres) %>%
  summarize(number = n(), average = mean(rating)) %>%
  arrange(desc(number)) %>%
  slice(1:10) %>%
  kable(caption = "Number and Average Rating by Overall Genre Combination") %>%
  kable_styling(font_size = 10)

# plot of the number of rating by overall genre combination (for genres with n >= 1000)
edx %>% group_by(genres) %>%
  summarise(number = n()) %>%
  filter(number >= 1000) %>% 
  mutate(genres = reorder(genres, -number)) %>%
  ggplot(aes(x = genres, y = number)) + 
  geom_bar(stat = "identity", color = "#00abff") +
  scale_y_sqrt(labels = comma, breaks = pretty_breaks(n = 10)) +
  xlab("Overall Genre Combinations") +
  ylab("Frequency of Ratings") +
  theme_light() +
  ggtitle("Figure 13: Frequency of Ratings by Overall Genre Combination") +
  theme(axis.title.x = element_text(vjust = -3)) +
  theme(axis.title.y = element_text(angle = 90, vjust = 3)) +
  theme(plot.title = element_text(size = 15, vjust = 5, hjust = 0.5)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(axis.text.x = element_blank())

# plot of average rating by overall genre combination (for genres with n >= 1000)
edx %>% group_by(genres) %>%
  summarize(number = n(), average = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(number >= 1000) %>% 
  mutate(genres = reorder(genres, -average)) %>%
  ggplot(aes(x = genres, y = average, ymin = average - 2*se, ymax = average + 2*se)) + 
  geom_point(color = "#00abff") +
  geom_errorbar(color = "Black") + 
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  xlab("Overall Genre Combinations") +
  ylab("Average Rating") +
  theme_light() +
  ggtitle("Figure 14: Average Rating by Overall Genre Combination") +
  theme(axis.title.x = element_text(vjust = -3)) +
  theme(axis.title.y = element_text(angle = 90, vjust = 3)) +
  theme(plot.title = element_text(size = 15, vjust = 5, hjust = 0.5)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(axis.text.x = element_blank())

# table of number of ratings and average rating by separate movie genres (CAUTION slow code)
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(number = n(), average = mean(rating)) %>%
  arrange(desc(number)) %>%
  kable(caption = "Number and Average Rating by Individual Genre Tags") %>%
  kable_styling(font_size = 10)


### Modeling Results















