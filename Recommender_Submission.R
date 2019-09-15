
#Importing necessary libraries

#Data manipulation
if(!require(tidyverse)) install.packages("tidyverse",
                                         repos = "http://cran.us.r-project.org")
# Train/Test split and Model Training 
if(!require(caret)) install.packages("caret", 
                                     repos = "http://cran.us.r-project.org")
#Handle Tables
if(!require(data.table)) install.packages("data.table",
                                          repos = "http://cran.us.r-project.org")
#SVD decomposition
if(!require(recommenderlab)) install.packages("recommenderlab", 
                                              repos = "http://cran.us.r-project.org")
# to plot multiple grobs alongside
if(!require(gridExtra)) install.packages("gridExtra", 
                                         repos = "http://cran.us.r-project.org")
# to separate stacked genres
if(!require(splitstackshape))install.packages("splitstackshape",
                                              repos = "http://cran.us.r-project.org") 
#to transform shape of our data
if(!require(reshape2)) install.packages("reshape2", 
                                        repos = "http://cran.us.r-project.org") 
#For rmse function
if(!require(ModelMetrics))  install.packages("ModelMetrics", 
                                             repos = "http://cran.us.r-project.org") 


#Data Import 
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

#if you want to run the whole dataset, disable the following command
movielens <- movielens %>% slice(1:floor(nrow(movielens)/4))

rm( ratings, movies )


movielens$timestamp <- movielens$timestamp %>% as.Date.POSIXct()

movielens$timestamp_year <- movielens$timestamp %>% year()

movielens <- movielens %>% 
  mutate_if(is.integer, as.factor) #integers to factors

movielens$year <- movielens %>% 
  pull(title) %>% 
  str_extract("\\([:digit:]{4}\\)") %>%
  str_remove_all("\\(|\\)") %>% 
  as.integer()


movielens$title <-movielens$title %>% str_remove("\\([:digit:]{4}\\)")



mu <- mean(movielens$rating)
med <- median(movielens$rating)


movielens %>% ggplot( aes(x=rating)) +   
  geom_density(aes(y=..density..), fill = "#99beff", col = '#d9a757', alpha = .2  )+ 
  geom_histogram(aes(y=..density..),binwidth = .5,fill="#498ee3")+ 
  geom_vline( aes( xintercept = mu, colour = "mean"), linetype ='dashed' ) + 
  geom_vline(aes( xintercept = med, colour = "median"), linetype ='dashed' )  +
  theme_minimal()+
  labs(title = "Histogram and Density of Ratings", x = "Rating", y = "Density")+
  theme(legend.title = element_blank())



#Create a new dataframe containing the count of each user
cnt <- movielens %>% group_by(userId) %>% summarise( user_cnt = n() )

#average rating of each user
rat <- movielens %>% group_by(userId) %>% summarise( mean_rating = mean(rating) )

p1 <- cnt %>% left_join( rat, by = "userId" ) %>%
  ggplot( aes(y = user_cnt, x = mean_rating ) ) + 
  geom_point(show.legend = F, alpha = .1,col = "#4685f2") + 
  labs( title = "Count of User vs Mean_Rating", x = "", y = "Times each user rated" )

#count of each movie
cnt_mov<- movielens %>% group_by(movieId) %>% summarise( movie_cnt = n() )

#average rating of each movie
rat_mov <- movielens %>% group_by(movieId) %>% summarise( mean_rating = mean(rating) )

mov_df <- cnt_mov %>% left_join(rat_mov, by = "movieId" )

p2 <-  mov_df %>%
  ggplot( aes(y = movie_cnt, x = mean_rating ) ) + 
  geom_point(show.legend = F, alpha = .1,col = "#4685f2") + 
  labs( title = "Count of Movie vs Mean_Rating", x = "Mean Ratings", y = "Times each movie was rated" )

grid.arrange(p1,p2)

rm(cnt,rat,cnt_mov,rat_mov,p1,p2)


#Add the title from original dataset
mov_df$title <- movielens[match(mov_df$movieId, movielens$movieId),"title"]

worst_movies <- mov_df %>%
  arrange((mean_rating)) %>% #arrange from highest to lowest rating 
  slice(1:10) %>% #grab the first 10
  ggplot( aes(x = reorder(title,mean_rating), y = mean_rating) ) + 
  geom_point(size = 9, col = "#baaa6e") + 
  coord_flip() +
  geom_vline(xintercept = 1:10, col = "grey80") + #lines on which our dots "walk"
  theme_linedraw()+ 
  theme(axis.line = element_blank()) +
  geom_text( aes(label = mean_rating), col = "black" ) +
  labs(title = "Worst Movies", subtitle =  "Regardless of the Number of Ratings" , y = "Mean Rating", x = "")

best_movies <- mov_df %>%
  arrange(desc(mean_rating)) %>% #arrange from highest to lowest rating 
  slice(1:10) %>% #grab the first 10
  ggplot( aes(x = reorder(title,mean_rating), y = mean_rating) ) + 
  geom_point(size = 9, col = "#ceabd1") + 
  coord_flip() +
  geom_vline(xintercept = 1:10, col = "grey80") + #lines on which our dots "walk"
  theme_linedraw()+ 
  theme(axis.line = element_blank()) +
  geom_text( aes(label = mean_rating), col = "black" ) +
  labs(title = "Best Movies", subtitle =  "Regardless of the Number of Ratings" , y = "Mean Rating", x = "")

grid.arrange(best_movies, worst_movies)

worst_movies <- mov_df %>%
  arrange((mean_rating)) %>% #arrange from highest to lowest rating 
  slice(1:10) %>% #grab the first 10
  ggplot( aes(x = reorder(title,mean_rating), y = mean_rating) ) + 
  geom_point(size = 9, col = "#baaa6e") + 
  coord_flip() +
  geom_vline(xintercept = 1:10, col = "grey80") + #lines on which our dots "walk"
  theme_linedraw()+ 
  theme(axis.line = element_blank()) +
  geom_text( aes(label = movie_cnt), col = "black" ) +
  labs(title = "Worst Movies", subtitle =  "Regardless of the Number of Ratings" , y = "Mean Rating", x = "")

best_movies <- mov_df %>%
  arrange(desc(mean_rating)) %>% #arrange from highest to lowest rating 
  slice(1:10) %>% #grab the first 10
  ggplot( aes(x = reorder(title,mean_rating), y = mean_rating) ) + 
  geom_point(size = 9, col = "#ceabd1") + 
  coord_flip() +
  geom_vline(xintercept = 1:10, col = "grey80") + #lines on which our dots "walk"
  theme_linedraw()+ 
  theme(axis.line = element_blank()) +
  geom_text( aes(label = movie_cnt), col = "black" ) +
  labs(title = "Best Movies", subtitle =  "Regardless of the Number of Ratings" , y = "Mean Rating", x = "")

grid.arrange(best_movies, worst_movies)

#pick the 1rd quantile of the movie count as a low limit
limit <- (summary(mov_df$movie_cnt))[5] 

best_movies <- mov_df %>%
  filter(movie_cnt > limit) %>% #throw away movies with few ratings
  arrange(desc(mean_rating)) %>% #arrange from highest to lowest rating 
  slice(1:10) %>% #grab the first 10
  ggplot( aes(x = reorder(title,mean_rating), y = mean_rating) ) + 
  geom_point(size = 9, col = "#ceabd1") + coord_flip() +
  geom_vline(xintercept = 1:10, col = "grey80") + #lines on which our dots "walk"
  theme_classic()+ 
  theme(axis.line = element_blank()) +
  geom_text( aes(label = movie_cnt), col = "black" ) +
  labs(title = paste("Best Movies with more than",limit,"Ratings"), y = "Mean Rating", x = "", subtitle = "Number of Ratings inside the Dots" )

worst_movies <- mov_df %>%
  filter(movie_cnt > limit) %>% #throw away movies with few ratings
  arrange((mean_rating)) %>% #arrange from highest to lowest rating 
  slice(1:10) %>% #grab the first 10
  ggplot( aes(x = reorder(title,mean_rating), y = mean_rating) ) + 
  geom_point(size = 9, col = "#baaa6e") + coord_flip() +
  geom_vline(xintercept = 1:10, col = "grey80") + #lines on which our dots "walk"
  theme_classic()+ 
  theme(axis.line = element_blank()) +
  geom_text( aes(label = movie_cnt), col = "black" ) +
  labs(title = paste("Worst Movies with more than",limit,"Ratings"), y = "Mean Rating", x = "", subtitle = "Number of Ratings inside the Dots" )

grid.arrange(best_movies, worst_movies)
rm(mov_df,best_movies,worst_movies)


#For every genre entry create a separate row containing only this entry
movielens_genres <- movielens %>%
  cSplit( "genres", #split the genres column 
          sep="|", #on separator |
          direction =  "long" ) #towards a long direction 

print(" Genres count of distinct values : ")
movielens_genres %>% pull(genres) %>% table() %>% sort(decreasing = TRUE) %>% print()

movielens_genres$genres <- plyr::revalue(movielens_genres$genres, c("Horror" = "Thriller", 
                                                                    "Adventure" = "Action",
                                                                    "War" = "Action" ,
                                                                    "Sci-Fi" = "Fantasy"))

print(" Genres count of distinct values : ")
movielens_genres %>% pull(genres) %>% table() %>% sort(decreasing = TRUE) %>% print()

pop_genres <- movielens_genres %>% 
  group_by( genres ) %>% 
  summarize( n = n() ) %>%
  arrange(desc(n)) %>% 
  head(10)

pop_genres %>%
  ggplot( aes(x = reorder(genres,n), y = n) ) + 
  geom_bar( aes(fill = genres), stat = "identity",width = .6, show.legend = FALSE)+ 
  coord_flip()+
  theme_linedraw()+
  labs( title = "Count of Ratings by Genre", y = "Count", x = ""  )

movielens_genres %>%
  filter(genres %in% pop_genres$genres ) %>%
  group_by(genres) %>%
  summarize(mean_rating = mean(rating)) %>% 
  mutate( n = pop_genres$n[(match(genres, pop_genres$genres))] ) %>%  
  ggplot( aes( x = reorder(genres,n), y = mean_rating ) )+
  geom_point(size = 9, col = "#2ecff0") + 
  geom_vline( xintercept = 1:10, col = "gray" )+
  theme(axis.line = element_blank()) +
  geom_text( aes(label = round(mean_rating,2), size = 8 ), col = "black", show.legend = FALSE ) +
  coord_flip() + 
  theme_minimal() + 
  labs( title = "Mean Rating per Genre", subtitle = "Ordered by number of ratings", x = "", y = "Mean Rating" )

movielens_genres %>%
  filter(genres %in% pop_genres$genres ) %>%
  ggplot() + 
  geom_violin( aes( x = genres, y = rating, fill = genres ), show.legend = FALSE ) + 
  coord_flip() + 
  labs( title = "Ratings by Genre", x = "", y = "Rating" ) + 
  theme_linedraw()

#store top genres and users in variables for ease of filtering
top_genres <- movielens_genres %>% 
  group_by( genres ) %>% 
  summarize( n = n() ) %>% 
  arrange(n%>% desc()) %>% 
  head(6) %>% 
  pull(genres)

top_users <- movielens_genres %>% 
  group_by(userId) %>% 
  summarize( user_count = n() ) %>% 
  arrange(desc(user_count)) %>% 
  head(6) %>% 
  pull(userId)

movielens_genres <- movielens_genres %>% 
  filter(genres %in% top_genres, userId %in% top_users)#keep only top genres and users


#transform data in wide format
movielens_genres <- movielens_genres %>% 
  dcast(userId + movieId + title + rating ~ genres,
        fun.aggregate = length, 
        value.var = "rating")  #convert to dummy columns

#Replace 1's with the actual rating
movielens_genres[,4:ncol(movielens_genres)] <- movielens_genres[,4:ncol(movielens_genres)] %>% 
  mutate_all( ~case_when( . !=0 ~ rating, . ==0 ~ 0 ) ) #whenever each dummy column is not zero, place the equivalent rating

movielens_genres[,c(1,5:10)] %>% 
  group_by(userId) %>% 
  summarize_all(mean)  %>% #compute the mean of each user in each genre
  gather(key = "Genre", value = "Average_Rating" , -userId) %>% #gather it for easier plotting
  ggplot( aes(x =  Genre, y = Average_Rating)) + 
  geom_point(col = "#b8182d") +
  facet_grid(~userId) +
  geom_segment(aes(x = Genre, y = 0, xend = Genre, yend = Average_Rating-.02), color = "#ffc64a") +
  labs(title = "Average Rating per Genre", subtitle = "Faceted by UserId", y = "Average Rating", x = "User")+
  theme_linedraw()+
  theme(axis.text.x = element_text(face = "bold", color = "#2b05b3", size = 8, angle = 60))

rm(movielens_genres,top_genres,top_users, pop_genres)



movielens %>% group_by(year) %>%  
  summarize( average_rating = mean(rating) ) %>%
  ggplot(aes(x = year , y = average_rating) ) + 
  geom_line(lwd = 1.1, col = "navyblue")+ 
  #geom_point( aes(size  ) +
  theme_minimal() + 
  scale_y_continuous(limits = c(0,5))+
  labs(y = "Yearly Average Rating",x = "Year", title = "Average Rating by Release Year")



#medi <- median(movielens$)

movielens %>% group_by( timestamp_year ) %>%
  summarise(avg_rating = mean(rating), Number_of_Ratings = n()) -> temp

medi <- median(temp$avg_rating)

temp %>%
  ggplot( aes( x = timestamp_year, y = avg_rating ) ) + 
  #geom_line(lwd = 1.1, col = "navyblue")+ 
  geom_point(aes(size = Number_of_Ratings ), col = "navyblue")+
  geom_hline(  yintercept = medi)+
  theme_minimal() + 
  scale_y_continuous(limits = c(0,5))+
  labs(y = "Yearly Average Rating",x = "Year", 
       title = "Average Rating by Year each Rating placed",
       subtitle = "Median Average Rating as Horizontal line")



movielens <- movielens %>% select( c(movieId,userId,rating) )

set.seed(1)
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

mu <- mean(edx$rating)

rm(test_index, temp, movielens, removed)

set.seed(1)
# if using R 3.5 or earlier, use `set.seed(1)` instead

test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.8, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set

removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

rm(test_index, temp, removed)

l_optimizer <- function(train_set, lambdas){
  
  rmses <- sapply(lambdas, function(l){
    
    #Calculate movie bias
    b_i <- train_set %>% 
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+l))
    
    #Calculate user bias
    b_u <- train_set %>% 
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    
    #Predict ratings  
    predicted_ratings <- test_set %>% 
      left_join(b_i, by='movieId') %>%
      left_join(b_u, by = "userId") %>%
      mutate(pred = mu + b_i + b_u) %>%
      pull(pred)
    
    #Calculate the error for the specific lambda
    return(RMSE(predicted_ratings, test_set$rating))
  })
  
  return(lambdas[which.min(rmses)])
}

#A vector of possible lambdas
lambdas <- seq(0, 5, 0.5)

#Assign the optimal lambda to a variable
lambda <- l_optimizer(train_set, lambdas)

#Take a look at it
print(paste("Optimal Lambda :",lambda))

#Calculate movie bias
b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

#Calculate user bias
b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

#Function to calculate the residuals

resids <- function( train_set = train_set, mu = mu ){
  
  #This function takes training set, calculates the residuals left after subtracting mu, user and movie bias
  #from actual rating and transforms it into a sparse matrix with movieIds as columns and users as row
  
  df <- train_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(res = rating - (mu + b_i + b_u) ) %>%
    select(userId, movieId, res) %>%
    spread( movieId, res ) %>% as.data.frame()
  
  #Name the rows
  rownames(df)<- df[,1]
  df <- df[,-1]
}

#convert our train set to residuals
#tr <- resids(train_set = train_set,mu=mu)

#A quick look at the residual values
#print("Residuals of the first 10 users and movies")
#tr[1:10,1:10] %>% print()

optimizer <- function(tr = tr, 
                      test_set = test_set,
                      ks,gammas,lambdas,min_epochs, min_improvements){
  
  #Inputs: parameters of funkSVD function
  d <- data.frame(k = NULL, 
                  gamma = NULL, 
                  lambda = NULL, 
                  min_epochs = NULL, 
                  min_improvement = NULL, 
                  rmse = NULL)
  
  for (k in ks){
    for (g in gammas){
      for (l in lambdas){
        for (e in min_epochs){
          for (imp in min_improvements) {
            
            #decompose residuals with funk SVD 
            a <- funkSVD(tr,
                         k = k, 
                         gamma = g, 
                         lambda = l, 
                         min_epochs = e, 
                         max_epochs = 200, 
                         min_improvement = imp, 
                         verbose = TRUE )
            
            #recompose them (returns a full matrix in place of the sparse tr)
            r <- tcrossprod(a$U, a$V)
            
            #pass the original colnames to the new matrix
            colnames(r) <- colnames(tr)
            
            #create a new vector (called re) on test set 
            #containing the appropriate p * q (recomposed) term
            test_set$re <- seq(0,nrow(test_set)-1) #to-be-filled vector of zeros
            
            #for each row of test set
            for (i in 1:nrow(test_set)){
              
              #fill the vector of zeros with the proper calculated p*q quantity
              test_set$re[i] <-  r[ test_set$userId[i] , which(test_set$movieId[i] == colnames(r)) ]          
            }
            
            #calculate our prediction
            tes <- test_set %>% 
              left_join(b_i, by = "movieId") %>% #bring the movie bias
              left_join(b_u, by = "userId") %>% #bring the user bias
              mutate(pred = mu + b_i + b_u + re) # calculate our prediction
            
            #store results in a data frame
            d <- d %>% rbind(data.frame(k = k, gamma = g, lambda = l,
                                        min_epochs = e, min_improvement = imp,
                                        rmse = ModelMetrics::rmse(tes$rating, tes$pred) ) )
            
          }}}}}
  
  return(d) 
}

#Define the parameters
ks <- c(1)
gammas <-  c(.009)
lambdas <- c(.005)
min_epochs <- c(5) 
min_improvements <- c(.001)

optimization <- FALSE

if(optimization){
  results <- optimizer(tr = tr, test_set = test_set, ks,gammas,lambdas,min_epochs, min_improvements)
  
  results %>% 
    ggplot( aes( x = reorder(k,rmse) , y = (rmse)) ) +
    geom_point(aes(col = as.factor(gamma) ), size = 9, alpha = .8) + 
    coord_flip() +
    geom_vline(xintercept = 1:nrow(results), col = "grey80") + #lines on which our dots "walk"
    theme_minimal()+ 
    theme(axis.line = element_blank()) +
    labs(title = "RMSE results", subtitle =  "" , y = "RMSE", x = "Ranks", col = "Gamma", shape = "Lambda")
  
  opt_params <- results[which(results$rmse == min(results$rmse)),]
  
  
}else{
  
  results <- c(ks,gammas,lambdas,min_epochs,min_improvements)
  
  opt_params <- results
}


"Optimal Parameters : " %>% print()
opt_params %>% print()
rm(train_set,test_set)

#subtract the mean, movie and user biases
tr <- resids(train_set = edx, mu=mu)

final_RMSE <- optimizer( tr =  tr,
                         test_set = validation,
                         ks = opt_params[1],
                         gammas = opt_params[2],
                         lambdas = opt_params[3],
                         min_epochs = opt_params[4],
                         min_improvements = opt_params[5])

print("Final RMSE:")
final_RMSE$rmse %>% print()


#Just the mean
mean_only <- rmse(validation$rating,rep(mu,nrow(validation)))

#Adding the user bias

pred_bu <- validation %>% 
  left_join(b_i, by = "movieId") %>%
  mutate(pred = (mu + b_i) ) %>%
  pull(pred)

mean_bu <- rmse(validation$rating,pred_bu)

pred_bu_bi <- validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = (mu + b_i + b_u) ) %>%
  pull(pred)

mean_bu_bi <- rmse(validation$rating,pred_bu_bi)

data.frame(model = c("Mean Only","Mean + User Bias", "Mean + User + Movie Bias", "Mean + User + Movie Bias +Matrix Factorization") , 
           rmses = c(mean_only, mean_bu, mean_bu_bi, final_RMSE$rmse)) %>% 
  ggplot( aes(x = model, y = rmses) ) + 
  geom_point( size = 6, col = "#4c7329" ) + 
  coord_flip()+
  theme_linedraw()+
  labs( title = "Comparison of Models", y = "RMSE", x ="")

