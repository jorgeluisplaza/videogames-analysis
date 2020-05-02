#library(tidyverse)
#library(caret)
#library(randomForest)
#library(rpart)
#library(factoextra)
#library(data.table)

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", repos = "http://cran.us.r-project.org")



# Load data from file
videogames <- read_csv("videogames.csv", na = c("N/A"))

# Omit all NA's
videogames <- na.omit(videogames)

class(videogames$User_Count)
class(videogames$User_Score)
class(videogames$Global_Sales)
class(videogames$Critic_Score)

# User score as numeric (was character)
videogames$User_Score <- as.numeric(videogames$User_Score)


# Company by platform
sony <- c('PS','PS2','PS3','PS4' ,'PSP','PSV')
microsoft<- c('PC','X360','XB','XOne')
nintendo <- c('3DS','DS','GBA','GC','N64','Wii','WiiU')
sega <- c('DC')

# Assign Company if platform
changePlatform <-function(x){
  if (x %in% sony == TRUE) {return('Sony')}
  else if(x %in% microsoft == TRUE) {return('Microsoft')}
  else if(x %in% nintendo == TRUE) {return('Nintendo')}
  else if(x %in% sega == TRUE) {return('Sega')}
  else{return('Other')}
}

# Create new column
videogames$company <- sapply(videogames$Platform, changePlatform)

# Summary of the dataset
summary(videogames)

# get global sales by publisher
sales_per_publisher <- videogames %>% group_by(Publisher) %>%
                       summarise(ttl_sales = sum(Global_Sales)) %>%
                       arrange(desc(ttl_sales))

# Obtain the top 10 and sort it to the plot
top10 <- sales_per_publisher[1:10,]
top10 <- top10 %>% map_df(rev)
top10$Publisher <- as.character(top10$Publisher)
top10$Publisher <- factor(top10$Publisher, levels = unique(top10$Publisher))

# Ploting the top 10 publisher based on sales
top10 %>% ggplot() + 
          geom_bar(mapping = aes(x = ttl_sales, y = Publisher, fill = Publisher)
           ,stat = "identity") +
          labs(x = "Total Sales", y = "Publisher") +
          ggtitle("Top 10 Total Sales by Publisher")
  
# Critic score vs global sales graph
videogames %>% ggplot(aes(Global_Sales, Critic_Score, col = company)) +
  geom_point()

# NA_Sales vs Global Sales
videogames %>% group_by(Genre) %>%
  ggplot(aes(Global_Sales, NA_Sales, col = Genre)) +
  geom_point()

# User score vs critic score
videogames %>% ggplot(aes(Critic_Score, User_Score, col = company)) + 
               geom_point()

# Total games in a year graph
videogames %>% group_by(Year_of_Release,Genre)%>% 
               summarise(no_of_games = n()) %>% 
               ggplot(aes(x = Year_of_Release, y = no_of_games, group = Genre, col = Genre)) + 
               geom_point()+ 
               geom_line() + 
               theme(legend.position = "bottom", 
                     axis.text.x = element_text(angle = 90), 
                     panel.grid.minor = element_blank()) + 
               labs(x = "Year of Release", y = "Total games", title = "Games Released in a year")


# Top games by global sales
videogames %>% group_by(Name) %>% 
               summarise(global_sales=sum(Global_Sales)) %>% 
               arrange(desc(global_sales)) %>% 
               head(15) %>% 
               ggplot(aes(x = Name, y = global_sales, group = 1)) + 
               geom_line(size=0.8) + 
               geom_point(aes(col=global_sales),size=3) + 
               scale_color_gradientn(colours = heat.colors(20)) + 
               theme(axis.text.x = element_text(angle = 90)) 
               labs(x = "Game", y = "Global Sales", title="Top Video games by Sales")



# To data frame
videogames <- as.data.frame(videogames)

###### SPLITTING DATA #########

# Split the data 80-20
test_index <- createDataPartition(videogames$User_Score, times = 1, p = 0.2, list = FALSE)

# Train set
train_set <- videogames[-test_index,]

# Test set
test_set <- videogames[test_index,]

###### LINEAR MODEL ######

# Correlation between the variables
cor(train_set$User_Score, train_set$Critic_Score)

# Linear model
fit_lm <- lm(User_Score ~ Critic_Score, data = train_set)
y_hat <- predict(fit_lm, test_set)

# RMSE of the model
RMSE(y_hat, test_set$User_Score)

######## KMEANS #########

# Select the variables
train_kmeans <- train_set %>% select(User_Score, Critic_Score)

# Fit the model with 5 clusters
fit_kmeans <- kmeans(train_kmeans, centers = 5, nstart = 25)

# Plot the clusters
fviz_cluster(fit_kmeans, train_kmeans, labelsize = 0)

# Applying the elbow
fviz_nbclust(train_kmeans, kmeans, method = "wss")

# With 10 clusters
fit_kmeans_10 <- kmeans(train_kmeans, centers = 10, nstart = 25)

# Plot 10 clusters
fviz_cluster(fit_kmeans_10, train_kmeans, labelsize = 0)


####### REGRESSION TREE #########

# FIt the tree
fit_reg_tree <- rpart(company ~ User_Score + 
                        User_Count + 
                        Critic_Count + 
                        Global_Sales +
                        Critic_Score +
                        NA_Sales +
                        JP_Sales +
                        Other_Sales +
                        EU_Sales, data = train_set)

# Plot the tree
plot(fit_reg_tree, margin = 0.1)
text(fit_reg_tree, cex = 0.55)


####### RANDOM FOREST ########

# Obtain ps3 results
ps3_train_set <- subset(train_set, Platform == "PS3")
ps3_test_set <- subset(test_set, Platform == "PS3")

# Fit the random forest
fit_rf <- randomForest(NA_Sales ~ User_Count +
                         User_Score +
                         Critic_Count +
                         Critic_Score +
                         Genre +
                         Year_of_Release, data = ps3_train_set)

# Predict and calculate RMSE
y_hat_rf <- predict(fit_rf, ps3_test_set)
RMSE(y_hat_rf, ps3_test_set$NA_Sales)

# Obtain the variables importance
varImp(fit_rf)
