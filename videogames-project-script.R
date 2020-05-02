library(tidyverse)
library(caret)
library(randomForest)
library(rpart)
library(factoextra)

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


summary(videogames)



# get global sales by publisher
sales_per_publisher <- videogames %>% group_by(Publisher) %>%
                       summarise(ttl_sales = sum(Global_Sales)) %>%
                       arrange(desc(ttl_sales))

top10 <- sales_per_publisher[1:10,]
top10 <- top10 %>% map_df(rev)
top10$Publisher <- as.character(top10$Publisher)
top10$Publisher <- factor(top10$Publisher, levels = unique(top10$Publisher))

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


videogames %>% ggplot(aes(Critic_Score, User_Score, col = company)) + 
               geom_point()


videogames %>% ggplot(aes(Critic_Count, Critic_Score, col = company)) +
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


               
cor(videogames$User_Score, videogames$Critic_Score)
    
videogames <- as.data.frame(videogames)
           
test_index <- createDataPartition(videogames$User_Score, times = 1, p = 0.2, list = FALSE)

train_set <- videogames[-test_index,]
test_set <- videogames[test_index,]



fit_lm <- lm(User_Score ~ Critic_Score, data = train_set)
y_hat <- predict(fit_lm, test_set)

RMSE(y_hat, test_set$User_Score)





videogames %>% ggplot(aes(User_Count, Critic_Score, col = company)) + geom_point()

fit_rtree <- rpart(Genre ~ , data = videogames)

plot(fit_rtree)
text(fit_rtree)

videogames %>% ggplot(aes(Critic_Score, Global_Sales, col = company)) + geom_point()

train_knn <- train(Global_Sales ~ Critic_Score + Year_of_Release, method = "knn", data = videogames)

plot(train_knn)


train(Global_Sales ~ User_Score + Critic_Score, method = "knn", data = videogames, tuneGrid = data.frame(k = seq(3, 27, 2)))


sonySet <- subset(videogames, company == "Sony")


fitSony <- randomForest(Global_Sales ~ Critic_Score + User_Count + User_Score + Critic_Count, data = sonySet)
dist(videogames)

demo <- videogames %>% select(User_Count, User_Score)
dist(demo)
fit_kmeans <- kmeans(demo, centers = 10, nstart = 25)
fit_kmeans
fviz_cluster(fit_kmeans, demo, labelsize = 0)
fviz_nbclust(demo, kmeans, method = "wss")

fit <- rpart(Genre ~ Critic_Score + User_Count + NA_Sales + company + Publisher + Global_Sales, data = videogames)
fit

plot(fit, margin = 0.1)
text(fit, cex = 0.75)



