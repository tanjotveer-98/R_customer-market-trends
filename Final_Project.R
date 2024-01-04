#Library
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggthemes)
library(RColorBrewer)
library(superml)
library(cluster)
library(ggfortify)
library(class)
library(tree)
library(randomForest)
library(caret)

#Reading the Data (UPDATE WITH YOUR FILE PATH)
shoppers<- read.csv("D:/Study/Business Analytics/R_Projects/shopping_trends.csv")


#Checking Null Values 
Null_Values <- colSums(is.na(shoppers))
print(Null_Values)


#---------EXPOLORATORY DATA ANALYSIS

#-----TOTAL SALES-------------

#Amount of Sales by Item purchased
total_sales <- shoppers %>%
  group_by(Item.Purchased) %>%
  summarise(TotalSales = sum(Purchase.Amount..USD.)) %>%
  arrange(desc(TotalSales))
View(total_sales)

#Visualizing Total Sales
ggplot(total_sales, aes(x =Item.Purchased, y = TotalSales, fill = Item.Purchased)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by Product", x = "Product", y = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#---ITEM COUNTS---------

#----------1.DESCRIPTIVE ANALYSIS---

#Counting how many of each item was sold (Also shows the top best selling items)
item_counts <- shoppers %>%
  group_by(Item.Purchased) %>%
  summarise(ItemCount = n()) %>%
  arrange(desc(ItemCount))
View(item_counts)

#Visualizing Item Count
ggplot(item_counts, aes(x = Item.Purchased, y = ItemCount, fill = Item.Purchased)) +
  geom_bar(stat = "identity") +
  labs(title = "Item Counts", x = "Item Purchased", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#------ITEMS SOLD BY CATEGORY------

#Counting How many items sold per each category
items_per_category <- shoppers %>%
  group_by(Category) %>%
  summarise(TotalItemsSold = n())
View(items_per_category)

#Visualizing Items per category
ggplot(items_per_category, aes(x = reorder(Category, TotalItemsSold), y = TotalItemsSold, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Items Sold by Category", x = "Category", y = "Total Items Sold") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#-----------POPULAR PAYMENT METHODS----------------------------------------------------
payment_count <- table(shoppers$`Payment.Method`)

# Create a data frame for plotting
payment_df <- data.frame(Payment_Method = names(payment_count), Count = as.numeric(payment_count))

# Create the bar plot with automatic color assignment
ggplot(payment_df, aes(x = Payment_Method, y = Count, fill = Payment_Method)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set2") +  # Using Set2 palette for colors
  theme_fivethirtyeight() +
  labs(title = "Payment Methods",
       x = "Payment Method",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))



#------------- Calculate season revenue
season_count <- table(shoppers$Season)

# Create a data frame for plotting
season_df <- data.frame(Season = names(season_count), Revenue = as.numeric(season_count))

# Create the bar plot with automatic color assignment
ggplot(season_df, aes(x = Season, y = Revenue, fill = Season)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set2") +  # Using Set2 palette for colors
  theme_fivethirtyeight() +
  labs(title = "Season Revenue",
       x = "Season",
       y = "Revenue") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))



#---------2.CONSUMER BASED ANALYSIS-------------------

# Define ranges
age_ranges <- c(0, 18, 25, 35, 45, 55, 65, Inf)
age_labels <- c("0-18", "19-25", "26-35", "36-45", "46-55", "56-65", "66+")

# Create a new column "Age Range" 
shoppers$AgeRange <- cut(shoppers$Age, breaks = age_ranges, labels = age_labels, include.lowest = TRUE)

consumer_segments_analysis <- shoppers %>%
  group_by(AgeRange, Gender, Item.Purchased) %>%
  summarise(Count = n())
View(consumer_segments_analysis)

#----------Visualize item purchased by age range.
ggplot(consumer_segments_analysis, aes(x = reorder(Item.Purchased, Count), y = Count, fill = AgeRange)) +
  geom_bar(stat = "identity") +
  labs(title = "Consumer Segments Analysis", x = "Item Purchased", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#----------- Age Distribution by Gender
plot_ly(data = shoppers, x = ~Gender, y = ~Age, color = ~Gender, type = 'box') %>%
  layout(
    xaxis = list(title = 'Gendre'),  # X-axis label
    yaxis = list(title = 'Age'),    # Y-axis label
    title = 'Age Distribution by Gender'  
  )


#---------------- Relationship between Age and Purchase Amount
plot_ly(data = shoppers, x = ~Age, y = ~`Purchase.Amount..USD.`, color = ~`Review.Rating`,
        size = ~`Review.Rating`, sizes = c(1, 10), type = 'scatter', mode = 'markers') %>%
  layout(
    xaxis = list(title = 'Age'),  # X-axis label
    yaxis = list(title = 'Purchase Amount (USD)'),  # Y-axis label
    title = 'Relationship between Age and Purchase Amount (with rating)'  # Plot title
  )

# Get the top 10 locations and their counts
top_locations <- head(sort(table(shoppers$Location), decreasing = TRUE), 10)

# Create a data frame for plotting
location_df <- data.frame(Location = names(top_locations), Frequency = as.numeric(top_locations))

# Create the bar plot
ggplot(location_df, aes(x = reorder(Location, -Frequency), y = Frequency, fill = Location)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Top 10 Occurrences by Location",
    x = "Location",
    y = "Number of Occurrences"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "YlOrRd") +  # Change the palette if needed
  xlab("Location") +
  ylab("Number of Occurrences")


#---------Locations by purchase amount.
ggplot(shoppers, aes(x = Location, y = `Purchase.Amount..USD.`, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Locations by Purchase Amount ($)",
       x = "Locations",
       y = "Purchase Amount ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")

#------------Categories by size---------------------------------------------------
ggplot(shoppers, aes(x = Category, fill = Size)) +
  geom_bar(position = "dodge") +
  labs(title = "Count of Categories by Size",
       x = "Category",
       y = "Count") +
  theme_minimal()


#-----3. SEASONAL ANALYSIS----
# Group by season and item, then summarize the counts
seasonal_product_analysis <- shoppers %>%
  group_by(Season, Item.Purchased) %>%
  summarise(Count = n())
View(seasonal_product_analysis)

#Seasonal Analysis
ggplot(seasonal_product_analysis, aes(x = reorder(Item.Purchased, Count), y = Count, fill = Season)) +
  geom_bar(stat = "identity") +
  labs(title = "Seasonal Product Analysis", x = "Item Purchased", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##_____________________________DATA TRANSFORMATION____________________

View(shoppers)
#Removing customer id and age range.

shoppers_tf= shoppers %>% 
  select(-1,-20)


#Check for NA values.
for(i in c(1:ncol(shoppers_tf))){
  if(any(is.na(shoppers_tf[i]))==TRUE)
  {
    print(shoppers_tf[i])
  }
  else
  {
    print(i)
  }
}

#checking if numeric columns have proper type.
numeric_cols<- sapply(shoppers_tf,is.numeric)
head(shoppers_tf[numeric_cols])

summary(shoppers_tf)

#Factorizing categorical variables.
shoppers_tf[!numeric_cols]<- lapply(shoppers_tf[!numeric_cols], factor)
sapply(shoppers_tf,levels)
str(shoppers_tf)

#-----------------------------Starting ML --------------------------------------

#-KNN----------

# PREDICTING SUBSCRIPTION STATUS

#for k-fold validation with 5 segments.
trControl <- trainControl(method  = "cv",
                          number  = 5)

knn_fit_Subs <- train(Subscription.Status ~ .,
                 method     = "knn",
                 tuneGrid   = expand.grid(k = 1:30),
                 trControl  = trControl,
                 metric     = "Accuracy",
                 data       = shoppers_tf)
knn_fit_Subs

#PREDICTING CATEGORY
knn_fit_Category <- train(Category ~ .,
                 method     = "knn",
                 tuneGrid   = expand.grid(k = 1:100),
                 trControl  = trControl,
                 metric     = "Accuracy",
                 data       = shoppers_tf)
knn_fit_Category

#------------------RANDOM FOREST---

TestError_Model<- double(5)
TrainError_Model<- double(5)


#k-fold cross validation
fold_v<- c(1:5)
fold_v

fold <- sample(fold_v, nrow(shoppers_tf), replace=TRUE) #choose from fold vector to generate 'nrow' no. of values.

#FOR SUBSCRIPTION STATUS
set.seed(1)
for(counter in c(1:5))
{
  train_set= shoppers_tf[fold!= counter,]
  test_set= shoppers_tf[fold == counter,]
  
  rf_subs <- randomForest(Subscription.Status~ ., data = train_set,
                     mtry = 5)
  
  yhat_train <- predict(rf_subs, newdata = train_set)
  TrainError_Model[counter] <-mean(yhat_train != train_set$Subscription.Status)
  
  yhat_test <- predict(rf_subs, newdata = test_set)
  TestError_Model[counter] <-mean(yhat_test != test_set$Subscription.Status)
  conf_matrix <- table(test_set$Subscription.Status, yhat_test)
  print(conf_matrix)
}

print(TestError_Model)
Model_Mean_Error<- mean(TestError_Model)
Model_Mean_Error 

rf_subs
importance(rf_subs)


#to check if 500 trees are required
oob.error.data<- data.frame(
  Trees=rep(1:nrow(rf_subs$err.rate),times= 3),
  Type=rep(c("OOB", "No","Yes"),each= nrow(rf_subs$err.rate)),
  Error=c(rf_subs$err.rate[,"OOB"],
          rf_subs$err.rate[,"No"],
          rf_subs$err.rate[,"Yes"]))
oob.error.data
ggplot(data= oob.error.data,aes(x=Trees, y=Error))+
  geom_line((aes(color= Type)))

#Will the error decrease more if we increase number of trees?

#final ntree= 200 because no significant decrease in test error after that.

oob.values<- vector(length=8)
for(i in 1:8){
  temp.rf<- randomForest(Subscription.Status~., data= shoppers_tf, mtry=i, ntree=1000)
  oob.values[i]<- temp.rf$err.rate[nrow(temp.rf$err.rate),1]
}
oob.values #mtry=3 has lowest Out of Bag error.

#so we will use mtry=3, ntree=200 for the final model.

set.seed(1)
for(counter in c(1:5))
{
  train_set= shoppers_tf[fold!= counter,]
  test_set= shoppers_tf[fold == counter,]
  
  rf_subs2 <- randomForest(Subscription.Status~ ., data = train_set,
                     mtry = 5, ntree=200)
  
  yhat_train <- predict(rf_subs2, newdata = train_set)
  TrainError_Model[counter] <-mean(yhat_train != train_set$Subscription.Status)
  
  yhat_test <- predict(rf_subs2, newdata = test_set)
  TestError_Model[counter] <-mean(yhat_test != test_set$Subscription.Status)
  conf_matrix <- table(test_set$Subscription.Status, yhat_test)
  print(conf_matrix)
}
rf_subs2

importance(rf_subs2)

##-Category

set.seed(1)
for(counter in c(1:5))
{
  train_set= shoppers_tf[fold!= counter,]
  test_set= shoppers_tf[fold == counter,]
  
  rf_Cat <- randomForest(Category~ ., data = train_set,
                     mtry = 5, ntree= 10)
  
  yhat_train <- predict(rf_Cat, newdata = train_set)
  TrainError_Model[counter] <-mean(yhat_train != train_set$Category)
  
  yhat_test <- predict(rf_Cat, newdata = test_set)
  TestError_Model[counter] <-mean(yhat_test != test_set$Category)
  conf_matrix <- table(test_set$Category, yhat_test)
  print(conf_matrix)
}

rf_Cat
importance(rf_Cat)

#----------------K-MEANS----------

#---Determining the clusters for segmentation of Age for predicting the Review Rating by age.
kmeans_data <- shoppers[, c("Age", "Review.Rating")]

k <- 3  # Number of clusters
kmeans_model <- kmeans(kmeans_data, centers = k)
kmeans_data$Cluster <- as.factor(kmeans_model$cluster)


ggplot(kmeans_data, aes(x = Age, y = Review.Rating, color = Cluster)) +
  geom_point() +
  labs(title = "K-Means Clustering", x = "Age", y = "Review Rating")

cluster_summary <- aggregate(. ~ Cluster, data = kmeans_data[, c("Cluster", "Age", "Review.Rating")], mean)
print(cluster_summary)


ggplot(kmeans_data, aes(x = Age, y = Review.Rating, color = Cluster)) +
  geom_point() +
  labs(title = "K-Means Clustering", x = "Age", y = "Review Rating") +
  scale_color_manual(values = c("red", "blue", "green"))  # Adjust colors as needed


#----------------Transformation-----

# create a label encoder object 
encoder = LabelEncoder$new() 

#shoppers<- apply(shoppers,2, encoder$fit_transform)

#View(shoppers)


{
# transforming the data 
shoppers$Customer.ID<- encoder$fit_transform(shoppers$Customer.ID) 

# transforming the data 
shoppers$Gender<- encoder$fit_transform(shoppers$Gender) 

# transforming the data 
shoppers$Item.Purchased<- encoder$fit_transform(shoppers$Item.Purchased) 


# transforming the data 
shoppers$Category<- encoder$fit_transform(shoppers$Category)

# transforming the data 
shoppers$Location<- encoder$fit_transform(shoppers$Location)


# transforming the data 
shoppers$Size<- encoder$fit_transform(shoppers$Size)


# transforming the data 
shoppers$Color<- encoder$fit_transform(shoppers$Color)

# transforming the data 
shoppers$Subscription.Status<- encoder$fit_transform(shoppers$Subscription.Status)

# transforming the data 
shoppers$Shipping.Type<- encoder$fit_transform(shoppers$Shipping.Type)

# transforming the data 
shoppers$Discount.Applied<- encoder$fit_transform(shoppers$Discount.Applied)

# transforming the data 
shoppers$Promo.Code.Used<- encoder$fit_transform(shoppers$Promo.Code.Used)

# transforming the data 
shoppers$Payment.Method<- encoder$fit_transform(shoppers$Payment.Method)

# transforming the data 
shoppers$Frequency.of.Purchases<- encoder$fit_transform(shoppers$Frequency.of.Purchases)

# transforming the data 
shoppers$AgeRange<- encoder$fit_transform(shoppers$AgeRange)

# transforming the data 
shoppers$Season<- encoder$fit_transform(shoppers$Season)

shoppers$Preferred.Payment.Method<- encoder$fit_transform(shoppers$Preferred.Payment.Method)
}

# Calculate within-cluster sum of squares for different values of K
wss <- sapply(1:10, function(k) {
  kmeans(shoppers, k, nstart = 10)$tot.withinss
})


# Plotting the elbow graph with specified font family
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters (K)", ylab = "Within clusters sum of squares (WSS)",
     family = "Arial")


colnames(shoppers)
# K-means clustering with 2 clusters
kmean2 <- kmeans(shoppers, 2)
kmean2$centers

#Getting kmeans for Purchase amount by age of the customer.
set.seed(1)
ggplot(shoppers, aes(x =Purchase.Amount..USD., y = Age)) + 
  geom_point(stat = "identity", aes(color = as.factor(kmean2$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2"),
                       labels=c("Cluster 1", "Cluster 2")) +
  ggtitle("Segments of Shoppers", subtitle = "Using K-means Clustering")


# Add the cluster labels to the dataset
shoppers$Cluster2 <- as.factor(kmean2$cluster)

# Create a 3D scatter plot with different colors
plot_ly(
  data = shoppers,
  x = ~Purchase.Amount..USD.,
  y = ~Age,
  z = ~Season,
  color = ~Cluster2,
  type = 'scatter3d',
  mode = 'markers',
  marker = list(size = 5),
  colors = c('#1f77b4', '#ff7f0e')  # Custom colors for clusters, you can change these
) %>% 
  layout(
    scene = list(
      xaxis = list(title = 'Purchase Amount (USD)'),
      yaxis = list(title = 'Age'),
      zaxis = list(title = 'Season')
    ),
    margin = list(l = 0, r = 0, b = 0, t = 0)
  ) %>%
  add_markers()

# K-means clustering with 3 clusters
kmean3 <- kmeans(shoppers, 3)
kmean3$centers


set.seed(1)
ggplot(shoppers, aes(x =Purchase.Amount..USD., y = Age)) + 
  geom_point(stat = "identity", aes(color = as.factor(kmean3$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2","3"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3")) +
  ggtitle("Segments of Shoppers", subtitle = "Using K-means Clustering")





# Add the cluster labels to the dataset
shoppers$Cluster3 <- as.factor(kmean3$cluster)


# Create a 3D scatter plot with different colors for 3 clusters
plot_ly(
  data = shoppers,
  x = ~Purchase.Amount..USD.,
  y = ~Age,
  z = ~Season,
  color = ~Cluster3,
  type = 'scatter3d',
  mode = 'markers',
  marker = list(size = 5),
  colors = c('#1f77b4', '#ff7f0e', '#2ca02c')  # Custom colors for 3 clusters
) %>% 
  layout(
    scene = list(
      xaxis = list(title = 'Purchase Amount (USD)'),
      yaxis = list(title = 'Age'),
      zaxis = list(title = 'Season')
    ),
    margin = list(l = 0, r = 0, b = 0, t = 0)
  ) %>%
  add_markers()
