############ Analysis ################

#importing data set
library(readxl)
Air_France <- read_excel("~/Desktop/HULT /Courses /R/Excel Files/Air France Case Spreadsheet Supplement.xls", sheet = 2)
#View(Air_France)
summary(Air_France)
#renaming the columns
names(Air_France)<- c("Publisher_ID", "Publisher_Name", "Keyword_ID", "Keyword", "Match_Type",
                      "Campaign", "Keyword_Group", "Category", "Bid_Strategy", "Keyword_Type",
                      "Status", "Search_Engine_Bid", "Clicks", "Click_Charges", "Avg_Cos_per_Click",
                      "Impressions", "Engine_Click_Thru%", "Avg_Pos", "Trans_Conv%",
                      "Total_Cost_Trans", "Amount", "Total_Cost", "Total_Volume_Bookings")
# identifying missing values
is.na(Air_France)
#variables correlation 
install.packages("corrplot")
library(corrplot)
vars_corr_df<- Air_France[ ,c(12,13,14,15,16,17,18,19,20,21,22,23)]

cor_df<-cor(vars_corr_df)

corrplot(cor_df,method = "color")

#Creating Binary variable
Air_France$Binary<- c()

#Beginning of for loop to fill Binary column
for (i in 1:nrow(Air_France)) {
  if (Air_France$Total_Volume_Bookings[i]>=1){
    Air_France$Binary[i]<- 1
  } else {Air_France$Binary[i]<- 0}
}
#Closing for loop

#Function for normalizing
normalization_func<- function(variable){
  (variable - min(variable))/
    (max(variable) - min(variable))
}

#Normalizing variables
Air_France$Clicks_Normalized<- normalization_func(variable = Air_France$Clicks)
Air_France$Impressions_Normalized<- normalization_func(variable = Air_France$Impressions)
Air_France$Avg_Pos_Normalized<- normalization_func(variable = Air_France$Avg_Pos)
Air_France$Total_Cost_Normalized<- normalization_func(variable = Air_France$Total_Cost)


#Creating index for train and test sets
index_logit<- sample(1:nrow(Air_France), size=0.8*nrow(Air_France))

#Creating train and test sets
train_Air_France_logit<- Air_France[index_logit,]
test_Air_France_logit<- Air_France[-index_logit,]


#### Logistic regression ######
#After running the regression multiple times with different variables,
#the following variables were choose as they were the ones with 
#statistically significant P-values.
logit_Air_France<- glm(Binary ~ Clicks+Impressions+Avg_Pos+Total_Cost,
                       data=train_Air_France_logit,
                       family="binomial")

summary(logit_Air_France)


#Logit regression
logit_normalized_Air_France<- glm(Binary ~ Clicks_Normalized+Impressions_Normalized+
                                    Avg_Pos_Normalized+Total_Cost_Normalized,
                                  data=train_Air_France_logit,
                                  family="binomial")

summary(logit_normalized_Air_France)

###Confusion Matrix###
library(caret)
#prediction for test data
logit_prediction<- predict(logit_Air_France, test_Air_France_logit, 
                           type="response")
#confusion matrix for test data
confusionMatrix(data= as.factor(as.numeric(logit_prediction>0.5)),
                reference= as.factor(as.numeric(test_Air_France_logit$Binary)))

#prediction for train data
logit_prediction_2<- predict(logit_Air_France, train_Air_France_logit, 
                           type="response")
#confusion matrix for train data
confusionMatrix(data= as.factor(as.numeric(logit_prediction_2>0.5)),
                reference= as.factor(as.numeric(train_Air_France_logit$Binary)))
####Decision Tree ########
#importing libraries
library(rpart)
library(rpart.plot)

tree_af <- rpart(Binary~ Clicks+Impressions+Avg_Pos+Total_Cost,
                  data=train_Air_France_logit, method = "class",cp=0.0010)
rpart.plot( tree_af, type=1, extra=1,box.palette = "Blues")




#################################### Calculations ########################################

# Publishers name
publisher_name <- unique(Air_France$`Publisher_Name`)
print(publisher_name)

# Total sales by publishers
sales <- c()
for (i in 1:length(publisher_name)) {
  sales <- c(sales,sum(Air_France$Amount[which(Air_France[,2] == publisher_name[i])]))
  i <- i + 1
}

cbind(publisher_name,as.numeric(sales))

# Total cost by publishers
cost <- c()
for (i in 1:length(publisher_name)) {
  cost <- c(cost,sum(Air_France$`Total_Cost`[which(Air_France[,2] == publisher_name[i])]))
  i <- i + 1
}
print(cost)
cbind(publisher_name,as.numeric(cost))
#Overall Total Cost 
total_cost<- sum(cost)
total_cost

#Overall Total Cost year
total_cost_year<- total_cost *12
total_cost_year
#Overall Total Cost year %
percentage_total_cost <- ( total_cost_year/9400000000)*100
percentage_total_cost

#Calculating the profit
profit <- round((sales - cost),2)
sum(profit)
print(profit)
profit_pub<-as.data.frame(cbind(publisher_name, as.numeric(profit)))
names(profit_pub) <- c('Publisher_Name', "Profit")
class(profit_pub)
profit_pub

### Graphing the profit by each publisher###
library(ggplot2)
#plotting  bookings per publisher graph
profit_pub_graph<- ggplot(data = profit_pub, aes(x = Publisher_Name, y = Profit))+
  #geom_bar()
  geom_bar(stat = "identity", fill= "Midnight Blue", position = "dodge")  + labs(x="Publisher Name",
                                                                                 y= "Profit $",
                                                                                 title = "Profit per Publisher") + 
  theme(plot.title = element_text(hjust = 0.5))

#Tuning the visualization.
profit_pub_graph_complete<- profit_pub_graph + theme(
  # Remove panel border
  panel.border = element_blank(),  
  # Remove panel grid lines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Remove panel background
  panel.background = element_blank(),
  # Add axis line
  axis.line = element_line(colour = "grey")
)

profit_pub_graph_complete


# finding the most profitable publisher
max_sales<-max(sales)
publisher_name[which(sales == max_sales)]

#Return on Advertisement 
ROA <- as.numeric(round((sales/cost),2))
print(ROA)
cbind(publisher_name, as.numeric(ROA))
#creating a data frame to Plot the ROA
df_roa <- data.frame("Publisher_Name" = publisher_name, "ROA"=as.numeric(ROA))
names(df_roa)<- c("Publisher_Name", "ROA")


library(ggplot2)
#plotting  bookings per publisher graph
roa_pub_graph<- ggplot(data = df_roa, aes(x = Publisher_Name, y = ROA))+
  #geom_bar()
  geom_bar(stat = "identity", fill= "Dark Red", position = "dodge")  + labs(x="Publisher Name",
                                                                                 y= "ROAS",
                                                                                 title = "Return on Advertising spend") + 
  theme(plot.title = element_text(hjust = 0.5))

#Tuning the visualization.
roa_pub_graph_complete<- roa_pub_graph + theme(
  # Remove panel border
  panel.border = element_blank(),  
  # Remove panel grid lines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Remove panel background
  panel.background = element_blank(),
  # Add axis line
  axis.line = element_line(colour = "grey")
)

roa_pub_graph_complete


###sum of bookings per publisher function
sum_bookings_function<- function(publisher_vector, publisher_name){
  publisher_vector<-c()
  for (i in 1:nrow(Air_France)){
    if (Air_France$Publisher_Name[i] == publisher_name){
      publisher_vector[i]<- Air_France$Total_Volume_Bookings[i]
    } else {publisher_vector[i] <- 0}
  }#closing if statement
  
 sum_bookings<- sum(publisher_vector)
 return(sum_bookings)
}#closing  for loop

### sum of bookings for Publisher Google-US
google_us_bookings<-sum_bookings_function(publisher_vector = google_us_vector, publisher_name = "Google - US")
print(google_us_bookings)
#sum of bookings for Publisher Google-Global
google_global_bookings<-sum_bookings_function(publisher_vector = google_global_vector, publisher_name = "Google - Global")
print(google_global_bookings)
#sum of bookings for Publisher MSN - global
MSN_global_bookings<-sum_bookings_function(publisher_vector = MSN_global_vector, publisher_name = "MSN - Global")
print(MSN_global_bookings)
#sum of bookings for Publisher MSN - US
MSN_us_bookings<-sum_bookings_function(publisher_vector = MSN_us_vector, publisher_name = "MSN - US")
print(MSN_us_bookings)
#sum of bookings for Publisher Overture_Global
overture_global_bookings<-sum_bookings_function(publisher_vector = overture_global_vector, publisher_name = "Overture - Global")
print(overture_global_bookings)
#sum of bookings for Publisher Overture_US
overture_us_bookings<-sum_bookings_function(publisher_vector = overture_us_vector, publisher_name = "Overture - US")
print(overture_us_bookings)
#sum of bookings for Publisher Yahoo_US
yahoo_us_bookings<-sum_bookings_function(publisher_vector = yahoo_us_vector, publisher_name = "Yahoo - US")
print(yahoo_us_bookings)


# Sum of clicks function
sum_clicks_function<- function(publisher_vector, publisher_name){
  publisher_vector<-c()
  for (i in 1:nrow(Air_France)){
    if (Air_France$Publisher_Name[i] == publisher_name){
      publisher_vector[i]<- Air_France$Clicks[i]
    } else {publisher_vector[i] <- 0}
  }#closing if statement
  
  sum_clicks<- sum(publisher_vector)
  return(sum_clicks)
}#closing  for loop

#sum of clicks for Publisher Google-US
google_us_clicks<-sum_clicks_function(publisher_vector = google_us_vector, publisher_name = "Google - US")
print(google_us_clicks)
#sum of clicks for Publisher Google-Global
google_global_clicks<-sum_clicks_function(publisher_vector = google_global_vector, publisher_name = "Google - Global")
print(google_global_clicks)
#sum of clicks for Publisher MSN - global
MSN_global_clicks<-sum_clicks_function(publisher_vector = MSN_global_vector, publisher_name = "MSN - Global")
print(MSN_global_clicks)
#sum of clicks for Publisher MSN - US
MSN_us_clicks<-sum_clicks_function(publisher_vector = MSN_us_vector, publisher_name = "MSN - US")
print(MSN_us_clicks)
#sum of clicks for Publisher Overture_Global
overture_global_clicks<-sum_clicks_function(publisher_vector = overture_global_vector, publisher_name = "Overture - Global")
print(overture_global_clicks)
#sum of clicks for Publisher Overture_US
overture_us_clicks<-sum_clicks_function(publisher_vector = overture_us_vector, publisher_name = "Overture - US")
print(overture_us_clicks)
#sum of clicks for Publisher Yahoo_US
yahoo_us_clicks<-sum_clicks_function(publisher_vector = yahoo_us_vector, publisher_name = "Yahoo - US")
print(yahoo_us_clicks)


#creating dataframe for sum of bookings, publisher name and sum of clicks.
df_bookings_clicks_publisher_name <- data.frame(publisher_name=c("Google US", "Google Global","MSN Global", "MSN US", "Overture Global", "Overture US", "Yahoo US"),
                                         bookings=c(google_us_bookings,google_global_bookings,MSN_global_bookings,
                                                    MSN_us_bookings,overture_global_bookings,overture_us_bookings,
                                                    yahoo_us_bookings),
                                         clicks = c(google_us_clicks,google_global_clicks,MSN_global_clicks,MSN_us_clicks,
                                                    overture_global_clicks,overture_us_clicks,yahoo_us_clicks))
#checking the class object of 
class(df_bookings_clicks_publisher_name)


### Graphing the clicks and bookings the company has by each publisher###

#importing ggplot library
library(ggplot2)
#plotting  bookings per publisher graph
pub_book_graph<- ggplot(data = df_bookings_clicks_publisher_name,aes(x = publisher_name, y = bookings))+
  #geom_bar()
  geom_bar(stat = "identity", fill= "Midnight Blue", position = "dodge")  + labs(x="Publisher Name",
                                                                                 y= "Bookings",
                                                                                 title = "Bookings per Publisher") + 
                                                                            theme(plot.title = element_text(hjust = 0.5))
# Tuning the visualization 
pub_book_graph_complete<- pub_book_graph + theme(
  # Remove panel border
  panel.border = element_blank(),  
  # Remove panel grid lines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Remove panel background
  panel.background = element_blank(),
  # Add axis line
  axis.line = element_line(colour = "grey")
)

#calling the graph
pub_book_graph_complete

#plotting clicks per publisher name.
pub_clicks_graph <-ggplot(data = df_bookings_clicks_publisher_name,aes(x = publisher_name, y = clicks))+
  #geom_bar()
  geom_bar(stat = "identity", fill= "Dark Red", position = "dodge")  + labs(x="Publisher Name",
                                                                                 y= "Clicks",
                                                                                 title = "Clicks per Publisher") + 
                                                                      theme(plot.title = element_text(hjust = 0.5)) 
#Tuning the visualization.
pub_clicks_graph_complete<- pub_clicks_graph + theme(
  # Remove panel border
  panel.border = element_blank(),  
  # Remove panel grid lines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Remove panel background
  panel.background = element_blank(),
  # Add axis line
  axis.line = element_line(colour = "grey")
)

pub_clicks_graph_complete


##Plotting both graphs in one frame
#Installing package 
install.packages("cowplot")
#calling the library
library(cowplot)
#plotting both graphs in one frame
both_graph <- plot_grid(pub_clicks_graph_complete, pub_book_graph_complete, ncol = 2, labels = "AUTO") 
both_graph


##Average Click Through Rate
Air_France_2 <- Air_France
Air_France_2$new_engineclick<-gsub("NA", 0 , Air_France_2$`Engine_Click_Thru%`)


#mean of engine click through per publisher function
mean_clickthru_function<- function(publisher_vector, publisher_name){
  publisher_vector<-c()
  publisher_vector2<-c()
  for (i in 1:nrow(Air_France)){
    if (Air_France$Publisher_Name[i] == publisher_name){
      publisher_vector[i]<- (Air_France$`Engine_Click_Thru%`[i])
    } else {publisher_vector[i] <- 0}
  }#closing if statement
  
  pn_engine_click<-publisher_vector
  for(i in 1:nrow(Air_France)){
    if (publisher_vector[i]== 0){
      publisher_vector2[i]<- NA
    }else{publisher_vector2[i] <- publisher_vector[i]}
  }
  MEAN<- round(mean(as.numeric(publisher_vector2),na.rm = TRUE),2)
  return(MEAN)
}#closing  for loop

#average of click through rate for Publisher Google-US
google_us_average_click<-mean_clickthru_function(publisher_vector = google_us_vector, publisher_name = "Google - US")
print(google_us_average_click)
#average of click through rate for Publisher Google-Global
google_global_average_click<-mean_clickthru_function(publisher_vector = google_global_vector, publisher_name = "Google - Global")
print(google_global_average_click)
#average of click through rate for Publisher MSN - global
MSN_global_average_click<-mean_clickthru_function(publisher_vector = MSN_global_vector, publisher_name = "MSN - Global")
print(MSN_global_average_click)
#average of click through rate for Publisher MSN - US
MSN_us_average_click<-mean_clickthru_function(publisher_vector = MSN_us_vector, publisher_name = "MSN - US")
print(MSN_us_average_click)
#average of click through ratefor Publisher Overture_Global
overture_global_average_click<-mean_clickthru_function(publisher_vector = overture_global_vector, publisher_name = "Overture - Global")
print(overture_global_average_click)
#average of click through rate for Publisher Overture_US
overture_us_average_click<-mean_clickthru_function(publisher_vector = overture_us_vector, publisher_name = "Overture - US")
print(overture_us_average_click)
#average of click through rate for Publisher Yahoo_US
yahoo_us_average_click<-mean_clickthru_function(publisher_vector = yahoo_us_vector, publisher_name = "Yahoo - US")
print(yahoo_us_average_click)


#creating data frame for average click through rate.
df_average_clicksthru_publisher_name <- data.frame(publisher_name=c("Google US", "Google Global","MSN Global", "MSN US", "Overture Global", "Overture US", "Yahoo US"),
                                                click_thru=c(google_us_average_click,google_global_average_click,MSN_global_average_click,
                                                             MSN_us_average_click,overture_global_average_click,overture_us_average_click,
                                                             yahoo_us_average_click))
                                                


#plotting clicks per publisher name.
average_clicksthru_graph <-ggplot(data = df_average_clicksthru_publisher_name,aes(x = publisher_name, y = click_thru))+
  #geom_bar()
  geom_bar(stat = "identity", fill= "Dark Red", position = "dodge")  + labs(x="Publisher Name",
                                                                            y= "Average Click Through %",
                                                                            title = "Average Engine Click Through") + 
  theme(plot.title = element_text(hjust = 0.5)) 
#Tuning the visualization.
average_clicksthru_graph_complete<- average_clicksthru_graph + theme(
  # Remove panel border
  panel.border = element_blank(),  
  # Remove panel grid lines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Remove panel background
  panel.background = element_blank(),
  # Add axis line
  axis.line = element_line(colour = "grey")
)

average_clicksthru_graph_complete


#### Kayak Calculations ####
#Creating Vectors
Search_Engine <- c("Kayak")
Clicks <- c(2839)
Media_Cost <- c(3567.13)
Total_Booking <- c(208)
Avg_Ticket <- c(1123.53)
Total_Revenue <- c(233694)
Net_Revenue <- c(230126.87)

#Creating Kayak Data Frame
Kayak_df <- data.frame(Search_Engine, Clicks, Media_Cost, Total_Booking, 
                       Avg_Ticket, Total_Revenue, Net_Revenue)
#calculating the ROA
ROA_Kayak <- Total_Revenue/Media_Cost
print(ROA_Kayak)
ROA_mean<- mean(ROA)
print(ROA_mean)

