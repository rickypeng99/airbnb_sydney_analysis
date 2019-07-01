# Final Project for IS 457
# Class id: 91

#Part I: Data processing
data = read.csv("Airbnb Sydney.csv", header = TRUE,  sep=",")

# 1.1 Find missing values 

#vector of variables with missing values
missing = c()
count = 1
#Variables that have blanks
for(i in (1 : ncol(data))){
  col = data[, colnames(data)[i]]
  if(class(col) == "factor" & length(col[col == ""]) >= 1){
    print(colnames(data)[i])
    missing[count] = colnames(data)[i]
    count = count + 1
  }
}

#Variables that have NA
for(i in (1 : ncol(data))){
  col = data[, colnames(data)[i]]
  if(length(col[is.na(col)]) >= 1){
    print(colnames(data)[i])
    missing[count] = colnames(data)[i]
    count = count + 1
    
  }
}


#Variables that have N/A
for(i in (1 : ncol(data))){
  col = data[, colnames(data)[i]]
  if(class(col) == "factor" & length(col[col == "N/A"]) >= 1){
    print(colnames(data)[i])
    missing[count] = colnames(data)[i]
    count = count + 1
  }
}

#Variables that have - as missing values

for(i in (1 : ncol(data))){
  col = data[, colnames(data)[i]]
  if(class(col) == "factor" & length(col[col == "-"]) >= 1){
    print(colnames(data)[i])
    missing[count] = colnames(data)[i]
    count = count + 1
    
  }
}
#There isn't any variables containing "-" as missing value
#Totally, there are 14 variables with missing values.
not_missing = c()
count = 1
for(i in (1:length(colnames(data)))){
  if(!colnames(data)[i] %in% missing){
    not_missing[count] = colnames(data)[i]
    count = count + 1
  }
}

#The variables that don't have missing values are:
not_missing
#1.2 How to deal with missing values and why?

#Factor:
#For factors, there is no need to drop the blanks for decription and neighborhood review... etc. These are 
#not related to numerical data anaysis, unless we implement sentiment analysis to understand the positivity of the descriptions...etc.
#However we should drop the factor oberservations with N/A when we are dealing with the specific variables.
#For instance, if we want to deal with host_response_rate, we should delete the observations of N/A.

#Numerics:
#Number of missing values of numeric variables

length(which(is.na(data$bathrooms)))
length(which(is.na(data$bedrooms)))
length(which(is.na(data$review_scores_rating)))
length(which(is.na(data$review_scores_accuracy)))
length(which(is.na(data$review_scores_cleanliness)))
length(which(is.na(data$review_scores_checkin)))
length(which(is.na(data$review_scores_communication)))

#For numerics and integers, we should convert all na values to the median values. Such value might be 
#different from the actual value, but it is not bad for the overall distribution as the median is never an
#outliers. Also, as the data above showed, there are only one missing value for every numeric variables that has missing values.
#Therefore, comibing the missing values to median won't actually affect anything

#1.3 Effects on later data analysis

#Similar as above. Having the NA values to be converted to medians does no harm to the overall distribution, 
#considering the fact that there aren't too much numerical missing values. Also, each observation has several 
#numnerical features (such as rating, accuracy...etc.). Ditching all the data of these features for one missing 
#feature wouldn't be a great choice

#1.4 Dealing with missing values
#Making numerical NAs to be the median
for(i in (1 : ncol(data))){
  col = data[, colnames(data)[i]]
  if(class(col) != "numeric" & class(col) != "integer" ){
    next
  } else{
    col[is.na(col)] = median(na.omit(col))
    data[i] = col
  }
}

# 1.5 After dealing with missing values, show the dimensions of the data.
ncol(data)
nrow(data)
#it should be the same as we are not deleting any N/A entries here.

# 1.6 Comment on and explain any other data cleaning or preparation steps you think would be
# necessary from your inspection of the data (you do not need to carry them out).

#I believe that we should clean the data beased on what features we want to explore.
#Since there are many variables, and some observatins only had one missing variable, which won't affect 
#our analysis if we are analyzing other variables. Therefore, we shouldn't consider deleting any oberservation at the data exploration phase

#Some variables are supposed to have numerical values, such as the host_response_rate and price. We later should
#convert them to numerics instead of factors.

#2 Preliminary exploration - Exploring some variables comprehensively
#Summary of all varibales related to prices
#Removing the $ signs from prices
data$price = as.numeric(gsub("[$,]", "", data$price))
data$cleaning_fee = as.numeric(gsub("[$,]", "", data$cleaning_fee))
data$extra_people = as.numeric(gsub("[$,]", "", data$extra_people))
#prices
summary(data$price)
#It is unexpected that there are houses that are free to live
#cleaning fee
summary(data$cleaning_fee)
#a lot of airbnb actually doesn't have a cleaning fee policy, we should remove those if we want to do analysis regaring cleaning fee
#fee for extra people
summary(data$extra_people)

#price of listings with highest cleaning fee
head(data[order(-data$cleaning_fee),]$price)


#Summary of all ratings
rating_index = c(28, 29, 30, 31, 32, 33, 34)
for(i in (rating_index)){
  print(summary(data[i]))
}

#Summary of other numeric varibles
# # of bedrooms
summary(data$bedrooms)
# # of bathrooms
summary(data$bathrooms)
# # of minimum nights
summary(data$minimum_nights)
# # of beds
summary(data$beds)
# # of number of reviews
summary(data$number_of_reviews)
# # of reviews per month
summary(data$reviews_per_month)


#Explorations of other factors
#area that has the most amount of airbnbs
summary(data$city)[1]
#number of superhosts and non superhosts
summary(data$host_is_superhost)
#most popular room type
summary(data$room_type)[1]
#most popular property type
summary(data$property_type, maxsum = 2)[1]
#most popular bed type
summary(data$bed_type, maxsum = 2)[1]

#Mean and min rating for superhosts's airbnb and non-superhosts's airbnb
mean(data[data$host_is_superhost == "t", ]$review_scores_rating)
min(data[data$host_is_superhost == "t", ]$review_scores_rating)
mean(data[data$host_is_superhost == "f", ]$review_scores_rating)
min(data[data$host_is_superhost == "f", ]$review_scores_rating)
#This is interesting, although the average score of airbnb with superhost is quite similar to the 
#airbnbs that don't have a superhost, (97 vs 93). The minimum bar of superhost's airbnb is much better
#(80 vs 20). Therefore, it is mostly likely better to find airbnb with a superhost. 

#3 visulizations of varibles
#distribution of prices
barplot(table(data$price), ylab = "Frequency", xlab = "Price", main = "Distribution of prices")
weighted_density = density(data$price)
weighted_density$y = density(data$price)$y * (max(table(data$price)) / max(density(data$price)$y))
lines(weighted_density)
abline(v = median(data$price), col="red", lwd=3, lty=2)
#distribution of rating
barplot(table(data$review_scores_rating), ylab = "Frequency", xlab = "Rating", main = "Distribution of rating")

# minimum night distribution
barplot(table(data$minimum_nights), ylab = "Frequency", xlab = "Minimum nights", main = "Distribution of minimum nights")

# # of airbnbs in area of the city 
#we use bar chart to show top 5 cities and other cities
barplot(summary(data$city, maxsum = 5), ylab = "Frequency", main = "Distribution of top 5 cities and others")
#This means that there are a lot cities levels and even the top 5 cities only compose a very little amount of it

#distribution of host_repsonse_time
#remove N/A from host_reponse time
withoutNA = table(droplevels(data$host_response_time[data$host_response_time != "N/A"]))
barplot((withoutNA), ylab = "Frequency", xlab = "Host response time frequency", main = "Distribution of hosts's response time")
#Most of the hosts (had recorded responses) response within an hour

#distribution of cancellation policies
barplot(table(data$cancellation_policy), cex.names=.7, ylab = "Frequency", xlab = "Cancellation policies", main = "Distribution of hosts' cancellation policies")

#percentage of super and non-super hosts
#We use pie chart here because superhost or not is a binary variable, and a pie chart is pretty clear for it.
pie(table(data$host_is_superhost), labels = c("non-superhosts", "superhosts"), main = ("Distribution of superhosts and non-superhosts"))

#distribution of room_types, property types, bed types
plot(data$room_type)
plot(data$property_type)
plot(data$bed_type)

#4 Relationships 
#4.1 Review_per_month vs number_of_reviews
month = head(data[order(-data$reviews_per_month), ]$id, 100)
number = head(data[order(-data$number_of_reviews), ]$id, 100)

similarity = length(intersect(month, number)) / 100
similarity
#There isn't a huge overlap of host_ids between these two variables (only 22%). Meaning that only 
#22 top host_ids reach the top 100 for both variables. 
#The host_ids in the number_of_reviews has some ids with low-digits id, that means that older hosts'
#probably have more number of reviews because they had their listings for a longer amount of time. Whereas
#the review_per_month denotes the most recent popularity of a listing.

#In overall, the two variables are related if the hosted_since for all the listings are the same, since the information that 
#they convey are both indications of popularities. But since the hosted_ since isn't the same for all listings, there are some noises interrupting the relations. 

#4.2 Relationships of other three groups of variables
#number of reviews get as a superhost or not
boxplot(data[data$host_is_superhost == "t", ]$number_of_reviews, data[data$host_is_superhost == "f", ]$number_of_reviews, names = c("Superhosts", "non-superhosts"), main = "Summary of number of reviews of superhosts and non-superhosts")
#We can tell that being a superhost or not does affect the number of reviews. The interquatile range and the median of number of reviews of superhosts are all higher than those of non-superhosts.

#relationship between overall rating rating and # of reviews per month.
plot(data$reviews_per_month ~ data$review_scores_rating, ylab = "Review per month", xlab = "Overall rating score", main = "Relationship between review per monnth and overall rating score")
abline(lm(data$reviews_per_month ~ data$review_scores_rating), col = "orange")
#The linear regression of the relationship between the reviews per month and the overall rating does have a weak postive correalation. 
#Also the maximum value of review per month of each level of overall rating is steadily increasing 

#boxplot of comparison between reply time and number of reviews
plot(data$number_of_reviews ~data$host_response_time, ylab = "Rating", main = "Relationship betwee reply time and number of reviews")
#we can tell that the hosts that reply faster typically have a higher median of number of reviews. Alos, their interqutile range is higher.

#5. Hypothesis

#5.1 Being a superhost or not will affect the overall rating of the listing and the number of reviews. 
#I will explore the relationship between number of reviews and overall rating for each category of being a superhost or not.
#From the explorations, I have discovered the distribution of superhosts or not and I discovered the relationship between superhosts and number of reviews to be a postive correlation

#5.2 Having a cleaning fee or not will affect the number of reviews.
#I will divide the listings into the listings with cleaning fee or not. And explore the relationship between amount of cleaning fee and the overall rating. 
#I have discovered the distribution of cleaning fee. And i noticed that the price of highest cleaning fee listings are overwhelmingly high, which probably have lower number of reviews since people won't want to go there.

#5.3 Different bed types will affect the overall rating of the listing.
#I will explore the relationship between overalll rating of the listing for each category of bed_types.
#From the explorations I have discovered the distributions of different bed types, and the most popular bed type. I figure that people would have preferences over bed_types as the distribution of bed_types is vastly different.


#II. Data analysis
#6.1 overall rating vs # of reviews for all property types
library("lattice")
xyplot(data$review_scores_rating ~ data$number_of_reviews | data$property_type, data = data,
       main = "overall rating vs # of reviews for all property types", xlab = "Number of reviews",
       ylab = "overall rating")
#This graph plotted the ralationship between number of reviews and overall ratings for all types of property types. we can see that for different property types, there isn't a correlation between number of reviews and overall ratings, which means that the different propertypes probably won't affect the overall rating and number of reviews of a listing

#6.2 Relationship betweem multiple variables of property types, room types, bed types and reviews per month.
library("ggplot2")
# # of listings for all categories:
propertyTypes = unique(data$property_type)
sumArray = rep(0, length(propertyTypes))
names(sumArray) = propertyTypes
for(i in (1 : length(data$property_type))){
  for(j in (1 : length(propertyTypes))){
    if(data$property_type[i] == propertyTypes[j]){
      sumArray[j] = sumArray[j] + 1
      break
    }
      
  }
}

sumArray = sort(sumArray, decreasing = TRUE)
top_10_property = head(names(sumArray), 10)
#clean data with only top 10 properties
top_10_prop_data = data[data$property_type %in% top_10_property,]

#dropping used levels
top_10_prop_data$property_type = droplevels(top_10_prop_data$property_type)
levels(top_10_prop_data$property_type)
# ggplot(data = top_10_prop_data, aes(x=top_10_prop_data$property_type, y=top_10_prop_data$reviews_per_month, fill = top_10_prop_data$bed_type)) + geom_bar(stat = "identity", position = "dodge") + facet_grid(top_10_prop_data$room_type~., scale="free_x")
#Plotting the graph
require("ggrepel")
ggplot(top_10_prop_data, aes(x=top_10_prop_data$property_type, y=top_10_prop_data$reviews_per_month, color = top_10_prop_data$bed_type, group = top_10_prop_data$bed_type)) +
  stat_summary(fun.y=mean, geom = "point") +
  stat_summary(fun.y=mean, geom = "line")  + facet_grid(top_10_prop_data$room_type~., scale="free_x") +ggtitle("Relationship between property types, room types, bed types and reviews per month") + xlab("property types") +ylab("Average reviews per month")+labs(color = "Bed types")

#In this graph, since we are finding the relationship among four varibles, we can tell that Cottages with entire-room and couch has the most amount of average reviews per month. We could also tell that entire home usually have more reviews per month, the popularity of shared rooms is very low.

#6.3 plots for hypothesis in q5

#6.3.1 Being a superhost or not will affect the overall rating of the listing and the number of reviews. 
host_superhost = data$host_is_superhost
levels(host_superhost) = c("Non-superhosts", "Superhosts")
xyplot(data$review_scores_rating ~ data$number_of_reviews | host_superhost, data = data,
       main = "overall rating vs # of reviews for all property types", xlab = "Number of reviews",
       ylab = "overall rating", panel = function(x, y){
         panel.xyplot(x, y)
         panel.lmline(x, y)
         
       })
#I chose scatter plot with regreesion line, because we are discovering the relationship between two numeric varibales of each category of being a superhost or not. The scatterplot includes all pairs of datas of overall rating and number of reviews. 
#For superhosts, the minimum rating is much lower. In terms of the regression line, we can see that the rating of superhosts' listing is higher than the rating of non-superhosts' rating for every level of number of reviews. Which means that superhosts generally get more reviews and higher ratings. 

#6.3.2 Having a cleaning fee or not will affect the number of reviews
having_cleaning = data$cleaning_fee
having_cleaning[!is.na(data$cleaning_fee)] = "Having cleaning fee"
having_cleaning[is.na(data$cleaning_fee)] = "Not having cleaning fee"
data = as.data.frame(cbind(data, having_cleaning))
ggplot(data[!is.na(data$cleaning_fee),], aes(x = data$cleaning_fee[!is.na(data$cleaning_fee)], y = data$number_of_reviews[!is.na(data$cleaning_fee)])) + geom_point() + geom_smooth(method='lm',formula=y~x) + ggtitle("Relationship between cleaning fees and number of reviews") +ylab("Number of reviews") + xlab("Cleaning fee")
#I chose this plot because there are two numeric variables to relate, so from the regression line, I can tell that once the listing has a cleaning fee, the higher the fee, the less of the number of reviews we generally get.

#6.3.3 Different room types will affect the overall rating of the listing
plot(data$review_scores_rating ~ data$bed_type ,
       main = "overall rating vs # of reviews for all room types", xlab = "Bed types",
       ylab = "overall rating")

#I chose box plot because there are three groups and one numeric value, we can see that all the other beds except airbed have higher medians and interquatile range, significantly than the air beds. also the Real bed has the highest median. From these explorations we can know that people do have a preference over bed types.

#7 data manipulation
#7.1 remove $ in prices and convert to numerics
data$price = as.numeric(gsub("[$,]", "", data$price))
#7.2 number of amenities column
num_amenities = rep(0, length(data$amenities))
for(i in (1 : length(data$amenities))){
  amenity = data$amenities[i]
  amenity = gsub("[{}]", "", amenity)
  amenity = strsplit(amenity, ",")
  num_amenities[i] = length(amenity[[1]])
  
}

data = as.data.frame(cbind(data, num_amenities))
#7.3 
tapply(data$review_scores_rating, data$cancellation_policy, mean)
#strict cancellation policies generally have lower rating

#7.4 more manipulations
#add a column of numers of verifications to the data frame
num_verifications = rep(0, length(data$host_verifications))
for(i in (1 : length(data$host_verifications))){
  verify = data$host_verifications[i]
  verify = gsub("[\\[\\]]", "", verify)
  verify = strsplit(verify, ",")
  num_verifications[i] = length(verify[[1]])
  
}
data = as.data.frame(cbind(data, num_verifications))

#8 fit simple linear model
#8.1 review_per_month vs number_of_reviews
#I would choose number_of_reviews. Because it means that how many people have visitied this lisiting, 
#also it is more attractive to new customers, as higher number of reviews means that more people have came
#and the listing might be more safe in some sense.

#Therefore there are several candidate variables that affect the choice of primary indicator
#1. city, position might affect popularity
#2. reviw_scores_rating, higher score, more popularity
#3. room_type people might prefer private room
#4. host_since, older listing might be more popular
#5. cleaning_fee, people might prefer rooms with lower cleaning fee
#6. cancellation_policy, people might prefer better cancellation policy
#7. host_is_superhost, superhost might be having more popularity
#8. accomodates, people might prefer to bring more people with
#9. bathrooms, more bathrooms might be more popular
#10. minimum nights, shorter minimum nights might be more popular

#I choose the overall ratings, as the higher the rating, the more likely it become really popular.

model = lm(data$number_of_reviews ~ data$review_scores_rating, data = data)
summary(model)
plot(model)
err = summary(model)$coefficients[2,2]
beta = model$coefficients[2]
c(beta-1.96*err,beta+1.96*err)
#it is statisically significant, as the 95% confidence interval doesn't contain 0, which means that review_scores_rating
#has an effct on number_of_reviews.

#The model seems far from the margin of scatterplot, also, the residual are far from the zero lines. The standardised
#residual also doesn't lie on the line, denoting that the random errors are not from the theoritical distributions.
#Hoever, all the data points are far from the cook's distance, denoting that there aren't too much influential points.Therefore, I wouldn't say that this model is a great model to suit the data.



#Part III Further analysis

#9.1 
#superhost or not vs host_response_rate
host_response_rate_super = data$host_response_rate[data$host_response_rate != "N/A" & data$host_is_superhost == "t"]
host_response_rate_not = data$host_response_rate[data$host_response_rate != "N/A" & data$host_is_superhost == "f"]
host_response_rate_super = as.numeric(as.character(gsub("%", "", host_response_rate_super)))
host_response_rate_not = as.numeric(as.character(gsub("%", "", host_response_rate_not)))
mean(host_response_rate_super)
mean(host_response_rate_not)
#being a superhost has a higher average response rate.

#superhost or not vs host since 
host_since =as.numeric((sapply(data$host_since, function(x){
  return(strsplit(as.character(x), "/")[[1]][3])
})))
superhost = data$host_is_superhost
levels(superhost) = c("non-superhosts", "superhosts")
tapply(host_since, superhost, summary)
boxplot(host_since ~ superhost, main = "Relationship between host_since and being a superhost or not", ylab = "year")
#being a superhost or not doesn't really have any effect on host_since

#superhost or not vs host_verification
#comparing superhost or not against the number of verifications
num_verifications = rep(0, length(data$host_verifications))
for(i in (1 : length(data$host_verifications))){
  verify = data$host_verifications[i]
  verify = gsub("[\\[\\]]", "", verify)
  verify = strsplit(verify, ",")
  num_verifications[i] = length(verify[[1]])
  
}
superhost = data$host_is_superhost
levels(superhost) = c("non-superhosts", "superhosts")
tapply(num_verifications, superhost, summary)
boxplot(num_verifications ~ superhost, main = "Relationship between being a superhost or not and number of verification methods", ylab = "number of verification methods")
#being a superhost or not doesn't really have any effect on host_verifications

#superhost or not vs host_identity_verified
verified = as.vector(data$host_identity_verified)
superhost = as.vector(data$host_is_superhost)
verified[verified == "f"] = "not verified"
verified[verified == "t"] = "verified"

superhost[superhost == "f"] = "non-superhost"
superhost[superhost == "t"] = "superhost"
require(ggmosaic)

ggplot() + geom_mosaic(aes(x = product(verified, superhost), fill = verified)) + xlab("Superhost or not") +ylab("Verified or not") +ggtitle("mosaic plot of verified or not for each host being superhost or not")
#more superhosts have their identities verified.

#superhost or not vs host_response_time (analyzed with mosaic plot in 9.2)
#9.2 mosaic plot of host_response_time vs host_is_superhost
#removing N/A in host_response_time
require(ggmosaic)
time = as.vector(data$host_response_time[data$host_response_time != "N/A"])
superhost = as.vector(data$host_is_superhost[data$host_response_time != "N/A"])
ggplot(data = data[data$host_response_time != "N/A", ]) + geom_mosaic(aes(x = product(time, superhost), fill = time))+ xlab("Superhost or not") +ylab("Response time interval") +ggtitle("mosaic plot of response time for each host being superhost or not")
#superhosts generally reply faster, the percentage of superhosts who reply within an hour is higher than non-superhosts

#10
#import stop words
stop_word = "a, able, about, across, after, all, almost, also, am, among, an, and, any, are, as,
              at, be, because, been, but, by, can, cannot, could, dear, did, do, does, either, else, ever, every, for,
              from, get, got, had, has, have, he, her, hers, him, his, how, however, i, if, in, into, is, it, its, just,
              least, let, like, likely, may, me, might, most, must, my, neither, no, nor, not, of, off, often, on,
              only, or, other, our, own, rather, said, say, says, she, should, since, so, some, than, that, the, their,
              them, then, there, these, they, this, is, to, too, was, us, wants, was, we, were, what, when, where,
              which, while, who, whom, why, will, with, would, yet, you, your"
stop_word = strsplit(gsub(" ", "", stop_word), ",")
all_description = gsub("-", " ", data$description)
all_description = gsub("[^[:alpha:] ]", "", all_description)
all_description = tolower(all_description)
all_description = strsplit(all_description, " ")
#splitedDescription = all_description
all_description = unlist(all_description)
all_description = all_description[all_description != ""]
all_description = all_description[!all_description %in% gsub("\n", "",(stop_word[[1]]))]
#all words splited
temp = all_description
#unique words
all_description = unique(all_description)



#calculating word frequncy
temp.freq = table(temp)
class(temp.freq)
class(as.integer(temp.freq))
df = as.data.frame(cbind(names(temp.freq), as.integer(temp.freq)))
names(df) = c("Word", "freq")
df$freq = as.numeric(as.character(df$freq))
df = df[order(-(df$freq)),]
head(df)



#word frequency function
wfreq = function(word, text){
  text = tolower(text)
  text = gsub("-", " ", text)
  text = gsub("[^[:alpha:] ]", "", text)
  temp = strsplit(text, " ")
  temp = unlist(temp)
  count = 0
  if(length(temp) >= 1){
    for(i in (1 : length(temp))){
      if(temp[i] == word){
        count = count + 1
      }
    }
  }
  return(count)
}

#beach & beaches
#Average price of listings with descriptions containing substring "beach" ("beaches" as well)

booleanArray = sapply(data$description, function(x){
  return(wfreq("beach", x) > 0 | wfreq("beaches", x) > 0)
})

withBeach = mean(data[booleanArray, ]$price)
withBeach
#no beach
withoutBeach = mean(data[!booleanArray, ]$price)
withoutBeach
#The difference is 
withBeach - withoutBeach

#3 other words
#contains "quiet"
booleanArray = sapply(data$description, function(x){
  return(wfreq("quiet", x) > 0)
})
withQuiet = mean(data[booleanArray, ]$price)
withoutQuiet = mean(data[!booleanArray, ]$price)
withQuiet
withoutQuiet
#Quieter places usually mean that the house is not at a popular area, thus the price is usually lower

#contains "large"
booleanArray = sapply(data$description, function(x){
  return(wfreq("large", x) > 0)
})
withLarge = mean(data[booleanArray, ]$price)
withoutLarge = mean(data[!booleanArray, ]$price)
withLarge
withoutLarge
#larger room often has more price, as the original house price is usually higher

#contains "garden"
booleanArray = sapply(data$description, function(x){
  return(wfreq("garden", x) > 0)
})
withGarden = mean(data[booleanArray, ]$price)
withoutGarden = mean(data[!booleanArray, ]$price)
withGarden
withoutGarden
#listing with garden has more price, as the original house price is usually higher

#10.2
#(1) top 100 zipcode
zipcode = data$zipcode[data$zipcode != ""]
top100zip = names(summary(zipcode, maxsum = 200))
weightedMean = function(location){
  total_rating =weighted.mean(data[data$zipcode == location, ]$review_scores_rating, data[data$zipcode == location, ]$number_of_reviews)
  return(total_rating)
}
top100zipmean = sapply(top100zip, function(x){
  return(weightedMean(x))
})  
plot(top100zipmean ~ summary(zipcode, maxsum = 200), ylab = "Weighted mean", xlab = "Number of listings")
#the location (being popular or not for listings) doesn't have any relationship with the rating

#(2) two other aspects from descriptions 
# the word "private"
booleanArray = sapply(data$description, function(x){
  return(wfreq("private", x) > 0)
})
withPrivate = weighted.mean(data[booleanArray, ]$review_scores_rating , data[booleanArray, ]$number_of_reviews)
withoutPrivate = weighted.mean(data[!booleanArray, ]$review_scores_rating , data[!booleanArray, ]$number_of_reviews)
withPrivate
withoutPrivate
#Private could affect the weighted mean, which means that we should look at the room types 
weightedMean = function(type){
  total_rating =weighted.mean(data[data$room_type == type, ]$review_scores_rating, data[data$room_type == type, ]$number_of_reviews)
  return(total_rating)
}
room_types = levels(data$room_type)


roomtypemean = sapply(room_types, function(x){
  return(weightedMean(x))
})  

plot(roomtypemean ~ seq(1:3), main = "Relationship between average rating of different room types", ylab = "Weighted mean of ratings", xlab = "index of room types")
lines(seq(1:3), y = roomtypemean, type = "l")

#Weighted mean of ratings of differnet room types
entire_apt_weighted_mean = weightedMean(room_types[1])
entire_apt_weighted_mean
private_weighted_mean = weightedMean(room_types[2])
private_weighted_mean
shared_weighted_mean = weightedMean(room_types[3])
shared_weighted_mean
#We can tell that that the weighted mean of ratings of private spaces (entire apt and private room) are much higher. 

#the word space
booleanArray = sapply(data$description, function(x){
  return(wfreq("parking", x) > 0)
})
with = weighted.mean(data[booleanArray, ]$review_scores_rating , data[booleanArray, ]$number_of_reviews)
without = weighted.mean(data[!booleanArray, ]$review_scores_rating , data[!booleanArray, ]$number_of_reviews)
with
without
#We can see that if the description contains the word "parking", the weighted mean has a slight increment.
#we can therefore look at ratings of houses with "paking" as amenties and house that aren't

has_parking = function(amenties){
  return(length(grep("parking", amenties)) >= 1)
}
booleanArray = sapply(data$amenities, function(x){
  x = gsub("[{}]", "", x)
  x = strsplit(x, ",")
  return(has_parking(x))
})
has_parking = data[booleanArray,]
no_parking = data[!booleanArray,]
weighted_mean_parking = c(weighted.mean(has_parking$review_scores_rating, has_parking$number_of_reviews), weighted.mean(no_parking$review_scores_rating, no_parking$number_of_reviews))
plot(weighted_mean_parking~ seq(1:2), main = "Comparison between weighted mean of having parking or not", xlab = "index of having parking(1 means having, 2 means no)", ylab = "Weighted mean")
lines(seq(1:2), y = weighted_mean_parking, type = "l")
#The listings that have parking lots as one of the amentities have higher weighted mean.

#IV More analysis on customer end
#Define a rating of "Rating - Price ratio", we will find where to live have higher Rating - Price ratio.
#Make a column of Rating - Price ratio to the dataframe

rating_price_ratio = c()
for(i in (1 : nrow(data))){
  price = data$price[i]
  rating = data$review_scores_rating[i]
  if(price == 0){
    price = mean(data$price)
    #print(price)
  }
  ratio = rating / price
  if(ratio == Inf){
    print(price)
    print(rating)
  }
    
  rating_price_ratio[i] = ratio
  
}
data = as.data.frame(cbind(data, rating_price_ratio))

#We could explore on what zipcodes averagely have higher rating-price ratio
plot(data$rating_price_ratio ~ data$host_is_superhost)

zipcode = data$zipcode[data$zipcode != ""]
allzip = names(summary(zipcode, maxsum = length(zipcode)))
allzip_mean = sapply(allzip, function(x){
  return(mean(data[data$zipcode == x,]$rating_price_ratio))
})  

allzip_mean[order(-allzip_mean)[1:5]]
#Living in 2146, 2173, 2151, 2119, 2212 zipcodes usually have the best experiences.


#V. Conclusion
#In overall, we have explored a lot of variables from this dataset, below are the suggestions that we can make based on the analysis
#For business end:
#Being a superhost generally can boost your number of reviews, which is popularity, and the overall rating score.
#Canceling or lowering the cleaning fee can generally earn you higher overall ratings.
#Hosting private spaces (entire room or private rooms) can earn you higher ratings and more popularity than shared rooms
#Having real bed, pull out sofa, futon and couch can be better than having an airbed. People like the real beds the most. so it is the best to not be using other beds by real beds for your overall rating.
#Hosting listings with parking lots can earn you higher ratings
#You can demand higher prices if your listing is near the beach, or it is large, or has a garden, or at a popular (noisy) area
#Your good location doesn't necessarily mean the higher rating, the location has no correlation with its weighted average mean.
#Strict cancellation policy can give you lower ratings
#Replying faster can boost your popularity
#It is also better to have yourself verified, as more of the superhosts verify themselves and they averagely having higher ratings and higher numbers of reviews.

#For Customer end:
#Superhosts usually reply faster
#it is better living in zipcodes of 2146, 2173, 2151, 2119 and 2212

#VI.Exaplanations of data science life cycles (for my project).
# 1. Data Processing
#During this phase, we comprehensively explore all the attributes of different variables, such that we observe the missing values, we observe the general distribution of different varibales (single feature) and we deal with the missing values accordingly. We also observe the potential trend and relationship between two obvious variables (direct relations), such that we have a general idea of how variables are related with each other, and how the data looks like in general.
#We should also analyze how variabels could potentially affect our later analysis.
# 2. Basic data analysis
#During this phase, we firstly define several hypothesis that gives us objectives to discover possible business insights, since we have done some baisc explorations. We then further explore the relationship between multiple varibales, which will give us a better overall idea of the data, and how several variables might affect our business sugeestions. 
# 3. Data manipulation
#During this phase, we started to notice that in order to do a more comprehensive analysis, it is necessary to manipulate more of the data such that it can be better utilized for analysis. For instance, we can transform the amenities to the number of amenities, such that we could explore the relationship between other variables and the number of amenities. 
#4. Further analysis
#During this phase, we could do more advanced and detailed analysis since we have the transformed dataset which can be better utilized. For instance, we can go into the details of description by noticing how the words in descrition could possibly affect other variables. We can also make predictions of how the ratings will go if we increase the cleaning price based on the trends that we discovered.
#5 Make conclusions
#We should make conclusions and provide suggestions based on the trends and calculations that we discover.

