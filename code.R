library(jsonlite)
yelpbusiness<-fromJSON("C:/DS/introduction to datascience/yelp/yelp_dataset/yelp_dataset~/business.json")

yelpbusines <- stream_in(file("C:/DS/introduction to datascience/yelp/yelp_dataset/yelp_dataset~/business.json"))
str(yelpbusines)
yelpbusines_flat <-flatten(yelpbusines)

#library(tibble)
#yelpbusines_flat_tbl <- as_data_frame(yelpbusines_flat)

#yelpbusines_flat_tbl

columnnames <- colnames(yelpbusines_flat)

columnnames
#review <- stream_in(file("C:/DS/introduction to datascience/yelp/yelp_dataset/yelp_dataset~/review.json"))
#str(review)
#review_flat <-flatten(yelpbusines)

columnnamesusing <- columnnames[1:20]
columnnamesusing

yelpbusines_flat_dataused<-yelpbusines_flat[columnnamesusing]
# subset data 

yelpbusines_flat_dataused 

# data on which we will work
str(yelpbusines_flat_dataused)
colnames(yelpbusines_flat_dataused)

# removing columns which has no relevance 
yfdn <- subset(yelpbusines_flat_dataused ,select = -c(1,3,13,14,15,16,17,18,19,20))
# after removing few more column which we wont be using 
yfdn
colnames(yfdn)

# checking Missing values in each column
sum(is.na(yfdn$name))
sum(is.na(yfdn$city))
sum(is.na(yfdn$state))
sum(is.na(yfdn$latitude))
sum(is.na(yfdn$longitude))
sum(is.na(yfdn$stars))
sum(is.na(yfdn$review_count))
sum(is.na(yfdn$is_open))
sum(is.na(yfdn$categories))


# Treating missing variables 
# rows reduced from 192609 to 192127

yfdn <- na.omit(yfdn)

categorycheck <-yfdn$categories 

strsplit(categorycheck,",")

head(categorycheck)


#since there are 1000 comma sepreated values which maybe similar to each other & differ by name so we cant implement 1 hot encoding .

#gsub(".*(_Restaurants_|_Food_|_Bakeries_).*","1",categorycheck)
#categorycheck[grep("_Restaurants_", categorycheck)] <- "1"
 #                gsub(".*(_scott80_|_harry11_).*", "incongruent", d)
#gsub(".*(_scott80_|_harry11_).*", "incongruent", categorycheck)

typeof(yfdn$categories)
# convertering category column to categorical values 

yfdn$categories[grepl("Restaurants",yfdn$categories)] <- "1"
yfdn$categories[grepl("Nightlife",yfdn$categories)] <- "1"
yfdn$categories[grepl("Food",yfdn$categories)] <- "1"
yfdn$categories[grepl("Bakeries",yfdn$categories)] <- "1"
yfdn$categories[grepl("Automotive",yfdn$categories)] <- "2"
yfdn$categories[grepl("Spa",yfdn$categories)] <- "3"
yfdn$categories[grepl("Salons",yfdn$categories)] <- "3"
yfdn$categories[grepl("Education",yfdn$categories)] <- "4"
yfdn$categories[grepl("Churches",yfdn$categories)] <- "4"
yfdn$categories[grepl("Religious",yfdn$categories)] <- "4"
yfdn$categories[grepl("Book",yfdn$categories)] <- "4"
yfdn$categories[grepl("Active Life",yfdn$categories)] <- "5"
yfdn$categories[grepl("Hotels",yfdn$categories)] <- "6"
yfdn$categories[grepl("Real Estate",yfdn$categories)] <- "6"
yfdn$categories[grepl("Shopping",yfdn$categories)] <- "7"
yfdn$categories[grepl("Entertainment",yfdn$categories)] <- "7"
yfdn$categories[grepl("Stores",yfdn$categories)] <- "7"
yfdn$categories[grepl("Services",yfdn$categories)] <- "8"
yfdn$categories[grepl("Pet",yfdn$categories)] <- "8"
yfdn$categories[grepl("Health & Medical",yfdn$categories)] <- "9"
yfdn$categories[grepl("Stations",yfdn$categories)]<- "10"
yfdn$categories[grepl("Media",yfdn$categories)]<- "10"
yfdn$categories[grepl("Public Art",yfdn$categories)]<- "10"
yfdn$categories[grepl("Local Flavor",yfdn$categories)]<- "10"

#1 restaurants /Nightlife/Food/Bakeries
#2 Automotive
#3 Spa/Salons
#4 Education/churches/Religious
#5 Active Life
#6 hotels/Real
#7 Shopping/Entertainment/Stores
#8 Services/Pet
#9 Health & Medical
#10 Public Art /Local Flavour


str(yfdn$categories)
sum(is.na(yfdn$categories))
as.numeric(yfdn$categories)
yfdn <- na.omit(yfdn) 
#$Converting data set into factor()
yfdn$is_open <- as.factor(yfdn$is_open)
yfdn$categories <- as.factor(yfdn$categories)
str(yfdn$is_open)
str(yfdn$categories) 

#Level 2 : EDA 

yfdn1 <-yfdn
library("ggplot2")

#ggplot(data=yfdn, aes(x=is_open, y=categories , fill=Bug)) + geom_bar(stat="identity")



#dotchart(yfdn$isopen ,)
# output not useful
p<-ggplot(yfdn, aes(x=city, y=categories)) + 
  geom_dotplot(binaxis='y', stackdir='center') 
p



## output not useful 
library("ggplot2")
ggplot(aes(x=city,y=categories),data=yfdn)+
  geom_point()

#analyse and create content(seems Useful)
ggplot(aes(y=state,x=stars),data=yfdn)+
  geom_point()
#there are many states in which people dont give  ratings or  yelp is not well established /
#or yelp should spend more promotion offers in these states
# data maybe biased


#Same as previous
ggplot(aes(state,stars),data=yfdn) +
  geom_line()

# box plots -Most of the ratings lies between 3-4.5 (for max states). also only 4 states shows 5 star service which means yelp work best in these states
boxplot(yfdn$stars~yfdn$state, xlab = 'states' ,ylab = 'stars',main = 'state Vs Stars', col= "Blue")

# Point 1 to covered as per edwin lo .summary of data set 
summary(yfdn)


# plot is openvs state gives no useful output
 
# star vs review_count
 boxplot(yfdn$stars~yfdn$review_count, xlab = 'review count' ,ylab = 'stars',main = 'stars Vs review_count', col= "blue" )
 
 #
 library("ggplot2")
 library("ggridges")


# review_count~stars  for higher number of review counts stars lies mostly between 2.5 -4.5
 plot(review_count~stars , data= yfdn)
 hist(review_count~stars , data= yfdn)
 typeof(yfdn$review_count)
 
 #  stars vs is_open , not useful
 boxplot(yfdn$stars~yfdn$is_open, xlab = 'xyz' ,ylab = 'stars',main = 'state Vs Stars', col= "yellow")
 
 # star vs categories Category 5 and category 8 are the best rated services in USA
 
 boxplot(yfdn$stars~yfdn$categories, xlab = 'xyz' ,ylab = 'stars',main = 'state Vs Stars', col= "yellow")

 # review_count vs category  Restaurants & hotel/Real state services are highly reviewd which may suggests that they have the highest market amongst these categories and people expect a lot from these businesses
 library("ggplot2")
 ggplot(aes(x=review_count,y=categories),data=yfdn)+
   geom_point()
 
 boxplot(yfdn$review_count~yfdn$categories, xlab = 'xyz' ,ylab = 'stars',main = 'state Vs Stars', col= "yellow")
 
 
 ## summary& descriptive statistics of data set Q1& Q2 as per edwin lo
 
 summary(yfdn) 
 
 # categorical data presentation " Top  AZ , ON ,PA are top 3 state, where people give maximun rating
 site.freq <- table(yfdn$state)
 barplot(site.freq , decreasing = T)
 barplot(site.freq[order(site.freq,decreasing = T)],space = c(7,7,7,7,7))
table(yfdn$state)
# top 3 catgories of yelp is
 

# 
# Restaurant > stores/entertainment > Pet/Services
sd(yfdn$city)
var(yfdn$city)
sd(yfdn$state)
var(yfdn$state)
sd(yfdn$postal_code)
var(yfdn$postal_code)
sd(yfdn$latitude)
var(yfdn$latitude)
sd(yfdn$longitude)
var(yfdn$longitude)
sd(yfdn$stars)
var(yfdn$stars)
sd(yfdn$review_count)
var(yfdn$review_count)
sd(yfdn$is_open)
var(yfdn$is_open)
sd(yfdn$categories)
var(yfdn$categories)


# top 5 rated business across us in terms of review_count 

yfdn_top5 <- yfdn[order(-yfdn$review_count),]
yfdn_top5 <- head(yfdn_top5)
  
 
library(RColorBrewer)
myPalette <- brewer.pal(6, "Set2") 

# You can change the border of each area with the classical parameters:
pie(yfdn_top5$review_count , labels = c("Mon Ami Gabi(8348)","Bacchanal Buffet(8339)","Wicked Spoon(6708)","Hash House A Go Go(5763)","Gordon Ramsay BurGR(5484)","	Earl of Sandwich(5075)"), border="White", col=myPalette )


# getting google APi keys 

if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)

#Load the library
library("ggmap")
library(ggplot2)
#Set your API Key
ggmap::register_google(key = "AIzaSyD7CbCCHcChmUSZFT_rZKq2mma0QE65uRU")

#Notes: If you get still have a failure then I suggest to restart R and run the library and register google commands again.
library(ggplot2)
p <- ggmap(get_googlemap(center = c(lon = 37.0902, lat = 95.7129),
                         zoom = 6, scale = 3,
                         maptype ='watercolor',
                         color = 'color'))
p + geom_point(aes(x = as.numeric(longitude), y =as.numeric(latitude) ,colour = stars ), data = yfdn, size = 0.5) + 
  theme(legend.position="bottom")

 typeof(yfdn$latitude)
# normality check
#qqnorm(yfdn$stars, main = "Normal Q-Q Plot",
#       xlab = "Theoretical Quantiles", ylab = "stars",
#       plot.it = TRUE, datax = FALSE)
#qqline(yfdn$stars)
# most of the reviews are rated as 3.5 to 4 left skewed
hist(yfdn$stars)


# most of the reviews are rated as 3.5 to 4 left skewed
# Calculate histogram, but do not draw it
my_hist=hist(yfdn$stars , breaks=40  , plot=F)

# Color vector
my_color= ifelse(my_hist$breaks < 2, rgb(0.2,0.8,0.5,0.5) , ifelse (my_hist$breaks > 3.5, "purple", rgb(0.2,0.2,0.2,0.2) ))

# Final plot
plot(my_hist, col=my_color , border=F , main="" , xlab="value of the variable", xlim=c(1,5) )



library(tidyverse)
library(hrbrthemes)




 #plot
#p <- yfdn %>%
#ggplot( aes(stars)) +
#geom_histogram( binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
#ggtitle("Bin size = 100") +
#theme_ipsum() +
#theme(
# plot.title = element_text(size=15)
 #)
#p


# Libraries
library(ggplot2)
library(hrbrthemes)



# Chart
p <- ggplot(yfdn, aes(x=x) ) +
  # Top
  geom_density( aes(x = review_count , y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=4.5, y=0.25, label="review_count"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = stars , y = -..density..), fill= "#404080") +
  geom_label( aes(x=4.5, y=-0.25, label="variable2"), color="#404080") +
  theme_ipsum() +
  xlab("value of x")

p




 
# statistical test

attach(yfdn)



aov1<-aov(yfdn$review_count~yfdn$categories)
names(aov1)
summary(aov1)

#Null hypothesis : reviews are equally distributed across each category. Since p value is low we will reject null hypothesis. and tcan say that
#review counts are not equally distributed across each categorycheck

t.test(yfdn$stars,yfdn$review_count )

# null Hypothesis : Review count for each ratings are equally distributed.
# AS p value is low , we will reject null hypothesis.
# this means review count across each star  rating are different

aov2<- aov(review_count~categories, data = yfdn )
names(aov2)
summary(aov2)

#Null hypothesis : review_count are equally distributed across categories
#P value is low so we will reject null hypothesis.
#which means review counts are not equally distributed across categories

chisq.test(yfdn$city , yfdn$categories, simulate.p.value = TRUE ,correct = TRUE)

#Null Hypotheis : all the categories are equally distributed in each city
#p value is low , so we will reject null hypothesis.
#which means : all the businesses are not equally distributed across cities

aov3 <- aov(stars~categories , data = yfdn)
names(aov3)
summary(aov3)

# Null hypothesis : ratings are equally distributed across business categories
# Since P value is low , we reject the null hypothesis 
# which means rating differs across categories


smart question : does prefrences is similar across US





