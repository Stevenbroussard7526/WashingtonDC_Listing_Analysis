# Steven Broussard
# Team Final submission

#Loading necessary packages
library(tidyverse)
library(scales)
library(ggplot2) 

listingsData=read.csv("Listings.csv")
reviewsData=read.csv("Reviews.csv")

# Combining data to see if those with missing acceptance rate have no avg rating
listingsData = left_join(listingsData,reviewsData,
            by = c("id" = "listing_id"))

#Check and locate missing values 
anyNA(listingsData)
apply(listingsData,2,anyNA)

missing_listings_HAR=which(is.na(listingsData$host_acceptance_rate))
(missing_listings_HAR)


missing_listings_TP=which(is.na(listingsData$room_type))
missing_listings_TP

missing_listings_BDR=which(is.na(listingsData$bedrooms))
missing_listings_BDR

missing_listings_AVGR=which(is.na(listingsData$avg_rating))
missing_listings_AVGR


# Replacing missing host acceptance rate with mean and check if any missing values
listingsData$host_acceptance_rate[missing_listings_HAR]=mean(listingsData$host_acceptance_rate,na.rm = T)
anyNA(listingsData$host_acceptance_rate)
mean(listingsData$host_acceptance_rate)

# Replacing missing room type with phrase "Unknown" and check if any missing values
#REMOVE ALL 15 MISSING
listingsData$room_type[missing_listings_TP]="Unknown"
anyNA(listingsData$room_type)

# Replacing bedrooms with equivalent to beds and check if any missing values
listingsData$bedrooms[is.na(listingsData$bedrooms)]=listingsData$beds[is.na(listingsData$bedrooms)]
anyNA(listingsData$bedrooms)

# Replacing average rating with average ratings of the whole dataset and check if any missing values
listingsData$avg_rating[missing_listings_AVGR]=mean(listingsData$avg_rating,na.rm = T)
anyNA(listingsData$avg_rating)
mean(listingsData$avg_rating)

#Final check if any missing values for entire dataset
anyNA(listingsData)
apply(listingsData,2,anyNA)
str(listingsData)
#----------

#Q1: Report your exploratory analysis of the data. 
#This can include data visualization, summary tables, or any other insightful findings about the data
#open to finalize
 #price has a strong correlation with accomodates, bedrooms and beds and vice versa
 #host acceptance rate has closest correlation to total reviews
 #min_nights and total reviews have a negative correlation 
 #host_total_listins have negative correlation with total_reviews and avg_rating

library(scales)
ggplot(listingsData,aes(factor(neighborhood),host_acceptance_rate,fill=superhost),superhost )+
  stat_boxplot(geom="errorbar")+
  geom_boxplot(alpha=0.5,outlier.shape = NA)+
  coord_cartesian(ylim = c(0.65,1.0))+
  labs(x="Neighborhoods",y="Host Acceptance Rate",
       title="Comparison of Superhost vs. Non-Superhost & Their Acceptance Rates", 
       subtitle = "Broken down by Neighboorhood & Superhost Status")+
  scale_fill_manual(values=c("purple",
                             "lightgreen"))+
  scale_y_continuous(labels = label_number(accuracy = .1))+
  theme(plot.title=element_text(hjust = 0.5,
                                size=15, 
                                color = "darkblue"), 
        plot.subtitle= element_text(hjust = 0.5,
                                    size=10, 
                                    color = "darkblue"), 
        panel.spacing.x=unit(5,units="mm"))+
  theme(legend.position="bottom")


#Q2. Which combination of neighborhood and room type has the highest average price? 
##Which one has the lowest? Which combination has the highest variability? 
##Which combination has the lowest


Average_Price_Table=aggregate(price~neighborhood+room_type,listingsData,FUN=mean)  #table answer for highest and lowest


#final box plot answer for variability - with outliers
ggplot(listingsData,aes(factor(neighborhood),price,fill=room_type),stat_function(mean(price)))+
  stat_boxplot(geom="errorbar")+
  geom_boxplot(alpha=0.5)+
  labs(x="Neighborhoods",y="Avg price ($)",
       title="Average AirBnb daily rates with Outliers", 
       subtitle = "Broken down by Neighboorhood & Room Type")+
  scale_fill_manual(values=c("purple",
                             "green",
                             "red",
                             "gray"))+
  theme(plot.title=element_text(hjust = 0.5,
                                size=15,
                                color="darkblue"),
        plot.subtitle= element_text(hjust = 0.5,
                                    size=10,
                                    color="darkblue"),
        panel.spacing.x=unit(5,units="mm"))+
  theme(legend.position="bottom")


#final box plot answer for variability  - without outliers
ggplot(listingsData,aes(factor(neighborhood),price,fill=room_type),stat_function(mean(price)))+
  stat_boxplot(geom="errorbar")+
  geom_boxplot(alpha=0.5,outlier.shape = NA)+
  coord_cartesian(ylim = c(0, 450))+
  labs(x="Neighborhoods",y="Avg price ($)",
       title="Average AirBnb daily rates Without Outliers", 
       subtitle = "Broken down by Neighboorhood & Room Type")+
  scale_fill_manual(values=c("purple",
                             "green",
                             "red",
                             "gray"))+
  theme(plot.title=element_text(hjust = 0.5,
                                size=15,
                                color="darkblue"),
        plot.subtitle= element_text(hjust = 0.5,
                                    size=10,
                                    color="darkblue"),
        panel.spacing.x=unit(5,units="mm"))+
  theme(legend.position="bottom")


#Q3. Compute and report a 90% confidence interval for the proportion of superhosts in the
#population using the available sample. 
##Provide an interpretation of the computed confidence interval.

#interpretation: For Airbnb listings across 6 neighborhoods in the DC area, totaling 1,718 individual listings,
##we are 90% confident that the percent of super hosts is between 40.6% to 44.7%. 
n1 = length(listingsData$superhost)
n1

table(listingsData$superhost)

k = sum(listingsData$superhost ==T)
k

pbar=k/n1  #42% of the dataset are superhosts
pbar

se_superhost = sqrt(pbar* (1 - pbar)/(n1))
se_superhost

#Since there are two tails of the normal distribution, the 90% confidence level 
#would imply the 95th percentile of the normal distribution at the upper tail. 
#Therefore, zα∕2 is given by qnorm(.95). 
#Hence we multiply it with the standard error estimate SE and compute the margin of error.

Ep = qnorm(0.95)*(se_superhost)

pbar + c(-Ep, +Ep)

#Q4. Compute and report a 95% confidence interval for the average price of the 
#population using the available sample. Provide an interpretation of the computed confidence interval.

avg_price_95CI=
  listingsData %>% 
  summarize ( n=length(listingsData$price),
              mean_price=mean(price,na.rm=T),
              se_price= sd(price/sqrt(n)),
              Em=(qt(.975,df=(n-1))*se_price),
              lower_CI=mean_price-Em,
              higher_CI=mean_price+Em)

avg_price_95CI$lower_CI
avg_price_95CI$higher_CI
hist(listingsData$price,xlab="Price")


#Q5. Test whether the average rating of all listings in the population is more than 4 
#(at 95% level of confidence).

hypoTest=t.test(listingsData$avg_rating,mu=4,alternative="greater")
hypoTest #check the output
hypoTest$p.value < .05
#data:  listingsData$avg_rating
#t = 56.679, df = 1717, p-value < 2.2e-16
#alternative hypothesis: true mean is greater than 4
#95 percent confidence interval:
# 4.699772      Inf
#sample estimates:
# mean of x 
#4.720698 



#Q6. What are the top two highly correlated variables with price?

listingsData2 = data.frame(listingsData$id,listingsData$host_acceptance_rate,listingsData$host_total_listings,listingsData$accommodates,listingsData$bedrooms,listingsData$beds,listingsData$price,listingsData$min_nights,listingsData$tota)
listingsData3 = cor(listingsData2)
listingsData4 = data.frame(sort(listingsData3[,"listingsData.price"]))
colnames(listingsData4)[1] = "Price_Correlation"
filter(listingsData4,Price_Correlation>0.53 & Price_Correlation<0.6)

#Answer: number of accommodates and bedrooms have the highest correlation with price, at 0.587 and 0.537 respectively

#Q7. Visualize price to test for normality (this plot will be counted toward the 
#total number of plots noted below).
#take a look Q4 histogram, and see if other functions besids average price to determine price normality 
  ##plot mode and median so we can speak to them in the report 
  #can also try to visualize normality by price and neighborhood 

summary(listingsData$price)  #summary showing the median (171) and mean (204)
which.max(listingsData$price) #showing mode at 207

ggplot(listingsData, aes(price),xmin=0,ymin=0) +
  geom_histogram( binwidth=80, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  geom_vline(xintercept = mean(listingsData$price), 
             color = "red", linetype = "dashed",linewidth=0.75)+
  geom_label(aes(260, 450), label = "Mean", show.legend = FALSE,label.size=0.1)+
  geom_vline(xintercept = median(listingsData$price), 
             color = "black", linetype = "dashed",linewidth=0.75)+
  geom_label(aes(110, 350), label = "Median", show.legend = FALSE,label.size=0.1)+
  xlab("Avg. Price per listing")+
  ylab("Count of listings")+
  ggtitle("Distribution of Average Price")+
  theme_classic()



#Q8.Implement two meaningful regression models for the “price” of the listings. 
##Compare/present the results of the two models as a table. 
##Note: The point is not to build the best regression models in terms of fit. 
##The point is to simply build and compare two relevant regression models.

#model 1
Price_Accomodates= lm(price~accommodates,data=listingsData)
summary(Price_Accomodates)

#model 2
Price_Bedrooms= lm(price~bedrooms,data=listingsData)
summary(Price_Bedrooms)

model_name=c("Price_Accomodates","Price_Bedrooms")
res.se_val=c(summary(Price_Accomodates)$sigma,summary(Price_Bedrooms)$sigma)
rsq_val=c(summary(Price_Accomodates)$r.squared,summary(Price_Bedrooms)$r.squared)
a.rsq_val=c(summary(Price_Accomodates)$adj.r.squared,summary(Price_Bedrooms)$adj.r.squared)
comb_lm_table=data.frame(model_name,res.se_val,rsq_val,a.rsq_val)
comb_lm_table
