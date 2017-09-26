#2nd R class
h<- c(174, 170, 160)
w<- c(90, 80, 70)

df<- data.frame(height = h, weight = w)
df

## df[rows, columns]
df[1, 1]
df[3, 2]
df[1, ]
df[, 1]
df[c(1,2),]
df[1:2,]

str(df)
df$height
df$weight[2]

#TODO
#adds new columns (same as cbind?) directly into df
df$bmi <- df$weight / (df$height / 100)^2   #we need meters
df

summary(df$bmi)

#read a .csv file from the web
df <- read.csv('http://bit.ly/CEU-R-heights')   
str(df)
#factors: 1 for f, 0 for m

## convert inch -> cm; lb -> kg
df$height <-df$heightIn*2.54   #1 inch 2.54 cm
df$weight <-df$weightLb *0.45

df$bmi <- df$weight / (df$height / 100)^2   
str(df)

#opt 2
df$bmi <- (df$weightLb / (df$heightIn)^2) * 703

summary(df$bmi) #low values, due to age range
summary(df$ageYear)
min(df$bmi)
max(df$bmi)
range(df$bmi) #returns min and max as a vector #c(min(df$bmi), max(df$bmi))
diff(range(df$bmi))
sum(df$weight)
length(df$bmi)
nrow(df)
ncol(df)
dim(df) #dimensions of df

hist(df$bmi)
abline(v = c(18.5, 25), col= 'red' )   #v - vertical axis where to draw the line
?ablin ##fit <- lm(...); abline(fit) done prior week, add line to fitted model

plot(density(df$bmi))  #whatever returns from density fct, added to plot
boxplot(df$bmi)

#iqr interquartile range
boxplot(bmi ~ sex, df) #bmi col broken down by gender, specify dataset used
?boxplot

str(df)
#drop variables
#df$heightIn <- NULL

install.packages('beanplot')
library(beanplot)
beanplot(df$bmi)
beanplot(bmi~sex, df)   #check vioplot!
?beanplot

#Optionals
boxplot(
  rbeta(1e3, 0.1, 0.1),
  runif(1e3) * 2 -0.5,
  rnorm(1e3, 0.5, 0.75))

beanplot(
  rbeta(1e3, 0.1, 0.1),
  runif(1e3) * 2 -0.5,
  rnorm(1e3, 0.5, 0.75))

hist(rbeta(1e3, 0.1, 0.1))
hist(runif(1e3) * 2 -0.5)
hist(rnorm(1e3, 0.5, 0.75))
#-----END #Optionals-----

#compute freq table for pie chart
pie(table(df$sex)) #not useful in seeing differences
barplot(table(df$sex))
dotchart(table(df$sex))
dotchart(table(df$sex), xlim = c(0, 150))  #can see max and the ratios compared to 0

pairs(df)
#optional and SLOW
library(GGally)
#ggpairs help figuring out which representation makes sense
ggpairs(df)

library(pairsD3)
pairsD3(df)  #R obj, no JS needed. -> JSON -> JS


#library(h2o)  CHECK

## intro stats

#compare stats for 2 values
t.test(bmi ~ sex, df)  #difference is not significant, p -value 0.7
t.test(height ~ sex, df) #difference is bigger, p-val is pretty small
t.test(weight ~ sex, df) #2 kg, BMI diff not significant, p-val >5%

## ANOVA

aov(height ~ sex, data = df) #difference w/in and between groups
summary(aov(height ~ sex, data = df)) #return details
summary(aov(weight ~ sex, data = df))


## post hoc -> see actual diff between groups
TukeyHSD(aov(weight ~ sex, data = df))
#diff between males and females is 2 kg; lower / upper bounds given
TukeyHSD(aov(height ~ sex, data = df))




## intro data.table

## hotels.com - don't share
df<- read.csv('http://bit.ly/CEU-R-hotels-2017')
summary(df)
str(df)
head(df)
tail(df)
tail(df,3)
#factor hotel name< total obs. --> hotel name duplications / recurring in diff cities
#not blanks!!
#dist_other_km -> place from which we know the distance + dist -> parse

hist(df$price_HUF)
hotels <- df #rename since ugly on plot
#hist(df$price) #partial matching, considers price_HUF, DON'T
hist(hotels$price_HUF)

min(df$price_HUF)
max(df$price_HUF)
mean(df$price_HUF)
str(df$price_HUF)
summary(df$price_HUF)
#need index of rownumber
which.max((hotels$price_HUF))
hotels[1374,]
hotels[which.max((hotels$price_HUF)),]
which.min((hotels$price_HUF))
hotels[which.min((hotels$price_HUF)),] #cheapest hotel


##logical vector
hotels[hotels$price_HUF>100000,]

str(hotels$price_HUF>100000)
str(hotels[hotels$price_HUF > 100000, ])

str(which(hotels$price_HUF > 100000))
#opposite
str(which((hotels$price_HUF > 100000) == FALSE))

##which
pricey <- hotels[which(hotels$price_HUF > 100000), ]
###subset
?subset
subset(hotels, price_HUF > 100000)

## TODO filter pricey
pricey_and_low_rated<- pricey[which(pricey$rating < 4),]   #use NEW dataset, same variables!!
str(pricey_and_low_rated)
nrow(pricey_and_low_rated) #no.of these

hist(pricey_and_low_rated$rating) #bin width 0.5
#change width either by tweaking hist or by convert. to cat. var

#define cat variable
pricey_and_low_rated$rate_cat <-1
pricey_and_low_rated[rating > 1] # ->2
pricey_and_low_rated[rating > 2] # ->3
#manual, use function instead

?cut
pricey_and_low_rated$rate_cat <- cut(pricey_and_low_rated$rating, 4)
#creates 4 buckets
table(pricey_and_low_rated$rate_cat) #check frequency table
#or provide actual break points
pricey_and_low_rated$rate_cat <- cut(pricey_and_low_rated$rating, c(1, 2, 3, 4))
table(pricey_and_low_rated$rate_cat) #check frequency table
pie(table(pricey_and_low_rated$rate_cat))
dotchart(table(pricey_and_low_rated$rate_cat))

## TODO cut => price_cat (cheap < 10L, exp > 100K, avg), barplot / dotplot
hotels$price_cat <- cut(hotels$price_HUF, 3, dig.lab = 8) #change no. of significant digits -> scientif form
table(hotels$price_cat)

hotels$price_cat <- cut(hotels$price_HUF,
                        breaks = c(0, 10000, 100000, Inf),   #infinity, could use max var instead
                        labels = c('cheap', 'avg', 'pricey'), 
                        dig.lab = 8)
table(hotels$price_cat)
str(hotels)  #-> some values are NA

#---------------------------------
# data.table    SEE DATACAMP CHEATSHEET!!
#dplyr, tideverse
library(data.table)

hotels <- data.table(hotels)
str(hotels)

## filtering = > dt[row indexes]
hotels[price_HUF > 100000] #returns 1st 5 and last 5 rows
str(hotels[price_HUF > 100000])
hotels[price_HUF > 100000 & rating < 4] #and
hotels[price_HUF > 100000 | rating < 4] #or

## summaries => dt[, R expression]
hotels[, .N]  #returns no. of rows
nrow(hotels)
hotels[, mean(price_HUF)] 
mean(hotels$price_HUF)

# group by
hotels[, mean(price_HUF), by = stars] 
str(hotels)

hotels[, mean(price_HUF), by = stars] #V1 == ?!, table not ordered
hotels_by_stars <- hotels[, list(price_avg = mean(price_HUF), 
              price_min = min(price_HUF),
              price_max = max(price_HUF)), by = stars]  #provide a list of R expressions
hotels_by_stars
setorder(hotels_by_stars, stars, price_min) #orders in the background
hotels_by_stars

hotels_by_stars <- hotels[, list(price_avg = mean(price_HUF), 
                                 price_min = min(price_HUF),
                                 price_max = max(price_HUF)), by = stars][order(stars)]  #extra commands possible

## dt[i, j, bv]
## SELECT / UPDATE ... table WHERE .i. GROUP BY .by.  in SQL!

## TODO identify cheapest place with rating min 4

hotels[rating >= 4, min(price_HUF)]  #filters. does not give name
hotels[rating >= 4][order(price_HUF)] [1]

# new col: price_EUR 
# visualize this
hotels$price_EUR<- hotels$price_HUF/311.261855
hotels[, price_EUR := price_HUF / 311.261855] #data table specific col generation
hist(hotels$price_EUR)
hotels[, table(city)] #creates freq table within
hotels[, .N, by = city] #data table way. Counts no. of rows in buckets defined by city

cities_avg <- hotels[, list(avg_rating = mean(rating, na.rm = TRUE), 
                                 avg_stars = min(stars,  na.rm = TRUE),
                                 avg_price = max(price_HUF,  na.rm = TRUE)), by = city]
cities_avg
#even 1 NA ruins analysis
mean(c(1, 2, NA), na.rm = TRUE)

hotels[, mean(rating, na.rm = TRUE), by = city]

hotels_by_cities <- hotels[, list(
  avg_rating = round(mean(rating, na.rm = TRUE), 2), 
  avg_stars = round(mean(stars, na.rm = TRUE), 2),
  avg_price = round(mean(price_HUF,  na.rm = TRUE), 2)), 
  by = city]

hotels_by_cities

##TODO plot ditribution of prices
plot(hotels_by_cities$avg_stars, hotels_by_cities$avg_rating)

plot(hotels_by_cities$avg_price, hotels_by_cities$avg_stars)


## merge GDP <- wiki

##TODO percentage of highly rated hotels