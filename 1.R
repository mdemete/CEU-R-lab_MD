1+3
3*2
3^2

pi
pi *2
'pi'
letters
LETTERS


str(letters)
letters[1]
month.abb #abbreviated month names
month.name

?pi
apropos('month') #lists all R functions / constants including the subject

x = 4 #bad code style
x <- 4 #good
x / 2

## TODO square root of x
sqrt(x)
x^0.5

##define functions
f <- function(x) {
  2 * x + 1
  }
f(x)  
x <- c(1, 2, 3, 4, 5)
f(x)
class(x)
str(x)

plot(x, f(x), type = 'l')  #show lines instead of 5 discrete points

##plot 1 period of sin
f <- function(y) {
  sin(y)
}
y <- c(0,60,180)
f(y)
sin(y)
plot(y,  sin(y))
plot(y,  sin(x))
1:5
x<- seq(1,5, by = 0.1)
plot(x,  sin(x), type='l')
x<- seq(from = 0,
        to = 2*pi,
        by = 0.1)

curve(sin) #by default between 0 and 1
curve(sin, from = 0, to = 2*pi)
?curve


## TODO Brownian motion
x <- 0
# +1
# -1
runif(5, max=10)
round(runif(1))
x<- round(runif(10))*2-1
cumsum(x) #cummulative sum
plot(cumsum(x), type='s')

x<- sample(c(1, -1), 
           size = 25,
           replace = TRUE) #25 random picks between 2 numbers
?sample
plot(x)
cumsum(x)
plot(cumsum(x), type='s')


##vectors
h <- c(174, 170, 160)
w <- c(90, 80, 70)
plot(h, w, xlab = "Height", ylab = "Weight")
cor(h, w)
lm(w ~ h)  #build a linear model, cor was high so should work pretty well

# 165 * 1.346 -146.154 #model could be used to predict weight of student

fit <- lm(w ~ h) #store model in a variable

predict(fit,
        newdata = list(h=165)) #predicted values where we provide height as 165
?predict

## compute weights if height equals 56 or 104
predict(fit, newdata = list(h=c(56, 104))) #negative NOOOO

plot(h, w, xlab = "Height", ylab = "Weight", 
     xlim = c(0, 200), ylim= c(0,150))

abline(fit) #puts a linear trend line on the plot from the coefficients of our model
#model just extrapolated, built on incorrect range!!!

#DATAFRAMES
df<- data.frame(
  height = h, 
  weight = w)

str(df)
cor(df)
plot(df)
