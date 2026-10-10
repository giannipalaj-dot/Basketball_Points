install.packages("corrplot")
library(corrplot)
#(a) Load the data and give a five-number summary: minimum, Q1, median, Q3, and maximum.
basketball.df<-read.csv("Basketball.csv", header = TRUE)
View(basketball.df)

summary(basketball.df$Points)

#(b) Make a boxplot and a histogram of the points scored. Describe the shape, center, and spread of the distribution.
boxplot(basketball.df$Points, main = "Points Scored", col = "blue")
text(x = 1.2,
     y = fivenum(basketball.df$Points),
     labels = fivenum(basketball.df$Points),
     pos = 4)

hist(basketball.df$Points, xlab = "Points" , main = "Points Scored", col = "red")
#The data is skewed to the right with the center being around 10-15 points


#(c) Find the mean and the sample standard deviation of the points scored.
mean(basketball.df$Points)
sd(basketball.df$Points)
points.mean<-mean(basketball.df$Points)
points.sd<-sd(basketball.df$Points)  

#(d) Use the 6 sigma rule to find the lower and upper limits for an outlier. Which games, if any, are flagged as outliers?
lower.limit <- points.mean - 3 * points.sd
upper.limit <- points.mean + 3 * points.sd

lower.limit
upper.limit

outliers.6sigma <- basketball.df[
  basketball.df$Points < lower.limit |
    basketball.df$Points > upper.limit, ]

outliers.6sigma


#(e) Use the IQR method to find the lower and upper fences. Which games, if any, are flagged as outliers?
Q1 <- quantile(basketball.df$Points, 0.25, na.rm = TRUE)
Q3 <- quantile(basketball.df$Points, 0.75, na.rm = TRUE)

IQR.points <- IQR(basketball.df$Points, na.rm = TRUE)


lower.fence <- Q1 - 1.5 * IQR.points
upper.fence <- Q3 + 1.5 * IQR.points

lower.fence
upper.fence

outliers.IQR <- basketball.df[
  basketball.df$Points < lower.fence |
    basketball.df$Points > upper.fence, ]

outliers.IQR

#(f) Compare your answers to (d) and (e). Did the two methods agree? Explain your findings in the context of this problem.
#The two methods disagree, the 6 sigma method gave us no outliers and the IQR method gave us 1 outlier in Game 13 of 36 point. 
