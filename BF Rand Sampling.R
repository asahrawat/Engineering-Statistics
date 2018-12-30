


bodyfat_df <- data.frame(BodyFat)
names(bodyfat_df)

#Question 2
equal40_subset <- subset(bodyfat_df, AGE == 40)
mean(equal40_subset$BODYFAT)
# Avg Bodyfat: 16.324
mean(equal40_subset$AGE)
# Avg Age: 40
mean(equal40_subset$DENSITY)
# Avg Density: 1.062
mean(equal40_subset$WEIGHT)
# Avg Weight: 164.471
mean(equal40_subset$HEIGHT)
# Avg Height: 70.235

#Question 3
quantile(bodyfat_df$AGE)
# 25th percentile: 35.75; 50th percentile: 43.00; 75th percentile: 54.00
# Maximum: 81.00
age_bins <- c(35.75, 43.00, 54.00)
first_quart <- subset(bodyfat_df, AGE <= 35.75)
second_quart <- subset(bodyfat_df, AGE <= 43.00)
second_quart_b <- subset(second_quart, AGE > 35.75)
third_quart <- subset(bodyfat_df, AGE <= 54.00)
third_quart_b <- subset(third_quart, AGE > 43.00)
fourth_quart <- subset(bodyfat_df, AGE > 54.00)

mean(first_quart$BODYFAT)
mean(first_quart$AGE)
mean(first_quart$DENSITY)
mean(first_quart$WEIGHT)
mean(first_quart$HEIGHT)
# Avg Bodyfat: 15.849 Age: 29.016  Density: 1.063 Weight: 178.086 Height: 70.75


mean(second_quart_b$BODYFAT)
mean(second_quart_b$AGE)
mean(second_quart_b$DENSITY)
mean(second_quart_b$WEIGHT)
mean(second_quart_b$HEIGHT)
# Avg Bodyfat:18.980 Age: 40.734 Density: 1.055 Weight: 178.789 Height: 70.777


mean(third_quart_b$BODYFAT)
mean(third_quart_b$AGE)
mean(third_quart_b$DENSITY)
mean(third_quart_b$WEIGHT)
mean(third_quart_b$HEIGHT)
# Avg Bodyfat: 19.956 Age: 48.691 Density: 1.054 Weight: 183.627 Height: 70


mean(fourth_quart$BODYFAT)
mean(fourth_quart$AGE)
mean(fourth_quart$DENSITY)
mean(fourth_quart$WEIGHT)
mean(fourth_quart$HEIGHT)
# Avg Bodyfat: 21.092 Age: 62.544 Density: 1.050 Weight: 174.393 Height: 68.956


# Question 4a

#upper bound for each weight class is the lower bound for the next weight class
weight_classes <- c(0, 125, 169, 203, 271, 999)

bins = paste(head(weight_classes, -1), weight_classes[-1], sep=" - ")
frequencies = hist(bodyfat_df$WEIGHT, breaks = weight_classes, include.lowest = FALSE, plot = FALSE)
freq <- data.frame(range = bins, frequency = frequencies$counts)
rel_freq <-data.frame(range = bins, relative_frequency = frequencies$counts/length(bodyfat_df$WEIGHT) * 100)

freq
rel_freq


# Question 4b
obe_subset <- subset(bodyfat_df, WEIGHT >= 203)
obe_subset2 <- subset(obe_subset, WEIGHT < 271)
mean(obe_subset2$BODYFAT)
obe_subset2


#Question 5

qqnorm(bodyfat_df$BODYFAT, pch = 1, frame = TRUE, main = "Bodyfat")
qqline(bodyfat_df$BODYFAT, col = "steelblue", lwd = 2)
# Looking at the Q-Q plot and comparing the Q-Q line to the plotted data, 
# we see that the Bodyfat variable appears normally distributed for values
# between Theoretical Quantiles -1 and 1. For all other values, the variable
# does not appear to be normally distributed.



qqnorm(bodyfat_df$AGE, pch = 1, frame = TRUE, main = "Age")
qqline(bodyfat_df$AGE, col = "steelblue", lwd = 2)
# Looking at the Q-Q plot and comparing the Q-Q line to the plotted data, 
# we see that the Age variable does not appear to be normally distributed.



qqnorm(bodyfat_df$DENSITY, pch = 1, frame = TRUE, main = "Density")
qqline(bodyfat_df$DENSITY, col = "steelblue", lwd = 2)
# Looking at the Q-Q plot and comparing the Q-Q line to the plotted data, 
# we see that the Density variable appears normally distributed for values
# between Theoretical Quantiles -1 and 1. For all other values, the variable
# does not appear to be normally distributed.



qqnorm(bodyfat_df$WEIGHT, pch = 1, frame = TRUE, main = "Weight")
qqline(bodyfat_df$WEIGHT, col = "steelblue", lwd = 2)
# Looking at the Q-Q plot and comparing the Q-Q line to the plotted data, 
# we see that the Weight variable appears normally distributed for most values.
# Values above Theoretical Quantile = 2.5 and values below Theoretical 
# Quantile = -2 do not appear to follow this distribution.



qqnorm(bodyfat_df$HEIGHT, pch = 1, frame = TRUE, main = "Height")
qqline(bodyfat_df$HEIGHT, col = "steelblue", lwd = 2)

# Looking at the Q-Q plot and comparing the Q-Q line to the plotted data, 
# we see that the Height variable appears normally distributed


#Question 6

#a
randSample <- bodyfat_df[sample(1:nrow(bodyfat_df), 30, replace=FALSE),]
x_bar <- mean(randSample$BODYFAT)
x_bar
se <- sd(randSample$BODYFAT)/sqrt(length(randSample))
se

#Example: Sample mean = 17.853; Standard Error = 1.75

#b
error <- qnorm(0.975)*se
leftBarr <- x_bar - error
rightBarr <- x_bar + error
interval <- c(leftBarr, rightBarr)
interval
# Example: Interval = (14.42, 21.28)

#c
pop_mean <- mean(bodyfat_df$BODYFAT)
pop_mean
# Population mean: 18.94

# comparing our the population to the interval obtained in part b, we would
# expect our population mean to lie within that interval 95% of the time.
# We see that when we take many different random samples (by running our
# code multiple times), a vast majority of the time our population mean lies
# within the confidence interval. Theoretically, if we took 100 samples, the
# population mean would lie within this interval 95 times.






