# Try to recreate the forgetting curve

# Decreasing this will show more (noisy) data points
noise_threshold = 50

# Round off the interval
datafilter <- data
datafilter["actual_interval"] <- round(datafilter["actual_interval"])

review_1 <- subset(datafilter, previous_repetition_number == 0)
review_2 <- subset(datafilter, previous_repetition_number == 1)
review_3 <- subset(datafilter, previous_repetition_number == 2)
review_4 <- subset(datafilter, previous_repetition_number == 3)
review_5 <- subset(datafilter, previous_repetition_number == 4)

tbl2 <- table(review_2$correct, review_2$actual_interval)
tbl3 <- table(review_3$correct, review_3$actual_interval)
tbl4 <- table(review_4$correct, review_4$actual_interval)
tbl5 <- table(review_5$correct, review_5$actual_interval)

# function to calculate percentage of correct responses
pctfun <- function(x) {
  tot = sum(x)
  if(tot < noise_threshold){
    NA
  }else{
    100 * (x["true"] / (x["true"]+x["false"]))
  }
}
pct2 <- apply(tbl2, 2, pctfun)
pct3 <- apply(tbl3, 2, pctfun)
pct4 <- apply(tbl4, 2, pctfun)
pct5 <- apply(tbl5, 2, pctfun)

days <- c(0:36)

interval_names <- paste(days)
pct2.all <- pct2[interval_names]
names(pct2.all) <- interval_names
pct3.all <- pct3[interval_names]
pct4.all <- pct4[interval_names]
pct5.all <- pct5[interval_names]
summary <- cbind(pct2.all,pct3.all,pct4.all,pct5.all)
plot(interval_names, summary[,1], type="p", ylim=c(0,100), col="#ff0000", xlab = "Days since last review", ylab = "% Chance of Remembering")
points(summary[,2], col="#00ff00")
points(summary[,3], col="#0000ff")
points(summary[,4], col="#00ffff")
#print(lapply(pct2, FUN=function(x) x[interval_names]))

df <- data.frame(x = days, y = summary[,1])
fit_m <- nls(y ~ (x^power)/A, data=df, start = list(power=1,A=1))
lines(days, predict(fit_m, df), col="#ff0000")
print(fit_m)

df <- data.frame(x = days, y = summary[,2])
#fit_m <- nls(y ~ x*A, data=df, start = list(A=-10))
fit_m <- lm(y ~ x, data=df)
lines(predict(fit_m, df), col="#00ff00")
print(fit_m)

df <- data.frame(x = days, y = summary[,3])
#fit_m <- nls(y ~ x*A, data=df, start = list(A=-10))
fit_m <- lm(y ~ x, data=df)
lines(predict(fit_m, df), col="#0000ff")
print(fit_m)

legend(22,95, c("After First Review", "After Second Review", "After Third Review"), fill=c("red", "green", "blue"))
title("Experimental Forgetting Curves")
#rev1_curve <- (days^-0.46742)/0.01549
#lines(rev1_curve, col="#ff0000")