customer_data=read.csv("Mall_Customers.csv")
str(customer_data)

names(customer_data)

head(customer_data)
summary(customer_data$Age)

sd(customer_data$Age)
summary(customer_data$Annual.Income..k..)
sd(customer_data$Annual.Income..k..)
summary(customer_data$Age)

sd(customer_data$Spending.Score..1.100.)
# customer gender visualization
a = table(customer_data$Gender)
barplot(a, main = "Barplot for gender comparision",
        ylab = "Count#",
        xlab = "Gender",
        col = rainbow(2),
        legend = rownames(a))

lbls = c("Female", "Male")
pct = round(a/sum(a)*100)

lbls = paste(lbls," ", pct, "%", sep="")
pie(a, labels = lbls, col = rainbow(length(lbls)),
    main = "piechart for gender")

# customer age visualization
summary(customer_data$Age)
hist(customer_data$Age,
     col = "green",
     main = "histogram of customer age",
     xlab = "Age range",
     ylab = "Number",
     labels = TRUE)

boxplot(customer_data$Age,
        col = "blue",
        main = "boxplot for Descriptive Analysis of Age")
