---
title: "PA1_template"
author: "Ekaterina Voronina"
date: "7/21/2020"
output: html_document
---
Loading library to work with

```{r}
library(tidyverse)
```

Downloading data
```{r}
 url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url,"repdata_data_activity.zip")
unzip("repdata_data_activity.zip")
```

Reading data and removing missing values
```{r}
data <- read.csv("activity.csv",header = TRUE, sep = ",")
data$date <- as.Date(data$date)
clean_data <- data[!is.na(data$steps), ]
```

Filtering data by date and calculating steps sum by day, changing column name
```{r}
sum_steps <- clean_data %>% group_by(date) %>% summarise(sum(steps))
names(sum_steps)[2] <- "steps"
```

Building histogram of total number of steps taken each day
```{r}
hist(sum_steps$steps, col = "purple", main = "Total number of steps taken each day",
     xlab = "Number of steps", breaks = 40)
```

Usuing summary function to calculate "mean and "median" of the total number of steps taken per day
```{r}
summary(sum_steps$steps)
```

Saving the histogram
```{r}
dev.copy(png,'steps.png', width=480, height=480)
dev.off()
```
Filtering data by interval and calculating average steps by day, changing column name
```{r}
average_steps <- clean_data %>% group_by(interval) %>% summarise(mean(steps))
names(average_steps)[2] <- "mean_steps"
```

Building the plot showing average number of steps taken across all days
```{r}
mean_steps_plot <-ggplot(average_steps, aes(x = interval,y = mean_steps)) + geom_line(color = "purple") + 
  labs(title="Average number of steps taken across all days", 
       y="Average number of steps", x="5-min Interval time series")
```

```{r}
show(mean_steps_plot)
```

Saving the plot
```{r}
dev.copy(png,'averagesteps.png', width=480, height=480)
dev.off()
```

Calculating the row number containing the maximum number of steps
```{r}
which.max(average_steps$interval)
```

```{r}
print(average_steps[104,])
```

Calculating and the total number of missing values in the dataset
```{r}
sum(is.na(data))
```
The number of rows with NAs is 2304

Writting a fuccion to impute missing values
```{r}
mean_step <- clean_data %>% group_by(date) %>% summarise(mean(steps))

for(i in 1:length(data$steps)){
  if(is.na(data[i, "steps"])){
    
    for(y in 1:length(mean_step$date)){
      
      if(mean_step$date[y] == data$date[i]){
        data$steps[i] <- mean_step$`mean(steps)`[y]
      }
      
    }
    
  }
}


names(mean_step)[2] <- "steps"
```

Building histogram of total number of steps taken each day
```{r}
hist(mean_step$steps, col = "green", main = "Total number of steps taken each day", 
     xlab = "Number of steps", breaks = 40)
```

Saving the histogram
```{r}
dev.copy(png,'na.png', width=480, height=480)
dev.off()
```

Calculating mean and the median using summary function 
```{r}
summary(mean_step$steps)
```

Creating a new variable including information about a weekday

```{r}
clean_data$day <- ifelse(weekdays(clean_data$date) %in% c("Saturday","Sunday"), 
                         "weekday", "weekend")
```

Sorting the data to make a plot
```{r}
by_weekday <- clean_data %>% group_by(interval,day) %>% summarise(mean_step=mean(steps))
```

Making a panel plot showing time series
```{r}
plot_weekday <- ggplot(by_weekday, aes(x=interval, y=mean_step, color=day)) + 
  facet_grid(day~.) + geom_line() + 
  labs(title="Average Number of Steps Taken vs 5-min Interval on Weekday/Weekend", 
       y="Average Number of Steps", x="5-min Interval Times Series")
```

```{r}
show(plot_weekday)
```

Saving a plot
```{r}
dev.copy(png,'timeseries.png', width=480, height=480)
dev.off()
```