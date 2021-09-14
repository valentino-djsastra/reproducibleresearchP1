fileURL = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURL, destfile ="./Data.zip")
unzip(zipfile="./Data.zip")
data_vec <- read.csv("activity.csv", header = TRUE)

data_vec$day <- weekdays(as.Date(data_vec$date))
data_vec$DateTime<- as.POSIXct(data_vec$date, format="%Y-%m-%d")

clean_data <- data_vec[!is.na(data_vec$steps),]
      
library(tibble)
df <- as_tibble(clean_data)
# rm(data_vec)
# rm(clean_data)

library(dplyr)
df_sum <- df %>% group_by(date) %>% 
          dplyr::summarize(steps = sum(steps, na.rm = TRUE)) %>%
          arrange(date)

str1 = "Total No. of Steps per Day"
str2 = "Histogram of Total Steps per Day"
hist(df_sum$steps, xlab = str1, main = str2)

summarize(df_sum, 
          avg_steps = mean(steps, na.rm = TRUE), 
          median_steps = median(steps, na.rm = TRUE))

df2 <- df %>% group_by(interval) %>% dplyr::summarize(avg_steps = mean(steps))
with(df2, plot(interval, avg_steps, 
               xlab = "Interval", 
               ylab = "Avg. No. of Steps", 
               main = "Avg. No. of Steps per Interval", 
               type = "l"))

max_steps <- max(df2$avg_steps)
max_interval <- df2[df2$avg_steps == max_steps, 1]

sum(is.na(data_vec$steps))

df3 <- df %>% group_by(interval, day) %>% dplyr::summarize(avg_steps = mean(steps))
nadata <- as_tibble(data_vec[is.na(data_vec$steps),])
newdata <- merge(nadata, df3, by = c("interval", "day"))
newdata2 <- select(newdata, steps = avg_steps, date, interval, day, DateTime)
new_master <- arrange(rbind(df, newdata2), DateTime)

df_sum_new <- new_master %>% group_by(date) %>% 
            dplyr::summarize(steps = sum(steps, na.rm = TRUE)) %>%
            arrange(date)

str1 = "Total No. of Steps per Day"
str2 = "Histogram of Total Steps per Day"
hist(df_sum_new$steps, xlab = str1, main = str2, col = "Black")
hist(df_sum$steps, xlab = str1, main = str2, col = "Grey", add = TRUE)
legend("topright", c("Imputed NAs", "Non-NA Data only"), fill = c("black", "grey"))

summarize(df_sum_new, 
          avg_steps = mean(steps, na.rm = TRUE), 
          median_steps = median(steps, na.rm = TRUE))

library(lattice)
new_master$DayCat <- ifelse(new_master$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

df_sum_3 <- new_master %>% group_by(interval, DayCat) %>% dplyr::summarize(avg_steps = mean(steps))
xyplot(avg_steps ~ interval | DayCat, data = df_sum_3, type="l",  layout = c(1,2),
       main="Avg. Steps per Interval Based on Type of Day", 
       ylab="Avg. Number of Steps", xlab="Interval")

