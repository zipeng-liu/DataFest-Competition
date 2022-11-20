
library(ggplot2)
library(tidyverse)


a <- read.csv("depression_canada_19.csv")
b <- read.csv("anxiety_canada_19.csv")
c <- read.csv("obsessive_compulsive_disorder_canada_19.csv")
d <- read.csv("ocd_canada_19.csv")
e <- read.csv("insomnia_canada_19.csv")
f <- read.csv("panic_attack_canada_19.csv")
g <- read.csv("mental_health_canada_19.csv")
h <- read.csv("psychiatrist_canada_19.csv")

data.2019 <- cbind(a, b, c, d, e, f, g, h)
data.2019 <- data.2019[, -c(3, 5, 7, 9, 11, 13, 15)]
data.2019 <- data.2019[-c(1:29),]
data.2019$date <- as.Date(data.2019$Week)

data.2020 <- read.csv("search_term_canada.csv")
data.2020 <- data.2020[-c(1:29),]
data.2020$date <- as.Date(data.2020$Week)

data.lockdown <- read.csv("Countries_usefulFeatures.csv")
data.lockdown <- data.lockdown[, -c(2, 3, 4, 5, 6, 7, 8, 11)]
data.lockdown$date <- as.Date(data.lockdown$Lockdown_Date)
canada.date.2020 = data.lockdown[33, 4]
canada.date.2019 = as.Date("2019-03-16")

data.2020.new <- data.2020 %>%
  select(date, depression, anxiety, obsessive.compulsive.disorder, ocd, 
         insomnia, panic.attack, mental.health, psychiatrist) %>%
  gather(key = "variable", value = "value", -date)

ggplot(data.2020.new, aes(x = date, y = value)) + 
  geom_line(aes(color = variable, linetype = variable)) + 
  xlab('Dates') +
  ylab('Frequency') +
  ggtitle('Mental Health Search Terms in Canada 2020') +
  geom_vline(xintercept = canada.date.2020, colour="red")

data.2019.new <- data.2019 %>%
  select(date, depression, anxiety, obsessive.compulsive.disorder, ocd, 
         insomnia, panic.attack, mental.health, psychiatrist) %>%
  gather(key = "variable", value = "value", -date)

ggplot(data.2019.new, aes(x = date, y = value)) + 
  geom_line(aes(color = variable, linetype = variable)) + 
  xlab('Dates') +
  ylab('Frequency') +
  ggtitle('Mental Health Search Terms in Canada 2019') +
  geom_vline(xintercept = canada.date.2019, colour="red")

data.2019$total <- data.2019$depression + data.2019$anxiety + data.2019$obsessive.compulsive.disorder + data.2019$ocd + data.2019$insomnia + data.2019$panic.attack + data.2019$mental.health + data.2019$psychiatrist

data.2019$avg = data.2019$total/8

data.2020$total <- data.2020$depression + data.2020$anxiety + data.2020$obsessive.compulsive.disorder + data.2020$ocd + data.2020$insomnia + data.2020$panic.attack + data.2020$mental.health + data.2020$psychiatrist

data.2020$avg = data.2020$total/8

ggplot() + 
  geom_line(data = data.2019, aes(x = date, y = avg), color = "blue") +
  geom_line(data = data.2020, aes(x = date, y = avg), color = "red") +
  xlab('Dates') +
  ylab('Frequency') +
  geom_vline(xintercept = canada.date.2020, linetype = "dashed", colour="black")











