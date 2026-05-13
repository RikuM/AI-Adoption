library(readr)
library(readxl)
library(dplyr)
library(ggplot2)

indeed_posting <- read.csv("data files/customer_service_indeed.csv")
chatgpt_trend <- read.csv("data files/chatgpt_trend.csv")
Ai_headline <- read.csv("data files/ai-headline-share.csv")


summary(indeed_posting)
sd(indeed_posting$IHLIDXUSTPCUSTSERV)

summary(chatgpt_trend)
sd(chatgpt_trend$chatgpt)

summary(Ai_headline)
sd(Ai_headline$value)
