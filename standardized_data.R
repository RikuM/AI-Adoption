library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)



indeed_posting <- read.csv("data files/customer_service_indeed.csv")
chatgpt_trend <- read.csv("data files/chatgpt_trend.csv")
Ai_headline <- read.csv("data files/ai-headline-share.csv")


summary(indeed_posting)
sd(indeed_posting$IHLIDXUSTPCUSTSERV)

summary(chatgpt_trend)
sd(chatgpt_trend$chatgpt)

summary(Ai_headline)
sd(Ai_headline$value)

count(indeed_posting)
count(chatgpt_trend)
count(Ai_headline)


colnames(indeed_posting)
colnames(chatgpt_trend)
colnames(Ai_headline)



#Monthly Data
indeed_monthly <- indeed_posting %>%
  mutate(
    observation_date = as.Date(observation_date),
    Month = floor_date(observation_date, "month"),
    JobPostings = as.numeric(IHLIDXUSTPCUSTSERV)
  ) %>%
  group_by(Month) %>%
  summarise(
    JobPostings = mean(JobPostings, na.rm = TRUE),
    .groups = "drop"
  )

ai_headline_monthly <- Ai_headline %>%
  mutate(
    dateString = as.Date(dateString),
    Month = floor_date(dateString, "month"),
    AIHeadlineShare = as.numeric(value)
  ) %>%
  group_by(Month) %>%
  summarise(
    AIHeadlineShare = mean(AIHeadlineShare, na.rm = TRUE),
    .groups = "drop"
  )


chatgpt_monthly <- chatgpt_trend %>%
  mutate(
    Time = as.Date(Time),
    Month = floor_date(Time, "month"),
    ChatGPTTrend = as.numeric(chatgpt)
  ) %>%
  group_by(Month) %>%
  summarise(
    ChatGPTTrend = mean(ChatGPTTrend, na.rm = TRUE),
    .groups = "drop"
  )




#Combined Data
final_data <- indeed_monthly %>%
  left_join(chatgpt_monthly, by = "Month") %>%
  left_join(ai_headline_monthly, by = "Month") %>%
  arrange(Month)

View(final_data)





regression_data <- final_data %>%
  filter(
    !is.na(JobPostings),
    !is.na(ChatGPTTrend),
    !is.na(AIHeadlineShare)
  )

model_chatgpt <- lm(JobPostings ~ ChatGPTTrend, data = regression_data)

model_ai_headline <- lm(JobPostings ~ AIHeadlineShare, data = regression_data)

model_combined <- lm(JobPostings ~ ChatGPTTrend + AIHeadlineShare, data = regression_data)

summary(model_chatgpt)
summary(model_ai_headline)
summary(model_combined)


# ChatGPT model
b0 <- coef(model_chatgpt)[1]
b1 <- coef(model_chatgpt)[2]

cat(
  "ChatGPT Model: JobPostings =",
  round(b0, 3),
  ifelse(b1 < 0, "-", "+"),
  abs(round(b1, 3)),
  "* ChatGPTTrend\n"
)

# AI headline model
b0 <- coef(model_ai_headline)[1]
b1 <- coef(model_ai_headline)[2]

cat(
  "AI Headline Model: JobPostings =",
  round(b0, 3),
  ifelse(b1 < 0, "-", "+"),
  abs(round(b1, 3)),
  "* AIHeadlineShare\n"
)

# Combined model
b0 <- coef(model_combined)[1]
b1 <- coef(model_combined)[2]
b2 <- coef(model_combined)[3]

cat(
  "Combined Model: JobPostings =",
  round(b0, 3),
  ifelse(b1 < 0, "-", "+"),
  abs(round(b1, 3)),
  "* ChatGPTTrend",
  ifelse(b2 < 0, "-", "+"),
  abs(round(b2, 3)),
  "* AIHeadlineShare\n"
)

