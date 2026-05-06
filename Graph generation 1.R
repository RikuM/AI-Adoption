install.packages("readr")
install.packages("readxl")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("writexl")
library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(writexl)

excel_sheets("C:/Users/lokch/Downloads/Customer Service Job Postings on Indeed in the United States.xlsx")

#Three files
google <- read_csv("C:/Users/lokch/Downloads/AI gpt trend.csv")
indeed_ai <- read_csv("C:/Users/lokch/Downloads/Indeed AI Tracker.csv")
job <- read_csv(
  "C:/Users/lokch/Downloads/Customer Service Job Postings on Indeed in the United States.csv"
  )
  
names(job)
View(job)


names(job)
View(job)

View(job)

View(job)

names(job)
View(job)

names(google)
names(indeed_ai)
names(job)
View(job)

# Convert data to monthly
# Google Trends monthly
google_monthly <- google %>%
  mutate(
    Time = as.Date(Time),
    Month = floor_date(Time, "month")
  ) %>%
  group_by(Month) %>%
  summarise(
    ChatGPTTrend = mean(chatgpt, na.rm = TRUE),
    AIChatbotTrend = mean(`AI chatbot`, na.rm = TRUE)
  )

# Indeed AI Tracker monthly
indeed_ai_monthly <- indeed_ai %>%
  mutate(
    dateString = as.Date(dateString),
    Month = floor_date(dateString, "month")
  ) %>%
  group_by(Month) %>%
  summarise(
    IndeedAI = mean(value, na.rm = TRUE)
  )
View(job)
names(job)
job_monthly <- job %>%
  mutate(
    Month = floor_date(observation_date, "month")
  ) %>%
  group_by(Month) %>%
  summarise(
    JobPostings = mean(IHLIDXUSTPCUSTSERV, na.rm = TRUE)
  )

View(job_monthly)

final_data <- google_monthly %>%
  left_join(indeed_ai_monthly, by = "Month") %>%
  left_join(job_monthly, by = "Month")

View(final_data)

model1 <- lm(JobPostings ~ ChatGPTTrend, data = final_data)
summary(model1)

model2 <- lm(JobPostings ~ IndeedAI, data = final_data)
summary(model2)

# graphs
ggplot(final_data, aes(x = ChatGPTTrend, y = JobPostings)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "ChatGPT Trends vs Customer Service Job Postings",
    x = "ChatGPT Google Trends",
    y = "Customer Service Job Postings"
  )

ggplot(final_data, aes(x = IndeedAI, y = JobPostings)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Indeed AI Tracker vs Customer Service Job Postings",
    x = "Indeed AI Tracker",
    y = "Customer Service Job Postings"
  )

ggplot(final_data, aes(x = Month)) +
  geom_line(aes(y = JobPostings, color = "Job Postings")) +
  geom_line(aes(y = ChatGPTTrend, color = "ChatGPT Trends")) +
  labs(
    title = "Job Postings and ChatGPT Trends Over Time",
    y = "Index / Value",
    color = "Variables"
  )
