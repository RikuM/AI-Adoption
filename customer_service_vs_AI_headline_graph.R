#2021 - 2024



#Variables:
  #Hiring - customer service job postings on indeed, maybe linkedin
  #AI interest - indeed jobs w/ AI, AI google trends
  #Salary
  #Unemployment
  #Inflation
  #AI spending of year (Maybe)
  #Customer growth
  #GDP growth

library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
job_posting <- read.csv("data files/Job_posting_CS_HeadLine.csv")
Ai_headline <- read.csv("data files/ai-headline-share.csv")

customer_df <- job_posting %>%
  filter(grepl("customer", sectorCode, ignore.case = TRUE))
control_df <- job_posting %>%
  filter(!grepl("customer", sectorCode, ignore.case = TRUE))


customer_plot <- customer_df %>%
  select(dateString, value) %>%
  mutate(source = "Customer Service Job Postings")

ai_plot <- Ai_headline %>%
  select(dateString, value) %>%
  mutate(value = value * 100) %>%
  mutate(source = "AI Headline Share")



scale_factor <- max(customer_df$value, na.rm = TRUE) / max(Ai_headline$value, na.rm = TRUE)


ggplot() +
  geom_line(
    data = customer_df,
    aes(x = as.Date(dateString), y = value, color = "Customer Service Job Postings"),
    linewidth = 1.2
  ) +
  geom_vline(
    xintercept = as.Date("2022-11-30"),
    linetype = "dashed",
    color = "white"
  ) +
  geom_line(
    data = Ai_headline,
    aes(x = as.Date(dateString), y = value * scale_factor, color = "AI Headline Share (%)"),
    linewidth = 1.2
  ) +
  scale_y_continuous(
    name = "Customer Service Job Postings",
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "AI Headline Share (%)"
    )
  ) +
  annotate(
    "text",
    color = "white",
    x = as.Date("2022-11-30"),
    y = max(customer_df$value, na.rm = TRUE),
    label = "ChatGPT Release",
    angle = 0,
    vjust = 0,
    hjust = -0.1,
    size = 4
  ) +
  labs(
    title = "Customer Service Job Postings vs AI Headline Share",
    x = "Date",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    
    text = element_text(color = "white"),
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.background = element_rect(fill = "black", color = NA),
    
    panel.grid.major = element_line(color = "gray40"),
    panel.grid.minor = element_line(color = "gray25"),
    axis.line = element_blank(),
    panel.border = element_blank()
  )
