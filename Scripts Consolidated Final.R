#Final Version of our R script
#Everything will be consolidated in here with documentation

#install packages ----
install.packages("readr")
install.packages("readxl")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("writexl")
install.packages("psych")
install.packages("corrplot")
install.packages("stargazer")
install.packages("sandwich")
install.packages("lmtest")
install.packages("car")




#libraries ----
library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(writexl)
library(psych)
library(corrplot)
library(lmtest)
library(sandwich)
library(car)


#Import Data ----
df <- read.csv("data files/final_data.csv")
df <- df %>%
  mutate(Month = mdy(Month))

#Script

  #Scaling for AI vs CS job graph
scale_factor = max(df$JobPostings, na.rm = TRUE) / max(df$AIHeadlineShare, na.rm = TRUE)



#Correlation Matrix ----
corr_df <- df %>%
  select(
    JobPostings,
    ChatGPTTrend,
    AIHeadlineShare,
    UnemploymentRate
  )

corr_df <- na.omit(corr_df)

corr_matrix <- cor(corr_df, method = "pearson")
#print(corr_matrix)

corrplot(
  corr_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  number.cex = 0.8,
  tl.col = "black",
  tl.srt = 45,
  diag = TRUE
  # Will change color once theme is agreed upon
)

#AI vs Customer Service Jobs Line Graphs ----

ggplot() +
  geom_line(
    data = df,
    aes(x = Month, y = JobPostings, color = "Customer Service Job Postings"),
    linewidth = 1.2
  ) +
  geom_line(
    data = df,
    aes(
      x = Month,
      y = AIHeadlineShare * scale_factor,
      color = "AI Headline Share (%)"
    ),
    linewidth = 1.2
  ) +
  geom_vline(
    xintercept = as.Date("2022-12-1"),
    linetype = "dashed",
    color = "black"
  ) +
  scale_x_date(
    date_breaks = "3 months", date_labels = "%b %Y"
    ) +
  scale_y_continuous(
    name = "Customer Service Job Postings",
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "AI Headline Share (%)"
    )
  ) +
  labs(
    x = "Date",
    title = "Monthly Trend",
    color = "Variables"
  ) +
  annotate(
    "text",
    color = "black",
    x = as.Date("2022-11-30"),
    y = max(df$JobPostings, na.rm = TRUE),
    label = "ChatGPT Release",
    angle = 0,
    vjust = 0,
    hjust = -0.1,
    size = 4
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 15))
    # Will change color once theme is agreed upon
  )


#Scatter Plots ----


#Linear Regression ----


#Etc Etc ----