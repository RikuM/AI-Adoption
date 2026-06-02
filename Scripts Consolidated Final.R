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
install.packages("modelsummary")
install.packages("tibble")
install.packages("knitr")



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
library(modelsummary)
library(viridis)

#Import Data ----
df <- read.csv("data files/final_data.csv")
df <- df %>%
  mutate(Month = mdy(Month))

#Script ----

  #Scaling for AI vs CS job graph
scale_factor_ChatGPT = max(df$JobPostings, na.rm = TRUE) / max(df$ChatGPTTrend, na.rm = TRUE)
scale_factor_AI = max(df$JobPostings, na.rm = TRUE) / max(df$AIHeadlineShare, na.rm = TRUE)



  #Min Max Std Dev
summary(df)
sd(df$UnemploymentRate)


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

png(
  filename = "correlation_matrix_high_res.png",
  width = 2400,
  height = 1800,
  res = 300,
  bg = "transparent"
)

par(
  bg = "transparent",
  fg = "white",
  col.axis = "white",
  col.lab = "white",
  col.main = "white"
)

corrplot(
  corr_matrix,
  method = "color",
  type = "upper",
  col = plasma(200),
  outline = FALSE,
  addCoef.col = "black",
  number.cex = 0.8,
  tl.col = "white",
  tl.srt = 45,
  diag = TRUE
)

dev.off()

#ChatGPT Search vs Cusomter Service Jobs Line Graph ----
pp <- ggplot() +
  geom_line(
    data = df,
    aes(
      x = Month,
      y = JobPostings,
      color = "Customer Service Job Postings"
    ),
    linewidth = 1.2
  ) +
  geom_line(
    data = df,
    aes(
      x = Month,
      y = ChatGPTTrend * scale_factor_ChatGPT,
      color = "ChatGPT Search Trend"
    ),
    linewidth = 1.2
  ) +
  scale_color_manual(
    values = c(
      "Customer Service Job Postings" = "gold",
      "ChatGPT Search Trend" = "deeppink"
    ),
    labels = c(
      "Customer Service Job Postings" = "Customer Service Job Postings",
      "ChatGPT Search Trend" = "ChatGPTSearch Trend\nScaled by 1.42"
    )
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y"
  ) +
  scale_y_continuous(
    name = "Customer Service Job Postings",
    sec.axis = sec_axis(
      ~ . / scale_factor_ChatGPT,
      name = "ChatGPT Search Trend"
    )
  ) +
  labs(
    x = "Date",
    title = "Monthly Trend",
    color = "Variables"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent", color = NA),
    
    plot.title = element_text(
      color = "white",
      face = "bold",
      size = 18
    ),
    
    axis.title.x = element_text(
      color = "white",
      face = "bold",
      margin = margin(t = 15)
    ),
    axis.title.y = element_text(
      color = "white",
      face = "bold",
      size = 12
    ),
    axis.title.y.right = element_text(
      color = "white",
      face = "bold",
      size = 12
    ),
    
    axis.text.x = element_text(
      color = "white",
      face = "bold",
      angle = 45,
      hjust = 1,
      vjust = 0.5
    ),
    axis.text.y = element_text(
      color = "white",
      face = "bold"
    ),
    axis.text.y.right = element_text(
      color = "white",
      face = "bold"
    ),
    
    legend.title = element_text(
      color = "white",
      face = "bold"
    ),
    legend.text = element_text(
      color = "white", 
      face = "bold"
    ),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.major.x = element_line(color = "gray85", linewidth = 0.3)
  )
 


ggsave(
  "monthly_trend_clear_background_Chatgpt_vs_Posting.png",
  plot = pp,
  width = 15,
  height = 8,
  dpi = 300,
  bg = "transparent"
)
#AI vs Customer Service Jobs Line Graph ----


p <- ggplot() +
  geom_line(
    data = df,
    aes(
      x = Month,
      y = JobPostings,
      color = "Customer Service Job Postings"
    ),
    linewidth = 1.2
  ) +
  geom_line(
    data = df,
    aes(
      x = Month,
      y = AIHeadlineShare * scale_factor_AI,
      color = "AI Headline Share (%)"
    ),
    linewidth = 1.2
  ) +
  geom_vline(
    xintercept = as.Date("2022-12-01"),
    linetype = "dashed",
    color = "white"
  ) +
  annotate(
    "text",
    color = "white",
    x = as.Date("2022-11-30"),
    y = max(df$JobPostings, na.rm = TRUE),
    label = "ChatGPT Release",
    angle = 0,
    vjust = 0,
    hjust = -0.1,
    size = 4
  ) +
  scale_color_manual(
    values = c(
      "Customer Service Job Postings" = "gold",
      "AI Headline Share (%)" = "deeppink"
    ),
    labels = c(
      "Customer Service Job Postings" = "Customer Service Job Postings",
      "AI Headline Share (%)" = "AI Headline Share (%)\nScaled by 26.284"
    )
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y"
  ) +
  scale_y_continuous(
    name = "Customer Service Job Postings",
    sec.axis = sec_axis(
      ~ . / scale_factor_AI,
      name = "AI Headline Share (%)"
    )
  ) +
  labs(
    x = "Date",
    title = "Monthly Trend",
    color = "Variables"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent", color = NA),
    
    plot.title = element_text(
      color = "white",
      face = "bold",
      size = 18
      ),
    
    axis.title.x = element_text(
      color = "white",
      face = "bold",
      margin = margin(t = 15)
    ),
    axis.title.y = element_text(
      color = "white",
      face = "bold",
      size = 12
    ),
    axis.title.y.right = element_text(
      color = "white",
      face = "bold",
      size = 12
    ),
    
    axis.text.x = element_text(
      color = "white",
      face = "bold",
      angle = 45,
      hjust = 1,
      vjust = 0.5
    ),
    axis.text.y = element_text(
      color = "white",
      face = "bold"
    ),
    axis.text.y.right = element_text(
      color = "white",
      face = "bold"
    ),
    
    legend.title = element_text(
      color = "white",
      face = "bold"
    ),
    legend.text = element_text(
      color = "white", 
      face = "bold"
    ),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.major.x = element_line(color = "gray85", linewidth = 0.3)
  )


ggsave(
  "monthly_trend_clear_background_AI_Headline_vs_Job_Postings.png",
  plot = p,
  width = 15,
  height = 8,
  dpi = 300,
  bg = "transparent"
)
#Scatter Plots ----


#Linear Regression ----
model_1 <- lm(JobPostings ~ ChatGPTTrend, data = df)

model_2 <- lm(JobPostings ~ AIHeadlineShare, data = df)

model_3 <- lm(JobPostings ~ UnemploymentRate, data = df)

model_4 <- lm(
  JobPostings ~ ChatGPTTrend + AIHeadlineShare + UnemploymentRate,
  data = df
)

model_5 <- lm(
  JobPostings ~ ChatGPTTrend * AIHeadlineShare + UnemploymentRate,
  data = df
)

model_6 <- lm(
  JobPostings ~ AIHeadlineShare * UnemploymentRate + ChatGPTTrend,
  data = df
)

models <- list(
  "ChatGPT Trend" = model_1,
  "AI Job Headline" = model_2,
  "Unemployment Rate" = model_3,
  "Full Model" = model_4,
  "ChatGPT x AI Headline" = model_5,
  "AI Headline x Unemployment" = model_6
)

modelsummary(
  models,
  vcov = "HC1",
  out = "regression_results.html",
  title = "Regression Results with Robust Standard Errors"
)

coeftest(
  model_4,
  vcov. = NeweyWest(model_4)
)

#Etc Etc ----