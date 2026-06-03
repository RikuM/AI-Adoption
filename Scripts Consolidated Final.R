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
install.packages("gt")
install.packages("magick")

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
library(magick)
library(gt)
#Import Data ----
df <- read.csv("data files/final_data.csv")
df <- df %>%
  mutate(Month = mdy(Month))

#Script ----

  #Scaling for AI vs CS job graph
scale_factor_ChatGPT = max(df$JobPostings, na.rm = TRUE) / max(df$ChatGPTTrend, na.rm = TRUE)
scale_factor_AI = max(df$JobPostings, na.rm = TRUE) / max(df$AIHeadlineShare, na.rm = TRUE)

custom_colors <- colorRampPalette(c(
  "purple", 
  "white",
  "#2E6F40"
))(200)


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
  col = custom_colors,
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

model_2 <- lm(JobPostings ~ ChatGPTTrend + AIHeadlineShare, data = df)

model_3 <- lm(JobPostings ~ ChatGPTTrend +  UnemploymentRate, data = df)

model_3.5 <- lm(JobPostings ~ AIHeadlineShare +  UnemploymentRate, data = df)

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

model_7 <- lm(
  JobPostings ~ ChatGPTTrend * UnemploymentRate + AIHeadlineShare,
  data = df
)

models_1 <- list(
  "ChatGPT Trend" = model_1,
  "ChatGPT Trend + AI Job Headline" = model_2,
  "ChatGPT Trend + Unemployment Rate " = model_3,
  "AI Headline + Unemployment Rate" = model_3.5,
  "Full Model" = model_4
)
  

models_2 <- list(
  "ChatGPT x AI Headline" = model_5,
  "AI Headline x Unemployment" = model_6,
  "ChatGPT x Unemployment Rate" = model_7
)


#modelsummary(
#  models_1,
#  vcov = "HC1",
#  out = "regression_results_1.html",
#  title = "Regression Results with Robust Standard Errors"
#)

#modelsummary(
#  models_2,
#  vcov = "HC1",
#  out = "regression_results_2.html",
#  title = "Regression Results with Robust Standard Errors"
#)


# Chroma-key background color.
# Use a color that is NOT used anywhere in the table.
key_bg <- "#00FF00"

# Adjustable settings ----
separator_row <- 9  
separator_width <- 2
separator_color <- "rgba(255, 255, 255, 1)"

reg_table_1 <- modelsummary(
  models_1,
  vcov = "HC1",
  title = "Regression Results with Robust Standard Errors",
  output = "gt",
  stars = TRUE
) %>%
  tab_options(
    # Temporary green background
    table.background.color = key_bg,
    heading.background.color = key_bg,
    column_labels.background.color = key_bg,
    
    # White content
    table.font.color = "white",
    table.font.color.light = "white",
    
    # Remove default gt borders
    table.border.top.color = "white",
    table.border.bottom.color = "white",
    column_labels.border.top.color = "white",
    column_labels.border.bottom.color = "white",
    table_body.border.top.color = "white",
    table_body.border.bottom.color = "white",
    table_body.hlines.color = "white",
    table_body.hlines.width = px(0),
    
    # Sizing
    table.font.size = px(12),
    heading.title.font.size = px(16),
    column_labels.font.size = px(12),
    data_row.padding = px(6)
  ) %>%
  tab_style(
    style = cell_text(color = "white", weight = "bold"),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = cell_text(color = "white", weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(color = "white"),
    locations = cells_body()
  ) %>%
  tab_style(
    style = cell_text(color = "white"),
    locations = cells_stub()
  ) %>%
  opt_css(
    css = paste0(
      "
      html, body {
        background-color: ", key_bg, " !important;
      }

      .gt_table,
      .gt_heading,
      .gt_title,
      .gt_subtitle,
      .gt_col_headings,
      .gt_col_heading,
      .gt_table_body,
      .gt_row,
      .gt_stub,
      .gt_stubhead,
      .gt_group_heading,
      .gt_sourcenotes,
      .gt_footnotes {
        background-color: ", key_bg, " !important;
        color: white !important;
      }

      .gt_table * {
        color: white !important;
      }

      /* Remove ALL internal borders */
      .gt_table td,
      .gt_table th {
        border-top: white !important;
        border-bottom: white !important;
        border-left: white !important;
        border-right: white !important;
      }

      /* Add ONLY the separator line */
      .gt_table tbody tr:nth-child(", separator_row, ") td,
      .gt_table tbody tr:nth-child(", separator_row, ") th {
        border-top: ", separator_width, "px solid ", separator_color, " !important;
      }
      "
    )
  )


# Save temporary green-background image
gtsave(
  reg_table_1,
  "regression_results_temp_green_1.png",
  zoom = 3,
  expand = 0,
  selector = ".gt_table"
)

# Convert the green background to transparent

image_read("regression_results_temp_green_1.png") %>%
  image_transparent(color = key_bg, fuzz = 50) %>%
  image_write("regression_results_1_transparent.png")



reg_table_2 <- modelsummary(
  models_2,
  vcov = "HC1",
  title = "Regression Results with Robust Standard Errors",
  output = "gt",
  stars = TRUE
) %>%
  tab_options(
    # Temporary green background
    table.background.color = key_bg,
    heading.background.color = key_bg,
    column_labels.background.color = key_bg,
    
    # White content
    table.font.color = "white",
    table.font.color.light = "white",
    
    # Remove default gt borders
    table.border.top.color = "white",
    table.border.bottom.color = "white",
    column_labels.border.top.color = "white",
    column_labels.border.bottom.color = "white",
    table_body.border.top.color = "white",
    table_body.border.bottom.color = "white",
    table_body.hlines.color = "white",
    table_body.hlines.width = px(0),
    
    # Sizing
    table.font.size = px(font_size_body),
    heading.title.font.size = px(font_size_title),
    column_labels.font.size = px(font_size_columns),
    data_row.padding = px(row_padding)
  ) %>%
  tab_style(
    style = cell_text(color = "white", weight = "bold"),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = cell_text(color = "white", weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(color = "white"),
    locations = cells_body()
  ) %>%
  tab_style(
    style = cell_text(color = "white"),
    locations = cells_stub()
  ) %>%
  opt_css(
    css = paste0(
      "
      html, body {
        background-color: ", key_bg, " !important;
      }

      .gt_table,
      .gt_heading,
      .gt_title,
      .gt_subtitle,
      .gt_col_headings,
      .gt_col_heading,
      .gt_table_body,
      .gt_row,
      .gt_stub,
      .gt_stubhead,
      .gt_group_heading,
      .gt_sourcenotes,
      .gt_footnotes {
        background-color: ", key_bg, " !important;
        color: white !important;
      }

      .gt_table * {
        color: white !important;
      }

      /* Remove ALL internal borders */
      .gt_table td,
      .gt_table th {
        border-top: white !important;
        border-bottom: white !important;
        border-left: white !important;
        border-right: white !important;
      }

      /* Add ONLY the separator line */
      .gt_table tbody tr:nth-child(", 15, ") td,
      .gt_table tbody tr:nth-child(", 15, ") th {
        border-top: ", separator_width, "px solid ", separator_color, " !important;
      }
      "
    )
  )

# Save temporary green-background image
gtsave(
  reg_table_2,
  "regression_results_temp_green_2.png",
  zoom = 3,
  expand = 10,
  selector = ".gt_table"
)

# Convert the green background to transparent
image_read("regression_results_temp_green_2.png") %>%
  image_transparent(color = key_bg, fuzz = 60) %>%
  image_write("regression_results_2_transparent.png")


coeftest(
  model_4,
  vcov. = NeweyWest(model_4)
)

#Etc Etc ----

library(dplyr)
library(ggplot2)

df_model4 <- df %>%
  mutate(
    predicted_model_4 = predict(model_4, newdata = df),
    residual_model_4 = resid(model_4)
  )

p_actual_predicted_4 <- ggplot(df_model4, aes(x = Month)) +
  geom_line(
    aes(y = JobPostings, color = "Actual Job Postings"),
    linewidth = 1.2
  ) +
  geom_line(
    aes(y = predicted_model_4, color = "Predicted Job Postings"),
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  scale_color_manual(
    values = c(
      "Actual Job Postings" = "purple",
      "Predicted Job Postings" = "gold"
    )
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y"
  ) +
  labs(
    title = "Full Model: Actual vs Predicted Job Postings",
    x = "Month",
    y = "Customer Service Job Postings",
    color = ""
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
    legend.title = element_text(
      color = "white",
      face = "bold"
    ),
    legend.text = element_text(
      color = "white", 
      face = "bold",
      size = 12
    ),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.major.x = element_line(color = "gray85", linewidth = 0.3)
  )

#p_actual_predicted_4



ggsave(
  "monthly_trend_clear_background_actual_vs_predicted.png",
  plot = p_actual_predicted_4,
  width = 15,
  height = 8,
  dpi = 300,
  bg = "transparent"
)
library(dplyr)
library(ggplot2)

# Store means of control variables
mean_chatgpt <- mean(df$ChatGPTTrend, na.rm = TRUE)
mean_ai <- mean(df$AIHeadlineShare, na.rm = TRUE)
mean_unemp <- mean(df$UnemploymentRate, na.rm = TRUE)

# Prediction data for ChatGPTTrend
chatgpt_effect <- data.frame(
  Predictor = "ChatGPT Search Trend",
  x_value = seq(
    min(df$ChatGPTTrend, na.rm = TRUE),
    max(df$ChatGPTTrend, na.rm = TRUE),
    length.out = 100
  ),
  ChatGPTTrend = seq(
    min(df$ChatGPTTrend, na.rm = TRUE),
    max(df$ChatGPTTrend, na.rm = TRUE),
    length.out = 100
  ),
  AIHeadlineShare = mean_ai,
  UnemploymentRate = mean_unemp
)

# Prediction data for AIHeadlineShare
ai_effect <- data.frame(
  Predictor = "AI Headline Share",
  x_value = seq(
    min(df$AIHeadlineShare, na.rm = TRUE),
    max(df$AIHeadlineShare, na.rm = TRUE),
    length.out = 100
  ),
  ChatGPTTrend = mean_chatgpt,
  AIHeadlineShare = seq(
    min(df$AIHeadlineShare, na.rm = TRUE),
    max(df$AIHeadlineShare, na.rm = TRUE),
    length.out = 100
  ),
  UnemploymentRate = mean_unemp
)

# Prediction data for UnemploymentRate
unemp_effect <- data.frame(
  Predictor = "Unemployment Rate",
  x_value = seq(
    min(df$UnemploymentRate, na.rm = TRUE),
    max(df$UnemploymentRate, na.rm = TRUE),
    length.out = 100
  ),
  ChatGPTTrend = mean_chatgpt,
  AIHeadlineShare = mean_ai,
  UnemploymentRate = seq(
    min(df$UnemploymentRate, na.rm = TRUE),
    max(df$UnemploymentRate, na.rm = TRUE),
    length.out = 100
  )
)

# Combine all effect datasets
effect_df <- bind_rows(
  chatgpt_effect,
  ai_effect,
  unemp_effect
)

# Predicted job postings from model 4
effect_df$PredictedJobPostings <- predict(model_4, newdata = effect_df)

# Plot
p_model4_effects <- ggplot(
  effect_df,
  aes(x = x_value, y = PredictedJobPostings)
) +
  geom_line(color = "deeppink", linewidth = 1.3) +
  facet_wrap(~ Predictor, scales = "free_x") +
  labs(
    title = "Model 4: Predicted Job Postings by Predictor",
    subtitle = "Each graph holds the other two variables at their average values",
    x = "Predictor Value",
    y = "Predicted Customer Service Job Postings"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

p_model4_effects
