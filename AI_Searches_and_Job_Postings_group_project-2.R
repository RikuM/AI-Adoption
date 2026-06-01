# Load data

library(readxl)
AI_Cust_Serv_Project <- read.csv("C:/Users/Kerry/Documents/school/UWB/BBECN 382, Intro To Econometrics/AI Searches and Job Postings-4.csv")

# if working from GitHub
AI_Cust_Serv_Project <- read.csv("data files/AI Searches and Job Postings-4.csv")

# or set work directory
setwd("C:/Users/Kerry/Documents/school/UWB/BBECN 382, Intro To Econometrics/project R material")
AI_Cust_Serv_Project <- read.csv("AI Searches and Job Postings-4.csv")

View(AI_Cust_Serv_Project)
attach(AI_Cust_Serv_Project)

# List the variables
names(AI_Cust_Serv_Project)

# Show first lines of data & first 10 observations
head(AI_Cust_Serv_Project)
AI_Cust_Serv_Project_1[1:10,] 

# Descriptive/summary statistics 

# (1) Summary stats for a specific variable
summary(Month)
sd(Month)
summary(JobPostings)
sd(JobPostings)
summary(ChatGPTTrend)
sd(ChatGPTTrend)
summary(AIHeadlineShare)
sd(AIHeadlineShare)
summary(UnemploymentRate)
sd(UnemploymentRate)
length(Month)

# (2) Detailed summary statistics for all variables
install.packages("psych")
library(psych)

describe(AI_Cust_Serv_Project)

# Correlation coefficients

# (1) Correlation between two variables
cor(JobPostings, ChatGPTTrend)
cor(JobPostings, AIHeadlineShare)
cor(JobPostings, UnemploymentRate)
cor(ChatGPTTrend, AIHeadlineShare)
cor(ChatGPTTrend, UnemploymentRate)
cor(AIHeadlineShare, UnemploymentRate)

# (2) Correlation among multiple selected numerical variables
cor(AI_Cust_Serv_Project[, c("JobPostings", "ChatGPTTrend", "AIHeadlineShare", "UnemploymentRate")])

# (3) Correlation among all numerical variables
cor(AI_Cust_Serv_Project) # it will give an error message, because there are non-numeric variables in this dataset

# Correct steps: 

# (i) Select only numeric variables from dataset
numeric_data <- AI_Cust_Serv_Project[sapply(AI_Cust_Serv_Project, is.numeric)] # sapply(AI_Cust_Serv_Project_1, is.numeric) returns a logical vector: TRUE for numeric columns, FALSE for others

# (ii) Get correlation matrix using complete observations (ignoring missing values)
cor_matrix <- cor(numeric_data, use = "complete.obs") # use = "complete.obs" removes rows with any NA values before computing correlations

# (iii) View the matrix
print(cor_matrix)

# Visualization: Correlation Matrix

install.packages("corrplot")  # Only run once
library(corrplot)

# Customize color scale; further formatting 
# Add correlation values as numbers        
corrplot(cor_matrix,
         method = "color",
         type = "upper",      # "full" to show the whole matrix, or "lower" to show the lower triangle
         tl.col = "black",    # label color for variable names
         tl.cex = 0.8,        # text label size (smaller font). The default is 1.0; so 0.6 is 60% of that
         tl.srt = 45,         # rotate 45 degrees rotates the text labels by 45 degrees; helps avoid overlapping when variable names are long
         col = colorRampPalette(c("gold4", "white", "purple4"))(200), # Smooth gradient from white → light blue → blue; (200) generates 200 shades between these colors for smoother color transitions
         addCoef.col = "grey", # display actual correlation in matrix
         number.cex = 0.6) 

# Scatterplot
install.packages("ggplot2")
library(ggplot2)

plot(JobPostings, ChatGPTTrend,
     xlab = "ChatGPT",
     ylab = "Job Postings",
     main = "ChatGPT vs. Job Postings",
     pch = 19,  # solid circle
     col = "purple")
plot(JobPostings, AIHeadlineShare,
     xlab = "AI Searches",
     ylab = "Job Postings",
     main = "AI Searches vs. Job Postings",
     pch = 19,  # solid circle
     col = "gold")
plot(JobPostings, UnemploymentRate,
     xlab = "Unemployment Rate",
     ylab = "Job Postings",
     main = "Unemployment Rate vs. Job Postings",
     pch = 19,  # solid circle
     col = "green")

ggsave("chatgptplot.png", width = 10, height = 10, dpi = 300) # save image

ggplot(data = AI_Cust_Serv_Project, aes(x = ChatGPTTrend, y = JobPostings)) +
  geom_point(col = "purple") +
  labs(x = "ChatGPT", y = "Job Postings", title = "ChatGPT vs. Job Postings") +
  theme_minimal()
ggplot(data = AI_Cust_Serv_Project, aes(x = AIHeadlineShare, y = JobPostings)) +
  geom_point(col = "gold") +
  labs(x = "AI Searches", y = "Job Postings", title = "AI Searches vs. Job Postings") +
  theme_minimal()
ggplot(data = AI_Cust_Serv_Project, aes(x = UnemploymentRate, y = JobPostings)) +
  geom_point(col = "green") +
  labs(x = "Unemployment Rate", y = "Job Postings", title = "Unemployment Rate vs. Job Postings") +
  theme_minimal()


ggplot(AI_Cust_Serv_Project, aes(x = ChatGPTTrend, y = JobPostings)) +
  geom_point(col = "purple") +
  geom_smooth(method = "lm", se = TRUE, color = "gold") +
  labs(x = "ChatGPT", y = "Job Postings", title = "Scatterplot with Regression Line") +
  theme_minimal()
ggplot(AI_Cust_Serv_Project, aes(x = AIHeadlineShare, y = JobPostings)) +
  geom_point(col = "gold") +
  geom_smooth(method = "lm", se = TRUE, color = "purple") +
  labs(x = "AI Searches", y = "Job Postings", title = "Scatterplot with Regression Line") +
  theme_minimal()
ggplot(AI_Cust_Serv_Project, aes(x = UnemploymentRate, y = JobPostings)) +
  geom_point(col = "gold") +
  geom_smooth(method = "lm", se = TRUE, color = "purple") +
  labs(x = "Unemployment Rate", y = "Job Postings", title = "Scatterplot with Regression Line") +
  theme_minimal()


# T-test for mean of one group
t.test(JobPostings, cutoff=61) 
t.test(ChatGPTTrend, cutoff=61) 
t.test(AIHeadlineShare, cutoff=61) 
t.test(UnemploymentRate, cutoff=61) 


# OLS regression - testscr (dependent variable); str (explanatory variables)
olsreg_1 <- lm(JobPostings ~ ChatGPTTrend)
summary(olsreg_1)
olsreg_2 <- lm(JobPostings ~ AIHeadlineShare)
summary(olsreg_2)
olsreg_3 <- lm(JobPostings ~ UnemploymentRate)
summary(olsreg_3)

# Construct a table
install.packages("stargazer")
library(stargazer)

stargazer(olsreg_1, olsreg_2, olsreg_3, type = "html", out = "simple_regression_table.html", title = "Regression Results" )

# Additional exercise: Test for heteroskedasticity
# Null hypothesis (H₀): Homoskedasticity (equal variance of errors); Alternative hypothesis (H₁): Heteroskedasticity (variance of errors depends on one or more regressors)
library(lmtest)
bptest(olsreg_1)
bptest(olsreg_2)
bptest(olsreg_3)

# Plot residuals vs fitted values to check heteroskedasticity visually
plot(fitted(olsreg_1), resid(olsreg_1),
     main = "Residuals vs Fitted",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red")
plot(fitted(olsreg_2), resid(olsreg_2),
     main = "Residuals vs Fitted",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red")
plot(fitted(olsreg_3), resid(olsreg_3),
     main = "Residuals vs Fitted",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red")


# OLS with "robust standard errors" -> always use it; Heteroskadasticity-consistent SE
install.packages("sandwich")
install.packages("lmtest")

library(sandwich)
library(lmtest)

#summary(lm(testscr ~ str))
olsreg_4 <- lm(JobPostings ~ ChatGPTTrend)
coeftest(olsreg_4, vcov = sandwich)
coeftest(olsreg_4, vcov = vcovHC(olsreg_4, type="HC0")) # sandwich SE is the same as HC0
coeftest(olsreg_4, vcov = vcovHC(olsreg_4, type="HC1")) # Heteroskedasticity-consistent (HC) standard errors; HC1 is preferred for small samples
olsreg_5 <- lm(JobPostings ~ AIHeadlineShare)
coeftest(olsreg_5, vcov = sandwich)
coeftest(olsreg_5, vcov = vcovHC(olsreg_5, type="HC0")) # sandwich SE is the same as HC0
coeftest(olsreg_5, vcov = vcovHC(olsreg_5, type="HC1")) # Heteroskedasticity-consistent (HC) standard errors; HC1 is preferred for small samples
olsreg_6 <- lm(JobPostings ~ UnemploymentRate)
coeftest(olsreg_6, vcov = sandwich)
coeftest(olsreg_6, vcov = vcovHC(olsreg_6, type="HC0")) # sandwich SE is the same as HC0
coeftest(olsreg_6, vcov = vcovHC(olsreg_6, type="HC1")) # Heteroskedasticity-consistent (HC) standard errors; HC1 is preferred for small samples


# OLS Multivariate regression - testscr (dependent variable); str + el_pct (explanatory variables)
olsreg_multi_1 <- lm(JobPostings ~ ChatGPTTrend + AIHeadlineShare)
coeftest(olsreg_multi_1, vcov = vcovHC(olsreg_multi_1, type="HC1"))
summary(olsreg_multi_1)
olsreg_multi_2 <- lm(JobPostings ~ AIHeadlineShare + UnemploymentRate)
coeftest(olsreg_multi_2, vcov = vcovHC(olsreg_multi_2, type="HC1"))
summary(olsreg_multi_2)
olsreg_multi_3 <- lm(JobPostings ~ UnemploymentRate + ChatGPTTrend)
coeftest(olsreg_multi_3, vcov = vcovHC(olsreg_multi_3, type="HC1"))
summary(olsreg_multi_3)
olsreg_multi_4 <- lm(JobPostings ~ ChatGPTTrend + AIHeadlineShare + UnemploymentRate)
coeftest(olsreg_multi_4, vcov = vcovHC(olsreg_multi_4, type="HC1"))
summary(olsreg_multi_4)

stargazer(olsreg_multi_1, olsreg_multi_2, olsreg_multi_3, olsreg_multi_4, type = "html", out = "multivariate_regression_table.html", title = "Regression Results" )

# Joint hypothesis testing
joint_hypo <- lm(JobPostings ~ ChatGPTTrend + AIHeadlineShare + UnemploymentRate, data = AI_Cust_Serv_Project)
coeftest(joint_hypo, vcov = vcovHC(joint_hypo, type="HC1"))

# robust joint F-test (heteroskedasticity-consistent SE); need to combine the car and sandwich packages.

install.packages("car")
install.packages("sandwich")

library(car)
library(sandwich)

linearHypothesis(joint_hypo, 
                 c("ChatGPTTrend = 0", "AIHeadlineShare = 0"), 
                 vcov = vcovHC(joint_hypo, type = "HC1")) # robust variance-covariance matrix; reject the null, at least one of str or expn_stu matters for predicting testsc
linearHypothesis(joint_hypo, 
                 c("ChatGPTTrend = 0", "UnemploymentRate = 0"), 
                 vcov = vcovHC(joint_hypo, type = "HC1")) # robust variance-covariance matrix; reject the null, at least one of str or expn_stu matters for predicting testsc
linearHypothesis(joint_hypo, 
                 c("AIHeadlineShare = 0", "UnemploymentRate = 0"), 
                 vcov = vcovHC(joint_hypo, type = "HC1")) # robust variance-covariance matrix; reject the null, at least one of str or expn_stu matters for predicting testsc

# Restriction on coefficients

linearHypothesis(joint_hypo,
                 "ChatGPTTrend = AIHeadlineShare",
                 vcov = vcovHC(olsreg3, type = "HC1")) # robust joint F-test (heteroskedasticity-consistent SE)
linearHypothesis(joint_hypo,
                 "ChatGPTTrend = UnemploymentRate",
                 vcov = vcovHC(olsreg3, type = "HC1")) # robust joint F-test (heteroskedasticity-consistent SE)
linearHypothesis(joint_hypo,
                 "AIHeadlineShare = UnemploymentRate",
                 vcov = vcovHC(olsreg3, type = "HC1")) # robust joint F-test (heteroskedasticity-consistent SE)


linearHypothesis(joint_hypo,
                 "ChatGPTTrend + AIHeadlineShare = 1",
                 vcov = vcovHC(olsreg3, type = "HC1")) # robust joint F-test (heteroskedasticity-consistent SE); testing if the sum of coefficients on str and expn_stu is 1 
linearHypothesis(joint_hypo,
                 "ChatGPTTrend + UnemploymentRate = 1",
                 vcov = vcovHC(olsreg3, type = "HC1")) # robust joint F-test (heteroskedasticity-consistent SE); testing if the sum of coefficients on str and expn_stu is 1 
linearHypothesis(joint_hypo,
                 "AIHeadlineShare + UnemploymentRate = 1",
                 vcov = vcovHC(olsreg3, type = "HC1")) # robust joint F-test (heteroskedasticity-consistent SE); testing if the sum of coefficients on str and expn_stu is 1 



# Chapter 8. Nonlinear Regression

# Topic 1: Interaction Terms

# 1. Binary x Binary

# (1) Create dummy variables
CAschool$HiSTR <- ifelse(CAschool$str >= 20, 1, 0)
CAschool$HiEL <- ifelse(CAschool$el_pct >= 10, 1, 0)

# (2) Create interaction term
CAschool$HiSTR_HiEL <- CAschool$HiSTR * CAschool$HiEL

# (3) Run regression
model_BB <- lm(testscr ~ HiSTR + HiEL + HiSTR_HiEL, data = CAschool)
coeftest(model_BB, vcov = vcovHC(model_BB, type="HC1")) # robust SE

# Compare regression results with sample means of 4 sub-samples
aggregate(testscr ~ HiSTR + HiEL, data = CAschool, mean)

# 2. Continuous (str) x Binary (HiEL)

CAschool$str_HiEL <- CAschool$str * CAschool$HiEL # Create interaction term

model_CB <- lm(testscr ~ str + HiEL + str_HiEL, data = CAschool)
coeftest(model_CB, vcov = vcovHC(model_CB, type="HC1")) # robust SE

# Testing if two regression lines are the same: F-test, combined effect of str and HiEL

library(car)
library(sandwich)

linearHypothesis(model_CB, 
                 c("HiEL = 0", "str_HiEL = 0"), 
                 vcov = vcovHC(model_CB, type = "HC1")) 

# Visualization

# Create a sequence of str values for prediction
str_seq <- seq(min(CAschool$str), max(CAschool$str), length.out = 100)

# Create prediction data for HiEL = 0 and HiEL = 1
pred_data <- data.frame(
  str = rep(str_seq, 2),
  HiEL = rep(c(0, 1), each = length(str_seq))
)

pred_data$str_HiEL <- pred_data$str * pred_data$HiEL

# Predict using the fitted model
pred_data$fit_CB <- predict(model_CB, newdata = pred_data)

# Load ggplot2
library(ggplot2)

# Plot
ggplot(CAschool, aes(x = str, y = testscr)) +
  #geom_point(alpha = 0.5) +
  geom_line(data = pred_data, aes(x = str, y = fit_CB, color = factor(HiEL)), size = 1.2) +
  labs(title = "Fitted Regression Lines by HiEL Group",
       x = "Student-Teacher Ratio (STR)",
       y = "Test Score",
       color = "HiEL") +
  scale_color_manual(values = c("0" = "blue", "1" = "red"),
                     labels = c("HiEL = 0", "HiEL = 1")) + theme_minimal()


# 3. Continuous (str) x Continuous (el_pct)

CAschool$str_elpct <- CAschool$str * CAschool$el_pct # Create interaction term

model_CC <- lm(testscr ~ str + el_pct + str_elpct, data = CAschool)
coeftest(model_CC, vcov = vcovHC(model_CC, type="HC1")) # robust SE

# Joint hypothesis testing

linearHypothesis(model_CC, 
                 c("str = 0", "str_elpct = 0"), 
                 vcov = vcovHC(model_CC, type = "HC1")) 

# Topic 2: Polynomial

# (1) Quadratic term

# Create the squared term
CAschool$avginc2 <- CAschool$avginc^2

# Fit the quadratic model
model_quad <- lm(testscr ~ avginc + avginc2, data = CAschool)
coeftest(model_quad, vcov = vcovHC(model_quad, type="HC1")) # robust SE

# Fit the linear model (benchmark for comparison)
model_lin <- lm(testscr ~ avginc, data = CAschool)
coeftest(model_lin, vcov = vcovHC(model_lin, type="HC1"))

# Graph

# Load necessary libraries
library(ggplot2)

# Create a sequence of avginc values for prediction
avginc_seq <- seq(min(CAschool$avginc), max(CAschool$avginc), length.out = 100)
pred_data <- data.frame(avginc = avginc_seq, avginc2 = avginc_seq^2)

# Get predictions
pred_data$pred_quad <- predict(model_quad, newdata = pred_data)
pred_data$pred_lin <- predict(model_lin, newdata = pred_data)

# Plot
ggplot(CAschool, aes(x = avginc, y = testscr)) +
  geom_point(color = "black", alpha = 0.6) +
  geom_line(data = pred_data, aes(x = avginc, y = pred_quad), color = "red", size = 1.2, linetype = "solid") +
  geom_line(data = pred_data, aes(x = avginc, y = pred_lin), color = "blue", size = 1.2, linetype = "dashed") +
  labs(title = "Test Score vs Average Income",
       x = "Average Income",
       y = "Test Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("Quadratic Fit" = "red", "Linear Fit" = "blue"))

# (2) Add cubic term

CAschool$avginc3 <- CAschool$avginc^3

# Fit the cubic regression model
model_cubic <- lm(testscr ~ avginc + avginc2 + avginc3, data = CAschool)
coeftest(model_cubic, vcov = vcovHC(model_cubic, type="HC1"))

# Joint hypothesis testing

linearHypothesis(model_cubic, 
                 c("avginc2 = 0", "avginc3 = 0"), 
                 vcov = vcovHC(model_cubic, type = "HC1")) 


# Graph predicted curves together: linear, quadratic, cubic

# Load necessary library
library(ggplot2)

# Create a sequence of avginc values for prediction
avginc_seq <- seq(min(CAschool$avginc), max(CAschool$avginc), length.out = 100)
pred_data <- data.frame(
  avginc = avginc_seq,
  avginc2 = avginc_seq^2,
  avginc3 = avginc_seq^3
)

# Get predictions
pred_data$pred_lin <- predict(model_lin, newdata = pred_data)
pred_data$pred_quad <- predict(model_quad, newdata = pred_data)
pred_data$pred_cubic <- predict(model_cubic, newdata = pred_data)

# Plot
ggplot(CAschool, aes(x = avginc, y = testscr)) +
  geom_point(color = "black", alpha = 0.6) +
  geom_line(data = pred_data, aes(x = avginc, y = pred_lin), color = "blue", linetype = "dashed", size = 1.2) +
  geom_line(data = pred_data, aes(x = avginc, y = pred_quad), color = "red", size = 1.2) +
  geom_line(data = pred_data, aes(x = avginc, y = pred_cubic), color = "purple", size = 1.2) +
  labs(title = "Test Score vs Average Income",
       x = "Average Income",
       y = "Test Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Topic 3: Log Transformation of variables

# Create log-transformed variables
CAschool$ln_testscr <- log(CAschool$testscr)
CAschool$ln_avginc <- log(CAschool$avginc)

# Compare scatter plots: 

# Scatterplot (1) : testscr vs. income
ggplot(data = CAschool, aes(x = avginc, y = testscr)) +
  geom_point(col = "black") +
  labs(x = "Income", y = "Test_Scores", title = "Income vs. Test_Scores") +
  theme_minimal()

# Scatterplot (2): ln_testscr vs. ln_avginc
ggplot(data = CAschool, aes(x = ln_avginc, y = ln_testscr)) +
  geom_point(col = "black") +
  labs(x = "log Income", y = "log Test_Scores", title = "Log Income vs. Log Test_Scores") +
  theme_minimal()

# Model variation:

# (1). Linear-log model
model_linlog <- lm(testscr ~ ln_avginc, data = CAschool)
coeftest(model_linlog, vcov = vcovHC(model_linlog, type="HC1"))

# (2). Log-linear model
model_loglin <- lm(ln_testscr ~ avginc, data = CAschool)
coeftest(model_loglin, vcov = vcovHC(model_loglin, type="HC1"))

# (3). Log-log model 
model_loglog <- lm(ln_testscr ~ ln_avginc, data = CAschool)
coeftest(model_loglog, vcov = vcovHC(model_loglog, type="HC1"))

# Visualization: compare log-linear and log-log (required same dependent variable)

# Load ggplot2
library(ggplot2)

# Create a sequence of avginc values for prediction
avginc_seq <- seq(min(CAschool$avginc), max(CAschool$avginc), length.out = 100)
ln_avginc_seq <- log(avginc_seq)

# Create a new data frame for prediction
pred_data <- data.frame(avginc = avginc_seq, ln_avginc = ln_avginc_seq)

# Get predicted values from both models
pred_data$loglin_fit <- predict(model_loglin, newdata = pred_data)
pred_data$loglog_fit <- predict(model_loglog, newdata = pred_data)

# Plot both fitted lines with the original data
ggplot(CAschool, aes(x = avginc, y = ln_testscr)) +
  geom_point(color = "black", alpha = 0.6) +
  geom_line(data = pred_data, aes(x = avginc, y = loglin_fit), color = "red", label='Log-Linear Fit', size = 1.2, linetype = "solid") +
  geom_line(data = pred_data, aes(x = avginc, y = loglog_fit), color = "green", label='Log-Log Fit', size = 1.2, linetype = "solid") +
  labs(title = "Log-Linear vs Log-Log Regression Fits",
       x = "Average Income",
       y = "Log Test Score") +
  theme_minimal() +
  scale_color_manual(values = c("Log-Linear Fit" = "red", "Log-Log Fit" = "green")) 


rm(list = ls())

# Help function is helpful! For example,

help(ggplot2)
