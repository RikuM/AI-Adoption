# Load data

library(readxl)
AI_Cust_Serv_Project_1 <- read_excel("C:/Users/Kerry/Documents/school/UWB/BBECN 382, Intro To Econometrics/AI Searches and Job Postings-2.xlsx")
AI_Cust_Serv_Project_2 <- read_excel("C:/Users/Kerry/Documents/school/UWB/BBECN 382, Intro To Econometrics/AI Searches and Job Postings-2.xlsx", sheet = 2)

# or set work directory
setwd("C:/Users/Kerry/Documents/school/UWB/BBECN 382, Intro To Econometrics")
AI_Cust_Serv_Project_1 <- read_excel("AI Searches and Job Postings-2.xlsx")
AI_Cust_Serv_Project_2 <- read_excel("AI Searches and Job Postings-2.xlsx", sheet = 2)

View(AI_Cust_Serv_Project_1)
attach(AI_Cust_Serv_Project_1)

# List the variables
names(AI_Cust_Serv_Project_1)

# Show first lines of data & first 10 observations
head(AI_Cust_Serv_Project_1)
AI_Cust_Serv_Project_1[1:10,] 

# Descriptive/summary statistics 

# (1) Summary stats for a specific variable
  summary(Date)
  summary(`AI Searches`)
  summary(`Job Postings`)
  summary(`Indeed AI`)
  summary(ChatGPT)
  sd(Date)
  sd(`AI Searches`)
  sd(`Job Postings`)
  sd(`Indeed AI`)
  sd(ChatGPT)
  length(Date)

# (2) Detailed summary statistics for all variables
  install.packages("psych")
  library(psych)
  
  describe(AI_Cust_Serv_Project_1)
  
# Correlation coefficients

  # (1) Correlation between two variables
  cor(`Job Postings`, `AI Searches`)
  cor(`Job Postings`, `Indeed AI`)
  cor(`Job Postings`, ChatGPT)
  cor(`Indeed AI`, `AI Searches`)
  cor(`Indeed AI`, ChatGPT)
  
  # (2) Correlation among multiple selected numerical variables
  cor(AI_Cust_Serv_Project_1[, c("Job Postings", "AI Searches", "ChatGPT", "Indeed AI")])

  # (3) Correlation among all numerical variables
  cor(AI_Cust_Serv_Project_1) # it will give an error message, because there are non-numeric variables in this dataset
  
    # Correct steps: 
  
    # (i) Select only numeric variables from dataset
    numeric_data <- AI_Cust_Serv_Project_1[sapply(AI_Cust_Serv_Project_1, is.numeric)] # sapply(AI_Cust_Serv_Project_1, is.numeric) returns a logical vector: TRUE for numeric columns, FALSE for others
  
    # (ii) Get correlation matrix using complete observations (ignoring missing values)
    cor_matrix <- cor(numeric_data, use = "complete.obs") # use = "complete.obs" removes rows with any NA values before computing correlations
    
    # (iii) View the matrix
    print(cor_matrix)

    # Visualization: Correlation Matrix
    
    install.packages("corrplot")  # Only run once
    library(corrplot)
    
    corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)

    # Customize color scale; further formatting 
    corrplot(cor_matrix,
             method = "color",
             type = "upper",      # "full" to show the whole matrix, or "lower" to show the lower triangle
             tl.col = "black",    # label color for variable names
             tl.cex = 0.8,        # text label size (smaller font). The default is 1.0; so 0.6 is 60% of that
             tl.srt = 45,         # rotate 45 degrees rotates the text labels by 45 degrees; helps avoid overlapping when variable names are long
             col = colorRampPalette(c("gold4", "white", "purple4"))(300)) # Smooth gradient from red → white → blue; (300) generates 300 shades between these colors for smoother color transitions
             
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

plot(`Job Postings`, ChatGPT,
     xlab = "ChatGPT",
     ylab = "Job Postings",
     main = "ChatGPT vs. Job Postings",
     pch = 19,  # solid circle
     col = "purple")
plot(`Job Postings`, `AI Searches`,
     xlab = "AI Searches",
     ylab = "Job Postings",
     main = "AI Searches vs. Job Postings",
     pch = 19,  # solid circle
     col = "gold")

ggsave("chatgptplot.png", width = 10, height = 10, dpi = 300) # save image

ggplot(data = AI_Cust_Serv_Project_1, aes(x = ChatGPT, y = `Job Postings`)) +
  geom_point(col = "purple") +
  labs(x = "ChatGPT", y = "Job Postings", title = "ChatGPT vs. Job Postings") +
  theme_minimal()
ggplot(data = AI_Cust_Serv_Project_1, aes(x = `AI Searches`, y = `Job Postings`)) +
  geom_point(col = "gold") +
  labs(x = "AI Searches", y = "Job Postings", title = "AI Searches vs. Job Postings") +
  theme_minimal()


ggplot(AI_Cust_Serv_Project_1, aes(x = ChatGPT, y = `Job Postings`)) +
  geom_point(col = "purple") +
  geom_smooth(method = "lm", se = TRUE, color = "gold") +
  labs(x = "ChatGPT", y = "Job Postings", title = "Scatterplot with Regression Line") +
  theme_minimal()
ggplot(AI_Cust_Serv_Project_1, aes(x = `AI Searches`, y = `Job Postings`)) +
  geom_point(col = "gold") +
  geom_smooth(method = "lm", se = TRUE, color = "purple") +
  labs(x = "AI Searches", y = "Job Postings", title = "Scatterplot with Regression Line") +
  theme_minimal()


# T-test for mean of one group
t.test(`AI Searches`, cutoff=20) 
t.test(`Job Postings`, cutoff=20) 
t.test(`Indeed AI`, cutoff=20) 
t.test(ChatGPT, cutoff=20) 


# OLS regression - testscr (dependent variable); str (explanatory variables)
olsreg_1 <- lm(`Job Postings` ~ `AI Searches`)
summary(olsreg_1)
olsreg_2 <- lm(`Job Postings` ~ `Indeed AI`)
summary(olsreg_2)
olsreg_3 <- lm(`Job Postings` ~ ChatGPT)
summary(olsreg_3)

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
  olsreg_4 <- lm(`Job Postings` ~ `AI Searches`)
  coeftest(olsreg_4, vcov = sandwich)
  coeftest(olsreg_4, vcov = vcovHC(olsreg_4, type="HC0")) # sandwich SE is the same as HC0
  coeftest(olsreg_4, vcov = vcovHC(olsreg_4, type="HC1")) # Heteroskedasticity-consistent (HC) standard errors; HC1 is preferred for small samples
  olsreg_5 <- lm(`Job Postings` ~ `Indeed AI`)
  coeftest(olsreg_5, vcov = sandwich)
  coeftest(olsreg_5, vcov = vcovHC(olsreg_5, type="HC0")) # sandwich SE is the same as HC0
  coeftest(olsreg_5, vcov = vcovHC(olsreg_5, type="HC1")) # Heteroskedasticity-consistent (HC) standard errors; HC1 is preferred for small samples
  olsreg_6 <- lm(`Job Postings` ~ ChatGPT)
  coeftest(olsreg_6, vcov = sandwich)
  coeftest(olsreg_6, vcov = vcovHC(olsreg_6, type="HC0")) # sandwich SE is the same as HC0
  coeftest(olsreg_6, vcov = vcovHC(olsreg_6, type="HC1")) # Heteroskedasticity-consistent (HC) standard errors; HC1 is preferred for small samples


# OLS Multivariate regression - testscr (dependent variable); str + el_pct (explanatory variables)
olsreg_multi_1 <- lm(`Job Postings` ~ `AI Searches` + ChatGPT)
coeftest(olsreg_multi_1, vcov = vcovHC(olsreg_multi_1, type="HC1"))
summary(olsreg_multi_1)
olsreg_multi_2 <- lm(`Job Postings` ~ `AI Searches` + ChatGPT + `Indeed AI`)
coeftest(olsreg_multi_2, vcov = vcovHC(olsreg_multi_2, type="HC1"))
summary(olsreg_multi_2)


