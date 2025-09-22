library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(report)

#DATA
#note: change the path for your own computer
data <- read.csv("/Users/pilardaronco/Code/MATH527-Tutorials-1/Project 2/Data/EdemaData.csv", 
    header = TRUE, sep = ",") 
#cleaning data
data <- data %>% mutate(trueID = ifelse(PatientID <= 90, 
    PatientID, PatientID - 90))
#print(data)




#ANCOVA_model:
ancova_model <- aov(Edema_Post ~ Edema_Pre + Group + 
    trueID + trueID*Edema_Pre, data = data)

print(ancova_model$coefficients)


#Assumptions check
data$residuals <- resid(ancova_model)

# Normality of residuals
qqplot <- ggqqplot(data$residuals, title = "Q-Q Plot of Residuals")
print(qqplot)


# Homoscedasticity
resid_plot <-ggplot(data, aes(x = fitted(ancova_model), y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuals vs Fitted Values for Edema_Post",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal()
#print(resid_plot)