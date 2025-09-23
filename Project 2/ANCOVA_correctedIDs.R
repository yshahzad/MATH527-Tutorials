library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(report)
library(gridExtra)

#DATA
#note: change the path for your own computer
data <- read.csv("/Users/pilardaronco/Code/MATH527-Tutorials-1/Project 2/Data/EdemaData.csv", 
    header = TRUE, sep = ",") 
#cleaning data
data <- data %>% mutate(trueID = ifelse(PatientID <= 90, 
    PatientID, PatientID - 90))
#print(data)

#visualization
#reg. boxplot
bxpdata <- data %>% 
  select(trueID, Group, Edema_Pre, Edema_Post) %>%
  pivot_longer(cols = c(Edema_Pre, Edema_Post), 
    names_to = "Time", values_to = "Edema")

bxp <- ggboxplot(
  bxpdata, x = "Group", y = "Edema",
  color = "Time", palette = "jco",
  )
print(bxp)


#paired plots
y_limits <- range(bxpdata$Edema, na.rm = TRUE)

datatreatment <- bxpdata %>% filter(Group == "Treatment")
dataplacebo <- bxpdata %>% filter(Group == "Placebo")

pairedplot1 <- ggpaired(
  datatreatment, x = "Time", y = "Edema", id = "trueID",
  line.color = "gray", linetype = "dashed"
  ) + ylim(y_limits)
pairedplot2 <- ggpaired(
  dataplacebo, x = "Time", y = "Edema", id = "trueID",
  line.color = "gray", linetype = "dashed"
  ) + ylim(y_limits)

grid.arrange(pairedplot1, pairedplot2, ncol = 2)

paired_plot <- ggpaired(
  bxpdata, x = "Time", y = "Edema", id = "trueID",
  color="Time", palette = "jco",
  line.color = "gray", linetype = "dashed",
  box.width = 0.7
) +
  facet_wrap(~ Group) +
  labs(
    title = "Paired Edema Measurements by Group and Time",
    x = NULL, # Remove x-axis label
    y = "Edema" # Adjust units as appropriate
  ) +
  theme_minimal(base_size = 30) + # Set base font size
  theme(
    axis.text.x = element_blank(),         # Remove x-axis tick labels
    axis.title.y = element_text(size = 22, face = "bold"), # Y axis label
    axis.text.y = element_text(size = 18), # Y axis ticks
    strip.text = element_text(size = 20, face = "bold"),   # Facet labels
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5), # Title
    legend.title = element_text(size = 20, face = "bold"), # Legend title
    legend.text = element_text(size = 18),                 # Legend text
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 1, colour = "black") # Add axis lines
  )+
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 8),           # Add tick marks
    labels = function(x) format(round(x, 0), nsmall = 0)
  )

ggsave(
  filename = "paired_plot_wide.png",
  plot = paired_plot,
  width = 14,      # Increase width (in inches)
  height = 10,      # Keep height reasonable
  dpi = 300        # High resolution for presentations
)


print(paired_plot)


#mixed ANOVA model
#res.aov <- anova_test(
#  data = data, dv = Edema_Post, wid = trueID,
#  between = Group, within = Edema_Pre
#  )
#get_anova_table(res.aov)
#ANCOVA_model:
#ancova_model <- aov(Edema_Post ~ Edema_Pre + Group + 
#    trueID + trueID*Edema_Pre, data = data)

#print(ancova_model$coefficients)



#T-test for 2 observations per subject
data <- data %>%
  mutate(Group = ifelse(Group == "Treatment", 1, 0)) %>% 
  select(-PatientID)

library(lme4)
#
#model1 = lm(Edema_Post ~ Group, data=data)
#summary(model1)
#print(model1)

model2 = lmer(Edema_Post ~ Edema_Pre + Group + (1|trueID), data=data)
summary(model2)
print(model2)
print(confint(model2))

#Assumptions check
data$residuals <- resid(model2)

# Normality of residuals
qqplot <- ggqqplot(data$residuals, title = "Q-Q Plot of Residuals")
print(qqplot)


# Homoscedasticity
resid_plot <-ggplot(data, aes(x = fitted(model2), y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuals vs Fitted Values for Edema_Post",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal()
print(resid_plot)