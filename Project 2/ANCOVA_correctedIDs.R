library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(report)
library(gridExtra)
library(knitr)
library(kableExtra)
library(broom.mixed)

#DATA
#note: change the path for your own computer
data <- read.csv("/Users/pilardaronco/Code/MATH527-Tutorials-1/Project 2/Data/EdemaData.csv", 
    header = TRUE, sep = ",") 
#cleaning data
data <- data %>% mutate(trueID = ifelse(PatientID <= 90, 
    PatientID, PatientID - 90))
#print(data)


#visualization
#reg. boxplot for gains
bxpdata <- data %>% 
  select(Group, Edema_Pre, Edema_Post) %>%
  mutate(Gains = Edema_Post - Edema_Pre) %>%
  select(-Edema_Pre, -Edema_Post)

bxp <- ggboxplot(
  bxpdata, x = "Group", y = "Gains",
  color = "Group", palette = "jco",
  )  +
  labs(
    title = "Change in Edema by Group (Post - Pre)",
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
    labels = function(x) format(round(x, 1), nsmall = 1)
  )

ggsave(
  filename = "Gains_boxplot.png",
  plot = bxp,
  width = 14,      # Increase width (in inches)
  height = 10,      # Keep height reasonable
  dpi = 300        # High resolution for presentations
)
print(bxp)


#paired plots

pivotData <- data %>%
  pivot_longer(
    cols = starts_with("Edema"),
    names_to = "Time",
    values_to = "Edema"
  ) %>% select(-PatientID)


y_limits <- range(pivotData$Edema, na.rm = TRUE)

print(pivotData)

datatreatment <- pivotData %>% filter(Group == "Treatment")
dataplacebo <- pivotData %>% filter(Group == "Placebo")

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
  pivotData, x = "Time", y = "Edema", id = "trueID",
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


#Model results table
# Tidy summary and confidence intervals for model2
model2_summary <- broom.mixed::tidy(model2, conf.int = TRUE)

# Select and rename columns for presentation
table_data <- model2_summary %>%
  select(term, estimate, std.error, statistic, conf.low, conf.high) %>%
  rename(
    Term = term,
    Estimate = estimate,
    `Std. Error` = std.error,
    `t value` = statistic,
    `CI Lower` = conf.low,
    `CI Upper` = conf.high
  )

# Create a neat, presentable table
# Save the presentable table as an HTML file for PowerPoint or Word
table_html <- kable(table_data, digits = 3, caption = "Model Coefficients and Confidence Intervals") %>%
  kable_styling(full_width = FALSE, font_size = 18, position = "center") %>%
  row_spec(0, bold = TRUE)

save_kable(table_html, "model2_table.html")


#Assumptions check
data$residuals <- resid(model2)

# Normality of residuals
qqplot <- ggqqplot(
  data$residuals,
  title = "Q-Q Plot of Model Residuals"
) +
  theme_minimal(base_size = 30) +
  theme(
    axis.title.x = element_text(size = 22, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 1, colour = "black")
  )
ggsave(
  filename = "qqplot.png",
  plot = qqplot,
  width = 14,      # Increase width (in inches)
  height = 14,      # Keep height reasonable
  dpi = 300        # High resolution for presentations
)
print(qqplot)


# Homoscedasticity
resid_plot <- ggplot(data, aes(x = fitted(model2), y = residuals)) +
  geom_point(size = 3, color = "#0073C2FF") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Residuals vs Fitted Values for Post-Edema values",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 30) +
  theme(
    axis.title.x = element_text(size = 22, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 1, colour = "black")
  )

  ggsave(
  filename = "residual plot.png",
  plot = resid_plot,
  width = 14,      # Increase width (in inches)
  height = 10,      # Keep height reasonable
  dpi = 300        # High resolution for presentations
)

print(resid_plot)