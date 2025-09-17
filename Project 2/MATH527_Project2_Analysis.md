MATH527_Project2_Analysis
================
Weixin Xiao
2025-09-14

``` r
data <- read.csv("EdemaData - Copy.csv") %>%
  mutate(Delta = Edema_Post - Edema_Pre)

head(data) %>%
  kable()
```

| PatientID | Edema_Pre | Edema_Post | Group   |      Delta |
|----------:|----------:|-----------:|:--------|-----------:|
|         1 |  9.900971 |   9.555952 | Placebo | -0.3450189 |
|         2 |  9.147286 |   7.707519 | Placebo | -1.4397672 |
|         3 | 10.638069 |  10.335972 | Placebo | -0.3020964 |
|         4 | 10.482560 |  10.149593 | Placebo | -0.3329674 |
|         5 |  6.809553 |   7.423108 | Placebo |  0.6135552 |
|         6 |  9.319809 |   9.142365 | Placebo | -0.1774437 |

``` r
summary_stats <- data %>%
  group_by(Group) %>%
  summarise(
    N = n(),
    Pre_Mean = mean(Edema_Pre),
    Pre_SD = sd(Edema_Pre),
    Post_Mean = mean(Edema_Post),
    Post_SD = sd(Edema_Post),
    Delta_Mean = mean(Delta),
    Delta_SD = sd(Delta)
  )

summary_stats %>%
  kable(digits = 3)
```

| Group     |   N | Pre_Mean | Pre_SD | Post_Mean | Post_SD | Delta_Mean | Delta_SD |
|:----------|----:|---------:|-------:|----------:|--------:|-----------:|---------:|
| Placebo   |  90 |    8.997 |  1.456 |     8.717 |   1.515 |     -0.280 |    0.652 |
| Treatment |  90 |    8.924 |  1.315 |     8.408 |   1.302 |     -0.516 |    0.651 |

``` r
data_long <- data %>%
  pivot_longer(cols = c(Edema_Pre, Edema_Post), names_to = "Time", values_to = "Edema")

ggplot(data_long, aes(x = Time, y = Edema, fill = Group)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Edema Levels by Time and Group", x = "Time Point", y = "Edema Level") +
  scale_fill_manual(values = c("Placebo" = "#ADD8E6", "Treatment" = "#90EE90"))
```

![](MATH527_Project2_Analysis_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
ggplot(data, aes(x = Group, y = Delta, fill = Group)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Change in Edema (Post - Pre)", x = "Group", y = "Delta") +
  scale_fill_manual(values = c("Placebo" = "#ADD8E6", "Treatment" = "#90EE90"))
```

![](MATH527_Project2_Analysis_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->

``` r
t_pre <- t.test(Edema_Pre ~ Group, data = data)
tidy(t_pre) %>%
  select(estimate1, estimate2, statistic, p.value) %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  kable(col.names = c("Placebo Mean", "Treatment Mean", "t", "p-value"))
```

| Placebo Mean | Treatment Mean |     t | p-value |
|-------------:|---------------:|------:|--------:|
|        8.997 |          8.924 | 0.355 |   0.723 |

``` r
t_delta <- t.test(Delta ~ Group, data = data, var.equal = FALSE)
tidy(t_delta) %>%
  select(estimate1, estimate2, statistic, p.value) %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  kable(col.names = c("Placebo Delta", "Treatment Delta", "t", "p-value"))
```

| Placebo Delta | Treatment Delta |     t | p-value |
|--------------:|----------------:|------:|--------:|
|         -0.28 |          -0.516 | 2.424 |   0.016 |

``` r
model <- lm(Edema_Post ~ Edema_Pre + Group, data = data)
tidy(model) %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  kable()
```

| term           | estimate | std.error | statistic | p.value |
|:---------------|---------:|----------:|----------:|--------:|
| (Intercept)    |    0.547 |     0.319 |     1.716 |   0.088 |
| Edema_Pre      |    0.908 |     0.035 |    26.237 |   0.000 |
| GroupTreatment |   -0.242 |     0.096 |    -2.535 |   0.012 |
