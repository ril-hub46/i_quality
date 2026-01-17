library(shiny)
library(readxl)
library(dplyr)
library(AER)
library(here)
library(stargazer)
library(gt)
library(ggrepel)
library(ggplot2)
library(here)

data <- read_excel(here("data", "processed", "Data.xlsx"))
data <- as.data.frame(data)

# Conversion
data <- data %>% mutate(
  `Main mortality estimate` = as.numeric(`Main mortality estimate`),
  Latitude <- as.numeric(Latitude)
)

data$`Main mortality estimate` = log(data$`Main mortality estimate`)
colnames(data)[colnames(data) == "Main mortality estimate"] <- "Log European settler mortality"

# Ajout PIB 2019
gdp_2019 <- c(
  14084.19953, 7533.511854, 23535.01275, 60574.6271, 34541.70749, 6025.087349,
  9091.927689, 15741.4785, 2233.261555, 4195.670175, 50498.97311, 25824.64653,
  16091.49905, 5766.754924, 22949.52866, 5947.924089, 19743.40045, 12540.32512,
  13608.49971, 9666.802079, 2274.214139, 16124.86386, 2371.492041, 5952.403657,
  11079.62731, 3143.646883, 13388.70608, 3220.942957, 5776.10388, 61221.41954,
  7181.522654, 12115.70207, 10117.56417, 4710.50374, 1652.49342, 29495.40352,
  2437.558036, 50327.68455, 21095.7137, 8024.405273, 45163.69643, 5819.315185,
  1389.164255, 5525.443784, 5206.907165, 33229.49603, 14117.16509, 13408.35324,
  3728.234533, 1623.650984, 105542.4136, 13706.71382, 14112.9525, 4123.92334,
  2947.126953, 2273.815278, 26641.38916, 12314.08594, 2444.248287, 25548.06338,
  65548.07078, 7046.0, 11190.1519, 1206.798113
)
data <- cbind(data, gdp_2019)
data$gdp_2019 <- log(data$gdp_2019) 
data$Latitude <- as.numeric(data$Latitude)

data <- data %>%
  mutate(
    log_gdp_2019 = log(`2019`), 
    log_gdp_1995 = log(`1995`),
    Latitude = abs(Latitude) / 90
  )

# Dummies
data$`asia_dummy` <- ifelse(data$`Former colonies` %in% c("India", "Bangladesh", "Malaysia", "Sri Lanka", "Pakistan", "Vietnam", "Indonesia", "Singapore", "Hong Kong"), 1, 0)
data$`africa_dummy` <- ifelse(data$`Former colonies`%in% c("Algeria", "Angola", "Egypt", "Kenya", "Nigeria", "Ethiopia", "Ghana", "South Africa", "Sudan", "Tanzania", "Uganda", "Zaire", "Morocco", "Tunisia", "Senegal", "Mali", "Burkina Faso", "Cameroon", "Côte d'Ivoire", "Gabon", "Guinea", "Sierra Leone", "Togo", "Gambia", "Madagascar", "Niger", "Congo (Brazzaville)"), 1, 0)
data$`neo_europes_dummy` <- ifelse(data$`Former colonies` %in% c("USA", "Canada", "Australia", "New Zealand"), 1, 0)
data$`other_regions_dummy` <- ifelse(data$`Former colonies` %in% c("Australia", "New Zealand", "Malta"), 1, 0)

base_sample_without_neo_europes <- subset(data, neo_europes_dummy == 0)
base_sample_without_africa <- subset(data, africa_dummy == 0)
base_sample_other_regions_dummy <- subset(data, other_regions_dummy == 0)

# --- Panel C: OLS (Table 4, Panel C) ---
ols_c1 <- lm(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995`, data = data)
ols_c2 <- lm(`Log GDP per capita(PPP) in 1995`~ `Average protection against expropriation risk 1985-1995`, data = data)
ols_c3 <- lm(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995`, data = base_sample_without_neo_europes)
ols_c4 <- lm(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995`, data = base_sample_without_neo_europes)
ols_c5 <- lm(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995`, data = base_sample_without_africa)
ols_c6 <- lm(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995`, data = base_sample_without_africa)
ols_c7 <- lm(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995`, data = base_sample_other_regions_dummy)
ols_c8 <- lm(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995`, data = base_sample_other_regions_dummy)
ols_c9 <- lm(log_gdp_1995 ~ `Average protection against expropriation risk 1985-1995`, data = data)

# --- Panel B: First Stage (Table 4, Panel B) ---
fs_b1 <- lm(`Average protection against expropriation risk 1985-1995` ~ `Log European settler mortality`, data = data)
fs_b2 <- lm(`Average protection against expropriation risk 1985-1995` ~ `Log European settler mortality` + `Latitude`, data = data)
fs_b3 <- lm(`Average protection against expropriation risk 1985-1995` ~ `Log European settler mortality`, data = base_sample_without_neo_europes)
fs_b4 <- lm(`Average protection against expropriation risk 1985-1995` ~ `Log European settler mortality` + `Latitude`, data = base_sample_without_neo_europes)
fs_b5 <- lm(`Average protection against expropriation risk 1985-1995` ~ `Log European settler mortality`, data = base_sample_without_africa)
fs_b6 <- lm(`Average protection against expropriation risk 1985-1995` ~ `Log European settler mortality` + `Latitude`, data = base_sample_without_africa)
fs_b7 <- lm(`Average protection against expropriation risk 1985-1995` ~ `Log European settler mortality` + `asia_dummy` + `africa_dummy` + `other_regions_dummy`, data = data)
fs_b8 <- lm(`Average protection against expropriation risk 1985-1995` ~ `Log European settler mortality` + `asia_dummy` + `africa_dummy` + `other_regions_dummy` + `Latitude`, data = data)
fs_b9 <- lm(`Average protection against expropriation risk 1985-1995` ~ `Log European settler mortality`, data = data)


# --- Panel A: 2SLS (IV) (Table 4, Panel A) ---
iv_a1 <- ivreg(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995` | `Log European settler mortality`, data = data)
iv_a2 <- ivreg(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995` + `Latitude` | `Log European settler mortality` + Latitude, data = data)
iv_a3 <- ivreg(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995` | `Log European settler mortality`, data = base_sample_without_neo_europes)
iv_a4 <- ivreg(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995` + Latitude | `Log European settler mortality` + Latitude, data = base_sample_without_neo_europes)
iv_a5 <- ivreg(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995` | `Log European settler mortality`, data = base_sample_without_africa)
iv_a6 <- ivreg(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995` + Latitude | `Log European settler mortality` + Latitude, data = base_sample_without_africa)
iv_a7 <- ivreg(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995` + asia_dummy + africa_dummy + other_regions_dummy | `Log European settler mortality` + asia_dummy + africa_dummy + other_regions_dummy, data = data)
iv_a8 <- ivreg(`Log GDP per capita(PPP) in 1995` ~ `Average protection against expropriation risk 1985-1995` + Latitude + asia_dummy + africa_dummy + other_regions_dummy | `Log European settler mortality` + Latitude + asia_dummy + africa_dummy + other_regions_dummy, data = data)
iv_a9 <- ivreg(log_gdp_1995 ~ `Average protection against expropriation risk 1985-1995` | `Log European settler mortality`, data = data)

# --- MODÈLES 2019 (Extension) ---
# OLS 2019
ols_2019_1 <- lm(gdp_2019 ~ `Average protection against expropriation risk 1985-1995`, data = data)
ols_2019_2 <- lm(gdp_2019 ~ `Average protection against expropriation risk 1985-1995` + `Latitude`, data = data)
ols_2019_3 <- lm(gdp_2019 ~ `Average protection against expropriation risk 1985-1995` , data = base_sample_without_neo_europes)
ols_2019_4 <- lm(gdp_2019 ~ `Average protection against expropriation risk 1985-1995` + `Latitude`, data = base_sample_without_neo_europes)
ols_2019_5 <- lm(gdp_2019 ~ `Average protection against expropriation risk 1985-1995`, data = base_sample_without_africa)
ols_2019_6 <- lm(gdp_2019 ~ `Average protection against expropriation risk 1985-1995` + `Latitude`, data = base_sample_without_africa)
ols_2019_7 <- lm(gdp_2019 ~ `Average protection against expropriation risk 1985-1995` + `asia_dummy` + `africa_dummy` + `other_regions_dummy`, data = data)
ols_2019_8 <- lm(gdp_2019 ~ `Average protection against expropriation risk 1985-1995` + `asia_dummy` + `africa_dummy` + `other_regions_dummy` + `Latitude` , data = data)
ols_2019_9 <- lm(log_gdp_2019 ~ `Average protection against expropriation risk 1985-1995`, data = data)

# IV 2SLS 2019
iv_2019_1 <- ivreg(gdp_2019 ~ `Average protection against expropriation risk 1985-1995` | `Log European settler mortality`, data = data)
iv_2019_2 <- ivreg(gdp_2019 ~ `Average protection against expropriation risk 1985-1995` + Latitude | `Log European settler mortality` + Latitude, data = data)
iv_2019_3 <- ivreg(gdp_2019 ~ `Average protection against expropriation risk 1985-1995` | `Log European settler mortality`, data = base_sample_without_neo_europes)
iv_2019_4 <- ivreg(gdp_2019 ~ `Average protection against expropriation risk 1985-1995` + Latitude | `Log European settler mortality` + Latitude, data = base_sample_without_neo_europes)
iv_2019_5 <- ivreg(gdp_2019 ~ `Average protection against expropriation risk 1985-1995` | `Log European settler mortality`, data = base_sample_without_africa)
iv_2019_6 <- ivreg(gdp_2019 ~ `Average protection against expropriation risk 1985-1995` + Latitude | `Log European settler mortality` + Latitude, data = base_sample_without_africa)
iv_2019_7 <- ivreg(gdp_2019 ~ `Average protection against expropriation risk 1985-1995` + asia_dummy + africa_dummy + other_regions_dummy | `Log European settler mortality` + asia_dummy + africa_dummy + other_regions_dummy, data = data)
iv_2019_8 <- ivreg(gdp_2019 ~ `Average protection against expropriation risk 1985-1995` + Latitude + asia_dummy + africa_dummy + other_regions_dummy | `Log European settler mortality` + Latitude + asia_dummy + africa_dummy + other_regions_dummy, data = data)
iv_2019_9 <- ivreg(log_gdp_2019 ~ `Average protection against expropriation risk 1985-1995` | `Log European settler mortality`, data = data)


graphique_ols <- ggplot(data, aes(x = `Average protection against expropriation risk 1985-1995`,
                                  y = `Log GDP per capita(PPP) in 1995`)) +

  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(color = "blue", alpha = 0.7) +
  geom_text_repel(
    aes(label = `Former colonies`),
    size = 3,
    max.overlaps = 10 
  ) +
  labs(
    title = "Figure 2: OLS RELATIONSHIP BETWEEN EXPROPRIATION RISK AND INCOME",
    x = "Average protection against expropriation risk 1985-1995",
    y = "Log GDP per capita(PPP) in 1995"
  ) +
  theme_minimal()
