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
str(data)

'''
data <- data %>%
  mutate(
    log_gdp_2019 = log(`2019`), 
    log_gdp_1995 = log(`1995`),
    Latitude = Latitude / 90
  )
head(data)
View(data)'''