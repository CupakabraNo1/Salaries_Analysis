# data loading
install.packages("readr")
library(readr)

salaries_raw <- read_csv("./assets/salaries.csv")
View(salaries_raw)

# data preprocessing
str(salaries_raw)
source("preprocessing.R")
salaries_raw <- preprocessData(salaries_raw)

salaries_raw$Fired <- preprocessFired(salaries_raw$Fired)
salaries_raw$Size <- preprocessSize(salaries_raw$Size)
salaries_raw$Other_Technologies <- preprocessOtherTechnologies(salaries_raw$Other_Technologies)

# data visualization

source("visualization.R")
install.packages('ggplot2')
install.packages('plotrix')
library(ggplot2)
library(plotrix)
visualizationOfMetrics(salaries_raw$Age[complete.cases(salaries_raw$Age)], "Age")
visualizationOfMetrics(salaries_raw$Bruto[complete.cases(salaries_raw$Bruto)], "Bruto")
visualizationOfMetrics(salaries_raw$Total[complete.cases(salaries_raw$Total)], "Total")
visualizationOfMetrics(salaries_raw$Total_Last_Year[complete.cases(salaries_raw$Total_Last_Year)], "Total_Last_Year")


visulizePie(salaries_raw$Gender, "Pie chart of gender")

visulizeLolipop(salaries_raw$Seniority, "Seniorities", "Percent ( % )")
visulizePie(salaries_raw$Size, "Pie chart of size")
visulizeBar(salaries_raw$Position, "Positions", "Percent ( % )")
visualizeSexEquality(salaries_raw[salaries_raw$Gender %in% c("Male", "Female"),])

column <- salaries_raw$Position



      