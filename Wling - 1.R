getwd()
setwd("/Users/cfleitas/Desktop/R/Evaluacion01")

path <- system.file("extdata", package="dslabs")
list.files(path)
filename <- "murders.csv"
fullpath <- file.path(path, filename)
fullpath
file.copy(fullpath, getwd())
file.exists(filename)


library(dslabs)
library(tidyverse)
library(readxl)
read_lines("murders.csv", n_max = 3)

dat <- read_csv(fullpath)
head(dat)

filename1 <- "life-expectancy-and-fertility-two-countries-example.csv"
filename2 <- "fertility-two-countries-example.csv"
dat=read.csv(file.path(path, filename))
dat1=read.csv(file.path(path, filename1))
dat2=read.csv(file.path(path, filename2))

race <- read.csv("murders.csv", stringsAsFactors = F)
class(race)

race1 <- read_csv("murders.csv", stringsAsFactors = F)
head(race1)
class(race$state)

url <- "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"

read_csv(url, col_names = FALSE)
length(Pr)

library(tidyverse)
library(dslabs)
data(gapminder)

tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
head(tidy_data)

path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)


new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)
head(new_tidy_data)

head(dat_wide)
??dat_wide




















