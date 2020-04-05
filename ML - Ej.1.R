###Ejercicios S2
mnist <- read_mnist()
str(mnist)
mnist$train[["images"]]


library(tidyverse)
library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)
dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex)
x <- factor(dat$type)

table(dat)
26/39
42/111

y_hat <- ifelse(x == "online", "Male", "Female") %>% factor(levels = levels(y))
y_hat <- factor(y_hat)
mean(y_hat == dat$sex)

x_hat <- ifelse(x == "inclass", "Female", "Male") %>% factor(levels = levels(y))
x_hat <- factor(x_hat)
mean(x_hat == dat$sex)

table(y_hat, y)
confusionMatrix(data = y_hat, reference = y)


library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species
levels(y)

set.seed(2, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]
train
head(iris)


iris %>% group_by(Species) %>% summarize(mean(Sepal.Length), 
                                         mean(Sepal.Width), 
                                         mean(Petal.Length), 
                                         mean(Petal.Width))


foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica', 'versicolor')
    mean(y_hat == test$Species)
  })
}
predictions <- apply(test[,-5],2,foo)
sapply(predictions,max)

plot(iris,pch=21,bg=iris$Species)


set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))
summary(disease)
summary(test)

sum(test)/length(test)


library(dslabs)
data("heights")
  heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>% qplot(height, p, data =.)

ps <- seq(0, 1, 0.1)
heights %>% mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>% group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
plot(dat)

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)


###Section 3
library(tidyverse)
library(caret)
options(digits = 3)
set.seed(1, sample.kind="Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
plot(dat)

RMSE <- replicate(n, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- fit$coef[1] + fit$coef[2]*test_set$x
  sqrt(mean((y_hat - test_set$y)^2))
})

mean(RMSE)
sd(RMSE)



options(digits = 3)
n <- c(100, 500, 1000, 5000, 10000)
n <- 500
models <- function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  RMSE <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- fit$coef[1] + fit$coef[2]*test_set$x
    sqrt(mean((y_hat - test_set$y)^2))
  })
  c(mean = mean(RMSE), sd = sd(RMSE))
}
set.seed(1, sample.kind="Rounding")
X1 <- sapply(n, models)
X1



set.seed(1, sample.kind = "Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
RMSE <- replicate(n, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- fit$coef[1] + fit$coef[2]*test_set$x
  sqrt(mean((y_hat - test_set$y)^2))
})

mean(RMSE)
sd(RMSE)



set.seed(1, sample.kind = "Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)

set.seed(1, sample.kind = "Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))




library(tidyverse)
library(caret)
set.seed(2, sample.kind="Rounding")
make_data <- function(mu_1 = 2, n = 1000, p = 0.5, 
                      mu_0 = 0, sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()
head(dat$test)
dat$train %>% ggplot(aes(x, color = y)) + geom_density()





mu_1 <- seq(0, 3, len=25)
set.seed(1, sample.kind="Rounding")
dat <- lapply(mu_1, make_data)

L_reg <- function(){
  glm_fit <- glm(y ~ x, data = dat$train, family = "binomial")
  p_hat_logit <- predict(glm_fit, newdata = dat$test, type = "response")
  y_hat_logit <- ifelse(p_hat_logit > 0.5, "1", "0") %>% factor
  confusionMatrix(y_hat_logit, dat$test$y)$overall[["Accuracy"]]  
}

map_df(dat, L_reg)


































































































