library(dslabs)
data(heights)
heights
class(heights$sex)
nrow(heights)

heights[777, 1]
max(heights$height)
which.min(heights$height)
median(heights$height)

sum(heights$sex=="Male")/nrow(heights)


heights %>% filter(height > c(78)) %>% group_by(sex) %>% summary()


##Sección 2
library(tidyverse)
library(caret)
library(dslabs)
data(heights)
y <- heights$sex 
x <- heights$height

set.seed(2007)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) 
##Selecciona arbitrariamente una fila (time=1) del dataset "y", que contiene el 50% de los datos (p=0.5)
##del mismo.

test_set <- heights[test_index, ]
##Resultados para entrenar.
train_set <- heights[-test_index, ]
##Resultados a predecir.


y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))
##Creamos el primer algoritmo. Su función es adivinar los datos. Convertimos a factor los niveles
##para usar la función summary.
summary(y_hat)
mean(y_hat == test_set$sex)



heights %>% group_by(sex) %>% summarize(mean(height), sd(height))
##Revisamos las alturas de ambos sexos para crear un vector logico que prediga mejor las alturas
##cuando es hombre.

y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)
##El modelo acierta al 79% de los datos.


cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
##Creamos una funcion que maximice el porcentaje del modelo anterior.
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff


y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
##Usamos solo el conjunto test_set y no x ya que contiene los datos del test, no de pruebas.
##Por eso baja de 0.84 a 0.80 el promedio de haciertos.
##x <- heights$height
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

table(V_Predicho = y_hat, V_Real = test_set$sex)
##Matriz de confución con el valor real y el predicho por el algoritmo.

test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))
##Notamos el porcentaje de aciertos de cada variable.

prev <- mean(y == "Male")
prev
##Debido a la prevalencia (cant.de veces que aparece Male) de hombres en nuestro dataset. 
##Determinamos que heights esta sesgado(desproporcional).

cm <- confusionMatrix(data = y_hat, reference = test_set$sex)
cm
##Conocemos la sensibilidad y especificidad del dataset.

cm$overall["Accuracy"] 
##Muestra la exactitud con la que esta prediciendo nuestro modelo.

cm$byClass[c("Sensitivity","Specificity", "Prevalence")]
##Muestra la sensibilidad, especificidad y prevalencia del mismo.


cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})
##Ajustamos el modelo teniendo en cuenta el F1-Score.
max(F_1)
best_cutoff <- cutoff[which.max(F_1)]
best_cutoff


y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
##Y ya tenemos el primer algoritmo de Machine Learning que necesita altura como predictor 
##y predice mujeres si tienes 65 pulgadas o menos.
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)




###Section 3
library(HistData)
library(tidyverse)
library(caret)
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

m <- mean(train_set$son)
# squared loss
mean((m - test_set$son)^2)

# fit linear regression model
fit <- lm(son ~ father, data = train_set)
fit$coef
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)

library(tidyverse)
library(caret)


library(dslabs)
data("heights")
y <- heights$height
y
set.seed(2, sample.kind = "Rounding")

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

train_set %>% 
  filter(round(height)==66) %>%
  summarize(y_hat = mean(sex=="Female"))

heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point()

lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>% lm(y ~ height, data = .)
p_hat <- predict(lm_fit, test_set)
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat, test_set$sex)$overall["Accuracy"]


glm_fit <- train_set %>% 
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y ~ height, data=., family = "binomial")
p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")
y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]]
























































































































