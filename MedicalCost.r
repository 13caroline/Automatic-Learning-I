# Library used to plot 
library(ggplot2)
# Library used to assist ggplot2 library
library(cowplot)

Health <- read.csv("C:\\Users\\f7car\\Desktop\\UM\\CD\\AA1\\Projeto\\insurance.csv")
Health <- read.csv("/home/dreamerz/Desktop/CD/Aprendizagem Automática I/Trabalho/insurance.csv")

dim(Health)
attach(Health)

plot(Health)
summary(Health)

# ------------------------------------------------------------------------------
# Analise Exploratoria dos Dados - Regressao Linear
# ------------------------------------------------------------------------------

hist(charges,
     main="Histogram for Variable Charges", 
     xlab="Charges", 
     border="#000000", 
     col="#EDB879"
)

par(mfrow=c(2,2))

counts <- table(sex)
barplot(counts,xlab = "Sex", col=c("#EDB879", "#69BDD2")) 

smoke <- table(smoker)
barplot(smoke,xlab = "Smoker",col=c("#EDB879", "#69BDD2"))

regiao <- table(region)
barplot(regiao,xlab = "Region",col=c("#EDB879", "#69BDD2", "#D06363", "#80CC80"))

filhos <- table(children)
barplot(filhos,xlab = "Children", col=c("#EDB879", "#69BDD2", "#D06363", "#80CC80"))

par(mfrow=c(1,2))
boxplot(bmi, xlab = "Bmi", col=c("#EDB879"))
boxplot(charges, xlab = "Charges", col=c("#EDB879"))


##### Analise Exploratoria dos Dados com a Variavel de Interesse

x <- ggplot(Health, aes(age, charges)) +
  geom_jitter(aes(color = age), alpha = 0.5) +
  theme_light()

y <- ggplot(Health, aes(bmi, charges)) +
  geom_jitter(color = "orange", alpha = 0.5) +
  theme_light()

p2 <- plot_grid(x, y)
title <- ggdraw() + draw_label("Correlation between charges and bmi/age", fontface='bold')
plot_grid(title, p2, ncol=1, rel_heights=c(0.1, 1))

ggplot(data=Health, mapping = aes(x = bmi, y = charges)) + 
  geom_point(aes(color = smoker)) +
  theme_bw()

par(mfrow=c(2,2))
boxplot(charges ~ smoker, col=c("#EDB879","#69BDD2"))
boxplot(charges ~ children, col=c("#EDB879", "#69BDD2", "#D06363", "#80CC80"))
boxplot(charges ~ sex, col=c("#EDB879","#69BDD2"))
boxplot(charges ~ region, col=c("#EDB879", "#69BDD2", "#D06363", "#80CC80"), xlab="Region")

# ------------------------------------------------------------------------------
# Ajuste do Modelo de Regressao Linear
# ------------------------------------------------------------------------------

# Modelo com todos os preditores 
m1 <- lm(charges ~., Health)
summary(m1)    # Adjusted R-squared:  0.7494 
extractAIC(m1) # 23316.43
coef(m1)
confint(m1)

# Modelo sem genero 
m2 <- lm(charges ~.-sex, Health)
summary(m2)    # Adjusted R-squared:  0.7496 
extractAIC(m2) # 23314.58
coef(m2)
confint(m2)
confint(m2,level=0.97)

# Modelo sem genero e regioes norte agrupadas 
Health_north <- Health
Health_north$region<-ifelse(Health_north$region == "northwest","north",
            ifelse(Health_north$region == "northeast" ,"north",
            ifelse(Health_north$region == "southwest","southwest",
            ifelse(Health_north$region == "southeast","southeast","erro"
            ))))

detach(Health)
attach(Health_north)

m3 <- lm(charges ~.-sex, Health_north)
summary(m3)    # Adjusted R-squared:  0.7497
extractAIC(m3) # 23313.13
coef(m3)
confint(m3)

# Modelo sem regiao e genero 
m4 <- lm(charges ~.-(sex+region), Health)
summary(m4)    # Adjusted R-squared:  0.7489
extractAIC(m4) #23314.96
coef(m4)
confint(m4)
confint(m4, level=0.99)

# ------------------------------------------------------------------------------
# Preparar Dataset para Predicao
# ------------------------------------------------------------------------------

Health <- read.csv("C:\\Users\\f7car\\Desktop\\UM\\CD\\AA1\\Projeto\\insurance.csv")
Health <- read.csv("/home/dreamerz/Desktop/CD/Aprendizagem Automática I/Trabalho/insurance.csv")
Health_outlier <- Health
Health_north <- Health
Health_north$region<-ifelse(Health_north$region == "northwest","north",
                    ifelse(Health_north$region == "northeast" ,"north",
                    ifelse(Health_north$region == "southwest","southwest",
                    ifelse(Health_north$region == "southeast","southeast","erro"
                    ))))

attach(Health)

par(mfrow=c(1,2))
boxplot(bmi, xlab = "Bmi", col=c("#EDB879"))
boxplot(charges, xlab = "Charges", col=c("#EDB879"))

#Possiveis outliers
boxplot.stats(bmi)$out
boxplot.stats(charges)$out

################################# BMI Outliers ################################# 

# Linhas dos possiveis outliers bmi
outlier_bmi <- boxplot.stats(bmi)$out
outlier_bmi_line <- which(bmi %in% c(outlier_bmi))
outlier_bmi_line
Health_outlier[outlier_bmi_line, ]
 
# Remover possiveis outliers de bmi
Health_outlier <- Health_outlier[-outlier_bmi_line, ]

# Remover entradas baseadas num valor de bmi (considerar como outlier)
Health_outlier<- Health_outlier[Health_outlier$bmi > 30, ]

############################### Charges Outliers ############################### 

# Linhas dos possiveis outliers charges
outlier_charges <- boxplot.stats(charges)$out
outlier_charges_line <- which(charges %in% c(outlier_charges))
outlier_charges_line
Health_outlier[outlier_charges_line, ]

# Remover possiveis outliers de charges
Health_outlier <- Health_outlier[-outlier_charges_line, ]

# Remover entradas baseadas num valor de bmi (considerar como outlier)
Health_outlier <- Health_outlier[Health_outlier$charges < 50000, ]
Health <- Health[Health$charges < 50000, ]
Health_north <- Health_north[Health_north$charges < 50000, ]

# ------------------------------------------------------------------------------
# Treino e Teste do modelo
# Dividir os dados em treino (80%) + teste (20%)
# ------------------------------------------------------------------------------

set.seed(1)

# Obter linhas que serao usadas para treino
train <- sample(1:nrow(Health), round(0.8 * nrow(Health)))

#Obter dados de treino
Health_train <- Health[train,]
Health_train_north <- Health_north[train,]
dim(Health_train)
fix(Health_train)
fix(Health_train_north)

#Obter dados de teste
Health_test <- Health[-train,]
Health_test_north <- Health_north[-train,]
dim(Health_test)
fix(Health_test)
fix(Health_test_north)

# Guarda os valores reais dos charges dos dados de teste 
# para comparar no fim do treino
valores_reais <- Health_test$charges
valores_reais_north <- Health_test_north$charges

###################################### M1 ######################################

# Treinar para o modelo m1 com todos os preditores
model_train_m1 <- lm(charges ~., data = Health_train)

summary(model_train_m1)    # Adjusted R-squared:  0.7479
extractAIC(model_train_m1) # 18611.12

test_result_m1 <- predict(model_train_m1,Health_test)
residuals_m1 <- Health_test$charges - test_result_m1
rmse_m1 <- sqrt(mean(residuals_m1^2))

Health_test$predicted_m1 <- predict(model_train_m1, Health_test)
ggplot(Health_test, aes(x = predicted_m1, y = charges)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Predicted vs. Real values")


# Testar accuracy com base em erro predefinido
Health_test$Accuracy_m1 <- ifelse(abs(Health_test$predicted_m1 - Health_test$charges) < 1000,1,0)
Health_test$Accuracy_m1 <- as.factor(Health_test$Accuracy_m1)
levels(Health_test$Accuracy_m1) <- c("no","yes")

summary(Health_test$Accuracy_m1) # no - 208; yes - 60

###################################### M2 ######################################

# Treinar para o modelo m2 com todos os preditores exceto genero
model_train_m2 <- lm(charges ~.-sex, data = Health_train)

summary(model_train_m2)    # Adjusted R-squared:  0.7481
extractAIC(model_train_m2) # 18609.13

test_result_m2 <- predict(model_train_m2,Health_test)
residuals_m2 <- Health_test$charges - test_result_m2
rmse_m2 <- sqrt(mean(residuals_m2^2))

Health_test$predicted_m2 <- predict(model_train_m2, Health_test)
ggplot(Health_test, aes(x = predicted_m2, y = charges)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Predicted vs. Real values")

# Testar accuracy com base em erro predefinido
Health_test$Accuracy_m2 <- ifelse(abs(Health_test$predicted_m2 - Health_test$charges) < 1000,1,0)
Health_test$Accuracy_m2 <- as.factor(Health_test$Accuracy_m2)
levels(Health_test$Accuracy_m2) <- c("no","yes")

summary(Health_test$Accuracy_m2) # no - 208; yes - 60

fix(Health_test)
fix(Health_train)

###################################### M3 ######################################

# Treinar para o modelo m3 com nortes agrupados e removendo o preditor genero
model_train_m3 <- lm(charges ~.-sex, data = Health_train_north)

summary(model_train_m3)    # Adjusted R-squared:  0.7483
extractAIC(model_train_m3) # 18607.19

test_result_m3 <- predict(model_train_m3,Health_test_north)
residuals_m3 <- Health_test_north$charges - test_result_m3
rmse_m3 <- sqrt(mean(residuals_m3^2))

Health_test_north$predicted_m3 <- predict(model_train_m3, Health_test_north)
ggplot(Health_test_north, aes(x = predicted_m3, y = charges)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Predicted vs. Real values")


# Testar accuracy com base em erro predefinido
Health_test_north$Accuracy_m3 <- ifelse(abs(Health_test_north$predicted_m3 - Health_test_north$charges) < 1000,1,0)
Health_test_north$Accuracy_m3 <- as.factor(Health_test_north$Accuracy_m3)
levels(Health_test_north$Accuracy_m3) <- c("no","yes")

summary(Health_test_north$Accuracy_m3) # no - 207; yes - 61

fix(Health_test_north)
fix(Health_train_north)

###################################### M4 ######################################

# Treinar para o modelo m4 com todos os preditores exceto genero e regiao
model_train_m4 <- lm(charges ~.-(sex+region), data = Health_train)

summary(model_train_m4)    # Adjusted R-squared:  0.748
extractAIC(model_train_m4) # 18606.51

test_result_m4 <- predict(model_train_m4,Health_test)
residuals_m4 <- Health_test$charges - test_result_m4
rmse_m4 <- sqrt(mean(residuals_m4^2))

Health_test$predicted_m4 <- predict(model_train_m4, Health_test)
ggplot(Health_test, aes(x = predicted_m4, y = charges)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Predicted vs. Real values")

# Testar accuracy com base em erro predefinido
Health_test$Accuracy_m4 <- ifelse(abs(Health_test$predicted_m4 - Health_test$charges) < 1000,1,0)
Health_test$Accuracy_m4 <- as.factor(Health_test$Accuracy_m4)
levels(Health_test$Accuracy_m4) <- c("no","yes")

summary(Health_test$Accuracy_m4) # no - 204; yes - 64

fix(Health_test)
fix(Health_train)

# ------------------------------------------------------------------------------
# K-Fold e LOOCV
# ------------------------------------------------------------------------------

library(tidyverse)
library(caret)

Health <- read.csv("C:\\Users\\f7car\\Desktop\\UM\\CD\\AA1\\Projeto\\insurance.csv")
Health <- read.csv("/home/dreamerz/Desktop/CD/Aprendizagem Automática I/Trabalho/insurance.csv")
fix(Health)
attach(Health)

set.seed(1)

############################ K-Fold e LOOCV para M1 ############################

train_control <- trainControl(method = "cv", number = 5)
model_kf_m1 <- train(charges ~., data = Health, method = "lm",trControl = train_control)
print(model_kf_m1)

train_control <- trainControl(method = "LOOCV")
model_loocv_m1 <- train(charges ~., data = Health, method = "lm",trControl = train_control)
print(model_loocv_m1)

############################ K-Fold e LOOCV para M2 ############################

train_control <- trainControl(method = "cv", number = 5)
model_kf_m2 <- train(charges ~.-sex, data = Health, method = "lm",trControl = train_control)
print(model_kf_m2)

train_control <- trainControl(method = "LOOCV")
model_loocv_m2 <- train(charges ~.-sex, data = Health, method = "lm",trControl = train_control)
print(model_loocv_m2)

############################ K-Fold e LOOCV para M3 ############################

Health_aux <- Health
Health_aux$region<-ifelse(Health_aux$region == "northwest","north",
                   ifelse(Health_aux$region == "northeast" ,"north",
                   ifelse(Health_aux$region == "southwest","southwest",
                   ifelse(Health_aux$region == "southeast","southeast","erro"
                   ))))

fix(Health_aux)
attach(Health_aux)

train_control <- trainControl(method = "cv", number = 5)
model_kf_m3 <- train(charges ~.-sex, data = Health_aux, method = "lm",trControl = train_control)
print(model_kf_m3)

train_control <- trainControl(method = "LOOCV")
model_loocv_m3 <- train(charges ~.-sex, data = Health_aux, method = "lm",trControl = train_control)
print(model_loocv_m3)

############################ K-Fold e LOOCV para M4 ############################

train_control <- trainControl(method = "cv", number = 5)
model_kf_m4 <- train(charges ~.-(sex+region), data = Health, method = "lm",trControl = train_control)
print(model_kf_m4)

train_control <- trainControl(method = "LOOCV")
model_loocv_m4 <- train(charges ~.-(sex+region), data = Health, method = "lm",trControl = train_control)
print(model_loocv_m4)

# ------------------------------------------------------------------------------
# KNN
# ------------------------------------------------------------------------------

library(FNN)
library(class)

Health_knn <- read.csv("C:\\Users\\f7car\\Desktop\\UM\\CD\\AA1\\Projeto\\insurance.csv", stringsAsFactors = TRUE)
Health_knn <- read.csv("/home/dreamerz/Desktop/CD/Aprendizagem Automática I/Trabalho/insurance.csv", stringsAsFactors = TRUE)

set.seed(1)

Health_knn$smoker <- ifelse(Health_knn$smoker == "yes", 1, 0)
Health_knn$sex <- ifelse(Health_knn$sex == "male", 1, 0)
Health_knn$region<-ifelse(Health_knn$region == "northwest",1,
                   ifelse(Health_knn$region == "northeast" ,2,
                   ifelse(Health_knn$region == "southwest",3,
                   ifelse(Health_knn$region == "southeast",4,0
                   ))))

fix(Health_knn)

train <- sample(1:nrow(Health_knn), round(0.8 * nrow(Health_knn)))

Health_knn_train <- Health_knn[train,]
Health_knn_teste <- Health_knn[-train,]
fix(Health_knn_teste)

# Fit a KNN regression with k = 1
# using the knn.reg() function from the FNN package
knn_charges <- knn.reg(train=Health_knn_train[,-Health_knn_train$charges],   
                    y=Health_knn_train$charges, 
                    test= Health_knn_teste[,-Health_knn_teste$charges],
                    k=1)

summary(knn_charges)
knn_charges$pred

Health_knn_teste$predicted <- knn_charges$pred

Health_knn_teste$Accuracy <- ifelse(abs(Health_knn_teste$predicted - Health_knn_teste$charges) < 100,1,0)
Health_knn_teste$Accuracy <- as.factor(Health_knn_teste$Accuracy)
levels(Health_knn_teste$Accuracy) <- c("no","yes")

summary(Health_knn_teste$Accuracy)

fix(Health_knn_teste)

plot(Health_knn_teste$charges)
lines(Health_knn_teste$predicted)
plot(Health_knn_teste$charges ~ Health_knn_teste$predicted, xlab="Valores Obtidos", ylab="Valores Reais")

knn_rmse  <- sqrt(mean((Health_knn_teste$predicted - Health_knn_teste$charges)^2) ) 
knn_rmse

# ------------------------------------------------------------------------------
# Alteracao da variavel de interesse 
# ------------------------------------------------------------------------------

Health <- read.csv("C:\\Users\\f7car\\Desktop\\UM\\CD\\AA1\\Projeto\\insurance.csv")
Health <- read.csv("/home/dreamerz/Desktop/CD/Aprendizagem Automática I/Trabalho/insurance.csv")

newdata <- Health
new_charges <- log(Health$charges)
newdata$charges <- log(newdata$charges)
#newdata <- data.frame(cbind(new_charges,Health))
fix(newdata)
attach(newdata)

hist(charges,
     main="Histogram for New Variable Charges", 
     xlab="Charges' Logarithm", 
     border="#000000", 
     col="#EDB879"
    )

x <- ggplot(newdata, aes(age, charges)) +
  geom_jitter(aes(color = age), alpha = 0.5) +
  theme_light()

y <- ggplot(newdata, aes(bmi, charges)) +
  geom_jitter(color = "orange", alpha = 0.5) +
  theme_light()

p2 <- plot_grid(x,y)
title <- ggdraw() + draw_label("Correlation between new_charges and bmi/age", fontface='bold')
plot_grid(title, p2, ncol=1, rel_heights=c(0.1, 1))

par(mfrow=c(2,2))
boxplot(charges ~ smoker, col=c("#EDB879","#69BDD2"))
boxplot(charges ~ children, col=c("#EDB879", "#69BDD2", "#D06363", "#80CC80"))
boxplot(charges ~ sex, col=c("#EDB879","#69BDD2"))
boxplot(charges ~ region, col=c("#EDB879", "#69BDD2", "#D06363", "#80CC80"), xlab="Region")

# ------------------------------------------------------------------------------
# Ajuste do modelo 
# ------------------------------------------------------------------------------

newdata_north <- newdata
fix(Health)
fix(newdata_north)
fix(newdata)
summary(Health)
summary(newdata)
summary(newdata_north)

# Modelo com todos os preditores 

mod1 <- lm(charges ~., newdata)
summary(mod1)    # Adjusted R-squared:  0.7666 
extractAIC(mod1) # -2162.046
coef(mod1)
confint(mod1) 


# Modelo com regioes norte agrupadas 

newdata_north$region<-ifelse(newdata_north$region == "northwest","north",
            ifelse(newdata_north$region == "northeast" ,"north",
            ifelse(newdata_north$region == "southwest","southwest",
            ifelse(newdata_north$region == "southeast","southeast","erro"
            ))))
fix(newdata_north)

mod2 <- lm(charges ~., newdata_north)
summary(mod2)    # Adjusted R-squared:  0.7661
extractAIC(mod2) # -2160.688
coef(mod2)
confint(mod2) 

# ------------------------------------------------------------------------------
# Treino e Teste do modelo
# Dividir os dados em treino (80%) + teste (20%)
# ------------------------------------------------------------------------------

set.seed(1)

# Obter linhas que serao usadas para treino
train <- sample(1:nrow(newdata), round(0.8 * nrow(newdata)))

#Obter dados de treino
newdata_train <- newdata[train,]
newdata_train_north <- newdata_north[train,]
dim(newdata_train)
dim(newdata_train_north)
fix(newdata_train)
fix(newdata_train_north)

#Obter dados de teste
newdata_test <- newdata[-train,]
newdata_test_north <- newdata_north[-train,]
dim(newdata_test)
dim(newdata_test_north)
fix(newdata_test)
fix(newdata_test_north)

##################################### Mod1 #####################################

# Treinar para o modelo m1 com todos os preditores
model_train_mod1 <- lm(charges ~., data = newdata_train)

summary(model_train_mod1) 
extractAIC(model_train_mod1) 

test_result_mod1 <- predict(model_train_mod1,newdata_test)
residuals_mod1 <- newdata_test$charges - test_result_mod1
rmse_mod1 <- sqrt(mean(residuals_mod1^2))

newdata_test$predicted_mod1 <- predict(model_train_mod1, newdata_test)
ggplot(newdata_test, aes(x = predicted_mod1, y = charges)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Predicted vs. Real values")

# Testar accuracy com base em erro pre-difinido
# Erro como 0.30 ou 0.5
newdata_test$Accuracy_mod1 <- ifelse((abs(newdata_test$charges - newdata_test$predicted_mod1) < 0.30),1,0)
newdata_test$Accuracy_mod1 <- as.factor(newdata_test$Accuracy_mod1)
levels(newdata_test$Accuracy_mod1) <- c("no","yes")

# Erro atraves da razao
newdata_test$Accuracy_mod1 <- ifelse((((abs(newdata_test$charges - newdata_test$predicted_mod1)) / (newdata_test$charges)) < 0.30),"true","false")
newdata_test$Accuracy_mod1 <- as.factor(newdata_test$Accuracy_mod1)

summary(newdata_test$Accuracy_mod1)

fix(newdata_test)
fix(newdata_train)

##################################### Mod2 #####################################

# Treinar para o modelo m2 com todos os preditores mas north juntos
model_train_mod2 <- lm(charges ~., data = newdata_train_north)

summary(model_train_mod2) 
extractAIC(model_train_mod2) 

test_result_mod2 <- predict(model_train_mod2,newdata_test_north)
residuals_mod2 <- newdata_test_north$charges - test_result_mod2
rmse_mod2 <- sqrt(mean(residuals_mod2^2))

newdata_test_north$predicted_mod2 <- predict(model_train_mod2, newdata_test_north)
ggplot(newdata_test_north, aes(x = predicted_mod2, y = charges)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Predicted vs. Real values")

# Testar accuracy com base em erro pre-difinido
# Erro como 0.3 ou 0.5
newdata_test_north$Accuracy_mod2 <- ifelse((abs(newdata_test_north$charges - newdata_test_north$predicted_mod2) < 0.30),1,0)
newdata_test_north$Accuracy_mod2 <- as.factor(newdata_test_north$Accuracy_mod2)
levels(newdata_test_north$Accuracy_mod2) <- c("no","yes")

summary(newdata_test_north$Accuracy_mod2)

fix(newdata_test_north)
fix(newdata_train_north)

# Erro atraves da razao
newdata_test_north$Accuracy_mod2 <- ifelse((((abs(newdata_test_north$charges - newdata_test_north$predicted_mod2)) / (newdata_test_north$charges)) < 0.30),"true","false")
newdata_test_north$Accuracy_mod2 <- as.factor(newdata_test_north$Accuracy_mod2)

summary(newdata_test_north$Accuracy_mod2)

fix(newdata_test_north)
fix(newdata_train_north)

# ------------------------------------------------------------------------------
# K-Fold e LOOCV
# ------------------------------------------------------------------------------

library(tidyverse)
library(caret)

Health <- read.csv("C:\\Users\\f7car\\Desktop\\UM\\CD\\AA1\\Projeto\\insurance.csv")
Health <- read.csv("/home/dreamerz/Desktop/CD/Aprendizagem Automática I/Trabalho/insurance.csv")
newdata <- Health
new_charges <- log(Health$charges)
newdata$charges <- log(newdata$charges)
newdata_north <- newdata
newdata_north$region<-ifelse(newdata_north$region == "northwest","north",
                      ifelse(newdata_north$region == "northeast" ,"north",
                      ifelse(newdata_north$region == "southwest","southwest",
                      ifelse(newdata_north$region == "southeast","southeast","erro"
                      ))))
fix(newdata_north)
fix(newdata)

set.seed(1)

########################### K-Fold e LOOCV para Mod1 ###########################

train_control <- trainControl(method = "cv", number = 5)
model_kf_mod1 <- train(charges ~., data = newdata, method = "lm",trControl = train_control)
print(model_kf_mod1)

train_control <- trainControl(method = "LOOCV")
model_loocv_mod1 <- train(charges ~., data = newdata, method = "lm",trControl = train_control)
print(model_loocv_mod1)

########################### K-Fold e LOOCV para Mod2 ###########################

train_control <- trainControl(method = "cv", number = 5)
model_kf_mod2 <- train(charges ~., data = newdata_north, method = "lm",trControl = train_control)
print(model_kf_mod2)

train_control <- trainControl(method = "LOOCV")
model_loocv_mod2 <- train(charges ~., data = newdata_north, method = "lm",trControl = train_control)
print(model_loocv_mod2)

# ------------------------------------------------------------------------------
# KNN
# ------------------------------------------------------------------------------

library(FNN)
library(class)

Health_knn <- read.csv("C:\\Users\\f7car\\Desktop\\UM\\CD\\AA1\\Projeto\\insurance.csv", stringsAsFactors = TRUE)
Health_knn <- read.csv("/home/dreamerz/Desktop/CD/Aprendizagem Automática I/Trabalho/insurance.csv", stringsAsFactors = TRUE)

set.seed(1)
Health_knn$charges <- log(Health_knn$charges)
Health_knn$smoker <- ifelse(Health_knn$smoker == "yes", 1, 0)
Health_knn$sex <- ifelse(Health_knn$sex == "male", 1, 0)
Health_knn$region<-ifelse(Health_knn$region == "northwest",1,
                   ifelse(Health_knn$region == "northeast",2,
                   ifelse(Health_knn$region == "southwest",3,
                   ifelse(Health_knn$region == "southeast",4,0
                   ))))
summary(Health_knn$charges)
fix(Health_knn)

normalize <- function(x) { (x-min(x)) / (max(x)-min(x))}
test <- Health_knn$charges
Health_knn <- as.data.frame(lapply(Health_knn[,c(1:6)],normalize))
Health_knn$charges <- test

train <- sample(1:nrow(Health_knn), round(0.8 * nrow(Health_knn)))

Health_knn_train <- Health_knn[train,]
Health_knn_teste <- Health_knn[-train,]
fix(Health_knn_teste)

# Fit a KNN regression with k = 3
# using the knn.reg() function from the FNN package
knn_charges <- knn.reg(train=Health_knn_train[,-Health_knn_train$charges],   
                       y=Health_knn_train$charges, 
                       test= Health_knn_teste[,-Health_knn_teste$charges],
                       k=8)

summary(knn_charges)
knn_charges$pred

Health_knn_teste$predicted <- knn_charges$pred

# Erro pre-definido 
Health_knn_teste$Accuracy <- ifelse(abs(Health_knn_teste$predicted - Health_knn_teste$charges) < 0.2,1,0)
Health_knn_teste$Accuracy <- as.factor(Health_knn_teste$Accuracy)
levels(Health_knn_teste$Accuracy) <- c("no","yes")

summary(Health_knn_teste$Accuracy)

Health_knn_teste$graph_maior <- Health_knn_teste$predicted + 0.2
Health_knn_teste$graph_menor <- Health_knn_teste$predicted - 0.2
plot(Health_knn_teste$charges)
plot(Health_knn_teste$predicted,col="green")
lines(Health_knn_teste$graph_maior,col="red")
lines(Health_knn_teste$graph_menor,col="red")

plot(Health_knn_teste$charges ~ Health_knn_teste$predicted)

knn_rmse  <- sqrt(mean((Health_knn_teste$predicted - Health_knn_teste$charges)^2) ) 
knn_rmse

fix(Health_knn_teste)

# Razao
Health_knn_teste$Accuracy <- ifelse(abs(Health_knn_teste$predicted - Health_knn_teste$charges) / Health_knn_teste$charges < 0.05,"true","false")
Health_knn_teste$Accuracy <- as.factor(Health_knn_teste$Accuracy)
levels(Health_knn_teste$Accuracy) <- c("no","yes")

summary(Health_knn_teste$Accuracy)

#fix(Health_knn_teste)

plot(Health_knn_teste$charges)
lines(Health_knn_teste$predicted)
plot(Health_knn_teste$charges ~ Health_knn_teste$predicted, xlab="Valores Obtidos", ylab="Valores Reais")

knn_rmse  <- sqrt(mean((Health_knn_teste$predicted - Health_knn_teste$charges)^2) ) 
knn_rmse

################################################################################
# Justificacao resposta 2
################################################################################

Health_child <- Health[Health$children > 3, ]
dim(Health_child)
attach(Health_child)

Health_aux2 <- Health_child[Health_child$smoker == "no",  ]
Health_aux2 <- Health_aux2[,-5]
dim(Health_aux2)
attach(Health_aux2)

m <- lm(charges ~., Health_aux2)
summary(m) # p_value children: 0.011

################################################################################
# Influencia do tabagismo na significancia do bmi para variavel charges
################################################################################

Health_aux3 <- Health[Health$smoker == "no",  ]
attach(Health_aux3)
Health_aux3 <- Health_aux3[,-5]
mnosmoker <- lm(charges ~., Health_aux3)
summary(mnosmoker)    # p_value bmi:  0.439265

Health_aux5 <- Health[Health$smoker == "yes",  ]
attach(Health_aux5)
Health_aux5 <- Health_aux5[,-5]
msmoker <- lm(charges ~., Health_aux5)

summary(msmoker)    # p_value bmi:  <2e-16