# Carregando bibliotecas
library(tidyverse)
library(lubridate)
library(naniar)
library(VIM)
library(corrplot)
library(psych)
library(inspectdf)
library(mlbench)
library(ModelMetrics)
library(caret)
library(forcats)
library(h2o)
library(nortest)
library(lmtest)
library(VIM)
library(psych)
library(mice)

# Importando as bases de dados
retail <- read.csv("C:/Users/Pichau/Documents/MBA/Analise Preditiva Avancada/Analise_Preditiva_Avancada-master/Trabalho Final/Retail.csv",header=TRUE, sep=",", encoding='UTF-8')

# Analisando a estrutura do dataset
str(retail2)

# Computando algumas Estatisticas descritivas
summary(retail)
describe(retail)

# A primeira observacao de Weekly_Sales e "#N/D"
table(retail$Weekly_Sales) 

# Data Cleaning

# Substituindo #N/D de Weekly_Sales por NA
retail <- retail %>% replace_with_na(replace = list(Weekly_Sales = c("#N/D")))

# Checando NA's
sapply(retail, function(x) sum(is.na(x)))

# Existem muitos NA's nas variaveis Markdown

# Excluindo NA's de Weekly_Sales, 
retail <- retail %>% filter(!is.na(Weekly_Sales))

retail$Weekly_Sales <- as.numeric(retail$Weekly_Sales)

# Transformando IsHoliday para o formato factor
retail$IsHoliday <- ifelse(retail$IsHoliday == TRUE, 1,0)

# # Transformando Weekly_Sales para o formato numeric
retail$Weekly_Sales <- as.numeric(retail$Weekly_Sales)

# Categorizando a variavel numerica week com base na media e no terceiro quadrante
summary(retail$Weekly_Sales)

bins <- c(-Inf, (0.95*summary(retail$Weekly_Sales)[3]), (1.05*summary(retail$Weekly_Sales)[5]), Inf)

bin_names <- c("Low", "Medium", "High")

retail$week_type <- cut(retail$Weekly_Sales, breaks = bins, labels = bin_names)

# Imputando NA's com KNN
retailknn <- VIM::kNN(retail, k = 6, imp_var=FALSE)
str(retailknn)
summary(retailknn)

# Transformando Date para o formato Date
retailknn$Date <- dmy(retailknn$Date)
retailknn$week <- week(retailknn$Date)
retailknn$year <- year(retailknn$Date)

# Aplicando cyclical encoding na variavel Date para que o modelo entenda a propriedade ciclica da variavel de tempo
retailknn$Date <- cyclic_encoding(
  retailknn$Date,
  periods = c("day", "week", "month", "year"),
  encoders = c("sin", "cos")
)

# A seguir foi feito uma analise exploratoria utilizando a base retailknn

###
# Analise Exploratoria
###

# Computando algumas Estatisticas descritivas
summary(retailknn)
describe(retailknn)

# Plotando Histogramas
ggplot(data=retailknn, aes(x=Temperature)) +
  geom_histogram()

ggplot(data=retailknn, aes(x=Fuel_Price)) +
  geom_histogram()

ggplot(data=retailknn, aes(x=MarkDown1)) +
  geom_histogram()

ggplot(data=retailknn, aes(x=MarkDown2)) +
  geom_histogram()

ggplot(data=retailknn, aes(x=MarkDown3)) +
  geom_histogram()

ggplot(data=retailknn, aes(x=MarkDown4)) +
  geom_histogram()

ggplot(data=retailknn, aes(x=MarkDown5)) +
  geom_histogram()

# As variaveis Markdown sao enviesadas a esquerda

ggplot(data=retailknn, aes(x=CPI)) +
  geom_histogram()

ggplot(data=retailknn, aes(x=Unemployment)) +
  geom_histogram()

# Analisando a variavel resposta
ggplot(data=retailknn, aes(x=Weekly_Sales)) +
  geom_histogram()

describe(retailknn$Weekly_Sales)

# com log
ggplot(data=retailknn, aes(x=log(Weekly_Sales))) +
  geom_histogram()

describe(log(retailknn$Weekly_Sales))

# Outliers
ggplot(data=retailknn, aes(y=Temperature)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)
ggplot(data=retailknn, aes(y=Fuel_Price)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)
ggplot(data=retailknn, aes(y=MarkDown1)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) # outliers
ggplot(data=retailknn, aes(y=MarkDown2)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) # outliers
ggplot(data=retailknn, aes(y=MarkDown3)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) # outliers
ggplot(data=retailknn, aes(y=MarkDown4)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) # outliers
ggplot(data=retailknn, aes(y=MarkDown5)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) # outliers
ggplot(data=retailknn, aes(y=CPI)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)
ggplot(data=retailknn, aes(y=Unemployment)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) 

# A seguir foi feito um plot para entender melhor a distribuição da variavel vendas ao longo das semanas, 
# a fim de definir um intervalo para categorizar a variavel week (semana).

# Obtendo a venda media por semana no ano de 2011.
# Este ano foi escolhido por ser um ano completo com todas os meses presentes no dataset
week_sales2011 <- retailknn %>% 
  filter(year==2011) %>% 
  group_by(week) %>% 
  summarize(week_sum = sum(Weekly_Sales))

# Definindo os intervalos numericos
bins <- c(-Inf, (0.95*summary(week_sales2011$week_sum)[3]
), (1.05*summary(week_sales2011$week_sum)[5]), Inf)

# Definindo os intervalos categoricos
bin_names <- c("Low", "Medium", "High")

# Criando as categorias
week_sales2011$week_type <- cut(week_sales2011$week_sum, breaks = bins, labels = bin_names)

# Visualizando a soma das vendas semanais colorindo por tipos de semana ao longo das semanas do ano de 2011
ggplot(data=week_sales2011, aes(x = week,y=week_sum, fill = week_type)) +
  geom_col()

# Pode-se observar que as vendas apresentam alguns picos durante o ano e no final apresenta o maior pico de vendas, indicando que
# esses picos de vendas podem ocorrer por serem datas festivas
str(retailknn)

# Observando a correlacao
str(numerics)
numerics <- select_if(retailknn, is.numeric)

correlacao <- cor(numerics)
corrplot(correlacao)

index <- findCorrelation(correlacao, cutoff = 0.70, verbose = FALSE)
head(numerics[,index]) 

# Outra forma de visualizar as correlacoes
df_cor  %>% inspect_cor(alpha=0.05)  %>% arrange(desc(p_value))  %>% show_plot()

# nenhuma variavel autocorrelacionada


# Base de validacao com NA's de Markdown imputados com KNN
set.seed(314)
options(warn=-1)
trainIndex <- createDataPartition(retailknn$Weekly_Sales,p=.7,list=FALSE)
trainknn <- retailknn[trainIndex,]
testknn <- retailknn[-trainIndex,]

# Modelagem KNN
# Regressao Linear 
set.seed(314)
str(trainknn)

tc <- trainControl(method="repeatedcv", number=10, savePredictions = T)
model_lm <- caret::train(Weekly_Sales~ ., method = "lm", data = trainknn, trControl = tc)
summary(model_lm)
model_lm[4]

#$results
#intercept     RMSE  Rsquared      MAE   RMSESD RsquaredSD    MAESD
#1      TRUE 204232.9 0.8713346 160705.8 17004.96 0.01506791 5857.684

tc <- trainControl(method="repeatedcv", number=10, savePredictions = T)
model_lm <- caret::train(Weekly_Sales ~ week + Temperature + MarkDown3 + MarkDown5 + week_type, method = "lm", data = trainknn, trControl = tc)
summary(model_lm)
model_lm[4]

# Adjusted R-squared:  0.2426

# Predict
pred <- predict(model_lm, testknn)
ModelMetrics::rmse(testknn$Weekly_Sales, pred)
# RMSE na base de teste : 206445

model_lm[4]
#$results
#intercept     RMSE  Rsquared      MAE   RMSESD RsquaredSD    MAESD
#1      TRUE 207683.5 0.8659382 164060.7 11482.72 0.01599791 5386.186

# Arvore de Decisoes
set.seed(314)
tc <- trainControl(method="repeatedcv", number=10, savePredictions = T)


model_tree <- caret::train(Weekly_Sales~ ., data = trainknn, method = "treebag", trControl= tc)
summary(model_tree)
model_tree[4]

#$results
#parameter     RMSE  Rsquared      MAE   RMSESD RsquaredSD    MAESD
#1      none 207365.9 0.8668983 164949.3 11338.24 0.01058079 4048.232

# Predict
pred <- predict(model_tree, testknn)
ModelMetrics::rmse(testknn$Weekly_Sales, pred)

# RMSE na base de teste 202963.6

set.seed(314)
model_tree2 <- caret::train(Weekly_Sales~ ., data = trainknn, method = "ranger", trControl= tc) 

# o modelo com todas as variaveis (model_tree2) possui RMSE menor e R^2 mais alto

summary(model_tree2)
summary(model_tree3)

model_tree2[4] # diminui o RMSE em relacao ao regressao linear

plot(model_tree2, plotType = "level")

# com splitrule = variance, mtry = 16 e min.node.size = 5 se obtem o melhor modelo
tgrid <- expand.grid(
  mtry = 23,
  splitrule = "extratrees",
  min.node.size = 5)

# melhor modelo
model_tree3 <- caret::train(Weekly_Sales~ ., data = trainknn ,method = "ranger", trControl= tc, tuneGrid = tgrid)
summary(model_tree3)
model_tree3[4]

#$results
#mtry  splitrule min.node.size     RMSE  Rsquared      MAE   RMSESD  RsquaredSD    MAESD
#1   23 extratrees             5 101905.7 0.9679055 60103.77 7862.361 0.004581154 2433.499

# Predict
pred <- predict(model_tree3, testknn)
ModelMetrics::rmse(testknn$Weekly_Sales, pred)

#RMSE na base de teste:93347.49

# Salvando a base como csv no diretorio para importar como h2o 
setwd("C:/Users/Pichau/Documents/MBA/Analise Preditiva Avancada/Analise_Preditiva_Avancada-master/Trabalho Final")

write.csv(retailknn, "retailknn.csv")

# Inicializando h2o
h2o.init()
options(warn=-1)
set.seed(314)

# Importando como h2o
retailknn <- h2o.importFile("C:/Users/Pichau/Documents/MBA/Analise Preditiva Avancada/Analise_Preditiva_Avancada-master/Trabalho Final/retailknn.csv")

# Redes Neurais
# Separando 70% para base de treino
class(retailknn)
df_split <- h2o.splitFrame(data=retailknn, ratios=0.7)

train <- df_split[[1]]
test <- df_split[[2]]

# definindo variaveis x e y 
y <- "Weekly_Sales"
x <- setdiff(names(train), y)

# Modelo mais simples
set.seed(314)
model <- h2o.deeplearning(y = y,
                          training_frame = train,
                          validation_frame = test,
                          seed = 314,
                          distribution = "gaussian",
                          epochs=30,
                          l1 = 1e-5,
                          activation = "RectifierWithDropout",
                          hidden = c(50,50),
                          hidden_dropout_ratios = c(0.2,0.2))

perf_test <- h2o.performance(model, newdata = test)
perf_test

#MSE:  39016613513
#RMSE:  197526.2  rmse menor que o modelo de regressao
#MAE:  154471.8
#RMSLE:  0.2933434
#Mean Residual Deviance :  39016613513

h2o.r2(model) # r^2: 0.8813095 pouco melhor que o modelo de regressao


# modelo mais complexo
model <- h2o.deeplearning(y = y,
                          training_frame = train,
                          validation_frame = test,
                          distribution = "gaussian",
                          hidden = c(100, 80, 100),
                          adaptive_rate = TRUE,
                          activation = "Rectifierwithdropout",
                          l1 = 1e-5,
                          epochs = 100)
model

# testando na base de teste
perf_test <- h2o.performance(model, newdata = test)
perf_test
#MSE:  60580454704
#RMSE:  246131  aumentou de 197526 para 246131
#MAE:  204712
#RMSLE:  0.3598253
#Mean Residual Deviance :  60580454704

# modelo mais complexo
model1 <- h2o.deeplearning(y = y,
                          training_frame = train,
                          validation_frame = test,
                          distribution = "gaussian",
                          hidden = c(1000, 1000),
                          adaptive_rate = TRUE,
                          activation = "Rectifierwithdropout",
                          l1 = 1e-5,
                          epochs = 100)
model1

# testando na base de teste
perf_test1 <- h2o.performance(model1, newdata = test)
perf_test1
#MSE:  35004942836
#RMSE:  187096.1
#MAE:  146836.5
#RMSLE:  0.2972896
#Mean Residual Deviance :  35004942836

# Criei um modelo com 4 camadas com quantidades de neuronios progressivas
set.seed(314)
model2 <- h2o.deeplearning(y = y,
                          training_frame = train,
                          validation_frame = test,
                          seed = 314,
                          distribution = "gaussian",
                          hidden = c(100, 300, 600, 1000),
                          adaptive_rate = TRUE,
                          activation = "Rectifier",
                          l1 = 1e-5,
                          epochs = 100)
model2
plot(model2)

perf_test2 <- h2o.performance(model2, newdata = test)
perf_test2

#H2ORegressionMetrics: deeplearning

#MSE:  15567477035
#RMSE:  124769.7
#MAE:  84773.52
#RMSLE:  0.1555802
#Mean Residual Deviance :  15567477035

# Dobrei os neuronios em cada camada do ultimo modelo
model3 <- h2o.deeplearning(y = y,
                          training_frame = train,
                          validation_frame = test,
                          seed = 314,
                          distribution = "gaussian",
                          hidden = c(500, 500, 500, 500),
                          adaptive_rate = TRUE,
                          activation = "Rectifier",
                          l1 = 1e-5,
                          epochs = 100)

#MSE:  15403192879
#RMSE:  124109.6
#MAE:  84189.51
#RMSLE:  0.156849
#Mean Residual Deviance :  15403192879

set.seed(314)

# Dobrei os neuronios em cada camada do ultimo modelo
model4 <- h2o.deeplearning(y = y,
                          training_frame = train,
                          validation_frame = test,
                          seed = 314,
                          distribution = "gaussian",
                          hidden = c(200, 400, 1200, 2000),
                          adaptive_rate = TRUE,
                          activation = "Rectifier",
                          l1 = 1e-5,
                          epochs = 100)

perf_test4 <- h2o.performance(model4, newdata = test)
perf_test4
#MSE:  12095428894
#RMSE:  109979.2
#MAE:  69167.01
#RMSLE:  0.126023
#Mean Residual Deviance :  12095428894

h2o.r2(model4) # r^2: 0.9863969 melhor modelo encontrado entre os modelos de regressao e arvore de decisao

# aparentemente se aumentarmos os neuronios nesta proporcao o modelo melhore, porem o custo computacional 
# sera demasiadamente grande para o proposito deste trabalho

predh2o <- h2o.predict(model4)


# Conclusao da comparacao dos modelos

# Regressao Logistica

#$results
#intercept     RMSE  Rsquared      MAE   RMSESD RsquaredSD    MAESD
#1      TRUE 207683.5 0.8659382 164060.7 11482.72 0.01599791 5386.186

# RMSE na base de teste : 206445

# Tree Bag
#$results
#parameter     RMSE  Rsquared      MAE   RMSESD RsquaredSD    MAESD
#1      none 207365.9 0.8668983 164949.3 11338.24 0.01058079 4048.232

# RMSE na base de teste 202963.6

# Ranger
#$results
#mtry  splitrule min.node.size     RMSE  Rsquared      MAE   RMSESD  RsquaredSD    MAESD
#1   23 extratrees             5 101905.7 0.9679055 60103.77 7862.361 0.004581154 2433.499

#RMSE na base de teste:93347.49

# h2o.deeplearning
#MSE:  12095428894
#RMSE:  109979.2
#MAE:  69167.01
#RMSLE:  0.126023
#Mean Residual Deviance :  12095428894

# r^2: 0.9863969 melhor modelo encontrado entre os modelos de regressao e arvore de decisao


# Com base nos resultados obtidos acima, o modelo de redes neurais com 4 camadas e estas respectivas quantidades
# neuronios em cada uma: 200, 400, 1200, 2000) é o melhor modelo com base no R^2 (coeficiente de determinacao).
# Com base no RMSE o melhor modelo é o de arvore de decisao com metodo Ranger (randomforest).






