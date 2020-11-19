# Instalando bibliotecas
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
library(mice)
library(pROC)

# Importando a base de dados
marketing <- read.csv("C:/Users/Pichau/Documents/MBA/Analise Preditiva Avancada/Analise_Preditiva_Avancada-master/Trabalho Final/Marketing.csv",header=TRUE, sep=";", encoding='UTF-8')
str(marketing)

# Observando os levels das seguintes colunas
varlist <- c("JOB", "MARITAL_STATUS", "EDUCATION","CONTACT", "HOUSING","LOAN", "SUBSCRIBED", "DEFAULT")
lapply(marketing[varlist], table)

sapply(marketing, function(x) sum(is.na(x)))

# Transformando em factor
varlist2 <- c("JOB", "MARITAL_STATUS","CONTACT","DEFAULT","HOUSING","LOAN","MONTH","DAY_OF_WEEK","POUTCOME", "SUBSCRIBED")
marketing[varlist2] <- lapply(marketing[varlist2], as.factor)

# Tratando valores para a variável 'DEFAULT'
3/41188
#Como não existe categoria "desconhecida" para o pagamento de contas atrasasadas e o número de observações de 
#inadimplentes ('yes) é extremamente baixo, iremos considerar todos os 'unknow' como 'no'.
marketing$DEFAULT <- gsub("unknown", "no", marketing$DEFAULT)

# Tratando valores para a variável 'HOUSING'
# Como o valor 'unknow' representa um NA e não podemos atribuir como sim ou não essas observações serão removidas 
# da base, 990 observações serão removidas. 
# Os valores 'unknown' para LOAN foram removidos junto com as observaçõe da variável 'HOUSING'
marketing <- marketing %>% filter(HOUSING != "unknown")

# # Tratando valores para a variável 'EDUCATION'
# MARKETING <- MARKETING %>% filter(EDUCATION != "unknown")

# mudar o nome das colunas (_ para .) e transformar em numerico
table(marketing$EMP_VAR_RATE)

# substituindo underline em EMP_VAR_RATE
marketing$EMP_VAR_RATE <- gsub("_", ".", marketing$EMP_VAR_RATE)
marketing$EMP_VAR_RATE <- as.numeric(marketing$EMP_VAR_RATE)

# Substituindo underline em CONS_CONF_IDX
marketing$CONS_CONF_IDX <- gsub("_", ".", marketing$CONS_CONF_IDX)
marketing$CONS_CONF_IDX <- as.numeric(marketing$CONS_CONF_IDX)

# mudar o nome das colunas (_ para .) 
table(marketing$CONS_PRICE_IDX)

# transformando em numerico
marketing$CONS_PRICE_IDX <- recode(marketing$CONS_PRICE_IDX, "93_2" = "93.2")
                                  
marketing$CONS_PRICE_IDX <- as.numeric(marketing$CONS_PRICE_IDX)

table(marketing$CONS_PRICE_IDX)

# Transformando EDUCATION em um factor ordenado 
marketing$EDUCATION <- factor(marketing$EDUCATION, order = TRUE, 
                                    levels = c("unknown", "illiterate", "basic_4y", "basic_6y", "basic_9y", "high_school", "professional_course", "university_degree"))
class(marketing$EDUCATION)

# Analise Exploratoria

# Estatistica descritivas
summary(marketing)

# Plotando as variveis categoricas em grafico de barras
ggplot(marketing, aes(x=fct_infreq(JOB), fill = SUBSCRIBED)) + geom_bar() + coord_flip() 
# admin, blue collar e technician são as tres maiores da cetegoria
# em valores abolutos, a variavel admin possui mais potenciais que aderiram ao servico

ggplot(marketing, aes(x=fct_infreq(MARITAL_STATUS), fill = SUBSCRIBED)) + geom_bar() + coord_flip()
# married possui mais observacoes que as demais
# em valores relativos a variavel single possui mais inscritos, já em valores absolutos married possui mais inscritos

ggplot(marketing, aes(x=fct_infreq(EDUCATION), fill = SUBSCRIBED)) + geom_bar() + coord_flip()
# a maior parte dos clientes da base possui university_degree e high_school

ggplot(marketing, aes(x=fct_infreq(CONTACT), fill = SUBSCRIBED)) + geom_bar() + coord_flip()
# a maior parte dos contatos e feito por celular

ggplot(marketing, aes(x=fct_infreq(DEFAULT), fill = SUBSCRIBED)) + geom_bar() + coord_flip()
# praticamento nenhum cliente possui contas atrasadas 

ggplot(marketing, aes(x=fct_infreq(HOUSING), fill = SUBSCRIBED)) + geom_bar() + coord_flip()
# existe um numero equilibrado entre quem tem e quem nao tem hipoteca, e também entre quem aderiu ou nao ao servico

ggplot(marketing, aes(x=fct_infreq(LOAN), fill = SUBSCRIBED)) + geom_bar() + coord_flip()
# a maior parte nao possui emprestimo pessoal

ggplot(marketing, aes(x=fct_infreq(MONTH), fill = SUBSCRIBED)) + geom_bar()
# a maior parte dos potenciais clientes foram atingidos pelas campanhas em maio, julho e agosto
# o mes com mais aderencia ao servico foi maio, em valores absolutos
# em valores relativos o mes abril foi o que teve mais aderencia ao servico


# Plotando as variveis discretas
ggplot(marketing, aes(x=AGE, fill = SUBSCRIBED)) + geom_density()
# a idade dos potenciais clientes se concetra em 25 e 55 anos
# onde quem é mais novo aderiu mais ao sevico que os mais velhos

ggplot(marketing, aes(x=DURATION, fill = SUBSCRIBED)) + geom_density()
# parece que campanha com maior duracao conseguiram uma maior aderencia ao servico

ggplot(marketing, aes(x=EMP_VAR_RATE, fill = SUBSCRIBED)) + geom_bar()
# a taxa de desemprego nao parece influenciar diretamento a taxa de aderencia ao servico 

# outliers
ggplot(marketing, aes(x=AGE, fill = SUBSCRIBED)) + geom_boxplot()
# nota outliers em torno de 75 anos para cima, porem de acordo com o grafico de densidade acima
# pessoas com mais de 75 anos tbm aderiram ao servico, por isso nao foram retirados estes outliers da base

ggplot(marketing, aes(x=DURATION, fill = SUBSCRIBED)) + geom_boxplot()
ggplot(marketing, aes(x=DURATION, fill = SUBSCRIBED)) + geom_density()
# apesar de existirem outliers, campanhas com maior duração tambem apresentaram significativa 
# aderencia ao servico de acordo com o grafico de densidade, por isso nao serao retirados da base

# nonexistent
table(marketing$POUTCOME)

pout_yes_subs <- marketing %>% filter(POUTCOME=="nonexistent" & SUBSCRIBED == "yes") #
dim(pout_yes_subs) # 3141 

pout_no_subs <- marketing %>% filter(POUTCOME=="nonexistent" & SUBSCRIBED == "no") #
dim(pout_no_subs) # 32422 

# obtendo os valores numericos para cada grupo de categorias sumarizando por aderencia
marketing %>% group_by(MARITAL_STATUS) %>% summarise(sum(SUBSCRIBED=="yes"))
marketing %>% group_by(JOB) %>% summarise(sum(SUBSCRIBED=="yes"))
marketing %>% group_by(EDUCATION) %>% summarise(sum(SUBSCRIBED=="yes"))
marketing %>% group_by(HOUSING) %>% summarise(sum(SUBSCRIBED=="yes"))
marketing %>% group_by(DEFAULT) %>% summarise(sum(SUBSCRIBED=="yes"))
marketing %>% group_by(CONTACT) %>% summarise(sum(SUBSCRIBED=="yes"))

# aqui confirma-se o observado nos graficos acima



str(marketing)

# Dividindo em Treino e teste
set.seed(314)
trainIndex <- createDataPartition(marketing$SUBSCRIBED, p = .7, list = FALSE)
dfTrain <- marketing[ trainIndex,]
dfTest  <- marketing[-trainIndex,]
str(dfTest)

# Criando base de teste com a variavel resposta no formato numerico para calcular ROC e AUC
dfTest_num <- dfTest
dfTest_num$SUBSCRIBED <- ifelse(dfTest$SUBSCRIBED == "yes",1,0)

# Modelagem de dados

# Regressao Logistica
set.seed(314)
tc <- trainControl(method="repeatedcv", number=10, savePredictions = T, summaryFunction = twoClassSummary, classProbs = TRUE)

model_glm1 <- caret::train(SUBSCRIBED~., data = dfTrain, method = "glm", metric="ROC", trControl= tc, control = list(maxit = 50))
model_glm1[4]
summary(model_glm1)

#$results
#parameter       ROC      Sens      Spec       ROCSD      SensSD     SpecSD
#1      none 0.9275943 0.9734838 0.4001488 0.006213152 0.001688337 0.03396477

model_glm2 <- caret::train(SUBSCRIBED~MONTH+EDUCATION+JOB+ CONTACT+DAY_OF_WEEK+DURATION+CAMPAIGN+POUTCOME+EMP_VAR_RATE+ CONS_PRICE_IDX+CONS_CONF_IDX, data = dfTrain, method = "glm", metric="ROC", trControl= tc, control = list(maxit = 50))
model_glm2[4]
summary(model_glm2)

# $results
# parameter       ROC      Sens      Spec       ROCSD      SensSD     SpecSD
# 1      none 0.9331338 0.9726828 0.4152441 0.005203187 0.003645929 0.03709518

dfProbs <- predict(model_glm2,dfTest,type="prob", SCALE=FALSE)

# Grafico das probabilidades
ggplot(dfProbs, aes(x=yes)) + geom_density() 
ggplot(dfProbs, aes(x=no)) + geom_density() # cutoff proximo de 0.9

dfPref <- ifelse(dfProbs[1] > 0.85, "no", "yes")
dfPref <- as.factor(dfPref)
confusionMatrix(dfPref, as.factor(dfTest$SUBSCRIBED), positive = "yes")

# Calculando ROC e AUC
dfPref_num<- ifelse(dfPref== "yes",1,0)


#treino
#ROC        Sens       Spec     
#0.9343589  0.9724826  0.4247079

#Teste
roc <- pROC::roc(dfTest_num$SUBSCRIBED, dfPref_num)
# AUC
auc(roc) 
# Area under the curve: 0.8423
plot(roc)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction   no  yes
# no  9402  264
# yes 1297 1095
# 
# Accuracy : 0.8705          
# 95% CI : (0.8644, 0.8765)
# No Information Rate : 0.8873          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.514           
# 
# Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.80574         
#             Specificity : 0.87877         
#          Pos Pred Value : 0.45778         
#          Neg Pred Value : 0.97269         
#              Prevalence : 0.11271         
#          Detection Rate : 0.09081         
#    Detection Prevalence : 0.19837         
#       Balanced Accuracy : 0.84226         
#                                           
#        'Positive' Class : yes 

dfPref <- ifelse(dfProbs[1] > 0.9, "no", "yes")
dfPref <- as.factor(dfPref)
confusionMatrix(dfPref, as.factor(dfTest$SUBSCRIBED), positive = "yes")

#Confusion Matrix and Statistics

#Reference
#Prediction  no   yes
#       no   9008  137   #FN 137   VN 9008     # captura menos negativos, mas erra menos negativos
#       yes  1691  1222  #FP 1691  VP 1222     # captura mais positivos, mas erra mais positivos

#Accuracy : 0.8484          
#95% CI : (0.8419, 0.8548)
#No Information Rate : 0.8873          
#P-Value [Acc > NIR] : 1               

#Kappa : 0.4944          

#Mcnemar's Test P-Value : <2e-16          
                                          
#            Sensitivity : 0.8992          
#            Specificity : 0.8419          
#         Pos Pred Value : 0.4195          
#         Neg Pred Value : 0.9850          
#             Prevalence : 0.1127          
#         Detection Rate : 0.1013          
#   Detection Prevalence : 0.2416          
#      Balanced Accuracy : 0.8706          
                                          
#       'Positive' Class : yes



# cutoff 0.85

#Reference
#Prediction   no  yes
#         no  9456  235   #FN 235   VN 9456     # captura mais negativos, mas erra mais negativos
#         yes 1243 1124   #FP 1243   VP 1124  # captura menos positivos, mas erra menos positivos


# cutoff 0.9

#Reference
#Prediction  no   yes
#       no   9008  137   #FN 137   VN 9008     # captura menos negativos, mas erra menos negativos
#       yes  1691  1222  #FP 1691  VP 1222     # captura mais positivos, mas erra mais positivos

# se o custo de rodar a campanha for alto, sera escolhido um modelo que erra menos
# se o custo de rodar a campanha for baixo, sera escolhido um modelo que acerta mais


# Arvore de Decisao
str(dfTrain)
set.seed(314)
tc <- trainControl(method="repeatedcv", number=10, savePredictions = T, classProbs = TRUE)
modeltree <- train(SUBSCRIBED~., data = dfTrain, method = "treebag", trControl= tc, num_trees = 100)
pred_tree <- predict(modeltree,dfTest)
?predict

caret::confusionMatrix(data=pred_tree, dfTest$SUBSCRIBED, positive="yes")
modeltree[4]

#Confusion Matrix and Statistics

#Reference
#Prediction    no   yes
#       no  10248   636
#       yes   451   723

#Accuracy : 0.9099          
#95% CI : (0.9046, 0.9149)
#No Information Rate : 0.8873          
#P-Value [Acc > NIR] : 3.684e-16       

#Kappa : 0.5208          

#Mcnemar's Test P-Value : 2.393e-08       
                                          
#            Sensitivity : 0.53201         
#            Specificity : 0.95785         
#         Pos Pred Value : 0.61584         
#         Neg Pred Value : 0.94157         
#             Prevalence : 0.11271         
#         Detection Rate : 0.05996         
#   Detection Prevalence : 0.09736         
#      Balanced Accuracy : 0.74493         
                                          
#       'Positive' Class : yes 

library(doParallel)
registerDoParallel(4) 
getDoParWorkers() 
memory.limit (9999999999)

set.seed(314)
mtry <- sqrt(ncol(dfTrain))
mtry <- 3
tunegrid <- expand.grid(mtry=3)

# C.5tree
modelc5 <- train(SUBSCRIBED~., data = dfTrain, method = "C5.0Tree", trControl= tc)
pred_c5 <- predict(modelc5,dfTest)
caret::confusionMatrix(data=pred_c5, dfTest$SUBSCRIBED, positive="yes")

#Confusion Matrix and Statistics

#Reference
#Prediction    no   yes
#no  10404   800
#yes   295   559

#Accuracy : 0.9092          
#95% CI : (0.9039, 0.9143)
#No Information Rate : 0.8873          
#P-Value [Acc > NIR] : 2.669e-15       

#Kappa : 0.4581          

#Mcnemar's Test P-Value : < 2.2e-16       
                                          
#            Sensitivity : 0.41133         
#            Specificity : 0.97243         
#         Pos Pred Value : 0.65457         
#         Neg Pred Value : 0.92860         
#             Prevalence : 0.11271         
#         Detection Rate : 0.04636         
#   Detection Prevalence : 0.07082         
#      Balanced Accuracy : 0.69188         
                                          
#       'Positive' Class : yes

# Calculando ROC e AUC
pred_c5_num <- ifelse(pred_c5 == "yes",1,0)
roc <- pROC::roc(dfTest_num$SUBSCRIBED, pred_c5_num)
plot(roc)

# AUC
auc(roc)

### SVM

# controle da funcao train
tcsvm <- trainControl(method="repeatedcv", number=5, savePredictions = T, classProbs = TRUE)
tcsvm <- trainControl(savePredictions = T, classProbs = TRUE)
str(dfTrain)

#modelsvm <- train(SUBSCRIBED~., data = dfTrain, method = "svmLinear", trControl= tcsvm, preProcess = c("center", "scale"))

modelsvm <- train(SUBSCRIBED~MONTH+EDUCATION+JOB+ CONTACT+DAY_OF_WEEK+DURATION+CAMPAIGN+POUTCOME+EMP_VAR_RATE+ CONS_PRICE_IDX+CONS_CONF_IDX, data = dfTrain, method = "svmLinear", trControl= tcsvm, preProcess = c("center", "scale"))
summary(modelsvm)
modelsvm[4]
predsvm <- predict(modelsvm,dfTest)
caret::confusionMatrix(predsvm, dfTest$SUBSCRIBED, positive="yes")

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    no   yes
# no  10452   907
# yes   247   452

# Accuracy : 0.9043          
# 95% CI : (0.8989, 0.9095)
# No Information Rate : 0.8873          
# P-Value [Acc > NIR] : 8.537e-10       
# 
# Kappa : 0.3928          
# 
# Mcnemar's Test P-Value : < 2.2e-16       
#                                           
#             Sensitivity : 0.33260         
#             Specificity : 0.97691         
#          Pos Pred Value : 0.64664         
#          Neg Pred Value : 0.92015         
#              Prevalence : 0.11271         
#          Detection Rate : 0.03749         
#    Detection Prevalence : 0.05797         
#       Balanced Accuracy : 0.65476         
#                                           
#        'Positive' Class : yes 

# Calculando ROC e AUC
predsvm <- ifelse(predsvm == "yes",1,0)
roc <- pROC::roc(dfTest_num$SUBSCRIBED, predsvm)
plot(roc)

# AUC
auc(roc) # Area under the curve: 0.6548


### Redes Neurais

# Inicializando h2o
h2o.init()

# df <- h2o.importFile("C:/Users/Pichau/Documents/MBA/Analise Preditiva Avancada/Analise_Preditiva_Avancada-master/Logisitc Regression/BostonCredit_Exercise.csv")

# transformando os dados processados em h2o
#df <- as.h2o(marketing_numerico[,-2])
str(marketing)
df <- as.h2o(marketing[,-4])

# Set Seed
set.seed(314)

# Dividindo em treino e teste
df_split <- h2o.splitFrame(data=df, ratios=0.7)

train <- df_split[[1]]
test <- df_split[[2]]

y <- "SUBSCRIBED"
x <- setdiff(names(train), y)

# Modelo deeplearning
options(warn=-1)
model2 <- h2o.deeplearning(  x = x, 
                             y = y,
                             training_frame = train,
                             validation_frame = test,
                             distribution = "bernoulli",
                             activation = "RectifierWithDropout",
                             hidden = c(32,32,32), #baixo numero de neuronios por camada para que o modelo rode mais rapido
                             l1 = 1e-5,
                             epochs = 100)
model2
#** Metrics reported on full validation frame **
  
# MSE:  0.06652376
# RMSE:  0.257922
# LogLoss:  0.2085054
# Mean Per-Class Error:  0.1671413
# AUC:  0.9339332
# AUCPR:  0.5960505
# Gini:  0.8678664

h2o.confusionMatrix(model2,test)

# Confusion Matrix (vertical: actual; across: predicted)  for max f1 @ threshold = 0.226512805072222:
#           no  yes    Error         Rate
# no      9728  906 0.085198   =906/10634
# yes      340 1025 0.249084    =340/1365
# Totals 10068 1931 0.103842  =1246/11999

# acuracia (VP + VN) / (P+N)
acuracia <- (1025+9728)/(10068+1931)
#0.896158


# Sensibilidade (VP/VP+FN)
sensibilidade <- 1025/(1025+906)
#0.5308131

# Especificidade (VN/VN+FP)
especificidade <- 9728/10068
#0.9662296

# eficiencia (Sensibilidade/Especificidade)/2
(acuracia+especificidade)/2

model3 <- h2o.deeplearning(  x = x, 
                             y = y,
                             training_frame = train,
                             validation_frame = test,
                             distribution = "bernoulli",
                             activation = "RectifierWithDropout",
                             hidden = c(100,100,100), 
                             l1 = 1e-5,
                             epochs = 100)
model3 # melhor modelo
# MSE:  0.06128743
# RMSE:  0.247563
# LogLoss:  0.201701
# Mean Per-Class Error:  0.156511
# AUC:  0.9375463
# AUCPR:  0.6231141
# Gini:  0.8750927
h2o.confusionMatrix(model3,test)

model4 <- h2o.deeplearning(  x = x, 
                             y = y,
                             training_frame = train,
                             validation_frame = test,
                             distribution = "bernoulli",
                             activation = "RectifierWithDropout",
                             hidden = c(100,100,100, 100), 
                             l1 = 1e-5,
                             epochs = 100)
model4
# MSE:  0.06432598
# RMSE:  0.2536257
# LogLoss:  0.220096
# Mean Per-Class Error:  0.1717447
# AUC:  0.9306267
# AUCPR:  0.5982647
# Gini:  0.8612534

# o modelo piorou em comparacao com o model3 

