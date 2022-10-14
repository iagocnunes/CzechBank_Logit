##################################################################################################
######## Trabalho final da Disciplina - Decisões Empresariais e Raciocínio Analítico [DERA] ######
##################################################################################################
##                                 Dados de um banco                                        ######
## Existem contas com e sem empréstimo, podemos propor aumentar o volume de empréstimos do  ######
## banco, oferecendo crédito para os indivíduos sem empréstimos que têm uma maior probabilidade ##
## de serem bons pagadores.                                                                 ######

## Autor: Iago Nunes (github.com/iagocnunes)

####----------------------------- PARTE TRÊS: REGRESSÃO LOGISTICA LASSO -----------------------------####

rm(list = ls())
library(caret)
library(glmnet)
library(MASS)
library(ROCR)
library(leaps)
library(tidyverse)
library(doParallel)
library(InformationValue)

### Funções e parametros

SelecionaCutOff <- function(fitted,y){
  
  ROCRpred <- prediction(fitted, y)
  
  sens <- data.frame(x=unlist(performance(ROCRpred, "sens")@x.values), 
                     y=unlist(performance(ROCRpred, "sens")@y.values))
  spec <- data.frame(x=unlist(performance(ROCRpred, "spec")@x.values), 
                     y=unlist(performance(ROCRpred, "spec")@y.values))
  
  sens %>% ggplot(aes(x,y)) + 
    geom_line() + 
    geom_line(data=spec, aes(x,y,col="red")) +
    scale_y_continuous(sec.axis = sec_axis(~., name = "Specificity")) +
    labs(x='Cutoff', y="Sensitivity") +
    theme(axis.title.y.right = element_text(colour = "red"), legend.position="none") 
  
  sens <- cbind(unlist(performance(ROCRpred, "sens")@x.values), unlist(performance(ROCRpred, "sens")@y.values))
  spec <- cbind(unlist(performance(ROCRpred, "spec")@x.values), unlist(performance(ROCRpred, "spec")@y.values))
  limiar <- sens[which.min(apply(sens, 1, function(x) min(colSums(abs(t(spec) - x))))), 1]
  
  return(limiar)
  
}

MetricasPrevisao <- function(real,previsto){
  mat <- table(real, previsto)
  if (dim(mat)[2] != 2){ 
    mat <- matrix(0,2,2)
  }
  
  TP <- mat[2,2]
  TN <- mat[1,1]
  FP <- mat[1,2]
  FN <- mat[2,1]
  
  tab_metricas <- matrix(NA,nrow=1,ncol=5)
  colnames(tab_metricas) <- c("accur","sens","spec","prec","f1")
  
  tab_metricas[1,1] <- (TP + TN) / (TP + FP + FN + TN)
  
  tab_metricas[1,2] <- TP/(TP + FN)
   
  tab_metricas[1,3] <- TN/(TN + FP)
  
  tab_metricas[1,4] <- TP/(TP+FP)
  
  tab_metricas[1,5] <- 2*(tab_metricas[1,2]*tab_metricas[1,4])/(tab_metricas[1,2]+tab_metricas[1,4])
  
  
  return(tab_metricas)
}


num_kfold  <-  10

dados_final <-readRDS('dados_final')

# para reprodutibilidade do exercício
set.seed(1)
# dividimos os dados entre duas amostras, uma de treino com 70% das observações, e uma de teste, com 30%
sample <- sample(c(TRUE, FALSE), nrow(dados_final), replace=TRUE, prob=c(0.7,0.3))
dados_treino <- dados_final[sample, ]
dados_teste <- dados_final[!sample, ]

##################################### Parametros do Logit ######################
# constante de regularização alfa=1 lambda=0
tuneGrid<-expand.grid(alpha = 1, lambda=0) # regressão tipo LASSO e penalidade do estimador ML

# controle dos clusters
trControl <- trainControl(method = 'cv', number = num_kfold, savePredictions =  TRUE,
                          classProbs = FALSE,
                          allowParallel = TRUE)

#logit com variaveis pre-determinadas.
model.fit <- train(status ~ idade+balance+gnr+junior+classic+gold+balance_distr+A10+A11, data = dados_treino, 
                   method = "glmnet", trControl = trControl,
                   tuneGrid=tuneGrid, verbose=FALSE,
                   family="binomial", savePredictions = TRUE)

coef(model.fit$finalModel,model.fit$finalModel$lambdaOpt)
#(Intercept)   -2.43177779989
#idade          0.00001302629
#balance        0.00002594232
#gnr1          -0.50617475989
#junior1        0.30013297928
#classic1       1.04999511436
#gold1         -0.02530097904
#balance_distr  0.00007543199
#A10           -0.00759807449
#A11            0.00012097503

################################################################################
####### Definicao Limiar com base no k-Fold Cross Validation.###################
################################################################################
# Limiar e Cutoff no CV 
mat.cutoff <- matrix(NA,ncol=1,nrow=num_kfold,
                     dimnames = list(paste("fold",c(1:num_kfold),sep="")))

metricas.in <- matrix(NA, nrow= num_kfold, ncol = 5, 
                      dimnames = list(paste("fold",c(1:num_kfold),sep=""),
                                      c("Acc","Sens","Spec","Prec","F1")))  


for (k in 1:num_kfold){
  if(num_kfold >= 10){ 
    
    if(k < 10){
      index_fold_aux <- filter(model.fit$pred, Resample == paste("Fold0",k,sep="") )
    }
    if(k >= 10){
      index_fold_aux <- filter(model.fit$pred, Resample == paste("Fold",k,sep="") )
    } 
    
  }
  if(num_kfold < 10){ 
    index_fold_aux <- filter(model.fit$pred, Resample == paste("Fold",k,sep="") )
  }
  
  prev_k_fold <- predict.train(model.fit, type = "prob", newdata = dados_treino[index_fold_aux$rowIndex , ] )
  mat.cutoff[k, 1 ] <- SelecionaCutOff(fitted = prev_k_fold$`1` , y = dados_treino$status[index_fold_aux$rowIndex])
  
  real <-  dados_treino$status[index_fold_aux$rowIndex]  
  previsto <- if_else(prev_k_fold$`1` <  mat.cutoff[ k,1] , 0  ,1)
  metricas.in[k, ] <-  MetricasPrevisao(real = real , previsto = previsto)
  
} 

# Analisando matrizes de confusão
Metricas.Totais.In <- colMeans(metricas.in)
cut.off.selected <- mean(mat.cutoff)
prob_in <-  predict.train(model.fit, type = "prob", newdata = dados_treino )
previsto.in <- if_else(prob_in$`1` <  cut.off.selected , 0  ,1)
Conf_in <- InformationValue::confusionMatrix(dados_treino$status, previsto.in)
Conf_in
Conf_in <- caret::confusionMatrix(as.factor(previsto.in),as.factor(dados_treino$status))
Conf_in
prob_out <- predict.train(model.fit, type = "prob", newdata = dados_teste )
previsto.out <- if_else(prob_out$`1` <  cut.off.selected , 0  ,1)
Conf_out <- InformationValue::confusionMatrix(dados_teste$status, previsto.out)
Conf_out
Conf_out <- caret::confusionMatrix(as.factor(previsto.out),as.factor(dados_teste$status))
Conf_out













