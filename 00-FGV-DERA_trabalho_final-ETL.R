##################################################################################################
######## Trabalho final da Disciplina - Decisões Empresariais e Raciocínio Analítico [DERA] ######
##################################################################################################
##                                 Dados de um banco                                        ######
## Existem contas com e sem empréstimo, podemos propor aumentar o volume de empréstimos do  ######
## banco, oferecendo crédito para os indivíduos sem empréstimos que têm uma maior probabilidade ##
## de serem bons pagadores.                                                                 ######

## Autor: Iago Nunes (github.com/iagocnunes)

# Etapas:
# Parte 1: ETL e integração: extrair váriaveis de interesse, transformá-las e unir os dados em uma base
# Parte 2: Análise descritiva: frequências, proporções e distribuição (gráficos)
# Parte 3: Análise preditiva:
#   - Criar grupo de treinamento [70% da base] e grupo de teste [30% da base]: todos com empréstimo
#   - Aplicar o Modelo de Regressão Logística no grupo de treinamento
#   - Utilizar o modelo de treinamento para prever a classificação do grupo de teste
#   - Avaliar se o modelo fez uma boa previsão ou não (Matriz de Confusão)

# Questões respondidas:
# O banco terá uma noção exata do perfil [nas variáveis selecionadas] dos bons e maus pagadores;
# Se o modelo for bom: O banco poderá traçar ações segmentadas para os bons e maus tomadores de
# empréstimo em pontencial; exemplo: oferecer empréstimo ou recusar um pedido de empréstimo
# Se o modelo for ruim: que ações o banco pode tomar para melhorá-lo?

####----------------------------- PARTE UM: ETL E INTEGRAÇÃO -----------------------------####

# comando para limpar os objetos do environment (ambiente de trabalho), caso tenha algum
rm(list = ls())

# designando pasta de trabalho; também pode ser feito pela aba "Sessão/Session" na barra de ferramentas
setwd("C:/Users/Hiago/Desktop/FGV/Decisões Empresariais e Raciocínio Analítico/0.R_Vitoria_2022_02")

options(scipen=999) # removendo notação científica dos outputs no console
library(tidyverse) # carregando todas as livrarias do tidyverse: dplyr, tidyr, tibble, readr, etc.


###### ETL das variáveis de interesse nas bases "contas" e "empréstimos" #######
# analisando as duas conjuntamente para verificar a quantidade de contas totais e a
# quantidade de contas na base de empréstimo (balanceamento da nossa população/amostra)

# carregando as bases no nosso ambiente de trabalho
#  - header: informa se a base tem (TRUE) ou não (FALSE) cabeçalho
#  - sep: informa o separados de colunas e valores (no nosso caso, um ponto e virgula)
#  - na.strings: informa como as informações faltantes em colunas de caracteres estão registradas,
#    no nosso caso elas estão vazias, nada entre as aspas ("")
conta <- read.csv('account.asc', header = TRUE, sep = ";", na.strings = "")
emprest <- read.csv('loan.asc', header = TRUE, sep = ";", na.strings = "")

# lista com o nome das colunas
colnames(conta)
colnames(emprest)
str(conta)

library(Hmisc) # comando describe()
# o comando describe() entrega um sumário da quantidade de valores reais,
# valores faltantes, valores distintos e outras informações a depender do tipo de variável

describe(conta$account_id) # sem NAs (NAs = valores faltantes/missing)

# analisando o ano dos empréstimos
describe(emprest$date) # emprestimos de 93 a 98

describe(emprest$account_id)
describe(emprest$loan_id)
# São 682 contas para 682 empréstimos, o que significa que, mesmo com o passar dos anos,
# só há um empréstimo por conta, não há um emprestimo seguido de outro para a mesma conta.
# Então aqui já abre uma possibilidade para o banco: além de buscar potenciais bons tomadores
# de empréstimo, pode-se oferecer novo empréstimo para bons pagadores já observados

describe(emprest$status) # sem NAs

# designando o formato de data para a nova coluna da data do empréstimo
emprest$emprest_date <- parse_date(as.character(emprest$date), format="%y%m%d")

# selecionando variáveis
conta2X <- conta %>%
  select(account_id, district_id)

emprest2X <- emprest %>%
  select(account_id, loan_id, status, emprest_date)

# unindo bancos
conta_loan <- conta2X %>% 
  left_join(emprest2X, by = "account_id")

# limpando do ambiente de trabalho os objetos que não precisamos mais
rm(conta2X, emprest2X, emprest)

# verificando balanceamento da nossa população/amostra
describe(conta_loan$loan_id)
# 3818 (84,8%) contas sem empréstimo, 682 (15,2%) com empréstimo
describe(conta_loan$status)
# bons pagadores (A e C): 88,9% // maus pagadores (B e D): 11,1%
# balanceamento bom

# selecionando apenas contas com empréstimo
conta_loan <- subset(conta_loan, !is.na(loan_id))

###### ETL das variáveis de interesse na base "transações" #######

# carregando
transacoes <- read.csv('trans.asc', header = TRUE, sep = ";", na.strings = "")

# nome das colunas
colnames(transacoes)

describe(transacoes$date) # transações de 93 a 98 // sem NAs
describe(transacoes$balance) # sem NAs
describe(transacoes$account_id) # 4.500 contas para 1.056.320 transações

# unindo bancos
trans_loan <- transacoes %>% 
  left_join(conta_loan, by = "account_id")

# limpando ambiente de trabalho
rm(conta_loan)

# calculando balanço médio do distrito
## selecionando variáveis
transacoes1 <- transacoes %>%
  select(account_id, balance)
## unindo dados
trans_acc <- transacoes1 %>% 
  left_join(conta, by = "account_id")
## sumarizando um balanço médio para cada distrito
trans_acc1 <- trans_acc %>%
  select(district_id, balance) %>% 
  group_by(district_id) %>% 
  summarise(balance_distr=mean(balance, na.rm=T)) %>% 
  ungroup()
## limpando ambiente de trabalho
rm(transacoes1, transacoes, trans_acc, conta)

# nome das colunas
colnames(trans_loan)

# analisando transações de contas com empréstimos
describe(trans_loan$loan_id) # 191.556 transações de 682 contas com empréstimo; 864.764 de contas sem

# selecionando apenas transações de contas com empréstimo
trans_loan <- subset(trans_loan, !is.na(loan_id))

# analisando o ano das transações
describe(trans_loan$date)

# designando o formato de data para a nova coluna da data da transação
trans_loan$trans_date <- parse_date(as.character(trans_loan$date), format="%y%m%d")


# dividindo a base de transações até a data do empréstimo,
# para que o balanço médio da conta não seja influenciado pelo empréstimo em si
trans_loan93 <- subset(trans_loan, emprest_date<=as.Date("1993-12-31"))
trans_loan93 <- subset(trans_loan93, trans_date<emprest_date)

trans_loan94 <- subset(trans_loan, emprest_date >= as.Date("1994-01-01") & emprest_date <=  as.Date("1994-12-31"))
trans_loan94 <- subset(trans_loan94, trans_date<emprest_date)

trans_loan95 <- subset(trans_loan, emprest_date >= as.Date("1995-01-01") & emprest_date <=  as.Date("1995-12-31"))
trans_loan95 <- subset(trans_loan95, trans_date<emprest_date)

trans_loan96 <- subset(trans_loan, emprest_date >= as.Date("1996-01-01") & emprest_date <=  as.Date("1996-12-31"))
trans_loan96 <- subset(trans_loan96, trans_date<emprest_date)

trans_loan97 <- subset(trans_loan, emprest_date >= as.Date("1997-01-01") & emprest_date <=  as.Date("1997-12-31"))
trans_loan97 <- subset(trans_loan97, trans_date<emprest_date)

trans_loan98 <- subset(trans_loan, emprest_date >= as.Date("1998-01-01") & emprest_date <=  as.Date("1998-12-31"))

# verificando se a função mean() calcula corretamente a média entre valores negativos e positivos
mean(c(1, 2, -1, 3, -1)) # resultado: 0,8, ok

# reduzindo o comprimento (vertical) dos dados para termos somente uma observação para cada conta
# através da sumarização do balanço médio até o ano do empréstimo, por conta
df_names <- c("trans_loan93","trans_loan94","trans_loan95","trans_loan96",
              "trans_loan97","trans_loan98")

for(df_name in df_names){
  base::get(df_name) %>%
    select(account_id, district_id, balance, status, emprest_date) %>% 
    group_by(account_id, district_id, status, emprest_date) %>%
    summarise(balance=mean(balance, na.rm=T)) %>%
    ungroup() %>%
    assign(value = .,
           x = df_name,
           envir = globalenv())
}

# unindo bases
transloan <- rbind(trans_loan93,trans_loan94,trans_loan95,trans_loan96,trans_loan97,trans_loan98)

# limpando ambiente de trabalho
rm(trans_loan,trans_loan93,trans_loan94,trans_loan95,trans_loan96,trans_loan97,trans_loan98,
   df_name, df_names)

describe(transloan$balance) # balanços médios de 1993 a 1998: de $5.350 a $79.500,54

###### ETL das variáveis de interesse nas bases "clientes" e "contector" #######

# carregando
cliente <- read.csv('client.asc', header = TRUE, sep = ";", na.strings = "")
conector <- read.csv('disp.asc', header = TRUE, sep = ";", na.strings = "")

# nome das colunas
colnames(cliente)
colnames(conector)

# selecionando apenas caracteristicas dos donos das contas
describe(conector$type) # verificando balanceamento 
conector <- subset(conector, type=="OWNER") # selecionando apenas observações sobre donos

# selecionando variáveis
cliente <- cliente %>%
  select(client_id, birth_number)

# tratando data e gênero
cliente$ano <- substr(cliente$birth_number, 1, 2)
cliente$ano <- paste(19, cliente$ano, sep = "")
cliente$ano <- as.numeric(cliente$ano)
cliente$mes <- as.numeric(substr(cliente$birth_number, 3, 4))
cliente$gnr <- ifelse(cliente$mes>50, 1, 0)  # 1 = Femino // 0 = Masculino
cliente$mes <- ifelse(cliente$mes>50, cliente$mes - 50, cliente$mes)
cliente$dia <- as.numeric(substr(cliente$birth_number, 5, 6))
cliente$data_nasc <- as.Date(paste0(cliente$ano, "-", cliente$mes, "-", cliente$dia), format = "%Y-%m-%d")
cliente$gnr <- as.factor(cliente$gnr)

# selecionando variáveis
cliente <- cliente %>%
  select(client_id, data_nasc, gnr)

conector <- conector %>%
  select(disp_id, account_id, client_id)

# unindo bancos
conect_acc <- conector %>% 
  left_join(cliente, by = "client_id")

# limpando ambiente de trabalho
rm(cliente, conector)

###### ETL das variáveis de interesse nas bases "cartao" e "distrito" #######

cartao <- read.csv('card.asc', header = TRUE, sep = ";", na.strings = "")
distrito <- read.csv('district.asc', header = TRUE, sep = ";", na.strings = "")

distrito <- distrito %>%
  select(A1, A10, A11) %>% 
  rename(district_id='A1')

cartao <- cartao %>% 
  mutate(junior=ifelse(type=="junior", 1, 0),
         classic=ifelse(type=="classic", 1, 0),
         gold=ifelse(type=="gold", 1, 0)) %>% 
  select(disp_id,junior,classic,gold) %>% 
  group_by(disp_id) %>% 
  summarise(junior=sum(junior), classic=sum(classic), gold=sum(gold)) %>% 
  ungroup()

###### Unindo dados e finalizando variáveis #######

# unindo balanço médio do distrito
dados_final <- transloan %>% 
  left_join(trans_acc1, by = "district_id")
# unindo proporção de habitantes urbanos e salário médio dos distritos
dados_final <- dados_final %>% 
  left_join(distrito, by = "district_id")
# unindo conector
dados_final <- dados_final %>% 
  left_join(conect_acc, by = "account_id")
# unindo cartão
dados_final <- dados_final %>% 
  left_join(cartao, by = "disp_id")

# idade (em dias) no dia do empréstimo
dados_final <- dados_final  %>% 
  mutate(idade=as.numeric(emprest_date - data_nasc))

describe(dados_final$idade) # idades: dos 14 [4958 dias] aos 62 [22585 dias]

# limpando ambiente
rm(transloan, conect_acc, trans_acc1, distrito, cartao)

# status de bom e mau pagador
dados_final <- dados_final  %>% 
  mutate(status=ifelse(status=="A"|status=="C", 1, 0), # 1= bom pagador // 0= mau pagador
         status=as.factor(status),
         junior=ifelse(is.na(junior), 0, junior),
         classic=ifelse(is.na(classic), 0, classic),
         gold=ifelse(is.na(gold), 0, gold),
         junior=as.factor(junior),
         classic=as.factor(classic),
         gold=as.factor(gold))

# mantendo apenas variáveis de interesse
dados_final <- dados_final %>%
  select(account_id, status, balance, idade, gnr, junior, classic, gold, balance_distr, A10, A11)

# mapa de NAs
missmap(dados_final, main='Mapa de NAs - banco de dados final', x.cex = 1.0, y.labels= NULL, y.at = NULL, margins = c(10, 10))

# salvando base final em formato RDS
# RDS= arquivo do R, carrega mais rápido na próxima vez que subir a base no ambiente
# comando pra ler uma base RDS:     dados_final <-readRDS('dados_final')
saveRDS(dados_final, 'dados_final')

## Analise descritiva
library(esquisse)








