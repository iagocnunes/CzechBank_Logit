rm(list = ls())

setwd("C:/Users/Iago/Desktop/FGV/Decisões Empresariais e Raciocínio Analítico/0.R_Vitoria_2022_02")

options(scipen=999)
library(tidyverse)

conta <- read.csv('account.asc', header = TRUE, sep = ";", na.strings = "")
emprest <- read.csv('loan.asc', header = TRUE, sep = ";", na.strings = "")

colnames(conta)
colnames(emprest)
str(conta)

library(Hmisc) 
describe(conta$account_id) 
describe(emprest$date) 
describe(emprest$account_id)
describe(emprest$loan_id)
describe(emprest$status)

emprest$emprest_date <- parse_date(as.character(emprest$date), format="%y%m%d")

conta2X <- conta %>%
  select(account_id, district_id)
emprest2X <- emprest %>%
  select(account_id, loan_id, status, emprest_date)

conta_loan <- conta2X %>% 
  left_join(emprest2X, by = "account_id")

rm(conta2X, emprest2X, emprest)

describe(conta_loan$loan_id)
describe(conta_loan$status)

conta_loan <- subset(conta_loan, !is.na(loan_id))

transacoes <- read.csv('trans.asc', header = TRUE, sep = ";", na.strings = "")

colnames(transacoes)

describe(transacoes$date) 
describe(transacoes$balance)
describe(transacoes$account_id)

trans_loan <- transacoes %>% 
  left_join(conta_loan, by = "account_id")

rm(conta_loan)

transacoes1 <- transacoes %>%
  select(account_id, balance)

trans_acc <- transacoes1 %>% 
  left_join(conta, by = "account_id")

trans_acc1 <- trans_acc %>%
  select(district_id, balance) %>% 
  group_by(district_id) %>% 
  summarise(balance_distr=mean(balance, na.rm=T)) %>% 
  ungroup()

rm(transacoes1, transacoes, trans_acc, conta)

colnames(trans_loan)

describe(trans_loan$loan_id) 

trans_loan <- subset(trans_loan, !is.na(loan_id))

describe(trans_loan$date)

trans_loan$trans_date <- parse_date(as.character(trans_loan$date), format="%y%m%d")

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

transloan <- rbind(trans_loan93,trans_loan94,trans_loan95,trans_loan96,trans_loan97,trans_loan98)

rm(trans_loan,trans_loan93,trans_loan94,trans_loan95,trans_loan96,trans_loan97,trans_loan98,
   df_name, df_names)

describe(transloan$balance)

cliente <- read.csv('client.asc', header = TRUE, sep = ";", na.strings = "")
conector <- read.csv('disp.asc', header = TRUE, sep = ";", na.strings = "")

colnames(cliente)
colnames(conector)

describe(conector$type)
conector <- subset(conector, type=="OWNER")

cliente <- cliente %>%
  select(client_id, birth_number)

cliente$ano <- substr(cliente$birth_number, 1, 2)
cliente$ano <- paste(19, cliente$ano, sep = "")
cliente$ano <- as.numeric(cliente$ano)
cliente$mes <- as.numeric(substr(cliente$birth_number, 3, 4))
cliente$gnr <- ifelse(cliente$mes>50, 1, 0)  # 1 = Femino // 0 = Masculino
cliente$mes <- ifelse(cliente$mes>50, cliente$mes - 50, cliente$mes)
cliente$dia <- as.numeric(substr(cliente$birth_number, 5, 6))
cliente$data_nasc <- as.Date(paste0(cliente$ano, "-", cliente$mes, "-", cliente$dia), format = "%Y-%m-%d")
cliente$gnr <- as.factor(cliente$gnr)

cliente <- cliente %>%
  select(client_id, data_nasc, gnr)
conector <- conector %>%
  select(disp_id, account_id, client_id)

conect_acc <- conector %>% 
  left_join(cliente, by = "client_id")

rm(cliente, conector)

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

dados_final <- transloan %>% 
  left_join(trans_acc1, by = "district_id")
dados_final <- dados_final %>% 
  left_join(distrito, by = "district_id")
dados_final <- dados_final %>% 
  left_join(conect_acc, by = "account_id")
dados_final <- dados_final %>% 
  left_join(cartao, by = "disp_id")

dados_final <- dados_final  %>% 
  mutate(idade=as.numeric(emprest_date - data_nasc))

describe(dados_final$idade)

rm(transloan, conect_acc, trans_acc1, distrito, cartao)

dados_final <- dados_final  %>% 
  mutate(status=ifelse(status=="A"|status=="C", 1, 0), # 1= bom pagador // 0= mau pagador
         status=as.factor(status),
         junior=ifelse(is.na(junior), 0, junior),
         classic=ifelse(is.na(classic), 0, classic),
         gold=ifelse(is.na(gold), 0, gold),
         junior=as.factor(junior),
         classic=as.factor(classic),
         gold=as.factor(gold))

dados_final <- dados_final %>%
  select(account_id, status, balance, idade, gnr, junior, classic, gold, balance_distr, A10, A11)

library(Amelia)
missmap(dados_final, main='Mapa de NAs - banco de dados final', x.cex = 1.0, y.labels= NULL, y.at = NULL, margins = c(10, 10))

saveRDS(dados_final, 'dados_final')

library(esquisse)


