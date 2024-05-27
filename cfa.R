install.packages("foreign")
install.packages("lavaan")

library(foreign) 
library(lavaan)
library(haven)
data <- read_sav("SAQ.sav")

getwd()

head(data)

# corr matrix
cor_matrix <- cor(data)
print(cor_matrix)

#q05 statistics
#q06 computers
#q08 math


logical_model <- 'f1_l =~ q01+ q04+ q05+ q16
                  f2_l =~ q06+ q07 +q18+ q13
                  f3_l =~ q08+q11 +q17'

sonuc_logical <- cfa(logical_model,data,ordered=T)

lavPredict(sonuc_logical)
gizil_logical <- as.data.frame(lavPredict(sonuc_logical))
bir_data <- cbind(data, gizil_logical)

                               
                               
# f1 indices
f1_cols_l <- c(1, 4, 5, 16)
# f2 indices
f2_cols_l <- c(6, 7, 18, 13)
# f3 indices
f3_cols_l <- c(8, 11, 17)
                               
# sum
bir_data$f1_suml <- rowSums(bir_data[, f1_cols_l])
bir_data$f2_suml <- rowSums(bir_data[, f2_cols_l])
bir_data$f3_suml <- rowSums(bir_data[, f3_cols_l])
                               
                               
sum_model_l <- 'f1_suml ~ f3_suml
                f3_suml ~ f2_suml'
sum_result_l <- sem(sum_model_l, bir_data)

summary(sum_result_l,fit.measures=T)


                            


sum_model_l2 <-  'istatistik =~ q01+ q04+ q05+ q16
                  bilgisayar =~ q06+ q07 +q18+ q13
                  matematik =~ q08+ q11 +q17'
                  #path model
                 'istatistik ~ matematik
                  matematik ~ bilgisayar'
                               
sum_sonuc_l2 <- sem(sum_model_l2,bir_data)
                               
summary(sum_sonuc_l2,fit.measures=T)