library(tidyverse)
library(scales)
library(gridExtra)
library(ggcorrplot)
library(caTools)
library(rpart)
library(rpart.plot)
library(vip)
library(caret)


df <- read_csv("D:/Internships/Technohack/Task - 7/data.csv")

View(df)
glimpse(df)

df %>% is.na() %>% sum()
df <- na.omit(df)

# modification
df %>% na.omit() %>% mutate(SeniorCitizen = case_when(
  SeniorCitizen == 0 ~ "No",
  SeniorCitizen == 1 ~ "Yes"
)) %>% select(-customerID) -> df


cols <- colnames(df)
num_vars <- c('tenure','MonthlyCharges','TotalCharges')
cat_vars <- (cols[!(cols %in% num_vars)])[1:16]

# Counts
par(mfrow = c(4,4))
par(mar = c(rep(2.5,4)))
for(i in cat_vars){
  table(df[i]) %>% plot(main = paste(i),
                        xlab = '', ylab = '',
                        col = 'red')
}

# churn rate:
df %>% count(Churn) %>% mutate('Perc' = percent(n/sum(n))) %>% 
  ggplot(aes(x = Churn, y = n)) +
  geom_bar(stat = 'identity', position = position_dodge2(),
           width = 0.3, fill = 'yellow', colour = 'black') +
  theme_minimal() + theme(axis.title.y = element_blank()) +
  geom_text(aes(label = Perc), vjust = 3)


# EDA (Churn vs Cat)
plot1 <- function(var){
  df %>% count({{var}}, Churn) %>% 
    ggplot(aes(x = {{var}}, y = n, fill = Churn)) +
    geom_col(position = 'fill', width = 0.3) + 
    theme_minimal() + 
    scale_y_continuous(label = percent, n.breaks = 6) +
    theme(axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 15))
}

attach(df)
plot1(gender) -> p1
plot1(SeniorCitizen) -> p2
plot1(Partner) -> p3
plot1(Dependents) -> p4
plot1(PhoneService) -> p5
plot1(MultipleLines) -> p6
plot1(InternetService) -> p7
plot1(OnlineSecurity) -> p8
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8, nrow = 2)

plot1(OnlineBackup) -> p9
plot1(DeviceProtection) -> p10
plot1(TechSupport) -> p11
plot1(StreamingTV) -> p12
plot1(StreamingMovies) -> p13
plot1(Contract) -> p14
plot1(PaperlessBilling) -> p15
plot1(PaymentMethod) -> p16
grid.arrange(p9,p10,p11,p12,p13,p14,p15,p16, nrow = 2)
detach(df)


# (Churn vs numeric)
df %>% select(all_of(num_vars), Churn) %>% 
  pivot_longer(tenure:TotalCharges, names_to = 'vars',
               values_to = 'value') %>% 
  ggplot(aes(x = Churn, y = value)) +
  geom_boxplot(outlier.colour = 'red', fill = 'lightyellow') +
  geom_violin(alpha = 0.4, fill = 'green', colour = NA) +
  theme_minimal() + facet_wrap(.~vars, scales = 'free') + 
  theme(axis.title.y = element_blank())



# some joint plots by considering the significant variables:
df %>% ggplot(aes(x = Churn, y = TotalCharges)) + 
  geom_boxplot(outlier.colour = 'red', fill = 'yellow') + 
  facet_grid(OnlineSecurity ~ Contract, scales = 'free') +
  theme_bw()

df %>% ggplot(aes(x = Churn, y = tenure)) + 
  geom_boxplot(outlier.colour = 'red', fill = 'yellow') + 
  facet_grid(OnlineSecurity ~ Contract, scales = 'free') +
  theme_bw()


df %>% ggplot(aes(x = Churn, y = TotalCharges)) + 
  geom_boxplot(outlier.colour = 'red', fill = 'yellow') + 
  facet_grid(TechSupport ~ Contract, scales = 'free') +
  theme_bw()




# Correlation between numeric features:
df %>% select(all_of(num_vars)) %>% cor() %>% 
  ggcorrplot(type = 'upper', lab = T)


# Statistical test for association (cat-cat)
M1 <- data.frame(matrix(ncol = 3, nrow = 0))

for(i in cat_vars){
  df %>% select(Churn, all_of(i)) %>% 
    table() %>% chisq.test() -> ch
  M1 %>% rbind(c(paste('Churn -', i),
                ch$statistic %>% round(1), 
                ch$p.value)) -> M1
} 

colnames(M1) <- c('Pairs','Statistic','p-value')
print(M1)

# num-cat
M2 <- matrix(ncol = 2, nrow = length(num_vars),
            dimnames = list(paste(1:length(num_vars)),
              c('Variables','p-value'))) %>% as.data.frame()

for(i in 1:length(num_vars)){
  f <- reformulate('Churn', response = num_vars[i])
  w <- wilcox.test(f, data = df)
  
  M2[i,] <- c(num_vars[i], w$p.value)
}
print(M2)



#========================================================================
# Model: Decision tree

set.seed(42)
s <- sample.split(df$Churn, SplitRatio = 3/4)
train_data <- df[s,]
test_data <- df[!s,]

f <- function(d)(paste(d[1], 'x', d[2]))
glue::glue("Dimension of train data: {d1}",
           "Dimension of test data: {d2}",
           d1 = f(dim(train_data)), d2 = f(dim(test_data)),
           .sep = '\n')


r <- rpart(Churn ~ ., data = train_data)
rpart.plot(r, cex = 0.6)

# Importance:
vip(r, aesthetics = list(fill = 'red'), num_features = 20) + 
  theme_minimal() + labs(title = 'Importance of the features')


# accuracy:
churn_pred_train <- predict(r, type = 'class')
confusionMatrix(churn_pred_train, as.factor(train_data$Churn)) -> C1

churn_pred_test <- predict(r, newdata = test_data, type = 'class')
confusionMatrix(churn_pred_test, as.factor(test_data$Churn)) -> C2


stats2 <- function(C){ # C := Confusion Matrix and Statistics
  t <- C$table
  
  acc <- C$overall[[1]]
  pre <- t[2,2]/(t[2,2]+t[2,1])
  rec <- t[2,2]/(t[2,2]+t[1,2])
  f1 <- 2*(rec*pre)/(rec+pre)
  
  matrix(c(acc,pre,rec,f1), byrow = T,
         dimnames = list(c('Accuracy','Precision',
                           'Recall','F1-Score'))) -> M
  return(list('Confusion Matrix' = t,
              'Metrics' = M))
}

stats2(C1)
stats2(C2)

C1


