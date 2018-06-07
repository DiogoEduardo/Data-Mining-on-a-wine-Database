wine <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"), header = FALSE)

# Análise inicial, conhencendo a base de dados

head(wine)

summary(wine)

install.packages("corrplot")

library(corrplot)

mat_cor <- cor(wine[, -5])

corrplot(mat_cor, method = "ellipse")

plot(wine, pch=as.numeric(wine$V1), col=as.numeric(wine$V1))

plot(wine[, "V13"],
     wine[, "V14"],
     pch=as.numeric(wine$V1), col=as.numeric(wine$V1))


hist(wine$V14, main="", xlab="Proline", ylab="Frequência")

boxplot(V14 ~ V1, data=wine)

#Usando o Naive Bayes

install.packages("e1071")
install.packages("klaR")
install.packages("caret")
install.packages("rminer")

library("e1071")
library(caret)
library("klaR")
library("rminer")

dim(wine)
  
amostra = sample(2,nrow(wine),replace=TRUE, prob=c(0.7,0.3))

head(amostra)

winetreino = wine[amostra==1,]
wineteste = wine[amostra==2,]

wine$V1 <- as.factor(wine$V1)

modelo <- naiveBayes(V1 ~ ., data=winetreino)
modelo

pred <- predict(modelo, wineteste)
head(pred)

confusao = table(wineteste$V1, pred)
confusao

taxaacerto = (confusao[1] + confusao[5] + confusao[9]) / sum(confusao)
taxaacerto

# Usando o KNN

library("class")

wineknn <- data.frame(V1 = wine$V1, V2 = as.integer(wine$V13), V3 = wine$V14)

wineknn$V1 <- as.factor(wineknn$V1)

head(wineknn)

amostra = sample(2,nrow(wineknn),replace=TRUE, prob=c(0.7,0.3))

head(amostra)

wineknntreino = wineknn[amostra==1,]
wineknnteste = wineknn[amostra==2,]

wineknnteste$V1 <- as.factor(wineknnteste$V1)
wineknntreino$V1 <- as.factor(wineknntreino$V1)

preds <- knn( wineknntreino[,-1], wineknnteste[,-1], factor(wineknntreino$V1),
  k=10, prob = TRUE, use.all = TRUE)


confusionMatrix(table(preds, wineknnteste$V1))


# Árvore de Decisão

install.packages("rpart.plot")
  
library(rpart)
modelo <- rpart(V1 ~., wine)

library(rpart.plot)
rpart.plot(modelo)

# Dendograma

set.seed(8953)
idx <- sample(1:dim(wine)[1], 40)
wineSample <- wine[idx,]
wineSample$V1 <- NULL
hc.single <- hclust(dist(wineSample), method="single")
plot(hc.single, hang = -1, labels=wine$V1[idx])
rect.hclust(hc.single, k=3)

