library(randomForest)

data(iris)

obj.count <- nrow(iris)

data.teach <- sample(1:obj.count, floor(0.9 * obj.count))

data.test  <- setdiff(1:obj.count, data.teach)



rf <- randomForest(Species ~ ., iris[data.teach,], ntree=10)

pr <- predict(rf, 
    iris[data.test,1:4]
)

res <- cbind(
  iris[data.test,],
  predicted = pr
)

res[res$predicted != res$Species,]

rf$importance

