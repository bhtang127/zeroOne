# library(keras)
library(randomForest)

# setwd("~/sandboxes/zeroOne/")

## load in the data
dat = as.matrix(read.table("data.txt"))

dat = dat[dat[,102] > 0 & dat[,102] < 1,]
ind = sample(dim(dat)[1],floor(0.8*dim(dat)[1]))

fit = randomForest(dat[ind,1:100],dat[ind,101],dat[-ind,1:100],dat[-ind,101],
                   ntree=1000)
summary(fit)

# ntest = 1000
# n = nrow(dat); c = ncol(dat)
# 
# xTest = dat[1 : ntest, 1 : (c-1)]
# yTest = dat[1 : ntest, c]
# 
# xTrain = dat[-(1 : ntest), 1 : (c-1)]
# yTrain = dat[-(1 : ntest), c]
# 
# model = keras_model_sequential() %>%
#     layer_dense(units = 256, activation = "relu", input_shape = dim(xTrain)[2]) %>%
#     layer_dropout(rate = .2) %>% 
#     layer_dense(units = 128, activation = "relu") %>%
#     layer_dropout(rate = .2) %>% 
#     layer_dense(units = 64, activation = "relu") %>%
#     layer_dropout(rate = .2) %>% 
#     layer_dense(units = 32, activation = "relu") %>%
#     layer_dense(units = 1)
# 
# model %>% compile(
#           loss = "mse",
#           optimizer = optimizer_rmsprop(),
#           metrics = list("mean_absolute_error")
#         )
# 
# history = model %>% fit(
#       xTrain, yTrain,
#       epochs = 500,
#       batch_size = 500,
#       validation_split = 0.2
#     )
# 
# 
# pdf("tracePlot.pdf")
# plot(history)
# dev.off()
# 
# ## try it out
# model %>% evaluate(xTest, yTest)
# 
# 
# yTestPred = model %>% predict(xTest)
# 
# plot(
#   (yTestPred + yTest) * .5, 
#    yTestPred - yTest
#   )
# 
# abline(h = c(-.1, .1))
