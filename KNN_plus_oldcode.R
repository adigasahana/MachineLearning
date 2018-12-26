#load libraries
#Import packages
library(ggplot2)
librARY(repr)
install.packages("repr")
librARY(repr)
library(repr)
library(dplyr)
options(repr.plot.width = 4, repr.plot.height = 4)
auto_prices = read.csv('/Downloads/Automobile price data _Raw_.csv', row.names = 1)
auto_prices = read.csv(file.choose(),header=T)
head(auto_prices,10)
lapply(auto_prices, function(x){any (x == '?')})
table(auto_prices)
dim(auto_prices)
dim(lapply(auto_prices, function(x){any (x == '?')}))
str(auto_prices)
names(auto_prices)
for (col in names(auto_prices){})
for (col in names(auto_prices)){
if(is.character(auto_prices[,cols]) {})
for (col in names(auto_prices)){
if(is.character(auto_prices[,cols])){
count = sum(ifelse(auto_pricess[,cols] == '?', 1, 0))
cat(paste(col, as.character(count),'\n'))}}
for (col in names(auto_prices)){
+ if(is.character(auto_prices[,col])){
+ count = sum(ifelse(auto_pricess[,col] == '?', 1, 0))
+ cat(paste(col, as.character(count),'\n'))}}
for(col in names(auto_prices)){
if(is.character(auto_prices[,col])){
count = sum(ifelse(auto_prices[,col] == '?', 1, 0))
cat(paste(col, as.character(count), '\n'))
}
}
for(col in names(auto_prices)){
if(is.character(auto_prices[,col])){
count = sum(ifelse(auto_prices[,col] == '?', 1, 0))
cat(paste(col, as.character(count), '\n'))
}
}
for(col in names(auto_prices)){
if (is.character(auto_names[,cols])){
count = sum(ifelse(auto_prices[,col] == '?', 1, 0))
cat(paste(col, as.character(count), '/n'))
}}
for(col in names(auto_prices)){
+ if (is.character(auto_prices[,cols])){
+         count = sum(ifelse(auto_prices[,col] == '?', 1, 0))
+         cat(paste(col, as.character(count), '/n'))
+ }}
for(col in names(auto_prices)){
if (is.character(auto_names[,cols])){
count = sum(ifelse(auto_prices[,col] == '?', 1, 0))
cat(paste(col, as.character(count), '/n'))
}
}
for(col in names(auto_prices)){
+ if (is.character(auto_prices[,cols])){
+ count = sum(ifelse(auto_prices[,col] == '?', 1, 0))
+ cat(paste(col, as.character(count), '/n'))
+ }
+ }
for(col in names(auto_prices)){
+ if (is.character(auto_names[,cols])){
+ count = sum(ifelse(auto_prices[,col] == '?', 1, 0))
+ cat(paste(col, as.character(count), '/n'))
for(col in names(auto_prices)){if (is.character(auto_names[,cols])){count = sum(ifelse(auto_prices[,col] == '?', 1, 0))cat(paste(col, as.character(count), '/n'))}}
for(col in names(auto_prices)){if (is.character(auto_names[,cols])){count = sum(ifelse(auto_prices[,col] == '?', 1, 0))
cat(paste(col, as.character(count),'/n'))}}
for(col in names(auto_prices)){if (is.character(auto_prices[,cols])){count = sum(ifelse(auto_prices[,col] == '?', 1, 0))
cat(paste(col, as.character(count),'/n'))}}
for(col in names(auto_prices)){if (is.character(auto_prices[,col])){count = sum(ifelse(auto_prices[,col] == '?', 1, 0))
cat(paste(col, as.character(count),'/n'))}}
for(col in names(auto_prices)){if (is.character(auto_prices[,col])){count = sum(ifelse(auto_prices[,col] == '?', 1, 0))
show_count = cat(paste(col, as.character(count),'/n'))}}
show_count
count
for(col in names(auto_prices)){if (is.character(auto_prices[,col])){count = sum(ifelse(auto_prices[,col] == '?', 1, 0))
print(paste(col, as.character(count),'/n'))}}
for(col in names(auto_prices)){if (is.character(auto_prices[,col])){count = sum(ifelse(auto_prices[,col] == '?', 1, 0))
cat(paste(col, as.character(count)))}}
for(col in names(auto_prices)){
if (is.character(auto_prices[,col])){
count = sum(ifelse(auto_prices[,col] == '?', 1, 0))
cat(paste(col, as.character(count), '/n'))}}
??complete.cases
auto_prices[,'normalized.losses'] = NULL
dim(auto_prices)
cols = c('price', 'bore', 'stroke', 'horsepower', 'pek.rpm')
auto_prices[,cols] = lapply(auto_prices[,cols], function(x){ ifelse(x == '?', 'NA',x)})
cols = c('price', 'bore', 'stroke', 'horsepower', 'peak.rpm')
auto_prices[,cols] = lapply(auto_prices[,cols], function(x){ifelse(x == '?', NA, x)})
auto_prices = auto_prices[complete.cases(auto_prices[,cols]),]
dim(auto_prices)
auto_prices[,cols] = lapply(auto_prices[,cols], as.numeric)
str(auto_prices[,cols])
body_cats = c('sedan' = 'sedan', 'hatchback' = 'hatchback', 'wagon' = 'wagon',
'hardtop' = 'hardtop_convert', 'convertible' = 'hardtop_convert')
nrow(auto_prices)
out = rep('i', length.out = nrow(auto_prices))
i=1
for(x in auto_prices[,'body.style']){}
for(x in auto_prices[,'body.style']){
out[i] = body_cats[[x]]
i = i+1
}
auto_prices[,'body.style'] = out
auto_prices[,'body.style']
table(auto_prices[,'body.style'])
ggplot(auto_prices, aes(body.style, price)) + geom_boxplot()
plot_hist = function(df, col = 'price', bins = 10){}
plot_hist = function(df, col = 'price', bins = 10){
options(repr.plot.width = 4, repr.plot.height = 4)
bw = (max(df[,col]) - min(df[,col]))/(bins +1)
p = ggplot(df, aes_string(col)) + geom_histogram(bandwidth = bw, aes(y = ..density..), alpha = 0.5) +
geom_density(aes(y=..density..), color = 'blue') + geom_rug()
print(p)
}
plot_hist(auto_prices)
auto_prices[, 'log_price'] = log(auto_prices[,'price'])
plot_hist(auto_prices, col = 'log_price')
#import packages
library(ggplot2)
library(repr)
library(hexbin)
library(GGally)
#set the initial plot are dimensionsoptions(repr.plot.width = 4, repr.plot.height = 4)
options(repr.plot.width = 4, repr.plot.height = 4)
auto.price = read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)
head(auto.price)
dim(auto.price)
for (col in auto.price[,cols]){
if(is.character(auto.price[,col])){
count = sum(ifelse(auto.price[,cols] == '?', 0, 1))
cat(paste(as.character(auto.price[,cols]),count), '/n')
}}
lapply(auto.price, function(x){any(x == '?')})
for (col in names(auto.price){
for (col in names(auto.price)){
if(is.character(auto.price[,col])){
count = sum(ifelse(auto.price[,col] == '?', 1, 0))
cat(paste(col, as.character(count), '/n'))
}}
col = c('nnum.of.doors')
auto.price[,col] = lapply(auto.price[,col], function(x){ifelse(x == '?', NA, x)})
col = c('num.of.doors')
auto.price[,col] = lapply(auto.price[,col], function(x){ifelse(x == '?', NA, x)})
dim(auto.price)
str(auto.price)
lapply(auto.price, function(x){any(x == '?')})
lapply(auto.price, function(x){any(x == 'NA')})
num_cols = c('price', 'bore', 'stroke', 'horsepower', 'peak.rpm')
for(col in num_cols){
temp = auto.price[,col]
auto.price[,col] = ifelse(temp == '?', 'NA', auto.price[,col])}
lapply(auto.price, function(x){any(x == 'NA')})
lapply(auto.price[,cols], function(x){any(x == 'NA')})
lapply(auto.price[,col], function(x){any(x == 'NA')})
auto.price[,num_cols] <- lapply(auto.price[,num_cols], as.numeric)
auto.price = auto.price[complete.cases(auto.price[,num_cols]),]
auto.price[,'symboling'] = NULL
auto.price[, 'normalized.losses'] = NULL
dim(auto.price)
colnames(auto.price)
head(auto.price)
str(auto.price)
summary(auto.price)
for (col in colnames(auto.price)){
if(is.numeric(auto.price[,col])){
cat(paste(col, as.character(round(sd(auto.price[,col]),2)), '/n'))
}}
for (col in colnames(auto.price)){
if(is.numeric(auto.price[,col])){
cat(paste(col, as.character(round(sd(auto.price[,col]),2)), '\n'))
}}
table(auto.price[,cols])
table(auto.price[,col])
table(auto.price[,"make"])
for(col in colnames(auto.price)){
if(is.numeric(auto.price[,col])){
cat(paste('Frequency table for', col))
print(table(auto.price[,col]))
cat('\n')
}}
for(col in colnames(auto.price)){
if(is.character(auto.price[,col])){
cat(paste('Frequency table for', col))
print(table(auto.price[,col]))
cat('\n')
}}
plot_bars = function(df){
options(repr.plot.width = 4, repr.plot.height =  4)
for (col in colnames(df)){
if(is.character(df[,col])){
p = ggplot(df, aes_string(col)) + geom_bar(alpha = 0.6 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p)
plot_bars = function(df){
options(repr.plot.width = 4, repr.plot.height =  4)
for (col in colnames(df)){
if(is.character(df[,col])){
p = ggplot(df, aes_string(col)) + geom_bar(alpha = 0.6 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p)}}}
plot_bars = function(df){
options(repr.plot.width = 4, repr.plot.height =  4)
for (col in colnames(df)){
if(is.character(df[,col])){
p = ggplot(df, aes_string(col)) + geom_bar(alpha = 0.6) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p) }}}
plot_bars(auto.price)
plot_hist = function(df, numcols, bins = 10)
{}
plot_hist = function(df, numcols, bins = 10){
options(repr.plot.width = 4, repr.plot.height = 4)
for(col in numcols){
if(is.numeric(df[,col])){
bw = (max(df[,col]) - min(df[,col])/ (bins + 1))
p = ggplot(df, aes_string(col)) + geom_histogram(alpha = 0.6, binwidth = bw)
print(p) }}}
numcols = c('curb.weight', 'engine.size', 'horsepower', 'city.mpg', 'price')
plot_hist(auto.price, numcols)
numcols = c('curb.weight', 'engine.size', 'horsepower', 'city.mpg', 'price')
options(repr.plot.width=6, repr.plot.height=6) # Set the initial plot area dimensions
ggpairs(auto_prices,
columns = numcols,
aes(color = fuel.type, alpha = 0.1),
lower = list(continuous = 'points'),
upper = list(continuous = ggally_density))
ggpairs(auto.price,
columns = numcols,
aes(color = fuel.type, alpha = 0.1),
lower = list(continuous = 'points'),
upper = list(continuous = ggally_density))
plot_hist_grid = function(df, numcols, bins = 10){
options(repr.plot.width=6, repr.plot.height=3) # Set the initial plot area dimensions
for(col in numcols){
if(is.numeric(df[,col])){
bw = (max(df[,col]) - min(df[,col]))/(bins + 1)
p = ggplot(df, aes_string(col)) +
geom_histogram(binwidth = bw, aes(y=..density..), alpha = 0.5) +
geom_density(aes(y=..density..), color = 'blue') +
geom_rug() +
facet_grid(. ~ drive.wheels)
print(p)
}
}
}
plot_hist_grid(auto.price, numcols)
plot_scatter_grid = function(df, cols, col_y = 'price', alpha = 1.0){
options(repr.plot.width=7, repr.plot.height=5) # Set the initial plot area dimensions
for(col in cols){
p = ggplot(df, aes_string(col, col_y)) +
geom_point(aes(color = fuel.type), alpha = alpha) +
ggtitle(paste('Scatter plot of', col_y, 'vs.', col,
'\n conditioned on drive wheels and body style',
'\n with color by fuel type')) +
facet_grid(drive.wheels ~ body.style)
print(p)
}
}
numcols = c('curb.weight', 'engine.size', 'horsepower', 'city.mpg')
plot_scatter_grid(auto.price, numcols, alpha = 0.2)
## Import packages
library(ggplot2)
library(repr)
options(repr.plot.width=4, repr.plot.height=4) # Set the initial plot area dimensions
set.seed(34567)
df = data.frame(x = seq(from = 0.0, to = 10.0, by = 0.1))
df$y = df$x + rnorm(length(df$x), mean = 0.0, sd = 1.0)
ggplot(df, aes(x,y)) + geom_point()
set.seed(9988)
test_idx <- sample(seq_len(nrow(df)), size = 50)
train <- df[test_idx, ] # Select the training rows
test <- df[-test_idx, ] # Select the test rows
dim(train)
dim(test)
set.seed(9988)
test_idx <- sample(seq_len(nrow(df)), size = 50)
train <- df[test_idx, ] # Select the training rows
test <- df[-test_idx, ] # Select the test rows
dim(train)
dim(test)
scale = sd(train$y)
center = mean(train$y)
train$y = (train$y - center)/scale
test$y = (test$y - center)/scale
## fit the linear regression model
lin_mod = lm(y ~ x, data = df)
summary(lin_mod)$coefficients
plot_regression = function(score, df){
df$score = score
ggplot(df) + geom_point(aes(x,y)) +
geom_line(aes(x,score), color = 'red', size = 1) +
ggtitle('Regression of X vs. Y')
}
score = predict(lin_mod, data = test)
plot_regression(score, df)
## Import packages
library(ggplot2)
library(repr)
library(caret)
library(ROCR)
library(pROC)
options(repr.plot.width=4, repr.plot.height=4) # Set the initial plot area dimensions
install.packages(lattice)
install.packages("lattice")
install.packages("lattice")
install.packages("lattice")
install.packages("lattice")
install.packages("lattice")
install.packages("lattice")
install.packages("lattice")
xseq = seq(-7, 7, length.out = 500)
xseq
plot.logistic = function(v){
options(repr.plot.width=5, repr.plot.height=4)
logistic = exp(xseq - v)/(1 + exp(xseq - v))
df = data.frame(x = xseq, y = logistic)
ggplot(df, aes(x,y)) +
geom_line(size = 1, color = 'red') +
geom_vline(xintercept = v, size = 1, color = 'black') +
geom_hline(yintercept = 0.5, size = 1, color = 'black') +
ylab('log likelihood') + xlab('Value of x') +
ggtitle('Logistic function for \n two-class classification') +
theme_grey(base_size = 18)
}
plot.logistic(0)
library(ggplot2)
plot.logistic = function(v){
options(repr.plot.width=5, repr.plot.height=4)
logistic = exp(xseq - v)/(1 + exp(xseq - v))
df = data.frame(x = xseq, y = logistic)
ggplot(df, aes(x,y)) +
geom_line(size = 1, color = 'red') +
geom_vline(xintercept = v, size = 1, color = 'black') +
geom_hline(yintercept = 0.5, size = 1, color = 'black') +
ylab('log likelihood') + xlab('Value of x') +
ggtitle('Logistic function for \n two-class classification') +
theme_grey(base_size = 18)
}
plot.logistic(0)
## Import packages
library(ggplot2)
library(repr)
library(dplyr)
library(caret)
library(glmnet)
options(repr.plot.width=4, repr.plot.height=4) # Set the initial plot area dimensions
install.packages("caret")
install.packages("caret")
library(caret)
install.packages("lattice")
install.packages("lattice")
library(caret)
install.packages("Matrix")
install.packages("Matrix")
library(caret)
library(glmnet)
install.packages("glmnet")
library(glmnet)
options(repr.plot.width=4, repr.plot.height=4) # Set the initial plot area dimensions
auto_prices = file.choose()
print(dim(auto_prices))
auto_prices = file.choose()
auto_prices = file.choose()
print(dim(auto_prices))
auto_prices = read.csv('Auto_Prices_Preped.csv')
auto_prices = file.choose()
Auto_Prices_Preped <- read.csv("C:/Users/arpitha/Downloads/Auto_Prices_Preped.csv")
View(Auto_Prices_Preped)
print(dim(Auto_Prices_Preped))
names(Auto_Prices_Preped)
set.seed(1995)
partition = CreateDataPartition(Auto_Prices_Preped[, "fuel.type"], times = 1, p = 0.75, list = FALSE   )
library(caret)
library(Matrix)
library(caret)
partition = CreateDataPartition(Auto_Prices_Preped[, "fuel.type"], times = 1, p = 0.75, list = FALSE   )
install.packages("caret", dependencies = TRUE)
partition = CreateDataPartition(Auto_Prices_Preped[, "fuel.type"], times = 1, p = 0.75, list = FALSE   )
library(caret)
partition = CreateDataPartition(Auto_Prices_Preped[, "fuel.type"], times = 1, p = 0.75, list = FALSE   )
partition = createDataPartition(auto_prices[,'fuel.type'], times = 1, p = 0.75, list = FALSE)
library(ggplot2)
library(repr)
library(tidyverse)
data("iris"
data("iris"
data("iris")
data(iris)
head(iris)
str(iris)
table(iris$Species ) #to know how many categories are in Species
#categories are spread out evenly in the dataset
options(repr.plot.height = 5, repr.plot.width = 4)#set the initial plot area dimenions
ggplot(iris, aes(sepal.width, Sepal.Length)) + geom_point(aes(color = Species))
ggplot(iris, aes(Sepal.Width, Sepal.Length)) + geom_point(aes(color = Species))
ggplot(iris, aes(Petal.Width, Sepal.Length)) + geom_point(aes(color = Species))
iris[,c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width')] = lapply(iris[,c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width')], scale)
print(summary(iris))
print(sapply(iris[,c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width')], sd))
library(dplyr)
set.seed(2345)
train.iris <- sample_frac(iris, 0.7)
test.iris <- iris[-as.numeric(rownames(train.iris)),]
dim(train.iris)
dim(test.iris)
library(kknn)
kknn.3 <- kknn(Species ~ ., train = train.iris, test = test.iris, k = 3)
summary(kknn.3)
test.iris.predicted <- predict(kknn.3)
test.iris.predicted
test.iris
test.iris$correct = test.iris.predicted == test.iris$Species
round(100 * sum(test.iris$correct)/nrow(test.iris))
test.iris$correct
ggplot(test.iris, aes(Sepal.Width, Sepal.Length)) + geom_point(aes(color = predicted, shape = correct))
ggplot(test.iris, aes(Sepal.Width, Sepal.Length)) + geom_point(aes(color = test.iris.predicted, shape = correct))
ggplot(test.iris, aes(Petal.Width, Sepal.Length)) + geom_point(aes(color = test.iris.predicted, shape = correct))
rstudioapi::documentSave()
savehistory("C:/Users/arpitha/Downloads/KNN_plus_oldcode.R")
