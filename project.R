require("car")
library(randomForest)
library(StatMatch)
library(FSelector)
library(plyr)
library(Metrics)
#load the data (Arizona restaurants only)
df.items <- read.csv("restaurants_AZ.csv")

## remove rows with more than 20 NA values
df.items = df.items[which(!(apply(df.items, MARGIN = 1, FUN = function(x) length(x[is.na(x)]) ) >20)),]

## convertt data to 0 (False) and 1 (True)
cname = colnames(df.items)
for(c in cname[!(cname %in% c("city","review_count","name","business_id","stars","price_range","alcohol","attire","noise_level","wifi"))]){
  df.items[[c]] = mapvalues(df.items[[c]], from=c('True', 'False'), to= c(1,0))
}

#random forest imputation for binary values
removedcol = c("city","review_count","name","business_id","stars","price_range","alcohol","attire","noise_level","wifi")
response= df.items$stars
predictor = df.items[ , !(names(df.items) %in% removedcol)]
df.items.impute = rfImpute(predictor, response, iter= 2, ntree=50)
##write.csv(df.items, file = "restaurants_AZ.csv",row.names=FALSE)

## evaluation for random Forest methons
d0= sample(which(df.items$dinner == 0), size= 50)
d1= sample(which(df.items$dinner == 1), size= 50)
test.dinner= c(rep(1,50), rep(0,50))
p0= sample(which(df.items$parking == 0), size= 50)
p1= sample(which(df.items$parking == 1), size= 50)
test.parking= c(rep(1,50), rep(0,50))
df.items$dinner[d0] = NA
df.items$dinner[d1] = NA
df.items$parking[p0] = NA
df.items$parking[p1] = NA
library(randomForest)
removedcol = c("city","review_count","name","business_id","stars","price_range","alcohol","attire","noise_level","wifi")
response= df.items$stars
predictor = df.items[ , !(names(df.items) %in% removedcol)]
df.items.impute = rfImpute(predictor, response, iter= 2, ntree=50)
##write.csv(df.items, file = "restaurants_AZ.csv",row.names=FALSE)
conf.matrix.dinner= table(data.frame(test.dinner, df.items.impute$dinner[c(d1, d0)]))
error.dinner=(conf.matrix.dinner[1,2]+conf.matrix.dinner[2,1])/100
conf.matrix.parking= table(data.frame(test.parking, df.items.impute$parking[c(p1, p0)]))
error.parking=(conf.matrix.parking[1,2]+conf.matrix.parking[2,1])/100

## combin the removed columns
df.items = merge(df.items.impute[,!(colnames(df.items.impute) %in% c("response"))], df.items[,removedcol], 0, all=TRUE)
df.items = df.items[,!(colnames(df.items) %in% c("Row.names"))]
summary(df.items)

# random forest for categorical values
removedcol = c("city","name","business_id","stars")
response= df.items$stars
predictor = df.items[ , !(names(df.items) %in% removedcol)]
df.items.impute = rfImpute(predictor, response, iter= 2, ntree=50)


## combin the removed columns
df.items = merge(df.items.impute[,!(colnames(df.items.impute) %in% c("response"))], df.items[,removedcol], 0, all=TRUE)
df.items = df.items[,!(colnames(df.items) %in% c("Row.names"))]

## the pre processed files were written to file as a backup
df.items.backup= df.items
df.items = read.csv("restaurantData.csv")
summary(df.items)
del = df.items[,(colnames(df.items) %in% c("alcohol","attire","noise_level","wifi","parking"))]
del$parking = as.factor(del$parking)
del$alcohol = as.factor(del$alcohol)
del$attire = as.factor(del$attire)
del$noise_level = as.factor(del$noise_level)
del$wifi = as.factor(del$wifi)


summary(del)
Xdel = model.matrix(parking~.,data=del, contrasts.arg = lapply(del[,2:5], contrasts, contrasts=FALSE))[,-1]
summary(Xdel)
dim(Xdel)
Xdel = data.frame(Xdel)
df.items = merge(df.items[,!(colnames(df.items) %in% c("alcohol","attire","noise_level","wifi","parking"))],Xdel,0, all=T)
df.items = df.items[,!(colnames(df.items) %in% c("Row.names"))]
cname = colnames(df.items)
for(c in cname[!(colnames(df.items) %in% c("business_id","city","name","price_range","review_count","stars"))]){
  df.items[[c]] = as.factor(df.items[[c]])
}
summary(df.items)
###plots###
colors = c("red","green") 
h= plot(df.items$accept_credit_cards, col=colors, xlab="Accept Credit Cards", ylab= "Frequency" ,main= "Distribution")
###########

### Feature selection using information.gain
df.items.filtered = df.items
weights.infgain = information.gain(name~., df.items.filtered[,!(colnames(df) %in% c("business_id","city"))])
weights.infgain
#features to be removed after analyzing weights of different features
feature.remove=c("city","accept_credit_cards","take_out","romantic","intimate","classy","hipster","touristy",
                 "upscale","dessert","attireformal","attiredressy","wifipaid","attirecasual","noise_levelquiet",
                 "noise_levelloud","noise_levelvery_loud","brunch","latenight","wheelchair_accessible","divey",
                 "trendy","good_for_groups","good_for_kids","alcoholbeer_and_wine","breakfast")
df.items.filtered = df.items.filtered[which(df.items.filtered$business_id %in% business.id),!(colnames(df.items.filtered) %in% feature.remove)]
summary(df.items.filtered)
dim(df.items.filtered)

####plots################
ggplot(df.items.filtered, aes(x=review_count))+geom_density() + labs(title = "Distribution of Review count")
ggplot(df.items.filtered, aes(x = factor(0), y = review_count)) + geom_boxplot() + xlab("") +scale_x_discrete(breaks = NULL) + coord_flip() + labs(title = "Box plot for Review count")
#############


df.review <- read.csv("review_AZ.csv")

##Filter users
user.filter = as.data.frame(table(df.review$user_id))
user.filter = user.filter$Var1[which(user.filter$Freq > 40)]
business.filter = as.data.frame(table(df.review$business_id))
business.filter = business.filter$Var1[which(business.filter$Freq > 50)]
df.review.filter = df.review[which(df.review$user_id %in% user.filter),]
df.review.filter = df.review.filter[which(df.review.filter$business_id %in% business.filter),]
business.id = unique(df.review.filter$business_id)
dfrow.sample = sample(business.id,1000)

df.items.sample = df.items.filtered[which(df.items.filtered$business_id %in% dfrow.sample),]
#calculating similarity based on gower.dist
isim.feature = 1 - gower.dist(df.items.sample[,!(colnames(df.items.sample) %in% c("business_id"))])
head(isim.feature)

df.review.sample = df.review.filter[which(df.review.filter$business_id %in% dfrow.sample),]
##plot
ggplot(df.review.sample, aes(x = user_id)) + geom_histogram(binwidth=2)
##
df.review.sample$business_id = as.factor(as.matrix(df.review.sample$business_id))
df.review.sample$user_id = as.factor(as.matrix(df.review.sample$user_id))
levels(df.review.sample$business_id) <- 1:length(levels(df.review.sample$business_id))
levels(df.review.sample$user_id) <- 1:length(levels(df.review.sample$user_id))
summary(df.review.sample)

ratingMatrix <- sparseMatrix(i = as.vector(as.numeric(df.review.sample$user_id)),
                             j = as.vector(as.numeric(df.review.sample$business_id)),
                             x = as.vector(df.review.sample$stars))
dim(ratingMatrix)
m = as.matrix(ratingMatrix)
m = as(m, "realRatingMatrix")
##plot
hist(getRatings(m), breaks=100,xlim = range(1,5), ylim = range(0,20000))
##
m.total = nrow(m)
m.sample = floor(sample(1:m.total,0.8*m.total))
m.train = m[m.sample,]
m.test = m[-m.sample,]
isim.ratings.cosine = similarity(m.train,method = "cosine", which="items")
# isim.ratings.jaccard = similarity(m.train,method = "jaccard", which="items")
# isim.ratings.pearson = similarity(m.train,method = "pearson", which="items")
isim.ratings = as.matrix(isim.ratings.cosine)

#function for getting the k nearest neighbours in rating matrix
getKNN <- function(isim,k){
  for(i in 1:nrow(isim)){
    topK = order(isim.combined[i,],decreasing = T)[1:k]
    isim[i,-topK] = 0
  }
  return(isim)
}

##function to get performance of hybrid model
getperf <- function(m.test,isim,withheld=7){
  perf.mae = 0.0
  perf.rmse = 0.0
  perf.mse = 0.0
  count = 0
  for(i in 1:nrow(m.test)){
    items.total = which(getRatings(m.test[i,]) !=0)
    
    if(length(items.total)>15){
      given = length(items.total) - withheld
      count = count+1
      items.given = sample(items.total,given)
      # print(items.given)
      items.withheld = setdiff(items.total,items.given)
      items.withheld.rating = getRatings(m.test[i,items.withheld])
      items.given.rating = getRatings(m.test[i,items.given])
      predicted.rating = items.withheld.rating
      for(j in 1:length(items.withheld)){
        # print(length(isim[items.withheld[j],items.given]))
        # print(class(isim[,items.given]))
        # print(length(items.given.rating))
        predicted.rating[j] = weighted.mean(items.given.rating,isim[items.withheld[j],items.given])
        if(is.na(predicted.rating[j])){
          predicted.rating[j]=1
        }
      }
      # print(predicted.rating)
      # print(items.withheld.rating)
      perf.mae = perf.mae + mae(items.withheld.rating,predicted.rating)
      perf.mse = perf.mse + mse(items.withheld.rating,predicted.rating)
      perf.rmse = perf.rmse + rmse(items.withheld.rating,predicted.rating)
      # print(perf.mse)
      # print(perf.rmse)
      # print(perf.mae)
    }
  }
  performance = data.frame("mae" = perf.mae/count, "rmse" = perf.rmse/count, "mse" = perf.mse/count)
  return(performance)
}
## Getting the best lambda (weight for getting combined cosine similarity) value
getBestlambda <- function(isim.ratings,isim.feature,m.test){
  hybrid.weight = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
  hybrid.perf = data.frame("mae"= rep.int(0,9),"rmse"=rep.int(0,9),"mse"=rep.int(0,9))
  for(i in 1:9){
    isim.combined = hybrid.weight[i]*isim.ratings + (1-hybrid.weight[i])*isim.feature
    isim.topk = getKNN(isim.combined,100)
    hybrid.perf[i,] = getperf(m.test,isim.topk)
  }
  return(hybrid.perf)
}
hybrid.perf.lambda = getBestlambda(isim.ratings,isim.feature,m.test)
hybrid.perf.lambda


#function for getting best k value
getBestK <- function(isim.combined,m.test){
  k = c(10,50,100,200,400,500)
  hybrid.perf = data.frame("mae"= rep.int(0,6),"rmse"=rep.int(0,6),"mse"=rep.int(0,6))
  for(i in 1:6){
    isim.topk = getKNN(isim.combined,k[i])
    hybrid.perf[i,] = getperf(m.test,isim.topk)
  }
  return(hybrid.perf)
}
hybrid.weight=0.3
isim.combined = hybrid.weight*isim.ratings + (1-hybrid.weight)*isim.feature
hybrid.perf.k = getBestK(isim.combined,m.test)
hybrid.perf.k

isim.topk = getKNN(isim.combined,500)
hybrid.perf = getperf(m.test,isim.topk)

######################### CF Models ##########################################

# read the file
df.review <- read.csv("review_AZ.csv")
df.user = read.csv("user_AZ.csv")
df <- read.csv("restaurants_AZ.csv")
# summary(df.review)
# dim(df.review)
restaurantCounts = unique(df.review$business_id)
length(restaurantCounts)
userCounts = unique(df.review$user_id)
length(userCounts)
starsMean= mean(df.review$stars)


business.id = df$business_id[which(df$city=="Phoenix")]
df.review = subset(df.review, business_id %in% business.id)
#df.review = subset(df.review, user_id %in% df.user$user_id)
summary(df.review)
library(dplyr)

library(plyr)
library(ggplot2)
library(tm)
library(lsa)
library(NMF)
library(scatterplot3d)
library(tm)
library(igraph)
library(recommenderlab)
library(Matrix)

##d4 = subset(df1, business_id %in% df.items.filtered)
df.review[1:4,]
class(df.review$stars)
d4 = data.frame(from = df.review$user_id, to = df.review$business_id, weight = df.review$stars)
# summary(d4)
g = graph.data.frame(d4)
#mat = as.matrix(g,matrix.type = "adjacency")

# get.edgelist(g)
# summary(g)
mat = get.adjacency(g)
#mat.matrix = signature(from = "dgCMatrix", to = "matrix")

#typeof(mat)
#which(mat!=0, arr.ind = T)
#head(mat)
mat.w = get.adjacency(g, attr = "weight")
resturant.idx = which(colSums(mat) > 50.0)
user.idx = which(rowSums(mat) > 30.0)
rmat = mat.w[user.idx , resturant.idx ]
#rmat = rmat[,!(colnames(rmat) %in% c('SstblABX7d_masaQkcsAVg'))]
#which(colnames(rmat)=='SstblABX7d_masaQkcsAVg')
# rownames(rmat)
# head(rmat)
# class(rmat)
# dim(rmat)
# summary(colSums(rmat))
# which(colSums(rmat)==0)
# summary(rowSums(rmat))
# which(rowSums(rmat)==0)
m = as.matrix(rmat)
#which(m!=0, arr.ind = T)
#head(m)
m = as(m, "realRatingMatrix")
#m[1:3]
# m.sample = m[sample(1:nrow(m),1500),]
# head(getRatings(m))
# dim(m)
# as(m[1,],"list")
# m = normalize(m)
#m.total = nrow(m)
#m.train = floor(0.8*m.total)
#m.test = m.total-m.train
#train = sample(1:m.total, m.train)
#train
#r.train = m[train,]
#r.test = m[-train,]
#r.train
# length(getRatings(r))
m_z= normalize(m, method= "Z-score")
hist(getRatings(m), breaks=100, xlim= range(0:5), xlab= "Rating", main= "Rating Distribution")
hist(getRatings(normalize(m)), breaks=100, xlim= range(0:5), xlab= "Rating", main= "Normalized Rating Distribution")
hist(getRatings(m_z), breaks=100, xlim= range(0:5), xlab= "Rating", main= "Z-score Normalized Rating Distribution")
hist(getRatings(m[10:25]), xlim= range(0:5), xlab= "Rating", main= "Rating Distribution")

r.ubcf = Recommender(m, method = "UBCF",)
recom = predict(r.ubcf, m[501:505] , n = 5)
as(recom,"matrix")[,1:10]

e = evaluationScheme(m, method = "split", train = 0.8, given = 5, goodRating = 4)

# measure Accuracy for 3 types of recommendation
r1.jaccard = Recommender(getData(e, "train"), "UBCF", param= list(normalize='Z-score', method='Jaccard',  minRating=1))
r1.cosine = Recommender(getData(e, "train"), "UBCF", param= list(normalize='Z-score', method='Cosine', minRating=1))
r2 = Recommender(getData(e, "train"), "IBCF", param= list(normalize='Z-score',minRating=1))
r3 = Recommender(getData(e, "train"), "POPULAR", param= list(normalize='Z-score'))
p1.cosine = predict(r1.cosine, getData(e, "known"), type = "topNList")
p1.jaccard = predict(r1.jaccard, getData(e, "known"), type = "topNList")
p2.cosine = predict(r1.cosine, getData(e, "known"), type = "ratings")
p2.jaccard = predict(r1.jaccard, getData(e, "known"), type = "ratings")
p3 = predict(r2, getData(e, "known"), type = "topNList")
p4 = predict(r2, getData(e, "known"), type = "ratings")
p5 = predict(r3, getData(e, "known"), type = "topNList")
p6 = predict(r3, getData(e, "known"), type = "ratings")
Accuracy = rbind(calcPredictionAccuracy(p1.cosine, getData(e, "unknown"), given = 5, goodRating = 3), calcPredictionAccuracy(p1.jaccard, getData(e, "unknown"), given = 5, goodRating = 3), calcPredictionAccuracy(p5, getData(e, "unknown"), given = 5, goodRating = 3))
rownames(Accuracy) = c("UBCF-topNList Cosine","UBCF-topNList Jaccard", "POPULAR-topNList")
Accuracy
Accuracy1 = rbind(calcPredictionAccuracy(p2.cosine, getData(e, "unknown")), calcPredictionAccuracy(p2.jaccard, getData(e, "unknown")), calcPredictionAccuracy(p6, getData(e, "unknown")))
# rownames(Accuracy1) = c("UBCF-ratings", "IBCF-ratings", "POPULAR-ratings")
rownames(Accuracy1) = c("UBCF-topNList Cosine","UBCF-topNList Jaccard", "POPULAR-topNList")
Accuracy1

algorithms = list( `UBCF Cosine` = list(name = "UBCF", param = list(method = "Cosine", minRating = 1)), `UBCF Jaccard` = list(name = "UBCF", param = list(method = "Jaccard", minRating = 1)))#, `item-based CF` = list(name = "IBCF", param =list(method = "Jaccard", minRating = 1)))
results = evaluate(e, algorithms, n = c(1, 3, 5, 10))
results1 = evaluate(e, algorithms, type = "ratings")

getConfusionMatrix(results)
avg(results)
plot(results, annotate=TRUE)
plot(results, annotate=c(1,2,3,4), legend="topleft")
plot(results, "prec/rec", annotate=2, ylim= range(0,1), legend="topleft")
plot(results1, ylim=c (0,3) ,legend= "topright")

########
## visualization
rating = df.review$stars
rating.freq = table(rating)
colors = c("violet", "orange", "blue", "pink", "cyan") 
h= barplot(rating.freq, col=colors, xlab="Stars", ylab= "Frequency" ,main= "Rating Distribution")

user = df.review$user_idx
user.freq = table(user)
barplot(user.freq, xlab="User", ylab= "Frequency" ,main= "User Rating Distribution")

restaurant = df.review$business_idx
restaurant.freq = table(restaurant)
barplot(restaurant.freq, xlab="Restaurant", ylab= "Frequency" ,main= "Restaurant Rating Distribution")


bestlambda = data.frame("lambda" = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),
  "mae"=c(1.508451,1.478609,1.422492,1.443728,1.478525,1.464875,1.495938,1.430129,1.415683),
  "rmse"= c(2.083071,1.999895,1.879108,1.908240,1.981210,1.946644,2.022335,1.910825,1.882350),
  "mse" = c(9.152869,6.264984,4.546357,4.917791,5.621328,7.900182,8.349384,5.606510,7.498962))

library(reshape2)
library(ggplot2)
df <- melt(bestlambda , id.vars = 'lambda', variable.name = 'error')
ggplot(df, aes(lambda,value)) + geom_line(aes(colour = error)) + labs(title = "Error variation with Lambda")

bestk = data.frame("kvalue" = c(10,50,100,200,400,500),"mse" = c(2.866232,1.987210,1.719492,1.413654,1.245514,1.209577),
  "rmse" =c(17.560421,8.627900,9.556941,5.457948,4.189812,3.261867),
  "mae"= c(3.48264,2.516032,2.310234,1.888753,1.645347,1.583238))
df.k <- melt(bestk , id.vars = 'kvalue', variable.name = 'error')
ggplot(df.k, aes(kvalue,value)) + geom_line(aes(colour = error)) + labs(title = "Error variation with K")
