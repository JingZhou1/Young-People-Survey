setwd('/Users/jing/Desktop/MSBA/Spring/MKT591/TeamAssignment')
# read csv file
survey = read.csv('responses.csv', , header = TRUE)

# preprocessing 
# remove rows with missing values
complete.cases(survey)
cleanedData <- survey[complete.cases(survey), ]
cleanedData <- na.omit(survey)

# remove outliers:(Multivariate) Outlier detection: Small number of 
# participants often cheats and randomly answers the questions
test = as.numeric(cleanedData[2,]);
var(test) == 1
test
nrow(cleanedData)

# only includes music perference and entertainment spending
cols = c(2:19, 137)
mydata = cleanedData[,cols]
head(mydata)
dim(mydata)

# Dimention Reduction 
#Step 1:Correlation Matrix of Independent Variables 
library(corrplot)
corr.music <- cor(mydata[,1:18])
corrplot(corr.music, method="color", tl.cex = 0.7)

#Step 2: Estimating # of Factors
library(nFactors)
nScree(mydata[,1:18]) # estimate the number of factors from scree tests:5
eigen(cor(mydata[,1:18])) #selection heuristics - eigenvalue 1

#Step 3:Factor Analysis
library(GPArotation)
library(psych)
factanal(mydata[,2:18], factors=5) 
factor_res=factanal(mydata[,2:18], factors=5, rotation="oblimin",scores="Bartlett")
X=as.matrix(mydata[,2:18])
fX=as.matrix(factor_res$scores)
Y=mydata[,19]
summary(lm(Y~X))
summary(lm(Y~fX))


# Clustering: Given the music preferences, do people make up
# any clusters of similar behavior?

### Method1: Kmeans 
# Determine number of clusters: 5
library(cluster)
library(factoextra)
fviz_nbclust(mydata, kmeans, method = "wss")

set.seed(1000)
mydata.k<-kmeans(mydata, centers=5)

mydata.k$cluster

mydata.summ<-function(data,groups){
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
} # computing group-level mean values

mydata.summ(mydata, mydata.k$cluster)

# boxplot in terms of income
boxplot(mydata$Entertainment.spending~mydata.k$cluster, 
        ylab="Entertainment.spending", xlab="Cluster")

# append cluster assignment
# mydata <- data.frame(mydata, fit$cluster)

# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph

clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)


### Method2: Hierarchical Clustering
mydata.dist<-daisy(mydata)#compute dissimilarity matrix

as.matrix(mydata.dist)[1:5,1:5]
dim(as.matrix(mydata.dist)) #distances between 686 members

mydata.hc<-hclust(mydata.dist, method="complete")
plot(mydata.hc) #resulting tree for all N=686 observations 

# decide number of segments based on dendrogram
cut(as.dendrogram(mydata.hc), h=13)

plot(cut(as.dendrogram(mydata.hc), h=13)$lower[[1]])#cut with 13 in the plot

### Check similarity example ###
mydata[c(686,19),]

### Specifying the number of groups we want ###
plot(mydata.hc)
rect.hclust(mydata.hc, k=4, border="red") #prespecified K=4

#assignment vector
mydata.hc.segment <- cutree(mydata.hc, k=4) #membership for 4 groups


### Method3:Mclust
library(mclust)

mydata.mc <- Mclust(mydata)

summary(mydata.mc)

mydata.summ(mydata, mydata.mc$classification) #2 components

clusplot(mydata, mydata.mc$class, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="Model-based cluster plot")

# plotcluster(mydata, fit$cluster)

