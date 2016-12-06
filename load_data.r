#============================================================
# Functions
#============================================================
#--------------------------------------
#load_data
#--------------------------------------
#input: path to file, number of rows per file,
#output: data frame of augmented data with $label
load_data<-function(path,nrow,from=0,to=9){
  f0=paste(path,"mnist_digit_0.csv",sep="")
  data=read.table(f0,header = FALSE,nrows=nrow)
  data$label=0
  for(i in from:to){
    currentf = paste(path,"mnist_digit_",i,".csv",sep="")
    currentdata = read.table(currentf,header = FALSE,nrows=nrow)
    currentdata$label=i
    data=rbind(data,currentdata)
  }
  data$label = as.factor(data$label)
  return(data)
}
#--------------------------------------
#PCA
#--------------------------------------
#this return PCA object
customPCA<-function(data,withLabel=FALSE){
  if (withLabel){
    pcadata=prcomp(data[,-which(names(data) == "label")])
    return(pcadata)
  }
  return(prcomp(data))
}

#============================================================
# Codes
#============================================================
#================================
# PCA implementation on MNIST
#================================
nrow=200
path="K:\\homework\\STAT 139\\project\\"
binaryData = load_data(path,nrow,0,1)
data = load_data(path,nrow)

data_without_label=data[,-which(names(data) == "label")]

pca = customPCA(data_without_label,FALSE)
#dimension reduced data
new_data = data.frame(data$label,pca$x[,1:50])

#-------------------------------
#Plot percentage of variance explained by principal component
#-------------------------------
par(mfrow=c(1,2))
var=pca$sdev^2
pvar = var/sum(var)
plot(pvar, xlab = "Principal Component",
       ylab = "% of Variance Explained",
       type = "b")
cumulative_var=pvar
for (i in 2:length(pvar)){
  cumulative_var[i]=cumulative_var[i-1]+pvar[i]
}
plot(cumulative_var, xlab = "Principal Component",
     ylab = "% of Variance Explained Cumlatively",
     type = "b")

#======================================
# Feature Creation
#======================================
#--------------------------------------
# Percentage of colored pixel
#--------------------------------------
percent_col_pixel = rowSums(data_without_label != 0)/length(data_without_label)
#--------------------------------------
# average grey scale
#--------------------------------------
average_col = rowSums(data_without_label)/length(data_without_label)
#--------------------------------------
# aggregate new features
#--------------------------------------
new_features = data.frame(data$label,percent_col_pixel,average_col)
#======================================
# Logistic Regression
#======================================
library(nnet)
mldata<-mlogit.data(new_data, choice="data.label")
covar <- names(new_data)[-1]
mlogit.model<- mlogit(paste("data.label", "~", paste(covar, collapse="+")), data = mldata)
#--------------------------------------
# Original data
#--------------------------------------
#binaryData = load_data(path,nrow,0,1)
fit1 <- glm(label ~ ., family=binomial("logit"), data=binaryData)
summary1<-summary(fit1)
#--------------------------------------
# PCAed data
#--------------------------------------
fit2 <- multinom(data.label ~ ., data = new_data)
summary2<-summary(fit2)
#--------------------------------------
# Created Feature
#--------------------------------------
fit3 <- multinom(data.label ~ ., data = new_features)
summary(fit3)