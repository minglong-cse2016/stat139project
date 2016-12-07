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
#read data
nrow=400
path="K:\\homework\\STAT 139\\project\\data\\"
binaryData = load_data(path,400,0,1)
data = load_data(path,nrow)
binary_data_without_label=binaryData[,-which(names(binaryData) == "label")]
data_without_label=data[,-which(names(data) == "label")]
#================================
# PCA implementation on MNIST
#================================
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
binary_percent_col_pixel = rowSums(binary_data_without_label != 0)/length(binary_data_without_label)
#--------------------------------------
# average grey scale
#--------------------------------------
average_col = rowSums(data_without_label)/length(data_without_label)
binary_average_col = rowSums(binary_data_without_label)/length(binary_data_without_label)
#--------------------------------------
# aggregate new features
#--------------------------------------
new_features = data.frame(data$label,percent_col_pixel,average_col)
binary_new_features = data.frame(binaryData$label,binary_percent_col_pixel,binary_average_col)
#======================================
# Logistic Regression
#======================================
library(nnet)
library("mlogit")

#--------------------------------------
# Original data
#--------------------------------------
#binaryData = load_data(path,nrow,0,1)
mldata<-mlogit.data(new_data, choice="data.label")
covar <- names(new_data)[1:10]
mlogit.model<- mlogit(as.formula(paste("data.label", "~1|", paste(covar, collapse="+"))), data = mldata)

##binary
samples = sample(1200)
fit1 <- glm(label ~ ., family=binomial("logit"), data=binaryData[samples[1:1000],])
summary1<-summary(fit1)
fitted.results <- predict(fit1,newdata=binaryData[samples[1000:1200],],type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != binaryData$label[samples[1000:1200]])
print(paste('Accuracy',1-misClasificError))

avona1<-anova(fit1)
#--------------------------------------
# PCAed data
#--------------------------------------
fit2 <- multinom(data.label ~ ., data = new_data)
summary2<-summary(fit2)
#--------------------------------------
# Created Feature
#--------------------------------------
#binary
samples = sample(1200)
fit1 <- glm(binaryData.label ~ ., family=binomial("logit"), data=binary_new_features[samples[1:1000],])
summary1<-summary(fit1)
fitted.results <- predict(fit1,newdata=binary_new_features[samples[1000:1200],],type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != binary_new_features$binaryData.label[samples[1000:1200]])
print(paste('Accuracy',1-misClasificError))
#multiclass
samples = sample(length(new_features[,1]))
fit3 <- multinom(data.label ~ ., data = new_features[samples[1:3400],])
fitted.results <- predict(fit3,newdata=new_features[samples[1:3400],],type="probs")
results<-apply(fitted.results,1,which.max)-1
misClasificError <- mean(results != new_features$data.label[samples[1:3400]])
print(paste('Accuracy',1-misClasificError))

fitted.results <- predict(fit3,newdata=new_features[samples[3400:4400],],type="probs")
results<-apply(fitted.results,1,which.max)-1
misClasificError <- mean(results != new_features$data.label[samples[3400:4400]])
print(paste('Accuracy',1-misClasificError))
summary(fit3)