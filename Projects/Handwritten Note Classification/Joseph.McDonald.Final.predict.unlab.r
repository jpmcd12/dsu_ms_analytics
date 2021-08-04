######## Framework code for this project was developed from code provided on D2L for STAT 602 on April 30, 2020 ########

#### To make predictions, change the below variable to the file 
#### name of the unlabeled data to be classified
predict_me.csv <- "unlab.example.trial.csv"

##### Libraries ########
library(MASS)
library(class)
########################

#############
set.seed(72118)

################## Load and format training data #######################
#load and format training data
trn.dat=read.csv("labeled.csv", stringsAsFactors = F)[, -1]
#head(trn.dat)[, 1:6]


#collapse segments on unique trials creating means vectors
trn.dat.means=NULL
inde.vars=NULL
index.var=apply(trn.dat[, 1:4], 1, paste, collapse = ":")
uni.vars=unique(index.var)
for (i in uni.vars){
  trn.dat.i=trn.dat[i==index.var, ]
  trn.mean.i=colMeans(trn.dat.i[,-(1:5)])
  inde.vars=rbind(inde.vars, trn.dat.i[1,(1:4)])
  trn.dat.means=rbind(trn.dat.means, trn.mean.i)
}

#dim(trn.dat.means)
#summary(trn.dat.means)
#cbind(colnames(trn.dat.means))
#format training data to match analysis script
trn.dat.means=trn.dat.means[, -c(13, 14)]

#PCA from original Train means
prin.comp.mod=princomp(trn.dat.means, cor = T)

################ Formatting Unlab data for testing #####################
#import unlabled data
unlabeled.examp=read.csv(predict_me.csv,
                         stringsAsFactors = F)[, -1]


#Making the Means vectors for each Trial
unlab.dat.means=NULL
uni.vars.unlab=unique(unlabeled.examp$Trial)
for (i in uni.vars.unlab){
  unlab.dat.i=unlabeled.examp[i==unlabeled.examp$Trial, ]
  unlab.mean.i=colMeans(unlab.dat.i[,-(1:2)])
  unlab.dat.means=rbind(unlab.dat.means, unlab.mean.i)
}

formated.unlab=unlab.dat.means[, -(13:14)]

################# Best model from analysis #############################
#collapse classes into joint classes
classes.joint = apply(inde.vars[,1:3], 1, paste, collapse = ":")

#train lda model on joint classes with 21 comps and CV for estimated accuracy
lda.mod.joint.prin.cv <- lda(x=prin.comp.mod$scores[, 1:21], grouping = classes.joint, CV=T)
final.joint.acc.lda <- mean(paste(lda.mod.joint.prin.cv$class)==classes.joint)

#estimated accuracy of predictions
print(paste("Estimated Accuracy:", round(final.joint.acc.lda,4)))

#train same model with no cv and make predictions
lda.mod.joint.prin <- lda(x=prin.comp.mod$scores[, 1:21], grouping = classes.joint)

#make predictions
unlab.pred <- predict(lda.mod.joint.prin, newdata = predict(prin.comp.mod, newdata=formated.unlab)[, 1:21])$class
unlab.pred.df <- data.frame("PredictedClass" = unlab.pred)
unlab.pred.df

####################################################################################################
####################################################################################################
#Double check for cor=f
# 
# dim(prin.comp.mod$loadings)
# dim(formated.unlab)
# matrix(prin.comp.mod$center, nrow = 2, ncol=23, byrow = T)
# centered.unlab=formated.unlab-
#   matrix(prin.comp.mod$center, nrow = 2, ncol=23, byrow = T)
# 
# dim(centered.unlab)
# dim(prin.comp.mod$loadings)
# 
# scores.unlab=t(t(prin.comp.mod$loadings)%*%t(centered.unlab))
# 
# cbind(t(predict(prin.comp.mod, newdata=formated.unlab)), t(scores.unlab))























