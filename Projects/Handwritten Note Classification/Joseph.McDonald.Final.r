
######## Framework code for this project was developed from code provided on D2L for STAT 602 on April 24, 2020 ########

##### Libraries ########
library(MASS)
library(class)
library(mclust)
library(randomForest)
########################
set.seed(72118)

#load and format data
trn.dat=read.csv("labeled.csv", stringsAsFactors = F)[, -1]
head(trn.dat)[, 1:6]

#inspect data
dim(trn.dat)

#collapse segments on unique trials
trn.dat.means=NULL
inde.vars=NULL
index.var=apply(trn.dat[, 1:4], 1, paste, collapse = ":")
table(index.var)
uni.vars=unique(index.var)

for (i in uni.vars){
  trn.dat.i=trn.dat[i==index.var, ]
  trn.mean.i=colMeans(trn.dat.i[,-(1:5)])
  inde.vars=rbind(inde.vars, trn.dat.i[1,(1:4)])
  trn.dat.means=rbind(trn.dat.means, trn.mean.i)
}


dim(trn.dat.means)


summary(trn.dat.means)
cbind(colnames(trn.dat.means))
trn.dat.means=trn.dat.means[, -c(13, 14)]
boxplot.dat.group=data.frame(inde.vars[,1],trn.dat.means)

#box plot of each variable vs Group -- adjusted for overplotting
boxplot(Direction~ inde.vars...1., boxplot.dat.group)
boxplot(log(Duration)~ inde.vars...1., boxplot.dat.group[log(boxplot.dat.group$Duration) < -1,])
boxplot(VerticalSize ~ inde.vars...1., boxplot.dat.group[boxplot.dat.group$VerticalSize > -0.05,])
boxplot(PeakVerticalVelocity ~ inde.vars...1., boxplot.dat.group[boxplot.dat.group$PeakVerticalVelocity > -2,])
boxplot(PeakVerticalAcceleration~ inde.vars...1., boxplot.dat.group[boxplot.dat.group$PeakVerticalAcceleration < 50 & boxplot.dat.group$PeakVerticalAcceleration > -50,])
boxplot(HorizontalSize~ inde.vars...1., boxplot.dat.group[boxplot.dat.group$HorizontalSize > 0,])
boxplot(StraightnessError~ inde.vars...1., boxplot.dat.group[boxplot.dat.group$StraightnessError < 0.2,])
boxplot(Slant~ inde.vars...1., boxplot.dat.group)
boxplot(LoopSurface~ inde.vars...1., boxplot.dat.group[boxplot.dat.group$LoopSurface < .02,])
boxplot(RelativeInitialSlant~ inde.vars...1., boxplot.dat.group)
boxplot(RelativeTimeToPeakVerticalVelocity~ inde.vars...1., boxplot.dat.group)
boxplot(RelativePenDownDuration~ inde.vars...1., boxplot.dat.group, xlab="Class: Group", main = "RelativePendownDuration ~ Group")
boxplot(AbsoluteSize~ inde.vars...1., boxplot.dat.group[boxplot.dat.group$AbsoluteSize < .75,])
boxplot(AverageAbsoluteVelocity~ inde.vars...1., boxplot.dat.group[boxplot.dat.group$AverageAbsoluteVelocity < 10,])
boxplot(Roadlength~ inde.vars...1., boxplot.dat.group[boxplot.dat.group$Roadlength < 1,])
boxplot(AbsoluteyJerk~ inde.vars...1., boxplot.dat.group[boxplot.dat.group$AbsoluteJerk < 2000,])
boxplot(AverageNormalizedyJerkPerTrial~ inde.vars...1., boxplot.dat.group[boxplot.dat.group$AverageNormalizedyJerkPerTrial < 100,])
boxplot(NumberOfPeakAccelerationPoints~ inde.vars...1., boxplot.dat.group[boxplot.dat.group$NumberOfPeakAccelerationPoints < 3,])
boxplot(AveragePenPressure~ inde.vars...1., boxplot.dat.group)


#box plot of each variable vs Subject -- adjusted for overplotting
boxplot.dat.subject=data.frame(inde.vars[,2],trn.dat.means)

boxplot(Direction~ inde.vars...2., boxplot.dat.subject)
boxplot(log(Duration)~ inde.vars...2., boxplot.dat.subject[log(boxplot.dat.subject$Duration) < -1.4,])
boxplot(VerticalSize ~ inde.vars...2., boxplot.dat.subject[boxplot.dat.subject$VerticalSize > -0.02,])
boxplot(PeakVerticalVelocity ~ inde.vars...2., boxplot.dat.subject[boxplot.dat.subject$PeakVerticalVelocity > -2,])
boxplot(PeakVerticalAcceleration~ inde.vars...2., boxplot.dat.subject[boxplot.dat.subject$PeakVerticalAcceleration < 40 & boxplot.dat.subject$PeakVerticalAcceleration > -50,])
boxplot(HorizontalSize~ inde.vars...2., boxplot.dat.subject[boxplot.dat.subject$HorizontalSize > 0.05 & boxplot.dat.subject$HorizontalSize < .25,])
boxplot(StraightnessError~ inde.vars...2., boxplot.dat.subject[boxplot.dat.subject$StraightnessError < 0.2,])
boxplot(Slant~ inde.vars...2., boxplot.dat.subject)
boxplot(LoopSurface~ inde.vars...2., boxplot.dat.subject[boxplot.dat.subject$LoopSurface < .02,])
boxplot(RelativeInitialSlant~ inde.vars...2., boxplot.dat.subject[boxplot.dat.subject$RelativeInitialSlant < .2 & boxplot.dat.subject$RelativeInitialSlant > -.3,])
boxplot(RelativeTimeToPeakVerticalVelocity~ inde.vars...2., boxplot.dat.subject[boxplot.dat.subject$RelativeTimeToPeakVerticalVelocity < .55,])
boxplot(RelativePenDownDuration~ inde.vars...2., boxplot.dat.subject)
boxplot(AbsoluteSize~ inde.vars...2., boxplot.dat.subject[boxplot.dat.subject$AbsoluteSize < .75,])
boxplot(AverageAbsoluteVelocity~ inde.vars...2., boxplot.dat.subject[boxplot.dat.subject$AverageAbsoluteVelocity < 10,])
boxplot(Roadlength~ inde.vars...2., boxplot.dat.subject[boxplot.dat.subject$Roadlength < 1,])
boxplot(AbsoluteyJerk~ inde.vars...2., boxplot.dat.subject[boxplot.dat.subject$AbsoluteJerk < 2000,])
boxplot(AverageNormalizedyJerkPerTrial~ inde.vars...2., boxplot.dat.subject[boxplot.dat.subject$AverageNormalizedyJerkPerTrial < 70,])
boxplot(NumberOfPeakAccelerationPoints~ inde.vars...2., boxplot.dat.subject[boxplot.dat.subject$NumberOfPeakAccelerationPoints < 3,])
boxplot(AveragePenPressure~ inde.vars...2., boxplot.dat.subject, xlab="Class: Subject", main="AveragePenPressure ~ subject")


#box plot of each variable vs Condition (Line) -- adjusted for overplotting
boxplot.dat.line=data.frame(inde.vars[,3],trn.dat.means)

boxplot(Direction~ inde.vars...3., boxplot.dat.line)
boxplot(log(Duration)~ inde.vars...3., boxplot.dat.line[log(boxplot.dat.line$Duration) < -1.4,])
boxplot(VerticalSize ~ inde.vars...3., boxplot.dat.line[boxplot.dat.line$VerticalSize > -0.02,])
boxplot(PeakVerticalVelocity ~ inde.vars...3., boxplot.dat.line[boxplot.dat.line$PeakVerticalVelocity > -2,])
boxplot(PeakVerticalAcceleration~ inde.vars...3., boxplot.dat.line[boxplot.dat.line$PeakVerticalAcceleration < 50 & boxplot.dat.line$PeakVerticalAcceleration > -50,])
boxplot(HorizontalSize~ inde.vars...3., boxplot.dat.line[boxplot.dat.line$HorizontalSize > 0.05 & boxplot.dat.line$HorizontalSize < .25,])
boxplot(StraightnessError~ inde.vars...3., boxplot.dat.line[boxplot.dat.line$StraightnessError < .2,])
boxplot(Slant~ inde.vars...3., boxplot.dat.line[boxplot.dat.line$StraightnessError < 0.2,])
boxplot(LoopSurface~ inde.vars...3., boxplot.dat.line[boxplot.dat.line$LoopSurface < .02,])
boxplot(RelativeInitialSlant~ inde.vars...3., boxplot.dat.line[boxplot.dat.line$RelativeInitialSlant < .2 & boxplot.dat.line$RelativeInitialSlant > -.3,])
boxplot(RelativeTimeToPeakVerticalVelocity~ inde.vars...3., boxplot.dat.line[boxplot.dat.line$RelativeTimeToPeakVerticalVelocity < .55,])
boxplot(RelativePenDownDuration~ inde.vars...3., boxplot.dat.line)
boxplot(AbsoluteSize~ inde.vars...3., boxplot.dat.line[boxplot.dat.line$AbsoluteSize < .75,])
boxplot(AverageAbsoluteVelocity~ inde.vars...3., boxplot.dat.line[boxplot.dat.line$AverageAbsoluteVelocity < 10,])
boxplot(Roadlength~ inde.vars...3., boxplot.dat.line[boxplot.dat.line$Roadlength < 1,])
boxplot(AbsoluteyJerk~ inde.vars...3., boxplot.dat.line[boxplot.dat.line$AbsoluteJerk < 2000,])
boxplot(AverageNormalizedyJerkPerTrial~ inde.vars...3., boxplot.dat.line[boxplot.dat.line$AverageNormalizedyJerkPerTrial < 70,])
boxplot(NumberOfPeakAccelerationPoints~ inde.vars...3., boxplot.dat.line[boxplot.dat.line$NumberOfPeakAccelerationPoints < 3,])
boxplot(AveragePenPressure~ inde.vars...3., boxplot.dat.line)

###explore principle components for Group###

my.cols=c("red", "blue")

#use cov matrix
prin.comp.mod=princomp(trn.dat.means, cor = F)
plot(prin.comp.mod)

#check plot for signal
plot(prin.comp.mod$scores[, 1:2], pch=16, cex=.5,
 col=my.cols[(inde.vars[,1]=="CUR")+1], ylim=c(-1400, 1400), xlim=c(-1400, 1400))

#use cor matrix
prin.comp.mod=princomp(trn.dat.means, cor = T)
plot(prin.comp.mod, main="PCA Model")

#check plots for signal
plot(prin.comp.mod$scores[, 1:2], pch=16, cex=.5,
     col=my.cols[(inde.vars[,1]=="CUR")+1])

plot(prin.comp.mod$scores[, 2:3], pch=16, cex=.5,
     col=my.cols[(inde.vars[,1]=="CUR")+1], main="PCA Comp3 ~ Comp2 Class: Group")

#use pairs plot to check all combos of prin comps for signal
pairs(prin.comp.mod$scores[, 1:10], pch=16, cex=.5,
      col=my.cols[(inde.vars[,1]=="CUR")+1], main = "Pairs Plot for Group")


###explore principle components for condition###

#use cov matrix
prin.comp.mod=princomp(trn.dat.means, cor = F)

#variance explained
plot(prin.comp.mod)

#check for signal of one line vs all -- no signal
plot(prin.comp.mod$scores[, 1:2], pch=16, cex=.5,
 col=my.cols[(inde.vars[,3]=="L6")+1],
 ylim=c(-1400, 1400), xlim=c(-1400, 1400))

#use cor matrix
prin.comp.mod=princomp(trn.dat.means, cor = T)
plot(prin.comp.mod)

#check for signal of one line vs all prin comp 1:2
plot(prin.comp.mod$scores[, 1:2], pch=16, cex=.5,
     col=my.cols[(inde.vars[,3]=="L1")+1])

plot(prin.comp.mod$scores[, 2:3], pch=16, cex=.5,
     col=my.cols[(inde.vars[,3]=="L3")+1])

#use pairs plot to check all combos of prin comps for signal for each line vs all
pairs(prin.comp.mod$scores[, 1:10], pch=16, cex=.5,
      col=my.cols[(inde.vars[,3]=="L1")+1])

pairs(prin.comp.mod$scores[, 1:10], pch=16, cex=.5,
      col=my.cols[(inde.vars[,3]=="L2")+1])

pairs(prin.comp.mod$scores[, 1:10], pch=16, cex=.5,
      col=my.cols[(inde.vars[,3]=="L3")+1])

pairs(prin.comp.mod$scores[, 1:10], pch=16, cex=.5,
      col=my.cols[(inde.vars[,3]=="L4")+1])

pairs(prin.comp.mod$scores[, 1:10], pch=16, cex=.5,
      col=my.cols[(inde.vars[,3]=="L5")+1])

pairs(prin.comp.mod$scores[, 1:10], pch=16, cex=.5,
      col=my.cols[(inde.vars[,3]=="L6")+1])

### No discernable signal found -- not worth pursueing Subject with 40 reps ###

########################################## LDA prin comp #####################################################
################### establish  baseline ######################

###train lda models for each target variable using optimal prin comps
lda.mod.group.prin=lda(x=prin.comp.mod$scores[, 1:10], grouping = inde.vars[,1], CV=T)
mean(lda.mod.group.prin$class==inde.vars[,1])

lda.mod.sub.prin=lda(x=prin.comp.mod$scores[, 1:10], grouping = inde.vars[,2], CV=T)
mean(lda.mod.sub.prin$class==inde.vars[,2])

lda.mod.con.prin=lda(x=prin.comp.mod$scores[, 1:10], grouping = inde.vars[,3], CV=T)
mean(lda.mod.con.prin$class==inde.vars[,3])

#check confusion matrix for condition since it seems to perform the worst
con.confus=data.frame(cbind(paste(lda.mod.con.prin$class),paste(inde.vars[,3])))
xtabs(~X1+X2, con.confus)

############## Check 1 set of predictions
paste(lda.mod.group.prin$class)[1]
paste(lda.mod.sub.prin$class)[1]
paste(lda.mod.con.prin$class)[1]

############# Find accuracy when predicting joint classes
classes.joint = apply(inde.vars[,1:3], 1, paste, collapse = ":")
lda.mod.joint.prin=lda(x=prin.comp.mod$scores[, 1:10], grouping = classes.joint, CV=T)
lda.mod.joint.prin$class[1:3]

#baseline accuracy for joint predictions
mean(paste(lda.mod.joint.prin$class)==classes.joint)
#not great, seems to be held up by condition


############# join Group and Condition and predict Subject seperately
group.con <- apply(inde.vars[,c(1,3)], 1, paste, collapse = ":")
lda.mod.groupcon.prin <- lda(x=prin.comp.mod$scores[,1:10], grouping = group.con, CV=T)

#predict sub
lda.mod.sub.prin=lda(x=prin.comp.mod$scores[, 1:10], grouping = inde.vars[,2], CV=T)

#join predictions from seperate models
lda.mod.prin.pred <- cbind(paste(lda.mod.groupcon.prin$class), paste(lda.mod.sub.prin$class))
lda.mod.prin.pred <- apply(lda.mod.prin.pred, 1, paste, collapse = ":")

#join ground truths
lda.mod.prin.gt <- cbind(paste(group.con), paste(inde.vars[,2]))
lda.mod.prin.gt <- apply(lda.mod.prin.gt, 1, paste, collapse = ":")

### much worse ###
mean(lda.mod.prin.pred == lda.mod.prin.gt)

############# add up 3 seperate models for joint predictions
lda.mod.group.prin=lda(x=prin.comp.mod$scores[, 1:10], grouping = inde.vars[,1], CV=T)
lda.mod.sub.prin=lda(x=prin.comp.mod$scores[, 1:10], grouping = inde.vars[,2], CV=T)
lda.mod.con.prin=lda(x=prin.comp.mod$scores[, 1:10], grouping = inde.vars[,3], CV=T)
lda.pred <- cbind(paste(lda.mod.group.prin$class), paste(lda.mod.sub.prin$class), paste(lda.mod.con.prin$class))
lda.pred <- apply(lda.pred, 1, paste, collapse = ":")

###again worse results than the factorialized classes
mean(lda.pred == classes.joint)

##### factorialized seems to be best classification method when working with single model

######################### find optimal numbers of prin comps #####################################

###determine optimal numb of prin comps using a loop for sub
#####################
lda.acc.check <- NULL
num.prin.comp <- NULL
for (i in (2:23)){
  lda.mod.sub.prin=lda(x=prin.comp.mod$scores[,1:i], grouping = inde.vars[,2], CV=T)
  lda.acc.check <- rbind(lda.acc.check, mean(lda.mod.sub.prin$class==inde.vars[,2]))
  num.prin.comp <- rbind(num.prin.comp, i)
}
lda.acc.check <- cbind(num.prin.comp, lda.acc.check)

#plot results and print num prin comp with max acc
plot(lda.acc.check, ylab="CV Accuracy", xlab = "Num Comp", main=paste(lda.acc.check[which.max(lda.acc.check[,2])], "Components = max Accuracy:", max(lda.acc.check[,2])))
opt.prin.comp.lda.sub <- lda.acc.check[which.max(lda.acc.check[,2])]
opt.prin.comp.lda.sub

#####################
##################### Repeat for Group
lda.acc.check <- NULL
num.prin.comp <- NULL
for (i in (2:23)){
  lda.mod.group.prin=lda(x=prin.comp.mod$scores[,1:i], grouping = inde.vars[,1], CV=T)
  lda.acc.check <- rbind(lda.acc.check, mean(lda.mod.group.prin$class==inde.vars[,1]))
  num.prin.comp <- rbind(num.prin.comp, i)
}
lda.acc.check <- cbind(num.prin.comp, lda.acc.check)

#plot results and print num prin comp with max acc
plot(lda.acc.check, ylab="CV Accuracy", xlab = "Num Comp", main=paste(lda.acc.check[which.max(lda.acc.check[,2])], "Components = max Accuracy:", max(lda.acc.check[,2])))
opt.prin.comp.lda.group <- lda.acc.check[which.max(lda.acc.check[,2])]
opt.prin.comp.lda.group

#####################
##################### Repeat for Condition
lda.acc.check <- NULL
num.prin.comp <- NULL
for (i in (2:23)){
  lda.mod.con.prin=lda(x=prin.comp.mod$scores[,1:i], grouping = inde.vars[,3], CV=T)
  lda.acc.check <- rbind(lda.acc.check, mean(lda.mod.con.prin$class==inde.vars[,3]))
  num.prin.comp <- rbind(num.prin.comp, i)
}
lda.acc.check <- cbind(num.prin.comp, lda.acc.check)

#plot results and print num prin comp with max acc
plot(lda.acc.check, ylab="CV Accuracy", xlab = "Num Comp", main=paste(lda.acc.check[which.max(lda.acc.check[,2])], "Components = max Accuracy:", max(lda.acc.check[,2])))
opt.prin.comp.lda.con <- lda.acc.check[which.max(lda.acc.check[,2])]
opt.prin.comp.lda.con

#####################

##################### Repeat for joint classes
classes.joint = apply(inde.vars[,1:3], 1, paste, collapse = ":")
lda.acc.check <- NULL
num.prin.comp <- NULL
for (i in (2:23)){
  lda.mod.joint.prin=lda(x=prin.comp.mod$scores[,1:i], grouping = classes.joint, CV=T)
  lda.acc.check <- rbind(lda.acc.check, mean(lda.mod.joint.prin$class==classes.joint))
  num.prin.comp <- rbind(num.prin.comp, i)
}
lda.acc.check <- cbind(num.prin.comp, lda.acc.check)

#plot results and print num prin comp with max acc
plot(lda.acc.check, ylab="CV Accuracy", xlab = "Num Comp", main=paste(lda.acc.check[which.max(lda.acc.check[,2])], "Components = max Accuracy:", max(lda.acc.check[,2])))
opt.prin.comp.lda.joint <- lda.acc.check[which.max(lda.acc.check[,2])]
opt.prin.comp.lda.joint

#####################

##################### Repeat for group.con
group.con <- apply(inde.vars[,c(1,3)], 1, paste, collapse = ":")
lda.acc.check <- NULL
num.prin.comp <- NULL
for (i in (2:23)){
  lda.mod.gc.prin=lda(x=prin.comp.mod$scores[,1:i], grouping = group.con, CV=T)
  lda.acc.check <- rbind(lda.acc.check, mean(lda.mod.gc.prin$class==group.con))
  num.prin.comp <- rbind(num.prin.comp, i)
}
lda.acc.check <- cbind(num.prin.comp, lda.acc.check)

#plot results and print num prin comp with max acc
plot(lda.acc.check, ylab="CV Accuracy", xlab = "Num Comp", main=paste(lda.acc.check[which.max(lda.acc.check[,2])], "Components = max Accuracy:", max(lda.acc.check[,2])))
opt.prin.comp.lda.gc <- lda.acc.check[which.max(lda.acc.check[,2])]
opt.prin.comp.lda.gc

#####################

prin.comp.mod$loadings
### 21 componenets explains 91% of variance

################# train lda models using optimal prin comps #################

#group
lda.mod.group.prin=lda(x=prin.comp.mod$scores[, 1:opt.prin.comp.lda.group], grouping = inde.vars[,1], CV=T)
lda.acc.group <- mean(lda.mod.group.prin$class==inde.vars[,1])
lda.acc.group

#subject
lda.mod.sub.prin=lda(x=prin.comp.mod$scores[, 1:opt.prin.comp.lda.sub], grouping = inde.vars[,2], CV=T)
lda.acc.sub <- mean(lda.mod.sub.prin$class==inde.vars[,2])
lda.acc.sub

#condition
lda.mod.con.prin=lda(x=prin.comp.mod$scores[, 1:opt.prin.comp.lda.sub], grouping = inde.vars[,3], CV=T)
lda.acc.con <- mean(lda.mod.con.prin$class==inde.vars[,3])
lda.acc.con

#check confusion matrix for condition since it seems to perform the worst
con.confus=data.frame(cbind(paste(lda.mod.con.prin$class), paste(inde.vars[,3])))
#its looking better!
xtabs(~X1+X2, con.confus)

#joint classes
classes.joint = apply(inde.vars[,1:3], 1, paste, collapse = ":")
lda.mod.joint.prin=lda(x=prin.comp.mod$scores[, 1:opt.prin.comp.lda.joint], grouping = classes.joint, CV=T)
lda.acc.joint <- mean(paste(lda.mod.joint.prin$class)==classes.joint)
lda.acc.joint

###join Group and Condition and predict Subject marginally
group.con <- apply(inde.vars[,c(1,3)], 1, paste, collapse = ":")
lda.mod.groupcon.prin <- lda(x=prin.comp.mod$scores[,1:opt.prin.comp.lda.gc], grouping = group.con, CV=T)

#predict sub
lda.mod.sub.prin=lda(x=prin.comp.mod$scores[, 1:opt.prin.comp.lda.sub], grouping = inde.vars[,2], CV=T)

#join predictions from seperate models
lda.mod.prin.pred <- cbind(paste(lda.mod.groupcon.prin$class), paste(lda.mod.sub.prin$class))
lda.mod.prin.pred <- apply(lda.mod.prin.pred, 1, paste, collapse = ":")

#join ground truths
lda.mod.prin.gt <- cbind(paste(group.con), paste(inde.vars[,2]))
lda.mod.prin.gt <- apply(lda.mod.prin.gt, 1, paste, collapse = ":")

### much worse ###
mean(lda.mod.prin.pred == lda.mod.prin.gt)

############# add up 3 seperate models
lda.mod.group.prin=lda(x=prin.comp.mod$scores[, 1:opt.prin.comp.lda.group], grouping = inde.vars[,1], CV=T)
lda.mod.sub.prin=lda(x=prin.comp.mod$scores[, 1:opt.prin.comp.lda.sub], grouping = inde.vars[,2], CV=T)
lda.mod.con.prin=lda(x=prin.comp.mod$scores[, 1:opt.prin.comp.lda.con], grouping = inde.vars[,3], CV=T)


lda.pred <- cbind(paste(lda.mod.group.prin$class), paste(lda.mod.sub.prin$class), paste(lda.mod.con.prin$class))
lda.pred <- apply(lda.pred, 1, paste, collapse = ":")

###again worse results than the factorialized classes
mean(lda.pred == classes.joint)

###only use factorialzed classes unless mixing and matching model types

#store accuracies before moving on to KNN
train.acc <- data.frame("LDA" = c(lda.acc.group, lda.acc.sub, lda.acc.con , lda.acc.joint))
rownames(train.acc) <- c("Group", "Subject", "Condition", "Joint")
train.acc

############################################################################################################
######################################## KNN w/ LOOCV - prin comp ##########################################

##################### Group
acc.check <- NULL
num.prin.comp <- NULL
for (i in (2:23)){
  knn.mod.group.prin = knn.cv(prin.comp.mod$scores[,1:i], cl = inde.vars[,1])
  acc.check <- rbind(acc.check, mean(knn.mod.group.prin==inde.vars[,1]))
  num.prin.comp <- rbind(num.prin.comp, i)
  }

acc.check <- cbind(num.prin.comp, acc.check)

#plot results and print num prin comp with max acc
plot(acc.check, ylab="CV Accuracy", xlab = "Num Comp", main=paste(acc.check[which.max(acc.check[,2])], "Components = max Accuracy:", max(acc.check[,2])))
opt.prin.comp.knn.group <- acc.check[which.max(acc.check[,2])]
knn.acc.group <- max(acc.check[,2])
#print num prin comp and acc
opt.prin.comp.knn.group
knn.acc.group
#####################

##################### Subject
acc.check <- NULL
num.prin.comp <- NULL
for (i in (2:23)){
  knn.mod.sub.prin = knn.cv(prin.comp.mod$scores[,1:i], cl = inde.vars[,2])
  acc.check <- rbind(acc.check, mean(knn.mod.sub.prin==inde.vars[,2]))
  num.prin.comp <- rbind(num.prin.comp, i)
}

acc.check <- cbind(num.prin.comp, acc.check)

#plot results and print num prin comp with max acc
plot(acc.check, ylab="CV Accuracy", xlab = "Num Comp", main=paste(acc.check[which.max(acc.check[,2])], "Components = max Accuracy:", max(acc.check[,2])))
opt.prin.comp.knn.sub <- acc.check[which.max(acc.check[,2])]
knn.acc.sub <- max(acc.check[,2])
#print num prin comp and acc
opt.prin.comp.knn.sub
knn.acc.sub
#####################

##################### Subject
acc.check <- NULL
num.prin.comp <- NULL
for (i in (2:23)){
  knn.mod.con.prin = knn.cv(prin.comp.mod$scores[,1:i], cl = inde.vars[,3])
  acc.check <- rbind(acc.check, mean(knn.mod.con.prin==inde.vars[,3]))
  num.prin.comp <- rbind(num.prin.comp, i)
}

acc.check <- cbind(num.prin.comp, acc.check)

#plot results and print num prin comp with max acc
plot(acc.check, ylab="CV Accuracy", xlab = "Num Comp", main=paste(acc.check[which.max(acc.check[,2])], "Components = max Accuracy:", max(acc.check[,2])))
opt.prin.comp.knn.con <- acc.check[which.max(acc.check[,2])]
knn.acc.con <- max(acc.check[,2])
#print num prin comp and acc
opt.prin.comp.knn.con
knn.acc.con
#####################

##################### Joint
acc.check <- NULL
num.prin.comp <- NULL
for (i in (2:23)){
  knn.mod.joint.prin = knn.cv(prin.comp.mod$scores[,1:i], cl = classes.joint)
  acc.check <- rbind(acc.check, mean(knn.mod.joint.prin==classes.joint))
  num.prin.comp <- rbind(num.prin.comp, i)
}

acc.check <- cbind(num.prin.comp, acc.check)

#plot results and print num prin comp with max acc
plot(acc.check, ylab="CV Accuracy", xlab = "Num Comp", main=paste(acc.check[which.max(acc.check[,2])], "Components = max Accuracy:", max(acc.check[,2])))
opt.prin.comp.knn.joint <- acc.check[which.max(acc.check[,2])]
knn.acc.joint <- max(acc.check[,2])
#print num prin comp and acc
opt.prin.comp.knn.joint
knn.acc.joint
#####################

#store accuracies before moving on to MclustDA
train.acc$KNN <- c(knn.acc.group, knn.acc.sub, knn.acc.con , knn.acc.joint)
train.acc

###########################################################################################################
###################################### MclustDA ###########################################################

##################### Group #############################
acc.check <- NULL
num.prin.comp <- NULL
for (i in (2:23)){
  mclustda.mod.group.prin = MclustDA(prin.comp.mod$scores[,1:i], inde.vars[,1])
  mclustda.mod.group.cv <- cvMclustDA(mclustda.mod.group.prin, nfold=10, metric = "error")
  acc.check <- rbind(acc.check, 1 - mclustda.mod.group.cv$error)
  num.prin.comp <- rbind(num.prin.comp, i)
}

acc.check <- cbind(num.prin.comp, acc.check)

#plot results and print num prin comp with max acc
plot(acc.check, ylab="CV Accuracy", xlab = "Num Comp", main=paste(acc.check[which.max(acc.check[,2])], "Components = max Accuracy:", max(acc.check[,2])))
opt.prin.comp.mclustda.group <- acc.check[which.max(acc.check[,2])]
mclustda.acc.group <- max(acc.check[,2])
#print num prin comp and acc
opt.prin.comp.mclustda.group
mclustda.acc.group
#####################

##################### Subject ########################### ~30min run time
acc.check <- NULL
num.prin.comp <- NULL
for (i in (2:23)){
  mclustda.mod.sub.prin = MclustDA(prin.comp.mod$scores[,1:i], inde.vars[,2])
  mclustda.mod.sub.cv <- cvMclustDA(mclustda.mod.sub.prin, nfold=10, metric = "error")
  acc.check <- rbind(acc.check, 1 - mclustda.mod.sub.cv$error)
  num.prin.comp <- rbind(num.prin.comp, i)
}

acc.check <- cbind(num.prin.comp, acc.check)

#plot results and print num prin comp with max acc
plot(acc.check, ylab="CV Accuracy", xlab = "Num Comp", main=paste(acc.check[which.max(acc.check[,2])], "Components = max Accuracy:", max(acc.check[,2])))
opt.prin.comp.mclustda.sub <- acc.check[which.max(acc.check[,2])]
mclustda.acc.sub <- max(acc.check[,2])
#manually set opt prin comp to 16, plot indicates marginal benefit/complexity after that
mclustda.acc.sub <- acc.check[15,2]
opt.prin.comp.mclustda.sub <- acc.check[15]
#print num prin comp and acc
opt.prin.comp.mclustda.sub
mclustda.acc.sub
#####################

##################### Condition #########################
acc.check <- NULL
num.prin.comp <- NULL
for (i in (2:23)){
  print(i)
  mclustda.mod.con.prin = MclustDA(prin.comp.mod$scores[,1:i], inde.vars[,3])
  mclustda.mod.con.cv <- cvMclustDA(mclustda.mod.con.prin, nfold=10, metric = "error")
  acc.check <- rbind(acc.check, 1 - mclustda.mod.con.cv$error)
  num.prin.comp <- rbind(num.prin.comp, i)
}

acc.check <- cbind(num.prin.comp, acc.check)

#plot results and print num prin comp with max acc
plot(acc.check, ylab="CV Accuracy", xlab = "Num Comp", main=paste(acc.check[which.max(acc.check[,2])], "Components = max Accuracy:", max(acc.check[,2])))
opt.prin.comp.mclustda.con <- acc.check[which.max(acc.check[,2])]
mclustda.acc.con <- max(acc.check[,2])
#print num prin comp and acc
opt.prin.comp.mclustda.con
mclustda.acc.con
#####################

### attempting joint classes with MclustDA resulted in too many models with too few replicates
### also computationally prohibitive
##################### joint classes ####################
# mclustda.mod.joint.prin <- MclustDA(prin.comp.mod$scores[,1:2], classes.joint)
# mclustda.mod.joint.cv <- cvMclustDA(mclustda.mod.joint.prin, nfold=10, metric = "error")
# mclustda.mod.joint.prin$models
# 
# acc.check <- NULL
# num.prin.comp <- NULL
# for (i in (2:23)){
#   print(i)
#   mclustda.mod.joint.prin <- MclustDA(prin.comp.mod$scores[,1:i], classes.joint)
#   mclustda.mod.joint.cv <- cvMclustDA(mclustda.mod.joint.prin, nfold=10, metric = "error")
#   acc.check <- rbind(acc.check, 1 - mclustda.mod.joint.cv$error)
#   num.prin.comp <- rbind(num.prin.comp, i)
# }
# 
# acc.check <- cbind(num.prin.comp, acc.check)
# 
# #plot results and print num prin comp with max acc
# plot(acc.check, ylab="CV Accuracy", xlab = "Num Comp", main=paste(acc.check[which.max(acc.check[,2])], "Components = max Accuracy:", max(acc.check[,2])))
# opt.prin.comp.mclustda.joint <- acc.check[which.max(acc.check[,2])]
# mclustda.acc.joint <- max(acc.check[,2])
# #print num prin comp and acc
# opt.prin.comp.mclustda.joint
# mclustda.acc.joint
########################################################

#quick look at MclustDA with EDDA with Group -- not a good fit comparatively
mclustda.mod.group.edda <- MclustDA(prin.comp.mod$scores[,1:20], inde.vars[,1], modelType = "EDDA")
mclustda.mod.group.edda.cv <- cvMclustDA(mclustda.mod.group.edda, nfold=10, metric = "error")
mclustda.acc.group.edda <- 1 - mclustda.mod.group.edda.cv$error
mclustda.acc.group.edda


#quick look at MclustDA with EDDA with subject -- not great comparatively
mclustda.mod.sub.edda <- MclustDA(prin.comp.mod$scores[,1:20], inde.vars[,2], modelType = "EDDA")
mclustda.mod.sub.edda.cv <- cvMclustDA(mclustda.mod.sub.edda, nfold=10, metric = "error")
mclustda.acc.sub.edda <- 1 - mclustda.mod.sub.edda.cv$error
mclustda.acc.sub.edda


#quick look at MclustDA with EDDA with Condition -- not great comparatively
mclustda.mod.con.edda <- MclustDA(prin.comp.mod$scores[,1:20], inde.vars[,3], modelType = "EDDA")
mclustda.mod.con.edda.cv <- cvMclustDA(mclustda.mod.con.edda, nfold=10, metric = "error")
mclustda.acc.con.edda <- 1 - mclustda.mod.con.edda.cv$error
mclustda.acc.con.edda

#store accuracies before moving on to Random Forests
train.acc$MclustDA <- c(mclustda.acc.group, mclustda.acc.sub, mclustda.acc.con , NA)
train.acc

###########################################################################################################
###################################### Random Forest with prin comp #######################################

##################### Group ############################
rf.acc.check <- NULL
num.prin.comp <- NULL
#opt.n.var.rf <- NULL
for (i in (2:23)){
  print(i)
  rf.mod.group.prin = rfcv(trainx=prin.comp.mod$scores[,1:i], trainy = as.factor(inde.vars[,1]), cv.fold = 10)
  rf.acc.check <- rbind(rf.acc.check, 1 - min(rf.mod.group.prin$error.cv))
  num.prin.comp <- rbind(num.prin.comp, i)
  #opt.n.var.rf <- rf.mod.group.prin$n.var[which.min(rf.mod.group.prin$error.cv)]
  }
rf.acc.check <- cbind(num.prin.comp, rf.acc.check)
#rf.acc.check <- cbind(rf.acc.check, opt.n.var.rf)

#plot results and print num prin comp with max acc
plot(rf.acc.check, ylab="CV Accuracy", xlab = "Num Comp", main=paste(rf.acc.check[which.max(rf.acc.check[,2])], "Components = max Accuracy:", max(rf.acc.check[,2])))
opt.prin.comp.rf.group <- rf.acc.check[which.max(rf.acc.check[,2])]
rf.acc.group <- max(rf.acc.check[,2])
#rf.opt.nvar <- acc.check[which.max(rf.acc.check[,2]),2]
opt.prin.comp.rf.group
rf.acc.group
#rf.opt.nvar
#######################################################

##################### Subject ######################### ~ 12 min runtime
rf.acc.check <- NULL
num.prin.comp <- NULL
for (i in (2:23)){
  print(i)
  print(Sys.time())
  rf.mod.sub.prin = rfcv(trainx=prin.comp.mod$scores[,1:i], trainy = as.factor(inde.vars[,2]), cv.fold = 10)
  rf.acc.check <- rbind(rf.acc.check, 1 - min(rf.mod.sub.prin$error.cv))
  num.prin.comp <- rbind(num.prin.comp, i)
}
rf.acc.check <- cbind(num.prin.comp, rf.acc.check)

#plot results and print num prin comp with max acc
plot(rf.acc.check, ylab="CV Accuracy", xlab = "Num Comp", main=paste(rf.acc.check[which.max(rf.acc.check[,2])], "Components = max Accuracy:", max(rf.acc.check[,2])))
opt.prin.comp.rf.sub <- rf.acc.check[which.max(rf.acc.check[,2])]
rf.acc.sub <- max(rf.acc.check[,2])
#manually set num of components to 21 see chart
opt.prin.comp.rf.sub <- rf.acc.check[20]
rf.acc.sub <- rf.acc.check[20,2]
#print opt prin comp and accuracy
opt.prin.comp.rf.sub
rf.acc.sub

#######################################################

##################### Condition ####################### ~ 9 min runtime
rf.acc.check <- NULL
num.prin.comp <- NULL
for (i in (2:23)){
  print(i)
  print(Sys.time())
  rf.mod.con.prin = rfcv(trainx=prin.comp.mod$scores[,1:i], trainy = as.factor(inde.vars[,3]), cv.fold = 10)
  rf.acc.check <- rbind(rf.acc.check, 1 - min(rf.mod.con.prin$error.cv))
  num.prin.comp <- rbind(num.prin.comp, i)
}
rf.acc.check <- cbind(num.prin.comp, rf.acc.check)

#plot results and print num prin comp with max acc
plot(rf.acc.check, ylab="CV Accuracy", xlab = "Num Comp", main=paste(rf.acc.check[which.max(rf.acc.check[,2])], "Components = max Accuracy:", max(rf.acc.check[,2])))
opt.prin.comp.rf.con <- rf.acc.check[which.max(rf.acc.check[,2])]
rf.acc.con <- max(rf.acc.check[,2])
opt.prin.comp.rf.con
rf.acc.con
######################################################

##################### joint ########################## ~ 1.5 hour runtime
rf.acc.check <- NULL
num.prin.comp <- NULL
for (i in (2:23)){
  print(i)
  print(Sys.time())
  rf.mod.joint.prin = rfcv(trainx=prin.comp.mod$scores[,1:i], trainy = as.factor(classes.joint), cv.fold = 10)
  rf.acc.check <- rbind(rf.acc.check, 1 - min(rf.mod.joint.prin$error.cv))
  num.prin.comp <- rbind(num.prin.comp, i)
}
rf.acc.check <- cbind(num.prin.comp, rf.acc.check)

#plot results and print num prin comp with max acc
plot(rf.acc.check, ylab="CV Accuracy", xlab = "Num Comp", main=paste(rf.acc.check[which.max(rf.acc.check[,2])], "Components = max Accuracy:", max(rf.acc.check[,2])))
opt.prin.comp.rf.joint <- rf.acc.check[which.max(rf.acc.check[,2])]
rf.acc.joint <- max(rf.acc.check[,2])
opt.prin.comp.rf.joint
rf.acc.joint
#####################################################

#store accuracies before moving on to Random Forests w/out prin comp
train.acc$RFpc <- c(rf.acc.group, rf.acc.sub, rf.acc.con , rf.acc.joint)
train.acc

###########################################################################################################
###################################### Random Forest w/out prin comp ######################################

#Citation: Referenced package documentation to help with coding cv with random forests 
#https://www.rdocumentation.org/packages/randomForest/versions/4.6-14/topics/rfcv

############## Group #######################
#train model
rf.mod.group <- rfcv(trainx = trn.dat.means, trainy = as.factor(inde.vars[,1]), cv.fold = 10)

#inspect plot to choose best number of var
plot(1 - rf.mod.group$error.cv ~ rf.mod.group$n.var, xlab = "# Var per tree", ylab= "Accuracy", main="Accuracy vs # Var per tree")

#manually set best choice for var -- 6
rf.opt.nvar.group.noprin <- rf.mod.group$n.var[3]
rf.acc.group.noprin <- 1 - rf.mod.group$error.cv[3]
rf.opt.nvar.group.noprin
rf.acc.group.noprin
############################################

############## Subject #####################
#train model
rf.mod.sub <- rfcv(trainx = trn.dat.means, trainy = as.factor(inde.vars[,2]), cv.fold = 10)

#inspect plot to choose best number of var
plot(1 - rf.mod.sub$error.cv ~ rf.mod.sub$n.var, xlab = "# Var per tree", ylab= "Accuracy", main="Accuracy vs # Var per tree")

#manually set best choice for var -- 12
rf.opt.nvar.sub.noprin <- rf.mod.sub$n.var[2]
rf.acc.sub.noprin <- 1 - rf.mod.sub$error.cv[2]
rf.opt.nvar.sub.noprin
rf.acc.sub.noprin
############################################

############## Condition ###################
#train model
rf.mod.con <- rfcv(trainx = trn.dat.means, trainy = as.factor(inde.vars[,3]), cv.fold = 10)

#inspect plot to choose best number of var
plot(1 - rf.mod.con$error.cv ~ rf.mod.con$n.var, xlab = "# Var per tree", ylab= "Accuracy", main="Accuracy vs # Var per tree")

#manually set best choice for var -- 12
rf.opt.nvar.con.noprin <- rf.mod.con$n.var[2]
rf.acc.con.noprin <- 1 - rf.mod.con$error.cv[2]
rf.opt.nvar.con.noprin
rf.acc.con.noprin
###########################################

############## Joint Classes ##############
#train model
rf.mod.joint <- rfcv(trainx = trn.dat.means, trainy = as.factor(classes.joint), cv.fold = 10)

#inspect plot to choose best number of var
plot(1 - rf.mod.joint$error.cv ~ rf.mod.joint$n.var, xlab = "# Var per tree", ylab= "Accuracy", main="Accuracy vs # Var per tree")

#manually set best choice for var -- 12
rf.opt.nvar.joint.noprin <- rf.mod.joint$n.var[2]
rf.acc.joint.noprin <- 1 - rf.mod.joint$error.cv[2]
rf.opt.nvar.joint.noprin
rf.acc.joint.noprin
###########################################

#store accuracies
train.acc$RF <- c(rf.acc.group.noprin, rf.acc.sub.noprin, rf.acc.con.noprin , rf.acc.joint.noprin)
train.acc


###########################################################################################################
###################################### compare model results ##############################################

#Citation: Referenced for writing data frame to csv
#https://stackoverflow.com/questions/8345759/how-to-save-a-data-frame-in-r

#view results and save to file for quick reference
write.csv(train.acc, "cv.train.acc.csv")
train.acc

#how many prin comps for joint LDA
opt.prin.comp.lda.joint

############# Brief notes on Results ##############
### LDA is the best model for predicting all classes together with 21 components
### Random forest without PCA performs best for each marginal classification
### Next step will be to build up predictor from the three RF to see if it competes with joint LDA
###########################################################################################################
###################################### build up best predictors ############################################

############## Joint LDA with 21 principle components #########################

#double check opt prin comp
opt.prin.comp.lda.joint

#use cor matrix for pca
prin.comp.mod=princomp(trn.dat.means, cor = T)

#train model on joint classes
classes.joint = apply(inde.vars[,1:3], 1, paste, collapse = ":")
lda.mod.joint.prin <- lda(x=prin.comp.mod$scores[, 1:opt.prin.comp.lda.joint], grouping = classes.joint, CV=T)
final.joint.acc.lda <- mean(paste(lda.mod.joint.prin$class)==classes.joint)
final.joint.acc.lda


############## Build with 3 marginal RF and piece together predictions #########################

#join classes
classes.joint = apply(inde.vars[,1:3], 1, paste, collapse = ":")

#Group
rf.mod.group <- rfcv(trainx = trn.dat.means, trainy = as.factor(inde.vars[,1]), cv.fold = 10)
#check optimal n.var
rf.opt.nvar.group.noprin
rf.group.pred <- rf.mod.group$predicted$`6`

#Subject
rf.mod.sub <- rfcv(trainx = trn.dat.means, trainy = as.factor(inde.vars[,2]), cv.fold = 10)
#check optimal n.var
rf.opt.nvar.sub.noprin
rf.sub.pred <- rf.mod.sub$predicted$`12`

#Condition
rf.mod.con <- rfcv(trainx = trn.dat.means, trainy = as.factor(inde.vars[,3]), cv.fold = 10)
#check optimal n.var
rf.opt.nvar.con.noprin
rf.con.pred <- rf.mod.con$predicted$`12`

#collapse predictions
rf.marg.joint.pred <- cbind(paste(rf.group.pred), paste(rf.sub.pred), paste(rf.con.pred))
rf.marg.joint.pred <- apply(rf.marg.joint.pred, 1, paste, collapse = ":")

#double check format
head(rf.marg.joint.pred)

### worse results than the factorialized classes
mean(rf.marg.joint.pred == classes.joint)

#################################################################################################################################
###################### final thoughts for paper #################################################################################
# the best model for predicting all three classes is LDA joint model
#this is probably toward cap of potential for this model
#it appears as though the higher order prin comps are picking up some sort of interaction between
#condition and one of other classes to allow the overall model to perform substantially better than the marginal
#predictions for condition. 
#next steps -> fine tune RF and work to increase/maximize DF and look for interaction that is being indicated by LDA PCA behavior
#################################################################################################################################

