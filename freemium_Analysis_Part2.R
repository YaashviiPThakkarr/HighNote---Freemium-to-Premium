###############################################################################
###
### This is script provides an analysis for the highnote freemium exercise which asks you
### to create a predictive model of adoption for the highnote freemium dataset.
### This script does the following:
###  0) sets up the environment
###  1) imports the freemium dataset from a text file
###  2) creates another version of the data with all missing values recoded to their mean
###  3) computes descriptive statistics and plots
###  4) estimates a logistic regression model and decision tree
###     for each of these models it computes predictions, a confusion matrix, and lift
###     of those observations in the top decline.
###  5) compares the trees
###  6) exports the results to a CSV table for you to do profit analysis
###
###  notice the !! denote areas to change the code !!
###############################################################################



###############################################################################
### setup
###############################################################################

# setup environment, make sure this library has been installed
if (!require(tree)) {install.packages("tree"); library(tree)}
# setup environment (if you want to use fancy tree plots)
if (!require(rpart)) {install.packages("rpart"); library(rpart)}
if (!require(rattle)) {install.packages("rattle"); library(rattle)}
if (!require(rpart.plot)) {install.packages("rpart.plot"); library(rpart.plot)}
if (!require(RColorBrewer)) {install.packages("RColorBrewer"); library(RColorBrewer)}
if (!require(party)) {install.packages("party"); library(party)}
if (!require(partykit)) {install.packages("partykit"); library(partykit)}
# a better scatterplot matrix routine
if (!require(car)) {install.packages("car"); library(car)}
# better summary tables
if (!require(psych)) {install.packages("psych"); library(psych)}
# for visualizing regressions
if (!require(visreg)) {install.packages("visreg"); library(visreg)}
if (!require(ROCR)) {install.packages("ROCR"); library(ROCR)}

# import dataset from file (change the directory to where your data is stored)
setwd("~/Documents/class/marketing analytics/cases/freemium/data")  # !! change to your directory !!
freemium=read.csv("High Note data.csv")



###############################################################################
### prepare the dataset for analysis
###############################################################################

# compute the number of observations in the freemium dataset
nobs=nrow(freemium)

# set the random number seed so the samples will be the same if regenerated
set.seed(1248765792)

# prepare new values using a uniform random number, each record in freemium has 
# a corresponding uniform random value which will be used to decide if the observation
# is assigned to the training, validation or prediction sample
randvalue=runif(nobs)
trainsample=randvalue<.6
validsample=(randvalue>=.6 & randvalue<.9)
predsample=(randvalue>=.9)
plotsample=sample(1:nrow(freemium),300)

# copy the dataset to one that has recoded values for all missing values
# this adds the columns age_Missing, male_Missing, good_country_Missing, shouts_Missing
# if the corresponding column of age, male, good_country, or shouts is NA
# if the values is missing then the Missing variable is set to 1 and 0 otherwise
# and the value in the original column is replaced with the average value
rfreemium=freemium

# create columns to code if the variable is missing
rfreemium$age_Missing=as.numeric(is.na(freemium$age))
rfreemium$age[rfreemium$age_Missing==1]=mean(freemium$age,na.rm=T)
rfreemium$male_Missing=as.numeric(is.na(freemium$male))
rfreemium$male[rfreemium$male_Missing==1]=mean(freemium$male,na.rm=T)
rfreemium$good_country_Missing=as.numeric(is.na(freemium$good_country))
rfreemium$good_country[rfreemium$good_country_Missing==1]=mean(freemium$good_country,na.rm=T)
rfreemium$shouts_Missing=as.numeric(is.na(freemium$shouts))
rfreemium$shouts[rfreemium$shouts_Missing==1]=mean(freemium$shouts,na.rm=T)
rfreemium$avg_friend_age_Missing=as.numeric(is.na(freemium$avg_friend_age))
rfreemium$avg_friend_age[rfreemium$avg_friend_age_Missing==1]=mean(freemium$avg_friend_age,na.rm=T)
rfreemium$avg_friend_male_Missing=as.numeric(is.na(freemium$avg_friend_male))
rfreemium$avg_friend_male[rfreemium$avg_friend_male_Missing==1]=mean(freemium$avg_friend_male,na.rm=T)
# since there are not too many missing observations for friend_cnt, subscriber_friend_cnt,
# friend_country_cnt, and tenure these then the missing values for these are simply set to the mean
rfreemium$friend_cnt[is.na(rfreemium$friend_cnt)]=mean(freemium$friend_cnt,na.rm=T)
rfreemium$subscriber_friend_cnt[is.na(rfreemium$subscriber_friend_cnt)]=mean(freemium$subscriber_friend_cnt,na.rm=T)
rfreemium$friend_country_cnt[is.na(rfreemium$friend_country_cnt)]=mean(freemium$friend_country_cnt,na.rm=T)
rfreemium$tenure[is.na(rfreemium$tenure)]=mean(freemium$tenure,na.rm=T)

##
# create a dummy variable for subscriber friend count change
##
# Note that the delta1_subscriber_friend_cnt variable represents changes in the number of
# subscriber friends in both directions.  If a user's subscribe friend count increases, the
# delta1_subscriber_friend_cnt is positive.  If a user's subscriber friend count decreases, the
# delta1_subscriber_friend_cnt is negative.  E.g., delta1_subscriber_friend_cnt=1 implies that the 
# user obtained one more friend who is a subscriber.  It could be that this user made a new friend
# who is a subscriber, or that an existing friend switched from being a non-subscriber to a
# subscriber.
#
# Similarly, delta1_subscriber_friend_cnt=-1 implies that the user lost a friend who is a subscriber,
# or that one of the user's friend swtiched from being a subscriber to a non-subscriber. For
# delta1_subscriber_friend_cnt=1, we generate dum_delta1_subsfrcnt=1; for
# delta1_subscriber_friend_cnt=-1, we generate dum_delta1_subscfrcnt=0.
#
# The impact of increasing subscriber friend and decreasing subscriber friend is asymmmetric. An
# additional subscriber friend might have a positive influence on the user.  However, the attribution of 
# a subscriber firned usually does not have a negative impact on the user's adoption decision. We
# hence transform the delta1_subscriber_friend_cnt variable to be a dummary variable,
# dum_delta1_subscfrnt.  If the delta1_subscriber_friend_cnt is positive, dum_delta1_usbsfrcnt
# equals to 1. Ohterwise, dum_delta1_subsfrcnt equals to 0. And we use the dum_delta1_usbsfrcnt
# as our explanatory variable instead of delta1_subscriber_friend_cnt, because
# dum_delta1_subsfrcnt is a more accurate variable that captures the actual influence.
rfreemium$dum_delta1_subsfrcnt=(rfreemium$delta1_subscriber_friend_cnt>0)+0  # the +0 forces logical to numeric

# create a list with the variables that will be used in the analysis
varlist=c("age","male","friend_cnt","subscriber_friend_cnt","avg_friend_age","avg_friend_male","friend_country_cnt",
          "songsListened","playlists","posts","shouts","lovedTracks","tenure","good_country")
# also create a list for the recoded values
rvarlist=c("age","age_Missing","male","male_Missing","friend_cnt","subscriber_friend_cnt","avg_friend_age","avg_friend_age_Missing",
           "avg_friend_male","avg_friend_male_Missing","friend_country_cnt","songsListened","playlists","posts",
           "shouts","shouts_Missing","lovedTracks","tenure","good_country","good_country_Missing")
crvarlist=c("adopter",rvarlist)
# create a list with the changes from the historical period
# notice that age and male are dropped since these are constant and do not change
rvarlistdelta=c("delta1_friend_cnt","dum_delta1_subsfrcnt","delta1_songsListened",
                "delta1_lovedTracks","delta1_posts","delta1_playlists","delta1_shouts")
crvarlistdelta=c("adopter",rvarlistdelta)




###############################################################################
### understanding the data with descriptive statistics and graphics
###############################################################################

# number of observations
sum(trainsample)
sum(validsample)
sum(predsample)

# describe the overall data
describe(rfreemium[,crvarlist],fast=TRUE)
# describe the freemium data for adopters and non-adopters
describeBy(rfreemium[,crvarlist],group=rfreemium$adopter,fast=TRUE)

# compare with the changes from the historical period
describe(rfreemium[,crvarlistdelta],fast=TRUE)
# describe the freemium data for adopters and non-adopters
describeBy(rfreemium[,crvarlistdelta],group=rfreemium$adopter,fast=TRUE)




###############################################################################
### estimate a stepwise regression model with all the variables and their interactions
###############################################################################

# estimate logistic regression (with just trainsample)
lrmdl=glm(adopter~.,data=rfreemium[trainsample,crvarlist],family='binomial')
summary(lrmdl)

# run a step-wise regression
null = glm(adopter~1,data=rfreemium[trainsample,crvarlist],family='binomial')  # define starting model
full = glm(adopter~.,data=rfreemium[trainsample,crvarlist],family='binomial')  # define complete model
# if you have enough time try adopter~.^2
#fulli = glm(adopter~.^2,data=rfreemium[trainsample,crvarlist],family='binomial')  # includes all interactions
fwd = step(null, scope=formula(full),steps=15,dir="forward")    # !! if you want to use the model with interactions change full to fulli !!, !! should steps=15?? !!
lrmdl = fwd  # choose the model to analyze

# give a summary of the model's trained parameters
summary(lrmdl)

# visualize the effects of the model
# plot the log of the odds ratio as function of playlists  ?? look for the S-shaped curve ??
par(mfrow=c(2,1))
visreg(lrmdl,"playlists",ylab="Log(OddsRatio of Adopt)",xlim=c(0,100))
visreg(lrmdl,"playlists",scale="response",ylab="Pr(Adopt)",xlim=c(0,100))
# plot the log of the odds ratio as function of lovedtracks
par(mfrow=c(2,1))
visreg(lrmdl,"lovedTracks",ylab="Log(OddsRatio of Adopt)",xlim=c(0,100))
visreg(lrmdl,"lovedTracks",scale="response",ylab="Pr(Adopt)",xlim=c(0,100))
# plot the log of the odds ratio as function of subscriber_friend_cnt
par(mfrow=c(2,1))
visreg(lrmdl,"subscriber_friend_cnt",ylab="Log(OddsRatio of Adopt)",xlim=c(0,100))
visreg(lrmdl,"subscriber_friend_cnt",scale="response",ylab="Pr(Adopt)",xlim=c(0,100))
# create a contour plot to visualize two effects at the same time
par(mfrow=c(1,1))
visreg2d(lrmdl,"playlists","subscriber_friend_cnt",plot.type="image",main="Log(OddsRatio of Adopt)",xlim=c(0,100),ylim=c(0,100))
visreg2d(lrmdl,"playlists","subscriber_friend_cnt",scale="response",plot.type="image",main="Pr(Adopt)",xlim=c(0,100),ylim=c(0,100))

# predict probability (for validation sample)
padopter = predict(lrmdl,newdata=rfreemium[validsample,crvarlist],type='response')
cadopter = (padopter>.25)+0    # notice that we use a cutoff of 25% because it is harder to predict adopters
trueadopter = freemium$adopter[validsample]
(results = xtabs(~cadopter+trueadopter) )  # confusion matrix (columns have truth, rows have predictions)
(accuracy = (results[1,1]+results[2,2])/sum(results) )  # how many correct guesses along the diagonal
(truepos = results[2,2]/(results[1,2]+results[2,2]))  # how many correct "adopter" guesses
(precision = results[2,2]/(results[2,1]+results[2,2])) # proportion of correct positive guesses 
(trueneg = results[1,1]/(results[2,1]+results[1,1]))  # how many correct "non-adopter" guesses

# compute the predictions for the 10% of most likely adopterers (for validation sample)
topadopter = as.vector(padopter>=as.numeric(quantile(padopter,probs=.9)))
( baseconv=sum(trueadopter==1)/length(trueadopter) )  # what proportion would we have expected purely due to chance
( actconv=sum(trueadopter[topadopter])/sum(topadopter))  # what proportion did we actually predict
( lift=actconv/baseconv )  # what is the ratio of how many we got to what we expected

# compute the predictions for each decline of most likely adopterers (for validation sample)
vprob=seq(.9,.1,-.1)  # define 90th to 10th percentiles
vlift.lr=rep(0,length(vprob))  # save results to vector
for (i in 1:length(vprob)) {
  topadopter = as.vector(padopter>=as.numeric(quantile(padopter,probs=vprob[i])))  # compute indices of topadopters
  ( baseconv=sum(trueadopter==1)/length(trueadopter) )  # what proportion would we have expected purely due to chance
  ( actconv=sum(trueadopter[topadopter])/sum(topadopter))  # what proportion did we actually predict
  ( vlift.lr[i]=actconv/baseconv )  # what is the ratio of how many we got to what we expected
}
plot(vlift.lr,axes=F,xlab="Percentile",ylab="Lift")   # plot the lift
axis(2)  # overlay y axis
axis(1,at=1:length(vprob),labels=vprob)  # overlay x axis, but use vprob as labels

# compute ROC and AUC
rocpred.lr = prediction(padopter,freemium$adopter[validsample])  # compute predictions using "prediction"
rocperf.lr = performance(rocpred.lr, measure = "tpr", x.measure = "fpr")
plot(rocperf.lr, col=rainbow(10)); abline(a=0, b= 1)
auc.tmp = performance(rocpred.lr,"auc")  # compute area under curve
(auc = as.numeric(auc.tmp@y.values))

# predict probability (for prediction sample)
padopter = predict(lrmdl,newdata=rfreemium[predsample,crvarlist],type='response')
cadopter = as.vector((padopter>.25)+0)  # classify the predictions as adopters or not
trueadopter = freemium$adopter[predsample]
(results = xtabs(~cadopter+trueadopter))  # confusion matrix
(accuracy = (results[1,1]+results[2,2])/sum(results) )  # how many correct guesses along the diagonal
(truepos = results[2,2]/(results[1,2]+results[2,2]))  # how many correct "adopter" guesses
(precision = results[2,2]/(results[2,1]+results[2,2])) # proportion of correct positive guesses 
(trueneg = results[1,1]/(results[2,1]+results[1,1]))  # how many correct "non-adopter" guesses


rocpred.lr = prediction(padopter,freemium$adopter[predsample])  # compute predictions using "prediction"
rocperf.lr = performance(rocpred.lr, measure = "tpr", x.measure = "fpr")
plot(rocperf.lr, col=rainbow(10)); abline(a=0, b= 1)
auc.tmp = performance(rocpred.lr,"auc")  # compute area under curve
(auc = as.numeric(auc.tmp@y.values))


###############################################################################
### estimate a decision tree with all the variables and their interactions
###############################################################################

# train your tree using rpart
# cp controls the complexity of the tree, larger values like .05 give small trees, .0004 give large trees
# you can  either set cp to a small value and then prune the tree in the code below or set cp to the value you want here
ctree.full = rpart(adopter~., data=rfreemium[trainsample,crvarlist], control=rpart.control(cp=0.0027), model=TRUE)  # !! try different values of cp !!

# estimate a model with all the variables (optional -- this uses a different tree algorithm)
ctree = tree(adopter~., data=rfreemium[trainsample,crvarlist],mindev=.001)

# these lines are helpful to find the "best" value of cp, !! uncomment to determine best cp -- but this is already done for treeB below !!
# A good choice of cp for pruning is often the leftmost value for which the mean lies below the horizontal line.
printcp(ctree.full)               # display table of optimal prunings based on complexity parameter
plotcp(ctree.full)                # visualize cross-validation results

# prune the tree back !! choose on of the lines below for simple or complex, and leave the other commented out !!
ctree=prune(ctree.full,cp=0.0027)  # prune tree using chosen complexity parameter !! better tree (base tree) !!
ctree = ctree.full   # just use the full tree

# visualize the trees (+++ see #@moretreeplot for manual pruning and additional plots +++)
par(mfrow=c(1,1))         # reset one graphic per panel
#plot(ctree); text(ctree)  # simple graph
#prp(ctree)                # tree graph
prp(ctree,extra=101,nn=TRUE)  # add the size and proportion of data in the node
#fancyRpartPlot(ctree)     # fancy graphic  !! uncomment if you load library(rattle) !!

# predict probability (for validation sample)
padopter = predict(ctree,newdata=rfreemium[validsample,crvarlist],type='vector')
cadopter = (padopter>.25)+0    # notice that we use a cutoff of 25% because it is harder to predict adopters
trueadopter = freemium$adopter[validsample]
(results = xtabs(~cadopter+trueadopter) )  # confusion matrix (columns have truth, rows have predictions)
(accuracy = (results[1,1]+results[2,2])/sum(results) )  # how many correct guesses along the diagonal
(truepos = results[2,2]/(results[1,2]+results[2,2]))  # how many correct "adopter" guesses
(precision = results[2,2]/(results[2,1]+results[2,2])) # proportion of correct positive guesses 
(trueneg = results[1,1]/(results[2,1]+results[1,1]))  # how many correct "non-adopter" guesses

# compute the predictions for the 10% of most likely adopterers (for validation sample)
topadopter = as.vector(padopter>=as.numeric(quantile(padopter,probs=.9)))
( baseconv=sum(trueadopter==1)/length(trueadopter) )  # what proportion would we have expected purely due to chance
( actconv=sum(trueadopter[topadopter])/sum(topadopter))  # what proportion did we actually predict
( lift=actconv/baseconv )  # what is the ratio of how many we got to what we expected

# compute the predictions for each decline of most likely adopters (for validation sample)
vprob=seq(.9,.1,-.1)  # define 90th to 10th percentiles
vlift.tree=rep(0,length(vprob))  # save results to vector
for (i in 1:length(vprob)) {
  topadopter = as.vector(padopter>=as.numeric(quantile(padopter,probs=vprob[i])))  # compute indices of topadopters
  ( baseconv=sum(trueadopter==1)/length(trueadopter) )  # what proportion would we have expected purely due to chance
  ( actconv=sum(trueadopter[topadopter])/sum(topadopter))  # what proportion did we actually predict
  ( vlift.tree[i]=actconv/baseconv )  # what is the ratio of how many we got to what we expected
}
plot(vlift.tree,axes=F,xlab="Percentile",ylab="Lift")   # plot the lift
axis(2)  # overlay y axis
axis(1,at=1:length(vprob),labels=vprob)  # overlay x axis, but use vprob as labels

# compute ROC and AUC
rocpred.tree = prediction(padopter,freemium$adopter[validsample])  # compute predictions using "prediction"
rocperf.tree = performance(rocpred.tree, measure = "tpr", x.measure = "fpr")
plot(rocperf.tree, col=rainbow(10)); abline(a=0, b= 1)
auc.tmp = performance(rocpred.tree,"auc")  # compute area under curve
(auc = as.numeric(auc.tmp@y.values))

# predict probability (for prediction sample)
padopter = predict(ctree,newdata=rfreemium[predsample,crvarlist],type='vector')
cadopter = as.vector((padopter>.25)+0)  # classify the predictions as adopters or not
trueadopter = freemium$adopter[predsample]
(results = xtabs(~cadopter+trueadopter))  # confusion matrix
(accuracy = (results[1,1]+results[2,2])/sum(results) )  # how many correct guesses along the diagonal
(truepos = results[2,2]/(results[1,2]+results[2,2]))  # how many correct "adopter" guesses
(precision = results[2,2]/(results[2,1]+results[2,2])) # proportion of correct positive guesses 
(trueneg = results[1,1]/(results[2,1]+results[1,1]))  # how many correct "non-adopter" guesses
myvarlist=c("adopter","net_user")
write.csv(cbind(freemium[predsample,myvarlist],padopter),"test_data_freemium_results.csv")



###############################################################################
### compare models
###############################################################################

# plot all ROC curves together
plot(rocperf.lr,col="blue"); abline(a=0,b=1)
plot(rocperf.tree,add=TRUE,col="green")
legend("bottomright",c("LogRegr","Tree"),pch=15,col=c("blue","green"),bty="n")

# plot lift
plot(vlift.lr,axes=F,xlab="Percentile",ylab="Lift",col="blue")   # plot the lift
lines(vlift.tree,col="green")
axis(2)  # overlay y axis
axis(1,at=1:length(vprob),labels=vprob)  # overlay x axis, but use vprob as labels
points(vlift.lr,col="blue"); lines(vlift.lr,col="blue")
points(vlift.tree,col="green"); lines(vlift.tree,col="green")
legend("topright",c("LogRegr","Tree"),pch=15,col=c("blue","green"),bty="n")




###############################################################################
### the information you need to do a profit analysis would be the probability
### if we do nothing, you'll need to think about how this would change if you
### give the offer, and adoption, you can add other variables to this CSV
### file if you wish
###############################################################################

# compute final predictions to include (make sure your lrmdl and ctree models are set)
padopter.lr = predict(lrmdl,newdata=rfreemium[,crvarlist],type='response')
padopter.tree = predict(ctree,newdata=rfreemium[,crvarlist],type='vector')
myvarlist=c("adopter","net_user")
write.csv(cbind(freemium[,myvarlist],padopter.lr,padopter.tree),"freemium_results.csv")
