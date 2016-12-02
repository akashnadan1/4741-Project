#reading in the data
data = read.csv("all_shots_data.csv")
str(data)
dim(data)

set.seed(3)
data = na.omit(data)
dim(data)

#removing game_id, matchup, final_margin, shot_result, location (same as BinLocation), W, BinResult, closest_defender,
#closest_defender_game_id, player_name, and player_id
#predicted variable will be FGM (whether or not the field goal is made)
#need to get rid of PTS as well, because it is exact correlation with FGM
drops <- c("GAME_ID","MATCHUP", "LOCATION", "W", "FINAL_MARGIN", "BinResult", "SHOT_RESULT", "CLOSEST_DEFENDER", "CLOSEST_DEFENDER_PLAYER_ID", "player_name", "player_id", "PTS")
#removes the columns names in the vector "drops"
data = data[ , !(names(data) %in% drops)]

str(data)

#convert GAME_CLOCK to integer and FGM to factor (for classification)####
data$GAME_CLOCK = as.integer(data$GAME_CLOCK)
data$FGM = as.factor(data$FGM)
str(data)

#initial linear regression to identify the significant predictors of FGM (this was run before changing FGM to a factor)
fit = lm(FGM~., data = data)
summary(fit)

#significant variables to look at for the rest of the models
keep = c("FGM", "SHOT_CLOCK", "DRIBBLES", "TOUCH_TIME", "SHOT_DIST", "PTS_TYPE", "CLOSE_DEF_DIST")
data = data[ , (names(data) %in% keep)]

#split into training and test data
samplesize = floor((4/5)*nrow(data))
train_ind = sample(seq_len(nrow(data)), size=samplesize)
train_ind

training = data[train_ind, ]
test = data[-train_ind, ]
test_outcome = test[, 7]
test_outcome
test$FGM = NULL


#RANDOM FOREST
library(randomForest)
rf1 = randomForest(FGM~., data = training, ntree = 100)
plot(rf1)
summary(rf1)
varImpPlot(rf1, main="Random Forest") # good graph to show strength of variables
legend(main="Random Forest")
## cross validation unecessary for random forest
names(rf1)

#misclassification rate on test data
p1 = predict(rf1, test)
p1
table1 = table(test_outcome, p1)
table1
miss1 = (table1[2] + table1[3]) / (table1[1] + table1[2] + table1[3] + table1[4])
miss1 #0.399 missed
accuracy = 1 - miss1
accuracy #0.601


#LOGISTIC REGRESSION
library(boot)
glm1 = glm(FGM~., data = training, family = binomial("logit"))
summary(glm1)
plot(glm1)

#cross validation
cv.glm1 = cv.glm(training, glm1, K = 5)
names(cv.glm1)
cv.glm1$delta #error = 0.235

#predicted test data
p2 = predict(glm1, test)
p2
actualp2 = ifelse(p2 > 0.5, 1, 0)

#misclassification rate on test data
table2 = table(test_outcome, actualp2)
table2
miss2 = (table2[2] + table2[3]) / (table2[1] + table2[2] + table2[3] + table2[4])
miss2 #0.42 error
accuracy2 = 1 - miss2
accuracy2
#0.58


#SUPPORT VECTOR MACHINE
library("e1071")
svm1 = svm(FGM~., data = training)
summary(svm1)

#predicting test data
p3 = predict(svm1, test)
p3

#misclassification rate 
table3 = table(test_outcome, p3)
table3
miss3 = (table3[2] + table3[3]) / (table3[1] + table3[2] + table3[3] + table3[4])
miss3 #0.396 error
accuracy3 = 1- miss3
accuracy3
#0.604

#K-NEAREST NEIGHBORS
library(class)
#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

training_norm = as.data.frame(lapply(training[1:6], normalize))

test_norm = as.data.frame(lapply(test[1:6], normalize))

knn1 = knn(train = training_norm, test = test_norm, cl = training$FGM, k = 100)
knn1

#misclassification rate
table4 = table(test_outcome, knn1)
table4
miss4 = (table4[2] + table4[3]) / (table4[1] + table4[2] + table4[3] + table4[4])
miss4 #0.40798
accuracy4 = 1 - miss4
accuracy4
#0.5920


#creating new spreadsheet to export data file with only significant variables into JuliaBox.org
write.csv(data, file = "JuliaDataFile.csv")


##### use ggvis for plotting
library(ggvis)
#layer_points(ggvis(data, x = ~FGM, y = ~SHOT_DIST))
plot(data$FGM, data$SHOT_DIST, main="Shot Made vs. Shot Distance", xlab="Shot Made (1 = make, 0 = miss)", ylab = "Distance from the hoop (feet)")
plot(data$FGM, data$SHOT_CLOCK)

#analyzing just made shots
madeShots = data[which(data$FGM == 1), ]
names(madeShots)
plot(madeShots$FGM, madeShots$SHOT_DIST)
mean(madeShots$SHOT_DIST)
#11.68 feet