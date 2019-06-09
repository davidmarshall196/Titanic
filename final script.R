

library(readr)
titanic <- read_csv("train.csv", col_types = 
                    cols(Embarked = col_factor(levels = c("S", "C", "Q")), 
                         PassengerId = col_character(), 
                         Pclass = col_factor(levels = c("1", "2", "3")), 
                         Sex = col_factor(levels = c("male","female")),
                         Survived = col_factor(levels = c("0", "1"))))


# Change the survived variable
titanic$Survived <- ifelse(titanic$Survived == 1, "Survived", "Died")
titanic$Survived <- as.factor(titanic$Survived)

# Let's start by exploring the data
library(tidyverse)

p1 <- ggplot(titanic, aes(Survived, fill = Survived)) +
  geom_bar(stat = 'count') +
  labs(x = "How many people survived?") +
  geom_label(stat = 'count', aes(label = ..count..), size = 7) +
  theme_grey(base_size = 18)
p1

# Now let's explore sex

p2 <- ggplot(titanic, aes(x = Sex, fill = Survived)) +
  geom_bar(stat = 'count', position = 'dodge') + theme_grey() +
  labs(x = "Is Sex Important?") +
  geom_label(stat = 'count', aes(label = ..count..))
p2



# How about class?
p3 <- ggplot(titanic, aes(x = Pclass, fill = "green")) +
  geom_bar(stat='count', position='dodge', show.legend = FALSE, fill = "darkgreen") +
  labs(x = 'P Class') +
  theme(legend.position="none") + coord_flip()
p4 <- ggplot(titanic, aes(x = Pclass, fill = Survived)) +
  geom_bar(stat='count', position='dodge', show.legend = FALSE) + labs(x = 'All data') +
  theme(legend.position="none") + theme_grey()+ coord_flip()
p5 <- ggplot(titanic, aes(x = Pclass, fill = Survived)) +
  geom_bar(stat='count', position='stack', show.legend = FALSE) +
  labs(x = 'Is Cabin class important?', y= "Count") + facet_grid(.~Sex) +
  theme(legend.position="pclass") + theme_grey()+ coord_flip()
p6 <- ggplot(titanic, aes(x = Pclass, fill = Survived)) +
  geom_bar(stat='count', position='fill', show.legend = FALSE) +
  labs(x = 'Is Cabin class important?', y= "Percent") + facet_grid(.~Sex) +
  theme(legend.position="none") + theme_grey()+ coord_flip()
library(gridExtra)
grid.arrange(p3, p4, p5, p6, ncol=2)

# We can combine class and sex
titanic$PclassSex[titanic$Pclass== 1  & titanic$Sex=='male'] <- 'P1Male'
titanic$PclassSex[titanic$Pclass== 2  & titanic$Sex=='male'] <- 'P2Male'
titanic$PclassSex[titanic$Pclass== 3  & titanic$Sex=='male'] <- 'P3Male'
titanic$PclassSex[titanic$Pclass== 1  & titanic$Sex=='female'] <- 'P1Female'
titanic$PclassSex[titanic$Pclass== 2  & titanic$Sex=='female'] <- 'P2Female'
titanic$PclassSex[titanic$Pclass== 3  & titanic$Sex=='female'] <- 'P3Female'
titanic$PclassSex <- as.factor(titanic$PclassSex)

# We can also do feature engineering, first taking the title and surname
titanic$Surname <- sapply(titanic$Name, function(x) {strsplit(x, split='[,.]')[[1]][1]})

#correcting some surnames that also include a maiden name
titanic$Surname <- sapply(titanic$Surname, function(x) {strsplit(x, split='[-]')[[1]][1]})

# Now to get the title of each person
titanic$Title <- sapply(titanic$Name, function(x) {strsplit(x, split='[,.]')[[1]][2]})
titanic$Title <- sub(' ', '', titanic$Title) #removing spaces before title

# Now to reduce the number of titles
titanic$Title[titanic$Title %in% c("Mlle", "Ms")] <- "Miss"
titanic$Title[titanic$Title== "Mme"] <- "Mrs"
titanic$Title[!(titanic$Title %in% c('Master', 'Miss', 'Mr', 'Mrs'))] <- "Rare Title"
titanic$Title <- as.factor(titanic$Title)

# And plot the result
p7 <- ggplot(titanic, aes(x = Title, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Title') + theme_grey()
p7

# A next step is to create a family size for each person
titanic$Fsize <- titanic$SibSp + titanic$Parch + 1

# And plot
p8 <- ggplot(titanic, aes(x = Fsize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') + theme_grey()
p8

# Perhaps if we summarise the data
library(skimr)
skimmed <- skim(titanic)
skimmed

# For cabin, there is too much to impute but it still could be a useful variable
titanic$Cabin[is.na(titanic$Cabin)] <- "U"
titanic$Cabin <- substring(titanic$Cabin, 1, 1)
titanic$Cabin <- as.factor(titanic$Cabin)

# We caan generate a plot
p9 <- ggplot(titanic[(!is.na(titanic$Survived)& titanic$Cabin!='U'),], 
             aes(x=Cabin, fill=Survived)) +
  geom_bar(stat='count') + theme_grey() + facet_grid(.~Pclass) + 
  labs(title="Survivor split by class and Cabin")
p9
# There's too much, will ignore

# We have missing values in embarked, age and fare. Before imputing I will code for
# whether age is present
titanic$age_present <- ifelse(is.na(titanic$Age),"No", "Yes")
titanic$age_present <- as.factor(titanic$age_present)

# Remove the remaining unhelpful variables
titanic$Name <- NULL
titanic$Ticket <- NULL
titanic$Parch <- NULL
titanic$Surname <- NULL
titanic$Cabin <- NULL
titanic$PassengerId <- NULL

# ANd change embarked to a factor
titanic$Embarked <- as.factor(titanic$Embarked)
skim_to_wide(titanic)

# Now lets attempt to impute the age, fare and embarked
library(mice)
tempData <- mice(titanic,m=5,maxit=50,meth='pmm',seed=500)
titanic <- complete(tempData,1)
skim_to_wide(titanic)
rm(tempData)

# Check
sum(is.na(titanic))

# Add the formula
form <- as.formula(survived ~ Pclass + Age + Fare + Fsize + SibSp + Title +
                     Sex + PclassSex + Embarked + age_present)

library(caret)
library(DMwR)
ctrl <- trainControl(method= "repeatedcv",
                     number = 5,
                     repeats= 5,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)
ctrl_down <- trainControl(method= "repeatedcv",
                          number = 5,
                          repeats= 5,
                          summaryFunction = twoClassSummary,
                          classProbs = TRUE,
                          sampling = "down")
ctrl_smote <- trainControl(method= "repeatedcv",
                           number = 5,
                           repeats= 5,
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE,
                           sampling = "smote")

# Configure multicore
library(doParallel)
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

# Define a function for training the models
train_models <- function(method, ctrl, tuning) {
  model.train <- train(form,
                       data = titanic,
                       method = method,
                       trControl = ctrl,
                       tuneGrid = tuning,
                       maxit = 1000000,
                       preProcess = c('range'))
  return(model.train)
}

# 1. Extreme gradient boosting model
set.seed(2018)
tune.grid.xgb <- expand.grid(eta = c(0.05, 0.075, 0.1),
                             nrounds = c(50, 75, 100),
                             max_depth = 6:8,
                             min_child_weight = c(2.0, 2.25, 2.5),
                             colsample_bytree = c(0.3, 0.4, 0.5),
                             gamma = 0,
                             subsample = 1)
# Normal non-sampled
model_xgb <- train_models("xgbTree", ctrl, tune.grid.xgb)
saveRDS(model_xgb, "model_xgb.rds")
# Down Sampled
model_xgb_down <- train_models("xgbTree", ctrl_down, tune.grid.xgb)
saveRDS(model_xgb_down, "model_xgb_down.rds")
# Smote Sampled
model_xgb_smote <- train_models("xgbTree", ctrl_smote, tune.grid.xgb)
saveRDS(model_xgb_smote, "model_xgb_smote.rds")

# 2. Random Forest
set.seed(2018)
tune.grid.rf <- expand.grid(.mtry = sqrt(ncol(titanic)))
# Normal non-sampled
model_rf <- train_models("rf", ctrl, tune.grid.rf)
saveRDS(model_rf, "model_rf.rds")
# Down Sampled
model_rf_down <- train_models("rf", ctrl_down, tune.grid.rf)
saveRDS(model_rf_down, "model_rf_down.rds")
# Smote Sampled
model_rf_smote <- train_models("rf", ctrl_smote, tune.grid.rf)
saveRDS(model_rf_smote, "model_rf_smote.rds")

# 4. Model Averaged Neural Network
set.seed(2018)
library(nnet)
tune.grid.nn <- expand.grid(size=c(10), decay=c(0.1))
# Normal non-sampled
model_net <- train_models("nnet", ctrl, tune.grid.nn)
saveRDS(model_net, "model_net.rds")
# Down Sampled
model_net_down <- train_models("nnet", ctrl_down, tune.grid.nn)
saveRDS(model_net_down, "model_net_down.rds")
# Smote Sampled
model_net_smote <- train_models("nnet", ctrl_smote, tune.grid.nn)
saveRDS(model_net_smote, "model_net_smote.rds")

# Load
model_xgb <- read_rds("C:/Users/david/Documents/Work/Project 2/R/Machine Learning/Titanic/Titanic/model_xgb.rds")
model_xgb_down <- read_rds("C:/Users/david/Documents/Work/Project 2/R/Machine Learning/Titanic/Titanic/model_xgb.rds")
model_xgb_smote <- read_rds("C:/Users/david/Documents/Work/Project 2/R/Machine Learning/Titanic/Titanic/model_xgb_smote.rds")

model_rf <- read_rds("C:/Users/david/Documents/Work/Project 2/R/Machine Learning/Titanic/Titanic/model_rf.rds")
model_rf_down <- read_rds("C:/Users/david/Documents/Work/Project 2/R/Machine Learning/Titanic/Titanic/model_rf.rds")
model_rf_smote <- read_rds("C:/Users/david/Documents/Work/Project 2/R/Machine Learning/Titanic/Titanic/model_rf_smote.rds")

model_nnet <- read_rds("C:/Users/david/Documents/Work/Project 2/R/Machine Learning/Titanic/Titanic/model_nnet.rds")
model_nnet_down <- read_rds("C:/Users/david/Documents/Work/Project 2/R/Machine Learning/Titanic/Titanic/model_nnet.rds")
model_nnet_smote <- read_rds("C:/Users/david/Documents/Work/Project 2/R/Machine Learning/Titanic/Titanic/model_nnet_smote.rds")

model_glm <- read_rds("C:/Users/david/Documents/Work/Project 2/R/Machine Learning/Titanic/Titanic/model_glm.rds")
model_glm_down <- read_rds("C:/Users/david/Documents/Work/Project 2/R/Machine Learning/Titanic/Titanic/model_glm.rds")
model_glm_smote <- read_rds("C:/Users/david/Documents/Work/Project 2/R/Machine Learning/Titanic/Titanic/model_glm_smote.rds")




# 5. Logistic Regression with elastic net
set.seed(2018)
lamda.grid <- 10^seq(2, -2, length = 100)
alpha.grid <- seq(0,1, length = 10)
srchGrd <- expand.grid(.alpha = alpha.grid,
                       .lambda = lamda.grid)
# Normal non-sampled
model_glm <- train_models("glmnet", ctrl, srchGrd)
saveRDS(model_glm, "model_glm.rds")
# Down Sampled
model_glm_down <- train_models("glmnet", ctrl_down, srchGrd)
saveRDS(model_glm_down, "model_glm_down.rds")
# Smote Sampled
model_glm_smote <- train_models("glmnet", ctrl_smote, srchGrd)
saveRDS(model_glm_smote, "model_glm_smote.rds")

# Group the models
models_compare <- resamples(list(RF = model_rf,
                                 XGB = model_xgb,
                                 ENet = model_glm,
                                 NNet = model_net))
models_compare_down <- resamples(list(RF = model_rf_down,
                                      XGB = model_xgb_down,
                                      ENet = model_glm_down,
                                      NNet = model_net_down))
models_compare_smote <- resamples(list(RF = model_rf_smote,
                                       XGB = model_xgb_smote,
                                       ENet = model_glm_smote,
                                       NNet = model_net_smote))
# Create the scales for the plotting
scales <- list(x=list(relation="free"), y=list(relation="free"))

# Dot plot each one so we can compare the models smote, down and normal sampling
dotplot <- dotplot(models_compare, scales = scales)
dotplot_down <- dotplot(models_compare_down, scales=scales)
dotplot_smote <- dotplot(models_compare_smote, scales=scales)
dotplot
dotplot_down
dotplot_smote

# Next I will only plot the normal as the resampling doesn't seem to have an impact
# Box plot
boxplot <- bwplot(models_compare, scales=scales)
boxplot
# Density Plot
denplot <- densityplot(models_compare, scales=scales, pch = "|")
denplot
# Dotplot
dotplot <- dotplot(models_compare, scales=scales)
dotplot
# Parallel plot
paraplot <- parallelplot(models_compare)
paraplot
# Scatterplot
scatter <- splom(models_compare)
scatter

# Now we can plot the feature importance
# Define Function
plot_imp <- function(model, title) {FI_mod <- varImp(model)
FI_dat <- FI_mod$importance
FI_gini <- data.frame(Variables = row.names(FI_dat),
                      MeanDecreaseGini = FI_dat$Overall)
FI_plot <- ggplot(FI_gini, aes(x=reorder(Variables, MeanDecreaseGini),
                               y=MeanDecreaseGini, fill= "green")) +
  geom_bar(stat='identity') + coord_flip() +
  theme(legend.position="none") + labs(x="") +
  ggtitle(title) +
  theme(plot.title = element_text(hjust = 0.5))
return(FI_plot)
}

# Now to plot the feature importance of the best group of models
xgb_imp_plot <- plot_imp(model_xgb, "xGB Feature Importance")
glm_imp_plot <- plot_imp(model_glm, "GLM Feature Importance")
rf_imp_plot <- plot_imp(model_rf, "RF Feature Importance")
net_imp_plot <- plot_imp(model_net, "NNet Feature Importance")


# It seems 4 of the models are easily comparable:
library(gridExtra)
Normal_plots_imp <- grid.arrange(xgb_imp_plot, rf_imp_plot,
                                 net_imp_plot, glm_imp_plot, ncol=2)
Normal_plots_imp

# Perhaps if we ranked the features of each model from 1 to 19 and then
# found the top 10...
rank_features <- function(model) {
  FI_mod <- varImp(model)
  FI_df <- FI_mod$importance
  FI_df <- data.frame(Variables = row.names(FI_df),
                      overall = FI_df$Overall)
  FI_df <- arrange(FI_df, desc(overall)) %>%
    mutate(rank = 1:nrow(FI_df))
  FI_df$overall <- NULL 
  
  return(FI_df)
}

rank_xgb <- rank_features(model_xgb)
rank_xgb <- rename(rank_xgb, xgb = rank)

rank_glm <- rank_features(model_glm)
rank_glm <- rename(rank_glm, glm = rank)

rank_rf <- rank_features(model_rf)
rank_rf <- rename(rank_rf, rf = rank)

rank_net <- rank_features(model_net)
rank_net <- rename(rank_net, net = rank)

# Now, we can join these data sets
list_dfs <- list(rank_xgb, rank_glm, rank_net, rank_rf)
ranks <- Reduce(function(...) merge(..., all=T), list_dfs)

# And create an average for each variable
ranks <- mutate(ranks, average = (xgb + glm + rf + net)/4) %>%
  arrange(average) %>%
  mutate(rank.average = 1:nrow(ranks))

# We can plot the average rank
plot <- ggplot(ranks, aes(x = reorder(Variables, -average), y = average,
                          fill = Variables)) + geom_col() +
  coord_flip()
plot


models_compare

models_compare_df <- as.data.frame(models_compare)
models_compare_tibble <- as.tibble(models_compare)


# Predict
library(readr)
test <- read_csv("test.csv")
test$Sex <- as.factor(test$Sex)
test$PclassSex[test$Pclass== 1  & test$Sex=='male'] <- 'P1Male'
test$PclassSex[test$Pclass== 2  & test$Sex=='male'] <- 'P2Male'
test$PclassSex[test$Pclass== 3  & test$Sex=='male'] <- 'P3Male'
test$PclassSex[test$Pclass== 1  & test$Sex=='female'] <- 'P1Female'
test$PclassSex[test$Pclass== 2  & test$Sex=='female'] <- 'P2Female'
test$PclassSex[test$Pclass== 3  & test$Sex=='female'] <- 'P3Female'
test$PclassSex <- as.factor(test$PclassSex)
test$Surname <- sapply(test$Name, function(x) {strsplit(x, split='[,.]')[[1]][1]})
test$Surname <- sapply(test$Surname, function(x) {strsplit(x, split='[-]')[[1]][1]})
test$Title <- sapply(test$Name, function(x) {strsplit(x, split='[,.]')[[1]][2]})
test$Title <- sub(' ', '', test$Title) #removing spaces before title
test$Title[test$Title %in% c("Mlle", "Ms")] <- "Miss"
test$Title[test$Title== "Mme"] <- "Mrs"
test$Title[!(test$Title %in% c('Master', 'Miss', 'Mr', 'Mrs'))] <- "Rare Title"
test$Title <- as.factor(test$Title)
test$Fsize <- test$SibSp + test$Parch + 1
test$cabin[is.na(titanic$cabin)] <- "U"
test$cabin <- substring(test$Cabin, 1, 1)
test$cabin <- as.factor(test$Cabin)
test$age_present <- ifelse(is.na(test$Age),"No", "Yes")
test$age_present <- as.factor(test$age_present)
test$Name <- NULL
test$Ticket <- NULL
test$Parch <- NULL
test$Surname <- NULL
test$Cabin <- NULL
test <- test %>% rename(pclass = Pclass, sex = Sex, age = Age,
                        sibsp = SibSp, fare = Fare, embarked = Embarked)
test <- test %>% rename(Title = title)
tempData <- mice(test,m=5,maxit=50,meth='pmm',seed=500)
test <- complete(tempData,1)


# Make Predictions
prediction_frame <- data.frame(matrix(nrow=418, ncol=0))
prediction_frame <- as.data.frame(predict(model_xgb, test))
prediction_frame$xgb_down <- predict(model_xgb_down, test)
prediction_frame$xgb_smote <- predict(model_xgb_smote, test)

prediction_frame$rf <- predict(model_rf, test)
prediction_frame$rf_down <- predict(model_rf_down, test)
prediction_frame$rf_smote <- predict(model_rf_smote, test)

prediction_frame$net <- predict(model_net, test)
prediction_frame$net_down <- predict(model_net_down, test)
prediction_frame$net_smote <- predict(model_net_smote, test)

prediction_frame$glm <- predict(model_glm, test)
prediction_frame$glm_down <- predict(model_glm_down, test)
prediction_frame$glm_smote <- predict(model_glm_smote, test)

prediction_frame$final <- 
  apply(prediction_frame,1,function(x) names(which.max(table(x))))


final_prediction <- prediction_frame %>% select(final)
passenger_ids <- test %>% select(PassengerId)
final_prediction <- cbind(passenger_ids, final_prediction)

final_prediction$final <- ifelse(final_prediction$final == "Survived",1,0)

write.csv(final_prediction, "final_prediction.csv")



















