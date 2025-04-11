########
#Data Driven Modelling QoE Code
########

library(nnet)
library(CrossValidate)
library(philentropy)
library(marginaleffects)
library(margins)

theme_idris <- function() {
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "grey20"),
    panel.border =  element_rect(fill = NA,
                                 colour = "grey20"),
  )
}

get_mode <- function(x){
  return(as.numeric(names(sort(table(x), decreasing = T, na.last = T)[1])))
}

###########
#Video Quality Dataset
##########

opinion_df <- read.csv("Frdna_Opinion_df.csv")
unique_video_features_df <- read.csv("Frdna_MOS_df.csv")
opinion_df <- opinion_df[,-1] #drop index column
unique_video_features_df <- unique_video_features_df[,-1]

opinion_factors <- factor(opinion_df$Opinion_Score)
n_videos <- 424

opinion_df$SSIM <- (opinion_df$SSIM + 1) / 2
opinion_df$VMAF <- opinion_df$VMAF / max(opinion_df$VMAF)
opinion_df$fair_better <- opinion_df$fair_better*100

set.seed(1000)
split_data <- balancedSplit(opinion_factors, 0.5) #50:50 split

train_df <- opinion_df[split_data,]
test_df <- opinion_df[!(split_data),]

#extract the data from the test set we need to judge performance:

test_mos <- test_most_common_class <- test_size_most_common_class <- rep(NA, n_videos)
for(i in 1:n_videos){
  current_video_features <- test_df[which(test_df$Unique_ID == i),]
  test_mos[i] <- sum(current_video_features$Opinion_Score) / nrow(current_video_features)
  test_most_common_class[i] <- get_mode(current_video_features$Opinion_Score)
  test_size_most_common_class[i] <- sum(current_video_features$Opinion_Score == test_most_common_class[i])
}

#fit multinomial to training data

multinomial_model <- multinom(Opinion_Score ~ ., data = train_df[,2:9], maxit = 1000)
model_coefs <- coef(multinomial_model)[,2:8]
predicted_probs <- predict(multinomial_model, test_df[,2:8], type = "probs")

exp(model_coefs[,1:6])  #risk ratio

#marginal_effects <- avg_slopes(multinomial_model)
#marginal_effects_table <- matrix(marginal_effects$estimate, nrow = 5, ncol = 7)

#LRT
null_model <- multinom(Opinion_Score ~ 1,  train_df[,2:9]) #intercept only
2*(null_model$value - multinomial_model$value)

#obtain MOS from predicted probs per unique video
unique_video_indices <- unlist(lapply(split(seq_along(test_df$Unique_ID),
                                            test_df$Unique_ID), function(x) x[1]))
predicted_probs_unique_videos <- predicted_probs[unique_video_indices,]
predicted_mos <- apply(predicted_probs_unique_videos, 1, function(x) sum(x*(1:5)))

mean(predicted_mos - test_mos)  #mean error
mean(abs(predicted_mos - test_mos)) #mean absolute error
mean((predicted_mos - test_mos)^2) #MSE

#obtain most common class predictions

predicted_most_common_class <- apply(predicted_probs_unique_videos, 1, which.max)
sum((predicted_most_common_class - test_most_common_class == 0) / n_videos) #83% accurate

#what proportion of opinions are accurately predicted?
proportion_diffs <- matrix(NA, nrow = n_videos, ncol = 5)
correction_sum <- 0
for(i in 1:n_videos){
  current_video_features <- test_df[which(test_df$Unique_ID == i),]
  current_video_freq <- table(factor(current_video_features$Opinion_Score, level = 1:5))
  predicted_video_freq <- round(predicted_probs_unique_videos[i,] * sum(current_video_freq))
  
  proportion_diffs[i,] <- abs(predicted_video_freq - current_video_freq)/2
  #divide by two to remove the fact we double count
  
  #remove rounding errors
  if(sum(current_video_freq) != sum(predicted_video_freq)){
    correction_sum <- correction_sum + 1
  }
  
}
(sum(floor(proportion_diffs)) - correction_sum) / nrow(test_df) #87%

##
#Assess difference between the predicted and actual prob distributions?
#Use average KL divergence and overall KL divergence

KL_by_video <- rep(NA, n_videos)
for(i in 1:n_videos){
  current_video_features <- test_df[which(test_df$Unique_ID == i),]
  current_video_freq <- table(factor(current_video_features$Opinion_Score, level = 1:5))
  current_video_dist <- current_video_freq / sum(current_video_freq)
  dist_mat <- rbind(current_video_dist, predicted_probs_unique_videos[i,])
  KL_by_video[i] <- KL(dist_mat)
}
mean(KL_by_video)

overall_dist <- table(test_df$Opinion_Score) / nrow(test_df)
predicted_dist <- apply(predicted_probs, 2, mean)
dist_mat <- rbind(overall_dist, predicted_dist)
KL_overall <- KL(dist_mat) #overall this is very accurate

#also try a KS test:

#ks.test(test_df$Opinion_Score, predicted_opinions)  #no evidence to suggest from different distributions
set.seed(1)
ks_test_p_vals <- rep(NA, n_videos)
for(i in 1:n_videos){
  current_video_features <- test_df[which(test_df$Unique_ID == i),]
  predicted_video_sample <- sample(1:5, nrow(current_video_features), replace = TRUE,
                                   prob = predicted_probs_unique_videos[i,])
  ks_test_p_vals[i] <- ks.test(current_video_features$Opinion_Score,
                               predicted_video_sample)$p.value
}
sum(ks_test_p_vals < 0.01) / n_videos #evidence to suggest 15% not from same dist. Not bad
#NB: I have used Bonferroni Correction here

overall_sample <- sample(1:5, nrow(test_df), replace = TRUE,
                         prob = predicted_dist)
ks.test(test_df$Opinion_Score, overall_sample) #no evidence to suggest different dist


##########
#Plotting
###########
library(ggplot2)
library(ggpubr)
#Plot difference in MOS and pMOS, and add mean
ggplot() + geom_point(aes(x = 1:n_videos, y = (predicted_mos - test_mos))) +
  geom_hline(aes(yintercept = mean(predicted_mos - test_mos)), col = "red") +
  labs(x = "Video ID", y = "Difference Between MOS and pMOS") + theme_idris() +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )

#Barplot of Difference In Most Common Class Predictions
ggplot() + geom_bar(aes(x = factor(predicted_most_common_class - test_most_common_class)),
                    col = "blue", fill = "white") + theme_idris() +
  labs(x = "Difference In Predicted and Actual Most Common Opinion Score By Video",
       y = "Count") +   theme(
         axis.title = element_text(size = 14),   # axis titles
         axis.text  = element_text(size = 14)    # axis tick labels
       )

#Side By Side Barplot of pmfs

g1 <- ggplot() + geom_bar(aes(x = factor(overall_sample)), 
                          col = "blue", fill = "white") + theme_idris() +
  labs(x = "Predicted Opinion", y = "Count") +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )

g2 <- ggplot() + geom_bar(aes(x = factor(test_df$Opinion_Score)), 
                          col = "red", fill = "white") + theme_idris() +
  labs(x = "True Opinion", y = "Count") +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )

ggarrange(g1, g2, ncol = 2)

#KL Divergences by video

ggplot() + geom_point(aes(x = 1:n_videos, y = KL_by_video)) +
  geom_hline(aes(yintercept  = 0.67), col = "red", lty = 2) +
  labs(x = "Video ID", y = "KL Divergence") + theme_idris() +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )

#Plot of a probs for video condition 1, with MOS estimate added

ggplot() + geom_point(aes(x = factor(1:5), y= predicted_probs_unique_videos[1,]), 
                      col = "blue", fill = "white") + theme_idris() +
  geom_vline(aes(xintercept = 3.07), col ="red", lty = 2) +
  labs(x = "Predicted Opinion", y = "Probability") +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )

ggplot() + geom_line(aes(x = factor(1:5), y= c(0.34, 0.16, 0.08, 0.09, 0.33)), group = 1,
                     col = "blue") + theme_idris() +
  geom_vline(aes(xintercept = 2.91), col ="red", lty = 2) +
  labs(x = "Predicted Opinion", y = "Probability") +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )

#####
#CODEC Dists
#####
h264_dist <- table(opinion_df$Opinion_Score[opinion_df$Codec == 0]) / nrow(opinion_df)
h265_dist <- table(opinion_df$Opinion_Score[opinion_df$Codec == 1]) / nrow(opinion_df)

ggplot() + geom_line(aes(x=1:5, y = h264_dist), col = "blue") +
  geom_line(aes(x = 1:5, y = h265_dist), col = "red") + 
  labs(x = "Opinion Score", y = "Probability") + theme_idris() +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )

#######
#Res Dists
#########

HD_dist <- table(opinion_df$Opinion_Score[opinion_df$Resolution == 0]) / nrow(opinion_df)
FHD_dist <- table(opinion_df$Opinion_Score[opinion_df$Resolution == 1]) / nrow(opinion_df)
UHD_dist <- table(opinion_df$Opinion_Score[opinion_df$Resolution == 2]) / nrow(opinion_df)
ggplot() + geom_line(aes(x=1:5, y = HD_dist), col = "blue") +
  geom_line(aes(x = 1:5, y = FHD_dist), col = "red") + 
  geom_line(aes(x = 1:5, y = UHD_dist), col = "purple") +
  labs(x = "Opinion Score", y = "Probability") + theme_idris() +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )

#################################


####
#This models the Opinion Distributions for the PIP Video
#####
library(nnet)
library(CrossValidate)
library(philentropy)
library(marginaleffects)

opinion_df <- read.csv("PIP_Opinion_Scores_df.csv")
unique_video_features_df <- read.csv("PIP_MOS_df.csv")
opinion_df <- opinion_df[,-1] #drop index column
unique_video_features_df <- unique_video_features_df[,-1]

opinion_factors <- factor(opinion_df$Opinion)
n_videos <- 27

set.seed(1000)
split_data <- balancedSplit(opinion_factors, 0.5) #50:50 split

train_df <- opinion_df[split_data,]
test_df <- opinion_df[!(split_data),]

#extract the data from the test set we need to judge performance:

test_mos <- test_most_common_class <- test_size_most_common_class <- rep(NA, n_videos)
for(i in 1:n_videos){
  current_video_features <- test_df[which(test_df$Unique_ID == i),]
  test_mos[i] <- sum(current_video_features$Opinion) / nrow(current_video_features)
  test_most_common_class[i] <- get_mode(current_video_features$Opinion)
  test_size_most_common_class[i] <- sum(current_video_features$Opinion == test_most_common_class[i])
}

#fit model
multinomial_model <- multinom(Opinion ~ ., data = train_df[,2:7], maxit = 1000)

predicted_probs <- predict(multinomial_model, test_df[,2:6], type = "probs")

exp(model_coefs)
#LRT
null_model <- multinom(Opinion ~ 1,  train_df[,2:7]) #intercept only
2*(null_model$value - multinomial_model$value)

#obtain MOS from predicted probs per unique video
unique_video_indices <- unlist(lapply(split(seq_along(test_df$Unique_ID),
                                            test_df$Unique_ID), function(x) x[1]))
predicted_probs_unique_videos <- predicted_probs[unique_video_indices,]
predicted_mos <- apply(predicted_probs_unique_videos, 1, function(x) sum(x*(1:5)))

mean(predicted_mos - test_mos)  #mean error 0.02
mean(abs(predicted_mos - test_mos)) #mean absolute error
mean((predicted_mos - test_mos)^2) #MSE 0.05

#obtain most common class predictions

predicted_most_common_class <- apply(predicted_probs_unique_videos, 1, which.max)
sum((predicted_most_common_class - test_most_common_class == 0) / n_videos) #81% accurate

#what proportion of opinions are accurately predicted?
proportion_diffs <- matrix(NA, nrow = n_videos, ncol = 5)
correction_sum <- 0
for(i in 1:n_videos){
  current_video_features <- test_df[which(test_df$Unique_ID == i),]
  current_video_freq <- table(factor(current_video_features$Opinion, level = 1:5))
  predicted_video_freq <- round(predicted_probs_unique_videos[i,] * sum(current_video_freq))
  
  proportion_diffs[i,] <- abs(predicted_video_freq - current_video_freq)/2
  #divide by two to remove the fact we double count
  
  #remove rounding errors
  if(sum(current_video_freq) != sum(predicted_video_freq)){
    correction_sum <- correction_sum + 1
  }
  
}
(sum(floor(proportion_diffs)) - correction_sum) / nrow(test_df) #94%


##
#Assess difference between the predicted and actual prob distributions?
#Use average KL divergence and overall KL divergence

KL_by_video <- rep(NA, n_videos)
for(i in 1:n_videos){
  current_video_features <- test_df[which(test_df$Unique_ID == i),]
  current_video_freq <- table(factor(current_video_features$Opinion, level = 1:5))
  current_video_dist <- current_video_freq / sum(current_video_freq)
  dist_mat <- rbind(current_video_dist, predicted_probs_unique_videos[i,])
  KL_by_video[i] <- KL(dist_mat)
}
mean(KL_by_video)

overall_dist <- table(test_df$Opinion) / nrow(test_df)
predicted_dist <- apply(predicted_probs, 2, mean)
dist_mat <- rbind(overall_dist, predicted_dist)
KL_overall <- KL(dist_mat) #overall this is very accurate

#also try a KS test:

#ks.test(test_df$Opinion_Score, predicted_opinions)  #no evidence to suggest from different distributions
set.seed(1)
ks_test_p_vals <- rep(NA, n_videos)
for(i in 1:n_videos){
  current_video_features <- test_df[which(test_df$Unique_ID == i),]
  predicted_video_sample <- sample(1:5, nrow(current_video_features), replace = TRUE,
                                   prob = predicted_probs_unique_videos[i,])
  ks_test_p_vals[i] <- ks.test(current_video_features$Opinion,
                               predicted_video_sample)$p.value
}
sum(ks_test_p_vals < 0.01) / n_videos #evidence to suggest 0% not from same dist. Excellent

overall_sample <- sample(1:5, nrow(test_df), replace = TRUE,
                         prob = predicted_dist)
ks.test(test_df$Opinion, overall_sample) #no evidence to suggest different dist

############
library(ggplot2)
library(ggpubr)
#Plot difference in MOS and pMOS, and add mean
ggplot() + geom_point(aes(x = 1:n_videos, y = (predicted_mos - test_mos))) +
  geom_hline(aes(yintercept = mean(predicted_mos - test_mos)), col = "red") +
  labs(x = "Video ID", y = "Difference Between MOS and pMOS") + theme_idris() +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )

#Barplot of Difference In Most Common Class Predictions
ggplot() + geom_bar(aes(x = factor(predicted_most_common_class - test_most_common_class)),
                    col = "blue", fill = "white") + theme_idris() +
  labs(x = "Difference In Predicted and Actual Most Common Opinion Score By Video",
       y = "Count") +   theme(
         axis.title = element_text(size = 14),   # axis titles
         axis.text  = element_text(size = 14)    # axis tick labels
       )

#Side By Side Barplot of pmfs

g1 <- ggplot() + geom_bar(aes(x = factor(overall_sample)), 
                          col = "blue", fill = "white") + theme_idris() +
  labs(x = "Predicted Opinion", y = "Count") +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )

g2 <- ggplot() + geom_bar(aes(x = factor(test_df$Opinion)), 
                          col = "red", fill =  "white") + theme_idris() +
  labs(x = "True Opinion", y = "Count")+   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )

ggarrange(g1, g2, ncol = 2)


#Plot of a probs for video condition 1, with MOS estimate added

ggplot() + geom_point(aes(x = factor(1:5), y= predicted_probs_unique_videos[1,]), 
                      col = "blue", fill = "white") + theme_idris() +
  geom_vline(aes(xintercept = 3.07), col ="red", lty = 2) +
  labs(x = "Predicted Opinion", y = "Probability") +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )

ggplot() + geom_point(aes(x = factor(1:5), y= c(0.34, 0.16, 0.08, 0.09, 0.33)), 
                      col = "blue", fill = "white") + theme_idris() +
  geom_vline(aes(xintercept = 2.91), col ="red", lty = 2) +
  labs(x = "Predicted Opinion", y = "Probability") +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )

#Plot of dists for different game quality

game_low_dist <- table(factor(opinion_df$Opinion[opinion_df$Game.Quality == 0], levels = 1:5)) / length(opinion_df$Unique_ID)
game_med_dist <- table(factor(opinion_df$Opinion[opinion_df$Game.Quality == 1], levels = 1:5)) / length(opinion_df$Unique_ID)
game_high_dist <- table(factor(opinion_df$Opinion[opinion_df$Game.Quality == 2], levels = 1:5)) / length(opinion_df$Unique_ID)

ggplot() + geom_line(aes(x = 1:5, y = game_low_dist), col = "red") +
  geom_line(aes(x = 1:5, y = game_med_dist), col = "purple") + 
  geom_line(aes(x = 1:5, y = game_high_dist), col = "blue") +
  theme_idris() + labs(x = "Opinion Score", y = "Probability") +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )

#######################################

##################
#Video Quality MOS Comparison
###################

library(nnet)
library(CrossValidate)
library(neuralnet)
library(funGp)
library(randomForest)
library(e1071)
library(glmnet)


opinion_df <- read.csv("Frdna_Opinion_df.csv")
unique_video_features_df <- read.csv("Frdna_MOS_df.csv")
opinion_df <- opinion_df[,-1] #drop index column
unique_video_features_df <- unique_video_features_df[,-1]

opinion_factors <- factor(opinion_df$Opinion_Score)

set.seed(100)
split_data <- balancedSplit(opinion_factors, 0.5) #50:50 split

train_df <- opinion_df[split_data,]
test_df <- opinion_df[!(split_data),]

train_unique_video_indices <-  unlist(lapply(split(seq_along(train_df$Unique_ID),
                                                   train_df$Unique_ID), function(x) x[1]))
test_unique_video_indices <- unlist(lapply(split(seq_along(test_df$Unique_ID),
                                                 test_df$Unique_ID), function(x) x[1]))

#non multinomial LR models only need one set of features for video as they will be
#regressing on the MOS only
train_df_per_video <- train_df[train_unique_video_indices,-9]
test_df_per_video <- test_df[test_unique_video_indices,-9]

#extract mos for train_data
train_mos <- rep(NA, 424)
for(i in 1:424){
  current_video_features <- test_df[which(train_df$Unique_ID == i), ]
  train_mos[i] <- sum(current_video_features$Opinion_Score) / nrow(current_video_features)
}

#extract mos for test data
test_mos <- rep(NA, 424)
for(i in 1:424){
  current_video_features <- test_df[which(test_df$Unique_ID == i), ]
  test_mos[i] <- sum(current_video_features$Opinion_Score) / nrow(current_video_features)
}

#Our train and test sets per video need these MOS:
train_df_per_video$MOS <- train_mos
test_df_per_video$MOS <- test_mos

#######
#Fit Multinomial LR
#######

multinomial_model <- multinom(Opinion_Score ~ ., data = train_df[,2:9], maxit = 1000)
multinom_predicted_probs <- predict(multinomial_model, test_df[,2:8], type = "probs")

multinom_predicted_probs_unique_videos <- multinom_predicted_probs[test_unique_video_indices,]
multinom_predicted_mos <- apply(multinom_predicted_probs_unique_videos, 1, function(x) sum(x*(1:5)))

mean(multinom_predicted_mos - test_mos)  #mean error
mean(abs(multinom_predicted_mos - test_mos)) #mean absolute error
mean((multinom_predicted_mos - test_mos)^2) #MSE

multinomial_mse <- mean((multinom_predicted_mos - test_mos)^2) #0.046 
cor(multinom_predicted_mos, test_mos)  #very correlated, 0.966

##############
#Linear Regression
##############

linear_model <- lm(MOS ~ ., data = train_df_per_video[,2:9])
linear_predicted_mos <- predict(linear_model, test_df_per_video[,2:9]) 
linear_mse <- mean((linear_predicted_mos - test_mos)^2) #0.245
cor(linear_predicted_mos, test_mos) #0.847

##########
#Elastic Net
#########

elastic_model <- glmnet(train_df_per_video[,2:8], train_df_per_video[,9], alpha = 0.5)
elastic_predicted_mos <- predict(elastic_model, as.matrix(test_df_per_video[,2:8]), s= 0.01 )
elastic_mse <- mean((elastic_predicted_mos - test_mos)^2) #0.308
cor(elastic_predicted_mos, test_mos) #0.851

#########
#GLM - use Gamma as suggested as the distribution for MOS in QoE beyond the MOS: an in-depth look at QoE via better metrics and their relation to MOS
##########

gamma_glm <- glm(MOS ~ . , data = train_df_per_video[,c(3:6, 9)], family= "Gamma")
glm_predicted_mos <- predict(gamma_glm, test_df_per_video[,2:9])
glm_mse <- mean((glm_predicted_mos - test_mos)^2) #3.25
cor(glm_predicted_mos, test_mos)  #-0.22, so really not a good fit

###########
#Polynomial Regression
##########

np_model <- loess(MOS ~ ., data = train_df_per_video[,7:9], degree= 2)
np_predicted_mos <- predict(np_model, test_df_per_video[,5:9])
np_mse <- mean((np_predicted_mos - test_mos)^2, na.rm=TRUE) #0.17
cor(np_predicted_mos, test_mos, use="complete.obs") #0.9

##########
#GP Regression
##########

gp_model <- fgpm(sIn = train_df_per_video[,2:8], sOut = train_df_per_video[,9])
gp_predicted_mos <- predict(gp_model, sIn.pr = test_df_per_video[,2:8])
gp_mse <- mean((gp_predicted_mos$mean - test_mos)^2) #0.59
cor(gp_predicted_mos$mean, test_mos) #0.56

#########
#Random Forest
#########

rf_model <- randomForest(MOS ~ ., data = train_df_per_video[,2:9])
rf_predicted_mos <- predict(rf_model, test_df_per_video[,2:8])
rf_mse <- mean((rf_predicted_mos - test_mos)^2) #0.38
cor(rf_predicted_mos, test_mos) #0.68

###########
#SVM
##################

svm_model <- svm(MOS ~ ., data = train_df_per_video[,2:9], degree = 3)
svm_predicted_mos <- predict(svm_model, test_df_per_video[,2:8])
svm_mse <- mean((svm_predicted_mos - test_mos)^2) #0.30
cor(svm_predicted_mos, test_mos) #0.76

##############
#MLP Neural Net
###############
nn_train_data <- scale(train_df_per_video)
nn_test_data <- scale(test_df_per_video)
nn <- neuralnet(MOS ~ ., data = train_df_per_video[,2:9],
                hidden = c(2), stepmax = 1e6, 
                linear.output = TRUE)
nn_predicted_mos <- predict(nn, test_df_per_video[,2:8])
nn_mse <- mean((nn_predicted_mos - test_mos)^2) #0.30 
cor(nn_predicted_mos, test_mos) #0.81

#######
#RNN Neural Net
########
library(rnn)
Xvals <- c()
for(i in 2:8){
  Xvals <- cbind(Xvals, int2bin(train_df_per_video[,i])) 
}
X <- array(Xvals, dim=c(dim(int2bin(train_df_per_video[,2])), 2))
Y <- int2bin(train_df_per_video[,9])
rnn_model <- trainr(Y=Y, 
                    X=X, 
                    learningrate   =  0.001,
                    hidden_dim     = 10,
                    batch_size = 100,
                    numepochs = 3)
predict_Xvals <- c()
for(i in 2:8){
  predict_Xvals <- cbind(predict_Xvals, test_df_per_video[,i])
}
predict_X <- array(predict_Xvals, dim = c(dim(int2bin(test_df_per_video[,2])), 2))
rnn_predicted_mos_binary <- predictr(rnn_model, predict_X)
#rnn_predicted_mos <- apply(rnn_predicted_mos_binary, 1, function(x) bin2int(as.matrix(x)))
rnn_predicted_mos <- apply(rnn_predicted_mos_binary, 1, sum)
rnn_mse <- mean((rnn_predicted_mos - test_mos)^2) #1.15
cor(rnn_predicted_mos, test_mos) #-0.12

#####################################################
##

############
#PIP MOS Comparison
############


opinion_df <- read.csv("PIP_Opinion_Scores_df.csv")
unique_video_features_df <- read.csv("PIP_MOS_df.csv")
opinion_df <- opinion_df[,-1] #drop index column
unique_video_features_df <- unique_video_features_df[,-1]

opinion_factors <- factor(opinion_df$Opinion)
n_videos <- 27

set.seed(100)
split_data <- balancedSplit(opinion_factors, 0.5) #50:50 split

train_df <- opinion_df[split_data,]
test_df <- opinion_df[!(split_data),]

train_unique_video_indices <-  unlist(lapply(split(seq_along(train_df$Unique_ID),
                                                   train_df$Unique_ID), function(x) x[1]))
test_unique_video_indices <- unlist(lapply(split(seq_along(test_df$Unique_ID),
                                                 test_df$Unique_ID), function(x) x[1]))

#non multinomial LR models only need one set of features for video as they will be
#regressing on the MOS only
train_df_per_video <- train_df[train_unique_video_indices,-9]
test_df_per_video <- test_df[test_unique_video_indices,-9]

#extract mos for train_data
train_mos <- rep(NA, 27)
for(i in 1:27){
  current_video_features <- test_df[which(train_df$Unique_ID == i), ]
  train_mos[i] <- sum(current_video_features$Opinion) / nrow(current_video_features)
}

#extract mos for test data
test_mos <- rep(NA, 27)
for(i in 1:27){
  current_video_features <- test_df[which(test_df$Unique_ID == i), ]
  test_mos[i] <- sum(current_video_features$Opinion) / nrow(current_video_features)
}

#Our train and test sets per video need these MOS:
train_df_per_video$MOS <- train_mos
test_df_per_video$MOS <- test_mos

train_df_per_video <- train_df_per_video[,-7]
test_df_per_video <- test_df_per_video[,-7]


#######
#Fit Multinomial LR
#######

multinomial_model <- multinom(Opinion_Score ~ ., data = train_df[,2:7], maxit = 1000)
multinom_predicted_probs <- predict(multinomial_model, test_df[,2:6], type = "probs")

multinom_predicted_probs_unique_videos <- multinom_predicted_probs[test_unique_video_indices,]
multinom_predicted_mos <- apply(multinom_predicted_probs_unique_videos, 1, function(x) sum(x*(1:5)))

mean(multinom_predicted_mos - test_mos)  #mean error
mean(abs(multinom_predicted_mos - test_mos)) #mean absolute error
mean((multinom_predicted_mos - test_mos)^2) #MSE

multinomial_mse <- mean((multinom_predicted_mos - test_mos)^2) #0.03 
cor(multinom_predicted_mos, test_mos)  #very correlated, 0.98

##############
#Linear Regression
##############

linear_model <- lm(MOS ~ ., data = train_df_per_video[,2:7])
linear_predicted_mos <- predict(linear_model, test_df_per_video[,2:6]) 
linear_mse <- mean((linear_predicted_mos - test_mos)^2) #0.11
cor(linear_predicted_mos, test_mos) #0.95

##########
#Elastic Net
#########

elastic_model <- glmnet(train_df_per_video[,2:6], train_df_per_video[,7], alpha = 0.5)
elastic_predicted_mos <- predict(elastic_model, as.matrix(test_df_per_video[,2:6]), s= 0.01 )
elastic_mse <- mean((elastic_predicted_mos - test_mos)^2) #0.11
cor(elastic_predicted_mos, test_mos) #0.95

#########
#GLM - use Gamma as suggested as the distribution for MOS in QoE beyond the MOS: an in-depth look at QoE via better metrics and their relation to MOS
##########

gamma_glm <- glm(MOS ~ . , data = train_df_per_video[,c(2:4, 7)], family= "Gamma")
glm_predicted_mos <- predict(gamma_glm, test_df_per_video_glm[,2:4])
glm_mse <- mean((glm_predicted_mos - test_mos)^2) #6.65
cor(glm_predicted_mos, test_mos)  #0.93, so really not a good fit

###########
#Polynomial Regression
##########

np_model <- loess(MOS ~ ., data = train_df_per_video[,3:7], degree= 1)
np_predicted_mos <- predict(np_model, test_df_per_video[,3:6])
np_mse <- mean((np_predicted_mos - test_mos)^2, na.rm=TRUE) #0.10
cor(np_predicted_mos, test_mos, use="complete.obs") #0.96

##########
#GP Regression
##########

gp_model <- fgpm(sIn = train_df_per_video[,2:6], sOut = train_df_per_video[,7])
gp_predicted_mos <- predict(gp_model, sIn.pr = test_df_per_video[,2:6])
gp_mse <- mean((gp_predicted_mos$mean - test_mos)^2) #0.12
cor(gp_predicted_mos$mean, test_mos) #0.94

#########
#Random Forest
#########

rf_model <- randomForest(MOS ~ ., data = train_df_per_video[,2:7])
rf_predicted_mos <- predict(rf_model, test_df_per_video[,2:6])
rf_mse <- mean((rf_predicted_mos - test_mos)^2) #0.39
cor(rf_predicted_mos, test_mos) #0.89

###########
#SVM
##################

svm_model <- svm(MOS ~ ., data = train_df_per_video[,2:7], degree = 3)
svm_predicted_mos <- predict(svm_model, test_df_per_video[,2:6])
svm_mse <- mean((svm_predicted_mos - test_mos)^2) #0.11
cor(svm_predicted_mos, test_mos)#0.96

##############
#MLP Neural Net
###############
nn_train_data <- scale(train_df_per_video)
nn_test_data <- scale(test_df_per_video)
nn <- neuralnet(MOS ~ ., data = train_df_per_video[,2:7],
                hidden = c(2,2,2), stepmax = 1e6, 
                linear.output = TRUE)
nn_predicted_mos <- predict(nn, test_df_per_video[,2:6])
nn_mse <- mean((nn_predicted_mos - test_mos)^2) #0.09
cor(nn_predicted_mos, test_mos) #0.95

#######
#RNN Neural Net
########
library(rnn)
Xvals <- c()
for(i in 2:6){
  Xvals <- cbind(Xvals, int2bin(train_df_per_video[,i])) 
}
X <- array(Xvals, dim=c(dim(int2bin(train_df_per_video[,4])), 2))
Y <- int2bin(train_df_per_video[,7])
rnn_model <- trainr(Y=Y, 
                    X=X, 
                    learningrate   =  0.1,
                    hidden_dim     = 8,
                    batch_size = 10,
                    numepochs = 20)
predict_Xvals <- c()
for(i in 2:6){
  predict_Xvals <- cbind(predict_Xvals, test_df_per_video[,i])
}
predict_X <- array(predict_Xvals, dim = c(dim(int2bin(test_df_per_video[,2])), 2))
rnn_predicted_mos_binary <- predictr(rnn_model, predict_X)
#rnn_predicted_mos <- apply(rnn_predicted_mos_binary, 1, function(x) bin2int(as.matrix(x)))
rnn_predicted_mos <- apply(rnn_predicted_mos_binary, 1, sum)
rnn_mse <- mean((rnn_predicted_mos - test_mos)^2) #2.44
cor(rnn_predicted_mos, test_mos) #0.59


