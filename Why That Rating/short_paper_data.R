############
#QOMEX - Why That Rating?
#############

library(nnet)
library(CrossValidate)
library(philentropy)
library(e1071)
library(marginaleffects)
library(margins)
library(randomForest)

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

opinion_df <- read.csv("Frdna_Opinion_df.csv")
unique_video_features_df <- read.csv("Frdna_MOS_df.csv")
opinion_df <- opinion_df[,-1] #drop index column
unique_video_features_df <- unique_video_features_df[,-1]
opinion_df$fair_better <- opinion_df$fair_better*100

opinion_factors <- factor(opinion_df$Opinion_Score)
n_videos <- 424

#opinion_df$SSIM <- (opinion_df$SSIM + 1) / 2
#opinion_df$VMAF <- opinion_df$VMAF / max(opinion_df$VMAF)
opinion_df <- opinion_df[,-2]  #drop codec

for(i in 2:7){
  opinion_df[,i] <- (opinion_df[,i] - min(opinion_df[,i])) / (max(opinion_df[,i]) - min(opinion_df[,i]))
}

# Create the pattern of 60 1s and 60 0s
train_indices <- rep(c(rep(1, 60), rep(0, 60)), 212)
test_indices <- rep(c(rep(0, 60), rep(1, 60)), 212)
train_df <- opinion_df[which(train_indices ==1),]
test_df <- opinion_df[which(test_indices==1),]

#fit multinomial to training data

multinomial_model <- multinom(Opinion_Score ~ ., data = train_df[,2:8], maxit = 1000)
model_coefs <- coef(multinomial_model)[,2:7]
predicted_probs <- predict(multinomial_model, test_df[,2:7], type = "probs")

exp(model_coefs[,1:5])  #risk ratio

#marginal_effects <- avg_slopes(multinomial_model)
#marginal_effects_table <- matrix(marginal_effects$estimate, nrow = 5, ncol = 7)

#LRT
null_model <- multinom(Opinion_Score ~ 1,  train_df[,2:8]) #intercept only
2*(null_model$value - multinomial_model$value)
qchisq(0.95, 20)

predict_se <- chisq_p_vals <- rep(NA, length = n_videos) #for predicted squared error
for(i in 1:n_videos){
  if(length(which(test_df$Unique_ID == i)) > 0){
    current_video_features <- test_df[which(test_df$Unique_ID == i), 2:7]
    current_video_opinions <- test_df[which(test_df$Unique_ID == i),8]
    current_probs <- table(factor(current_video_opinions, levels = 1:5)) / 60
    predicted_probs <- predict(multinomial_model, current_video_features, type = "probs")[1,]
    predict_se[i] <- sum((as.vector(current_probs)-as.vector(predicted_probs))^2)
  
    chisq_p_vals[i] <- chisq.test(rbind(current_probs, predicted_probs),
                                          simulate.p.value = TRUE)$p.value
  }
}
predict_se <- na.omit(predict_se)
mean(predict_se)  #MSE - 0.0924
chisq_p_vals <- na.omit(chisq_p_vals)
sum(chisq_p_vals < 0.05) /n_videos  #no evidence to reject at 5% level that any dist diff

#compute average KLD
predicted_probs <- predict(multinomial_model, test_df[,2:7], type = "probs")
true_outcome <- as.integer(test_df$Opinion_Score)
neg_log_probs <- -log( predicted_probs[ cbind(seq_len(nrow(predicted_probs)), true_outcome)] )
mean_kl_mlr <- mean(neg_log_probs)  #0.702
################
#Plotting
#################
library(ggplot2)
library(ggpubr)


g1 <- ggplot() + geom_bar(aes(x = factor(predict(multinomial_model, test_df[which(test_df$Resolution == 0.5 & test_df$Bitrate == 0.5),2:7]))), 
                          col = "blue", fill = "white") + theme_idris() +
  labs(x = "Predicted Opinion", y = "Count") +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )

g2 <- ggplot() + geom_bar(aes(x = factor(test_df$Opinion_Score[which(test_df$Resolution == 0.5 & test_df$Bitrate == 0.5)])), 
                          col = "red", fill = "white") + theme_idris() +
  labs(x = "True Opinion", y = "Count") +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )

ggarrange(g1, g2, ncol = 2)


##############
#Comparisons
###############
#We use an SVM, a RF, a NN, and mixed binary logistic regressions

#####
#SVM
#####

svm_model <- svm(factor(Opinion_Score) ~ ., data = train_df[,2:8], probability = TRUE,
                 scale = TRUE, degree = 3, kernel = "radial")
svm_se <- rep( NA, n_videos)
for(i in 1:n_videos){
  if(length(which(test_df$Unique_ID == i)) > 0){
    current_video_features <- test_df[which(test_df$Unique_ID == i), 2:7]
    current_video_opinions <- test_df[which(test_df$Unique_ID == i),8]
    current_probs <- table(factor(current_video_opinions, levels = 1:5)) / 60
    predicted_probs <- attr(predict(svm_model, current_video_features[1,], probability = TRUE), "probabilities")
    svm_se[i] <- sum((as.vector(current_probs)-as.vector(predicted_probs))^2)
  }
}
svm_se <- na.omit(svm_se)
mean(svm_se)  #MSE - 0.755
#compute average KLD
predicted_probs <- predict(svm_model, test_df[,2:7], probability = TRUE)
predicted_probs <- attr(predicted_probs, "probabilities")
true_outcome <- as.integer(test_df$Opinion_Score)
neg_log_probs <- -log( predicted_probs[ cbind(seq_len(nrow(predicted_probs)), true_outcome) ] )
mean_kl_svm <- mean(neg_log_probs)  #2.615

##########
#Random Forest
###########

rf_model <- randomForest(as.factor(Opinion_Score) ~ ., data = train_df[,2:8], ntree=500, corr.bias = TRUE)
rf_se <- rep(NA, n_videos)
for(i in 1:n_videos){
  if(length(which(test_df$Unique_ID == i)) > 0){
    current_video_features <- test_df[which(test_df$Unique_ID == i), 2:7]
    current_video_opinions <- test_df[which(test_df$Unique_ID == i),8]
    current_probs <- table(factor(current_video_opinions, levels = 1:5)) / 60
    predicted_probs <- predict(rf_model, current_video_features, type = "prob")[1,]
    rf_se[i] <- sum((as.vector(current_probs)-as.vector(predicted_probs))^2)
  }
}
rf_se <- na.omit(rf_se)
mean(rf_se)  #MSE - 0.0924

predicted_probs <- predict(rf_model, test_df[,2:7], type = "prob")
true_outcome <- as.integer(test_df$Opinion_Score)
neg_log_probs <- -log( predicted_probs[ cbind(seq_len(nrow(predicted_probs)), true_outcome)] )
mean_kl_rf <- mean(neg_log_probs[neg_log_probs < 1e10])  #0.724

############
# NN 
##############

library(nnet)
set.seed(1)
train_df$Opinion_Score <- factor(train_df$Opinion_Score, levels = 1:5)

mlp_model <- nnet(Opinion_Score ~ ., data = train_df[, 2:8], size = c(5), decay = 0.01, maxit = 500)
nn_se <- rep(NA, n_videos)

for (i in 1:n_videos) {
  if (length(which(test_df$Unique_ID == i)) > 0) {
    current_video_features <- test_df[which(test_df$Unique_ID == i), 2:8]
    current_video_opinions <- test_df[which(test_df$Unique_ID == i), 8]
    
    current_probs <- table(factor(current_video_opinions, levels = 1:5)) / 60
    current_input <- as.data.frame(current_video_features[1, , drop = FALSE])
    colnames(current_input) <- colnames(train_df)[2:8]
    predicted_probs <- predict(mlp_model, newdata = current_input, type = "raw")
    nn_se[i] <- sum((as.vector(current_probs) - as.vector(predicted_probs))^2)
  }
}

nn_se <- na.omit(nn_se)
mean(nn_se) #0.087

predicted_probs <- predict(mlp_model, newdata = test_df[,2:7], type = "raw")
true_outcome <- as.integer(test_df$Opinion_Score)
neg_log_probs <- -log( predicted_probs[ cbind(seq_len(nrow(predicted_probs)), true_outcome)] )
mean_kl_nn <- mean(neg_log_probs[neg_log_probs < 1e10])  #0.707

 
###########
#Mixed Binary Logistic
###########

mixed_binary_opinion <- function(df){
  
  logistic_list <- vector(mode="list", 5)
  for(i in 1:5){
    df$Opinion_Score <- sapply(as.vector(df$Opinion_Score), function(x) {if(x <= i) return(1) else return(0)})
    logistic_list[[i]] <- glm(Opinion_Score ~ ., data = df, family = binomial)
  }
  return(logistic_list)
}

mixed_binary_opinion_predict <- function(logit_list, new_data){
  prob_vec <- rep(NA, 5)
  for(i in 1:5){
    prob_vec[i] <- predict(logit_list[[i]], new_data, type = "response")
  }
  prob_vec <- c(prob_vec[i], diff(prob_vec))
  prob_vec <- prob_vec / sum(prob_vec)
  return(prob_vec)
}

mixed_binary_logistic_model <- mixed_binary_opinion(train_df)
mixed_se <- rep(NA, n_videos)

for(i in 1:n_videos){
  if(length(which(test_df$Unique_ID == i)) > 0){
    current_video_features <- test_df[which(test_df$Unique_ID == i), 1:7]
    current_video_opinions <- test_df[which(test_df$Unique_ID == i),8]
    current_probs <- table(factor(current_video_opinions, levels = 1:5)) / 60
    predicted_probs <- mixed_binary_opinion_predict(mixed_binary_logistic_model, current_video_features[1,])
    mixed_se[i] <- sum((as.vector(current_probs)-as.vector(predicted_probs))^2)
  }
}
mixed_se <- na.omit(mixed_se)
mean(mixed_se)  #0.389

predicted_probs <- matrix(NA, nrow = 212, ncol = 5)
for(i in 1:212){
  predicted_probs[i,] <- mixed_binary_opinion_predict(mixed_binary_logistic_model, test_df[i,])
  
}
predicted_probs[predicted_probs == 0] <- 1e-4
true_outcome <- as.integer(test_df$Opinion_Score)
neg_log_probs <- -log( predicted_probs[ cbind(seq_len(nrow(predicted_probs)), true_outcome)] )
mean_kl_mixed <- mean(neg_log_probs[neg_log_probs < 1e12])  #5.53
##############
#Decision Tree
#
##############
library(rpart)
tree_model <- rpart(Opinion_Score ~., data = train_df, method = "class", 
                    parms = list(prior = c(0.2, 0.2, 0.2, 0.2, 0.2))) #uniform prior
tree_se <- rep(NA, n_videos)

for(i in 1:n_videos){
  if(length(which(test_df$Unique_ID == i)) > 0){
    current_video_features <- test_df[which(test_df$Unique_ID == i), 1:7]
    current_video_opinions <- test_df[which(test_df$Unique_ID == i),8]
    current_probs <- table(factor(current_video_opinions, levels = 1:5)) / 60
    predicted_probs <- predict(tree_model, current_video_features[1,], type = "prob")
    tree_se[i] <- sum((as.vector(current_probs)-as.vector(predicted_probs))^2)
  }
}
tree_se <- na.omit(tree_se)
mean(tree_se)  #0.120

predicted_probs <- predict(tree_model, test_df[,1:7], type = "prob")
true_outcome <- as.integer(test_df$Opinion_Score)
neg_log_probs <- -log( predicted_probs[ cbind(seq_len(nrow(predicted_probs)), true_outcome)] )
mean_kl_tree <- mean(neg_log_probs[neg_log_probs < 1e10])  #0.766

