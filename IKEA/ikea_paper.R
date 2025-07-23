library(ggplot2)
library(ggpubr)
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

###########
#Analysis of IKEA Data
###########

ikea_data <- read.csv("ikea-study-data-5mos.csv")

group_A <- ikea_data[ikea_data$Group == "A",]
group_B <- ikea_data[ikea_data$Group == "B",]
group_C <- ikea_data[ikea_data$Group == "C",]

group_A_overall <- unlist(lapply(group_A[,7:14], as.vector))
group_B_overall <- unlist(lapply(group_B[,7:14], as.vector))
group_C_overall <- unlist(lapply(group_C[1:40,7:14], as.vector))

##########
#A v B Overall
##########
set.seed(2000)
mean(group_A_overall) - mean(group_B_overall) #Group B is 0.06 higher MOS-wise
var.test(group_A_overall, group_B_overall, alternative = "two.sided") #equal var test
t.test(group_A_overall, group_B_overall, alternative = "two.sided", var.equal = TRUE)
#no evidence that the MOS is different
#but what about the distributions?
chisq.test(table(factor(group_A_overall)),
           p = table(factor(group_B_overall))/length(group_B_overall),
           simulate.p.value = TRUE)
#p value 0.048 so some evidence to suggest that the two groups are different
#look at the proportions:
table(factor(group_A_overall)) / length(group_A_overall)
table(factor(group_B_overall))/length(group_B_overall)
#11% more likely for group A to rate video 2 or lower
#41% more likely for group B to rate a video 5 
#Summary: Group A are giving more lower ratings overall, a reverse IKEA effect

g1 <- ggplot() + geom_bar(aes(x = factor(group_A_overall)), col = "black", fill = "red") +
  theme_idris() + labs(x = "Opinion Score", y = "Frequency of Rating") +
  ylim(0,120) +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )
  
g2 <- ggplot() + geom_bar(aes(x = factor(group_B_overall)), col = "black", fill= "blue") +
  theme_idris() + labs(x = "Opinion Score", y = "Frequency of Rating") +
  ylim(0,120) +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )
 
ggarrange(g1, g2, ncol = 2)

############
#A v B per metric
############
set.seed(1000)
p_vals_A_B  <- rep(NA, 8)
for(i in 7:14){
  
  min_level <- max(min(group_A[,i]), min(group_B[,i]))
  max_level <- max(group_A[,i], group_B[,i]) 
  p_vals_A_B[(i-6)] <- chisq.test(table(factor(group_A[,i], levels = min_level:max_level)),
                                       p =  table(factor(group_B[,i], levels = min_level:max_level)) / sum( table(factor(group_B[,i], levels = min_level:max_level))),
                                       simulate.p.value = TRUE)$p.value
}
p_vals_A_B
#High quality: p=0.09, some evidence
#Low quality: p=0.017, evidence of difference
#Rebuffering 1s: p=0.075, some evidence
#No evidence for the other four settings

####High Quality###########

g1 <- ggplot() + 
  geom_bar(aes(x = factor(group_A[,7], levels = 1:5)), col = "black", fill = "red") +
  scale_x_discrete(drop = FALSE) +
  theme_idris() +
  labs(x = "Opinion Score", y = "Frequency of Rating") +
  ylim(0, 25) +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )


g2 <- ggplot() + 
  geom_bar(aes(x = factor(group_B[,7], levels = 1:5)), col = "black", fill = "blue") +
  scale_x_discrete(drop = FALSE) +
  theme_idris() +
  labs(x = "Opinion Score", y = "Frequency of Rating") + ylim(0,25) +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )

ggarrange(g1, g2, ncol = 2)

mean(group_A[,7]) - mean(group_B[,7]) #-0.01, so very similar MOS
mean(group_A[,7] > 3) / mean(group_B[,7] > 3) #but 8% more likely to be a 4+ in A
mean(group_B[,7] == 3)/  mean(group_A[,7] > 3) #10% more likely to be a 3 in Group B
#This suggests there is some IKEA effect: more likely to rate a video highly when HQ

######Low Quality#################

g1 <- ggplot() + 
  geom_bar(aes(x = factor(group_A[,9], levels = 1:5)), col = "black", fill = "red") +
  scale_x_discrete(drop = FALSE) +
  theme_idris() +
  labs(x = "Opinion Score", y = "Frequency of Rating") + ylim(0, 35) +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )


g2 <- ggplot() + 
  geom_bar(aes(x = factor(group_B[,9], levels = 1:5)), col = "black", fill = "blue") +
  scale_x_discrete(drop = FALSE) +
  theme_idris() +
  labs(x = "Opinion Score", y = "Frequency of Rating") + ylim(0,35) +  theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )

ggarrange(g1, g2, ncol = 2)

mean(group_A[,9]) - mean(group_B[,9]) #0.06, so very similar MOS
mean(group_B[,9] > 2) / mean(group_A[,9] > 2) #185% more likely to rate a video fair or better in B
mean(group_A[,9] < 3)/  mean(group_B[,9] < 3) #5% more likely to be a 1 or 2 in group A
#This suggests the IKEA effect with a possible twist; slightly more sympathetic to low quality in B

#######Rebuffering############

g1 <- ggplot() + 
  geom_bar(aes(x = factor(group_A[,14], levels = 1:5)), col = "black", fill = "red") +
  scale_x_discrete(drop = FALSE) +
  theme_idris() + ylim(0, 20) +
  labs(x = "Opinion Score", y = "Frequency of Rating") +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )


g2 <- ggplot() + 
  geom_bar(aes(x = factor(group_B[,14], levels = 1:5)), col = "black", fill = "blue") +
  scale_x_discrete(drop = FALSE) +
  theme_idris() + ylim(0,20) +
  labs(x = "Opinion Score", y = "Frequency of Rating") +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )

ggarrange(g1, g2, ncol = 2)

mean(group_A[,14]) - mean(group_B[,14]) #-0.15, so B has a higher MOS
var.test(group_A[,14], group_B[,14], alternative = "two.sided") #equal var test
t.test(group_A[,14], group_B[,14], alternative = "two.sided", var.equal = TRUE)

mean(group_B[,14] > 3) / mean(group_A[,14] > 3) #38% more likely to rate a video 4+
mean(group_A[,14] < 3)/  mean(group_B[,14] < 3) #47% more likely to be a 1 or 2 in group A
#########
#This shows that users in B are more "resilient" to rebuffering whereas users in A
#downweight their rating much more
#########


g1 <- ggplot() + 
  geom_bar(aes(x = factor(group_A[,11], levels = 1:5)), col = "black", fill = "red") +
  scale_x_discrete(drop = FALSE) +
  theme_idris() + ylim(0, 28) + theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  ) +  labs(x = "Opinion Score", y = "Frequency of Rating")

g2 <- ggplot() + 
  geom_bar(aes(x = factor(group_B[,11], levels = 1:5)), col = "black", fill = "blue") +
  scale_x_discrete(drop = FALSE) +
  theme_idris() + ylim(0, 28) +
  labs(x = "Opinion Score", y = "Frequency of Rating") +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )

ggarrange(g1, g2, ncol = 2)

mean(group_A[,7]) - mean(group_A[,11])  #a drop of 2.25 in MOS is significant!
t.test(group_A[,7], group_A[,11], alternative = "two.sided") #equal var test
#also shows MOS change is significant
mean(group_B[,7]) - mean(group_B[,11])  #a drop of 2.17 in MOS is significant!
t.test(group_B[,7], group_B[,11], alternative = "two.sided") #equal var test
#also shows MOS change is significant

#######
#There is no difference between these two dists, however note that this is because both
#groups are equally "disappointed" with the quality drops
#######

#######
#Correlation / Association
#######

library(confintr)
cramersv(rbind(table(group_A[,7]), table(group_A$Selection.No..of.Clicks))) #0.529
#Moderate Association Between clicks and score
cramersv(rbind(table(group_A[,7]), table(group_A$Selection.Time..s.))) #0.383

A_five <- sapply(group_A[,7], function(x) if(x==5) return(1) else return(0))
logit_model <- glm(A_five ~ group_A$Selection.No..of.Clicks, family = binomial(link = "logit"))
coef(logit_model)

#This shows more clicks increases the MOS
cramersv(rbind(table(group_C[,7]), table(group_C$Selection.No..of.Clicks))) #0.476
#Moderate Association Between clicks and score
cramersv(rbind(table(group_C[,7]), table(group_C$Selection.Time..s.))) #0.359

C_five <- sapply(group_C[,7], function(x) if(x==5) return(1) else return(0))
logit_model <- glm(C_five ~ group_C$Selection.No..of.Clicks, family = binomial(link = "logit"))
coef(logit_model) #0.139
##############
#A v C
##############

set.seed(200)
mean(group_A_overall) - mean(group_C_overall) #Group A is 0.09 higher MOS-wise
var.test(group_A_overall, group_C_overall, alternative = "two.sided") #equal var test
t.test(group_A_overall, group_C_overall, alternative = "two.sided", var.equal = TRUE)
#no evidence that the MOS is different
#but what about the distributions?
chisq.test(table(factor(group_A_overall)),
           p = table(factor(group_C_overall))/length(group_C_overall),
           simulate.p.value = TRUE)
#p value 0.001 so evidence to suggest that the two groups are different
#look at the proportions:
table(factor(group_A_overall)) / length(group_A_overall)
table(factor(group_C_overall))/length(group_C_overall)
#12% more likely for group C to rate video 1
#151% more likely for group A to rate a video 5 
#Summary: Group A are giving more higher ratings overall, as expected

g1 <- ggplot() + geom_bar(aes(x = factor(group_A_overall)), col = "black", fill = "red") +
  theme_idris() + labs(x = "Opinion Score", y = "Frequency of Rating") +
  ylim(0,120) +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )

g2 <- ggplot() + geom_bar(aes(x = factor(group_C_overall)), col = "black", fill= "blue") +
  theme_idris() + labs(x = "Opinion Score", y = "Frequency of Rating") +
  ylim(0,120) +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )

ggarrange(g1, g2, ncol = 2)

#############
#B v C Overall
################


set.seed(400)
mean(group_B_overall) - mean(group_C_overall) #Group B is 0.15 higher MOS-wise
var.test(group_B_overall, group_C_overall, alternative = "two.sided") #equal var test
#some evidence that the var is different 
t.test(group_B_overall, group_C_overall, alternative = "two.sided", var.equal = FALSE)
#p = 0.089 so evidence that the MOS is different
#but what about the distributions?
chisq.test(table(factor(group_B_overall)),
           p = table(factor(group_C_overall))/length(group_C_overall),
           simulate.p.value = TRUE)
#p value 0.001 so evidence to suggest that the two groups are different
#look at the proportions:
table(factor(group_B_overall)) / length(group_B_overall)
table(factor(group_C_overall))/length(group_C_overall)
#12% more likely for group C to rate video 2 or lower
#11% more likely for group B to rate a video 4 or higher
#Summary: Group A are giving more higher ratings overall, as expected

g1 <- ggplot() + geom_bar(aes(x = factor(group_B_overall)), col = "black", fill = "red") +
  theme_idris() + labs(x = "Opinion Score", y = "Frequency of Rating") +
  ylim(0,120) +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )

g2 <- ggplot() + geom_bar(aes(x = factor(group_C_overall)), col = "black", fill= "blue") +
  theme_idris() + labs(x = "Opinion Score", y = "Frequency of Rating") +
  ylim(0,120) +   theme(
    axis.title = element_text(size = 14),   # axis titles
    axis.text  = element_text(size = 14)    # axis tick labels
  )

ggarrange(g1, g2, ncol = 2)

