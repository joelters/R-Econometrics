rm(list = ls())
library(devtools)
library(dplyr)
library(stargazer)
library(PSweight)
# see https://github.com/jjchern/lalonde
devtools::install_github("jjchern/lalonde")
nsw_lalonde <- as_tibble(lalonde::nsw)
psid_lalonde <- as_tibble(lalonde::psid_controls)
cps_lalonde <- lalonde::cps_controls
Y <- nsw_lalonde$re78
D <- nsw_lalonde$treat
X <- nsw_lalonde[,3:8]

#EXPERIMENTAL ATE
m1exp <- lm(re78 ~ treat, nsw_lalonde)
summary(m1exp)
tau1exp <- m1exp$coefficients[2]
m2exp <- lm(re78 ~ treat + poly(age,2) + education + black + hispanic + nodegree, nsw_lalonde)
summary(m2exp)
tau2exp <- m2exp$coefficients[2]
# m3exp <- lm(re78 ~ treat + age + age^2 + education + black + hispanic + nodegree + 
#            treat*(age + education + black + hispanic + married + nodegree), nsw_lalonde)
# summary(m3exp)
# b1_binter <- m3$coefficients[c(2,9:15)]
# tau3exp <- b1_binter[1] + b1_binter[2:7] %*% colMeans(nsw_lalonde[,3:8])
# 
# #to get inference in 3
# Y <- nsw_lalonde$re78
# D <- nsw_lalonde$treat
# X <- nsw_lalonde[,3:8]
# DXc <- D*scale(X, center = TRUE, scale = FALSE)
# colnames(DXc) <- paste("dx",names(X), sep = "_")
# df3 <- cbind(Y = Y,D,X,DXc)
# m3exp <- lm(Y ~., df3)
# summary(m3exp)


#OBSERVATIONAL ASSUMING CMIA
obs_df <- nsw_lalonde %>%
  filter(treat == 1) %>% 
  bind_rows(cps_lalonde) %>% 
  select(-data_id)

m1obs <- lm(re78 ~ treat, obs_df)
summary(m1obs)
tau1obs <- m1obs$coefficients[2]
m2obs <- lm(re78 ~ treat + poly(age,2) + education + black + hispanic + nodegree, obs_df)
summary(m2obs)
tau2obs <- m2obs$coefficients[2]
tau1obs
tau2obs



#Logit Model
logm = glm(treat ~ poly(age,2) + poly(education,2) + married + poly(re75,2) + black + hispanic + nodegree,
             data = obs_df, family = "binomial")
summary(logm)
obs_df$pscore <- logm$fitted.values


#IPW


# obs_df <- filter(obs_df, pscore > min(filter(obs_df,treat == 1)$pscore))
Y <- obs_df$re78
D <- obs_df$treat
PS <- obs_df$pscore

ipw1 <- mean(D*Y/PS - (1-D)*Y/(1-PS))
ipw2 <- (1/(sum(D/PS)))*sum(D*Y/PS) - (1/(sum((1-D)/(1-PS))))*sum((1-D)*Y/(1-PS))

# Doubly Robust
m1 <- lm(re78 ~ poly(age,2) + education + black + hispanic + nodegree, filter(obs_df,treat == 1))
m1hat <- predict(m1, obs_df)
m0 <- lm(re78 ~ poly(age,2) + education + black + hispanic + nodegree, filter(obs_df,treat == 0))
m0hat <- predict(m0, obs_df)

ipwdr <- mean((D*Y - (D-PS)*m1hat)/PS) - mean(((1-D)*Y - (D-PS)*m0hat)/(1-PS))
c("IPW1" = ipw1, "IPW2" = ipw2, "DR" = ipwdr)

#Common support and Trimming

c1 <- rgb(173,216,230,max = 255, alpha = 180, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 180, names = "lt.pink")

par(mar = c(5, 5, 5, 5) + 0.3)
hist(filter(obs_df, treat == 1)$pscore, freq = TRUE, col = c1, axes = FALSE, xlab = "", ylab = "", main = "")
axis(side = 1, xlim = c(0,1))
axis(side = 4, ylab = "")
mtext(side = 4, text = "NSW", line = 2.5, col = "blue")
par(new=TRUE)
hist(filter(obs_df, treat == 0)$pscore, ylim = c(0,16000), freq = TRUE, axes = FALSE, col = c2, xlab = "", ylab = "", main = "Common Support")
axis(side = 2)
mtext(side = 2, text = "CPS", line = 2.5, col = "pink")

obs_df <- filter(obs_df, pscore > 0.25)

par(mar = c(5, 5, 5, 5) + 0.3)
hist(filter(obs_df, treat == 1)$pscore, freq = TRUE, col = c1, axes = FALSE, xlab = "", ylab = "", main = "")
axis(side = 1, xlim = c(0,1))
axis(side = 4, ylab = "")
mtext(side = 4, text = "NSW", line = 2.5, col = "blue")
par(new=TRUE)
hist(filter(obs_df, treat == 0)$pscore, freq = TRUE, axes = FALSE, col = c2, xlab = "", ylab = "", main = "Common Support")
axis(side = 2)
mtext(side = 2, text = "CPS", line = 2.5, col = "pink")

Y <- obs_df$re78
D <- obs_df$treat
PS <- obs_df$pscore

ipw1 <- mean(D*Y/PS - (1-D)*Y/(1-PS))
ipw2 <- (1/(sum(D/PS)))*sum(D*Y/PS) - (1/(sum((1-D)/(1-PS))))*sum((1-D)*Y/(1-PS))

# Doubly Robust
m1 <- lm(re78 ~ poly(age,2) + education + black + hispanic + nodegree, filter(obs_df,treat == 1))
m1hat <- predict(m1, obs_df)
m0 <- lm(re78 ~ poly(age,2) + education + black + hispanic + nodegree, filter(obs_df,treat == 0))
m0hat <- predict(m0, obs_df)

ipwdr <- mean((D*Y - (D-PS)*m1hat)/PS) - mean(((1-D)*Y - (D-PS)*m0hat)/(1-PS))
c("IPW1" = ipw1, "IPW2" = ipw2, "DR" = ipwdr)
