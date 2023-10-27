#Genís Ruiz Menárguez
#Dataset from the end of 19-20 season in football Top5 leagues
#data from https://www.kaggle.com/datasets/kriegsmaschine/soccer-players-values-and-their-statistics

dataset = read.csv("transfermarkt_fbref_201920_short.csv", sep=';', row.names = NULL, header=TRUE)
dataset = read.csv("transfermarkt_fbref_201920_copia.csv", sep=';', row.names = NULL, header=TRUE)

summary(dataset)
#From the summary we can see several things:
#-The mean of the age of the Top 5 leagues is 25.32, as it is the best physical moment for a human being.
#-A 25% of the players have their value under 1 milion€
#-The 50% of the players play at least 19 games, the 50% of the league games.
#-The most part of the players didn't score a gol in the 19-20 season
#-The stats about saves are useless as it include all types of position and not only GK 
#-The average of Points per game in a player are 1.368. The maximum average is 2.61, from Liverpool
#-Only a 18.12% of players are in a team which has played in the Champions League

head(dataset)
str(dataset)

#Number of players per league
bar_data = table(dataset$league)
barplot(bar_data, main = "Number of Players by League", xlab = "League", ylab = "Count")


boxplot(dataset$value ~ dataset$league, main = "Player Value by League", xlab = "League", ylab = "Value")
#We can see that the outliers in Ligue1 might be Mbappe and Neymar, who are higly valued

hist(dataset$age, main = "Distribution of Player Ages", xlab = "Age", ylab = "Frequency")

plot(dataset$minutes, dataset$goals, main = "Player Goals v. Minutes Played", xlab = "Minutes Played", ylab = "Goals")
#As they play more minutes, they score more goals. 
#The cases with lots of minutes and 0 goals are the goalkeepers (from now on GK)

plot(dataset$saves, dataset$value, main = "GK saves vs its value", xlab = "Number of saves", ylab = "Value")

library(ggplot2)
subset_dataset = subset(dataset, position == "GK" & minutes > 1800)

ggplot(subset_dataset, aes(x = dataset$minutes, y = dataset$saves, color = dataset$value)) +
  geom_jitter(width = 0.2) +
  scale_color_gradientn(colors = c("blue", "lightblue", "yellow", "red"))
#We can see there is no pattern in the relation of the first GK of a team(minutes >1800) + saves
#with the value. The explanation is simple, good GK are in good teams which have good defense, so
#GK do not need to do receive lots of attacks. 

#After all this plots, I have decided to remove the GK from the dataset, as they have different
#stats from other positions. I have also removed the column 'saves', an exclusive data from GK.


#we use attach to use directly league instead of players_df$league
attach(players_df)
#Number of players per position
bar_data2 = table(dataset$position)
barplot(bar_data2, main = "Number of Players by position", xlab = "Position", ylab = "Count")
players_df = subset(dataset, position != "GK", select = c(-saves))

#Because of the value will be the variable to study and predict, it is better to reescalate its value.
#As the player values are high numbers, dividing by 1Milion will let us study the parameters and
#coefficients more easily. 
players_df$value = players_df$value/1000000

#----------------------------------------------------------------------------------------------

#Before computing the linear regression, response variables correlations with other numeric columns
#will be studied. 
#numeric_df is a numeric dataset which will be used to compute correlations.
numeric_df = subset(players_df, select = c(-nationality,-squad,-league,-position, -player))
cor(numeric_df$value, numeric_df)
#From these correlations we see that each one is different from 0, meaning there is a relation between
#the player's value and every column. Nevertheless, height and age present low correlations. As both
#parameters seem to be meaningful to predict values, other statistical studies will be performed. 
#A linear regression will be computed, looking for the best fit for the model. 

#!
#fit = lm(value ~ nationality + position + squad + age + games + goals + assists + minutes, data=players_df)
#!
fit = lm(value ~ nationality + position + squad + age + games + goals + assists +
               minutes + goalsxgame +height + Pts.G + CL, data=players_df)
#fit_league = lm(value ~ nationality + position + league + age + games + goals + assists + CL, data=players_df)
anova(fit)
summary(fit)
#If we want to see the coefficients in all the rows, we execute summary(fit)$coefficients
#We can see that the nationality has not a great impact in the regression, as there is not any nationality which
#has a lower pvalue than 0.1. In the following graphic, pvalues of different nationalities will be plotted

#The  length(unique(players_df$nationality)) command which is equal to 102, so we have 102 different nationalities.
# As summary(fit)$coefficients[1] gives us the coefficient of the intercept, the pvalues of the nationality parameters
#will be summary(fit)$coefficients[2:102, "Pr(>|t|)"]

boxplot(summary(fit)$coefficients[2:102, "Pr(>|t|)"], main = "Nationalities P-value")
#We can see that the lowest nationality pvalue is around 0.5, meaning the probability of H0: nationality_x != 0
#equals to 0.5. The reason why the nationality has high values is because every country has
#valuable and non-valuable players. The lowest nationality pvalue is from New Zeland with a
#a negative coefficient, meaning their players have low value.

#From the summary(fit)$coefficients it is also clear that the positions "FW" and "MF" are the ones who have a greater
#impact in the prediction of the value, as forwards and midfielders are the most expensive positions.

#Is is also observable that the bigger the club is, the more valuables players it has. For example, FC Barcelona or
#Bayern Munich have a tiny pvalue. Teams such as Manchester City, which make huge inversions in players every summer,
#have the tiniest p-values, meaning that belonging to these clubs has a huge repercussion while predicting player's value.

#Regarding age, it is visible that the age coefficient is negative, meaning the older a player is, the cheaper it costs.
#Besides, fixing the parameters and increasing the number of games played will reduce the player's cost.

#In addition, goalsxgame has the greatest coefficient positive value, as it implies a great correlation with the response variable
#Moreover, parameters such as the assists, minutes and the height are determinant too.

#Nevertheless, Pts.G or CL result in a NA, even not containing missing values.
#A new model must be created, with just the reliable variables Backward selection by AIC will be applied since it is pretended
#to minimize the number of variables used (as there is few observed data) and maximize the goodness of the fit.


#---BACKWARD SELECTION CRITERIA---------------------
#Backward selection will be applied, to realize which is the best possible fit of the parameters following AIC.
#Applying Backward Criteria to the fit will dismiss "Nationality", Pts.G and CL
library(MASS)
summary(fit)
stepAIC(fit, trace=TRUE, direction="backward")
fit = update(fit, .~. - nationality - Pts.G -CL)

#-----------predicts------------------------
predict(fit)
df_pred1 = data.frame(position = "FW", squad = "Betis", age = 25, games = 25, goals = 30, assists = 3, minutes = 2200)
predict(fit, newdata = df_pred1, interval = "prediction")

#stats Alexander Isak Real Sociedad 2020-2021: Transfermarkt 08/10/20 22M, 19/03/21 30M, 31/05/21 40M
df_pred2 = data.frame(position = "FW", squad = "Real_Sociedad", age = 21, 
            games = 34, goals = 17, assists = 2, minutes = 2361, goalsxgame = 0.49, height = 192)
predict(fit, newdata = df_pred2, interval = "prediction")
#For this player, the value in 19/20 was 22M, and the prediction for the next year with the stats of 20/21 is 34.1M.
#The player value in the final part of the 20/21 season was between 30-40M, our prediction fits with the real value 


#Fofana 18/03/21 30M 08/06/21 40M 20-21 
df_pred3 = data.frame(position = "DF", squad = "Leicester_City", age = 20, games = 28, goals = 0, assists = 1,
                      minutes = 2265, goalsxgame = 0.00, height= 186)
predict(fit, newdata = df_pred3, interval = "prediction")
#This player in the previous season was valued 5M. As he was one of the promises of the 20/21 season, the prediction
#obtained (17.78M) is far from the player value at the end of that season. It is observable it is difficult to make
#an accurate prediction for those players who have an unexpected growth. 

#pellegrini 18/03/21 30M 08/06/21 40M 20-21 
df_pred4 = data.frame(position = "MF", squad = "Roma", age = 24, games = 34, goals = 7, assists = 6,
                      minutes = 2607, goalsxgame = 0.21, height= 186 )
predict(fit, newdata = df_pred4, interval = "prediction")
 
#pezzella
df_pred5 = data.frame(position = "DF", squad = "Fiorentina", age = 29, games = 32, goals = 1, assists = 2,
                      minutes = 2748, goalsxgame = 0.032, height= 187 )
predict(fit, newdata = df_pred5, interval = "prediction")

#nice player: Jeff Reine-Adélaïde
df_pred6 = data.frame(position = "MF", squad = "Nice", age = 23, games = 14, goals = 1, assists = 3,
                      minutes = 1053, goalsxgame = 0.071, height= 184 )
predict(fit, newdata = df_pred6, interval = "prediction")

#isco
df_pred7 = data.frame(position = "MF", squad = "Real_Madrid", age = 29, games = 25, goals = 0, assists = 2,
                      minutes = 898, goalsxgame = 0.0, height= 176 )
predict(fit, newdata = df_pred7, interval = "prediction")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---------------------------------------
#from plots: 1859 is Mbappe, 1863 is Neymar, 2342 is Harry Kane no CL. Outliers!
plot(fit, which = 1)
plot(fit, which = 2)
plot(fit, which = 3)
plot(fit, which = 5)
plot(players_df$value)

#-----------------NONPARAMETRIC BOOTSTRAP FOR PREDICTING VALUE MEDIAN-----------------------------
#Computing the median of the players' value will allow us to obtain an estimation of the value of a medium level player.
estimated_median = median(players_df$value) 
#However, the median of the predicted players' value will be performed by bootstrapping, so k samples with replacement
#from the players_df dataset will be considered. To each auxiliar dataset, a linear regression and prediction has been
#carried out. And for each prediction, the median has been computed and stored. Once there is a vector of k medians,
#calculating the mean will give us a more reliable result. 

k <- 10000
boot_median <- numeric(k)

# Run bootstrap
for (i in 1:k) {
  #k samples with replacement
  bootstrap_sample <- players_df[sample(nrow(players_df) , replace = TRUE), ]
  
  fit <- lm(value ~ position + squad + age + games + goals + assists +
              minutes + goalsxgame + height, data = bootstrap_sample)
  
  predictions <- predict(fit)
  
  #Median for each value prediciton
  boot_median[i] <- median(predictions)
}

#stats
summary(boot_median)
ci_medians = quantile(boot_median, c(0.025, 0.975))
cat("Estimated median:", estimated_median, "\n")
cat("Bootstrap mean of medians:", mean(boot_median), "\n")
cat("Bootstrap Confidence Interval for the medians:", ci_medians, "\n")

#The mean of the vector of predicted values median named boot_median is 6,759,325€
#The interval of minimum and maximum medians obtained is [5.730M, 7.935M], and thicker than the CI.
#So, the medium value of the players in the Top5 Leagues is in the interval [6.202M, 7.342M]


#---------------------------------------------------------------------------------------
#///////////////////////PARAMETRIC BOOTSTRAP FOR THE RESIDUALS///////////////////////////
estimated_sigma = summary(fit)$sigma

residuals = residuals(fit)
residual_mean = mean(residuals)

k = 100000  
bootstrap_sigmas = numeric(k)

for (i in 1:k) {
  #bootstrap residuals
  bootstrap_residuals = rnorm(length(residuals), mean = residual_mean, sd = estimated_sigma)
  
  #standard deviation of bootstrap sample
  bootstrap_sigmas[i] = sd(bootstrap_residuals)
  
}

#Boot mean sigma
boot_mean_sigma = mean(bootstrap_sigmas)

ci = quantile(bootstrap_sigmas, c(0.025, 0.975))

cat("Estimated sigma:", estimated_sigma, "\n")
cat("Bootstrap Mean sigma:", boot_mean_sigma, "\n")
cat("Bootstrap Confidence Interval for sigma:", ci, "\n")


# Plot histograms
hist(bootstrap_sigmas, main = "Bootstrap Sigma Estimates", xlab = "Sigma")
abline(v = estimated_sigma, col = "red", lwd = 2)
abline(v = boot_mean_sigma, col = "blue", lwd = 2)
abline(v = ci, col = "green", lty = 2) #CI
legend("topleft", legend = c("Estimated σ", "N.B σ","95% CI"),
       col = c("red", "blue", "green"), lwd = 2, pt.cex = 2)





#---------------------------------------------------
#/////////NONPARAMETRIC BOOTSTRAP FOR COMPARATING GOALS X GAME IN TWO LEAGUES////////////////

#nonparametric bootstrap:
k = 100000  # Number of bootstrap samples
FW_laliga_df = subset(laliga_df, position == "FW" & minutes > 1700) 
laliga_gxg = FW_laliga_df$goalsxgame

boot_gxg_laliga = replicate(k, sample(laliga_gxg, replace = TRUE))
boot_mean_gxg_laliga = apply(boot_gxg_laliga, 2, mean)
CI_laliga = quantile(boot_mean_gxg_laliga, c(0.025,0.975))
mean(laliga_gxg)
cat("Non-Parametric Bootstrapping mean La Liga:",mean(boot_mean_gxg_laliga))

hist(boot_mean_gxg_laliga, main = "GoalsxGame NonParametric Bootstrap La Liga",
     xlab = "Mean GoalsxGame")
#pl pl pl pl pl pl pl pl pl
FW_pl_df = subset(pl_df, position == "FW" & minutes > 1700) 
pl_gxg = FW_pl_df$goalsxgame

boot_gxg_pl = replicate(k, sample(pl_gxg, replace = TRUE))
boot_mean_gxg_pl = apply(boot_gxg_pl, 2, mean)
CI_pl = quantile(boot_mean_gxg_pl, c(0.025,0.975))
mean(pl_gxg)
cat("Non-Parametric Bootstrapping mean PL:",mean(boot_mean_gxg_pl))


hist(boot_mean_gxg_pl, main = "GoalsxGame NonParametric Bootstrap PL",
     xlab = "Mean GoalsxGame")


density_laliga = density(boot_mean_gxg_laliga)
plot(density_laliga, main = "Density - FW mean GoalsxGame in La Liga",
     xlab = "Mean GoalsxGame", ylab = "Density")
abline(v = mean(laliga_gxg), col = "red", lwd = 2)  #Observed mean
abline(v = CI_laliga, col = "blue", lty = 2)  #CI

density_pl = density(boot_mean_gxg_pl)
plot(density_pl, main = "Density - FW mean GoalsxGame in Premier League",
     xlab = "Mean GoalsxGame", ylab = "Density")
abline(v = mean(pl_gxg), col = "red", lwd = 2) #Observed mean
abline(v = CI_pl, col = "blue", lty = 2) #CI


#/////////POISSON BOOTSTRAPPING FOR COMPARATING GOALS X GAME IN TWO LEAGUES////////////////

#Let's use poisson:
k <- 1000000  # Number of bootstrap samples
FW_laliga_df <- subset(laliga_df, position == "FW" & minutes > 1700) 
laliga_gxg <- FW_laliga_df$goalsxgame

lambda_laliga <- mean(laliga_gxg)  # Parameter for the Poisson distribution

boot_gxg_laliga <- replicate(k, rpois(length(laliga_gxg), lambda_laliga))
boot_mean_gxg_laliga <- apply(boot_gxg_laliga, 2, mean)
CI_laliga <- quantile(boot_mean_gxg_laliga, c(0.025, 0.975))
mean(laliga_gxg)
cat("Bootstrapping mean La Liga: ",mean(boot_gxg_laliga))


hist(boot_mean_gxg_laliga, main = "Poisson - GoalsxGame Bootstrap in La Liga",
     xlab = "Mean GoalsxGame", xlim=c(0,0.8))

#----------------PREMIER LEAGUE-----------------------------
FW_pl_df = subset(pl_df, position == "FW" & minutes > 1700) 
pl_gxg = FW_pl_df$goalsxgame
lambda_pl = mean(pl_gxg)
boot_gxg_pl = replicate(k, rpois(length(pl_gxg), lambda_pl))
boot_mean_gxg_pl = apply(boot_gxg_pl, 2, mean)
CI_pl = quantile(boot_mean_gxg_pl, c(0.025,0.975))
mean(pl_gxg)
cat("Bootstrapping mean PL: ",mean(boot_gxg_pl))

hist(boot_mean_gxg_pl, main = "Poisson - GoalsxGame Bootstrap in PL",
     xlab = "Mean GoalsxGame",xlim=c(0,0.8))


density_laliga = density(boot_mean_gxg_laliga)
plot(density_laliga, main = "Poisson Density\n FW mean GoalsxGame in La Liga",
     xlab = "Mean GoalsxGame", ylab = "Density", xlim=c(0,0.8))
abline(v = mean(laliga_gxg), col = "red", lwd = 2)  #Observed mean
abline(v = CI_laliga, col = "blue", lty = 2)  #CI

density_pl = density(boot_mean_gxg_pl)
plot(density_pl, main = "Poisson Density\n FW mean GoalsxGame in PL",
     xlab = "Mean GoalsxGame", ylab = "Density")
abline(v = mean(pl_gxg), col = "red", lwd = 2) #Observed mean
abline(v = CI_pl, col = "blue", lty = 2) #CI






