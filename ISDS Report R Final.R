library(PPforest)
data(fishcatch)
summary(fishcatch)
head(fishcatch)

#Exploratory Analysis

require(ggplot2)
require(GGally)
ggpairs(fishcatch)

#Check for Normality
hist(fishcatch$weight) #Replace with other variables once checked 
plot(fishcatch$Type,xlab="Number of Fish Caught", ylab = "Fish Species")


#Check for Linearity

plot(fishcatch$weight) #Replace with other variables once checked

#Simple Linear Regression Models

Type.lm = lm(weight~Type, data = fishcatch) 
length1.lm = lm(weight~length1, data=fishcatch)
length2.lm = lm(weight~length2, data=fishcatch)
length3.lm = lm(weight~length3, data=fishcatch)
width.lm = lm(weight~width, data=fishcatch)
height.lm = lm(weight~height, data=fishcatch)

summary(Type.lm) #Replace with other models to see statistics

#Plotting SLR
require(ggplot2)
ggplot(data=fishcatch, aes(x=length3, y = weight))+
  geom_point()+
  stat_smooth(method = "lm", col ="dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model for Weight~#'Predictor' Fitted to Data") #Replace with other variables once plotted

#Multiple Regression (Without Length1 and Length2 due to Multicollinearity)

null = lm(weight ~ 1, data=fishcatch)
full = lm(weight ~ .-length1 - length2, data=fishcatch)

step(null, scope=formula(full), direction="forward")
step(full, direction="backward")
step(null, scope = list(upper=full), direction="both")

#Therefore Final Model
final_model = lm(weight~ length3 + Type + height, data = fishcatch)

#Predictions with Final Model

pred = predict(final_model, fishcatch)

plot(fishcatch_new$weight, type = 'l', col = "red", ylab = "Weight", xlab = "Entry Number", main = "Actual Weight of Fish")
lines(pred, type = "l", lty = 1.8, col = 'cornflowerblue' ) 
legend(1,1500, legend=c("Actual Weight", "Predicted Weight"),
       col=c("red", "cornflowerblue"), lty=1:2, cex=0.8)

#Looking at and testing accuracy

summary(final_model)
rmse = sqrt(mean(pred-fishcatch$weight)^2)
rmse

#Appendix tweaked Model, Remove outliers and apply transformations

fishcatch_new=fishcatch[-c(101,102,104),]

null = lm(weight ~ 1, data=fishcatch_new)
full = lm(weight ~ Type + I(length3^2) + I(log(height)), data=fishcatch_new)

step(null, scope=formula(full), direction="forward")
step(full, direction="backward")
step(null, scope = list(upper=full), direction="both")

tweaked_model = lm(weight~ Type + I(length3^2) + I(log(height)), data=fishcatch_new)
summary(tweaked_model)

pred = predict(tweaked_model, fishcatch_new)
rmse = sqrt(mean(pred-fishcatch_new$weight)^2)

#Length3 Model Appendix

null = lm(weight ~ 1, data=fishcatch)
full = lm(weight ~ length3, data=fishcatch)

step(null, scope=formula(full), direction="forward")
step(null, scope = list(upper=full), direction="both")

summary(length3.lm)

pred = predict(length3.lm, fishcatch)
rmse = sqrt(mean(pred-fishcatch$weight)^2)
