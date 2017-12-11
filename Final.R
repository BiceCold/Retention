#Client retention based on adwords performance
#Project by Jeremy Bice
#Contributions by Trevor Miller
#Import data
library(rpart.plot)
DPSRaw <- read.csv('DPS_Raw.csv', header = TRUE, sep = ",")
Real_Fake_Clients <- read.csv("Test_Clients.csv", header = TRUE, sep = ",")

#investigate data
summary(DPSRaw)
#47 inactive clients, 156 active clients
#182 ecommerce clients, 21 lead gen clients

#visualize all data and correlations
scatterplotMatrix(DPSRaw,
                  spread = FALSE,
                  smoother.args = list(lty = 2),
                  cex = 1,
    main = "Scatter Plot All Data")


#there seem to be consistent trends. test the power of cost
names(DPSRaw)
cost.cor <- DPSRaw[,4:7]
cor(cost.cor)
cov(cost.cor)

#Create a fit model as well as further iterations of it
DPS_OF <- glm(Status ~ Lifespan + Type + Cost + Conversions + Clicks, family = binomial, data = DPSRaw)
DPS_Fit1 <- glm(Status ~ Lifespan + Conversions, family = binomial, data = DPSRaw)
DPS_Fit2 <- glm(Status ~ Lifespan * Type * Cost * Conversions * Clicks, family = binomial, data = DPSRaw)

#stepwise regression
step(DPS_Fit1)
summary(DPS_Fit1)
vif(DPS_Fit1)

plot(DPS_Fit1)
anova(DPS_Fit1, DPS_OF, test = "Chisq")

#Check for overdispersion
DPS_Fit3 <- glm(formula = Status ~ Lifespan + Conversions, family = quasibinomial, data = DPSRaw)
pchisq(summary(DPS_Fit3)$dispersion * DPS_Fit1$df.residual,
       DPS_Fit1$df.residual, lower = F)

#Overdispersion is not a problem
#check the predictive power
prob <- predict(DPS_Fit2, DPSRaw, type = "response")
logit.pred <- factor(prob > .5, levels = c(FALSE, TRUE))
logit.perf <- table(DPSRaw$Status, logit.pred,
                      dnn = c("Actual", "Predicted"))
logit.perf

#make a decision tree
set.seed(1234)
dtree <- rpart(Status ~ Lifespan + Conversions, data = DPSRaw, method = "class",
               parms = list(split = "information"))
dtree$cptable
plotcp(dtree)
dtree.pruned <- prune(dtree, cp= .0125)
prp(dtree.pruned, type = 2, extra = 104,
    fallen.leaves = TRUE, main = "Decision Tree")

dtree.pred <- predict(dtree.pruned, DPSRaw, type = "class")
dtree.perf <- table(DPSRaw$Status, dtree.pred,
                    dnn = c("Actual", "Predicted"))
dtree.perf

